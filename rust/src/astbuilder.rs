use super::ast::*;
use super::codereader::union_of;
use super::dlogger::DiagnosticLogger;
use super::token::*;
use lsp_types::Range;
use peekmore::{PeekMore, PeekMoreIterator};

// clobbers the cursor
fn peek_past_metadata<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
) -> &Token {
  tkiter
    .advance_cursor_while(|tk| {
      matches!(
        tk,
        Some(&Token {
          kind: Some(TokenKind::Metadata { .. }),
          ..
        })
      )
    })
    .peek()
    .unwrap()
}

fn get_metadata<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
) -> Vec<Metadata> {
  let mut metadata = vec![];
  while let Some(Token {
    kind: Some(TokenKind::Metadata { significant, value }),
    range,
  }) = tkiter.peek_nth(0).cloned()
  {
    metadata.push(Metadata {
      significant,
      value,
      range,
    });
    // consume
    tkiter.next();
  }

  // return data
  metadata
}

fn parse_prefix_op<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
  lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Expr,
  operator_fn: impl Fn(
    &mut PeekMoreIterator<TkIter>,
    &mut DiagnosticLogger,
  ) -> Option<(UnaryOpKind, Range, Vec<Metadata>)>,
) -> Expr {
  // operator function consumes operator, returning unop
  if let Some((op, range, metadata)) = operator_fn(tkiter, dlogger) {
    // parse rest of expression with same fn (this may stackoverflow)
    let operand = Box::new(parse_prefix_op(tkiter, dlogger, lower_fn, operator_fn));

    // return
    Expr {
      range: union_of(range, operand.range),
      kind: ExprKind::UnaryOp { op, operand },
      metadata,
    }
  } else {
    // if no operator descend
    lower_fn(tkiter, dlogger)
  }
}

fn parse_suffix_op<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
  lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Expr,
  operator_fn: impl Fn(
    &mut PeekMoreIterator<TkIter>,
    &mut DiagnosticLogger,
  ) -> Option<(UnaryOpKind, Range, Vec<Metadata>)>,
) -> Expr {
  // parse lower expr
  let mut expr = lower_fn(tkiter, dlogger);

  loop {
    // operator function consumes operator, returning unop
    if let Some((op, range, metadata)) = operator_fn(tkiter, dlogger) {
      // define the old expr as our left side
      let operand = Box::new(expr);

      // set new expr which contains the lhs and op
      expr = Expr {
        metadata,
        range: union_of(operand.range, range),
        kind: ExprKind::UnaryOp { op, operand },
      }
    } else {
      // if we dont have a definition just return the current expr
      return expr;
    }
  }
}

fn parse_l_binary_op<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
  lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Expr,
  operator_fn: impl Fn(
    &mut PeekMoreIterator<TkIter>,
    &mut DiagnosticLogger,
  ) -> Option<(BinaryOpKind, Range, Vec<Metadata>)>,
) -> Expr {
  // parse lower expr
  let mut expr = lower_fn(tkiter, dlogger);

  loop {
    // operator function consumes operator, returning binop
    if let Some((op, _, metadata)) = operator_fn(tkiter, dlogger) {
      // define the old expr as our left side
      let left_operand = Box::new(expr);

      // parse rest of expression
      let right_operand = Box::new(lower_fn(tkiter, dlogger));

      // set new expr which contains the lhs and rhs
      expr = Expr {
        range: union_of(left_operand.range, right_operand.range),
        kind: ExprKind::BinaryOp {
          op,
          left_operand,
          right_operand,
        },
        metadata,
      }
    } else {
      // if we dont have a definition just return the current expr
      return expr;
    }
  }
}

fn parse_r_binary_op<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
  lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Expr,
  operator_fn: impl Fn(
    &mut PeekMoreIterator<TkIter>,
    &mut DiagnosticLogger,
  ) -> Option<(BinaryOpKind, Range, Vec<Metadata>)>,
) -> Expr {
  // parse lower expr
  let expr = lower_fn(tkiter, dlogger);

  // operator function consumes operator, returning binop
  if let Some((op, _, metadata)) = operator_fn(tkiter, dlogger) {
    // define the old expr as our left side
    let left_operand = Box::new(expr);

    // parse rest of expression with same fn (this may stackoverflow)
    let right_operand = Box::new(parse_r_binary_op(tkiter, dlogger, lower_fn, operator_fn));

    // return
    Expr {
      range: union_of(left_operand.range, right_operand.range),
      kind: ExprKind::BinaryOp {
        op,
        left_operand,
        right_operand,
      },
      metadata,
    }
  } else {
    // if token is invalid we can just return the current expr
    expr
  }
}

// a simple operator that checks if the next token is valid, and then advances
fn simple_operator_fn<'a, TkIter: Iterator<Item = Token>, OpKind>(
  decide_fn: impl Fn(&TokenKind) -> Option<OpKind> + 'a,
) -> impl Fn(
  &mut PeekMoreIterator<TkIter>,
  &mut DiagnosticLogger,
) -> Option<(OpKind, Range, Vec<Metadata>)>
     + 'a {
  move |tkiter: &mut PeekMoreIterator<TkIter>, _: &mut DiagnosticLogger| {
    if let Token {
      kind: Some(kind),
      range,
    } = peek_past_metadata(tkiter)
    {
      if let Some(binop) = decide_fn(kind) {
        let range = *range;
        let metadata = get_metadata(tkiter);
        // drop peeked operator
        tkiter.next();
        return Some((binop, range, metadata));
      }
    }
    None
  }
}

fn parse_exact_struct_literal<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  assert!(tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::BraceLeft));
  let Token { range: lrange, .. } = tkiter.next().unwrap();

  let body = Box::new(parse_expr(tkiter, dlogger));

  let Token {
    range: rrange,
    kind,
  } = tkiter.next().unwrap();
  match kind {
    Some(TokenKind::BraceRight) => Expr {
      range: union_of(lrange, rrange),
      metadata,
      kind: ExprKind::StructLiteral(body),
    },
    _ => {
      dlogger.log_unexpected_token_specific(rrange, Some(TokenKind::BraceRight), kind);
      Expr {
        range: union_of(lrange, body.range),
        metadata,
        kind: ExprKind::StructLiteral(body),
      }
    }
  }
}

fn parse_exact_group<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  assert!(tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::ParenLeft));
  let Token { range: lrange, .. } = tkiter.next().unwrap();

  let body = Box::new(parse_expr(tkiter, dlogger));

  let Token {
    range: rrange,
    kind,
  } = tkiter.next().unwrap();
  match kind {
    Some(TokenKind::ParenRight) => Expr {
      range: union_of(lrange, rrange),
      metadata,
      kind: ExprKind::Group(body),
    },
    _ => {
      dlogger.log_unexpected_token_specific(rrange, Some(TokenKind::ParenRight), kind);
      Expr {
        range: union_of(lrange, body.range),
        metadata,
        kind: ExprKind::Group(body),
      }
    }
  }
}

// parses an exact reference or panics
fn parse_exact_reference<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  _dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);

  if let Token {
    range,
    kind: Some(TokenKind::Identifier(identifier)),
  } = tkiter.next().unwrap()
  {
    Expr {
      metadata,
      range,
      kind: ExprKind::Reference(identifier),
    }
  } else {
    unimplemented!()
  }
}

// parses a ret or panics
fn parse_exact_ret<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  assert!(tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::Ret));
  let ret_tk = tkiter.next().unwrap();
  let label_tk = tkiter.next().unwrap();
  let label;
  if let Some(TokenKind::Identifier(identifier)) = label_tk.kind {
    label = Some(identifier);
  } else {
    // log the expected label
    dlogger.log_unexpected_token_specific(
      label_tk.range,
      Some(TokenKind::Label(vec![])),
      label_tk.kind,
    );
    label = None;
  }

  let body = Box::new(parse_expr(tkiter, dlogger));

  Expr {
    metadata,
    range: union_of(ret_tk.range, body.range),
    kind: ExprKind::Ret { label, body },
  }
}

// parses a defer or panics
fn parse_exact_defer<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  assert!(tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::Defer));
  let defer_tk = tkiter.next().unwrap();
  let label_tk = tkiter.next().unwrap();
  let label;
  if let Some(TokenKind::Identifier(identifier)) = label_tk.kind {
    label = Some(identifier);
  } else {
    // log the expected label
    dlogger.log_unexpected_token_specific(
      label_tk.range,
      Some(TokenKind::Label(vec![])),
      label_tk.kind,
    );
    label = None;
  }

  let body = Box::new(parse_expr(tkiter, dlogger));

  Expr {
    metadata,
    range: union_of(defer_tk.range, body.range),
    kind: ExprKind::Defer { label, body },
  }
}

fn parse_case_options<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_assign,
    simple_operator_fn(|x| match x {
      TokenKind::CaseOption => Some(BinaryOpKind::Sequence),
      _ => None,
    }),
  )
}

// parses a case or panics
fn parse_exact_case<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  assert!(tkiter.peek_nth(0).unwrap().kind == Some(TokenKind::Case));
  let case_tk = tkiter.next().unwrap();
  let expr = Box::new(parse_term(tkiter, dlogger));
  let of_tk = tkiter.next().unwrap();
  if of_tk.kind != Some(TokenKind::Of) {
    dlogger.log_unexpected_token_specific(of_tk.range, Some(TokenKind::Of), of_tk.kind);
  }
  let cases = Box::new(parse_case_options(tkiter, dlogger));

  Expr {
    metadata,
    range: union_of(case_tk.range, cases.range),
    kind: ExprKind::CaseOf { expr, cases },
  }
}

// parses an exact label or panics
fn parse_exact_label<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  if let Token {
    range,
    kind: Some(TokenKind::Label(label)),
  } = tkiter.next().unwrap()
  {
    let body = Box::new(parse_term(tkiter, dlogger));
    Expr {
      metadata,
      range: union_of(range, body.range),
      kind: ExprKind::Label { label, body },
    }
  } else {
    unimplemented!()
  }
}

fn parse_exact_bool_type<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::BoolType, ExprKind::BoolType)(tkiter, dlogger)
}

// parses a bool
fn parse_exact_bool<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  _: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  if let Token {
    range,
    kind: Some(TokenKind::Bool(value)),
  } = tkiter.next().unwrap()
  {
    Expr {
      metadata,
      range,
      kind: ExprKind::Bool(value),
    }
  } else {
    unreachable!();
  }
}

// parses a string
fn parse_exact_string<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  _: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  if let Token {
    range,
    kind: Some(TokenKind::String { value, block }),
  } = tkiter.next().unwrap()
  {
    Expr {
      metadata,
      range,
      kind: ExprKind::String { value, block },
    }
  } else {
    unreachable!();
  }
}

fn parse_exact_int_type<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::IntType, ExprKind::IntType)(tkiter, dlogger)
}

// parses a int
fn parse_exact_int<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  _dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  if let Token {
    range,
    kind: Some(TokenKind::Int(value)),
  } = tkiter.next().unwrap()
  {
    Expr {
      metadata,
      range,
      kind: ExprKind::Int(value),
    }
  } else {
    unreachable!();
  }
}

fn parse_exact_real_type<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::RealType, ExprKind::RealType)(tkiter, dlogger)
}

// parses a real
fn parse_exact_real<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  _dlogger: &mut DiagnosticLogger,
) -> Expr {
  let metadata = get_metadata(tkiter);
  if let Token {
    range,
    kind: Some(TokenKind::Real(value)),
  } = tkiter.next().unwrap()
  {
    Expr {
      metadata,
      range,
      kind: ExprKind::Real(value),
    }
  } else {
    unreachable!();
  }
}

// creates a parser that parses a single token of the type specified
fn parse_exact_simple<TkIter: Iterator<Item = Token>>(
  expected_kind: TokenKind,
  result_kind: ExprKind,
) -> impl FnOnce(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Expr {
  move |tkiter: &mut PeekMoreIterator<TkIter>, _: &mut DiagnosticLogger| {
    let metadata = get_metadata(tkiter);
    let Token {
      range,
      kind: token_kind,
    } = tkiter.next().unwrap();
    let expected_kind_opt = Some(expected_kind);
    if token_kind == expected_kind_opt {
      Expr {
        metadata,
        range,
        kind: result_kind,
      }
    } else {
      unreachable!()
    }
  }
}

fn parse_exact_this<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::This, ExprKind::This)(tkiter, dlogger)
}

fn parse_exact_ignore<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::Ignore, ExprKind::BindIgnore)(tkiter, dlogger)
}

fn parse_exact_splat<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::Splat, ExprKind::BindSplat)(tkiter, dlogger)
}

fn parse_exact_nil_type<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::NilType, ExprKind::NilType)(tkiter, dlogger)
}

fn parse_exact_never_type<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_exact_simple(TokenKind::NeverType, ExprKind::NeverType)(tkiter, dlogger)
}

fn decide_term<TkIter: Iterator<Item = Token>>(
  tkkind: &TokenKind,
) -> Option<fn(&mut PeekMoreIterator<TkIter>, &mut DiagnosticLogger) -> Expr> {
  match *tkkind {
    TokenKind::This => Some(parse_exact_this::<TkIter>),
    TokenKind::Ignore => Some(parse_exact_ignore::<TkIter>),
    TokenKind::Splat => Some(parse_exact_splat::<TkIter>),
    TokenKind::NilType => Some(parse_exact_nil_type::<TkIter>),
    TokenKind::NeverType => Some(parse_exact_never_type::<TkIter>),
    TokenKind::BoolType => Some(parse_exact_bool_type::<TkIter>),
    TokenKind::Bool(_) => Some(parse_exact_bool::<TkIter>),
    TokenKind::IntType => Some(parse_exact_int_type::<TkIter>),
    TokenKind::Int(_) => Some(parse_exact_int::<TkIter>),
    TokenKind::RealType => Some(parse_exact_real_type::<TkIter>),
    TokenKind::Real(_) => Some(parse_exact_real::<TkIter>),
    TokenKind::BraceLeft => Some(parse_exact_struct_literal::<TkIter>),
    TokenKind::String { .. } => Some(parse_exact_string::<TkIter>),
    TokenKind::ParenLeft => Some(parse_exact_group::<TkIter>),
    TokenKind::Ret => Some(parse_exact_ret::<TkIter>),
    TokenKind::Defer => Some(parse_exact_defer::<TkIter>),
    TokenKind::Case => Some(parse_exact_case::<TkIter>),
    TokenKind::Label(_) => Some(parse_exact_label::<TkIter>),
    TokenKind::Identifier(_) => Some(parse_exact_reference::<TkIter>),
    _ => None,
  }
}

// parses basic term
fn parse_term<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  let Token {
    kind: maybe_kind,
    range,
  } = peek_past_metadata(tkiter).clone();
  if let Some(kind) = maybe_kind {
    if let Some(parser) = decide_term(&kind) {
      parser(tkiter, dlogger)
    } else {
      // grab metadata
      let metadata = get_metadata(tkiter);
      // consume unexpected token
      dlogger.log_unexpected_token(range, "term", Some(kind));
      tkiter.next();
      Expr {
        range,
        metadata,
        kind: ExprKind::None,
      }
    }
  } else {
    dlogger.log_unexpected_token(range, "term", maybe_kind);
    Expr {
      range,
      kind: ExprKind::None,
      metadata: get_metadata(tkiter),
    }
  }
}

fn decide_prefix(tkkind: &TokenKind) -> Option<UnaryOpKind> {
  match tkkind {
    TokenKind::Ref => Some(UnaryOpKind::Ref),
    TokenKind::Deref => Some(UnaryOpKind::Deref),
    TokenKind::Struct => Some(UnaryOpKind::Struct),
    TokenKind::Enum => Some(UnaryOpKind::Enum),
    TokenKind::New => Some(UnaryOpKind::New),
    TokenKind::Not => Some(UnaryOpKind::Not),
    TokenKind::Loop => Some(UnaryOpKind::Loop),
    TokenKind::Pat => Some(UnaryOpKind::Pat),
    TokenKind::Val => Some(UnaryOpKind::Val),
    TokenKind::Bind => Some(UnaryOpKind::Bind),
    _ => None,
  }
}

fn parse_prefix_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_prefix_op(
    tkiter,
    dlogger,
    parse_term,
    simple_operator_fn(decide_prefix),
  )
}

fn parse_suffix_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_suffix_op(
    tkiter,
    dlogger,
    parse_prefix_operators,
    simple_operator_fn(|x| match x {
      TokenKind::NilSafeAssert => Some(UnaryOpKind::NilSafeAssert),
      _ => None,
    }),
  )
}

fn parse_tight_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_suffix_operators,
    simple_operator_fn(|x| match x {
      TokenKind::RevApply => Some(BinaryOpKind::RevApply),
      TokenKind::NilSafeRevApply => Some(BinaryOpKind::NilSafeRevApply),
      TokenKind::ModuleAccess => Some(BinaryOpKind::ModuleAccess),
      _ => None,
    }),
  )
}

fn parse_apply_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(tkiter, dlogger, parse_tight_operators, |tkiter, _| {
    if let Token {
      kind: Some(kind),
      range,
    } = peek_past_metadata(tkiter)
    {
      if decide_prefix(kind).is_some() || decide_term::<TkIter>(kind).is_some() {
        return Some((BinaryOpKind::Apply, *range, vec![]));
      }
    }
    None
  })
}

fn parse_range_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_apply_operators,
    simple_operator_fn(|x| match x {
      TokenKind::Range => Some(BinaryOpKind::Range),
      TokenKind::RangeInclusive => Some(BinaryOpKind::RangeInclusive),
      _ => None,
    }),
  )
}

fn parse_as_operator<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_range_operators,
    simple_operator_fn(|x| match x {
      TokenKind::As => Some(BinaryOpKind::As),
      _ => None,
    }),
  )
}

fn parse_constrain<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_as_operator,
    simple_operator_fn(|x| match x {
      TokenKind::Constrain => Some(BinaryOpKind::Constrain),
      _ => None,
    }),
  )
}

fn parse_async_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_prefix_op(
    tkiter,
    dlogger,
    parse_constrain,
    simple_operator_fn(|x| match x {
      TokenKind::Async => Some(UnaryOpKind::Async),
      TokenKind::Await => Some(UnaryOpKind::Await),
      _ => None,
    }),
  )
}

fn parse_pow<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_async_operators,
    simple_operator_fn(|x| match x {
      TokenKind::Pow => Some(BinaryOpKind::Pow),
      _ => None,
    }),
  )
}

fn parse_multiplication_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_pow,
    simple_operator_fn(|x| match x {
      TokenKind::Mul => Some(BinaryOpKind::Mul),
      TokenKind::Div => Some(BinaryOpKind::Div),
      TokenKind::Rem => Some(BinaryOpKind::Rem),
      _ => None,
    }),
  )
}

fn parse_addition_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_multiplication_operators,
    simple_operator_fn(|x| match x {
      TokenKind::Plus => Some(BinaryOpKind::Add),
      TokenKind::Minus => Some(BinaryOpKind::Sub),
      _ => None,
    }),
  )
}

fn parse_compare_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_addition_operators,
    simple_operator_fn(|x| match x {
      TokenKind::Less => Some(BinaryOpKind::Less),
      TokenKind::Greater => Some(BinaryOpKind::Greater),
      TokenKind::LessEqual => Some(BinaryOpKind::LessEqual),
      TokenKind::GreaterEqual => Some(BinaryOpKind::GreaterEqual),
      TokenKind::Equal => Some(BinaryOpKind::Equal),
      TokenKind::NotEqual => Some(BinaryOpKind::NotEqual),
      _ => None,
    }),
  )
}

fn parse_binary_bool_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_compare_operators,
    simple_operator_fn(|x| match x {
      TokenKind::And => Some(BinaryOpKind::And),
      TokenKind::Or => Some(BinaryOpKind::Or),
      _ => None,
    }),
  )
}

fn parse_compose<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_binary_bool_operators,
    simple_operator_fn(|x| match x {
      TokenKind::Compose => Some(BinaryOpKind::Compose),
      _ => None,
    }),
  )
}

fn parse_binary_list_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_compose,
    simple_operator_fn(|x| match x {
      TokenKind::Append => Some(BinaryOpKind::Append),
      _ => None,
    }),
  )
}

fn parse_binary_set_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_binary_list_operators,
    simple_operator_fn(|x| match x {
      TokenKind::RelativeComplement => Some(BinaryOpKind::RelativeComplement),
      TokenKind::Union => Some(BinaryOpKind::Union),
      TokenKind::Intersection => Some(BinaryOpKind::Intersection),
      TokenKind::SymmetricDifference => Some(BinaryOpKind::SymmetricDifference),
      _ => None,
    }),
  )
}

fn parse_binary_type_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_binary_set_operators,
    simple_operator_fn(|x| match x {
      TokenKind::Both => Some(BinaryOpKind::Both),
      TokenKind::Either => Some(BinaryOpKind::Either),
      _ => None,
    }),
  )
}

fn parse_pipe_backward<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_binary_type_operators,
    simple_operator_fn(|x| match x {
      TokenKind::PipeBackward => Some(BinaryOpKind::PipeBackward),
      _ => None,
    }),
  )
}
fn parse_pipe_forward<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_pipe_backward,
    simple_operator_fn(|x| match x {
      TokenKind::PipeForward => Some(BinaryOpKind::PipeForward),
      _ => None,
    }),
  )
}

fn parse_defun<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_pipe_forward,
    simple_operator_fn(|x| match x {
      TokenKind::Defun => Some(BinaryOpKind::Defun),
      _ => None,
    }),
  )
}

fn parse_assign<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_defun,
    simple_operator_fn(|x| match x {
      TokenKind::Assign => Some(BinaryOpKind::Assign),
      _ => None,
    }),
  )
}

fn parse_sequence<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_l_binary_op(
    tkiter,
    dlogger,
    parse_assign,
    simple_operator_fn(|x| match x {
      TokenKind::Sequence => Some(BinaryOpKind::Sequence),
      _ => None,
    }),
  )
}

fn parse_expr<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: &mut DiagnosticLogger,
) -> Expr {
  parse_sequence(tkiter, dlogger)
}

pub fn construct_ast<TkIterSource: IntoIterator<Item = Token>>(
  tokens: TkIterSource,
  mut dlogger: DiagnosticLogger,
) -> Expr {
  parse_expr(&mut tokens.into_iter().peekmore(), &mut dlogger)
}
