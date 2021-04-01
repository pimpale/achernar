use super::ast::*;
use super::codereader::union_of;
use super::dlogger::DiagnosticLogger;
use super::token::*;
use peekmore::{PeekMore, PeekMoreIterator};

// clobbers the cursor
fn peek_past_metadata<TkIter: Iterator<Item = Token>>(
  tkiter: &PeekMoreIterator<TkIter>,
) -> Option<Token> {
  tkiter
    .advance_cursor_while(|tk| match tk {
      Some(&Token {
        kind: TokenKind::Metadata { .. },
        ..
      }) => true,
      _ => false,
    })
    .peek()
    .cloned()
}

fn get_metadata<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
) -> Vec<Metadata> {
  let mut metadata = vec![];
  while let Some(Token {
    kind: TokenKind::Metadata { significant, value },
    range,
  }) = tkiter.peek_nth(0)
  {
    metadata.push(Metadata {
      significant: *significant,
      value: *value,
      range: *range,
    });
    // consume
    tkiter.next();
  }

  // return data
  metadata
}

fn parse_l_binary_op<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
  lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, DiagnosticLogger) -> Expr,
  operator_fn: impl Fn(&mut PeekMoreIterator<TkIter>, DiagnosticLogger) -> Option<BinaryOpKind>,
) -> Expr {
  // parse lower expr
  let mut expr = lower_fn(tkiter, dlogger);

  loop {
    // operator function consumes operator, returning binop
    if let Some(op) = operator_fn(tkiter, dlogger) {
      // define the old expr as our left side
      let left_operand = Box::new(expr);

      // parse rest of expression
      let right_operand = Box::new(lower_fn(tkiter, dlogger));

      // set new expr which contains the lhs and rhs
      expr = Expr {
        kind: ExprData::BinaryOp {
          op,
          left_operand,
          right_operand,
        },
        metadata: vec![],
        range: union_of(left_operand.range, right_operand.range),
      }
    } else {
      // if we dont have a definition just return the current expr
      return expr;
    }
  }
}

fn parse_r_binary_op<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
  lower_fn: impl Fn(&mut PeekMoreIterator<TkIter>, DiagnosticLogger) -> Expr,
  operator_fn: impl Fn(&mut PeekMoreIterator<TkIter>, DiagnosticLogger) -> Option<BinaryOpKind>,
) -> Expr {
  // parse lower expr
  let expr = lower_fn(tkiter, dlogger);

  // operator function consumes operator, returning binop
  if let Some(op) = operator_fn(tkiter, dlogger) {
    // define the old expr as our left side
    let left_operand = Box::new(expr);

    // retrieve metadata
    let metadata = get_metadata(&mut tkiter);

    // consume operator
    tkiter.next();

    // parse rest of expression with same fn (this may stackoverflow)
    let right_operand = Box::new(parse_r_binary_op(tkiter, dlogger, lower_fn, operator_fn));

    // return
    return Expr {
      kind: ExprData::BinaryOp {
        op,
        left_operand,
        right_operand,
      },
      metadata: vec![],
      range: union_of(left_operand.range, right_operand.range),
    };
  } else {
    // if token is invalid we can just return the current expr
    return expr;
  };
}

// a simple operator that checks if the next token is valid, and then advances
fn simple_operator_fn<'a, TkIter: Iterator<Item = Token>>(
  decide_fn: impl Fn(&TokenKind) -> Option<BinaryOpKind> + 'a,
) -> impl Fn(&mut PeekMoreIterator<TkIter>, DiagnosticLogger) -> Option<BinaryOpKind> + 'a {
  move |tkiter: &mut PeekMoreIterator<TkIter>, _: DiagnosticLogger| {
    if let Some(Some(binop)) = tkiter.peek_nth(0).map(|tk| decide_fn(&tk.kind)) {
      tkiter.next();
      Some(binop)
    } else {
      None
    }
  }
}

fn parse_apply_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(tkiter, dlogger, parse_as_operator, |tkiter, _| match tkiter
    .peek_nth(0)
    .map(|tk| tk.kind) {
        Some(tkkind) => {
            if decide_immediate
        }
        None => None
    })
}

fn parse_range_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_as_operator,
    simple_operator_fn(|x| match x {
      TokenKind::Range => Some(BinaryOpKind::Range),
      TokenKind::RangeInclusive => Some(BinaryOpKind::RangeInclusive),
      _ => None,
    }),
  )
}

fn parse_as_operator<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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

fn parse_pow<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
) -> Expr {
  parse_r_binary_op(
    tkiter,
    dlogger,
    parse_constrain,
    simple_operator_fn(|x| match x {
      TokenKind::Pow => Some(BinaryOpKind::Pow),
      _ => None,
    }),
  )
}

fn parse_multiplication_operators<TkIter: Iterator<Item = Token>>(
  tkiter: &mut PeekMoreIterator<TkIter>,
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
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
  dlogger: DiagnosticLogger,
) -> Expr {
  parse_sequence(tkiter, dlogger)
}

pub fn construct_ast<TkIterSource: IntoIterator<Item = Token>>(
  tokens: TkIterSource,
  dlogger: DiagnosticLogger,
) -> Expr {
  parse_expr(&mut tokens.into_iter().peekmore(), dlogger)
}
