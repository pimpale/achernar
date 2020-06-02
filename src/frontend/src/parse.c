#include "parse.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "error.h"
#include "json.h"
#include "lex.h"
#include "std_allocator.h"
#include "token.h"
#include "utils.h"
#include "vector.h"

// convoluted function to save repetitive tasks
#define PARSE_LIST(members_vec_ptr, diagnostics_vec_ptr,                       \
                   member_parse_function, member_kind, delimiting_token_kind,  \
                   missing_delimiter_error, end_lncol, parser)                 \
                                                                               \
  while (true) {                                                               \
    Token pl_ntk = parse_peek(parser); /* next token kind */                   \
    if (pl_ntk.kind == delimiting_token_kind) {                                \
      end_lncol = pl_ntk.span.end;                                             \
      parse_next(parser, diagnostics_vec_ptr); /* accept delimiting tk */      \
      break;                                                                   \
    } else if (pl_ntk.kind == tk_Eof) {                                        \
      *VEC_PUSH(diagnostics_vec_ptr, Diagnostic) =                             \
          DIAGNOSTIC(missing_delimiter_error, pl_ntk.span);                    \
      end_lncol = pl_ntk.span.end;                                             \
      break;                                                                   \
    }                                                                          \
    /* if there wasn't an end delimiter, push the last token back */           \
    member_parse_function(VEC_PUSH(members_vec_ptr, member_kind), parser);     \
  }

// Parser
Parser parse_create(Lexer *lp, Allocator *a) {
  return (Parser){
      .a = a,                                  // Allocator
      .lexer = lp,                             // Lexer Pointer
      .next_tokens_stack = vec_create(a),      // Stack of all peeked tokens
      .next_diagnostics_stack = vec_create(a), // Stack<Vector<Comment>>
      .next_comments_stack = vec_create(a),    // Stack<Vector<Comment>>
      .comments = vec_create(a),               // Vector<Comment>
      .paren_depth = 0,
      .brace_depth = 0,
      .bracket_depth = 0,
  };
}

/// gets the next token, ignoring buffering
static Token parse_rawNext(Parser *parser, Vector *comments,
                           Vector *diagnostics) {
  while (true) {
    Token c = tk_next(parser->lexer, diagnostics, parser->a);
    switch (c.kind) {
    case tk_Comment: {
      *VEC_PUSH(comments, Comment) = (Comment){
          .span = c.span, .scope = c.comment.scope, .data = c.comment.comment};
      // keep reading
      break;
    }
    case tk_ParenLeft: {
      parser->paren_depth++;
      return c;
    }
    case tk_ParenRight: {
      parser->paren_depth--;
      return c;
    }
    case tk_BraceLeft: {
      parser->brace_depth++;
      return c;
    }
    case tk_BraceRight: {
      parser->brace_depth--;
      return c;
    }
    case tk_BracketLeft: {
      parser->bracket_depth++;
      return c;
    }
    case tk_BracketRight: {
      parser->bracket_depth--;
      return c;
    }
    default: {
      return c;
    }
    }
  }
}

// If the peeked token stack is not empty:
//    Return the top element of the top of the token
//    Pop the top of the next_comments_stack
//    For each element in the next comments stack, push it to the top of the
//    current scope
// If has an ungotten token, return that. Else return next in line, and cache it
static Token parse_next(Parser *pp, Vector *diagnostics) {
  // the current scope we aim to push the comments to
  size_t top_index = VEC_LEN(&pp->comments, Vector);
  assert(top_index > 0);
  Vector *current_scope = VEC_GET(&pp->comments, top_index - 1, Vector);
  if (VEC_LEN(&pp->next_tokens_stack, Token) != 0) {
    // we want to merge together the next token's diagnostics and comments into
    // the provided ones

    // Vector containing all comments for the next token
    Vector next_token_comments;
    // pop a vector off the stack and assign it to next_token_comments
    VEC_POP(&pp->next_comments_stack, &next_token_comments, Vector);

    // append next_token_comments to the current scope
    vec_append(current_scope, &next_token_comments);

    // Vector containing all diagnostics for the next
    Vector next_token_diagnostics;
    VEC_POP(&pp->next_diagnostics_stack, &next_token_diagnostics, Vector);

    // append next_token_diagnostics to the diagnostics vector
    vec_append(diagnostics, &next_token_diagnostics);

    // set the token
    Token ret;
    VEC_POP(&pp->next_tokens_stack, &ret, Token);
    return ret;
  } else {
    return parse_rawNext(pp, current_scope, diagnostics);
  }
}

// gets the k'th token
// K must be greater than 0
static Token parse_peekNth(Parser *pp, size_t k) {
  assert(k > 0);
  for (size_t i = VEC_LEN(&pp->next_tokens_stack, Token); i < k; i++) {
    // Create vector to store any comments
    Vector *next_token_comments = VEC_PUSH(&pp->next_comments_stack, Vector);
    *next_token_comments = vec_createWithCapacity(pp->a, 0);

    // Create vector to store any diagnostics
    Vector *next_token_diagnostics =
        VEC_PUSH(&pp->next_diagnostics_stack, Vector);
    *next_token_diagnostics = vec_createWithCapacity(pp->a, 0);

    // parse the token and add it to the top of the stack
    *VEC_PUSH(&pp->next_tokens_stack, Token) =
        parse_rawNext(pp, next_token_comments, next_token_diagnostics);
  }
  // peek the token at the desired location's kind
  return *VEC_GET(&pp->next_tokens_stack, k - 1, Token);
}

static Token parse_peek(Parser *parser) { return parse_peekNth(parser, 1); }

// pops the top comment scope off the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// REQUIRES: the stack has at least one member
/// GUARANTEES: return value is the topmost element of the comment stack
/// GUARANTEES: the topmost element fo the comment stack has been removed
static Vector popCommentScopeParser(Parser *parser) {
  assert(VEC_LEN(&parser->comments, Vector) >= 1);
  Vector v;
  VEC_POP(&parser->comments, &v, Vector);
  return v;
}

// pushes a new empty comment scope to the top of the comment stack
/// REQUIRES: `parser` is pointer to valid Parser
/// GUARANTEES: `parser`'s comment stack has new empty scope on top of stack
static void pushCommentScopeParser(Parser *parser) {
  // We create the vector with zero capacity initially so that
  // allocation is deferred until we actually encounter a comment
  // Most scopes will not have a comment
  *VEC_PUSH(&parser->comments, Vector) = vec_createWithCapacity(parser->a, 0);
}

Allocator *parse_release(Parser *pp) {
  for (size_t i = 0; i < VEC_LEN(&pp->next_comments_stack, Vector); i++) {
    vec_destroy(VEC_GET(&pp->next_comments_stack, i, Vector));
    vec_destroy(VEC_GET(&pp->next_diagnostics_stack, i, Vector));
  }
  vec_destroy(&pp->next_diagnostics_stack);
  vec_destroy(&pp->next_comments_stack);
  vec_destroy(&pp->next_tokens_stack);

  // ensure that caller has popped the current scope
  assert(VEC_LEN(&pp->comments, Vector) == 0);
  vec_destroy(&pp->comments);
  return pp->a;
}

// Heuristic to resync the parser after reaching a syntax error
// Continues reading tokens until we jump out of a paren, brace, or bracket
// group discards any comments and tokens found
// Will store any lexer errors into the diagnostics
static void resyncParser(Parser *parser, Vector *diagnostics) {
  Token t;
  Vector comments = vec_create(&std_allocator);
  int64_t initial_paren_depth = parser->paren_depth;
  int64_t initial_brace_depth = parser->brace_depth;
  int64_t initial_bracket_depth = parser->bracket_depth;
  while (true) {
    if (initial_brace_depth <= parser->brace_depth &&
        initial_paren_depth <= parser->paren_depth &&
        initial_bracket_depth <= parser->bracket_depth) {
      break;
    }
    t = parse_rawNext(parser, &comments, diagnostics);
  }
  vec_destroy(&comments);
}

// Note that all errors resync at the statement level
static void parseStmnt(Stmnt *sp, Parser *parser);
static void parseValueExpr(ValueExpr *vep, Parser *parser);
static void parseTypeExpr(TypeExpr *tep, Parser *parser);
static void parsePatternExpr(PatternExpr *pp, Parser *parser);

static void certain_parseBuiltin(Builtin *bp, Parser *parser) {
  ZERO(bp);
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);

  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Builtin);

  LnCol start = t.span.start;
  LnCol end;

  bp->name = t.builtin;

  Vector parameters = vec_create(parser->a);

  t = parse_next(parser, &diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_BuiltinExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&parameters,                  // members_vec_ptr
             &diagnostics,                 // diagnostics_vec_ptr
             parseStmnt,                   // member_parse_function
             Stmnt,                        // member_kind
             tk_ParenRight,                // delimiting_token_kind
             DK_BuiltinExpectedRightParen, // missing_delimiter_error
             end,                          // end_lncol
             parser                        // parser
  )

CLEANUP:
  bp->parameters_len = VEC_LEN(&parameters, Stmnt);
  bp->parameters = vec_release(&parameters);

  bp->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  bp->diagnostics = vec_release(&diagnostics);
  bp->span = SPAN(start, end);
}

static void parsePath(Path *pp, Parser *parser) {
  // start comment scope
  pushCommentScopeParser(parser);

  Vector diagnostics = vec_create(parser->a);

  Token t = parse_next(parser, &diagnostics);
  // start and finish
  LnCol start = t.span.start;
  LnCol end;

  Vector pathSegments = vec_create(parser->a);

  // ensure that it is a valid path
  if (t.kind != tk_Identifier) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  *VEC_PUSH(&pathSegments, char *) = t.identifier;

  while (true) {
    t = parse_peek(parser);
    if (t.kind == tk_ScopeResolution) {
      // discard the scope resolution
      t = parse_next(parser, &diagnostics);
      // now check if we have an issue
      t = parse_next(parser, &diagnostics);
      if (t.kind != tk_Identifier) {
        *VEC_PUSH(&diagnostics, Diagnostic) =
            DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
        end = t.span.end;
        goto CLEANUP;
      }

      *VEC_PUSH(&pathSegments, char *) = t.identifier;
    } else {
      // we've reached the end of the path
      end = t.span.end;
      break;
    }
  }

CLEANUP:
  pp->pathSegments_len = VEC_LEN(&pathSegments, char *);
  pp->pathSegments = vec_release(&pathSegments);

  pp->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  pp->diagnostics = vec_release(&diagnostics);

  pp->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  pp->comments_len = VEC_LEN(&comments, Comment);
  pp->comments = vec_release(&comments);
}

static void certain_parseVoidValueExpr(ValueExpr *vep, Parser *parser) {
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Void);
  vep->kind = VEK_VoidLiteral;
  vep->span = t.span;
  vep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  vep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseIntValueExpr(ValueExpr *vep, Parser *parser) {
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Int);
  vep->kind = VEK_IntLiteral;
  vep->intLiteral.value = t.int_literal;
  vep->span = t.span;
  vep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  vep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseBoolValueExpr(ValueExpr *vep, Parser *parser) {
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Bool);
  vep->kind = VEK_BoolLiteral;
  vep->boolLiteral.value = t.bool_literal;
  vep->span = t.span;
  vep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  vep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseFloatValueExpr(ValueExpr *vep, Parser *parser) {
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Float);
  vep->kind = VEK_FloatLiteral;
  vep->floatLiteral.value = t.float_literal;
  vep->span = t.span;
  vep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  vep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseCharValueExpr(ValueExpr *vep, Parser *parser) {
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Char);
  vep->kind = VEK_CharLiteral;
  vep->charLiteral.value = t.char_literal;
  vep->span = t.span;
  vep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  vep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseStringValueExpr(ValueExpr *svep, Parser *parser) {
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_String);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = t.string_literal;
  svep->span = t.span;
  svep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  svep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseFnValueExpr(ValueExpr *fvep, Parser *parser) {
  ZERO(fvep);

  fvep->kind = VEK_Fn;

  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Fn);
  LnCol start = t.span.start;
  LnCol end = t.span.end;

  // check for leftparen
  t = parse_next(parser, &diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValueExprExpectedLeftParen, t.span);
    goto CLEANUP;
  }

  Span lparenspan = t.span;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                      // members_vec_ptr
             &diagnostics,                     // diagnostics_vec_ptr
             parsePatternExpr,                 // member_parse_function
             PatternExpr,                      // member_kind
             tk_ParenRight,                    // delimiting_token_kind
             DK_FnValueExprExpectedRightParen, // missing_delimiter_error
             end,                              // end_lncol
             parser                            // parser
  )

  fvep->fnExpr.parameters_len = VEC_LEN(&parameters, PatternExpr);
  fvep->fnExpr.parameters = vec_release(&parameters);

  t = parse_peek(parser);
  if (t.kind == tk_Colon) {
    fvep->fnExpr.type = ALLOC(parser->a, TypeExpr);
    // advance
    parse_next(parser, &diagnostics);

    parseTypeExpr(fvep->fnExpr.type, parser);
  } else {
    fvep->fnExpr.type = ALLOC(parser->a, TypeExpr);
    fvep->fnExpr.type->kind = TEK_Omitted;
    fvep->fnExpr.type->span = lparenspan;
    fvep->fnExpr.type->diagnostics_len = 0;
    fvep->fnExpr.type->comments_len = 0;
  }

  t = parse_next(parser, &diagnostics);

  if (t.kind != tk_Arrow) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValueExprExpectedArrow, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.body = ALLOC(parser->a, ValueExpr);
  parseValueExpr(fvep->fnExpr.body, parser);
  end = fvep->fnExpr.body->span.end;

CLEANUP:
  fvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  fvep->diagnostics = vec_release(&diagnostics);
  fvep->span = SPAN(start, end);
}

static void certain_parseBlockValueExpr(ValueExpr *bvep, Parser *parser) {
  ZERO(bvep);
  bvep->kind = VEK_Block;

  // List of diagnostics
  Vector diagnostics = vec_create(parser->a);

  Token t = parse_next(parser, &diagnostics);

  LnCol start = t.span.start;

  // blocks may be labeled
  if (t.kind == tk_Label) {
    bvep->blockExpr.has_label = true;
    bvep->blockExpr.label = t.label;
    parse_next(parser, &diagnostics);
  } else {
    bvep->blockExpr.has_label = false;
  }

  assert(t.kind == tk_BraceLeft);

  // Create list of statements
  Vector statements = vec_create(parser->a);

  LnCol end;

  PARSE_LIST(&statements,                // members_vec_ptr
             &diagnostics,               // diagnostics_vec_ptr
             parseStmnt,                 // member_parse_function
             Stmnt,                      // member_kind
             tk_BraceRight,              // delimiting_token_kind
             DK_BlockExpectedRightBrace, // missing_delimiter_error
             end,                        // end_lncol
             parser                      // parser
  )

  bvep->blockExpr.statements_len = VEC_LEN(&statements, Stmnt);
  bvep->blockExpr.statements = vec_release(&statements);
  bvep->span = SPAN(start, end);
  bvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  bvep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseBuiltinValueExpr(ValueExpr *bvep, Parser *parser) {
  bvep->kind = VEK_Builtin;
  bvep->builtinExpr.builtin = ALLOC(parser->a, Builtin);
  certain_parseBuiltin(bvep->builtinExpr.builtin, parser);
  bvep->diagnostics_len = 0;
  bvep->span = bvep->builtinExpr.builtin->span;
}

static void certain_parseDeferValueExpr(ValueExpr *dep, Parser *parser) {
  dep->kind = VEK_Defer;
  Vector diagnostics = vec_createWithCapacity(parser->a, 0);
  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Defer);
  dep->deferExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(dep->deferExpr.value, parser);
  dep->span = SPAN(t.span.start, dep->deferExpr.value->span.end);
  dep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  dep->diagnostics = vec_release(&diagnostics);
  return;
}

static void certain_parseReturnValueExpr(ValueExpr *rep, Parser *parser) {
  ZERO(rep);
  rep->kind = VEK_Return;

  Vector diagnostics = vec_createWithCapacity(parser->a, 0);

  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Return);

  LnCol start = t.span.start;
  LnCol end;

  parse_next(parser, &diagnostics);

  if (t.kind != tk_Label) {
    *VEC_PUSH(&diagnostics, Diagnostic) = DIAGNOSTIC(DK_ReturnExpectedLabel, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  rep->returnExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(rep->returnExpr.value, parser);
  end = rep->returnExpr.value->span.end;

CLEANUP:
  rep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  rep->diagnostics = vec_release(&diagnostics);
  rep->span = SPAN(start, end);
  return;
}

static void certain_parseContinueValueExpr(ValueExpr *cep, Parser *parser) {
  ZERO(cep);
  cep->kind = VEK_Continue;

  Vector diagnostics = vec_createWithCapacity(parser->a, 0);

  Token t = parse_next(parser, &diagnostics);
  assert(t.kind == tk_Continue);

  LnCol start = t.span.start;
  LnCol end;

  parse_next(parser, &diagnostics);
  end = t.span.end;

   // TODO from here

  if (t.kind != tk_Label) {
    *VEC_PUSH(&diagnostics, Diagnostic) = DIAGNOSTIC(DK_ContinueExpectedLabel, t.span);
    goto CLEANUP;
  }

CLEANUP:
  cep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  cep->diagnostics = vec_release(&diagnostics);
  cep->span = SPAN(start, end);
  return;
}

static void certain_parseLoopValueExpr(ValueExpr *lep, Parser *parser) {
  lep->kind = VEK_Loop;

  Vector diagnostics = vec_createWithCapacity(parser->a, 0);

  Token t = parse_next(parser, &diagnostics);
  LnCol start = t.span.start;

  if (t.kind == tk_Label) {
    lep->loopExpr.has_label = true;
    lep->loopExpr.label = t.label;
    // get next 
    parse_next(parser,&diagnostics);
  } else {
    lep->loopExpr.has_label = false;
  }

  assert(t.kind == tk_Loop);

  lep->loopExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(lep->loopExpr.value, parser);

  lep->span = SPAN(start, lep->loopExpr.value->span.end);
  lep->diagnostics_len = 0;
  return;
}

// Pattern : Expr,
static void parseMatchCaseExpr(struct MatchCaseExpr_s *mcep, Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(mcep);

  Vector diagnostics = vec_createWithCapacity(parser->a, 0);

  // Get Pat
  Token t = parse_next(parser, &diagnostics);

  LnCol start = t.span.start;
  LnCol end;
  if (t.kind != tk_Pat) {
    *VEC_PUSH(&diagnostics, Diagnostic) = DIAGNOSTIC(DK_MatchCaseNoPat, t.span);
    start = t.span.end;
    end = t.span.end;
    goto CLEANUP;
  }

  // Get pattern
  mcep->pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(mcep->pattern, parser);

  end = mcep->pattern->span.end;

  // Expect colon
  t = parse_next(parser, &diagnostics);
  if (t.kind != tk_Arrow) {
    *VEC_PUSH(&diagnostics, Diagnostic) = DIAGNOSTIC(DK_MatchCaseNoArrow, t.span);
    resyncParser(parser, &diagnostics);
    goto CLEANUP;
  }

  // Get Value
  mcep->value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(mcep->value, parser);
  end = mcep->value->span.end;

CLEANUP:
  mcep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  mcep->diagnostics = vec_release(&diagnostics);

  mcep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  mcep->comments_len = VEC_LEN(&comments, Comment);
  mcep->comments = vec_release(&comments);

  return;
}

static void certain_parseMatchValueExpr(ValueExpr *mvep, Parser *parser) {
  ZERO(mvep);
  mvep->kind = VEK_Match;

  Vector diagnostics = vec_create(parser->a);

  Token t = parse_next(parser, &diagnostics);

  LnCol start = t.span.start;

  if (t.kind == tk_Label) {
    mvep->matchExpr.has_label = true;
    mvep->matchExpr.label = t.label;
    parse_next(parser, &diagnostics);
  } else {
    mvep->matchExpr.has_label = false;
  }

  assert(t.kind == tk_Match);

  // Get expression to match against
  mvep->matchExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(mvep->matchExpr.value, parser);
  // now we must parse the block containing the cases
  Vector cases = vec_create(parser->a);

  LnCol end;

  // Expect beginning brace
  t = parse_next(parser, &diagnostics);

  if (t.kind != tk_BraceLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&cases,                 // members_vec_ptr
             &diagnostics,           // diagnostics_vec_ptr
             parseMatchCaseExpr,     // member_parse_function
             struct MatchCaseExpr_s, // member_kind
             tk_BraceRight,          // delimiting_token_kind
             DK_MatchNoRightBrace,   // missing_delimiter_error
             end,                    // end_lncol
             parser                  // parser
  )

CLEANUP:
  // Get interior cases
  mvep->matchExpr.cases_len = VEC_LEN(&cases, struct MatchCaseExpr_s);
  mvep->matchExpr.cases = vec_release(&cases);

  mvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  mvep->diagnostics = vec_release(&diagnostics);
  mvep->span = SPAN(start, end);
  return;
}

static void parseReferenceValueExpr(ValueExpr *rvep, Parser *parser) {
  ZERO(rvep);
  rvep->kind = VEK_Reference;
  rvep->reference.path = ALLOC(parser->a, Path);
  parsePath(rvep->reference.path, parser);
  rvep->span = rvep->reference.path->span;
  rvep->diagnostics_len = 0;
  return;
}

// field = Value
static void parseValueStructMemberExpr(struct ValueStructMemberExpr_s *vsmep,
                                       Parser *parser) {
  // zero-initialize bp
  ZERO(vsmep);
  pushCommentScopeParser(parser);

  Token t;

  // diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start;
  LnCol end;

  // get identifier
  nextTokenParser(parser, &t);

  Span identitySpan = t.span;

  start = identitySpan.start;

  if (t.kind != tk_Identifier) {
    vsmep->name = NULL;
    vsmep->value = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  vsmep->name = t.identifier;

  // check if assign
  nextTokenParser(parser, &t);
  if (t.kind == tk_Colon) {
    // Get value of variable
    vsmep->value = ALLOC(parser->a, ValueExpr);
    parseValueExpr(vsmep->value, parser);
    end = vsmep->value->span.end;
  } else {
    diagnostic = DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    vsmep->value = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  vsmep->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    vsmep->diagnostics_len = 1;
    vsmep->diagnostics = ALLOC(parser->a, Diagnostic);
    vsmep->diagnostics[0] = diagnostic;
  } else {
    vsmep->diagnostics_len = 0;
    vsmep->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  vsmep->comments_len = VEC_LEN(&comments, Comment);
  vsmep->comments = vec_release(&comments);
  return;
}

static void certain_parseValueStructExpr(ValueExpr *sve, Parser *parser) {
  ZERO(sve);
  sve->kind = VEK_StructLiteral;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;

  Vector members = vec_create(parser->a);

  Vector diagnostics = vec_create(parser->a);

  LnCol end;

  nextTokenParser(parser, &t);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructLiteralExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             &diagnostics,                       // diagnostics_vec_ptr
             parseValueStructMemberExpr,         // member_parse_function
             struct ValueStructMemberExpr_s,     // member_kind
             tk_BraceRight,                      // delimiting_token_kind
             DK_StructLiteralExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )

CLEANUP:
  sve->structExpr.members_len =
      VEC_LEN(&members, struct ValueStructMemberExpr_s);
  sve->structExpr.members = vec_release(&members);
  sve->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  sve->diagnostics = vec_release(&diagnostics);
  sve->span = SPAN(start, end);
  return;
}

// Level1ValueExpr parentheses, braces, literals
// Level2ValueExpr as () [] & @ . -> (postfixes)
// Level3ValueExpr -- ++ ! (prefixes)
// Level4ValueExpr -> (pipeline)
// Level5ValueExpr * / % (multiplication and division)
// Level6ValueExpr + - (addition and subtraction)
// Level7ValueExpr < <= > >= == != (comparators)
// Level8ValueExpr && (logical and)
// Level9ValueExpr || (logical or)
// Level10ValueExpr , (create tuple)
// Level11ValueExpr = += -= *= /= %= (Assignment)

static void parseL1ValueExpr(ValueExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  peekTokenParser(parser, &t);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Int: {
    certain_parseIntValueExpr(l1, parser);
    break;
  }
  case tk_Bool: {
    certain_parseBoolValueExpr(l1, parser);
    break;
  }
  case tk_Float: {
    certain_parseFloatValueExpr(l1, parser);
    break;
  }
  case tk_Char: {
    certain_parseCharValueExpr(l1, parser);
    break;
  }
  case tk_Void: {
    certain_parseVoidValueExpr(l1, parser);
    break;
  }
  case tk_Builtin: {
    certain_parseBuiltinValueExpr(l1, parser);
    break;
  }
  case tk_String: {
    certain_parseStringValueExpr(l1, parser);
    break;
  }
  case tk_BraceLeft: {
    certain_parseBlockValueExpr(l1, parser);
    break;
  }
  case tk_Fn: {
    certain_parseFnValueExpr(l1, parser);
    break;
  }
  case tk_Struct: {
    certain_parseValueStructExpr(l1, parser);
    break;
  }
  case tk_Defer: {
    certain_parseDeferValueExpr(l1, parser);
    break;
  }
  case tk_Continue: {
    certain_parseContinueValueExpr(l1, parser);
    break;
  }
  case tk_Return: {
    certain_parseReturnValueExpr(l1, parser);
    break;
  }
  case tk_Loop: {
    certain_parseLoopValueExpr(l1, parser);
    break;
  }
  case tk_Match: {
    certain_parseMatchValueExpr(l1, parser);
    break;
  }
  case tk_Identifier: {
    parseReferenceValueExpr(l1, parser);
    break;
  }
  case tk_None: {
    // put the token error in the value expression.
    ZERO(l1);
    l1->kind = VEK_None;
    l1->span = t.span;
    l1->diagnostics_len = 1;
    l1->diagnostics = ALLOC(parser->a, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(t.error, t.span);
    // discard this token
    nextTokenParser(parser, &t);
    break;
  }
  default: {
    logInternalError(__LINE__, __func__, "unimplemented: %d at %d, %d", t.kind,
                     t.span.start.ln, t.span.start.col);
    PANIC();
  }
  }
  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseFieldAccessValueExpr(ValueExpr *fave, Parser *parser,
                                      ValueExpr *root) {
  ZERO(fave);
  fave->kind = VEK_FieldAccess;
  fave->fieldAccess.value = root;

  Token t;

  nextTokenParser(parser, &t);
  if (t.kind != tk_FieldAccess) {
    INTERNAL_ERROR("expected field access operator");
    PANIC();
  }

  // Now we get the field
  peekTokenParser(parser, &t);
  if (t.kind != tk_Identifier) {
    // it is possible we encounter an error
    fave->fieldAccess.field = NULL;
    fave->diagnostics_len = 1;
    fave->diagnostics = ALLOC(parser->a, Diagnostic);
    fave->diagnostics[0] = DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, t.span);
  } else {
    fave->fieldAccess.field = t.identifier;
    fave->diagnostics_len = 0;
  }

  fave->span = SPAN(root->span.start, t.span.end);
}

static void parseCallValueExpr(ValueExpr *cvep, Parser *parser,
                               ValueExpr *root) {
  ZERO(cvep);

  cvep->kind = VEK_Call;
  cvep->callExpr.function = root;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != tk_ParenLeft) {
    INTERNAL_ERROR("called call value expression parser where there was none");
    PANIC();
  }

  LnCol end;

  Vector parameters = vec_create(parser->a);
  Vector diagnostics = vec_create(parser->a);

  PARSE_LIST(&parameters,          // members_vec_ptr
             &diagnostics,         // diagnostics_vec_ptr
             parseValueExpr,       // member_parse_function
             ValueExpr,            // member_kind
             tk_ParenRight,        // delimiting_token_kind
             DK_CallExpectedParen, // missing_delimiter_error
             end,                  // end_lncol
             parser                // parser
  )

  cvep->callExpr.parameters_len = VEC_LEN(&parameters, ValueExpr);
  cvep->callExpr.parameters = vec_release(&parameters);

  cvep->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  cvep->diagnostics = vec_release(&diagnostics);
  cvep->span = SPAN(root->span.start, end);
}

static void parseAsValueExpr(ValueExpr *avep, Parser *parser, ValueExpr *root) {
  ZERO(avep);
  avep->kind = VEK_As;
  avep->asExpr.value = root;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != tk_As) {
    INTERNAL_ERROR("called as value expression parser where there was none");
    PANIC();
  }

  avep->asExpr.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(avep->asExpr.type, parser);
  avep->diagnostics_len = 0;
}

static void parseL2ValueExpr(ValueExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff

  ValueExpr *root = l2;
  parseL1ValueExpr(root, parser);

  while (true) {
    Token t;
    // represents the old operation
    ValueExpr *v;

    peekTokenParser(parser, &t);
    switch (t.kind) {
    case tk_Ref: {
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      pushCommentScopeParser(parser);
      root->kind = VEK_UnaryOp;
      root->unaryOp.operator= VEUOK_Ref;
      root->unaryOp.operand = v;
      root->span = SPAN(v->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case tk_Deref: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      root->kind = VEK_UnaryOp;
      root->unaryOp.operator= VEUOK_Deref;
      root->unaryOp.operand = v;
      root->span = SPAN(v->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case tk_FieldAccess: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      parseFieldAccessValueExpr(root, parser, v);
      break;
    }
    case tk_ParenLeft: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      parseCallValueExpr(root, parser, v);
      break;
    }
    case tk_As: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      parseAsValueExpr(root, parser, v);
      break;
    }
    default: {
      // there are no more level 2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    root->comments_len = VEC_LEN(&comments, Comment);
    root->comments = vec_release(&comments);
  }
}

static void parseL3ValueExpr(ValueExpr *l3, Parser *parser) {
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case tk_Negate: {
    l3->unaryOp.operator= VEUOK_Negate;
    break;
  }
  case tk_Posit: {
    l3->unaryOp.operator= VEUOK_Posit;
    break;
  }
  case tk_Not: {
    l3->unaryOp.operator= VEUOK_Not;
    break;
  }
  default: {
    // there is no level 3 expression
    parseL2ValueExpr(l3, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l3->kind = VEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  nextTokenParser(parser, &t);

  // Now parse the rest of the expression
  l3->unaryOp.operand = ALLOC(parser->a, ValueExpr);
  parseL3ValueExpr(l3->unaryOp.operand, parser);

  // finally calculate the misc stuff
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);
  l3->diagnostics_len = 0;

  // comments
  Vector comments = popCommentScopeParser(parser);
  l3->comments_len = VEC_LEN(&comments, Comment);
  l3->comments = vec_release(&comments);
}

// type is the type of object that the generated function will parse
// x is the index level of the function
// lower_fn is the name of the function that will be called to evaluate the left
// and right op_det_fn is the name of the function that determines the binary
// operator this function should take a pointer to the type and return a bool if
// successful
#define FN_BINOP_PARSE_LX_EXPR(type, type_shorthand, x, lower_fn)              \
  static void parseL##x##type(type *l##x, Parser *parser) {                    \
    type v;                                                                    \
    lower_fn(&v, parser);                                                      \
                                                                               \
    Token t;                                                                   \
    peekTokenParser(parser, &t);                                               \
    bool success = opDetL##x##type(t.kind, &l##x->binaryOp.operator);          \
    if (!success) {                                                            \
      /* there is no level x expression */                                     \
      *l##x = v;                                                               \
      return;                                                                  \
    }                                                                          \
    /* this will only execute if the operator exists */                        \
    l##x->kind = type_shorthand##_BinaryOp;                                    \
                                                                               \
    /* set the left side */                                                    \
    l##x->binaryOp.left_operand = ALLOC(parser->a, type);                    \
    *l##x->binaryOp.left_operand = v;                                          \
                                                                               \
    /* first create comment scope and go through operator */                   \
    pushCommentScopeParser(parser);                                            \
    nextTokenParser(parser, &t);                                               \
                                                                               \
    /* now parse the rest of the expression */                                 \
    l##x->binaryOp.right_operand = ALLOC(parser->a, type);                   \
    parseL##x##type(l##x->binaryOp.right_operand, parser);                     \
                                                                               \
    /* calculate misc stuff */                                                 \
    l##x->span = SPAN(l##x->binaryOp.left_operand->span.start,                 \
                      l##x->binaryOp.right_operand->span.end);                 \
    l##x->diagnostics_len = 0;                                                 \
                                                                               \
    /* comments */                                                             \
    Vector comments = popCommentScopeParser(parser);                           \
    l##x->comments_len = VEC_LEN(&comments, Comment);                          \
    l##x->comments = vec_release(&comments));                                  \
    return;                                                                    \
  }

static inline bool opDetL4ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Pipe: {
    *val = VEBOK_Pipeline;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 4, parseL3ValueExpr)

// Parses a single term that will not collide with patterns
static inline void parseValueExprTerm(ValueExpr *term, Parser *parser) {
  parseL4ValueExpr(term, parser);
}

static inline bool opDetL5ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Mul: {
    *val = VEBOK_Mul;
    return true;
  }
  case tk_Div: {
    *val = VEBOK_Div;
    return true;
  }
  case tk_Mod: {
    *val = VEBOK_Mod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 5, parseL4ValueExpr)

static inline bool opDetL6ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Add: {
    *val = VEBOK_Add;
    return true;
  }
  case tk_Sub: {
    *val = VEBOK_Sub;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 6, parseL5ValueExpr)

static inline bool opDetL7ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_CompLess: {
    *val = VEBOK_CompLess;
    return true;
  }
  case tk_CompGreater: {
    *val = VEBOK_CompGreater;
    return true;
  }
  case tk_CompLessEqual: {
    *val = VEBOK_CompLessEqual;
    return true;
  }
  case tk_CompGreaterEqual: {
    *val = VEBOK_CompGreaterEqual;
    return true;
  }
  case tk_CompEqual: {
    *val = VEBOK_CompEqual;
    return true;
  }
  case tk_CompNotEqual: {
    *val = VEBOK_CompNotEqual;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 7, parseL6ValueExpr)

static inline bool opDetL8ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_And: {
    *val = VEBOK_CompLess;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 8, parseL7ValueExpr)

static inline bool opDetL9ValueExpr(TokenKind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Or: {
    *val = VEBOK_CompGreater;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 9, parseL8ValueExpr)

static inline bool opDetL10ValueExpr(TokenKind tk,
                                     enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = VEBOK_Tuple;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 10, parseL9ValueExpr)

static bool opDetL11ValueExpr(TokenKind tk, enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Assign: {
    *val = VEBOK_Assign;
    return true;
  }
  case tk_AssignAdd: {
    *val = VEBOK_AssignAdd;
    return true;
  }
  case tk_AssignSub: {
    *val = VEBOK_AssignSub;
    return true;
  }
  case tk_AssignMul: {
    *val = VEBOK_AssignMul;
    return true;
  }
  case tk_AssignDiv: {
    *val = VEBOK_AssignDiv;
    return true;
  }
  case tk_AssignMod: {
    *val = VEBOK_AssignMod;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 11, parseL10ValueExpr)

// shim method
static void parseValueExpr(ValueExpr *vep, Parser *parser) {
  parseL11ValueExpr(vep, parser);
}

// field : Type,
static void parseTypeStructMemberExpr(struct TypeStructMemberExpr_s *tsmep,
                                      Parser *parser) {
  // zero-initialize bp
  ZERO(tsmep);
  pushCommentScopeParser(parser);

  Token t;

  // diagnostics
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  LnCol start;
  LnCol end;

  // get identifier
  nextTokenParser(parser, &t);

  Span identitySpan = t.span;

  start = identitySpan.start;

  if (t.kind != tk_Identifier) {
    tsmep->name = NULL;
    diagnostic = DIAGNOSTIC(DK_StructMemberExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  tsmep->name = t.identifier;

  // check if colon
  peekTokenParser(parser, &t);
  if (t.kind == tk_Colon) {
    // advance through colon
    nextTokenParser(parser, &t);
    // Get type of variable
    tsmep->type = ALLOC(parser->a, TypeExpr);
    parseTypeExpr(tsmep->type, parser);
    end = tsmep->type->span.end;
  } else {
    end = identitySpan.end;
    tsmep->type = ALLOC(parser->a, TypeExpr);
    tsmep->type->kind = TEK_Omitted;
    tsmep->type->span = identitySpan;
    tsmep->type->diagnostics_len = 0;
    tsmep->type->comments_len = 0;
  }

CLEANUP:
  tsmep->span = SPAN(start, end);

  if (diagnostic.kind != DK_Ok) {
    tsmep->diagnostics_len = 1;
    tsmep->diagnostics = ALLOC(parser->a, Diagnostic);
    tsmep->diagnostics[0] = diagnostic;
  } else {
    tsmep->diagnostics_len = 0;
    tsmep->diagnostics = NULL;
  }

  Vector comments = popCommentScopeParser(parser);
  tsmep->comments_len = VEC_LEN(&comments, Comment);
  tsmep->comments = vec_release(&comments);
  return;
}

static void certain_parseStructTypeExpr(TypeExpr *ste, Parser *parser) {
  ZERO(ste);
  ste->kind = TEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  switch (t.kind) {
  case tk_Struct: {
    ste->structExpr.kind = TSEK_Struct;
    break;
  }
  case tk_Enum: {
    ste->structExpr.kind = TSEK_Enum;
    break;
  }
  default: {
    INTERNAL_ERROR("called struct type expression parser where there was no "
                   "struct declaration");
    PANIC();
  }
  }

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  Vector diagnostics = vec_create(parser->a);

  nextTokenParser(parser, &t);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                      // members_vec_ptr
             &diagnostics,                  // diagnostics_vec_ptr
             parseTypeStructMemberExpr,     // member_parse_function
             struct TypeStructMemberExpr_s, // member_kind
             tk_BraceRight,                 // delimiting_token_kind
             DK_StructExpectedRightBrace,   // missing_delimiter_error
             end,                           // end_lncol
             parser                         // parser
  )

CLEANUP:
  ste->structExpr.members_len =
      VEC_LEN(&members, struct TypeStructMemberExpr_s);
  ste->structExpr.members = vec_release(&members);
  ste->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  ste->diagnostics = vec_release(&diagnostics);
  ste->span = SPAN(start, end);
  return;
}

static void parseReferenceTypeExpr(TypeExpr *rtep, Parser *parser) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = ALLOC(parser->a, Path);
  parsePath(rtep->referenceExpr.path, parser);
  rtep->diagnostics_len = 0;
  rtep->span = rtep->referenceExpr.path->span;
}

static void certain_parseVoidTypeExpr(TypeExpr *vte, Parser *parser) {
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == tk_Void);
  vte->kind = TEK_Void;
  vte->span = t.span;
  vte->diagnostics_len = 0;
  return;
}

static void certain_parseBuiltinTypeExpr(TypeExpr *btep, Parser *parser) {
  btep->kind = TEK_Builtin;
  btep->builtinExpr.builtin = ALLOC(parser->a, Builtin);
  certain_parseBuiltin(btep->builtinExpr.builtin, parser);
  btep->diagnostics_len = 0;
  btep->span = btep->builtinExpr.builtin->span;
}

static void parseFnTypeExpr(TypeExpr *fte, Parser *parser) {
  ZERO(fte);
  Token t;
  nextTokenParser(parser, &t);

  if (t.kind != tk_Fn) {
    INTERNAL_ERROR("called function type expression parser where there was no "
                   "function");
    PANIC();
  }

  LnCol start = t.span.start;
  LnCol end;

  Vector diagnostics = vec_create(parser->a);

  // check for leftparen
  nextTokenParser(parser, &t);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                     // members_vec_ptr
             &diagnostics,                    // diagnostics_vec_ptr
             parseTypeExpr,                   // member_parse_function
             TypeExpr,                        // member_kind
             tk_ParenRight,                   // delimiting_token_kind
             DK_FnTypeExprExpectedRightParen, // missing_delimiter_error
             end,                             // end_lncol
             parser                           // parser
  )

  fte->fnExpr.parameters_len = VEC_LEN(&parameters, TypeExpr);
  fte->fnExpr.parameters = vec_release(&parameters);

  return;
  fte->fnExpr.parameters = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(fte->fnExpr.parameters, parser);

  nextTokenParser(parser, &t);
  if (t.kind != tk_Colon) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedColon, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  fte->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  fte->diagnostics = vec_release(&diagnostics);
  fte->span = SPAN(start, end);
}

static void parseL1TypeExpr(TypeExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case tk_Identifier: {
    parseReferenceTypeExpr(l1, parser);
    break;
  }
  case tk_Builtin: {
    certain_parseBuiltinTypeExpr(l1, parser);
    break;
  }
  case tk_Enum:
  case tk_Struct: {
    certain_parseStructTypeExpr(l1, parser);
    break;
  }
  case tk_Void: {
    certain_parseVoidTypeExpr(l1, parser);
    break;
  }
  case tk_Fn: {
    parseFnTypeExpr(l1, parser);
    break;
  }
  default: {
    l1->kind = TEK_None;
    l1->span = t.span;
    l1->diagnostics_len = 1;
    l1->diagnostics = ALLOC(parser->a, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseScopeResolutionTypeExpr(TypeExpr *srte, Parser *parser,
                                         TypeExpr *root) {
  ZERO(srte);
  srte->kind = TEK_FieldAccess;
  srte->fieldAccess.value = root;

  Token t;

  nextTokenParser(parser, &t);
  if (t.kind != tk_FieldAccess) {
    INTERNAL_ERROR("expected field access operator");
    PANIC();
  }

  // Now we get the field
  peekTokenParser(parser, &t);
  if (t.kind != tk_Identifier) {
    // it is possible we encounter an error
    srte->fieldAccess.field = NULL;
    srte->diagnostics_len = 1;
    srte->diagnostics = ALLOC(parser->a, Diagnostic);
    srte->diagnostics[0] =
        DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, t.span);
  } else {
    srte->fieldAccess.field = t.identifier;
    srte->diagnostics_len = 0;
  }

  srte->span = SPAN(root->span.start, t.span.end);
}

static void parseL2TypeExpr(TypeExpr *l2, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr *root = l2;
  parseL1TypeExpr(root, parser);

  while (true) {
    Token t;
    // represents the new operation
    TypeExpr *ty;

    peekTokenParser(parser, &t);
    switch (t.kind) {
    case tk_Ref: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Ref;
      root->unaryOp.operand = ty;
      root->span = SPAN(ty->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case tk_Deref: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Deref;
      root->unaryOp.operand = ty;
      root->span = SPAN(ty->span.start, t.span.end);
      root->diagnostics_len = 0;
      nextTokenParser(parser, &t);
      break;
    }
    case tk_ScopeResolution: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      parseScopeResolutionTypeExpr(root, parser, ty);
      break;
    }
    default: {
      // there are no more level2 expressions
      return;
    }
    }

    Vector comments = popCommentScopeParser(parser);
    root->comments_len = VEC_LEN(&comments, Comment);
    root->comments = vec_release(&comments);
  }
}

static inline bool opDetL3TypeExpr(TokenKind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = TEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 3 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 3, parseL2TypeExpr)

static inline bool opDetL4TypeExpr(TokenKind tk,
                                   enum TypeExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Union: {
    *val = TEBOK_Union;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(TypeExpr, TEK, 4, parseL3TypeExpr)

static void parseTypeExpr(TypeExpr *tep, Parser *parser) {
  parseL4TypeExpr(tep, parser);
}

static void certain_parseValueRestrictionPatternExpr(PatternExpr *vrpe,
                                                     Parser *parser) {
  ZERO(vrpe);

  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.start;

  vrpe->kind = PEK_ValueRestriction;

  switch (t.kind) {
  case tk_CompEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompEqual;
    break;
  }
  case tk_CompNotEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompNotEqual;
    break;
  }
  case tk_CompGreaterEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompGreaterEqual;
    break;
  }
  case tk_CompGreater: {
    vrpe->valueRestriction.restriction = PEVRK_CompGreater;
    break;
  }
  case tk_CompLess: {
    vrpe->valueRestriction.restriction = PEVRK_CompLess;
    break;
  }
  case tk_CompLessEqual: {
    vrpe->valueRestriction.restriction = PEVRK_CompLessEqual;
    break;
  }
  default: {
    INTERNAL_ERROR("called value restrict pattern expr parser where there was "
                   "no value restrict pattern");
    PANIC();
  }
  }
  vrpe->valueRestriction.valueExpr = ALLOC(parser->a, ValueExpr);
  parseValueExprTerm(vrpe->valueRestriction.valueExpr, parser);
  LnCol end = vrpe->valueRestriction.valueExpr->span.end;

  vrpe->span = SPAN(start, end);
  vrpe->diagnostics_len = 0;
  return;
}

static void certain_parseTypeRestrictionPatternExpr(PatternExpr *trpe,
                                                    Parser *parser) {
  ZERO(trpe);

  trpe->kind = PEK_TypeRestriction;

  bool parseType = false;

  Token t;
  nextTokenParser(parser, &t);

  LnCol start = t.span.start;
  LnCol end;

  switch (t.kind) {
  // No binding created
  case tk_Colon: {
    trpe->typeRestriction.has_binding = false;
    parseType = true;
    end = t.span.end;
    break;
  }
  // Create a binding
  case tk_Identifier: {
    trpe->typeRestriction.has_binding = true;
    trpe->typeRestriction.binding = t.identifier;
    end = t.span.end;
    peekTokenParser(parser, &t);
    if (t.kind == tk_Colon) {
      parseType = true;
      // advance through it
      nextTokenParser(parser, &t);
    } else {
      parseType = false;
    }
    break;
  }
  default: {
    INTERNAL_ERROR("called type restrict pattern expr parser where there was "
                   "no type restrict pattern");
    PANIC();
  }
  }

  if (parseType) {
    trpe->typeRestriction.type = ALLOC(parser->a, TypeExpr);
    parseTypeExpr(trpe->typeRestriction.type, parser);
    end = t.span.end;
  } else {
    trpe->typeRestriction.type = ALLOC(parser->a, TypeExpr);
    trpe->typeRestriction.type->kind = TEK_Omitted;
    trpe->typeRestriction.type->span = SPAN(start, end);
    trpe->typeRestriction.type->diagnostics_len = 0;
    trpe->typeRestriction.type->comments_len = 0;
  }

  trpe->span = SPAN(start, end);
  trpe->diagnostics_len = 0;
}

static void
parsePatternStructMemberExpr(struct PatternStructMemberExpr_s *psmep,
                             Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(psmep);
  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;
  Token t;
  nextTokenParser(parser, &t);
  LnCol start = t.span.end;
  LnCol end;
  switch (t.kind) {
  case tk_Rest: {
    psmep->kind = PSMEK_Rest;
    break;
  }
  case tk_Identifier: {
    // copy identifier
    psmep->kind = PSMEK_Field;
    psmep->field.field = t.identifier;
    break;
  }
  default: {
    diagnostic = DIAGNOSTIC(DK_PatternStructExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }
  }

  // test if the statement has an assign
  // The assignment is only omitted if it is a value restriction
  bool has_assign;

  peekTokenParser(parser, &t);
  if (t.kind == tk_Assign) {
    has_assign = true;
    nextTokenParser(parser, &t);
  } else {
    has_assign = false;
  }
  psmep->pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(psmep->pattern, parser);

  end = psmep->pattern->span.end;

  if (psmep->pattern->kind == PEK_ValueRestriction && has_assign) {
    diagnostic =
        DIAGNOSTIC(DK_PatternStructUnexpectedAssignForValueRestriction, t.span);
    goto CLEANUP;
  } else if (psmep->pattern->kind != PEK_ValueRestriction && !has_assign) {
    diagnostic = DIAGNOSTIC(
        DK_PatternStructExpectedAssignForNonValueRestriction, t.span);
    goto CLEANUP;
  }

CLEANUP:
  if (diagnostic.kind != DK_Ok) {
    psmep->diagnostics_len = 1;
    psmep->diagnostics = ALLOC(parser->a, Diagnostic);
    psmep->diagnostics[0] = diagnostic;
  } else {
    psmep->diagnostics_len = 0;
  }

  psmep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  psmep->comments_len = VEC_LEN(&comments, Comment);
  psmep->comments = vec_release(&comments);
  return;
}

static void certain_parseStructPatternExpr(PatternExpr *spe, Parser *parser) {
  ZERO(spe);
  spe->kind = PEK_Struct;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  Vector diagnostics = vec_create(parser->a);

  nextTokenParser(parser, &t);
  if (t.kind != tk_BraceLeft) {
    *VEC_PUSH(&diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             &diagnostics,                       // diagnostics_vec_ptr
             parsePatternStructMemberExpr,       // member_parse_function
             struct PatternStructMemberExpr_s,   // member_kind
             tk_BraceRight,                      // delimiting_token_kind
             DK_PatternStructExpectedRightBrace, // missing_delimiter_error
             end,                                // end_lncol
             parser                              // parser
  )
CLEANUP:
  spe->structExpr.members_len =
      VEC_LEN(&members, struct PatternStructMemberExpr_s);
  spe->structExpr.members = vec_release(&members);
  spe->diagnostics_len = VEC_LEN(&diagnostics, Diagnostic);
  spe->diagnostics = vec_release(&diagnostics);
  spe->span = SPAN(start, end);
  return;
}

static void certain_parseGroupPatternExpr(PatternExpr *gpep, Parser *parser) {
  ZERO(gpep);
  gpep->kind = PEK_Group;
  Token t;
  nextTokenParser(parser, &t);
  assert(t.kind == tk_BraceLeft);
  LnCol start = t.span.start;
  LnCol end;

  gpep->groupExpr.value = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(gpep->groupExpr.value, parser);

  Diagnostic diagnostic;
  diagnostic.kind = DK_Ok;

  nextTokenParser(parser, &t);
  if (t.kind != tk_BraceRight) {
    diagnostic = DIAGNOSTIC(DK_PatternGroupExpectedRightBrace, t.span);
    end = t.span.end;
  } else {
    end = gpep->groupExpr.value->span.end;
  }

  if (diagnostic.kind == DK_Ok) {
    gpep->diagnostics_len = 0;
  } else {
    gpep->diagnostics_len = 1;
    gpep->diagnostics = ALLOC(parser->a, Diagnostic);
    *gpep->diagnostics = diagnostic;
  }
  gpep->span = SPAN(start, end);
}

static void parseL1PatternExpr(PatternExpr *l1, Parser *parser) {
  pushCommentScopeParser(parser);
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case tk_BraceLeft: {
    certain_parseGroupPatternExpr(l1, parser);
    break;
  }
  case tk_Struct: {
    certain_parseStructPatternExpr(l1, parser);
    break;
  }
  case tk_Identifier:
  case tk_Colon: {
    certain_parseTypeRestrictionPatternExpr(l1, parser);
    break;
  }
  case tk_CompEqual:
  case tk_CompNotEqual:
  case tk_CompGreaterEqual:
  case tk_CompGreater:
  case tk_CompLess:
  case tk_CompLessEqual: {
    certain_parseValueRestrictionPatternExpr(l1, parser);
    break;
  }
  default: {
    l1->kind = PEK_None;
    l1->span = t.span;
    l1->diagnostics_len = 1;
    l1->diagnostics = ALLOC(parser->a, Diagnostic);
    l1->diagnostics[0] = DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseL2PatternExpr(PatternExpr *l2, Parser *parser) {
  Token t;
  peekTokenParser(parser, &t);
  switch (t.kind) {
  case tk_Not: {
    l2->unaryOp.operator= PEUOK_Not;
    break;
  }
  default: {
    // there is no level expression
    parseL1PatternExpr(l2, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l2->kind = PEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  nextTokenParser(parser, &t);

  // Now parse the rest of the expression
  l2->unaryOp.operand = ALLOC(parser->a, PatternExpr);
  parseL2PatternExpr(l2->unaryOp.operand, parser);

  // finally calculate the misc stuff
  l2->span = SPAN(t.span.start, l2->unaryOp.operand->span.end);
  l2->diagnostics_len = 0;

  // comments
  Vector comments = popCommentScopeParser(parser);
  l2->comments_len = VEC_LEN(&comments, Comment);
  l2->comments = vec_release(&comments);
}

static inline bool opDetL3PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Tuple: {
    *val = PEBOK_Tuple;
    return true;
  }
  default: {
    // there is no level 4 expression
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 3, parseL2PatternExpr)

static inline bool opDetL4PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Union: {
    *val = PEBOK_Union;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 4, parseL3PatternExpr)

static inline bool opDetL5PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_And: {
    *val = PEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 5, parseL4PatternExpr)

static inline bool opDetL6PatternExpr(TokenKind tk,
                                      enum PatternExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Or: {
    *val = PEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}

FN_BINOP_PARSE_LX_EXPR(PatternExpr, PEK, 6, parseL5PatternExpr)

static void parsePatternExpr(PatternExpr *ppe, Parser *parser) {
  parseL6PatternExpr(ppe, parser);
}

static void parseValDecl(Stmnt *vdsp, Parser *parser) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = SK_ValDecl;
  // these variables will be reused
  Token t;

  // Get Binding
  vdsp->valDecl.pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(vdsp->valDecl.pattern, parser);

  LnCol start = vdsp->valDecl.pattern->span.start;
  LnCol end;

  // Expect Equal Sign
  peekTokenParser(parser, &t);
  if (t.kind == tk_Assign) {
    // accept the equal sign
    nextTokenParser(parser, &t);

    vdsp->valDecl.has_value = true;
    vdsp->valDecl.value = ALLOC(parser->a, ValueExpr);
    parseValueExpr(vdsp->valDecl.value, parser);
    end = vdsp->valDecl.value->span.end;
  } else {
    vdsp->valDecl.has_value = false;
    end = vdsp->valDecl.pattern->span.end;
  }

  vdsp->span = SPAN(start, end);
  vdsp->diagnostics_len = 0;
}

static void parseTypeDecl(Stmnt *tdp, Parser *parser) {
  ZERO(tdp);
  tdp->kind = SK_TypeDecl;
  Token t;

  LnCol end;

  nextTokenParser(parser, &t);

  LnCol start = t.span.start;

  if (t.kind != tk_Identifier) {
    tdp->typeDecl.name = NULL;
    tdp->diagnostics = ALLOC(parser->a, Diagnostic);
    tdp->diagnostics[0] = DIAGNOSTIC(DK_TypeDeclExpectedIdentifier, t.span);
    tdp->diagnostics_len = 1;
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.name = t.identifier;

  // Now get equals sign
  nextTokenParser(parser, &t);
  if (t.kind != tk_Assign) {
    tdp->diagnostics = ALLOC(parser->a, Diagnostic);
    tdp->diagnostics[0] = DIAGNOSTIC(DK_TypeDeclExpectedAssign, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(tdp->typeDecl.type, parser);
  end = tdp->typeDecl.type->span.end;
  tdp->diagnostics_len = 0;
  tdp->diagnostics = NULL;

CLEANUP:
  tdp->span = SPAN(start, end);
  return;
}

static void parsePatternExprStmnt(Stmnt *pesp, Parser *parser) {
  ZERO(pesp);
  pesp->kind = SK_PatExpr;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != tk_Pat) {
    INTERNAL_ERROR(
        "called pat expr stmnt parser where there was no pat expr stmnt");
    PANIC();
  }
  LnCol start = t.span.start;

  pesp->patExpr.pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(pesp->patExpr.pattern, parser);
  pesp->span = SPAN(start, pesp->patExpr.pattern->span.end);
  pesp->diagnostics_len = 0;
  return;
}

static void parseTypeExprStmnt(Stmnt *tesp, Parser *parser) {
  ZERO(tesp);
  tesp->kind = SK_TypeExpr;

  Token t;
  nextTokenParser(parser, &t);
  if (t.kind != tk_Type) {
    INTERNAL_ERROR(
        "called type expr stmnt parser where there was no type expr stmnt");
    PANIC();
  }
  LnCol start = t.span.start;

  tesp->typeExpr.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(tesp->typeExpr.type, parser);
  tesp->span = SPAN(start, tesp->typeExpr.type->span.end);
  tesp->diagnostics_len = 0;
  return;
}

static void parseStmnt(Stmnt *sp, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  // peek next token
  peekTokenParser(parser, &t);
  switch (t.kind) {
    // Macros
  case tk_Macro: {
    sp->kind = SK_Macro;
    sp->macroStmnt.name = t.macro_call;
    sp->diagnostics_len = 0;
    sp->span = t.span;
    break;
  }
  case tk_Use: {
    LnCol start = t.span.start;
    nextTokenParser(parser, &t); // drop use token
    sp->kind = SK_Use;
    sp->useStmnt.path = ALLOC(parser->a, Path);
    parsePath(sp->useStmnt.path, parser);
    sp->span = SPAN(start, sp->useStmnt.path->span.end);
    sp->diagnostics_len = 0;
    break;
  }
  case tk_Namespace: {
    LnCol start = t.span.start;
    nextTokenParser(parser, &t); // drop namespace token
    sp->kind = SK_Namespace;
    sp->namespaceStmnt.path = ALLOC(parser->a, Path);
    parsePath(sp->namespaceStmnt.path, parser);
    sp->namespaceStmnt.stmnt = ALLOC(parser->a, Stmnt);
    parseStmnt(sp->namespaceStmnt.stmnt, parser);
    sp->span = SPAN(start, sp->namespaceStmnt.stmnt->span.end);
    sp->diagnostics_len = 0;
    break;
  }
  // Let Stmnt (Decl)
  case tk_Let: {
    LnCol start = t.span.start;
    nextTokenParser(parser, &t);
    peekTokenParser(parser, &t);
    switch (t.kind) {
    case tk_Type: {
      nextTokenParser(parser, &t);
      parseTypeDecl(sp, parser);
      break;
    }
    default: {
      parseValDecl(sp, parser);
      break;
    }
    }
    sp->span = SPAN(start, sp->span.end);
    break;
  }
  // Expressions
  case tk_Type: {
    parseTypeExprStmnt(sp, parser);
    break;
  }
  case tk_Pat: {
    parsePatternExprStmnt(sp, parser);
    break;
  }
  default: {
    // Value Expr Statement
    sp->kind = SK_ValExpr;
    sp->valExpr.value = ALLOC(parser->a, ValueExpr);
    parseValueExpr(sp->valExpr.value, parser);
    sp->span = sp->valExpr.value->span;
    sp->diagnostics_len = 0;
    break;
  }
  }
  Vector comments = popCommentScopeParser(parser);
  sp->comments_len = VEC_LEN(&comments, Comment);
  sp->comments = vec_release(&comments);
}

static void parseTranslationUnit(TranslationUnit *tu, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t;
  Vector statements = vec_create(parser->a);

  LnCol end;

  while (true) {
    peekTokenParser(parser, &t);
    if (t.kind == tk_None && t.error == DK_EOF) {
      end = t.span.end;
      break;
    }
    /* if there wasn't an end delimiter, push the last token back */
    parseStmnt(VEC_PUSH(&statements, Stmnt), parser);
  }

  tu->statements_len = VEC_LEN(&statements, Stmnt);
  tu->statements = vec_release(&statements);
  tu->span = SPAN(LNCOL(0, 0), end);
  tu->diagnostics_len = 0;

  Vector comments = popCommentScopeParser(parser);
  tu->comments_len = VEC_LEN(&comments, Comment);
  tu->comments = vec_release(&comments);
  return;
}

void parseTranslationUnitParser(Parser *pp, TranslationUnit *tu) {
  parseTranslationUnit(tu, pp);
}
