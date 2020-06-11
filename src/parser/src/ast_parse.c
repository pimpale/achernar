#include "ast_parse.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "constants.h"
#include "json.h"
#include "lex.h"
#include "queue.h"
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
    member_parse_function(VEC_PUSH(members_vec_ptr, member_kind), diagnostics, \
                          parser);                                             \
  }

// Parser
Parser parse_create(Lexer *lp, Allocator *a) {
  return (Parser){
      .a = a,      // Allocator
      .lexer = lp, // Lexer Pointer
      .next_tokens_queue =
          queue_create(vec_create(a)), // Queue of peeked tokens
      .next_diagnostics_queue =
          queue_create(vec_create(a)), // Queue<Vector<Comment>>
      .next_comments_queue =
          queue_create(vec_create(a)), // Queue<Vector<Comment>>
      .comments = vec_create(a),       // Vector<Comment>
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
//    Return the first element of the top of the token
//    Pop the first element of the next_comments_stack
//    For each element in the next comments stack, push it to the top of the
//    current scope
// Else fetch next raw token
static Token parse_next(Parser *pp, Vector *diagnostics) {
  // the current scope we aim to push the comments to
  size_t top_index = VEC_LEN(&pp->comments, Vector);
  assert(top_index > 0);
  Vector *current_scope = VEC_GET(&pp->comments, top_index - 1, Vector);
  if (QUEUE_LEN(&pp->next_tokens_queue, Token) > 0) {
    // we want to merge together the next token's diagnostics and comments into
    // the provided ones

    // Vector containing all comments for the next token
    // pop a vector off the stack and assign it to next_token_comments
    Vector next_token_comments;
    QUEUE_POP(&pp->next_comments_queue, &next_token_comments, Vector);

    // append next_token_comments to the current scope
    vec_append(current_scope, &next_token_comments);

    // Vector containing all diagnostics for the next
    Vector next_token_diagnostics;
    QUEUE_POP(&pp->next_diagnostics_queue, &next_token_diagnostics, Vector);

    // append next_token_diagnostics to the diagnostics vector
    vec_append(diagnostics, &next_token_diagnostics);

    // set the token
    Token ret;
    QUEUE_POP(&pp->next_tokens_queue, &ret, Token);
    return ret;
  } else {
    return parse_rawNext(pp, current_scope, diagnostics);
  }
}

// gets the k'th token
// K must be greater than 0
static Token parse_peekNth(Parser *pp, size_t k) {
  assert(k > 0);

  for (size_t i = QUEUE_LEN(&pp->next_tokens_queue, Token); i < k; i++) {
    // Create vector to store any comments
    Vector *next_token_comments = QUEUE_PUSH(&pp->next_comments_queue, Vector);
    *next_token_comments = vec_create(pp->a);

    // Create vector to store any diagnostics
    Vector *next_token_diagnostics =
        QUEUE_PUSH(&pp->next_diagnostics_queue, Vector);
    *next_token_diagnostics = vec_create(pp->a);

    // parse the token and add it to the top of the stack
    *QUEUE_PUSH(&pp->next_tokens_queue, Token) =
        parse_rawNext(pp, next_token_comments, next_token_diagnostics);
  }
  // return the most recent token added
  return *QUEUE_GET(&pp->next_tokens_queue, 0, Token);
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
  *VEC_PUSH(&parser->comments, Vector) = vec_create(parser->a);
}

Allocator *parse_release(Parser *pp) {
  while (QUEUE_LEN(&pp->next_comments_queue, Vector) != 0) {
    Vector comments;
    QUEUE_POP(&pp->next_comments_queue, &comments, Vector);
    vec_destroy(&comments);

    Vector diagnostics;
    QUEUE_POP(&pp->next_diagnostics_queue, &diagnostics, Vector);
    vec_destroy(&diagnostics);
  }
  queue_destroy(&pp->next_diagnostics_queue);
  queue_destroy(&pp->next_comments_queue);
  queue_destroy(&pp->next_tokens_queue);

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
  Vector comments = vec_create(parser->a);
  int64_t initial_paren_depth = parser->paren_depth;
  int64_t initial_brace_depth = parser->brace_depth;
  int64_t initial_bracket_depth = parser->bracket_depth;
  while (true) {
    if (initial_brace_depth > parser->brace_depth &&
        initial_paren_depth > parser->paren_depth &&
        initial_bracket_depth > parser->bracket_depth) {
      break;
    }
    Token t = parse_rawNext(parser, &comments, diagnostics);
    if (t.kind == tk_Eof) {
      break;
    }
  }
  vec_destroy(&comments);
}

// Note that all errors resync at the statement level
static void parseStmnt(Stmnt *stmnt, Vector *diagnostics, Parser *parser);
static void parseValueExpr(ValueExpr *vep, Vector *diagnostics, Parser *parser);
static void parseTypeExpr(TypeExpr *tep, Vector *diagnostics, Parser *parser);
static void parsePatternExpr(PatternExpr *pp, Vector *diagnostics,
                             Parser *parser);

static void certain_parseBuiltin(Builtin *bp, Vector *diagnostics,
                                 Parser *parser) {
  ZERO(bp);

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Builtin);

  LnCol start = t.span.start;
  LnCol end;

  bp->name = t.builtin;

  Vector parameters = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_BuiltinExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&parameters,                  // members_vec_ptr
             diagnostics,                  // diagnostics_vec_ptr
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

  bp->span = SPAN(start, end);
}

static void parsePath(Path *pp, Vector *diagnostics, Parser *parser) {
  // start comment scope
  pushCommentScopeParser(parser);

  Token t = parse_next(parser, diagnostics);
  // start and finish
  LnCol start = t.span.start;
  LnCol end;

  Vector pathSegments = vec_create(parser->a);

  // ensure that it is a valid path
  if (t.kind != tk_Identifier) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PathExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  *VEC_PUSH(&pathSegments, char *) = t.identifier;

  while (true) {
    t = parse_peek(parser);
    if (t.kind == tk_ScopeResolution) {
      // discard the scope resolution
      parse_next(parser, diagnostics);
      // now check if we have an issue
      t = parse_next(parser, diagnostics);
      if (t.kind != tk_Identifier) {
        *VEC_PUSH(diagnostics, Diagnostic) =
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

  pp->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  pp->comments_len = VEC_LEN(&comments, Comment);
  pp->comments = vec_release(&comments);
}

static void certain_parseNilValueExpr(ValueExpr *vep, Vector *diagnostics,
                                      Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Nil);
  vep->kind = VEK_NilLiteral;
  vep->span = t.span;
  return;
}

static void certain_parseIntValueExpr(ValueExpr *vep, Vector *diagnostics,
                                      Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Int);
  vep->kind = VEK_IntLiteral;
  vep->intLiteral.value = t.int_literal;
  vep->span = t.span;
  return;
}

static void certain_parseBoolValueExpr(ValueExpr *vep, Vector *diagnostics,
                                       Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Bool);
  vep->kind = VEK_BoolLiteral;
  vep->boolLiteral.value = t.bool_literal;
  vep->span = t.span;
  return;
}

static void certain_parseFloatValueExpr(ValueExpr *vep, Vector *diagnostics,
                                        Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Float);
  vep->kind = VEK_FloatLiteral;
  vep->floatLiteral.value = t.float_literal;
  vep->span = t.span;
  return;
}

static void certain_parseCharValueExpr(ValueExpr *vep, Vector *diagnostics,
                                       Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Char);
  vep->kind = VEK_CharLiteral;
  vep->charLiteral.value = t.char_literal;
  vep->span = t.span;
  return;
}

static void certain_parseStringValueExpr(ValueExpr *svep, Vector *diagnostics,
                                         Parser *parser) {
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_String);
  svep->kind = VEK_StringLiteral;
  svep->stringLiteral.value = t.string_literal;
  svep->span = t.span;
  return;
}

static void certain_parseFnValueExpr(ValueExpr *fvep, Vector *diagnostics,
                                     Parser *parser) {
  ZERO(fvep);

  fvep->kind = VEK_Fn;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Fn);
  LnCol start = t.span.start;
  LnCol end = t.span.end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValueExprExpectedLeftParen, t.span);
    goto CLEANUP;
  }

  Span lparenspan = t.span;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                      // members_vec_ptr
             diagnostics,                      // diagnostics_vec_ptr
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
    parse_next(parser, diagnostics);

    parseTypeExpr(fvep->fnExpr.type, diagnostics, parser);
  } else {
    fvep->fnExpr.type = ALLOC(parser->a, TypeExpr);
    fvep->fnExpr.type->kind = TEK_Omitted;
    fvep->fnExpr.type->span = lparenspan;
    fvep->fnExpr.type->comments_len = 0;
  }

  t = parse_next(parser, diagnostics);

  if (t.kind != tk_Arrow) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnValueExprExpectedArrow, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  fvep->fnExpr.body = ALLOC(parser->a, ValueExpr);
  parseValueExpr(fvep->fnExpr.body, diagnostics, parser);
  end = fvep->fnExpr.body->span.end;

CLEANUP:
  fvep->span = SPAN(start, end);
}

static void certain_parseBlockValueExpr(ValueExpr *bvep, Vector *diagnostics,
                                        Parser *parser) {
  ZERO(bvep);
  bvep->kind = VEK_Block;

  // Parse leftbrace
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  LnCol start = t.span.start;

  t = parse_peek(parser);
  // blocks may be labeled
  if (t.kind == tk_Label) {
    bvep->blockExpr.has_label = true;
    bvep->blockExpr.label = t.label;
    // accept label
    parse_next(parser, diagnostics);
  } else {
    bvep->blockExpr.has_label = false;
  }

  // Create list of statements
  Vector statements = vec_create(parser->a);

  LnCol end;

  PARSE_LIST(&statements,                // members_vec_ptr
             diagnostics,                // diagnostics_vec_ptr
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
  return;
}
static void certain_parseReturnValueExpr(ValueExpr *rep, Vector *diagnostics,
                                         Parser *parser) {
  ZERO(rep);
  rep->kind = VEK_Return;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Return);

  LnCol start = t.span.start;
  LnCol end;

  parse_next(parser, diagnostics);

  if (t.kind != tk_Label) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_ReturnExpectedLabel, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  rep->returnExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(rep->returnExpr.value, diagnostics, parser);
  end = rep->returnExpr.value->span.end;

CLEANUP:
  rep->span = SPAN(start, end);
  return;
}

static void certain_parseContinueValueExpr(ValueExpr *cep, Vector *diagnostics,
                                           Parser *parser) {
  ZERO(cep);
  cep->kind = VEK_Continue;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Continue);

  LnCol start = t.span.start;
  LnCol end;

  parse_next(parser, diagnostics);
  end = t.span.end;

  // TODO from here

  if (t.kind != tk_Label) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_ContinueExpectedLabel, t.span);
    goto CLEANUP;
  }

CLEANUP:
  cep->span = SPAN(start, end);
  return;
}

static void certain_parseLoopValueExpr(ValueExpr *lep, Vector *diagnostics,
                                       Parser *parser) {
  lep->kind = VEK_Loop;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Loop);
  LnCol start = t.span.start;

  t = parse_peek(parser);
  if (t.kind == tk_Label) {
    lep->loopExpr.has_label = true;
    lep->loopExpr.label = t.label;
    // accept label
    parse_next(parser, diagnostics);
  } else {
    lep->loopExpr.has_label = false;
  }

  lep->loopExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(lep->loopExpr.value, diagnostics, parser);
  lep->span = SPAN(start, lep->loopExpr.value->span.end);
  return;
}

// Pattern : Expr,
static void parseMatchCaseExpr(struct MatchCaseExpr_s *mcep,
                               Vector *diagnostics, Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(mcep);

  // Get Pat
  Token t = parse_next(parser, diagnostics);

  LnCol start = t.span.start;
  LnCol end;
  if (t.kind != tk_Pat) {
    *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_MatchCaseNoPat, t.span);
    start = t.span.end;
    end = t.span.end;
    goto CLEANUP;
  }

  // Get pattern
  mcep->pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(mcep->pattern, diagnostics, parser);

  end = mcep->pattern->span.end;

  // Expect colon
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Arrow) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_MatchCaseNoArrow, t.span);
    resyncParser(parser, diagnostics);
    goto CLEANUP;
  }

  // Get Value
  mcep->value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(mcep->value, diagnostics, parser);
  end = mcep->value->span.end;

CLEANUP:
  mcep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  mcep->comments_len = VEC_LEN(&comments, Comment);
  mcep->comments = vec_release(&comments);

  return;
}

static void certain_parseMatchValueExpr(ValueExpr *mvep, Vector *diagnostics,
                                        Parser *parser) {
  ZERO(mvep);
  mvep->kind = VEK_Match;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Match);
  LnCol start = t.span.start;

  if (t.kind == tk_Label) {
    mvep->matchExpr.has_label = true;
    mvep->matchExpr.label = t.label;
    // accept label
    parse_next(parser, diagnostics);
  } else {
    mvep->matchExpr.has_label = false;
  }

  // Get expression to match against
  mvep->matchExpr.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(mvep->matchExpr.value, diagnostics, parser);
  // now we must parse the block containing the cases
  Vector cases = vec_create(parser->a);

  LnCol end;

  // Expect beginning brace
  t = parse_next(parser, diagnostics);

  if (t.kind != tk_BraceLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_MatchNoLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&cases,                 // members_vec_ptr
             diagnostics,            // diagnostics_vec_ptr
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

  mvep->span = SPAN(start, end);
  return;
}

static void parseReferenceValueExpr(ValueExpr *rvep, Vector *diagnostics,
                                    Parser *parser) {
  ZERO(rvep);
  rvep->kind = VEK_Reference;
  rvep->reference.path = ALLOC(parser->a, Path);
  parsePath(rvep->reference.path, diagnostics, parser);
  rvep->span = rvep->reference.path->span;
  return;
}

// field = Value
static void parseValueStructMemberExpr(struct ValueStructMemberExpr_s *vsmep,
                                       Vector *diagnostics, Parser *parser) {
  // zero-initialize bp
  ZERO(vsmep);
  pushCommentScopeParser(parser);

  LnCol start;
  LnCol end;

  // get identifier
  Token t = parse_next(parser, diagnostics);

  Span identitySpan = t.span;

  start = identitySpan.start;

  if (t.kind != tk_Identifier) {
    vsmep->name = NULL;
    vsmep->value = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructMemberLiteralExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  vsmep->name = t.identifier;

  // check if define
  t = parse_next(parser, diagnostics);
  if (t.kind == tk_Define) {
    // Get value of variable
    vsmep->value = ALLOC(parser->a, ValueExpr);
    parseValueExpr(vsmep->value, diagnostics, parser);
    end = vsmep->value->span.end;
  } else {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructMemberLiteralExpectedDefine, t.span);
    vsmep->value = NULL;
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  vsmep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  vsmep->comments_len = VEC_LEN(&comments, Comment);
  vsmep->comments = vec_release(&comments);
  return;
}

static void certain_parseValueStructExpr(ValueExpr *sve, Vector *diagnostics,
                                         Parser *parser) {
  ZERO(sve);
  sve->kind = VEK_StructLiteral;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructLiteralExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             diagnostics,                        // diagnostics_vec_ptr
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

static void parseL1ValueExpr(ValueExpr *l1, Vector *diagnostics,
                             Parser *parser) {
  pushCommentScopeParser(parser);

  Token t = parse_peek(parser);
  // Decide which expression it is
  switch (t.kind) {
  // Literals
  case tk_Int: {
    certain_parseIntValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Bool: {
    certain_parseBoolValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Float: {
    certain_parseFloatValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Char: {
    certain_parseCharValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    certain_parseNilValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_String: {
    certain_parseStringValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_BraceLeft: {
    certain_parseBlockValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    certain_parseFnValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    certain_parseValueStructExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Continue: {
    certain_parseContinueValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Return: {
    certain_parseReturnValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Loop: {
    certain_parseLoopValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Match: {
    certain_parseMatchValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier: {
    parseReferenceValueExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Eof: {
    // throw error for whatever was wrong with the piece
    l1->kind = VEK_None;
    l1->span = t.span;
    *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_EOF, t.span);
    break;
  }
  case tk_None: {
    // throw error for whatever was wrong with the piece
    ZERO(l1);
    l1->kind = VEK_None;
    l1->span = t.span;
    parse_next(parser, diagnostics);
    break;
  }
  default: {
    ZERO(l1);
    l1->kind = VEK_None;
    l1->span = t.span;
    parse_next(parser, diagnostics);
    *VEC_PUSH(diagnostics, Diagnostic) = DIAGNOSTIC(DK_UnexpectedToken, t.span);
  }
  }
  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseFieldAccessValueExpr(ValueExpr *fave, Vector *diagnostics,
                                      Parser *parser, ValueExpr *root) {
  ZERO(fave);
  fave->kind = VEK_FieldAccess;
  fave->fieldAccess.value = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_FieldAccess);

  // Now we get the field
  t = parse_peek(parser);
  if (t.kind != tk_Identifier) {
    // it is possible we encounter an error
    fave->fieldAccess.field = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FieldAccessExpectedIdentifier, t.span);
  } else {
    fave->fieldAccess.field = t.identifier;
  }
  fave->span = SPAN(root->span.start, t.span.end);
}

static void parseCallValueExpr(ValueExpr *cvep, Vector *diagnostics,
                               Parser *parser, ValueExpr *root) {
  ZERO(cvep);

  cvep->kind = VEK_Call;
  cvep->callExpr.function = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_ParenLeft);

  LnCol end;

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,          // members_vec_ptr
             diagnostics,          // diagnostics_vec_ptr
             parseValueExpr,       // member_parse_function
             ValueExpr,            // member_kind
             tk_ParenRight,        // delimiting_token_kind
             DK_CallExpectedParen, // missing_delimiter_error
             end,                  // end_lncol
             parser                // parser
  )

  cvep->callExpr.parameters_len = VEC_LEN(&parameters, ValueExpr);
  cvep->callExpr.parameters = vec_release(&parameters);

  cvep->span = SPAN(root->span.start, end);
}

static void parseAsValueExpr(ValueExpr *avep, Vector *diagnostics,
                             Parser *parser, ValueExpr *root) {
  ZERO(avep);
  avep->kind = VEK_As;
  avep->asExpr.value = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_As);

  avep->asExpr.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(avep->asExpr.type, diagnostics, parser);
}

static void parseL2ValueExpr(ValueExpr *l2, Vector *diagnostics,
                             Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff

  ValueExpr *root = l2;
  parseL1ValueExpr(root, diagnostics, parser);

  while (true) {
    // represents the old operation
    ValueExpr *v;

    Token t = parse_peek(parser);
    switch (t.kind) {
    case tk_Ref: {
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      pushCommentScopeParser(parser);
      root->kind = VEK_UnaryOp;
      root->unaryOp.operator= VEUOK_Ref;
      root->unaryOp.operand = v;
      root->span = SPAN(v->span.start, t.span.end);
      parse_next(parser, diagnostics);
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
      parse_next(parser, diagnostics);
      break;
    }
    case tk_FieldAccess: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      parseFieldAccessValueExpr(root, diagnostics, parser, v);
      break;
    }
    case tk_ParenLeft: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      parseCallValueExpr(root, diagnostics, parser, v);
      break;
    }
    case tk_As: {
      pushCommentScopeParser(parser);
      v = ALLOC(parser->a, ValueExpr);
      *v = *root;
      parseAsValueExpr(root, diagnostics, parser, v);
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

static void parseL3ValueExpr(ValueExpr *l3, Vector *diagnostics,
                             Parser *parser) {
  Token t = parse_peek(parser);
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
    parseL2ValueExpr(l3, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l3->kind = VEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l3->unaryOp.operand = ALLOC(parser->a, ValueExpr);
  parseL3ValueExpr(l3->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);

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
  static void parseL##x##type(type *l##x, Vector *diagnostics,                 \
                              Parser *parser) {                                \
    type v;                                                                    \
    lower_fn(&v, diagnostics, parser);                                         \
                                                                               \
    Token t = parse_peek(parser);                                              \
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
    l##x->binaryOp.left_operand = ALLOC(parser->a, type);                      \
    *l##x->binaryOp.left_operand = v;                                          \
                                                                               \
    /* first create comment scope and go through operator */                   \
    pushCommentScopeParser(parser);                                            \
    /* create vector for diagnostics */                                        \
    parse_next(parser, diagnostics);                                           \
                                                                               \
    /* now parse the rest of the expression */                                 \
    l##x->binaryOp.right_operand = ALLOC(parser->a, type);                     \
    parseL##x##type(l##x->binaryOp.right_operand, diagnostics, parser);        \
                                                                               \
    /* calculate misc stuff */                                                 \
    l##x->span = SPAN(l##x->binaryOp.left_operand->span.start,                 \
                      l##x->binaryOp.right_operand->span.end);                 \
                                                                               \
    /* comments */                                                             \
    Vector comments = popCommentScopeParser(parser);                           \
    l##x->comments_len = VEC_LEN(&comments, Comment);                          \
    l##x->comments = vec_release(&comments);                                   \
    return;                                                                    \
  }

static inline bool opDetL4ValueExpr(tk_Kind tk,
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
static inline void parseValueExprTerm(ValueExpr *term, Vector *diagnostics,
                                      Parser *parser) {
  parseL4ValueExpr(term, diagnostics, parser);
}

static inline bool opDetL5ValueExpr(tk_Kind tk,
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

static inline bool opDetL6ValueExpr(tk_Kind tk,
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

static inline bool opDetL7ValueExpr(tk_Kind tk,
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

static inline bool opDetL8ValueExpr(tk_Kind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_And: {
    *val = VEBOK_And;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 8, parseL7ValueExpr)

static inline bool opDetL9ValueExpr(tk_Kind tk,
                                    enum ValueExprBinaryOpKind_e *val) {
  switch (tk) {
  case tk_Or: {
    *val = VEBOK_Or;
    return true;
  }
  default: {
    return false;
  }
  }
}
FN_BINOP_PARSE_LX_EXPR(ValueExpr, VEK, 9, parseL8ValueExpr)

static inline bool opDetL10ValueExpr(tk_Kind tk,
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

static bool opDetL11ValueExpr(tk_Kind tk, enum ValueExprBinaryOpKind_e *val) {
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
static void parseValueExpr(ValueExpr *vep, Vector *diagnostics,
                           Parser *parser) {
  parseL11ValueExpr(vep, diagnostics, parser);
}

// field : Type,
static void parseTypeStructMemberExpr(struct TypeStructMemberExpr_s *tsmep,
                                      Vector *diagnostics, Parser *parser) {
  // zero-initialize bp
  ZERO(tsmep);
  pushCommentScopeParser(parser);

  LnCol start;
  LnCol end;

  // get identifier
  Token t = parse_next(parser, diagnostics);
  Span identitySpan = t.span;
  start = identitySpan.start;

  if (t.kind != tk_Identifier) {
    tsmep->name = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructMemberExpectedIdentifier, t.span);
    end = identitySpan.end;
    goto CLEANUP;
  }

  tsmep->name = t.identifier;

  // check if colon
  t = parse_peek(parser);
  if (t.kind == tk_Colon) {
    // advance through colon
    parse_next(parser, diagnostics);
    // Get type of variable
    tsmep->type = ALLOC(parser->a, TypeExpr);
    parseTypeExpr(tsmep->type, diagnostics, parser);
    end = tsmep->type->span.end;
  } else {
    end = identitySpan.end;
    tsmep->type = ALLOC(parser->a, TypeExpr);
    tsmep->type->kind = TEK_Omitted;
    tsmep->type->span = identitySpan;
    tsmep->type->comments_len = 0;
  }

CLEANUP:
  tsmep->span = SPAN(start, end);
  Vector comments = popCommentScopeParser(parser);
  tsmep->comments_len = VEC_LEN(&comments, Comment);
  tsmep->comments = vec_release(&comments);
  return;
}

static void certain_parseStructTypeExpr(TypeExpr *ste, Vector *diagnostics,
                                        Parser *parser) {
  ZERO(ste);
  ste->kind = TEK_Struct;

  Token t = parse_next(parser, diagnostics);
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
    assert(t.kind == tk_Struct || t.kind == tk_Enum);
  }
  }

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    end = t.span.end;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_StructExpectedLeftBrace, t.span);
    goto CLEANUP;
  }

  PARSE_LIST(&members,                      // members_vec_ptr
             diagnostics,                   // diagnostics_vec_ptr
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
  ste->span = SPAN(start, end);
  return;
}

static void parseReferenceTypeExpr(TypeExpr *rtep, Vector *diagnostics,
                                   Parser *parser) {
  ZERO(rtep);
  rtep->kind = TEK_Reference;
  rtep->referenceExpr.path = ALLOC(parser->a, Path);
  parsePath(rtep->referenceExpr.path, diagnostics, parser);
  rtep->span = rtep->referenceExpr.path->span;
}

static void certain_parseNilTypeExpr(TypeExpr *vte, Vector *diagnostics,
                                     Parser *parser) {

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Nil);
  vte->kind = TEK_Nil;
  vte->span = t.span;
  return;
}

static void parseFnTypeExpr(TypeExpr *fte, Vector *diagnostics,
                            Parser *parser) {
  ZERO(fte);

  Token t = parse_next(parser, diagnostics);

  assert(t.kind == tk_Fn);

  LnCol start = t.span.start;
  LnCol end;

  // check for leftparen
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_ParenLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedLeftParen, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  Vector parameters = vec_create(parser->a);

  PARSE_LIST(&parameters,                     // members_vec_ptr
             diagnostics,                     // diagnostics_vec_ptr
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
  parseTypeExpr(fte->fnExpr.parameters, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Colon) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_FnTypeExprExpectedColon, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

CLEANUP:
  fte->span = SPAN(start, end);
}

static void parseL1TypeExpr(TypeExpr *l1, Vector *diagnostics, Parser *parser) {
  pushCommentScopeParser(parser);

  Token t = parse_peek(parser);
  switch (t.kind) {
  case tk_Identifier: {
    parseReferenceTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Enum:
  case tk_Struct: {
    certain_parseStructTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Nil: {
    certain_parseNilTypeExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Fn: {
    parseFnTypeExpr(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = TEK_None;
    l1->span = t.span;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser, diagnostics);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseScopeResolutionTypeExpr(TypeExpr *srte, Vector *diagnostics,
                                         Parser *parser, TypeExpr *root) {
  ZERO(srte);
  srte->kind = TEK_FieldAccess;
  srte->fieldAccess.value = root;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_FieldAccess);

  // Now we get the field
  t = parse_peek(parser);
  if (t.kind != tk_Identifier) {
    // it is possible we encounter an error
    srte->fieldAccess.field = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeExprFieldAccessExpectedIdentifier, t.span);
  } else {
    srte->fieldAccess.field = t.identifier;
  }

  srte->span = SPAN(root->span.start, t.span.end);
}

static void parseL2TypeExpr(TypeExpr *l2, Vector *diagnostics, Parser *parser) {
  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  TypeExpr *root = l2;
  parseL1TypeExpr(root, diagnostics, parser);

  while (true) {
    // represents the new operation
    TypeExpr *ty;

    Token t = parse_peek(parser);
    switch (t.kind) {
    case tk_Ref: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      root->kind = TEK_UnaryOp;
      root->unaryOp.operator= TEUOK_Ref;
      root->unaryOp.operand = ty;
      root->span = SPAN(ty->span.start, t.span.end);
      // discard token
      parse_next(parser, diagnostics);
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
      // discard token
      parse_next(parser, diagnostics);
      break;
    }
    case tk_ScopeResolution: {
      pushCommentScopeParser(parser);
      ty = ALLOC(parser->a, TypeExpr);
      *ty = *root;
      parseScopeResolutionTypeExpr(root, diagnostics, parser, ty);
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

static inline bool opDetL3TypeExpr(tk_Kind tk,
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

static inline bool opDetL4TypeExpr(tk_Kind tk,
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

static void parseTypeExpr(TypeExpr *tep, Vector *diagnostics, Parser *parser) {
  parseL4TypeExpr(tep, diagnostics, parser);
}

static void certain_parseValueRestrictionPatternExpr(PatternExpr *vrpe,
                                                     Vector *diagnostics,
                                                     Parser *parser) {
  ZERO(vrpe);

  Token t = parse_next(parser, diagnostics);
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
    assert(t.kind == tk_CompEqual || t.kind == tk_CompNotEqual ||
           t.kind == tk_CompGreaterEqual || t.kind == tk_CompGreater ||
           t.kind == tk_CompLess || t.kind == tk_CompLessEqual);
    PANIC();
  }
  }
  vrpe->valueRestriction.valueExpr = ALLOC(parser->a, ValueExpr);
  parseValueExprTerm(vrpe->valueRestriction.valueExpr, diagnostics, parser);
  LnCol end = vrpe->valueRestriction.valueExpr->span.end;

  vrpe->span = SPAN(start, end);
  return;
}

static void certain_parseTypeRestrictionPatternExpr(PatternExpr *trpe,
                                                    Vector *diagnostics,
                                                    Parser *parser) {
  ZERO(trpe);

  trpe->kind = PEK_TypeRestriction;

  bool parseType = false;

  Token t = parse_next(parser, diagnostics);

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
    t = parse_peek(parser);
    if (t.kind == tk_Colon) {
      parseType = true;
      // advance through it
      t = parse_next(parser, diagnostics);
    } else {
      parseType = false;
    }
    break;
  }
  default: {
    assert(t.kind == tk_Colon || t.kind == tk_Identifier);
    PANIC();
  }
  }

  if (parseType) {
    trpe->typeRestriction.type = ALLOC(parser->a, TypeExpr);
    parseTypeExpr(trpe->typeRestriction.type, diagnostics, parser);
    end = t.span.end;
  } else {
    trpe->typeRestriction.type = ALLOC(parser->a, TypeExpr);
    trpe->typeRestriction.type->kind = TEK_Omitted;
    trpe->typeRestriction.type->span = SPAN(start, end);
    trpe->typeRestriction.type->comments_len = 0;
  }

  trpe->span = SPAN(start, end);
}

// pattern ':=' ('..' | identifier )
static void
parsePatternStructMemberExpr(struct PatternStructMemberExpr_s *psmep,
                             Vector *diagnostics, Parser *parser) {
  pushCommentScopeParser(parser);
  ZERO(psmep);

  // pattern
  psmep->pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(psmep->pattern, diagnostics, parser);

  LnCol start = psmep->pattern->span.end;
  LnCol end;

  // get define token
  Token t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedDefine, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  // field value
  t = parse_next(parser, diagnostics);
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
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }
  }

  end = t.span.end;

CLEANUP:
  psmep->span = SPAN(start, end);

  Vector comments = popCommentScopeParser(parser);
  psmep->comments_len = VEC_LEN(&comments, Comment);
  psmep->comments = vec_release(&comments);
  return;
}

static void certain_parseStructPatternExpr(PatternExpr *spe,
                                           Vector *diagnostics,
                                           Parser *parser) {
  ZERO(spe);
  spe->kind = PEK_Struct;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Struct);

  LnCol start = t.span.start;
  LnCol end;

  Vector members = vec_create(parser->a);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceLeft) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternStructExpectedLeftBrace, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  PARSE_LIST(&members,                           // members_vec_ptr
             diagnostics,                        // diagnostics_vec_ptr
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
  spe->span = SPAN(start, end);
  return;
}

static void certain_parseGroupPatternExpr(PatternExpr *gpep,
                                          Vector *diagnostics, Parser *parser) {
  ZERO(gpep);
  gpep->kind = PEK_Group;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_BraceLeft);
  LnCol start = t.span.start;
  LnCol end;

  gpep->groupExpr.value = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(gpep->groupExpr.value, diagnostics, parser);

  t = parse_next(parser, diagnostics);
  if (t.kind != tk_BraceRight) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_PatternGroupExpectedRightBrace, t.span);
    end = t.span.end;
  } else {
    end = gpep->groupExpr.value->span.end;
  }

  gpep->span = SPAN(start, end);
}

static void parseL1PatternExpr(PatternExpr *l1, Vector *diagnostics,
                               Parser *parser) {
  pushCommentScopeParser(parser);

  Token t = parse_peek(parser);
  switch (t.kind) {
  case tk_BraceLeft: {
    certain_parseGroupPatternExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Struct: {
    certain_parseStructPatternExpr(l1, diagnostics, parser);
    break;
  }
  case tk_Identifier:
  case tk_Colon: {
    certain_parseTypeRestrictionPatternExpr(l1, diagnostics, parser);
    break;
  }
  case tk_CompEqual:
  case tk_CompNotEqual:
  case tk_CompGreaterEqual:
  case tk_CompGreater:
  case tk_CompLess:
  case tk_CompLessEqual: {
    certain_parseValueRestrictionPatternExpr(l1, diagnostics, parser);
    break;
  }
  default: {
    l1->kind = PEK_None;
    l1->span = t.span;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncParser(parser, diagnostics);
    break;
  }
  }

  Vector comments = popCommentScopeParser(parser);
  l1->comments_len = VEC_LEN(&comments, Comment);
  l1->comments = vec_release(&comments);
}

static void parseL2PatternExpr(PatternExpr *l2, Vector *diagnostics,
                               Parser *parser) {
  Token t = parse_peek(parser);
  switch (t.kind) {
  case tk_Not: {
    l2->unaryOp.operator= PEUOK_Not;
    break;
  }
  default: {
    // there is no level expression
    parseL1PatternExpr(l2, diagnostics, parser);
    return;
  }
  }

  // this will only execute if an L3 operator exists
  l2->kind = PEK_UnaryOp;

  // first create comment scope and go through op
  pushCommentScopeParser(parser);
  t = parse_next(parser, diagnostics);

  // Now parse the rest of the expression
  l2->unaryOp.operand = ALLOC(parser->a, PatternExpr);
  parseL2PatternExpr(l2->unaryOp.operand, diagnostics, parser);

  // finally calculate the misc stuff
  l2->span = SPAN(t.span.start, l2->unaryOp.operand->span.end);

  // comments
  Vector comments = popCommentScopeParser(parser);
  l2->comments_len = VEC_LEN(&comments, Comment);
  l2->comments = vec_release(&comments);
}

static inline bool opDetL3PatternExpr(tk_Kind tk,
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

static inline bool opDetL4PatternExpr(tk_Kind tk,
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

static inline bool opDetL5PatternExpr(tk_Kind tk,
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

static inline bool opDetL6PatternExpr(tk_Kind tk,
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

static void parsePatternExpr(PatternExpr *ppe, Vector *diagnostics,
                             Parser *parser) {
  parseL6PatternExpr(ppe, diagnostics, parser);
}

static void parseValDecl(Stmnt *vdsp, Vector *diagnostics, Parser *parser) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = SK_ValDecl;

  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Let);
  LnCol start = t.span.start;

  // Get Binding
  vdsp->valDecl.pattern = ALLOC(parser->a, PatternExpr);
  parsePatternExpr(vdsp->valDecl.pattern, diagnostics, parser);

  LnCol end;

  // Expect define
  t = parse_peek(parser);
  if (t.kind == tk_Define) {
    // accept the define token
    parse_next(parser, diagnostics);

    vdsp->valDecl.has_value = true;
    vdsp->valDecl.value = ALLOC(parser->a, ValueExpr);
    parseValueExpr(vdsp->valDecl.value, diagnostics, parser);
    end = vdsp->valDecl.value->span.end;
  } else {
    vdsp->valDecl.has_value = false;
    end = vdsp->valDecl.pattern->span.end;
  }

  vdsp->span = SPAN(start, end);
}

static void parseTypeDecl(Stmnt *tdp, Vector *diagnostics, Parser *parser) {
  ZERO(tdp);
  tdp->kind = SK_TypeDecl;
  Token t = parse_next(parser, diagnostics);
  // enforce that next token is type
  assert(t.kind == tk_Type);
  LnCol start = t.span.start;

  LnCol end;

  // get identifier of type decl
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Identifier) {
    tdp->typeDecl.name = NULL;
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeDeclExpectedIdentifier, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.name = t.identifier;

  // Now get define
  t = parse_next(parser, diagnostics);
  if (t.kind != tk_Define) {
    *VEC_PUSH(diagnostics, Diagnostic) =
        DIAGNOSTIC(DK_TypeDeclExpectedDefine, t.span);
    end = t.span.end;
    goto CLEANUP;
  }

  tdp->typeDecl.type = ALLOC(parser->a, TypeExpr);
  parseTypeExpr(tdp->typeDecl.type, diagnostics, parser);
  end = tdp->typeDecl.type->span.end;

CLEANUP:
  tdp->span = SPAN(start, end);
  return;
}

static void certain_parseDeferStmnt(Stmnt *dsp, Vector *diagnostics,
                                    Parser *parser) {
  dsp->kind = SK_DeferStmnt;
  Token t = parse_next(parser, diagnostics);
  assert(t.kind == tk_Defer);
  dsp->deferStmnt.value = ALLOC(parser->a, ValueExpr);
  parseValueExpr(dsp->deferStmnt.value, diagnostics, parser);
  dsp->span = SPAN(t.span.start, dsp->deferStmnt.value->span.end);
  return;
}

static void parseStmnt(Stmnt *sp, Vector *diagnostics, Parser *parser) {
  pushCommentScopeParser(parser);

  // peek next token
  Token t = parse_peek(parser);
  switch (t.kind) {
    // Macros
  case tk_Macro: {
    sp->kind = SK_Macro;
    sp->macroStmnt.name = t.macro_call;
    sp->span = t.span;
    break;
  }
  case tk_Use: {
    LnCol start = t.span.start;
    parse_next(parser, diagnostics); // drop use token
    sp->kind = SK_Use;
    sp->useStmnt.path = ALLOC(parser->a, Path);
    parsePath(sp->useStmnt.path, diagnostics, parser);
    sp->span = SPAN(start, sp->useStmnt.path->span.end);
    break;
  }
  case tk_Namespace: {
    LnCol start = t.span.start;
    parse_next(parser, diagnostics); // drop namespace token
    sp->kind = SK_Namespace;
    sp->namespaceStmnt.path = ALLOC(parser->a, Path);
    parsePath(sp->namespaceStmnt.path, diagnostics, parser);
    sp->namespaceStmnt.stmnt = ALLOC(parser->a, Stmnt);
    parseStmnt(sp->namespaceStmnt.stmnt, diagnostics, parser);
    sp->span = SPAN(start, sp->namespaceStmnt.stmnt->span.end);
    break;
  }
  // Let Stmnt (Decl)
  case tk_Let: {
    parseValDecl(sp, diagnostics, parser);
    break;
  }
  case tk_Type: {
    parseTypeDecl(sp, diagnostics, parser);
    break;
  }
  // Expressions
  default: {
    sp->kind = SK_ValExpr;
    sp->valExpr.value = ALLOC(parser->a, ValueExpr);
    parseValueExpr(sp->valExpr.value, diagnostics, parser);
    sp->span = sp->valExpr.value->span;
    break;
  }
  }
  Vector comments = popCommentScopeParser(parser);
  sp->comments_len = VEC_LEN(&comments, Comment);
  sp->comments = vec_release(&comments);
}

bool parse_nextStmntIfExists(Stmnt *s, Vector *diagnostics, Parser *parser) {
  Token t = parse_peek(parser);
  if (t.kind == tk_Eof) {
    return true;
  }
  parseStmnt(s, diagnostics, parser);
  return false;
}
