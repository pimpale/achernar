#include "ast.h"

#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "lexer.h"
#include "token.h"
#include "vector.h"

#define EXPECT_TYPE(token, tokenType, onErrLabel)                              \
  do {                                                                         \
    if ((token).type != (tokenType)) {                                         \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

#define EXPECT_NO_ERROR(thingPtr, onErrLabel)                                  \
  do {                                                                         \
    if ((thingPtr)->diagnostic.type != E_Ok) {                                 \
      goto onErrLabel;                                                         \
    }                                                                          \
  } while (false)

// Call this method after token error detected to skip to the next valid state

// Jump till after semicolon, taking into account parentheses, brackets,
// attributes, and braces It will ignore subexprs, but can halt at the end
static void resyncStmnt(BufferedLexer *blp) {
  int64_t parenDepth = 0;
  int64_t braceDepth = 0;
  int64_t bracketDepth = 0;
  int64_t attrDepth = 0;

  while (true) {
    Token t;
    advanceToken(blp, &t);
    switch (t.type) {
    case T_ParenLeft: {
      parenDepth++;
      break;
    }
    case T_ParenRight: {
      parenDepth--;
      if (parenDepth < 0) {
        return;
      }
      break;
    }
    case T_BraceLeft: {
      braceDepth++;
      break;
    }
    case T_BraceRight: {
      braceDepth--;
      if (braceDepth < 0) {
        return;
      }
      break;
    }
    case T_BracketLeft: {
      bracketDepth++;
      break;
    }
    case T_BracketRight: {
      bracketDepth--;
      if (bracketDepth < 0) {
        return;
      }
      break;
    }
    case T_AttrLeft: {
      attrDepth++;
      break;
    }
    case T_AttrRight: {
      attrDepth--;
      if (attrDepth < 0) {
        return;
      }
      break;
    }
    case T_Semicolon: // Fallthrough
    case T_Comma: {
      if (braceDepth <= 0 && bracketDepth <= 0 && parenDepth <= 0 &&
          attrDepth <= 0) {
        // put the comma or semicolon back on the stack to parse
        setNextToken(blp, &t);
        return;
      }
      break;
    }
    case T_None: {
      if (t.error == E_EOF) {
        return;
      }
      break;
    }
    default: {
      break;
    }
    }
  }
}

// Jumps till we hit identifier or assign sign then backtracks one
// Not aware of parens, braces, brackets or attributes
// This is because all types are linear in nature in BPlus
static void resyncType(BufferedLexer *blp) {
  while (true) {
    Token t;
    advanceToken(blp, &t);
    switch (t.type) {
    case T_Assign:
    case T_Identifier: {
      setNextToken(blp, &t);
      return;
    }
    default: {
      break;
    }
    }
  }
}

// Note that all errors resynch at the statement level
static void parseStmnt(Stmnt *sp, BufferedLexer *blp);
static void parseValueExpr(ValueExpr *vep, BufferedLexer *blp);
static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp);
static void parsePattern(Pattern *pp, BufferedLexer *blp);

static void parseStmnt(Stmnt *s, BufferedLexer *blp) {
  ZERO(s);
}

// Level0Term (literals of any kind)
// Level1Term parentheses, braces
// Level2Term () [] $ @ . -> (suffixes)
// Level3Term - + ~ ! (prefixes)
// Level4Term * / % (multiplication and division)
// Level5Term + - (addition and subtraction)
// Level6Term << >> & | ^ (bitwise operators)
// Level7Term < <= > >= == != (comparators)
// Level8Term && || (Boolean Operators)

static void parseL1Term(ValueExpr *l1, BufferedLexer *blp) { ZERO(l1); }

static void parseL2Term(ValueExpr *l2, BufferedLexer *blp) {

  // Because it's postfix, we must take a somewhat unorthodox approach here
  // We Parse the level one expr and then use a while loop to process the rest
  // of the stuff
  ValueExpr currentTopLevel;
  parseL1Term(&currentTopLevel, blp);
  LnCol start = currentTopLevel.span.start;

  while (true) {
    Token t;
    advanceToken(blp, &t);
    switch (t.type) {
    case T_Ref: {
      ValueExpr v;
      v.kind = VE_UnaryOp;
      v.unaryOp.operator= EUO_Ref;
      v.unaryOp.operand = malloc(sizeof(ValueExpr));
      *v.unaryOp.operand = currentTopLevel;
      v.span = SPAN(start, t.span.end);
      v.diagnostic = DIAGNOSTIC(E_Ok, v.span);
      currentTopLevel = v;
      break;
    }
    case T_Deref: {
      ValueExpr v;
      v.kind = VE_UnaryOp;
      v.unaryOp.operator= EUO_Deref;
      v.unaryOp.operand = malloc(sizeof(ValueExpr));
      *v.unaryOp.operand = currentTopLevel;
      v.span = SPAN(start, t.span.end);
      v.diagnostic = DIAGNOSTIC(E_Ok, v.span);
      currentTopLevel = v;
      return;
    }
    case T_BracketLeft: {
      ValueExpr v;
      v.kind = VE_BinaryOp;
      v.binaryOp.operator= EBO_ArrayAccess;
      v.binaryOp.operand_1 = malloc(sizeof(ValueExpr));
      v.binaryOp.operand_2 = malloc(sizeof(ValueExpr));
      *v.binaryOp.operand_1 = v;
      parseValueExpr(v.binaryOp.operand_2, blp);
      // expect closing bracket
      advanceToken(blp, &t);

      // Calculate span and diagnostics
      v.span = SPAN(start, t.span.end);
      if (t.type != T_BracketRight) {
        v.diagnostic = DIAGNOSTIC(E_ArrayAccessExpectedBracket, v.span);
      }
      v.diagnostic = DIAGNOSTIC(E_Ok, v.span);
      // TODO handle error??
      return;
    }
    case T_ParenLeft: {
      ValueExpr v;
      Vector arguments;
      createVector(&arguments);
      // Parse the arguments (Comma Seperated list of valueexprs)
      while (true) {
        // Check for end paren
        advanceToken(blp, &t);
        if (t.type == T_ParenRight) {
          break;
        }
        // If it wasn't an end paren, we push it back
        setNextToken(blp, &t);

        // Parse and push the argument
        parseValueExpr(VEC_PUSH(&arguments, ValueExpr), blp);

        // Accept comma, if any
        // If there's no comma then it MUST be followed by an end paren
        // This also allows trailing commas
        advanceToken(blp, &t);
        if (t.type != T_Comma) {
          if (t.type == T_ParenRight) {
            break;
          } else {
            // If the next value isn't an end paren, then we throw an error
            v.kind = VE_Call;
            v.callExpr.function = malloc(sizeof(ValueExpr));
            *v.callExpr.function = currentTopLevel;
            v.callExpr.arguments_length = VEC_LEN(&arguments, ValueExpr);
            v.callExpr.arguments = releaseVector(&arguments);

            // Calculate span and diagnostics
            v.span = SPAN(start, t.span.end);
            v.diagnostic = DIAGNOSTIC(E_FunctionCallExpectedParen, v.span);
            // TODO handle error??
            return;
          }
        }
      }

      v.kind = VE_Call;
      v.callExpr.function = malloc(sizeof(ValueExpr));
      *v.callExpr.function = currentTopLevel;
      v.callExpr.arguments_length = VEC_LEN(&arguments, ValueExpr);
      v.callExpr.arguments = releaseVector(&arguments);

      // Calculate span and diagnostics
      v.span = SPAN(start, t.span.end);
      v.diagnostic = DIAGNOSTIC(E_Ok, l2->span);
      break;
    }
    default: {
      // there are no more level 2 expressions
      setNextToken(blp, &t);
      *l2 = currentTopLevel;
      return;
    }
    }
  }
}

static void parseL3Term(ValueExpr *l3, BufferedLexer *blp) {

  Token t;
  advanceToken(blp, &t);
  switch (t.type) {
  case T_Sub: {
    l3->unaryOp.operator= EUO_Negate;
    break;
  }
  case T_Add: {
    l3->unaryOp.operator= EUO_Posit;
    break;
  }
  case T_BitNot: {
    l3->unaryOp.operator= EUO_BitNot;
    break;
  }
  case T_Not: {
    l3->unaryOp.operator= EUO_LogicalNot;
    break;
  }
  default: {
    // there is no level 3 expression
    setNextToken(blp, &t);
    parseL2Term(l3, blp);
    return;
  }
  }

  // Now parse the rest of the expression
  l3->kind = VE_UnaryOp;
  l3->unaryOp.operand = malloc(sizeof(ValueExpr));
  parseL3Term(l3->unaryOp.operand, blp);
  l3->span = SPAN(t.span.start, l3->unaryOp.operand->span.end);
  l3->diagnostic = DIAGNOSTIC(E_Ok, l3->span);
  return;
}

static void parseL4Term(ValueExpr *l4, BufferedLexer *blp) {
  ValueExpr v;
  parseL3Term(&v, blp);

  Token t;
  advanceToken(blp, &t);
  switch (t.type) {
  case T_Mul: {
    l4->binaryOp.operator= EBO_Mul;
    break;
  }
  case T_Div: {
    l4->binaryOp.operator= EBO_Div;
    break;
  }
  case T_Mod: {
    l4->binaryOp.operator= EBO_Mod;
    break;
  }
  default: {
    // there is no level 4 expression
    setNextToken(blp, &t);
    *l4 = v;
  }
  }

  l4->kind = VE_BinaryOp;
  l4->binaryOp.operand_1 = malloc(sizeof(ValueExpr));
  *l4->binaryOp.operand_1 = v;
  l4->binaryOp.operand_2 = malloc(sizeof(ValueExpr));
  parseL4Term(l4->binaryOp.operand_2, blp);
  l4->span = SPAN(l4->binaryOp.operand_1->span.start,
                  l4->binaryOp.operand_2->span.end);
  l4->diagnostic = DIAGNOSTIC(E_Ok, l4->span);
  return;
}

static void parseL5Term(ValueExpr *l5, BufferedLexer *blp) {
  ValueExpr v;
  parseL4Term(&v, blp);

  Token t;
  advanceToken(blp, &t);
  switch (t.type) {
  case T_Add: {
    l5->binaryOp.operator= EBO_Add;
    break;
  }
  case T_Sub: {
    l5->binaryOp.operator= EBO_Sub;
    break;
  }
  default: {
    // there is no level 5 expression
    setNextToken(blp, &t);
    *l5 = v;
  }
  }

  l5->kind = VE_BinaryOp;
  l5->binaryOp.operand_1 = malloc(sizeof(ValueExpr));
  *l5->binaryOp.operand_1 = v;
  l5->binaryOp.operand_2 = malloc(sizeof(ValueExpr));
  parseL5Term(l5->binaryOp.operand_2, blp);
  l5->span = SPAN(l5->binaryOp.operand_1->span.start,
                  l5->binaryOp.operand_2->span.end);
  l5->diagnostic = DIAGNOSTIC(E_Ok, l5->span);
  return;
}

static void parseL6Term(ValueExpr *l6, BufferedLexer *blp) {
  ValueExpr v;
  parseL5Term(&v, blp);

  Token t;
  advanceToken(blp, &t);
  switch (t.type) {
  case T_ShiftLeft: {
    l6->binaryOp.operator= EBO_BitShl;
    break;
  }
  case T_ShiftRight: {
    l6->binaryOp.operator= EBO_BitShr;
    break;
  }
  case T_BitAnd: {
    l6->binaryOp.operator= EBO_BitAnd;
    break;
  }
  case T_BitOr: {
    l6->binaryOp.operator= EBO_BitOr;
    break;
  }
  case T_BitXor: {
    l6->binaryOp.operator= EBO_BitXor;
    break;
  }
  default: {
    // there is no level 6 expression
    setNextToken(blp, &t);
    *l6 = v;
  }
  }

  l6->kind = VE_BinaryOp;
  l6->binaryOp.operand_1 = malloc(sizeof(ValueExpr));
  *l6->binaryOp.operand_1 = v;
  l6->binaryOp.operand_2 = malloc(sizeof(ValueExpr));
  parseL6Term(l6->binaryOp.operand_2, blp);
  l6->span = SPAN(l6->binaryOp.operand_1->span.start,
                  l6->binaryOp.operand_2->span.end);
  l6->diagnostic = DIAGNOSTIC(E_Ok, l6->span);
  return;
}

static void parseL7Term(ValueExpr *l7, BufferedLexer *blp) {
  ValueExpr v;
  parseL6Term(&v, blp);

  Token t;
  advanceToken(blp, &t);
  switch (t.type) {
  case T_CompLess: {
    l7->binaryOp.operator= EBO_CompLess;
    break;
  }
  case T_CompGreater: {
    l7->binaryOp.operator= EBO_CompGreater;
    break;
  }
  case T_CompLessEqual: {
    l7->binaryOp.operator= EBO_CompLessEqual;
    break;
  }
  case T_CompGreaterEqual: {
    l7->binaryOp.operator= EBO_CompGreaterEqual;
    break;
  }
  case T_Equal: {
    l7->binaryOp.operator= EBO_CompEqual;
    break;
  }
  case T_NotEqual: {
    l7->binaryOp.operator= EBO_CompNotEqual;
    break;
  }
  default: {
    // there is no level 7 expression
    setNextToken(blp, &t);
    *l7 = v;
  }
  }

  l7->kind = VE_BinaryOp;
  l7->binaryOp.operand_1 = malloc(sizeof(ValueExpr));
  *l7->binaryOp.operand_1 = v;
  l7->binaryOp.operand_2 = malloc(sizeof(ValueExpr));
  parseL7Term(l7->binaryOp.operand_2, blp);
  l7->span = SPAN(l7->binaryOp.operand_1->span.start,
                  l7->binaryOp.operand_2->span.end);
  l7->diagnostic = DIAGNOSTIC(E_Ok, l7->span);
  return;
}

static void parseL8Term(ValueExpr *l8, BufferedLexer *blp) {
  ValueExpr v;
  parseL7Term(&v, blp);

  Token t;
  advanceToken(blp, &t);
  switch (t.type) {
  case T_And: {
    l8->binaryOp.operator= EBO_LogicalAnd;
    break;
  }
  case T_Or: {
    l8->binaryOp.operator= EBO_LogicalOr;
    break;
  }
  default: {
    // There is no level 8 expr
    setNextToken(blp, &t);
    *l8 = v;
    return;
  }
  }

  l8->kind = VE_BinaryOp;
  l8->binaryOp.operand_1 = malloc(sizeof(ValueExpr));
  *l8->binaryOp.operand_1 = v;
  l8->binaryOp.operand_2 = malloc(sizeof(ValueExpr));
  parseL8Term(l8->binaryOp.operand_2, blp);
  l8->span = SPAN(l8->binaryOp.operand_1->span.start,
                  l8->binaryOp.operand_2->span.end);
  l8->diagnostic = DIAGNOSTIC(E_Ok, l8->span);
  return;
}

static void parseTypeExpr(TypeExpr *tep, BufferedLexer *blp) {
  // zero-initialize tep
  ZERO(tep);

  Token t;

  advanceToken(blp, &t);
  LnCol start = t.span.start;
  switch (t.type) {
  case T_Identifier: {
    tep->type.name = strdup(t.identifier);
    while (true) {
      advanceToken(blp, &t);
      if (t.type == T_Ref) {
        tep->type.ptrCount++;
      } else {
        setNextToken(blp, &t);
        break;
      }
    }
    tep->span = SPAN(start, t.span.end);
    tep->diagnostic = DIAGNOSTIC(E_Ok, tep->span);
    break;
  }
  case T_Typeof: {
    tep->typeofExpr.value = malloc(sizeof(ValueExpr));
    parseValueExpr(tep->typeofExpr.value, blp);

    tep->span = SPAN(start, tep->typeofExpr.value->span.end);
    tep->diagnostic = DIAGNOSTIC(E_Ok, tep->span);
    break;
  }
  default: {
    tep->span = t.span;
    tep->diagnostic = DIAGNOSTIC(E_TypeExprUnexpectedToken, t.span);
    // Resync
    resyncType(blp);
    break;
  }
  }
}

static void parseBinding(Binding *bp, BufferedLexer *blp) {
  // zero-initialize bp
  ZERO(bp);

  // these variables will be reused
  Token t;

  // Get type of variable
  bp->type = malloc(sizeof(TypeExpr));
  parseTypeExpr(bp->type, blp);

  // get identifier
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Identifier, HANDLE_NO_IDENTIFIER);
  bp->name = strdup(t.identifier);

  // Error handling
  // Set error, and give back the error causing thing
  // Generally, we use the t token to find the last parsed token

HANDLE_NO_IDENTIFIER:
  // If it's not an identifier token, we must resync
  bp->span = bp->type->span;
  bp->diagnostic = DIAGNOSTIC(E_BindingExpectedIdentifier, t.span);
  resyncStmnt(blp);
  return;
}

static void parseVarDeclStmnt(Stmnt *vdsp, BufferedLexer *blp) {
  // zero-initialize vdsp
  ZERO(vdsp);
  vdsp->kind = S_VarDecl;
  // these variables will be reused
  Token t;

  // Skip let declaration
  advanceToken(blp, &t);
  // The start of the variable declaration
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_Let, HANDLE_NO_LET);

  // Get Binding
  vdsp->varDecl.binding = malloc(sizeof(Binding));
  parseBinding(vdsp->varDecl.binding, blp);

  // Expect Equal Sign
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Assign, HANDLE_NO_ASSIGN);

  // Get Value;
  vdsp->varDecl.value = malloc(sizeof(ValueExpr));
  parseValueExpr(vdsp->varDecl.value, blp);

HANDLE_NO_LET:
  INTERNAL_ERROR("called variable declaration parser where there was no "
                 "variable declaration");
  PANIC();

HANDLE_NO_ASSIGN:
  vdsp->span = SPAN(start, t.span.end);
  vdsp->diagnostic = DIAGNOSTIC(E_VarDeclStmntExpectedAssign, t.span);
  resyncStmnt(blp);
}

static void parseFuncDeclStmnt(Stmnt *fdsp, BufferedLexer *blp) {
  // zero-initialize fdsp
  ZERO(fdsp);

  fdsp->kind = S_FuncDecl;

  // these variables will be reused
  Token t;
  Vector parameterDeclarations;
  createVector(&parameterDeclarations);

  // Skip fn declaration
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_Function, HANDLE_NO_FN);

  // get name
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Identifier, HANDLE_NO_IDENTIFIER);
  fdsp->funcDecl.name = strdup(t.identifier);

  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_ParenLeft, HANDLE_NO_LEFTPAREN);

  // Parse the parameters (Comma Seperated list of bindings)
  while (true) {
    // Check for end paren
    advanceToken(blp, &t);
    if (t.type == T_ParenRight) {
      break;
    }
    // If it wasn't an end paren, we push it back
    setNextToken(blp, &t);

    // Parse and push the parameter
    parseBinding(VEC_PUSH(&parameterDeclarations, Binding), blp);

    // Accept comma, if any
    // If there's no comma then it MUST be followed by an end paren
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.type != T_Comma) {
      // If the next value isn't an end paren, then we throw an error
      EXPECT_TYPE(t, T_ParenRight, HANDLE_NO_RIGHTPAREN);
      break;
    }
  }

  // Copy arguments in
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);

  // Colon return type delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Colon, HANDLE_NO_COLON);

  // Return type
  parseTypeExpr(fdsp->funcDecl.type, blp);

  // Equal sign expression delimiter
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Assign, HANDLE_NO_ASSIGN);

  fdsp->funcDecl.body = malloc(sizeof(ValueExpr));
  parseValueExpr(fdsp->funcDecl.body, blp);
  fdsp->span = SPAN(start, fdsp->funcDecl.body->span.end);
  fdsp->diagnostic = DIAGNOSTIC(E_Ok, fdsp->span);
  return;

  // Error handlers
HANDLE_NO_FN:
  INTERNAL_ERROR("called function declaration parser where there was no "
                 "function declaration");
  PANIC();

HANDLE_NO_IDENTIFIER:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedIdentifier, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  resyncStmnt(blp);
  return;

HANDLE_NO_LEFTPAREN:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  resyncStmnt(blp);
  return;

HANDLE_NO_RIGHTPAREN:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedParen, t.span);
  fdsp->span = SPAN(start, t.span.end);
  fdsp->funcDecl.params_length = VEC_LEN(&parameterDeclarations, Binding);
  fdsp->funcDecl.params = releaseVector(&parameterDeclarations);
  resyncStmnt(blp);
  return;

HANDLE_NO_COLON:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedColon, t.span);
  fdsp->span = SPAN(start, t.span.end);
  resyncStmnt(blp);
  return;

HANDLE_NO_ASSIGN:
  fdsp->diagnostic = DIAGNOSTIC(E_FuncDeclStmntExpectedAssign, t.span);
  fdsp->span = SPAN(start, t.span.end);
  resyncStmnt(blp);
  return;
}

static void parseBreakExpr(ValueExpr *bep, BufferedLexer *blp) {
  bep->kind = VE_Break;
  Token t;
  advanceToken(blp, &t);
  bep->span = t.span;
  EXPECT_TYPE(t, T_Break, HANDLE_NO_BREAK);
  bep->diagnostic = DIAGNOSTIC(E_Ok, t.span);
  return;

HANDLE_NO_BREAK:
  INTERNAL_ERROR("called break parser where there was no break");
  PANIC();
}

static void parseContinueExpr(ValueExpr *cep, BufferedLexer *blp) {
  cep->kind = VE_Continue;
  Token t;
  advanceToken(blp, &t);
  cep->span = t.span;
  EXPECT_TYPE(t, T_Continue, HANDLE_NO_CONTINUE);
  cep->diagnostic = DIAGNOSTIC(E_Ok, t.span);
  return;

HANDLE_NO_CONTINUE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

static void parseWhileExpr(ValueExpr *wep, BufferedLexer *blp) {
  wep->kind = VE_While;
  Token t;
  advanceToken(blp, &t);
  LnCol start = t.span.start;
  EXPECT_TYPE(t, T_While, HANDLE_NO_WHILE);

  wep->whileExpr.condition = malloc(sizeof(ValueExpr));
  parseValueExpr(wep->whileExpr.condition, blp);

  wep->whileExpr.body = malloc(sizeof(ValueExpr));
  parseValueExpr(wep->whileExpr.body, blp);

  wep->span = SPAN(start, wep->whileExpr.body->span.end);
  wep->diagnostic = DIAGNOSTIC(E_Ok, wep->span);
  return;

HANDLE_NO_WHILE:
  INTERNAL_ERROR("called continue parser where there was no continue");
  PANIC();
}

// Pattern : Expr,
static void parseMatchCaseExpr(MatchCaseExpr *mcep, BufferedLexer *blp) {
  ZERO(mcep);

  Token t;
  // Get pattern
  mcep->pattern = malloc(sizeof(MatchCaseExpr));
  parsePattern(mcep->pattern, blp);
  // Expect colon
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Colon, HANDLE_NO_COLON);
  // Get Value
  mcep->value = malloc(sizeof(ValueExpr));
  parseValueExpr(mcep->value, blp);
  mcep->span = SPAN(mcep->pattern->span.start, mcep->value->span.end);
  mcep->diagnostic = DIAGNOSTIC(E_Ok, mcep->span);
  return;

HANDLE_NO_COLON:
  mcep->span = mcep->pattern->span;
  mcep->diagnostic = DIAGNOSTIC(E_MatchNoColon, t.span);
  resyncStmnt(blp);
  return;
}

static void parseMatchExpr(ValueExpr *mep, BufferedLexer *blp) {
  ZERO(mep);

  Token t;
  // Ensure match
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_Match, HANDLE_NO_MATCH);

  LnCol start = t.span.start;

  // Get expression to match against
  mep->matchExpr.value = malloc(sizeof(ValueExpr));
  parseValueExpr(mep->matchExpr.value, blp);
  // now we must parse the block containing the cases

  // Expect beginning brace
  advanceToken(blp, &t);
  EXPECT_TYPE(t, T_BraceLeft, HANDLE_NO_LEFTBRACE);

  Vector matchCases;
  createVector(&matchCases);
  while (true) {
    // Check if right brace
    advanceToken(blp, &t);
    if (t.type == T_BraceRight) {
      break;
    }
    setNextToken(blp, &t);

    // Parse the match case expr
    parseMatchCaseExpr(VEC_PUSH(&matchCases, MatchCaseExpr), blp);

    // Accept comma, if any
    // If there's no comma then it MUST be followed by an end brace
    // This also allows trailing commas
    advanceToken(blp, &t);
    if (t.type != T_Comma) {
      // If the next value isn't an end brace, then we throw an error
      EXPECT_TYPE(t, T_BraceRight, HANDLE_NO_RIGHTBRACE);
      break;
    }
  }

  // Get interior cases
  mep->matchExpr.cases_length = VEC_LEN(&matchCases, MatchCaseExpr);
  mep->matchExpr.cases = releaseVector(&matchCases);

  // TODO Diagnostics and Span
  return;

HANDLE_NO_MATCH:
  INTERNAL_ERROR("called match parser where there was no match");
  PANIC();

HANDLE_NO_RIGHTBRACE:
  mep->diagnostic = DIAGNOSTIC(E_MatchNoRightBrace, t.span);
  mep->span = SPAN(start, t.span.end);
  mep->matchExpr.cases_length = VEC_LEN(&matchCases, MatchCaseExpr);
  mep->matchExpr.cases = releaseVector(&matchCases);
  resyncStmnt(blp);
  return;

HANDLE_NO_LEFTBRACE:
  mep->diagnostic = DIAGNOSTIC(E_MatchNoLeftbrace, t.span);
  mep->span = SPAN(start, t.span.end);
  mep->matchExpr.cases_length = VEC_LEN(&matchCases, MatchCaseExpr);
  mep->matchExpr.cases = releaseVector(&matchCases);
  resyncStmnt(blp);
  return;
}

void parseTranslationUnit(TranslationUnit *tu, BufferedLexer *blp) {
  Vector data;
  createVector(&data);

  while (true) {
    Stmnt s;
    parseStmnt(&s, blp);
    *VEC_PUSH(&data, Stmnt) = s;

    if (s.diagnostic.type == E_EOF) {
      break;
    }
  }

  tu->statements_length = VEC_LEN(&data, Stmnt);
  tu->statements = releaseVector(&data);
  return;
}
