#include "token.h"

#include <stdlib.h>
#include <stdio.h>

Token* newToken(SymType type, void* payload) {
  Token* t = malloc(sizeof(Token));
  t->type = type;
  t->payload = payload;
  return t;
}

void deleteToken(Token* token) {
  if (token->type == SymStringLiteral || token->type == SymFloatLiteral ||
      token->type == SymIntLiteral) {
    free(token->payload);
  }
  free(token);
}

void printToken(Token* token) {
  char* str;
  switch (token->type) {
    case SymIdentifier: {
      str = "Identifier";
      break;
    }
    case SymIf: {
      str = "If";
      break;
    }
    case SymElse: {
      str = "Else";
      break;
    }
    case SymWhile: {
      str = "While";
      break;
    }
    case SymFor: {
      str = "For";
      break;
    }
    case SymWith: {
      str = "With";
      break;
    }
    case SymMatch: {
      str = "Match";
      break;
    }
    case SymBreak: {
      str = "Break";
      break;
    }
    case SymContinue: {
      str = "Continue";
      break;
    }
    case SymReturn: {
      str = "Return";
      break;
    }
    case SymStringLiteral: {
      str = "StringLiteral";
      break;
    }
    case SymCharacterLiteral: {
      str = "CharacterLiteral";
      break;
    }
    case SymFloatLiteral: {
      str = "FloatLiteral";
      break;
    }
    case SymIntLiteral: {
      str = "IntLiteral";
      break;
    }
    case SymAdd: {
      str = "Add";
      break;
    }
    case SymSub: {
      str = "Sub";
      break;
    }
    case SymMul: {
      str = "Mul";
      break;
    }
    case SymDiv: {
      str = "Div";
      break;
    }
    case SymMod: {
      str = "Mod";
      break;
    }
    case SymAnd: {
      str = "And";
      break;
    }
    case SymOr: {
      str = "Or";
      break;
    }
    case SymNot: {
      str = "Not";
      break;
    }
    case SymBitAnd: {
      str = "BitAnd";
      break;
    }
    case SymBitOr: {
      str = "BitOr";
      break;
    }
    case SymBitXor: {
      str = "BitXor";
      break;
    }
    case SymBitNot: {
      str = "BitNot";
      break;
    }
    case SymShiftLeft: {
      str = "ShiftLeft";
      break;
    }
    case SymShiftRight: {
      str = "ShiftRight";
      break;
    }
    case SymEqual: {
      str = "Equal";
      break;
    }
    case SymNotEqual: {
      str = "NotEqual";
      break;
    }
    case SymCompLess: {
      str = "CompLess";
      break;
    }
    case SymCompLessEqual: {
      str = "CompLessEqual";
      break;
    }
    case SymCompGreater: {
      str = "CompGreater";
      break;
    }
    case SymCompGreaterEqual: {
      str = "CompGreaterEqual";
      break;
    }
    case SymRef: {
      str = "Ref";
      break;
    }
    case SymDeref: {
      str = "Deref";
      break;
    }
    case SymAssign: {
      str = "Assign";
      break;
    }
    case SymPipe: {
      str = "Pipe";
      break;
    }
    case SymParenLeft: {
      str = "ParenLeft";
      break;
    }
    case SymParenRight: {
      str = "ParenRight";
      break;
    }
    case SymBracketLeft: {
      str = "BracketLeft";
      break;
    }
    case SymBracketRight: {
      str = "BracketRight";
      break;
    }
    case SymBraceLeft: {
      str = "BraceLeft";
      break;
    }
    case SymBraceRight: {
      str = "BraceRight";
      break;
    }
    case SymDot: {
      str = "Dot";
      break;
    }
    case SymComma: {
      str = "Comma";
      break;
    }
    case SymColon: {
      str = "Colon";
      break;
    }
    case SymSemicolon: {
      str = "Semicolon";
      break;
    }
    case SymComment: {
      str = "Comment";
      break;
    }
    case SymDocumentation: {
      str = "Documentation";
      break;
    }
    case SymAnnotation: {
      str = "Annotation";
      break;
    }
  }
  puts(str);
}

