#include "token.h"

#include <stdlib.h>

void destroyToken(Token* token) {
  switch (token->type) {
  case TokenIdentifier: {
    free(token->identifier);
    break;
  }
  case TokenComment: {
    free(token->comment);
    break;
  }
  case TokenStringLiteral: {
    free(token->stringLiteral);
    break;
  }
  case TokenAnnotation: {
    free(token->annotationLiteral);
    break;
  }
  default: {
    break;
  }
  }
}

void printToken(Token* token) {
  char* str;
  switch (token->type) {
    case TokenIdentifier: {
      str = "Identifier";
      break;
    }
    case TokenIf: {
      str = "If";
      break;
    }
    case TokenElse: {
      str = "Else";
      break;
    }
    case TokenWhile: {
      str = "While";
      break;
    }
    case TokenFor: {
      str = "For";
      break;
    }
    case TokenWith: {
      str = "With";
      break;
    }
    case TokenMatch: {
      str = "Match";
      break;
    }
    case TokenBreak: {
      str = "Break";
      break;
    }
    case TokenContinue: {
      str = "Continue";
      break;
    }
    case TokenReturn: {
      str = "Return";
      break;
    }
    case TokenFunction: {
      str = "Function";
      break;
    }
    case TokenLet: {
      str = "Let";
      break;
    }
    case TokenMut: {
      str = "Mutable";
      break;
    }
    case TokenStruct: {
      str = "Struct";
      break;
    }
    case TokenAlias: {
      str = "Alias";
      break;
    }
    case TokenStringLiteral: {
      str = "StringLiteral";
      break;
    }
    case TokenCharLiteral: {
      str = "CharacterLiteral";
      break;
    }
    case TokenFloatLiteral: {
      str = "FloatLiteral";
      break;
    }
    case TokenIntLiteral: {
      str = "IntLiteral";
      break;
    }
    case TokenAdd: {
      str = "Add";
      break;
    }
    case TokenSub: {
      str = "Sub";
      break;
    }
    case TokenMul: {
      str = "Mul";
      break;
    }
    case TokenDiv: {
      str = "Div";
      break;
    }
    case TokenMod: {
      str = "Mod";
      break;
    }
    case TokenAnd: {
      str = "And";
      break;
    }
    case TokenOr: {
      str = "Or";
      break;
    }
    case TokenNot: {
      str = "Not";
      break;
    }
    case TokenBitAnd: {
      str = "BitAnd";
      break;
    }
    case TokenBitOr: {
      str = "BitOr";
      break;
    }
    case TokenBitXor: {
      str = "BitXor";
      break;
    }
    case TokenBitNot: {
      str = "BitNot";
      break;
    }
    case TokenShiftLeft: {
      str = "ShiftLeft";
      break;
    }
    case TokenShiftRight: {
      str = "ShiftRight";
      break;
    }
    case TokenEqual: {
      str = "Equal";
      break;
    }
    case TokenNotEqual: {
      str = "NotEqual";
      break;
    }
    case TokenCompLess: {
      str = "CompLess";
      break;
    }
    case TokenCompLessEqual: {
      str = "CompLessEqual";
      break;
    }
    case TokenCompGreater: {
      str = "CompGreater";
      break;
    }
    case TokenCompGreaterEqual: {
      str = "CompGreaterEqual";
      break;
    }
    case TokenRef: {
      str = "Ref";
      break;
    }
    case TokenDeref: {
      str = "Deref";
      break;
    }
    case TokenAssign: {
      str = "Assign";
      break;
    }
    case TokenPipe: {
      str = "Pipe";
      break;
    }
    case TokenParenLeft: {
      str = "ParenLeft";
      break;
    }
    case TokenParenRight: {
      str = "ParenRight";
      break;
    }
    case TokenBracketLeft: {
      str = "BracketLeft";
      break;
    }
    case TokenBracketRight: {
      str = "BracketRight";
      break;
    }
    case TokenBraceLeft: {
      str = "BraceLeft";
      break;
    }
    case TokenBraceRight: {
      str = "BraceRight";
      break;
    }
    case TokenDot: {
      str = "Dot";
      break;
    }
    case TokenComma: {
      str = "Comma";
      break;
    }
    case TokenColon: {
      str = "Colon";
      break;
    }
    case TokenSemicolon: {
      str = "Semicolon";
      break;
    }
    case TokenComment: {
      str = "Comment";
      break;
    }
    case TokenAnnotation: {
      str = "Annotation";
      break;
    }
  }
  puts(str);
}
