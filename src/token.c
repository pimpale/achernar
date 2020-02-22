#include "token.h"

#include <stdio.h>
#include <stdlib.h>

void destroyToken(Token *token) {
  switch (token->type) {
  case T_Identifier: {
    free(token->identifier);
    break;
  }
  case T_Comment: {
    free(token->comment);
    break;
  }
  case T_StringLiteral: {
    free(token->stringLiteral);
    break;
  }
  case T_Macro: {
    free(token->macro);
    break;
  }
  default: {
    break;
  }
  }
}

void printToken(Token *token) {
  char *str;
  switch (token->type) {
  case T_None: {
    str = "(Not A T_)";
    break;
  }
  case T_Identifier: {
    str = "Identifier";
    break;
  }
  case T_If: {
    str = "If";
    break;
  }
  case T_Else: {
    str = "Else";
    break;
  }
  case T_While: {
    str = "While";
    break;
  }
  case T_For: {
    str = "For";
    break;
  }
  case T_With: {
    str = "With";
    break;
  }
  case T_Match: {
    str = "Match";
    break;
  }
  case T_Break: {
    str = "Break";
    break;
  }
  case T_Continue: {
    str = "Continue";
    break;
  }
  case T_Return: {
    str = "Return";
    break;
  }
  case T_Function: {
    str = "Function";
    break;
  }
  case T_Let: {
    str = "Let";
    break;
  }
  case T_Struct: {
    str = "Struct";
    break;
  }
  case T_Alias: {
    str = "Alias";
    break;
  }
  case T_StringLiteral: {
    str = "StringLiteral";
    break;
  }
  case T_CharLiteral: {
    str = "CharacterLiteral";
    break;
  }
  case T_FloatLiteral: {
    str = "FloatLiteral";
    break;
  }
  case T_IntLiteral: {
    str = "IntLiteral";
    break;
  }
  case T_Add: {
    str = "Add";
    break;
  }
  case T_Sub: {
    str = "Sub";
    break;
  }
  case T_Mul: {
    str = "Mul";
    break;
  }
  case T_Div: {
    str = "Div";
    break;
  }
  case T_Mod: {
    str = "Mod";
    break;
  }
  case T_And: {
    str = "And";
    break;
  }
  case T_Or: {
    str = "Or";
    break;
  }
  case T_Not: {
    str = "Not";
    break;
  }
  case T_BitAnd: {
    str = "BitAnd";
    break;
  }
  case T_BitOr: {
    str = "BitOr";
    break;
  }
  case T_BitXor: {
    str = "BitXor";
    break;
  }
  case T_BitNot: {
    str = "BitNot";
    break;
  }
  case T_ShiftLeft: {
    str = "ShiftLeft";
    break;
  }
  case T_ShiftRight: {
    str = "ShiftRight";
    break;
  }
  case T_Equal: {
    str = "Equal";
    break;
  }
  case T_NotEqual: {
    str = "NotEqual";
    break;
  }
  case T_CompLess: {
    str = "CompLess";
    break;
  }
  case T_CompLessEqual: {
    str = "CompLessEqual";
    break;
  }
  case T_CompGreater: {
    str = "CompGreater";
    break;
  }
  case T_CompGreaterEqual: {
    str = "CompGreaterEqual";
    break;
  }
  case T_Ref: {
    str = "Ref";
    break;
  }
  case T_Deref: {
    str = "Deref";
    break;
  }
  case T_Assign: {
    str = "Assign";
    break;
  }
  case T_Pipe: {
    str = "Pipe";
    break;
  }
  case T_ParenLeft: {
    str = "ParenLeft";
    break;
  }
  case T_ParenRight: {
    str = "ParenRight";
    break;
  }
  case T_BracketLeft: {
    str = "BracketLeft";
    break;
  }
  case T_BracketRight: {
    str = "BracketRight";
    break;
  }
  case T_BraceLeft: {
    str = "BraceLeft";
    break;
  }
  case T_BraceRight: {
    str = "BraceRight";
    break;
  }
  case T_AttrLeft: {
    str = "AttrLeft";
    break;
  }
  case T_AttrRight: {
    str = "AttrRight";
    break;
  }
  case T_Dot: {
    str = "Dot";
    break;
  }
  case T_Comma: {
    str = "Comma";
    break;
  }
  case T_Colon: {
    str = "Colon";
    break;
  }
  case T_Semicolon: {
    str = "Semicolon";
    break;
  }
  case T_Macro: {
    str = "Macro";
    break;
  }
  case T_Comment: {
    str = "Comment";
    break;
  }
  }
  puts(str);
}
