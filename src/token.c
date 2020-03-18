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
  switch (token->type) {
  case T_None: {
    printf("(Not A Token)\n");
    break;
  }
  case T_Identifier: {
    printf("Identifier: %s\n", token->identifier);
    break;
  }
  case T_If: {
    printf("If\n");
    break;
  }
  case T_Else: {
    printf("Else\n");
    break;
  }
  case T_While: {
    printf("While\n");
    break;
  }
  case T_For: {
    printf("For\n");
    break;
  }
  case T_With: {
    printf("With\n");
    break;
  }
  case T_Match: {
    printf("Match\n");
    break;
  }
  case T_Break: {
    printf("Break\n");
    break;
  }
  case T_Continue: {
    printf("Continue\n");
    break;
  }
  case T_Return: {
    printf("Return\n");
    break;
  }
  case T_Pass: {
    printf("Pass\n");
    break;
  }
  case T_Function: {
    printf("Function\n");
    break;
  }
  case T_Let: {
    printf("Let\n");
    break;
  }
  case T_Struct: {
    printf("Struct\n");
    break;
  }
  case T_Alias: {
    printf("Alias\n");
    break;
  }
  case T_Sizeof: {
    printf("Sizeof\n");
    break;
  }
  case T_Typeof: {
    printf("Typeof\n");
    break;
  }
  case T_Alignof: {
    printf("Alignof\n");
    break;
  }
  case T_StringLiteral: {
    printf("StringLiteral: %s\n", token->stringLiteral);
    break;
  }
  case T_CharLiteral: {
    printf("CharacterLiteral: %c\n", token->charLiteral);
    break;
  }
  case T_FloatLiteral: {
    printf("FloatLiteral %f\n", token->floatLiteral);
    break;
  }
  case T_IntLiteral: {
    printf("IntLiteral %zu\n", token->intLiteral);
    break;
  }
  case T_Add: {
    printf("Add\n");
    break;
  }
  case T_Sub: {
    printf("Sub\n");
    break;
  }
  case T_Mul: {
    printf("Mul\n");
    break;
  }
  case T_Div: {
    printf("Div\n");
    break;
  }
  case T_Mod: {
    printf("Mod\n");
    break;
  }
  case T_And: {
    printf("And\n");
    break;
  }
  case T_Or: {
    printf("Or\n");
    break;
  }
  case T_Not: {
    printf("Not\n");
    break;
  }
  case T_BitAnd: {
    printf("BitAnd\n");
    break;
  }
  case T_BitOr: {
    printf("BitOr\n");
    break;
  }
  case T_BitXor: {
    printf("BitXor\n");
    break;
  }
  case T_BitNot: {
    printf("BitNot\n");
    break;
  }
  case T_ShiftLeft: {
    printf("ShiftLeft\n");
    break;
  }
  case T_ShiftRight: {
    printf("ShiftRight\n");
    break;
  }
  case T_Equal: {
    printf("Equal\n");
    break;
  }
  case T_NotEqual: {
    printf("NotEqual\n");
    break;
  }
  case T_CompLess: {
    printf("CompLess\n");
    break;
  }
  case T_CompLessEqual: {
    printf("CompLessEqual\n");
    break;
  }
  case T_CompGreater: {
    printf("CompGreater\n");
    break;
  }
  case T_CompGreaterEqual: {
    printf("CompGreaterEqual\n");
    break;
  }
  case T_Ref: {
    printf("Ref\n");
    break;
  }
  case T_Deref: {
    printf("Deref\n");
    break;
  }
  case T_Assign: {
    printf("Assign\n");
    break;
  }
  case T_Pipe: {
    printf("Pipe\n");
    break;
  }
  case T_ParenLeft: {
    printf("ParenLeft\n");
    break;
  }
  case T_ParenRight: {
    printf("ParenRight\n");
    break;
  }
  case T_BracketLeft: {
    printf("BracketLeft\n");
    break;
  }
  case T_BracketRight: {
    printf("BracketRight\n");
    break;
  }
  case T_BraceLeft: {
    printf("BraceLeft\n");
    break;
  }
  case T_BraceRight: {
    printf("BraceRight\n");
    break;
  }
  case T_AttrLeft: {
    printf("AttrLeft\n");
    break;
  }
  case T_AttrRight: {
    printf("AttrRight\n");
    break;
  }
  case T_Dot: {
    printf("Dot\n");
    break;
  }
  case T_Comma: {
    printf("Comma\n");
    break;
  }
  case T_Colon: {
    printf("Colon\n");
    break;
  }
  case T_Semicolon: {
    printf("Semicolon\n");
    break;
  }
  case T_Underscore: {
    printf("Underscore\n");
    break;
  }
  case T_Macro: {
    printf("Macro: %s\n", token->macro);
    break;
  }
  case T_Comment: {
    printf("Comment: %s\n", token->comment);
    break;
  }
  }
}
