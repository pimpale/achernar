#ifndef TOKEN_H
#define TOKEN_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "diagnostic.h"
#include "lncol.h"

typedef enum {
  // These are not tokens, and do not contain token data
  tk_Eof,
  tk_None,
  // function, type, or variable
  tk_Identifier,
  // Keywords
  tk_Unreachable, // unreachable type for when a function does not return
  tk_Loop,        // loop
  tk_Match,       // match
  tk_Val,         // val
  tk_Return,      // return
  tk_Defer,       // defer
  tk_Fn,          // fn
  tk_Pat,         // pat
  tk_As,          // as
  tk_Struct,      // struct
  tk_Enum,        // enum
  tk_Type,        // type
  tk_Namespace,   // namespace
  tk_Use,         // use
  // Literals and constants
  tk_Nil,    // nil
  tk_Bool,   // true
  tk_String, // "string"
  tk_Char,   // 'a'
  tk_Float,  // 0.7
  tk_Int,    // 7
  // Math Operators
  // Unary
  tk_Negate, // --
  tk_Posit,  // ++
  tk_Add,    // +
  tk_Sub,    // -
  tk_Mul,    // *
  tk_Div,    // /
  tk_Mod,    // %
  // Logical Operators
  tk_And, // &&
  tk_Or,  // ||
  tk_Not, // !
  // Comparison and Equality
  tk_CompEqual,        // ==
  tk_CompNotEqual,     // !=
  tk_CompLess,         // <
  tk_CompLessEqual,    // <=
  tk_CompGreater,      // >
  tk_CompGreaterEqual, // >=
  // Type Modifiers
  tk_Ref,   // &
  tk_Deref, // @
  // Assignment
  tk_Define,    // :=
  tk_Assign,    // =
  tk_AssignAdd, // +=
  tk_AssignSub, // -=
  tk_AssignMul, // *=
  tk_AssignDiv, // /=
  tk_AssignMod, // %=
  // Arrows
  tk_Pipe,  // ->
  tk_Arrow, // =>
  // Scope resolution
  tk_ScopeResolution, // ::
  // Types
  tk_Tuple, // ,
  tk_Union, // |
  // Other Miscellaneous Operator Things
  tk_ParenLeft,    // (
  tk_ParenRight,   // )
  tk_BracketLeft,  // [
  tk_BracketRight, // ]
  tk_BraceLeft,    // {
  tk_BraceRight,   // }
  tk_FieldAccess,  // .
  tk_Colon,        // :
  tk_Semicolon,    // ;
  tk_Underscore,   // _
  tk_Backtick,     // `
  tk_Rest,         // ..
  tk_Dollar,       // $
  // Macros
  tk_Label, // 'label
  tk_Macro, // macroidentifier!
  // Comments, and Attributes
  tk_Comment, // #{ comment }# and # comment
} tk_Kind;

static const char *tk_strKind(tk_Kind val) {
  switch (val) {
  case tk_Eof:
    return "Eof";
  case tk_None:
    return "None";
  case tk_Identifier:
    return "Identifier";
  case tk_Unreachable:
    return "Unreachable";
  case tk_Loop:
    return "Loop";
  case tk_Match:
    return "Match";
  case tk_Val:
    return "Val";
  case tk_Return:
    return "Return";
  case tk_Defer:
    return "Defer";
  case tk_Fn:
    return "Fn";
  case tk_Pat:
    return "Pat";
  case tk_As:
    return "As";
  case tk_Struct:
    return "Struct";
  case tk_Enum:
    return "Enum";
  case tk_Type:
    return "Type";
  case tk_Namespace:
    return "Namespace";
  case tk_Use:
    return "Use";
  case tk_Nil:
    return "Nil";
  case tk_Bool:
    return "Bool";
  case tk_String:
    return "String";
  case tk_Char:
    return "Char";
  case tk_Float:
    return "Float";
  case tk_Int:
    return "Int";
  case tk_Negate:
    return "Negate";
  case tk_Posit:
    return "Posit";
  case tk_Add:
    return "Add";
  case tk_Sub:
    return "Sub";
  case tk_Mul:
    return "Mul";
  case tk_Div:
    return "Div";
  case tk_Mod:
    return "Mod";
  case tk_And:
    return "And";
  case tk_Or:
    return "Or";
  case tk_Not:
    return "Not";
  case tk_CompEqual:
    return "CompEqual";
  case tk_CompNotEqual:
    return "CompNotEqual";
  case tk_CompLess:
    return "CompLess";
  case tk_CompLessEqual:
    return "CompLessEqual";
  case tk_CompGreater:
    return "CompGreater";
  case tk_CompGreaterEqual:
    return "CompGreaterEqual";
  case tk_Ref:
    return "Ref";
  case tk_Deref:
    return "Deref";
  case tk_Define:
    return "Define";
  case tk_Assign:
    return "Assign";
  case tk_AssignAdd:
    return "AssignAdd";
  case tk_AssignSub:
    return "AssignSub";
  case tk_AssignMul:
    return "AssignMul";
  case tk_AssignDiv:
    return "AssignDiv";
  case tk_AssignMod:
    return "AssignMod";
  case tk_Pipe:
    return "Pipe";
  case tk_Arrow:
    return "Arrow";
  case tk_ScopeResolution:
    return "ScopeResolution";
  case tk_Tuple:
    return "Tuple";
  case tk_Union:
    return "Union";
  case tk_ParenLeft:
    return "ParenLeft";
  case tk_ParenRight:
    return "ParenRight";
  case tk_BracketLeft:
    return "BracketLeft";
  case tk_BracketRight:
    return "BracketRight";
  case tk_BraceLeft:
    return "BraceLeft";
  case tk_BraceRight:
    return "BraceRight";
  case tk_FieldAccess:
    return "FieldAccess";
  case tk_Colon:
    return "Colon";
  case tk_Semicolon:
    return "Semicolon";
  case tk_Underscore:
    return "Underscore";
  case tk_Backtick:
    return "Backtick";
  case tk_Rest:
    return "Rest";
  case tk_Dollar:
    return "Dollar";
  case tk_Label:
    return "Label";
  case tk_Macro:
    return "Macro";
  case tk_Comment:
    return "Comment";
  }
}

typedef struct Token_s {
  tk_Kind kind; // The type of this token
  Span span;    // position in the file
  // This points to
  // null terminated string in case of identifier, tk_StringLiteral,
  // tk_Comment, tk_Documentation, or tk_Annotation uint64_t in case of
  // tk_IntLiteral double double in case of tk_FloatLiteral Otherwise must
  // be null
  union {
    struct {
      char *data;
    } identifierToken;
    struct {
      char *data;
    } macroToken;
    struct {
      char *data;
    } labelToken;
    struct {
      char *comment;
      char *scope;
    } commentToken;
    struct {
      bool data;
    } boolToken;
    struct {
      char *data;
      size_t data_len;
    } stringToken;
    struct {
      uint64_t data;
    } intToken;
    struct {
      double data;
    } floatToken;
    struct {
      char data;
    } charToken;
  };
} Token;

#endif
