#ifndef TOKEN_H
#define TOKEN_H

#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"

typedef enum {
  // These are not tokens, and do not contain token data
  tk_Eof,
  // if it's tk none you can't assume anything
  tk_None,
  // function, type, or variable
  tk_Identifier,
  // Keywords
  tk_Never,     // unreachable type for when a function does not return
  tk_Loop,      // loop
  tk_Match,     // match
  tk_Val,       // val
  tk_Template,  // template
  tk_Ret,       // ret
  tk_Defer,     // defer
  tk_Fn,        // fn
  tk_Pat,       // pat
  tk_As,        // as
  tk_Struct,    // struct
  tk_Enum,      // enum
  tk_Type,      // type
  tk_Typefn,    // typefn
  tk_Mod,       // mod
  tk_Use,       // use
  // Literals and constants
  tk_Nil,    // nil
  tk_Bool,   // true
  tk_String, // "string"
  tk_Char,   // 'a'
  tk_Float,  // 0.7
  tk_Int,    // 7
  // Math Operators
  tk_Add,    // +
  tk_Sub,    // -
  tk_Mul,    // *
  tk_IRem,   // %%
  tk_FRem,   // %.
  tk_IDiv,   // //
  tk_FDiv,   // /.
  // Logical Operators
  tk_And, // and
  tk_Or,  // or
  tk_Not, // not
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
  tk_AssignIDiv, // //=
  tk_AssignFDiv, // /.=
  tk_AssignIRem, // %%=
  tk_AssignFRem, // %.=
  // Arrows
  tk_Pipe,  // ->
  tk_Arrow, // =>
  // Scope resolution
  tk_MemberResolution, // ::
  tk_ModResolution,    // /
  // Type operators
  tk_Product,      // ,
  tk_Sum,          // |
  tk_Union,        // ||
  tk_Intersection, // ,,
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
  // Macros
  tk_Label, // 'label
  tk_Macro, // macroidentifier!
  // Comments, and Attributes
  tk_Metadata, // #{ comment }#, #comment and ##comment  ${ attribute }$, $attribute and $$attribute
} tk_Kind;


typedef struct Token_s {
  // The type of this token
  tk_Kind kind;
  // position in the file
  com_loc_Span span;
  // This points to
  // null terminated string in case of identifier, tk_StringLiteral,
  // tk_Comment, tk_Documentation, or tk_Annotation u64 in case of
  // tk_IntLiteral double double in case of tk_FloatLiteral Otherwise must
  // be null
  union {
    struct {
      com_str data;
    } identifierToken;
    struct {
      com_str data;
    } macroToken;
    struct {
      com_str data;
    } labelToken;
    struct {
      bool significant;
      com_str content;
    } metadataToken;
    struct {
      bool data;
    } boolToken;
    struct {
      com_str data;
    } stringToken;
    struct {
      u64 data;
    } intToken;
    struct {
      f64 data;
    } floatToken;
    struct {
      u8 data;
    } charToken;
  };
} Token;

com_str tk_strKind(tk_Kind val);

#endif
