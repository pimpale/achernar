#include "ast.h"
#include "com_assert.h"
#include "com_str.h"

com_str ast_strExprKind(ast_ExprKind val) {
  switch (val) {
  case ast_EK_None:
    return com_str_lit_m("ast_EK_None");
  case ast_EK_Struct:
    return com_str_lit_m("ast_EK_Struct");
  case ast_EK_Defer:
    return com_str_lit_m("ast_EK_Defer");
  case ast_EK_Int:
    return com_str_lit_m("ast_EK_Int");
  case ast_EK_Real:
    return com_str_lit_m("ast_EK_Real");
  case ast_EK_String:
    return com_str_lit_m("ast_EK_String");
  case ast_EK_Loop:
    return com_str_lit_m("ast_EK_Loop");
  case ast_EK_BinaryOp:
    return com_str_lit_m("ast_EK_BinaryOp");
  case ast_EK_UnaryOp:
    return com_str_lit_m("ast_EK_UnaryOp");
  case ast_EK_Ret:
    return com_str_lit_m("ast_EK_Ret");
  case ast_EK_Match:
    return com_str_lit_m("ast_EK_Match");
  case ast_EK_Block:
    return com_str_lit_m("ast_EK_Block");
  case ast_EK_FieldAccess:
    return com_str_lit_m("ast_EK_FieldAccess");
  case ast_EK_Reference:
    return com_str_lit_m("ast_EK_Reference");
  case ast_EK_BindIgnore:
    return com_str_lit_m("ast_EK_BindIgnore");
  case ast_EK_Bind:
    return com_str_lit_m("ast_EK_Bind");
  case ast_EK_AtBind:
    return com_str_lit_m("ast_EK_AtBind");
  }
}

com_str ast_strIdentifierKind(ast_IdentifierKind val) {
  switch (val) {
  case ast_IK_None:
    return com_str_lit_m("ast_IK_None");
  case ast_IK_Identifier:
    return com_str_lit_m("ast_IK_Identifier");
  }
}

com_str ast_strLabelKind(ast_LabelKind val) {
  switch (val) {
  case ast_LK_None:
    return com_str_lit_m("ast_LK_None");
  case ast_LK_Omitted:
    return com_str_lit_m("ast_LK_Omitted");
  case ast_LK_Label:
    return com_str_lit_m("ast_LK_Label");
  }
}

com_str ast_strCompoundElementKind(ast_CompoundElementKind val) {
  switch (val) {
  case ast_CEK_None:
    return com_str_lit_m("ast_CEK_None");
  case ast_CEK_Element:
    return com_str_lit_m("ast_CEK_Element");
  }
}

com_str ast_strExprUnaryOpKind(ast_ExprUnaryOpKind val) {
  switch (val) {
  case ast_EUOK_None:
    return com_str_lit_m("ast_EUOK_None");
  case ast_EUOK_Mut:
    return com_str_lit_m("ast_EUOK_Mut");
  case ast_EUOK_Not:
    return com_str_lit_m("ast_EUOK_Not");
  case ast_EUOK_Ref:
    return com_str_lit_m("ast_EUOK_Ref");
  case ast_EUOK_Deref:
    return com_str_lit_m("ast_EUOK_Deref");
  case ast_EUOK_Copy:
    return com_str_lit_m("ast_EUOK_Copy");
  }
}

com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val) {

  switch (val) {
  case ast_EBOK_Assign:
    return com_str_lit_m("ast_EBOK_Assign");
  case ast_EBOK_Sequence:
    return com_str_lit_m("ast_EBOK_Sequence");
  case ast_EBOK_Call:
    return com_str_lit_m("ast_EBOK_Call");
  case ast_EBOK_Pipe:
    return com_str_lit_m("ast_EBOK_Pipe");
  case ast_EBOK_None:
    return com_str_lit_m("ast_EBOK_None");
  case ast_EBOK_Constrain:
    return com_str_lit_m("ast_EBOK_Constrain");
  case ast_EBOK_Fn:
    return com_str_lit_m("ast_EBOK_Fn");
  case ast_EBOK_Add:
    return com_str_lit_m("ast_EBOK_Add");
  case ast_EBOK_Sub:
    return com_str_lit_m("ast_EBOK_Sub");
  case ast_EBOK_Mul:
    return com_str_lit_m("ast_EBOK_Mul");
  case ast_EBOK_Div:
    return com_str_lit_m("ast_EBOK_Div");
  case ast_EBOK_Rem:
    return com_str_lit_m("ast_EBOK_Rem");
  case ast_EBOK_And:
    return com_str_lit_m("ast_EBOK_And");
  case ast_EBOK_Or:
    return com_str_lit_m("ast_EBOK_Or");
  case ast_EBOK_Xor:
    return com_str_lit_m("ast_EBOK_Xor");
  case ast_EBOK_CompEqual:
    return com_str_lit_m("ast_EBOK_CompEqual");
  case ast_EBOK_CompNotEqual:
    return com_str_lit_m("ast_EBOK_CompNotEqual");
  case ast_EBOK_CompLess:
    return com_str_lit_m("ast_EBOK_CompLess");
  case ast_EBOK_CompLessEqual:
    return com_str_lit_m("ast_EBOK_CompLessEqual");
  case ast_EBOK_CompGreater:
    return com_str_lit_m("ast_EBOK_CompGreater");
  case ast_EBOK_CompGreaterEqual:
    return com_str_lit_m("ast_EBOK_CompGreaterEqual");
  case ast_EBOK_Union:
    return com_str_lit_m("ast_EBOK_Union");
  case ast_EBOK_Difference:
    return com_str_lit_m("ast_EBOK_Difference");
  case ast_EBOK_Intersection:
    return com_str_lit_m("ast_EBOK_Intersection");
  case ast_EBOK_SymDifference:
    return com_str_lit_m("ast_EBOK_SymDifference");
  case ast_EBOK_Product:
    return com_str_lit_m("ast_EBOK_Product");
  case ast_EBOK_Sum:
    return com_str_lit_m("ast_EBOK_Sum");
  case ast_EBOK_Range:
    return com_str_lit_m("ast_EBOK_Range");
  case ast_EBOK_RangeInclusive:
    return com_str_lit_m("ast_EBOK_RangeInclusive");
  }
}
