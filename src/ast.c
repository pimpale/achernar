#include "ast.h"
#include "com_assert.h"
#include "com_str.h"

com_str ast_strExprKind(ast_ExprKind val) {
  switch (val) {
  case ast_EK_None:
    return com_str_lit_m("ast_EK_None");
  case ast_EK_Nil:
    return com_str_lit_m("ast_EK_Nil");
  case ast_EK_Label:
    return com_str_lit_m("ast_EK_Label");
  case ast_EK_ModuleAccess:
    return com_str_lit_m("ast_EK_ModuleAccess");
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
  case ast_EK_Ret:
    return com_str_lit_m("ast_EK_Ret");
  case ast_EK_CaseOf:
    return com_str_lit_m("ast_EK_CaseOf");
  case ast_EK_Group:
    return com_str_lit_m("ast_EK_Group");
  case ast_EK_Reference:
    return com_str_lit_m("ast_EK_Reference");
  case ast_EK_BindIgnore:
    return com_str_lit_m("ast_EK_BindIgnore");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strIdentifierKind(ast_IdentifierKind val) {
  switch (val) {
  case ast_IK_None:
    return com_str_lit_m("ast_IK_None");
  case ast_IK_Identifier:
    return com_str_lit_m("ast_IK_Identifier");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strLabelKind(ast_LabelKind val) {
  switch (val) {
  case ast_LK_None:
    return com_str_lit_m("ast_LK_None");
  case ast_LK_Label:
    return com_str_lit_m("ast_LK_Label");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strExprBinaryOpKind(ast_ExprBinaryOpKind val) {

  switch (val) {
  case ast_EBOK_ModuleAccess:
    return com_str_lit_m("ast_EBOK_ModuleAccess");
  case ast_EBOK_At:
    return com_str_lit_m("ast_EBOK_At");
  case ast_EBOK_In:
    return com_str_lit_m("ast_EBOK_In");
  case ast_EBOK_CaseOption:
    return com_str_lit_m("ast_EBOK_CaseOption");
  case ast_EBOK_Pow:
    return com_str_lit_m("ast_EBOK_Pow");
  case ast_EBOK_PipeBackward:
    return com_str_lit_m("ast_EBOK_PipeBackward");
  case ast_EBOK_PipeForward:
    return com_str_lit_m("ast_EBOK_PipeForward");
  case ast_EBOK_Compose:
    return com_str_lit_m("ast_EBOK_Compose");
  case ast_EBOK_Assign:
    return com_str_lit_m("ast_EBOK_Assign");
  case ast_EBOK_Sequence:
    return com_str_lit_m("ast_EBOK_Sequence");
  case ast_EBOK_Apply:
    return com_str_lit_m("ast_EBOK_Apply");
  case ast_EBOK_RevApply:
    return com_str_lit_m("ast_EBOK_RevApply");
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
  case ast_EBOK_Product:
    return com_str_lit_m("ast_EBOK_Product");
  case ast_EBOK_Sum:
    return com_str_lit_m("ast_EBOK_Sum");
  case ast_EBOK_Range:
    return com_str_lit_m("ast_EBOK_Range");
  case ast_EBOK_RangeInclusive:
    return com_str_lit_m("ast_EBOK_RangeInclusive");
  }
  com_assert_unreachable_m("unreachable");
}
