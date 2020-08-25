#include "ast.h"
#include "com_assert.h"
#include "com_str.h"

com_str ast_strPatValRestrictionKind(ast_PatValRestrictionKind val) {
  switch (val) {
  case ast_PEVRK_CompEqual:
    return com_str_lit_m("CompEqual");
  case ast_PEVRK_CompNotEqual:
    return com_str_lit_m("CompNotEqual");
  case ast_PEVRK_CompLess:
    return com_str_lit_m("CompLess");
  case ast_PEVRK_CompLessEqual:
    return com_str_lit_m("CompLessEqual");
  case ast_PEVRK_CompGreater:
    return com_str_lit_m("CompGreater");
  case ast_PEVRK_CompGreaterEqual:
    return com_str_lit_m("CompGreaterEqual");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strPatKind(ast_PatKind val) {
  switch (val) {
  case ast_PK_None:
    return com_str_lit_m("ast_PK_None");
  case ast_PK_Macro:
    return com_str_lit_m("ast_PK_Macro");
  case ast_PK_ValRestriction:
    return com_str_lit_m("ast_PK_ValRestriction");
  case ast_PK_TypeRestriction:
    return com_str_lit_m("ast_PK_TypeRestriction");
  case ast_PK_Struct:
    return com_str_lit_m("ast_PK_Struct");
  case ast_PK_Group:
    return com_str_lit_m("ast_PK_Group");
  case ast_PK_UnaryOp:
    return com_str_lit_m("ast_PK_UnaryOp");
  case ast_PK_BinaryOp:
    return com_str_lit_m("ast_PK_BinaryOp");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strPatBinaryOpKind(ast_PatBinaryOpKind val) {
  switch (val) {
  case ast_PEBOK_Product:
    return com_str_lit_m("ast_PEBOK_Product");
  case ast_PEBOK_Sum:
    return com_str_lit_m("ast_PEBOK_Sum");
  case ast_PEBOK_Union:
    return com_str_lit_m("ast_PEBOK_Union");
  case ast_PEBOK_Intersection:
    return com_str_lit_m("ast_PEBOK_Intersection");
  case ast_PEBOK_And:
    return com_str_lit_m("ast_PEBOK_And");
  case ast_PEBOK_Or:
    return com_str_lit_m("ast_PEBOK_Or");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strPatStructMemberKind(ast_PatStructMemberKind val) {
  switch (val) {
  case ast_PSMK_None:
    return com_str_lit_m("None");
  case ast_PSMK_Macro:
    return com_str_lit_m("Macro");
  case ast_PSMK_Field:
    return com_str_lit_m("Field");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strTypeKind(ast_TypeKind val) {
  switch (val) {
  case ast_TK_None:
    return com_str_lit_m("ast_TK_None");
  case ast_TK_Omitted:
    return com_str_lit_m("ast_TK_Omitted");
  case ast_TK_Macro:
    return com_str_lit_m("ast_TK_Macro");
  case ast_TK_Nil:
    return com_str_lit_m("ast_TK_Nil");
  case ast_TK_Never:
    return com_str_lit_m("ast_TK_Never");
  case ast_TK_Group:
    return com_str_lit_m("ast_TK_Group");
  case ast_TK_Reference:
    return com_str_lit_m("ast_TK_Reference");
  case ast_TK_Struct:
    return com_str_lit_m("ast_TK_Struct");
  case ast_TK_Fn:
    return com_str_lit_m("ast_TK_Fn");
  case ast_TK_UnaryOp:
    return com_str_lit_m("ast_TK_UnaryOp");
  case ast_TK_BinaryOp:
    return com_str_lit_m("ast_TK_BinaryOp");
  case ast_TK_FieldAccess:
    return com_str_lit_m("ast_TK_FieldAccess");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strTypeStructKind(ast_TypeStructKind val) {
  switch (val) {
  case ast_TSK_Struct:
    return com_str_lit_m("Struct");
  case ast_TSK_Enum:
    return com_str_lit_m("Enum");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strTypeStructMemberKind(ast_TypeStructMemberKind val) {
  switch (val) {
  case ast_TSMK_None:
    return com_str_lit_m("None");
  case ast_TSMK_Macro:
    return com_str_lit_m("Macro");
  case ast_TSMK_StructMember:
    return com_str_lit_m("StructMember");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strTypeUnaryOpKind(ast_TypeUnaryOpKind val) {
  switch (val) {
  case ast_TEUOK_Ref:
    return com_str_lit_m("Ref");
  case ast_TEUOK_Deref:
    return com_str_lit_m("Deref");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strTypeBinaryOpKind(ast_TypeBinaryOpKind val) {
  switch (val) {
  case ast_TEBOK_Product:
    return com_str_lit_m("ast_TEBOK_Product");
  case ast_TEBOK_Sum:
    return com_str_lit_m("ast_TEBOK_Sum");
  case ast_TEBOK_Union:
    return com_str_lit_m("ast_TEBOK_Union");
  case ast_TEBOK_Intersection:
    return com_str_lit_m("ast_TEBOK_Intersection");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strValKind(ast_ValKind val) {
  switch (val) {
  case ast_VK_None:
    return com_str_lit_m("ast_VK_None");
  case ast_VK_Macro:
    return com_str_lit_m("ast_VK_Macro");
  case ast_VK_NilLiteral:
    return com_str_lit_m("ast_VK_NilLiteral");
  case ast_VK_BoolLiteral:
    return com_str_lit_m("ast_VK_BoolLiteral");
  case ast_VK_IntLiteral:
    return com_str_lit_m("ast_VK_IntLiteral");
  case ast_VK_FloatLiteral:
    return com_str_lit_m("ast_VK_FloatLiteral");
  case ast_VK_CharLiteral:
    return com_str_lit_m("ast_VK_CharLiteral");
  case ast_VK_Fn:
    return com_str_lit_m("ast_VK_Fn");
  case ast_VK_Loop:
    return com_str_lit_m("ast_VK_Loop");
  case ast_VK_As:
    return com_str_lit_m("ast_VK_As");
  case ast_VK_StringLiteral:
    return com_str_lit_m("ast_VK_StringLiteral");
  case ast_VK_StructLiteral:
    return com_str_lit_m("ast_VK_StructLiteral");
  case ast_VK_BinaryOp:
    return com_str_lit_m("ast_VK_BinaryOp");
  case ast_VK_UnaryOp:
    return com_str_lit_m("ast_VK_UnaryOp");
  case ast_VK_Call:
    return com_str_lit_m("ast_VK_Call");
  case ast_VK_Pipe:
    return com_str_lit_m("ast_VK_Pipe");
  case ast_VK_Ret:
    return com_str_lit_m("ast_VK_Ret");
  case ast_VK_Match:
    return com_str_lit_m("ast_VK_Match");
  case ast_VK_Block:
    return com_str_lit_m("ast_VK_Block");
  case ast_VK_FieldAccess:
    return com_str_lit_m("ast_VK_FieldAccess");
  case ast_VK_Reference:
    return com_str_lit_m("ast_VK_Reference");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strLabelBindingKind(ast_LabelBindingKind val) {
  switch (val) {
  case ast_LBK_Omitted:
    return com_str_lit_m("Omitted");
  case ast_LBK_Label:
    return com_str_lit_m("Label");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strLabelReferenceKind(ast_LabelReferenceKind val) {
  switch (val) {
  case ast_LRK_None:
    return com_str_lit_m("None");
  case ast_LRK_Label:
    return com_str_lit_m("Label");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strMatchCaseKind(ast_MatchCaseKind val) {
  switch (val) {
  case ast_MCK_None:
    return com_str_lit_m("None");
  case ast_MCK_Case:
    return com_str_lit_m("Case");
  case ast_MCK_Macro:
    return com_str_lit_m("Macro");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strValStructMemberKind(ast_ValStructMemberKind val) {
  switch (val) {
  case ast_VSMK_None:
    return com_str_lit_m("None");
  case ast_VSMK_Macro:
    return com_str_lit_m("Macro");
  case ast_VSMK_Member:
    return com_str_lit_m("Member");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strValUnaryOpKind(ast_ValUnaryOpKind val) {
  switch (val) {
  case ast_VEUOK_Not:
    return com_str_lit_m("Not");
  case ast_VEUOK_Ref:
    return com_str_lit_m("Ref");
  case ast_VEUOK_Deref:
    return com_str_lit_m("Deref");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strValBinaryOpKind(ast_ValBinaryOpKind val) {
  switch (val) {
  case ast_VEBOK_Add:
    return com_str_lit_m("ast_VEBOK_Add");
  case ast_VEBOK_Sub:
    return com_str_lit_m("ast_VEBOK_Sub");
  case ast_VEBOK_Mul:
    return com_str_lit_m("ast_VEBOK_Mul");
  case ast_VEBOK_IDiv:
    return com_str_lit_m("ast_VEBOK_IDiv");
  case ast_VEBOK_FDiv:
    return com_str_lit_m("ast_VEBOK_FDiv");
  case ast_VEBOK_IRem:
    return com_str_lit_m("ast_VEBOK_IRem");
  case ast_VEBOK_FRem:
    return com_str_lit_m("ast_VEBOK_FRem");
  case ast_VEBOK_And:
    return com_str_lit_m("ast_VEBOK_And");
  case ast_VEBOK_Or:
    return com_str_lit_m("ast_VEBOK_Or");
  case ast_VEBOK_Xor:
    return com_str_lit_m("ast_VEBOK_Xor");
  case ast_VEBOK_CompEqual:
    return com_str_lit_m("ast_VEBOK_CompEqual");
  case ast_VEBOK_CompNotEqual:
    return com_str_lit_m("ast_VEBOK_CompNotEqual");
  case ast_VEBOK_CompLess:
    return com_str_lit_m("ast_VEBOK_CompLess");
  case ast_VEBOK_CompLessEqual:
    return com_str_lit_m("ast_VEBOK_CompLessEqual");
  case ast_VEBOK_CompGreater:
    return com_str_lit_m("ast_VEBOK_CompGreater");
  case ast_VEBOK_CompGreaterEqual:
    return com_str_lit_m("ast_VEBOK_CompGreaterEqual");
  case ast_VEBOK_Pipeline:
    return com_str_lit_m("ast_VEBOK_Pipeline");
  case ast_VEBOK_Assign:
    return com_str_lit_m("ast_VEBOK_Assign");
  case ast_VEBOK_AssignAdd:
    return com_str_lit_m("ast_VEBOK_AssignAdd");
  case ast_VEBOK_AssignSub:
    return com_str_lit_m("ast_VEBOK_AssignSub");
  case ast_VEBOK_AssignMul:
    return com_str_lit_m("ast_VEBOK_AssignMul");
  case ast_VEBOK_AssignIDiv:
    return com_str_lit_m("ast_VEBOK_AssignIDiv");
  case ast_VEBOK_AssignFDiv:
    return com_str_lit_m("ast_VEBOK_AssignFDiv");
  case ast_VEBOK_AssignIRem:
    return com_str_lit_m("ast_VEBOK_AssignIRem");
  case ast_VEBOK_AssignFRem:
    return com_str_lit_m("ast_VEBOK_AssignFRem");
  case ast_VEBOK_Product:
    return com_str_lit_m("ast_VEBOK_Product");
  case ast_VEBOK_Union:
    return com_str_lit_m("ast_VEBOK_Union");
  case ast_VEBOK_Intersection:
    return com_str_lit_m("ast_VEBOK_Intersection");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strStmntKind(ast_StmntKind val) {
  switch (val) {
  case ast_SK_None:
    return com_str_lit_m("ast_SK_None");
  case ast_SK_Use:
    return com_str_lit_m("ast_SK_Use");
  case ast_SK_Macro:
    return com_str_lit_m("ast_SK_Macro");
  case ast_SK_Mod:
    return com_str_lit_m("ast_SK_Mod");
  case ast_SK_ValDecl:
    return com_str_lit_m("ast_SK_ValDecl");
  case ast_SK_ValDeclDefine:
    return com_str_lit_m("ast_SK_ValDeclDefine");
  case ast_SK_TypeDecl:
    return com_str_lit_m("ast_SK_TypeDecl");
  case ast_SK_Val:
    return com_str_lit_m("ast_SK_Val");
  case ast_SK_DeferStmnt:
    return com_str_lit_m("ast_SK_DeferStmnt");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strPatUnaryOpKind(ast_PatUnaryOpKind val) {
  switch (val) {
  case ast_PEUOK_Not:
    return com_str_lit_m("ast_PEUOK_Not");
  case ast_PEUOK_Ref:
    return com_str_lit_m("ast_PEUOK_Ref");
  case ast_PEUOK_Deref:
    return com_str_lit_m("ast_PEUOK_Deref");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strBindingKind(ast_BindingKind val) {
  switch (val) {
  case ast_BK_None:
    return com_str_lit_m("None");
  case ast_BK_Bind:
    return com_str_lit_m("Bind");
  case ast_BK_Ignore:
    return com_str_lit_m("Ignore");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strReferenceKind(ast_ReferenceKind val) {
  switch (val) {
  case ast_RK_None:
    return com_str_lit_m("None");
  case ast_RK_Reference:
    return com_str_lit_m("Reference");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strFieldKind(ast_FieldKind val) {
  switch (val) {
  case ast_FK_None:
    return com_str_lit_m("ast_FK_None");
  case ast_FK_FieldStr:
    return com_str_lit_m("ast_FK_FieldStr");
  case ast_FK_FieldInt:
    return com_str_lit_m("ast_FK_FieldInt");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strModReferenceKind(ast_ModReferenceKind val) {
  switch (val) {
  case ast_MRK_None:
    return com_str_lit_m("ast_MRK_None");
  case ast_MRK_Omitted:
    return com_str_lit_m("ast_MRK_Omitted");
  case ast_MRK_Reference:
    return com_str_lit_m("ast_MRK_Reference");
  }
  com_assert_unreachable_m("unreachable");
}

com_str ast_strModBindingKind(ast_ModBindingKind val) {
  switch (val) {
  case ast_MBK_None:
    return com_str_lit_m("ast_MBK_None");
  case ast_MBK_Binding:
    return com_str_lit_m("ast_MBK_Binding");
  }
  com_assert_unreachable_m("unreachable");
}
