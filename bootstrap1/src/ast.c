#include "ast.h"
#include <stdlib.h>

const char *strPatValRestrictionKind(ast_PatValRestrictionKind val) {
  switch (val) {
  case ast_PEVRK_CompEqual:
    return "CompEqual";
  case ast_PEVRK_CompNotEqual:
    return "CompNotEqual";
  case ast_PEVRK_CompLess:
    return "CompLess";
  case ast_PEVRK_CompLessEqual:
    return "CompLessEqual";
  case ast_PEVRK_CompGreater:
    return "CompGreater";
  case ast_PEVRK_CompGreaterEqual:
    return "CompGreaterEqual";
  }
  abort();
}

const char *strPatKind(ast_PatKind val) {
  switch (val) {
  case ast_PK_None:
    return "None";
  case ast_PK_Macro:
    return "Macro";
  case ast_PK_ValRestriction:
    return "ValRestriction";
  case ast_PK_TypeRestriction:
    return "TypeRestriction";
  case ast_PK_TypeRestrictionBinding:
    return "TypeRestrictionBinding";
  case ast_PK_Struct:
    return "Struct";
  case ast_PK_Group:
    return "Group";
  case ast_PK_UnaryOp:
    return "UnaryOp";
  case ast_PK_BinaryOp:
    return "BinaryOp";
  }
  abort();
}

const char *strPatBinaryOpKind(ast_PatBinaryOpKind val) {
  switch (val) {
  case ast_PEBOK_Tuple:
    return "Tuple";
  case ast_PEBOK_Union:
    return "Union";
  case ast_PEBOK_And:
    return "And";
  case ast_PEBOK_Or:
    return "Or";
  }
  abort();
}

const char *strPatStructMemberKind(ast_PatStructMemberKind val) {
  switch (val) {
  case ast_PSMK_None:
    return "None";
  case ast_PSMK_Macro:
    return "Macro";
  case ast_PSMK_Field:
    return "Field";
  case ast_PSMK_Rest:
    return "Rest";
  }
  abort();
}

const char *strTypeKind(ast_TypeKind val) {
  switch (val) {
  case ast_TK_None:
    return "None";
  case ast_TK_Omitted:
    return "Omitted";
  case ast_TK_Macro:
    return "Macro";
  case ast_TK_Never:
    return "Never";
  case ast_TK_Nil:
    return "Nil";
  case ast_TK_Group:
    return "Group";
  case ast_TK_Reference:
    return "Reference";
  case ast_TK_Struct:
    return "Struct";
  case ast_TK_Fn:
    return "Fn";
  case ast_TK_UnaryOp:
    return "UnaryOp";
  case ast_TK_BinaryOp:
    return "BinaryOp";
  case ast_TK_FieldAccess:
    return "FieldAccess";
  }
  abort();
}

const char *strTypeStructKind(ast_TypeStructKind val) {
  switch (val) {
  case ast_TSK_Struct:
    return "Struct";
  case ast_TSK_Enum:
    return "Enum";
  }
  abort();
}

const char *strTypeStructMemberKind(ast_TypeStructMemberKind val) {
  switch (val) {
  case ast_TSMK_None:
    return "None";
  case ast_TSMK_Macro:
    return "Macro";
  case ast_TSMK_StructMember:
    return "StructMember";
  }
  abort();
}

const char *strTypeUnaryOpKind(ast_TypeUnaryOpKind val) {
  switch (val) {
  case ast_TEUOK_Ref:
    return "Ref";
  case ast_TEUOK_Deref:
    return "Deref";
  }
  abort();
}

const char *strTypeBinaryOpKind(ast_TypeBinaryOpKind val) {
  switch (val) {
  case ast_TEBOK_Tuple:
    return "Tuple";
  case ast_TEBOK_Union:
    return "Union";
  }
  abort();
}

const char *strValKind(ast_ValKind val) {
  switch (val) {
  case ast_VK_None:
    return "None";
  case ast_VK_Macro:
    return "Macro";
  case ast_VK_NilLiteral:
    return "NilLiteral";
  case ast_VK_BoolLiteral:
    return "BoolLiteral";
  case ast_VK_IntLiteral:
    return "IntLiteral";
  case ast_VK_FloatLiteral:
    return "FloatLiteral";
  case ast_VK_CharLiteral:
    return "CharLiteral";
  case ast_VK_Fn:
    return "Fn";
  case ast_VK_Loop:
    return "Loop";
  case ast_VK_As:
    return "As";
  case ast_VK_StringLiteral:
    return "StringLiteral";
  case ast_VK_StructLiteral:
    return "StructLiteral";
  case ast_VK_BinaryOp:
    return "BinaryOp";
  case ast_VK_UnaryOp:
    return "UnaryOp";
  case ast_VK_Call:
    return "Call";
  case ast_VK_Return:
    return "Return";
  case ast_VK_Match:
    return "Match";
  case ast_VK_Block:
    return "Block";
  case ast_VK_FieldAccess:
    return "FieldAccess";
  case ast_VK_Reference:
    return "Reference";
  }
  abort();
}

const char *strLabelKind(ast_LabelKind val) {
  switch (val) {
  case ast_LK_Omitted:
    return "Omitted";
  case ast_LK_Label:
    return "Label";
  }
  abort();
}

const char *strMatchCaseKind(ast_MatchCaseKind val) {
  switch (val) {
  case ast_MCK_None:
    return "None";
  case ast_MCK_Case:
    return "Case";
  case ast_MCK_Macro:
    return "Macro";
  }
  abort();
}

const char *strValStructMemberKind(ast_ValStructMemberKind val) {
  switch (val) {
  case ast_VSMK_None:
    return "None";
  case ast_VSMK_Macro:
    return "Macro";
  case ast_VSMK_Member:
    return "Member";
  }
  abort();
}

const char *strValUnaryOpKind(ast_ValUnaryOpKind val) {
  switch (val) {
  case ast_VEUOK_Negate:
    return "Negate";
  case ast_VEUOK_Posit:
    return "Posit";
  case ast_VEUOK_Not:
    return "Not";
  case ast_VEUOK_Ref:
    return "Ref";
  case ast_VEUOK_Deref:
    return "Deref";
  }
  abort();
}

const char *strValBinaryOpKind(ast_ValBinaryOpKind val) {
  switch (val) {
  case ast_VEBOK_Add:
    return "Add";
  case ast_VEBOK_Sub:
    return "Sub";
  case ast_VEBOK_Mul:
    return "Mul";
  case ast_VEBOK_Div:
    return "Div";
  case ast_VEBOK_Mod:
    return "Mod";
  case ast_VEBOK_And:
    return "And";
  case ast_VEBOK_Or:
    return "Or";
  case ast_VEBOK_CompEqual:
    return "CompEqual";
  case ast_VEBOK_CompNotEqual:
    return "CompNotEqual";
  case ast_VEBOK_CompLess:
    return "CompLess";
  case ast_VEBOK_CompLessEqual:
    return "CompLessEqual";
  case ast_VEBOK_CompGreater:
    return "CompGreater";
  case ast_VEBOK_CompGreaterEqual:
    return "CompGreaterEqual";
  case ast_VEBOK_Pipeline:
    return "Pipeline";
  case ast_VEBOK_Assign:
    return "Assign";
  case ast_VEBOK_AssignAdd:
    return "AssignAdd";
  case ast_VEBOK_AssignSub:
    return "AssignSub";
  case ast_VEBOK_AssignMul:
    return "AssignMul";
  case ast_VEBOK_AssignDiv:
    return "AssignDiv";
  case ast_VEBOK_AssignMod:
    return "AssignMod";
  case ast_VEBOK_Tuple:
    return "Tuple";
  }
  abort();
}

const char *strStmntKind(ast_StmntKind val) {
  switch (val) {
  case ast_SK_None:
    return "None";
  case ast_SK_Use:
    return "Use";
  case ast_SK_Macro:
    return "Macro";
  case ast_SK_Namespace:
    return "Namespace";
  case ast_SK_ValDecl:
    return "ValDecl";
  case ast_SK_ValDeclDefine:
    return "ValDeclDefine";
  case ast_SK_TypeDecl:
    return "TypeDecl";
  case ast_SK_Val:
    return "Val";
  case ast_SK_DeferStmnt:
    return "DeferStmnt";
  }
  abort();
}

const char *strPatUnaryOpKind(ast_PatUnaryOpKind val) {
  switch (val) {
  case ast_PEUOK_Not:
    return "Not";
  }
  abort();
}
