#include "ast.h"
#include <stdlib.h>

const char *strPatExprValRestrictionKind(PatExprValRestrictionKind val) {
  switch (val) {
  case PEVRK_CompEqual:
    return "CompEqual";
  case PEVRK_CompNotEqual:
    return "CompNotEqual";
  case PEVRK_CompLess:
    return "CompLess";
  case PEVRK_CompLessEqual:
    return "CompLessEqual";
  case PEVRK_CompGreater:
    return "CompGreater";
  case PEVRK_CompGreaterEqual:
    return "CompGreaterEqual";
  }
  abort();
}

const char *strPatExprKind(PatExprKind val) {
  switch (val) {
  case PEK_None:
    return "None";
  case PEK_Macro:
    return "Macro";
  case PEK_ValRestriction:
    return "ValRestriction";
  case PEK_TypeRestriction:
    return "TypeRestriction";
  case PEK_TypeRestrictionBinding:
    return "TypeRestrictionBinding";
  case PEK_Struct:
    return "Struct";
  case PEK_Group:
    return "Group";
  case PEK_UnaryOp:
    return "UnaryOp";
  case PEK_BinaryOp:
    return "BinaryOp";
  }
  abort();
}

const char *strPatExprBinaryOpKind(PatExprBinaryOpKind val) {
  switch (val) {
  case PEBOK_Tuple:
    return "Tuple";
  case PEBOK_Union:
    return "Union";
  case PEBOK_And:
    return "And";
  case PEBOK_Or:
    return "Or";
  }
  abort();
}

const char *strPatStructMemberExprKind(PatStructMemberExprKind val) {
  switch (val) {
  case PSMEK_None:
    return "None";
  case PSMEK_Macro:
    return "Macro";
  case PSMEK_Field:
    return "Field";
  case PSMEK_Rest:
    return "Rest";
  }
  abort();
}

const char *strTypeExprKind(TypeExprKind val) {
  switch (val) {
  case TEK_None:
    return "None";
  case TEK_Omitted:
    return "Omitted";
  case TEK_Macro:
    return "Macro";
  case TEK_Nil:
    return "Nil";
  case TEK_Group:
    return "Group";
  case TEK_Reference:
    return "Reference";
  case TEK_Struct:
    return "Struct";
  case TEK_Fn:
    return "Fn";
  case TEK_UnaryOp:
    return "UnaryOp";
  case TEK_BinaryOp:
    return "BinaryOp";
  case TEK_FieldAccess:
    return "FieldAccess";
  }
  abort();
}

const char *strTypeStructExprKind(TypeStructExprKind val) {
  switch (val) {
  case TSEK_Struct:
    return "Struct";
  case TSEK_Enum:
    return "Enum";
  }
  abort();
}

const char *strTypeStructMemberExprKind(TypeStructMemberExprKind val) {
  switch (val) {
  case TSMEK_None:
    return "None";
  case TSMEK_Macro:
    return "Macro";
  case TSMEK_StructMember:
    return "StructMember";
  }
  abort();
}

const char *strTypeExprUnaryOpKind(TypeExprUnaryOpKind val) {
  switch (val) {
  case TEUOK_Ref:
    return "Ref";
  case TEUOK_Deref:
    return "Deref";
  }
  abort();
}

const char *strTypeExprBinaryOpKind(TypeExprBinaryOpKind val) {
  switch (val) {
  case TEBOK_Tuple:
    return "Tuple";
  case TEBOK_Union:
    return "Union";
  }
  abort();
}

const char *strValExprKind(ValExprKind val) {
  switch (val) {
  case VEK_None:
    return "None";
  case VEK_Macro:
    return "Macro";
  case VEK_NilLiteral:
    return "NilLiteral";
  case VEK_BoolLiteral:
    return "BoolLiteral";
  case VEK_IntLiteral:
    return "IntLiteral";
  case VEK_FloatLiteral:
    return "FloatLiteral";
  case VEK_CharLiteral:
    return "CharLiteral";
  case VEK_Fn:
    return "Fn";
  case VEK_Loop:
    return "Loop";
  case VEK_As:
    return "As";
  case VEK_StringLiteral:
    return "StringLiteral";
  case VEK_StructLiteral:
    return "StructLiteral";
  case VEK_BinaryOp:
    return "BinaryOp";
  case VEK_UnaryOp:
    return "UnaryOp";
  case VEK_Call:
    return "Call";
  case VEK_Return:
    return "Return";
  case VEK_Match:
    return "Match";
  case VEK_Block:
    return "Block";
  case VEK_FieldAccess:
    return "FieldAccess";
  case VEK_Reference:
    return "Reference";
  }
  abort();
}

const char *strLabelExprKind(LabelExprKind val) {
  switch (val) {
  case LEK_Omitted:
    return "Omitted";
  case LEK_Label:
    return "Label";
  }
  abort();
}

const char *strMatchCaseExprKind(MatchCaseExprKind val) {
  switch (val) {
  case MCEK_None:
    return "None";
  case MCEK_Case:
    return "Case";
  case MCEK_Macro:
    return "Macro";
  }
  abort();
}

const char *strValStructMemberExprKind(ValStructMemberExprKind val) {
  switch (val) {
  case VSMEK_None:
    return "None";
  case VSMEK_Macro:
    return "Macro";
  case VSMEK_Member:
    return "Member";
  }
  abort();
}

const char *strValExprUnaryOpKind(ValExprUnaryOpKind val) {
  switch (val) {
  case VEUOK_Negate:
    return "Negate";
  case VEUOK_Posit:
    return "Posit";
  case VEUOK_Not:
    return "Not";
  case VEUOK_Ref:
    return "Ref";
  case VEUOK_Deref:
    return "Deref";
  }
  abort();
}

const char *strValExprBinaryOpKind(ValExprBinaryOpKind val) {
  switch (val) {
  case VEBOK_Add:
    return "Add";
  case VEBOK_Sub:
    return "Sub";
  case VEBOK_Mul:
    return "Mul";
  case VEBOK_Div:
    return "Div";
  case VEBOK_Mod:
    return "Mod";
  case VEBOK_And:
    return "And";
  case VEBOK_Or:
    return "Or";
  case VEBOK_CompEqual:
    return "CompEqual";
  case VEBOK_CompNotEqual:
    return "CompNotEqual";
  case VEBOK_CompLess:
    return "CompLess";
  case VEBOK_CompLessEqual:
    return "CompLessEqual";
  case VEBOK_CompGreater:
    return "CompGreater";
  case VEBOK_CompGreaterEqual:
    return "CompGreaterEqual";
  case VEBOK_Pipeline:
    return "Pipeline";
  case VEBOK_Assign:
    return "Assign";
  case VEBOK_AssignAdd:
    return "AssignAdd";
  case VEBOK_AssignSub:
    return "AssignSub";
  case VEBOK_AssignMul:
    return "AssignMul";
  case VEBOK_AssignDiv:
    return "AssignDiv";
  case VEBOK_AssignMod:
    return "AssignMod";
  case VEBOK_Tuple:
    return "Tuple";
  }
  abort();
}

const char *strStmntKind(StmntKind val) {
  switch (val) {
  case SK_None:
    return "None";
  case SK_Use:
    return "Use";
  case SK_Macro:
    return "Macro";
  case SK_Namespace:
    return "Namespace";
  case SK_ValDecl:
    return "ValDecl";
  case SK_ValDeclDefine:
    return "ValDeclDefine";
  case SK_TypeDecl:
    return "TypeDecl";
  case SK_ValExpr:
    return "ValExpr";
  case SK_DeferStmnt:
    return "DeferStmnt";
  }
  abort();
}

const char *strPatExprUnaryOpKind(PatExprUnaryOpKind val) {
  switch (val) {
  case PEUOK_Not:
    return "Not";
  }
  abort();
}
