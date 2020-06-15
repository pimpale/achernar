#ifndef AST_H
#define AST_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "lncol.h"
#include "token.h"

typedef struct {
  Span span;
  char *scope;
  char *data;
} Comment;

typedef struct {
  Span span;
  Comment *comments;
  size_t comments_len;
} AstNode;

typedef struct {
  AstNode node;

  char *name;
  Token *tokens;
  size_t tokens_len;
} MacroExpr;

typedef struct {
  AstNode node;

  char **pathSegments;
  size_t pathSegments_len;
} Path;

typedef struct TypeExpr_s TypeExpr;
typedef struct ValExpr_s ValExpr;
typedef struct PatExpr_s PatExpr;
typedef struct Stmnt_s Stmnt;

typedef enum {
  PEVRK_CompEqual,        // ==
  PEVRK_CompNotEqual,     // !=
  PEVRK_CompLess,         // <
  PEVRK_CompLessEqual,    // <=
  PEVRK_CompGreater,      // >
  PEVRK_CompGreaterEqual, // >=
} PatExprValRestrictionKind;

static const char *strPatExprValRestrictionKind(PatExprValRestrictionKind val) {
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
}

typedef enum {
  PEK_None,            // Error type
  PEK_Macro,           // a macro representing a pattern
  PEK_ValRestriction,  // matches a constant val
  PEK_TypeRestriction, // matches a type, without binding
  PEK_TypeRestrictionBinding, // matches a type, and binds it
  PEK_Struct,          // a container for struct based patterns
  PEK_Group,           // ()
  PEK_UnaryOp,         // !
  PEK_BinaryOp,        // , |
} PatExprKind;

static const char *strPatExprKind(PatExprKind val) {
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
}

typedef enum {
  PEBOK_Tuple,
  PEBOK_Union,
  PEBOK_And,
  PEBOK_Or,
} PatExprBinaryOpKind;

static const char *strPatExprBinaryOpKind(PatExprBinaryOpKind val) {
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
}

typedef enum { PEUOK_Not } PatExprUnaryOpKind;

static const char *strPatExprUnaryOpKind(PatExprUnaryOpKind val) {
  switch (val) {
  case PEUOK_Not:
    return "Not";
  }
}

typedef enum {
  PSMEK_None,
  PSMEK_Macro,
  PSMEK_Field,
  PSMEK_Rest,
} PatStructMemberExprKind;

static const char *strPatStructMemberExprKind(PatStructMemberExprKind val) {
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
}

typedef struct {
  AstNode node;
  PatStructMemberExprKind kind;
  union {
    struct {
      MacroExpr *macro;
    } macro;
    struct {
      PatExpr *pattern;
      char *field;
    } field;
    struct {
      PatExpr *pattern;
    } rest;
  };
} PatStructMemberExpr;

typedef struct PatExpr_s {
  AstNode node;

  PatExprKind kind;
  union {
    struct {
      PatExprValRestrictionKind restriction;
      ValExpr *valExpr;
    } valRestriction;
    struct {
      MacroExpr *macro;
    } macro;
    struct {
      TypeExpr *type;
    } typeRestriction;
    struct {
      TypeExpr *type;
      char *name;
    } typeRestrictionBinding;
    struct {
      PatStructMemberExpr *members;
      size_t members_len;
    } structExpr;
    struct {
      PatExpr *inner;
    } groupExpr;
    struct {
      PatExprUnaryOpKind op;
      PatExpr *operand;
    } unaryOp;
    struct {
      PatExprBinaryOpKind op;
      PatExpr *left_operand;
      PatExpr *right_operand;
    } binaryOp;
  };
} PatExpr;

typedef enum {
  TEK_None,        // Error type
  TEK_Omitted,     // Omitted
  TEK_Macro,       // MacroExpr Type
  TEK_Nil,         // Nil type
  TEK_Group,       // { nil }
  TEK_Reference,   // Reference (primitive or aliased or path)
  TEK_Struct,      // struct
  TEK_Fn,          // function pointer
  TEK_UnaryOp,     // & or @
  TEK_BinaryOp,    // , or |
  TEK_FieldAccess, // .
} TypeExprKind;

static const char *strTypeExprKind(TypeExprKind val) {
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
}

typedef enum {
  TSEK_Struct,
  TSEK_Enum,
} TypeStructExprKind;

static const char *strTypeStructExprKind(TypeStructExprKind val) {
  switch (val) {
  case TSEK_Struct:
    return "Struct";
  case TSEK_Enum:
    return "Enum";
  }
}

typedef enum {
  TSMEK_None,
  TSMEK_Macro,
  TSMEK_StructMember,
} TypeStructMemberExprKind;

static const char *strTypeStructMemberExprKind(TypeStructMemberExprKind val) {
  switch (val) {
  case TSMEK_None:
    return "None";
  case TSMEK_Macro:
    return "Macro";
  case TSMEK_StructMember:
    return "StructMember";
  }
}

typedef struct TypeStructMemberExpr_s {
  AstNode node;
  TypeStructMemberExprKind kind;

  union {
    struct {
      char *name;
      TypeExpr *type;
    } structMember;
    struct {
      MacroExpr *macro;
    } macro;
  };
} TypeStructMemberExpr;

typedef enum {
  TEUOK_Ref,   // $
  TEUOK_Deref, // @
} TypeExprUnaryOpKind;

static const char *strTypeExprUnaryOpKind(TypeExprUnaryOpKind val) {
  switch (val) {
  case TEUOK_Ref:
    return "Ref";
  case TEUOK_Deref:
    return "Deref";
  }
}

typedef enum {
  TEBOK_Tuple, // ,
  TEBOK_Union, // |
} TypeExprBinaryOpKind;

static const char *strTypeExprBinaryOpKind(TypeExprBinaryOpKind val) {
  switch (val) {
  case TEBOK_Tuple:
    return "Tuple";
  case TEBOK_Union:
    return "Union";
  }
}

// Expressions and operations yielding a type
typedef struct TypeExpr_s {
  AstNode node;
  TypeExprKind kind;

  union {
    struct {
      MacroExpr *macro;
    } macro;
    struct {
      Path *path;
    } referenceExpr;
    struct {
      TypeStructExprKind kind;
      TypeStructMemberExpr *members;
      size_t members_len;
    } structExpr;
    struct {
      TypeExpr *parameters;
      size_t parameters_len;
      TypeExpr *type;
    } fnExpr;
    struct {
      TypeExpr *inner;
    } groupExpr;
    struct {
      TypeExprUnaryOpKind op;
      TypeExpr *operand;
    } unaryOp;
    struct {
      TypeExprBinaryOpKind op;
      struct TypeExpr_s *left_operand;
      struct TypeExpr_s *right_operand;
    } binaryOp;
    struct {
      struct TypeExpr_s *root;
      char *field;
    } fieldAccess;
  };
} TypeExpr;

typedef enum {
  VEK_None,
  VEK_Macro,
  VEK_NilLiteral,
  VEK_BoolLiteral,
  VEK_IntLiteral,
  VEK_FloatLiteral,
  VEK_CharLiteral,
  VEK_Fn,
  VEK_Loop,
  VEK_As,
  VEK_StringLiteral,
  VEK_StructLiteral,
  VEK_BinaryOp,
  VEK_UnaryOp,
  VEK_Call,
  VEK_Continue,
  VEK_Return,
  VEK_Match,
  VEK_Block,
  VEK_FieldAccess,
  VEK_Reference,
} ValExprKind;

static const char *strValExprKind(ValExprKind val) {
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
  case VEK_Continue:
    return "Continue";
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
}

typedef enum {
  LEK_Omitted,
  LEK_Label,
} LabelExprKind;

static const char *strLabelExprKind(LabelExprKind val) {
  switch (val) {
  case LEK_Omitted:
    return "Omitted";
  case LEK_Label:
    return "Label";
  }
}

typedef struct {
  AstNode node;
  LabelExprKind kind;
  union {
    struct {
      char *label;
    } label;
  };
} LabelExpr;

typedef enum {
  MCEK_None,
  MCEK_Case,
  MCEK_Macro,
} MatchCaseExprKind;

static const char *strMatchCaseExprKind(MatchCaseExprKind val) {
  switch (val) {
  case MCEK_None:
    return "None";
  case MCEK_Case:
    return "Case";
  case MCEK_Macro:
    return "Macro";
  }
}

typedef struct {
  AstNode node;
  MatchCaseExprKind kind;

  union {
    struct {
      PatExpr *pattern;
      ValExpr *val;
    } matchCase;
    struct {
      MacroExpr *macro;
    } macro;
  };

} MatchCaseExpr;

typedef enum {
  VSMEK_None,
  VSMEK_Macro,
  VSMEK_Member,
} ValStructMemberExprKind;

static const char *strValStructMemberExprKind(ValStructMemberExprKind val) {
  switch (val) {
  case VSMEK_None:
    return "None";
  case VSMEK_Macro:
    return "Macro";
  case VSMEK_Member:
    return "Member";
  }
}

typedef struct {
  AstNode node;

  ValStructMemberExprKind kind;

  union {
    struct {
      char *name;
      ValExpr *val;
    } memberExpr;
    struct {
      MacroExpr *macro;
    } macro;
  };
} ValStructMemberExpr;

typedef enum {
  VEUOK_Negate,
  VEUOK_Posit,
  VEUOK_Not,
  VEUOK_Ref,
  VEUOK_Deref,
} ValExprUnaryOpKind;

static const char *strValExprUnaryOpKind(ValExprUnaryOpKind val) {
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
}

typedef enum {
  VEBOK_Add,
  VEBOK_Sub,
  VEBOK_Mul,
  VEBOK_Div,
  VEBOK_Mod,
  VEBOK_And,
  VEBOK_Or,
  VEBOK_CompEqual,
  VEBOK_CompNotEqual,
  VEBOK_CompLess,
  VEBOK_CompLessEqual,
  VEBOK_CompGreater,
  VEBOK_CompGreaterEqual,
  VEBOK_Pipeline,
  VEBOK_Assign,
  VEBOK_AssignAdd,
  VEBOK_AssignSub,
  VEBOK_AssignMul,
  VEBOK_AssignDiv,
  VEBOK_AssignMod,
  VEBOK_Tuple,
} ValExprBinaryOpKind;

static const char *strValExprBinaryOpKind(ValExprBinaryOpKind val) {
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
}

typedef struct ValExpr_s {
  AstNode node;
  ValExprKind kind;

  union {
    struct {
      MacroExpr* macro;
    } macro;
    struct {
      bool value;
    } boolLiteral;
    struct {
      uint64_t value;
    } intLiteral;
    struct {
      double value;
    } floatLiteral;
    struct {
      char value;
    } charLiteral;
    struct {
      char *value;
      size_t value_len;
    } stringLiteral;
    struct {
      ValStructMemberExpr *members;
      size_t members_len;
    } structExpr;
    struct {
      ValExpr *root;
      TypeExpr *type;
    } asExpr;
    struct {
      ValExpr *body;
      LabelExpr *label;
    } loopExpr;
    struct {
      ValExpr *root;
      char *name;
    } fieldAccess;
    struct {
      Path *path;
    } reference;
    struct {
      ValExprUnaryOpKind op;
      ValExpr *operand;
    } unaryOp;
    struct {
      ValExprBinaryOpKind op;
      ValExpr *left_operand;
      ValExpr *right_operand;
    } binaryOp;
    struct {
      ValExpr *function;
      ValExpr *parameters;
      size_t parameters_len;
    } callExpr;
    struct {
      PatExpr *parameters;
      size_t parameters_len;
      TypeExpr *type;
      ValExpr *body;
    } fnExpr;
    struct {
      ValExpr *value;
      LabelExpr *label;
    } returnExpr;
    struct {
      ValExpr *val;
      MatchCaseExpr *cases;
      size_t cases_len;
    } matchExpr;
    struct {
      Stmnt *statements;
      size_t statements_len;
      LabelExpr *label;
    } blockExpr;
  };
} ValExpr;

typedef enum {
  SK_None,
  SK_Use,
  SK_Macro,
  SK_Namespace,
  SK_ValDecl,
  SK_TypeDecl,
  SK_ValExpr,
  SK_DeferStmnt,
} StmntKind;

static const char *strStmntKind(StmntKind val) {
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
  case SK_TypeDecl:
    return "TypeDecl";
  case SK_ValExpr:
    return "ValExpr";
  case SK_DeferStmnt:
    return "DeferStmnt";
  }
}

typedef struct Stmnt_s {
  AstNode node;
  StmntKind kind;

  size_t comments_len;
  union {
    // Declarations
    struct {
      PatExpr *pattern;
      bool has_val;
      ValExpr *val;
    } valDecl;
    struct {
      TypeExpr *type;
      char *name;
    } typeDecl;
    // Things
    struct {
      Path *path;
    } useStmnt;
    struct {
      Path *path;
      Stmnt *stmnt;
    } namespaceStmnt;
    struct {
      MacroExpr *macro;
    } macroStmnt;
    // Expressions
    struct {
      ValExpr *val;
    } valExpr;
    struct {
      char *scope;
      ValExpr *val;
    } deferStmnt;
  };
} Stmnt;

#endif
