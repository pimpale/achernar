#ifndef HIR_H
#define HIR_H

#include "com_define.h"
#include "ast.h"

typedef struct {
  bool valid;
  usize id;
} IdentifierId;

typedef enum {
  hir_BK_None,
  hir_BK_Bind,
  hir_BK_Ignore,
} hir_BindingKind;

typedef struct {
  hir_BindingKind kind;
  ast_Binding *source;
  union {
    struct {
      IdentifierId id;
    } binding;
  };
} hir_Binding;

typedef enum {
  hir_RK_None,
  hir_RK_Path,
} hir_ReferenceKind;

typedef struct {
  hir_ReferenceKind kind;
  ast_Reference *source;
  union {
    struct {
      ast_Binding *first_decl;
      IdentifierId id;
    } path;
  };
} hir_Reference;

typedef enum {
  hir_FK_None,
  hir_FK_Field,
} hir_FieldKind;

typedef struct {
  ast_Field *source;

} hir_Field;

typedef enum {
  hir_LBK_Omitted,
  hir_LBK_Label,
} hir_LabelBindingKind;

typedef struct {
  ast_LabelBinding *source;

  ast_LabelBindingKind kind;
  union {
    struct {
        IdentifierId id;
        com_str full;
    } label;
  };
} hir_LabelBinding;

typedef enum {
  hir_LRK_None,
  hir_LRK_Label,
} hir_LabelReferenceKind;

typedef struct {
  ast_LabelReference *source;

  hir_LabelReferenceKind kind;
  union {
      struct {
          IdentifierId id;
          ast_LabelBinding *first_decl;
      } label;
  };
} hir_LabelReference;

typedef struct hir_Type_s hir_Type;
typedef struct hir_Val_s hir_Val;
typedef struct hir_Pat_s hir_Pat;
typedef struct hir_Stmnt_s hir_Stmnt;

typedef enum {
  hir_PEVRK_CompEqual,        // ==
  hir_PEVRK_CompNotEqual,     // !=
  hir_PEVRK_CompLess,         // <
  hir_PEVRK_CompLessEqual,    // <=
  hir_PEVRK_CompGreater,      // >
  hir_PEVRK_CompGreaterEqual, // >=
} hir_PatValRestrictionKind;

typedef enum {
  hir_PK_None,            // Error type
  hir_PK_ValRestriction,  // matches a constant val
  hir_PK_TypeRestriction, // matches a type, with binding
  hir_PK_Struct,          // permits struct destructuring
  hir_PK_UnaryOp,         // !
  hir_PK_BinaryOp,        // , ||
} hir_PatKind;

typedef enum {
  hir_PEBOK_Tuple,
  hir_PEBOK_Union,
  hir_PEBOK_And,
  hir_PEBOK_Or,
} hir_PatBinaryOpKind;

typedef enum { hir_PEUOK_Not } hir_PatUnaryOpKind;

typedef enum {
  hir_PSMK_None,
  hir_PSMK_Field,
  hir_PSMK_Rest,
} hir_PatStructMemberKind;

typedef struct {
  // where it came from
  ast_PatStructMember *source;

  hir_PatStructMemberKind kind;
  union {
    struct {
      hir_Pat *pattern;
      hir_Field *field;
    } field;
    struct {
      hir_Pat *pattern;
    } rest;
  };
} hir_PatStructMember;

typedef struct hir_Pat_s {
  ast_Pat *source;

  hir_PatKind kind;
  union {
    struct {
      hir_PatValRestrictionKind restriction;
      hir_Val *val;
    } valRestriction;
    struct {
      hir_Type *type;
    } typeRestriction;
    struct {
      hir_Type *type;
      hir_Binding name;
    } typeRestrictionBinding;
    struct {
      hir_PatStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      hir_PatUnaryOpKind op;
      hir_Pat *operand;
    } unaryOp;
    struct {
      hir_PatBinaryOpKind op;
      hir_Pat *left_operand;
      hir_Pat *right_operand;
    } binaryOp;
  };
} hir_Pat;

typedef enum {
  hir_TK_None,        // Error type
  hir_TK_Omitted,     // Omitted
  hir_TK_Nil,         // Nil type
  hir_TK_Never,       // Never type
  hir_TK_Reference,   // Reference (primitive or aliased or path)
  hir_TK_Struct,      // struct
  hir_TK_Fn,          // function pointer
  hir_TK_UnaryOp,     // & or @
  hir_TK_BinaryOp,    // , or |
  hir_TK_FieldAccess, // .
} hir_TypeKind;

typedef enum {
  hir_TSK_Struct,
  hir_TSK_Enum,
} hir_TypeStructKind;

typedef enum {
  hir_TSMK_None,
  hir_TSMK_StructMember,
} hir_TypeStructMemberKind;

typedef struct hir_TypeStructMember_s {
  ast_TypeStructMember *source;

  hir_TypeStructMemberKind kind;
  union {
    struct {
      hir_Field *field;
      hir_Type *type;
    } structMember;
  };
} hir_TypeStructMember;

typedef enum {
  hir_TEUOK_Ref,   // $
  hir_TEUOK_Deref, // @
} hir_TypeUnaryOpKind;

typedef enum {
  hir_TEBOK_Tuple, // ,
  hir_TEBOK_Union, // |
} hir_TypeBinaryOpKind;

// essions and operations yielding a type
typedef struct hir_Type_s {
  ast_Type *source;
  hir_TypeKind kind;

  union {
    struct {
      hir_Reference *path;
    } reference;
    struct {
      hir_TypeStructKind kind;
      hir_TypeStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      hir_Type *parameters;
      usize parameters_len;
      hir_Type *type;
    } fn;
    struct {
      hir_TypeUnaryOpKind op;
      hir_Type *operand;
    } unaryOp;
    struct {
      hir_TypeBinaryOpKind op;
      hir_Type *left_operand;
      hir_Type *right_operand;
    } binaryOp;
    struct {
      hir_Type *root;
      hir_Field *field;
    } fieldAccess;
  };
} hir_Type;

typedef enum {
  hir_VK_None,
  hir_VK_NilLiteral,
  hir_VK_BoolLiteral,
  hir_VK_IntLiteral,
  hir_VK_FloatLiteral,
  hir_VK_CharLiteral,
  hir_VK_Fn,
  hir_VK_Loop,
  hir_VK_As,
  hir_VK_StringLiteral,
  hir_VK_StructLiteral,
  hir_VK_BinaryOp,
  hir_VK_UnaryOp,
  hir_VK_Call,
  hir_VK_Return,
  hir_VK_Match,
  hir_VK_Block,
  hir_VK_FieldAccess,
  hir_VK_Reference,
} hir_ValKind;

typedef enum {
  hir_MCK_None,
  hir_MCK_Case,
} hir_MatchCaseKind;

typedef struct {
  ast_MatchCase *source;
  hir_MatchCaseKind kind;

  union {
    struct {
      hir_Pat *pattern;
      hir_Val *val;
    } matchCase;
  };

} hir_MatchCase;

typedef enum {
  hir_VSMK_None,
  hir_VSMK_Member,
} hir_ValStructMemberKind;

typedef struct {
  ast_ValStructMember *source;

  hir_ValStructMemberKind kind;

  union {
    struct {
      hir_Field *field;
      hir_Val *val;
    } member;
  };
} hir_ValStructMember;

typedef enum {
  hir_VEUOK_Negate,
  hir_VEUOK_Posit,
  hir_VEUOK_Not,
  hir_VEUOK_Ref,
  hir_VEUOK_Deref,
} hir_ValUnaryOpKind;

typedef enum {
  hir_VEBOK_Add,
  hir_VEBOK_Sub,
  hir_VEBOK_Mul,
  hir_VEBOK_Div,
  hir_VEBOK_Mod,
  hir_VEBOK_And,
  hir_VEBOK_Or,
  hir_VEBOK_CompEqual,
  hir_VEBOK_CompNotEqual,
  hir_VEBOK_CompLess,
  hir_VEBOK_CompLessEqual,
  hir_VEBOK_CompGreater,
  hir_VEBOK_CompGreaterEqual,
  hir_VEBOK_Pipeline,
  hir_VEBOK_Assign,
  hir_VEBOK_AssignAdd,
  hir_VEBOK_AssignSub,
  hir_VEBOK_AssignMul,
  hir_VEBOK_AssignDiv,
  hir_VEBOK_AssignMod,
  hir_VEBOK_Tuple,
} hir_ValBinaryOpKind;

typedef struct hir_Val_s {
  ast_Val *source;

  hir_ValKind kind;

  union {
    struct {
      bool value;
    } boolLiteral;
    struct {
      u64 value;
    } intLiteral;
    struct {
      f64 value;
    } floatLiteral;
    struct {
      u8 value;
    } charLiteral;
    struct {
      com_str value;
      usize value_len;
    } stringLiteral;
    struct {
      hir_ValStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      hir_Val *root;
      hir_Type *type;
    } as;
    struct {
      hir_Val *body;
      hir_LabelBinding *label;
    } loop;
    struct {
      hir_Val *root;
      hir_Field *field;
    } fieldAccess;
    struct {
      hir_Reference *path;
    } reference;
    struct {
      hir_ValUnaryOpKind op;
      hir_Val *operand;
    } unaryOp;
    struct {
      hir_ValBinaryOpKind op;
      hir_Val *left_operand;
      hir_Val *right_operand;
    } binaryOp;
    struct {
      hir_Val *function;
      hir_Val *parameters;
      usize parameters_len;
    } call;
    struct {
      hir_Pat *parameters;
      usize parameters_len;
      hir_Type *type;
      hir_Val *body;
    } fn;
    struct {
      hir_Val *value;
      hir_LabelReference *label;
    } returnExpr;
    struct {
      hir_Val *root;
      hir_MatchCase *cases;
      usize cases_len;
    } match;
    struct {
      hir_LabelBinding *label;
      hir_Stmnt *stmnts;
      usize stmnts_len;
    } block;
  };
} hir_Val;

typedef enum {
  hir_SK_None,
  hir_SK_ValDecl,
  hir_SK_ValDeclDefine,
  hir_SK_TypeDecl,
  hir_SK_Val,
  hir_SK_DeferStmnt,
} hir_StmntKind;

typedef struct hir_Stmnt_s {
  ast_Stmnt *source;

  hir_StmntKind kind;

  union {
    // Declarations
    struct {
      hir_Pat *pat;
    } valDecl;
    struct {
      hir_Binding *name;
      hir_Type *type;
    } typeDecl;
    struct {
      hir_Val *val;
    } val;
  };
} hir_Stmnt;

com_str hir_strPatValRestrictionKind(hir_PatValRestrictionKind val);
com_str hir_strPatKind(hir_PatKind val);
com_str hir_strPatBinaryOpKind(hir_PatBinaryOpKind val);
com_str hir_strPatStructMemberKind(hir_PatStructMemberKind val);
com_str hir_strTypeKind(hir_TypeKind val);
com_str hir_strTypeStructKind(hir_TypeStructKind val);
com_str hir_strTypeStructMemberKind(hir_TypeStructMemberKind val);
com_str hir_strTypeUnaryOpKind(hir_TypeUnaryOpKind val);
com_str hir_strTypeBinaryOpKind(hir_TypeBinaryOpKind val);
com_str hir_strValKind(hir_ValKind val);
com_str hir_strLabelReferenceKind(hir_LabelReferenceKind val);
com_str hir_strLabelBindingKind(hir_LabelBindingKind val);
com_str hir_strMatchCaseKind(hir_MatchCaseKind val);
com_str hir_strValStructMemberKind(hir_ValStructMemberKind val);
com_str hir_strValUnaryOpKind(hir_ValUnaryOpKind val);
com_str hir_strValBinaryOpKind(hir_ValBinaryOpKind val);
com_str hir_strStmntKind(hir_StmntKind val);
com_str hir_strPatUnaryOpKind(hir_PatUnaryOpKind val);
com_str hir_strBindingKind(hir_BindingKind val);
com_str hir_strFieldKind(hir_FieldKind val);
com_str hir_strReferenceKind(hir_ReferenceKind val);

#endif
