#ifndef HIR_H
#define HIR_H

#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"
#include "token.h"
#include "ast.h"

typedef struct {
  ast_Binding* declaration;
  com_str name;
} hir_Identifier;

typedef enum {
  hir_RK_None,      // some kind of error
  hir_RK_Reference, // an actual reference
} hir_ReferenceKind;

typedef struct hir_Reference_s {
  hir_ReferenceKind kind;
  union {
    struct {
      hir_Identifier *val;
    } reference;
  };
  ast_Reference source;
} hir_Reference;

typedef enum {
  hir_BK_None,
  hir_BK_Bind,
  hir_BK_Ignore,
} hir_BindingKind;

typedef struct {
  hir_BindingKind kind;
  union {
    struct {
      hir_Identifier *val;
    } bind;
  };
} hir_Binding;

typedef enum {
  hir_FK_None,
  hir_FK_FieldStr,
  hir_FK_FieldInt,
} hir_FieldKind;

typedef struct {
  hir_FieldKind kind;
  union {
    struct {
      com_str val;
    } strField;
    struct {
      com_bigint val;
    } intField;
  };
} hir_Field;

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
  hir_PK_TypeRestriction, // matches a type, and binds it
  hir_PK_Struct,          // a container for struct based patterns
  hir_PK_Group,           // ()
  hir_PK_UnaryOp,         // !
  hir_PK_BinaryOp,        // , |
} hir_PatKind;

typedef enum {
  hir_PEBOK_Product,
  hir_PEBOK_Sum,
  hir_PEBOK_Union,
  hir_PEBOK_Intersection,
  hir_PEBOK_And,
  hir_PEBOK_Or,
} hir_PatBinaryOpKind;

typedef enum {
  hir_PEUOK_Not,
  hir_PEUOK_Ref,
  hir_PEUOK_Deref,
} hir_PatUnaryOpKind;

typedef enum {
  hir_PSMK_None,
  hir_PSMK_Field,
} hir_PatStructMemberKind;

typedef struct {
  ast_Stmnt source;
  hir_PatStructMemberKind kind;
  union {
    struct {
      hir_Pat *pat;
      hir_Field *field;
    } field;
  };
} hir_PatStructMember;

typedef struct hir_Pat_s {
  ast_Stmnt source;

  hir_PatKind kind;
  union {
    struct {
      hir_PatValRestrictionKind restriction;
      hir_Val *val;
    } valRestriction;
    struct {
      hir_Type *type;
      hir_Binding *name;
    } typeRestriction;
    struct {
      hir_PatStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      hir_Pat *inner;
    } group;
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
  hir_TK_Group,       // { something }
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
  ast_Stmnt source;
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
  hir_TEBOK_Product,      // ,
  hir_TEBOK_Sum,          // |
  hir_TEBOK_Union,        // ,,
  hir_TEBOK_Intersection, // ||
} hir_TypeBinaryOpKind;

// essions and operations yielding a type
typedef struct hir_Type_s {
  ast_Stmnt source;
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
      hir_Type *inner;
    } group;
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
  hir_VK_Pipe,
  hir_VK_Ret,
  hir_VK_Match,
  hir_VK_Block,
  hir_VK_FieldAccess,
  hir_VK_Reference,
} hir_ValKind;

typedef struct {
  ast_LabelBinding* declaration;
  com_str name;
} hir_Label ;


typedef enum {
  hir_LBK_Omitted,
  hir_LBK_Label,
} hir_LabelBindingKind;

typedef struct {
  hir_LabelBindingKind kind;
  union {
    struct {
      hir_Label* label;
    } label;
  };
} hir_LabelBinding;

typedef enum {
  hir_LRK_None,
  hir_LRK_Label,
} hir_LabelReferenceKind;

typedef struct {
  hir_LabelReferenceKind kind;
  union {
    struct {
      hir_Label* label;
    } label;
  };
} hir_LabelReference;

typedef enum {
  hir_MCK_None,
  hir_MCK_Case,
} hir_MatchCaseKind;

typedef struct {
  ast_Stmnt source;
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
  ast_Stmnt source;

  hir_ValStructMemberKind kind;

  union {
    struct {
      hir_Field *field;
      hir_Val *val;
    } member;
  };
} hir_ValStructMember;

typedef enum {
  hir_VEUOK_Not,
  hir_VEUOK_Ref,
  hir_VEUOK_Deref,
} hir_ValUnaryOpKind;

typedef enum {
  hir_VEBOK_Add,
  hir_VEBOK_Sub,
  hir_VEBOK_Mul,
  hir_VEBOK_IDiv,
  hir_VEBOK_FDiv,
  hir_VEBOK_IRem,
  hir_VEBOK_FRem,
  hir_VEBOK_And,
  hir_VEBOK_Or,
  hir_VEBOK_Xor,
  hir_VEBOK_CompEqual,
  hir_VEBOK_CompNotEqual,
  hir_VEBOK_CompLess,
  hir_VEBOK_CompLessEqual,
  hir_VEBOK_CompGreater,
  hir_VEBOK_CompGreaterEqual,
  hir_VEBOK_Assign,
  hir_VEBOK_Product,
  hir_VEBOK_Union,
  hir_VEBOK_Intersection,
} hir_ValBinaryOpKind;

typedef struct hir_Val_s {
  ast_Stmnt source;
  hir_ValKind kind;

  union {
    struct {
      bool value;
    } boolLiteral;
    struct {
      com_bigint value;
    } intLiteral;
    struct {
      com_bigdecimal value;
    } floatLiteral;
    struct {
      u8 value;
    } charLiteral;
    struct {
      com_str value;
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
  hir_SK_Use,
  hir_SK_Mod,
  hir_SK_ValDecl,
  hir_SK_ValDeclDefine,
  hir_SK_TypeDecl,
  hir_SK_Val,
} hir_StmntKind;

typedef struct hir_Stmnt_s {
  ast_Stmnt source;
  hir_StmntKind kind;

  union {
    // Declarations
    struct {
      hir_Pat *pat;
    } valDecl;
    struct {
      hir_Pat *pat;
      hir_Val *val;
    } valDeclDefine;
    struct {
      hir_Binding *name;
      hir_Type *type;
    } typeDecl;
    struct {
      hir_Val *val;
    } val;
  };
} hir_Stmnt;

#endif
