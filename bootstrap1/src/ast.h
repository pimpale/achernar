#ifndef AST_H
#define AST_H

#include "com_define.h"
#include "com_str.h"
#include "com_loc.h"
#include "token.h"

typedef struct {
  com_loc_Span span;
  bool significant;
  com_str data;
} ast_Metadata;

typedef struct {
  com_loc_Span span;
  com_str name;
  Token *tokens;
  usize tokens_len;
} ast_Macro;

typedef enum {
  ast_MBK_None,
  ast_MBK_Binding,
} ast_ModBindingKind;

typedef struct {
  com_loc_Span span;
  ast_ModBindingKind kind;
  union {
    struct {
      com_str value;
    } binding;
  };
} ast_ModBinding;

typedef enum {
  ast_MRK_None,       // some kind of error
  ast_MRK_Omitted,    // the current namespace
  ast_MRK_Reference,  // a named namespace
} ast_ModReferenceKind;

typedef struct ast_ModReference_s ast_ModReference;

typedef struct ast_ModReference_s {
  com_loc_Span span;
  ast_ModReferenceKind kind;
  union {
    struct {
      com_str name;
      ast_ModReference *mod;
    } reference;
  };
} ast_ModReference;

typedef enum {
  ast_RK_None,      // some kind of error
  ast_RK_Reference, // an actual reference
} ast_ReferenceKind;

typedef struct ast_Reference_s {
  com_loc_Span span;
  ast_ReferenceKind kind;
  union {
    struct {
      com_str name;
      ast_ModReference *mod;
    } reference;
  };
} ast_Reference;

typedef enum {
  ast_BK_None,
  ast_BK_Bind,
  ast_BK_Ignore,
} ast_BindingKind;

typedef struct {
  com_loc_Span span;
  ast_BindingKind kind;
  union {
    struct {
      com_str val;
    } bind;
  };
} ast_Binding;

typedef enum {
  ast_FK_None,
  ast_FK_FieldStr,
  ast_FK_FieldInt,
} ast_FieldKind;

typedef struct {
  com_loc_Span span;
  ast_FieldKind kind;
  union {
    struct {
      com_str val;
    } strField;
    struct {
      u64 val;
    } intField;
  };
} ast_Field;

typedef struct {
  com_loc_Span span;
  ast_Metadata *metadata;
  usize metadata_len;
} ast_Common;

typedef struct ast_Type_s ast_Type;
typedef struct ast_Val_s ast_Val;
typedef struct ast_Pat_s ast_Pat;
typedef struct ast_Stmnt_s ast_Stmnt;

typedef enum {
  ast_PEVRK_CompEqual,        // ==
  ast_PEVRK_CompNotEqual,     // !=
  ast_PEVRK_CompLess,         // <
  ast_PEVRK_CompLessEqual,    // <=
  ast_PEVRK_CompGreater,      // >
  ast_PEVRK_CompGreaterEqual, // >=
} ast_PatValRestrictionKind;

typedef enum {
  ast_PK_None,            // Error type
  ast_PK_Macro,           // a macro representing a pattern
  ast_PK_ValRestriction,  // matches a constant val
  ast_PK_TypeRestriction, // matches a type, and binds it
  ast_PK_Struct,          // a container for struct based patterns
  ast_PK_Group,           // ()
  ast_PK_UnaryOp,         // !
  ast_PK_BinaryOp,        // , |
} ast_PatKind;

typedef enum {
  ast_PEBOK_Tuple,
  ast_PEBOK_Union,
  ast_PEBOK_And,
  ast_PEBOK_Or,
} ast_PatBinaryOpKind;

typedef enum { ast_PEUOK_Not } ast_PatUnaryOpKind;

typedef enum {
  ast_PSMK_None,
  ast_PSMK_Macro,
  ast_PSMK_Field,
} ast_PatStructMemberKind;

typedef struct {
  ast_Common common;
  ast_PatStructMemberKind kind;
  union {
    struct {
      ast_Macro *macro;
    } macro;
    struct {
      ast_Pat *pat;
      ast_Field *field;
    } field;
  };
} ast_PatStructMember;

typedef struct ast_Pat_s {
  ast_Common common;

  ast_PatKind kind;
  union {
    struct {
      ast_PatValRestrictionKind restriction;
      ast_Val *val;
    } valRestriction;
    struct {
      ast_Macro *macro;
    } macro;
    struct {
      ast_Type *type;
      ast_Binding *name;
    } typeRestriction;
    struct {
      ast_PatStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      ast_Pat *inner;
    } group;
    struct {
      ast_PatUnaryOpKind op;
      ast_Pat *operand;
    } unaryOp;
    struct {
      ast_PatBinaryOpKind op;
      ast_Pat *left_operand;
      ast_Pat *right_operand;
    } binaryOp;
  };
} ast_Pat;

typedef enum {
  ast_TK_None,        // Error type
  ast_TK_Omitted,     // Omitted
  ast_TK_Macro,       // Macro type
  ast_TK_Nil,         // Nil type
  ast_TK_Never,       // Never type
  ast_TK_Group,       // { something }
  ast_TK_Reference,   // Reference (primitive or aliased or path)
  ast_TK_Struct,      // struct
  ast_TK_Fn,          // function pointer
  ast_TK_UnaryOp,     // & or @
  ast_TK_BinaryOp,    // , or |
  ast_TK_FieldAccess, // .
} ast_TypeKind;

typedef enum {
  ast_TSK_Struct,
  ast_TSK_Enum,
} ast_TypeStructKind;

typedef enum {
  ast_TSMK_None,
  ast_TSMK_Macro,
  ast_TSMK_StructMember,
} ast_TypeStructMemberKind;

typedef struct ast_TypeStructMember_s {
  ast_Common common;
  ast_TypeStructMemberKind kind;

  union {
    struct {
      ast_Field *field;
      ast_Type *type;
    } structMember;
    struct {
      ast_Macro *macro;
    } macro;
  };
} ast_TypeStructMember;

typedef enum {
  ast_TEUOK_Ref,   // $
  ast_TEUOK_Deref, // @
} ast_TypeUnaryOpKind;

typedef enum {
  ast_TEBOK_Tuple, // ,
  ast_TEBOK_Union, // |
} ast_TypeBinaryOpKind;

// essions and operations yielding a type
typedef struct ast_Type_s {
  ast_Common common;
  ast_TypeKind kind;

  union {
    struct {
      ast_Macro *macro;
    } macro;
    struct {
      ast_Reference *path;
    } reference;
    struct {
      ast_TypeStructKind kind;
      ast_TypeStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      ast_Type *parameters;
      usize parameters_len;
      ast_Type *type;
    } fn;
    struct {
      ast_Type *inner;
    } group;
    struct {
      ast_TypeUnaryOpKind op;
      ast_Type *operand;
    } unaryOp;
    struct {
      ast_TypeBinaryOpKind op;
      ast_Type *left_operand;
      ast_Type *right_operand;
    } binaryOp;
    struct {
      ast_Type *root;
      ast_Field *field;
    } fieldAccess;
  };
} ast_Type;

typedef enum {
  ast_VK_None,
  ast_VK_Macro,
  ast_VK_NilLiteral,
  ast_VK_BoolLiteral,
  ast_VK_IntLiteral,
  ast_VK_FloatLiteral,
  ast_VK_CharLiteral,
  ast_VK_Fn,
  ast_VK_Loop,
  ast_VK_As,
  ast_VK_StringLiteral,
  ast_VK_StructLiteral,
  ast_VK_BinaryOp,
  ast_VK_UnaryOp,
  ast_VK_Call,
  ast_VK_Return,
  ast_VK_Match,
  ast_VK_Block,
  ast_VK_FieldAccess,
  ast_VK_Reference,
} ast_ValKind;

typedef enum {
  ast_LBK_Omitted,
  ast_LBK_Label,
} ast_LabelBindingKind;

typedef struct {
  com_loc_Span span;
  ast_LabelBindingKind kind;
  union {
    struct {
      com_str label;
    } label;
  };
} ast_LabelBinding;

typedef enum {
  ast_LRK_None,
  ast_LRK_Label,
} ast_LabelReferenceKind;

typedef struct {
  com_loc_Span span;
  ast_LabelReferenceKind kind;
  union {
    struct {
      com_str label;
    } label;
  };
} ast_LabelReference;

typedef enum {
  ast_MCK_None,
  ast_MCK_Case,
  ast_MCK_Macro,
} ast_MatchCaseKind;

typedef struct {
  ast_Common common;
  ast_MatchCaseKind kind;

  union {
    struct {
      ast_Pat *pattern;
      ast_Val *val;
    } matchCase;
    struct {
      ast_Macro *macro;
    } macro;
  };

} ast_MatchCase;

typedef enum {
  ast_VSMK_None,
  ast_VSMK_Macro,
  ast_VSMK_Member,
} ast_ValStructMemberKind;

typedef struct {
  ast_Common common;

  ast_ValStructMemberKind kind;

  union {
    struct {
      ast_Field *field;
      ast_Val *val;
    } member;
    struct {
      ast_Macro *macro;
    } macro;
  };
} ast_ValStructMember;

typedef enum {
  ast_VEUOK_Negate,
  ast_VEUOK_Posit,
  ast_VEUOK_Not,
  ast_VEUOK_Ref,
  ast_VEUOK_Deref,
} ast_ValUnaryOpKind;

typedef enum {
  ast_VEBOK_Add,
  ast_VEBOK_Sub,
  ast_VEBOK_Mul,
  ast_VEBOK_Div,
  ast_VEBOK_Rem,
  ast_VEBOK_And,
  ast_VEBOK_Or,
  ast_VEBOK_CompEqual,
  ast_VEBOK_CompNotEqual,
  ast_VEBOK_CompLess,
  ast_VEBOK_CompLessEqual,
  ast_VEBOK_CompGreater,
  ast_VEBOK_CompGreaterEqual,
  ast_VEBOK_Pipeline,
  ast_VEBOK_Assign,
  ast_VEBOK_AssignAdd,
  ast_VEBOK_AssignSub,
  ast_VEBOK_AssignMul,
  ast_VEBOK_AssignDiv,
  ast_VEBOK_AssignRem,
  ast_VEBOK_Tuple,
} ast_ValBinaryOpKind;

typedef struct ast_Val_s {
  ast_Common common;
  ast_ValKind kind;

  union {
    struct {
      ast_Macro *macro;
    } macro;
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
      ast_ValStructMember *members;
      usize members_len;
    } structExpr;
    struct {
      ast_Val *root;
      ast_Type *type;
    } as;
    struct {
      ast_Val *body;
      ast_LabelBinding *label;
    } loop;
    struct {
      ast_Val *root;
      ast_Field *field;
    } fieldAccess;
    struct {
      ast_Reference *path;
    } reference;
    struct {
      ast_ValUnaryOpKind op;
      ast_Val *operand;
    } unaryOp;
    struct {
      ast_ValBinaryOpKind op;
      ast_Val *left_operand;
      ast_Val *right_operand;
    } binaryOp;
    struct {
      ast_Val *function;
      ast_Val *parameters;
      usize parameters_len;
    } call;
    struct {
      ast_Pat *parameters;
      usize parameters_len;
      ast_Type *type;
      ast_Val *body;
    } fn;
    struct {
      ast_Val *value;
      ast_LabelReference *label;
    } returnExpr;
    struct {
      ast_Val *root;
      ast_MatchCase *cases;
      usize cases_len;
    } match;
    struct {
      ast_LabelBinding *label;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } block;
  };
} ast_Val;

typedef enum {
  ast_SK_None,
  ast_SK_Use,
  ast_SK_Macro,
  ast_SK_Mod,
  ast_SK_ValDecl,
  ast_SK_ValDeclDefine,
  ast_SK_TypeDecl,
  ast_SK_Val,
  ast_SK_DeferStmnt,
} ast_StmntKind;

typedef struct ast_Stmnt_s {
  ast_Common common;
  ast_StmntKind kind;

  union {
    // Declarations
    struct {
      ast_Pat *pat;
    } valDecl;
    struct {
      ast_Pat *pat;
      ast_Val *val;
    } valDeclDefine;
    struct {
      ast_Binding *name;
      ast_Type *type;
    } typeDecl;
    struct {
      ast_Reference *path;
    } useStmnt;
    struct {
      ast_ModBinding *binding;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } namespaceStmnt;
    struct {
      ast_Macro *macro;
    } macro;
    struct {
      ast_Val *val;
    } val;
    struct {
      ast_LabelReference *label;
      ast_Val *val;
    } deferStmnt;
  };
} ast_Stmnt;

com_str ast_strPatValRestrictionKind(ast_PatValRestrictionKind val);
com_str ast_strPatKind(ast_PatKind val);
com_str ast_strPatBinaryOpKind(ast_PatBinaryOpKind val);
com_str ast_strPatStructMemberKind(ast_PatStructMemberKind val);
com_str ast_strTypeKind(ast_TypeKind val);
com_str ast_strTypeStructKind(ast_TypeStructKind val);
com_str ast_strTypeStructMemberKind(ast_TypeStructMemberKind val);
com_str ast_strTypeUnaryOpKind(ast_TypeUnaryOpKind val);
com_str ast_strTypeBinaryOpKind(ast_TypeBinaryOpKind val);
com_str ast_strValKind(ast_ValKind val);
com_str ast_strLabelReferenceKind(ast_LabelReferenceKind val);
com_str ast_strLabelBindingKind(ast_LabelBindingKind val);
com_str ast_strMatchCaseKind(ast_MatchCaseKind val);
com_str ast_strValStructMemberKind(ast_ValStructMemberKind val);
com_str ast_strValUnaryOpKind(ast_ValUnaryOpKind val);
com_str ast_strValBinaryOpKind(ast_ValBinaryOpKind val);
com_str ast_strStmntKind(ast_StmntKind val);
com_str ast_strPatUnaryOpKind(ast_PatUnaryOpKind val);
com_str ast_strBindingKind(ast_BindingKind val);
com_str ast_strFieldKind(ast_FieldKind val);
com_str ast_strReferenceKind(ast_ReferenceKind val);

#endif
