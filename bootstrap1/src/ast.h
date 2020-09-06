#ifndef AST_H
#define AST_H

#include "com_define.h"
#include "com_loc.h"
#include "com_str.h"
#include "token.h"

typedef struct {
  com_loc_Span span;
  bool significant;
  com_str data;
} ast_Metadata;

typedef enum {
  ast_MBK_None,
  ast_MBK_Omitted,
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
  ast_MRK_None,      // some kind of error
  ast_MRK_Omitted,   // the current mod
  ast_MRK_Reference, // a named mod
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
  ast_PK_ValRestriction,  // matches a constant val
  ast_PK_TypeRestriction, // matches a type, and binds it
  ast_PK_Record,          // a container for struct based patterns
  ast_PK_Group,           // ()
  ast_PK_UnaryOp,         // !
  ast_PK_BinaryOp,        // , |
} ast_PatKind;

typedef enum {
  ast_PEBOK_Product,
  ast_PEBOK_Sum,
  ast_PEBOK_Union,
  ast_PEBOK_Intersection,
  ast_PEBOK_And,
  ast_PEBOK_Or,
} ast_PatBinaryOpKind;

typedef enum {
  ast_PEUOK_Not,
  ast_PEUOK_Ref,
  ast_PEUOK_Deref,
} ast_PatUnaryOpKind;

typedef struct ast_Pat_s {
  ast_Common common;

  ast_PatKind kind;
  union {
    struct {
      ast_PatValRestrictionKind restriction;
      ast_Val *val;
    } valRestriction;
    struct {
      ast_Type *type;
      ast_Binding *name;
    } typeRestriction;
    struct {
      ast_Pat *pat;
      ast_Field *field;
    } record;
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
  ast_TEUOK_Ref,   // $
  ast_TEUOK_Deref, // @
} ast_TypeUnaryOpKind;

typedef enum {
  ast_TEBOK_Product,      // ,
  ast_TEBOK_Sum,          // |
  ast_TEBOK_Union,        // ,,
  ast_TEBOK_Intersection, // ||
} ast_TypeBinaryOpKind;

typedef enum {
  ast_TK_None,        // Error type
  ast_TK_Omitted,     // Omitted
  ast_TK_Nil,         // Nil type
  ast_TK_Never,       // Never type
  ast_TK_Group,       // { something }
  ast_TK_Reference,   // Reference (primitive or aliased or path)
  ast_TK_Record,      // record
  ast_TK_Fn,          // function pointer
  ast_TK_Typefn,      // type constructor
  ast_TK_TypefnCall,  // constructing a type
  ast_TK_UnaryOp,     // & or @
  ast_TK_BinaryOp,    // , | ,, ||
  ast_TK_FieldAccess, // .
} ast_TypeKind;

// essions and operations yielding a type
typedef struct ast_Type_s {
  ast_Common common;
  ast_TypeKind kind;

  union {
    struct {
      ast_Reference *path;
    } reference;
    struct {
      ast_Field *field;
      ast_Type *type;
    } record;
    struct {
      ast_Type *parameters;
      usize parameters_len;
      ast_Type *type;
    } fn;
    struct {
      ast_Binding *name;
      ast_Binding *parameters;
      usize parameters_len;
      ast_Type *body;
    } typefn;
    struct {
      ast_Type *root;
      ast_Type *parameters;
      usize parameters_len;
    } call;
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
} ast_MatchCaseKind;

typedef struct {
  ast_Common common;
  ast_MatchCaseKind kind;
  union {
    struct {
      ast_Pat *pattern;
      ast_Val *val;
    } matchCase;
  };

} ast_MatchCase;

typedef enum {
  ast_VEUOK_Not,
  ast_VEUOK_Ref,
  ast_VEUOK_Deref,
} ast_ValUnaryOpKind;

typedef enum {
  ast_VEBOK_Add,
  ast_VEBOK_Sub,
  ast_VEBOK_Mul,
  ast_VEBOK_IDiv,
  ast_VEBOK_FDiv,
  ast_VEBOK_IRem,
  ast_VEBOK_FRem,
  ast_VEBOK_And,
  ast_VEBOK_Or,
  ast_VEBOK_Xor,
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
  ast_VEBOK_AssignIDiv,
  ast_VEBOK_AssignFDiv,
  ast_VEBOK_AssignIRem,
  ast_VEBOK_AssignFRem,
  ast_VEBOK_Product,
  ast_VEBOK_Union,
  ast_VEBOK_Intersection,
} ast_ValBinaryOpKind;

typedef enum {
  ast_VK_None,
  ast_VK_Omitted,
  ast_VK_NilLiteral,
  ast_VK_BoolLiteral,
  ast_VK_IntLiteral,
  ast_VK_FloatLiteral,
  ast_VK_CharLiteral,
  ast_VK_Fn,
  ast_VK_Loop,
  ast_VK_As,
  ast_VK_Record,
  ast_VK_StringLiteral,
  ast_VK_BinaryOp,
  ast_VK_UnaryOp,
  ast_VK_Call,
  ast_VK_Pipe,
  ast_VK_Ret,
  ast_VK_Match,
  ast_VK_Block,
  ast_VK_FieldAccess,
  ast_VK_Reference,
} ast_ValKind;

typedef struct ast_Val_s {
  ast_Common common;
  ast_ValKind kind;

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
      ast_Field *field;
      ast_Val *val;
    } record;
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
      ast_Val *root;
      ast_Val *fn;
    } pipe;
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
      ast_Binding* name;
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
  ast_SK_Mod,
  ast_SK_ValDecl,
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
      ast_Val *val;
    } valDeclDefine;
    struct {
      ast_Binding *name;
    } typeDecl;
    struct {
      ast_Binding *name;
      ast_Type *type;
    } typeDeclDefine;
    struct {
      ast_ModReference *path;
    } useStmnt;
    struct {
      ast_ModBinding *name;
      ast_Stmnt *stmnts;
      usize stmnts_len;
    } modStmnt;
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
com_str ast_strTypeKind(ast_TypeKind val);
com_str ast_strTypeUnaryOpKind(ast_TypeUnaryOpKind val);
com_str ast_strTypeBinaryOpKind(ast_TypeBinaryOpKind val);
com_str ast_strValKind(ast_ValKind val);
com_str ast_strLabelReferenceKind(ast_LabelReferenceKind val);
com_str ast_strLabelBindingKind(ast_LabelBindingKind val);
com_str ast_strMatchCaseKind(ast_MatchCaseKind val);
com_str ast_strValUnaryOpKind(ast_ValUnaryOpKind val);
com_str ast_strValBinaryOpKind(ast_ValBinaryOpKind val);
com_str ast_strStmntKind(ast_StmntKind val);
com_str ast_strPatUnaryOpKind(ast_PatUnaryOpKind val);
com_str ast_strBindingKind(ast_BindingKind val);
com_str ast_strFieldKind(ast_FieldKind val);
com_str ast_strReferenceKind(ast_ReferenceKind val);
com_str ast_strModReferenceKind(ast_ModReferenceKind val);
com_str ast_strModBindingKind(ast_ModBindingKind val);

#endif
