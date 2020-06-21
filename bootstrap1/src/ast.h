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
} ast_Comment;

typedef struct {
  Span span;
  char *name;
  Token *tokens;
  size_t tokens_len;
} ast_Macro;

typedef enum {
  ast_RK_None,
  ast_RK_Path,
} ast_ReferenceKind;

typedef struct {
  Span span;
  ast_ReferenceKind kind;
  union {
    struct {
      char **segments;
      size_t segments_len;
    } path;
  };
} ast_Reference;

typedef enum {
  ast_BK_None,
  ast_BK_Bind,
  ast_BK_Ignore,
} ast_BindingKind;

typedef struct {
  Span span;
  ast_BindingKind kind;
  union {
    struct {
      char *val;
    } bind;
  };
} ast_Binding;

typedef enum {
  ast_FK_None,
  ast_FK_Field,
} ast_FieldKind;

typedef struct {
  Span span;
  ast_FieldKind kind;
  union {
    struct {
      char *name;
    } field;
  };
} ast_Field;

typedef struct {
  Span span;
  ast_Comment *comments;
  size_t comments_len;
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
      size_t members_len;
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
      size_t members_len;
    } structExpr;
    struct {
      ast_Type *parameters;
      size_t parameters_len;
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
  ast_LK_Omitted,
  ast_LK_Label,
} ast_LabelKind;

typedef struct {
  Span span;
  ast_LabelKind kind;
  union {
    struct {
      char *label;
    } label;
  };
} ast_Label;

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
  ast_VEBOK_Mod,
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
  ast_VEBOK_AssignMod,
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
      ast_ValStructMember *members;
      size_t members_len;
    } structExpr;
    struct {
      ast_Val *root;
      ast_Type *type;
    } as;
    struct {
      ast_Val *body;
      ast_Label *label;
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
      size_t parameters_len;
    } call;
    struct {
      ast_Pat *parameters;
      size_t parameters_len;
      ast_Type *type;
      ast_Val *body;
    } fn;
    struct {
      ast_Val *value;
      ast_Label *label;
    } returnExpr;
    struct {
      ast_Val *root;
      ast_MatchCase *cases;
      size_t cases_len;
    } match;
    struct {
      ast_Label *label;
      ast_Stmnt *stmnts;
      size_t stmnts_len;
    } block;
  };
} ast_Val;

typedef enum {
  ast_SK_None,
  ast_SK_Use,
  ast_SK_Macro,
  ast_SK_Namespace,
  ast_SK_ValDecl,
  ast_SK_ValDeclDefine,
  ast_SK_TypeDecl,
  ast_SK_Val,
  ast_SK_DeferStmnt,
} ast_StmntKind;

typedef struct ast_Stmnt_s {
  ast_Common common;
  ast_StmntKind kind;

  size_t comments_len;
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
      ast_Binding *name;
      ast_Stmnt *stmnts;
      size_t stmnts_len;
    } namespaceStmnt;
    struct {
      ast_Macro *macro;
    } macro;
    struct {
      ast_Val *val;
    } val;
    struct {
      ast_Label *label;
      ast_Val *val;
    } deferStmnt;
  };
} ast_Stmnt;

const char *ast_strPatValRestrictionKind(ast_PatValRestrictionKind val);
const char *ast_strPatKind(ast_PatKind val);
const char *ast_strPatBinaryOpKind(ast_PatBinaryOpKind val);
const char *ast_strPatStructMemberKind(ast_PatStructMemberKind val);
const char *ast_strTypeKind(ast_TypeKind val);
const char *ast_strTypeStructKind(ast_TypeStructKind val);
const char *ast_strTypeStructMemberKind(ast_TypeStructMemberKind val);
const char *ast_strTypeUnaryOpKind(ast_TypeUnaryOpKind val);
const char *ast_strTypeBinaryOpKind(ast_TypeBinaryOpKind val);
const char *ast_strValKind(ast_ValKind val);
const char *ast_strLabelKind(ast_LabelKind val);
const char *ast_strMatchCaseKind(ast_MatchCaseKind val);
const char *ast_strValStructMemberKind(ast_ValStructMemberKind val);
const char *ast_strValUnaryOpKind(ast_ValUnaryOpKind val);
const char *ast_strValBinaryOpKind(ast_ValBinaryOpKind val);
const char *ast_strStmntKind(ast_StmntKind val);
const char *ast_strPatUnaryOpKind(ast_PatUnaryOpKind val);
const char *ast_strBindingKind(ast_BindingKind val);
const char *ast_strFieldKind(ast_FieldKind val);
const char *ast_strReferenceKind(ast_ReferenceKind val);

#endif
