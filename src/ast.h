#ifndef AST_H_
#define AST_H_

#include <stdbool.h>
#include <stdint.h>

#include "error.h"
#include "lexer.h"
#include "token.h"

typedef enum {
  SK_None,
  SK_FnDecl,
  SK_VarDecl,
  SK_TypeAliasStmnt,
  SK_Expr,
} StmntKind;

typedef enum {
  VEK_None,
  VEK_IntLiteral,
  VEK_BoolLiteral,
  VEK_FloatLiteral,
  VEK_CharLiteral,
  VEK_StringLiteral,
  VEK_StructLiteral,
  VEK_BinaryOp,
  VEK_UnaryOp,
  VEK_Call,
  VEK_If,
  VEK_When,
  VEK_Else,
  VEK_While,
  VEK_With,
  VEK_Pass,
  VEK_Break,
  VEK_Continue,
  VEK_Return,
  VEK_Match,
  VEK_Block,
  VEK_FieldAccess,
  VEK_Reference,
} ValueExprKind;

typedef enum {
  TEK_None,        // Error type
  TEK_Omitted,     // Omitted
  TEK_Void,        // void type
  TEK_Reference,   // Reference (primitive or aliased or path)
  TEK_Struct,      // struct
  TEK_Fn,          // function pointer
  TEK_UnaryOp,     // $ or @
  TEK_BinaryOp,    // , or |
  TEK_FieldAccess, // .
} TypeExprKind;

typedef enum PatternExprValueRestrictionKind_e {
  PEVRK_CompEqual,        // ==
  PEVRK_CompNotEqual,     // !=
  PEVRK_CompLess,         // <
  PEVRK_CompLessEqual,    // <=
  PEVRK_CompGreater,      // >
  PEVRK_CompGreaterEqual, // >=
} PatternExprValueRestrictionKind;

typedef enum {
  PEK_None, // Error type
  PEK_ValueRestriction, // matches a value, and optionally binds it
  PEK_TypeRestriction, // matches a type, and optionally binds it (this is also the one used for wildcards)
  PEK_StructWrapper, // a container for struct based patterns
  PEK_StructValueRestriction, // (in struct) by name matches a value to a literal
  PEK_StructTypeRestriction, // (in struct) by name matches a value to a type
  PEK_StructRest,            // (in struct) all values that were not matched
  PEK_UnaryOp,  // ()
  PEK_BinaryOp, // , |
} PatternExprKind;

typedef struct TypeExpr_s TypeExpr;
typedef struct ValueExpr_s ValueExpr;
typedef struct Binding_s Binding;
typedef struct Stmnt_s Stmnt;
typedef struct Pattern_s Pattern;

typedef struct Comment_s {
  Span span;
  char *scope;
  char *data;
} Comment;

typedef struct Pattern_s {
  PatternExprKind kind;
  Span span;

  // diagnostics
  Diagnostic *diagnostics;
  size_t diagnostics_length;

  bool has_binding;
  char* binding;

  union {
    struct {
      PatternExprValueRestrictionKind restriction;
      ValueExpr* value;
    } literalRestriction;
    struct {
      TypeExpr* type;
    } typeRestriction;
    struct {
      Pattern* patterns;
      size_t patterns_length;
    } structWrapper;
    struct {
      PatternExprValueRestrictionKind restriction;
      char* field;
      ValueExpr* value;
    } structValueRestriction;
    struct {
      TypeExpr* type;
      char* field;
      ValueExpr* value;
    } structTypeRestriction;
    struct {
      enum PatternExprUnaryOperatorKind_e {
        PEUOK_Group,
      } operator;
      Pattern* operand;
    } unaryOperator;
    struct {
      enum PatternExprBinaryOperatorKind_e {
        PEBOK_Product,
        PEBOK_Sum,
      } operator;
      Pattern* operand1;
      Pattern* operand2;
    } binaryOperator;
  };
} Pattern;


typedef struct Path_s {
  Span span;
  // diagnostics
  Diagnostic *diagnostics;
  size_t diagnostics_length;

  // comments
  Comment *comments;
  size_t comments_length;

  char **pathSegments;
  size_t pathSegments_length;
} Path;

// Expressions and operations yielding a type
typedef struct TypeExpr_s {
  TypeExprKind kind;
  Span span;

  // diagnostics
  Diagnostic *diagnostics;
  size_t diagnostics_length;

  // comments
  Comment *comments;
  size_t comments_length;

  union {
    struct {
      Path *path;
    } referenceExpr;
    struct {
      enum TypeExprStructKind_e {
        TESK_Struct,
        TESK_Pack,
        TESK_Union,
        TESK_Enum,
      } kind;
      Binding *members;
      size_t members_length;
      bool trailing_comma;
    } structExpr;
    struct {
      TypeExpr *parameters;
      size_t parameters_length;
      bool parameters_trailing_comma;
      TypeExpr *result;
    } fnExpr;
    struct {
      enum TypeExprUnaryOpKind_e {
        TEUOK_Ref,    // $
        TEUOK_Deref,  // @
      }
      operator;
      struct TypeExpr_s *operand;
    } unaryOp;
    struct {
      enum TypeExprBinaryOpKind_e {
        TEBOK_Product,    // ,
        TEBOK_Sum,        // |
      }
      operator;
      struct TypeExpr_s *operand;
    } binaryOp;
    struct {
      struct TypeExpr_s *value;
      char *field;
    } fieldAccess;
  };
} TypeExpr;

// Represents var:Type
typedef struct Binding_s {
  Span span;
  Diagnostic *diagnostics;
  size_t diagnostics_length;

  // comments
  Comment *comments;
  size_t comments_length;

  // Elements
  char *name;
  TypeExpr *type;
} Binding;

typedef struct ValueExpr_s {
  ValueExprKind kind;
  Span span;

  // diagnostics
  Diagnostic *diagnostics;
  size_t diagnostics_length;

  // comments
  Comment *comments;
  size_t comments_length;

  union {
    struct {
      uint64_t value;
    } intLiteral;
    struct {
      bool value;
    } boolLiteral;
    struct {
      double value;
    } floatLiteral;
    struct {
      char value;
    } charLiteral;
    struct {
      char *value;
      size_t value_length;
    } stringLiteral;
    struct {
      struct {
        Span span;
        Diagnostic *diagnostics;
        size_t diagnostics_length;

        // comments
        Comment *comments;
        size_t comments_length;

        char *name;
        ValueExpr *value;
      } * entries;
      size_t entries_length;
    } structLiteral;
    struct {
      ValueExpr *value;
      char *field;
    } fieldAccess;
    struct {
      Path *path;
    } reference;
    struct {
      enum ValueExprUnaryOpKind_e {
        VEUOK_Negate,     // -
        VEUOK_Posit,      // +
        VEUOK_LogicalNot, // !
        VEUOK_BitNot,     // ~
        VEUOK_Ref,        // $
        VEUOK_Deref       // @
      }
      operator;
      ValueExpr *operand;
    } unaryOp;
    struct {
      enum ValueExprBinaryOpKind_e {
        VEBOK_Add,              // +
        VEBOK_Sub,              // -
        VEBOK_Mul,              // *
        VEBOK_Div,              // /
        VEBOK_Mod,              // %
        VEBOK_BitAnd,           // &
        VEBOK_BitOr,            // |
        VEBOK_BitXor,           // ^
        VEBOK_BitShl,           // <<
        VEBOK_BitShr,           // >>
        VEBOK_LogicalAnd,       // &&
        VEBOK_LogicalOr,        // ||
        VEBOK_CompEqual,        // ==
        VEBOK_CompNotEqual,     // !=
        VEBOK_CompLess,         // <
        VEBOK_CompLessEqual,    // <=
        VEBOK_CompGreater,      // >
        VEBOK_CompGreaterEqual, // >=
        VEBOK_Pipeline,         // ->
        VEBOK_Assign,           // =
        VEBOK_AssignAdd,        // +=
        VEBOK_AssignSub,        // -=
        VEBOK_AssignMul,        // *=
        VEBOK_AssignDiv,        // /=
        VEBOK_AssignMod,        // &=
        VEBOK_AssignBitAnd,     // &=
        VEBOK_AssignBitOr,      // |=
      }
      operator;
      ValueExpr *left_operand;
      ValueExpr *right_operand;
    } binaryOp;
    struct {
      ValueExpr *condition;
      ValueExpr *body;
      ValueExpr *else_body;
    } ifExpr;
    struct {
      ValueExpr *condition;
      ValueExpr *body;
    } whenExpr;
    struct {
      ValueExpr *condition;
      ValueExpr *body;
    } whileExpr;
    struct {
      ValueExpr *function;
      ValueExpr *arguments;
      size_t arguments_length;
      bool trailing_comma;
    } callExpr;
    struct {
      ValueExpr *value;
    } returnExpr;
    struct {
      ValueExpr *value;
    } breakExpr;
    struct Match_s {
      ValueExpr *value;
      struct MatchCaseExpr_s {
        Span span;
        Diagnostic *diagnostics;
        size_t diagnostics_length;

        // comments
        Comment *comments;
        size_t comments_length;

        ValueExpr *pattern;
        ValueExpr *value;
      } * cases;
      size_t cases_length;
      bool trailing_comma;
    } matchExpr;
    struct Block_s {
      Stmnt *statements;
      size_t statements_length;
      bool suppress_value;
    } blockExpr;
  };
} ValueExpr;

typedef struct Stmnt_s {
  StmntKind kind;
  Span span;
  // diagnostics
  Diagnostic *diagnostics;
  size_t diagnostics_length;

  // comments
  Comment *comments;
  size_t comments_length;

  union {
    struct {
      char *name;
      Binding *params;
      size_t params_length;
      bool params_trailing_comma;
      TypeExpr *type;
      ValueExpr *body;
    } fnDecl;
    struct {
      Binding *binding;
      ValueExpr *value;
    } varDecl;
    struct {
      TypeExpr *type;
      char *name;
    } typeAliasStmnt;
    struct {
      ValueExpr *value;
    } exprStmnt;
  };
} Stmnt;

typedef struct TranslationUnit_s {
  Span span;               // span of the translation unit

  Diagnostic *diagnostics; // any errors that occur during parsing
  size_t diagnostics_length;

  Stmnt *statements;          // The top level is just a series of statements
  size_t statements_length; // The number of statements

  Comment *comments;        // top level comments
  size_t comments_length; // number of comments

} TranslationUnit;

#endif
