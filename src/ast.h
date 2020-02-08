#ifndef AST_H_
#define AST_H_

#include <stdbool.h>
#include <stdint.h>

#include "error.h"
#include "token.h"

typedef enum {
  StmntFuncDecl,
  StmntVarDecl,
  StmntStructDecl,
  StmntTypeAliasDecl,
  StmntExpr,
} StmntType;

typedef enum {
  ExprIntLiteral,
  ExprFloatLiteral,
  ExprCharLiteral,
  ExprStringLiteral,
  ExprArrayLiteral,
  ExprStructLiteral,
  ExprBinaryOp,
  ExprUnaryOp,
  ExprIndex,
  ExprCall,
  ExprFieldAccess,
  ExprPipe,
  ExprIf,
  ExprWhile,
  ExprFor,
  ExprWith,
  ExprBreak,
  ExprContinue,
  ExprReturn,
  ExprMatch,
  ExprEntry,
  ExprBlock,
} ExprType;

typedef enum {
  BinaryOpExprAdd,             // +
  BinaryOpExprSub,             // -
  BinaryOpExprMul,             // *
  BinaryOpExprDiv,             // /
  BinaryOpExprMod,             // %
  BinaryOpExprBitAnd,          // &
  BinaryOpExprBitOr,           // |
  BinaryOpExprBitXor,          // ^
  BinaryOpExprBitShl,          // <<
  BinaryOpExprBitShr,          // >>
  BinaryOpExprLogicalAnd,      // &&
  BinaryOpExprLogicalOr,       // ||
  BinaryOpExprCompEqual,       // ==
  BinaryOpExprCompNotEqual,    // !=
  BinaryOpExprCompLess,        // <
  BinaryOpExprCompLessEqual,   // <=
  BinaryOpExprCompGreater,     // >
  BinaryOpExprCompGreaterEqual // >=
} BinaryOpExprType;

typedef enum {
  UnaryOpExprNegate,     // -
  UnaryOpExprLogicalNot, // !
  UnaryOpExprBitNot,     // ~
  UnaryOpExprRef,        // $
  UnaryOpExprDeref       // @
} UnaryOpExprType;

#define STANDARD_AST_STUFF                                                     \
  Diagnostic *errors;                                                          \
  uint64_t errorLength;

struct Expr_s {
  ExprType type;
  void *value;
};

struct IntLiteralExpr_s {
  uint64_t value;
  STANDARD_AST_STUFF
};

struct FloatLiteralExpr_s {
  double value;
  STANDARD_AST_STUFF
};

struct CharLiteralExpr_s {
  char value;
  STANDARD_AST_STUFF
};

struct StringLiteralExpr_s {
  char *value;
  uint64_t length; // Number of characters in string
  STANDARD_AST_STUFF
};

struct ArrayLiteralExpr_s {
  char *type;              // Type of array
  struct Expr_s *elements; // List
  uint64_t length;         // Number of elements
  STANDARD_AST_STUFF
};

struct EntryExpr_s {
  struct Expr_s *key;   // An integer literal to be matched
  struct Expr_s *value; // The returned value
  STANDARD_AST_STUFF
};

struct StructLiteralExpr_s {
  char *structName;                  // Name of struct
  struct EntryExpr_s *structEntries; // List of structEntries
  uint64_t length;                   // number of structAttributes specified
  STANDARD_AST_STUFF
};

struct BinaryOpExpr_s {
  BinaryOpExprType type; // The type of the operation
  struct Expr_s *a;      // First operand
  struct Expr_s *b;      // Second operand
  STANDARD_AST_STUFF
};

struct UnaryOpExpr_s {
  UnaryOpExprType type;
  struct Expr_s *a; // Operand
  STANDARD_AST_STUFF
};

struct IndexExpr_s {
  struct Expr_s *pointer; // The pointer to be referenced
  struct Expr_s *index;   // Expression evaluating to the index
  STANDARD_AST_STUFF
};

struct CallExpr_s {
  struct Expr_s *function;  // The function being called
  struct Expr_s *arguments; // The arguments to this function
  uint64_t length;          // Number of arguments
  STANDARD_AST_STUFF
};

struct FieldAccessExpr_s {
  struct Expr_s *record; // the struct
  char *field;           // the field of the struct
  STANDARD_AST_STUFF
};

struct PipeExpr_s {
  struct Expr_s *source;
  struct Expr_s *transformation;
  STANDARD_AST_STUFF
};

struct IfExpr_s {
  struct Expr_s *condition;
  struct Expr_s *body;
  bool hasElse;
  struct Expr_s *elsebody;
  STANDARD_AST_STUFF
};

struct WhileExpr_s {
  struct Expr_s *condition;
  struct Expr_s *body;
  STANDARD_AST_STUFF
};

struct ForExpr_s {
  struct Stmnt_s *init;
  struct Expr_s *test;
  struct Expr_s *update;
  struct Expr_s *body;
  STANDARD_AST_STUFF
};

struct WithExpr_s {
  struct Stmnt_s *constructor; // Statement called to construct environment
  struct Stmnt_s
      *destructor; // Statement that will always be called before exit
  struct Expr_s *body;
  STANDARD_AST_STUFF
};

struct BreakExpr_s {
  STANDARD_AST_STUFF
};

struct ContinueExpr_s {
  STANDARD_AST_STUFF
};

struct ReturnExpr_s {
  struct Expr_s *value;
  STANDARD_AST_STUFF
};

struct MatchExpr_s {
  struct EntryExpr_s *cases; // Points to array of cases
  uint64_t length;           // number of cases
  STANDARD_AST_STUFF
};

struct BlockExpr_s {
  struct Stmnt_s *statements; // The statements of the block
  uint64_t length;            // The number of statements
  struct Expr_s *lastExpr;    // The ending expression (value gets passed on)
  bool hasExpr; // If it has an ending expression or it passes on () type
  STANDARD_AST_STUFF
};

struct Stmnt_s {
  StmntType type; // The type of statement
  void *value;    // The value of the statement
  STANDARD_AST_STUFF
};

struct FuncDeclStmnt_s {
  char *name;                       // Name of Function
  struct VarDeclStmnt_s *arguments; // Arguments to the function
  uint64_t arguments_length;
  char *type;
  struct Expr_s *body;
  STANDARD_AST_STUFF
};

struct VarDeclStmnt_s {
  char *name;     // The name of the variable
  char *type;     // The type of the variable
  bool isMutable; // If the variable is mutable
  STANDARD_AST_STUFF
};

struct StructDeclStmnt_s {
  char *name; // The name of the struct (struct is not necessary)
  struct VarDeclStmnt_s *fields; // An array of the fields in the struct
  uint64_t length;               // The number of elements in fields
  STANDARD_AST_STUFF
};

struct AliasDeclStmnt_s {
  char *type;  // The original type name
  char *alias; // The new type name
  STANDARD_AST_STUFF
};

struct ExprStmnt_s {
  struct Expr_s expr; // the expression
  STANDARD_AST_STUFF
};

struct TranslationUnit_s {
  struct Stmnt_s *statements; // The top level is just a series of statements
  uint64_t length;            // The number of statements
  STANDARD_AST_STUFF
};

#define DECL_RESULT_TYPE(type) typedef struct type##_s type;

DECL_RESULT_TYPE(Expr)              // Expression
DECL_RESULT_TYPE(IntLiteralExpr)    // Integer Literal Expression
DECL_RESULT_TYPE(FloatLiteralExpr)  // Float Literal Expression
DECL_RESULT_TYPE(CharLiteralExpr)   // Float Literal Expression
DECL_RESULT_TYPE(StringLiteralExpr) // Float Literal Expression
DECL_RESULT_TYPE(ArrayLiteralExpr)  // Array Literal Expression
DECL_RESULT_TYPE(StructLiteralExpr) // Array Literal Expression
DECL_RESULT_TYPE(BinaryOpExpr)      // Binary Operator Expression
DECL_RESULT_TYPE(UnaryOpExpr)       // Unary Operator Expression
DECL_RESULT_TYPE(IndexExpr)         // Index Expresson
DECL_RESULT_TYPE(CallExpr)          // Function Call Expression
DECL_RESULT_TYPE(FieldAccessExpr)   // Field Access Expression
DECL_RESULT_TYPE(PipeExpr)          // Pipeline Expression
DECL_RESULT_TYPE(IfExpr)            // If Expression
DECL_RESULT_TYPE(WhileExpr)         // While Expression
DECL_RESULT_TYPE(ForExpr)           // For Expression
DECL_RESULT_TYPE(WithExpr)          // With Expression
DECL_RESULT_TYPE(BreakExpr)         // Break Expression
DECL_RESULT_TYPE(ContinueExpr)      // Continue Expression
DECL_RESULT_TYPE(ReturnExpr)        // Return Expression
DECL_RESULT_TYPE(MatchExpr)         // Match Expression
DECL_RESULT_TYPE(EntryExpr)         // Match or Struct Entry Expression
DECL_RESULT_TYPE(BlockExpr)         // Expression in Parentheses
DECL_RESULT_TYPE(Stmnt)             // Statement
DECL_RESULT_TYPE(FuncDeclStmnt)     // Function Declaration Statement
DECL_RESULT_TYPE(VarDeclStmnt)      // Variable Declaration Statement
DECL_RESULT_TYPE(StructDeclStmnt)   // Struct Declaration Statement
DECL_RESULT_TYPE(AliasDeclStmnt)    // Type Alias Declaration Statement
DECL_RESULT_TYPE(ExprStmnt)         // Expression Statement
DECL_RESULT_TYPE(TranslationUnit)   // The whole program

#endif
