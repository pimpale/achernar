#ifndef AST_H_
#define AST_H_

#include <stdbool.h>
#include <stdint.h>

#include "error.h"
#include "lexer.h"
#include "token.h"

typedef enum {
  StmntFuncDecl,
  StmntVarDecl,
  StmntStructDecl,
  StmntTypeAliasDecl,
  StmntEntry,
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
  ExprCall,
  ExprIf,
  ExprWhile,
  ExprFor,
  ExprWith,
  ExprBreak,
  ExprContinue,
  ExprReturn,
  ExprMatch,
  ExprBlock,
} ExprType;

typedef enum {
  BinaryOpExprAdd,              // +
  BinaryOpExprSub,              // -
  BinaryOpExprMul,              // *
  BinaryOpExprDiv,              // /
  BinaryOpExprMod,              // %
  BinaryOpExprBitAnd,           // &
  BinaryOpExprBitOr,            // |
  BinaryOpExprBitXor,           // ^
  BinaryOpExprBitShl,           // <<
  BinaryOpExprBitShr,           // >>
  BinaryOpExprLogicalAnd,       // &&
  BinaryOpExprLogicalOr,        // ||
  BinaryOpExprCompEqual,        // ==
  BinaryOpExprCompNotEqual,     // !=
  BinaryOpExprCompLess,         // <
  BinaryOpExprCompLessEqual,    // <=
  BinaryOpExprCompGreater,      // >
  BinaryOpExprCompGreaterEqual, // >=
  BinaryOpExprFieldAccess,      // .
  BinaryOpExprArrayAccess,      // []
  BinaryOpExprPipeline,         // ->
} BinaryOpExprType;

typedef enum {
  UnaryOpExprNegate,     // -
  UnaryOpExprLogicalNot, // !
  UnaryOpExprBitNot,     // ~
  UnaryOpExprRef,        // $
  UnaryOpExprDeref       // @
} UnaryOpExprType;

#define STANDARD_AST_STUFF \
    Span span; \
    Diagnostic error; \

#define DECL_TYPE(type) typedef struct type##_s type;

DECL_TYPE(ExprProxy)              // Expression
DECL_TYPE(StmntProxy)             // Statement
DECL_TYPE(IntLiteralExpr)         // Integer Literal Expression
DECL_TYPE(FloatLiteralExpr)       // Float Literal Expression
DECL_TYPE(CharLiteralExpr)        // Float Literal Expression
DECL_TYPE(StringLiteralExpr)      // Float Literal Expression
DECL_TYPE(ArrayLiteralExpr)       // Array Literal Expression
DECL_TYPE(StructLiteralEntryExpr) // Array Literal Expression
DECL_TYPE(StructLiteralExpr)      // Array Literal Expression
DECL_TYPE(BinaryOpExpr)           // Binary Operator Expression
DECL_TYPE(UnaryOpExpr)            // Unary Operator Expression
DECL_TYPE(CallExpr)               // Function Call Expression
DECL_TYPE(FieldAccessExpr)        // Field Access Expression
DECL_TYPE(IfExpr)                 // If Expression
DECL_TYPE(WhileExpr)              // While Expression
DECL_TYPE(ForExpr)                // For Expression
DECL_TYPE(WithExpr)               // With Expression
DECL_TYPE(BreakExpr)              // Break Expression
DECL_TYPE(ContinueExpr)           // Continue Expression
DECL_TYPE(ReturnExpr)             // Return Expression
DECL_TYPE(MatchCaseExpr)          // Match or Struct Entry Expression
DECL_TYPE(MatchExpr)              // Match Expression
DECL_TYPE(BlockExpr)              // Expression in Parentheses
DECL_TYPE(FuncDeclStmnt)          // Function Declaration Statement
DECL_TYPE(VarDeclStmnt)           // Variable Declaration Statement
DECL_TYPE(StructDeclStmnt)        // Struct Declaration Statement
DECL_TYPE(AliasDeclStmnt)         // Type Alias Declaration Statement
DECL_TYPE(ExprStmnt)              // Expression Statement
DECL_TYPE(TranslationUnit)        // The whole program

// Even though we forward declare the structs, we do have to keep Proxies on top
struct ExprProxy_s {
  ExprType type;
  void *value;
};

struct StmntProxy_s {
  StmntType type; // The type of statement
  void *value;    // The value of the statement
};

// Exprssions

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
  char *type;                   // Type of array
  struct ExprProxy_s *elements; // List
  uint64_t elements_length;     // Number of elements
  STANDARD_AST_STUFF
};

struct StructLiteralEntryExpr_s {
  char *field;
  struct ExprProxy_s;
  STANDARD_AST_STUFF
};

struct StructLiteralExpr_s {
  char *structName;            // Name of struct
  struct EntryExpr_s *entries; // List of structEntries
  uint64_t entries_length;     // number of structAttributes specified
  STANDARD_AST_STUFF
};

struct BinaryOpExpr_s {
  BinaryOpExprType type; // The type of the operation
  struct ExprProxy_s a;  // First operand
  struct ExprProxy_s b;  // Second operand
  STANDARD_AST_STUFF
};

struct UnaryOpExpr_s {
  UnaryOpExprType type;
  struct ExprProxy_s a; // Operand
  STANDARD_AST_STUFF
};

struct CallExpr_s {
  struct ExprProxy_s function;  // The function being called
  struct ExprProxy_s arguments; // The arguments to this function
  uint64_t arguments_length;    // Number of arguments
  STANDARD_AST_STUFF
};

struct IfExpr_s {
  struct ExprProxy_s condition;
  struct ExprProxy_s body;
  bool hasElse;
  struct ExprProxy_s elsebody;
  STANDARD_AST_STUFF
};

struct WhileExpr_s {
  struct ExprProxy_s condition;
  struct ExprProxy_s body;
  STANDARD_AST_STUFF
};

struct ForExpr_s {
  struct StmntProxy_s init;
  struct ExprProxy_s test;
  struct StmntProxy_s update;
  struct ExprProxy_s body;
  STANDARD_AST_STUFF
};

struct WithExpr_s {
  struct StmntProxy_s constructor; // Statement called to construct environment
  struct StmntProxy_s
      destructor; // Statement that will always be called before exit
  struct ExprProxy_s body;
  STANDARD_AST_STUFF
};

struct BreakExpr_s {
  STANDARD_AST_STUFF
};

struct ContinueExpr_s {
  STANDARD_AST_STUFF
};

struct ReturnExpr_s {
  struct ExprProxy_s value;
  STANDARD_AST_STUFF
};

struct MatchCaseExpr_s {
  struct ExprProxy_s pattern; // The value to match against
  struct ExprProxy_s value;   // The expression to evaluate if true
  STANDARD_AST_STUFF
};

struct MatchExpr_s {
  struct ExprProxy_s value;  // The value to be matched
  struct EntryExpr_s *cases; // Points to array of cases
  uint64_t cases_length;     // number of cases
  STANDARD_AST_STUFF
};

struct BlockExpr_s {
  struct StmntProxy_s *statements; // The statements of the block
  uint64_t statements_length;      // The number of statements
  bool hasExpr; // If it has an ending expression or it passes on () type
  STANDARD_AST_STUFF
};

struct FuncDeclStmnt_s {
  char *name;                       // Name of Function
  struct VarDeclStmnt_s *arguments; // Arguments to the function
  uint64_t arguments_length;
  char *type;
  struct ExprProxy_s body;
  STANDARD_AST_STUFF
};

struct VarDeclStmnt_s {
  char *name;               // The name of the variable
  char *type;               // The original type of the variable
  bool isMutable;           // If the variable is mutable
  uint64_t pointerCount;    // Layers of pointers
  bool hasValue;            // If it has a value assigned
  struct ExprProxy_s value; // The value itself
  STANDARD_AST_STUFF
};

struct StructDeclStmnt_s {
  char *name; // The name of the struct (struct is not necessary)
  struct VarDeclStmnt_s *fields; // An array of the fields in the struct
  uint64_t fields_length;        // The number of elements in fields
  STANDARD_AST_STUFF
};

struct AliasDeclStmnt_s {
  char *type;  // The original type name
  char *alias; // The new type name
  STANDARD_AST_STUFF
};

struct StmntExpr_s {
  struct ExprProxy_s key;   // An integer literal to be matched
  struct ExprProxy_s value; // The returned value
  STANDARD_AST_STUFF
};

struct ExprStmnt_s {
  struct ExprProxy_s expr; // the expression
  STANDARD_AST_STUFF
};

struct TranslationUnit_s {
  struct StmntProxy_s
      *statements;            // The top level is just a series of statements
  uint64_t statements_length; // The number of statements
  STANDARD_AST_STUFF
};

void parseTranslationUnit(TranslationUnit *tup, BufferedLexer *blp);

#endif
