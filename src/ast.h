#ifndef AST_H_
#define AST_H_

#include <stdint.h>

#include "error.h"
#include "identifier.h"

typedef enum {
  StmntFuncDec,
  StmntVarDec,
  StmntStructDec,
  StmntExpr,
  StmntTypeAlias,
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
  ExprIfElse,
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

typedef struct BinaryOpExpr_s {
  BinaryOpExprType type; // The type of the operation
  struct Expr_s *a;      // First operand
  struct Expr_s *b;      // Second operand
} BinaryOpExpr;

typedef enum {
  UnaryOpExprNegate,     // -
  UnaryOpExprLogicalNot, // !
  UnaryOpExprBitNot,     // ~
  UnaryOpExprRef,        // $
  UnaryOpExprDeref       // @
} UnaryOpExprType;

// Need to forward declare all structs because recursion

// Expression
struct Expr_s;
// Literal
struct IntLiteralExpr_s;    // Integer Literal Expression
struct FloatLiteralExpr_s;  // Float Literal Expression
struct CharLiteralExpr_s;   // Float Literal Expression
struct StringLiteralExpr_s; // Float Literal Expression
struct ArrayLiteralExpr_s;  // Array Literal Expression
struct StructLiteralExpr_s; // Array Literal Expression
// Operators
struct BinaryOpExpr_s; // Binary Operator Expression
struct UnaryOpExpr_s;  // Unary Operator Expression
struct IndexExpr_s;    // Index Expresson
// Misc
struct CallExpr_s;        // Function Call Expression
struct FieldAccessExpr_s; // Field Access Expression
struct PipeExpr_s;        // Pipeline Expression
// Control Flow
struct IfExpr_s;         // If Expression
struct IfElseExpr_s;     // If Else Expression
struct WhileExpr_s;      // While Expression
struct ForExpr_s;        // For Expression
struct WithExpr_s;       // With Expression
struct BreakExpr_s;      // Break Expression
struct ContinueExpr_s;   // Continue Expression
struct ReturnExpr_s;     // Return Expression
struct MatchExpr_s;      // Match Expression
struct MatchEntryExpr_s; // Match Expression
struct BlockExpr_s;      // Expression in Parentheses

// Statement
struct Stmnt_s;
// Declarations
struct FuncDeclStmnt_s;   // Function Declaration Statement
struct VarDeclStmnt_s;    // Variable Declaration Statement
struct StructDeclStmnt_s; // Struct Declaration Statement
struct AliasDeclStmnt_s;  // Type Alias Declaration Statement
// Misc Statements
struct ExprStmnt_s; // Expression Statement

// Other
struct TranslationUnit_s; // The whole program

typedef struct Expr_s {
  ExprType type;
  void *value;
} Expr;

typedef struct IntLiteralExpr_s {
  uint64_t value;
} IntLiteralExpr;

typedef struct FloatLiteralExpr_s {
  double value;
} FloatLiteralExpr;

typedef struct CharLiteralExpr_s {
  char value;
} CharLiteralExpr;

typedef struct StringLiteralExpr_s {
  char *value;
  uint64_t length; // Number of characters in string
} StringLiteralExpr;

typedef struct ArrayLiteralExpr_s {
  Identifier *type; // Type of array
  Expr *elements;   // List
  uint64_t length;  // Number of elements
} ArrayLiteralExpr;

typedef struct StructLiteralExpr_s {
  Identifier *structName;                // Name of struct
  IdentifierValuePair *structAttributes; // List of structAttributes
  uint64_t length;                       // number of structAttributes specified
} StructLiteralExpr;

typedef struct UnaryOpExpr_s {
  UnaryOpExprType type;
  struct Expr_s *a; // Operand
} UnaryOpExpr;

typedef struct IndexExpr_s {
  struct Expr_s *pointer; // The pointer to be referenced
  struct Expr_s *index;   // Expression evaluating to the index
} IndexExpr;

typedef struct CallExpr_s {
  struct Expr_s *function;  // The function being called
  struct Expr_s *arguments; // The arguments to this function
  uint64_t length;          // Number of arguments
} CallExpr;

typedef struct FieldAccessExpr_s {
  struct Expr_s *record; // the struct
  Identifier *field;     // the field of the struct
} FieldAccessExpr;

typedef struct PipeExpr_s {
  struct Expr_s *source;
  struct Expr_s *transformation;
} PipeExpr;

typedef struct IfExpr_s {
  struct Expr_s *condition;
  struct Expr_s *body;
} IfExpr;

typedef struct IfElseExpr_s {
  struct Expr_s *condition;
  struct Expr_s *ifbody;
  struct Expr_s *elsebody;
} IfElseExpr;

typedef struct WhileExpr_s {
  struct Expr_s *condition;
  struct Expr_s *body;
} WhileExpr;

typedef struct ForExpr_s {
  struct Stmnt_s *init;
  struct Expr_s *test;
  struct Expr_s *update;
  struct Expr_s *body;
} ForExpr;

typedef struct WithExpr_s {
  struct Stmnt_s *constructor; // Statement called to construct environment
  struct Stmnt_s
      *destructor; // Statement that will always be called before exit
  struct Expr_s *body;
} WithExpr;

typedef struct BreakExpr_s {
} BreakExpr;

typedef struct ContinueExpr_s {
} ContinueExpr;

typedef struct ReturnExpr_s {
  struct Expr_s *value;
} ReturnExpr;

typedef struct MatchEntryExpr_s {
  struct IntLiteralExpr_s key; // An integer literal to be matched
  struct Expr_s *value;        // The returned value
} MatchEntryExpr;

typedef struct MatchExpr_s {
  struct MatchEntryExpr_s *cases; // Points to array of cases
  uint64_t length;                // number of cases
} MatchExpr;

typedef struct BlockExpr_s {
  struct Stmnt_s *statements; // The statements of the block
  uint64_t length;            // The number of statements
  struct Expr_s *lastExpr;    // The ending expression (value gets passed on)
  bool hasExpr; // If it has an ending expression or it passes on () type
} BlockExpr;

typedef struct Stmnt_s {
  StmntType type; // The type of statement
  void *value;    // The value of the statement
} Stmnt;

typedef struct FuncDeclStmnt_s {
  Identifier *funcName;             // Name of Function
  struct VarDeclStmnt_s *arguments; // Arguments to the function
  Identifier *type;
  Expr *body;
} FuncDeclStmnt;

typedef struct VarDeclStmnt_s {
  Identifier *type;    // The type of the variable
  Identifier *varName; // The name of the variable
  bool isMutable;      // If the variable is mutable
} VarDeclStmnt;

typedef struct StructDeclStmnt_s {
  Identifier name; // The name of the struct (struct is not necessary)
  struct VarDeclStmnt_s *fields; // An array of the fields in the struct
  uint64_t length;               // The number of elements in fields
} StructDeclStmnt;

typedef struct AliasDeclStmnt_s {
  Identifier *type;  // The original type name
  Identifier *alias; // The new type name
} AliasDeclStmnt;

typedef struct ExprStmnt_s {
  struct Expr_s expr; // the expression
} ExprStmnt;

typedef struct TranslationUnit_s {
  struct Stmnt_s* statements; // The top level is just a series of statements
  uint64_t length; // The number of statements
} TranslationUnit;

// Ast Generation from tokens

typedef struct AstBuilder_s {
  //TODO
} AstBuilder;

typedef struct ResultAstPtr_s {
  AstBuilder *val;
  ErrVal err;
} ResultAstPtr;

// Initializes Ast Builder
AstBuilder* newAst();

ErrVal parse(Ast


#endif
