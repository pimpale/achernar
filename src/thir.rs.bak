use super::ast;
use super::hireval::Val;
use std::alloc::Allocator;
// TA-> ThirAllocator

#[derive(Debug)]
pub enum ExprKind<'thir, 'hir, 'ast, HA:Allocator + Clone, TA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Loops until a scope is returned
  Loop(&'thir Expr<'thir, 'hir, 'ast, HA, TA>),
  // applies a function
  Apply {
    fun: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    arg: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label(&'thir Expr<'thir, 'hir, 'ast, HA, TA>),
  // Returns from a scope with a value
  Ret {
    // the number of labels up to find the correct one (de brujin index)
    labels_up: usize,
    value: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
  },
  // constructs a new compound ty
  StructLiteral(Vec<(&'thir Vec<u8, TA>, Expr<'thir, 'hir, 'ast, HA, TA>), TA>),
  // Accessing the module of a module object
  StructAccess {
    root: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    field: Vec<u8, TA>,
  },
  // A reference to a previously defined variable
  // this number refers to the debruijin index of the variable in the environment
  Reference(usize),
  // Switches on a pattern
  CaseOf {
    expr: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    case_options: Vec<(Pat<'thir, 'hir, 'ast, HA, TA>, Expr<'thir, 'hir, 'ast, HA, TA>), TA>,
  },
  // Quotes pattern
  Pat(&'thir Pat<'thir, 'hir, 'ast, HA, TA>),

  // Literals for values
  Nil,
  NilType,
  NeverType,
  Bool(bool),
  BoolType,

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  Struct(&'thir Expr<'thir, 'hir, 'ast, HA, TA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'thir Expr<'thir, 'hir, 'ast, HA, TA>),
  // Creates a function constructing the compound type provided
  New(&'thir Expr<'thir, 'hir, 'ast, HA, TA>),
  // creates a tuple
  Cons {
    fst: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    snd: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
  },
  // Create Function
  Defun {
    pattern: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
    result: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    infer_pattern: bool,
  },
  // Sequence
  Sequence {
    fst: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    snd: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
  },
  // Assign value to place
  LetIn {
    pat: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
    val: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    body: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
  },
}

#[derive(Debug)]
pub struct Expr<'thir, 'hir, 'ast, HA:Allocator + Clone, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: ExprKind<'thir, 'hir, 'ast, HA, TA>,
  pub ty: &'thir Val<'hir, 'ast, HA>,
}

#[derive(Debug)]
pub enum PatKind<'thir, 'hir, 'ast, HA:Allocator + Clone, TA: Allocator + Clone> {
  // An error when parsing
  Error,
  // Irrefutably matches a single element to new variable
  // the variable has already been resolved to debruijin indexes
  BindVariable,
  // Ignore
  BindIgnore,
  // destructure a tuple
  Cons {
    fst: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
    snd: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    fun: &'thir Expr<'thir, 'hir, 'ast, HA, TA>,
    pat: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
  },
  // Refutable pattern of a value
  Value(&'thir Expr<'thir, 'hir, 'ast, HA, TA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  And {
    left_operand: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
    right_operand: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  Or {
    left_operand: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
    right_operand: &'thir Pat<'thir, 'hir, 'ast, HA, TA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral {
    // whether or not the struct TAs an other matcher
    // $* = _
    splat: Option<&'thir Pat<'thir, 'hir, 'ast, HA, TA>>,
    patterns: Vec<(&'thir Vec<u8, TA>, Pat<'thir, 'hir, 'ast, HA, TA>), TA>,
  },
}

#[derive(Debug)]
pub struct Pat<'thir, 'hir, 'ast, HA:Allocator + Clone, TA: Allocator + Clone> {
  pub source: &'ast ast::Expr,
  pub kind: PatKind<'thir, 'hir, 'ast, HA, TA>,
  pub ty: &'thir Val<'hir, 'ast, HA>,
}
