use super::ast;
use std::alloc::Allocator;
use hashbrown::HashMap;


#[derive(Debug)]
pub enum Ty<'thir, TA: Allocator> {
  Ty,
  Nil,
  Bool,
  Int {
    // INVARIANT: bits is 8 | 16 | 32 | 64
    bits: u8,
    signed: bool,
  },
  Float {
    // INVARIANT: bits is 32 | 64
    bits: u8,
  },
  Cons {
    left: &'thir Ty<'thir, TA>,
    right: &'thir Ty<'thir, TA>,
  },
  Struct {
    fields: HashMap<Vec<u8, TA>, Ty<'thir, TA>, TA>,
  },
  Enum {
    fields: HashMap<Vec<u8, TA>, Ty<'thir, TA>, TA>,
  },
  Func {
    inType: &'thir Ty<'thir, TA>,
    outType: &'thir Ty<'thir, TA>,
  },
}


// TA-> ThirAllocator

#[derive(Debug)]
pub enum ExprKind<'thir, 'ast, TA: Allocator> {
  // An error when parsing
  None,
  This,
  // Hole
  Hole,
  // Loops until a scope is returned
  Loop(&'thir Expr<'thir, 'ast, TA>),
  // applies a function
  Apply {
    fun: &'thir Expr<'thir, 'ast, TA>,
    arg: &'thir Expr<'thir, 'ast, TA>,
  },
  // Wraps a term in a label that can be deferred or returned from
  Label {
    defers: Vec<Expr<'thir, 'ast, TA>, TA>,
    scope: &'thir Expr<'thir, 'ast, TA>,
  },
  // Returns from a scope with a value
  Ret {
    // the number of labels up to find the correct one
    labels_up: u64,
    value: &'thir Expr<'thir, 'ast, TA>,
  },
  // constructs a new compound ty
  StructLiteral(&'thir Expr<'thir, 'ast, TA>),
  // Accessing the module of a module object
  StructAccess {
    root: &'thir Expr<'thir, 'ast, TA>,
    field: Vec<u8, TA>,
  },
  // A reference to a previously defined variable
  Reference(Vec<u8, TA>),
  // Switches on a pattern
  CaseOf {
    expr: &'thir Expr<'thir, 'ast, TA>,
    case_options: Vec<(Pat<'thir, 'ast, TA>, Expr<'thir, 'ast, TA>), TA>,
  },
  // Quotes pattern
  Pat(&'thir Pat<'thir, 'ast, TA>),
  // Constrain the value
  Annotate {
    expr: &'thir Expr<'thir, 'ast, TA>,
    ty: &'thir Expr<'thir, 'ast, TA>,
  },
  // short circuiting and
  And {
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
  },
  // short circuiting or
  Or {
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
  },
  // Creates a new type that always matches the pattern provided
  Refinement {
    ty: &'thir Expr<'thir, 'ast, TA>,
    refinement: &'thir Pat<'thir, 'ast, TA>,
  },
  // Literals for values
  Nil,
  NilType,
  NeverType,
  Bool(bool),
  BoolType,

  // Type stuff
  // creates a pub struct from an ad hoc compound object
  Struct(&'thir Expr<'thir, 'ast, TA>),
  // creates a disjoint union from an ad hoc compound object
  Enum(&'thir Expr<'thir, 'ast, TA>),
  // Creates a function constructing the compound type provided
  New(&'thir Expr<'thir, 'ast, TA>),
  // creates a tuple
  Cons {
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
  },
  // Create Function
  Defun {
    pattern: &'thir Pat<'thir, 'ast, TA>,
    result: &'thir Expr<'thir, 'ast, TA>,
    infer_pattern: bool,
  },
  // Sequence
  Sequence {
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
  },
  // Assign value to place
  LetIn {
    pat: &'thir Pat<'thir, 'ast, TA>,
    val: &'thir Expr<'thir, 'ast, TA>,
    body: &'thir Expr<'thir, 'ast, TA>,
  },
}

#[derive(Debug)]
pub struct Expr<'thir, 'ast, TA: Allocator> {
  pub source: &'ast ast::Expr,
  pub kind: ExprKind<'thir, 'ast, TA>,
  pub ty: Option<Ty<'thir, TA>>
}

#[derive(Debug)]
pub enum PatKind<'thir, 'ast, TA: Allocator> {
  // An error when parsing
  None,
  // Irrefutably matches a single element to new variable
  BindIdentifier(Vec<u8, TA>),
  // Ignore
  BindIgnore,
  // Hole
  Hole,
  // match with a variety of types
  Range {
    inclusive: bool,
    left_operand: &'thir Expr<'thir, 'ast, TA>,
    right_operand: &'thir Expr<'thir, 'ast, TA>,
  },
  // constrains the type of a value
  Annotate {
    pattern: &'thir Pat<'thir, 'ast, TA>,
    ty: &'thir Expr<'thir, 'ast, TA>,
  },
  // destructure a tuple
  Cons {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Selects a function and calls it with the scrutinee.
  // The result is then refutably matched with the argument provided
  // Example: Array($a, $b, $c) = someFunc();
  ActivePattern {
    function: &'thir Expr<'thir, 'ast, TA>,
    param: &'thir Pat<'thir, 'ast, TA>,
  },
  // Refutable pattern of a value
  Value(&'thir Expr<'thir, 'ast, TA>),
  // Evaluates the second pattern iff the first pattern matches, matches if both are true
  And {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Evaluates the second pattern iff the first pattern doesn't match, matches if at least one is true
  Or {
    left_operand: &'thir Pat<'thir, 'ast, TA>,
    right_operand: &'thir Pat<'thir, 'ast, TA>,
  },
  // Depub structures a field of a pub struct object
  StructLiteral {
    // whether or not the struct TAs an other matcher
    // $* = _
    splat: &'thir Option<Pat<'thir, 'ast, TA>>,
    patterns: Vec<(Vec<u8, TA>, Pat<'thir, 'ast, TA>), TA>,
  },
}

#[derive(Debug)]
pub struct Pat<'thir, 'ast, TA: Allocator> {
  pub source: &'ast ast::Expr,
  pub kind: PatKind<'thir, 'ast, TA>,
  pub ty: Option<Ty<'thir, TA>>
}





