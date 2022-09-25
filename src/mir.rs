use super::hir;
use std::alloc::Allocator;

// Function-Local
pub struct BasicBlockIdx(i32);
pub struct StackVariableIdx(i32);
// Module-Local
pub struct GlobalConstantIdx(i32);

pub enum Place {
  StackVariableIdx(StackVariableIdx),
  GlobalConstantIdx(GlobalConstantIdx),
}

pub struct Statement<'ast, 'hir, HA: Allocator + Clone> {
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
  kind: StatementKind<'ast, 'hir, HA>,
}

pub enum StatementKind<'ast, 'hir, HA: Allocator + Clone> {
  /// pushes a local onto the stack
  DeclareLocal {
    value: RValue<'ast, 'hir, HA>,
  },
  /// Mutates a place
  Mutate {
    target: Place,
    value: RValue<'ast, 'hir, HA>,
  },
}

pub enum RValueKind {
  Builtin(Builtin),
  Use(Place),
  GetRef {
    mutable: bool,
    place: Place,
  },
}

pub struct RValue<'ast, 'hir, HA: Allocator + Clone> {
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
  kind: RValueKind,
}

pub enum Terminator<'ast, 'hir, HA: Allocator + Clone> {
  // switch
  Switches {
    place: Place,
    cases: (GlobalConstantIdx, BasicBlockIdx),
  },
  // leaves the program
  Exit,
  // Calls another function
  Call {
    // the function to call
    function: RValue<'ast, 'hir, HA>,
    // the argument provided to the call
    arg: RValue<'ast, 'hir, HA>,
    // the call will write its return value in this place
    write_result: Place,
    // execution will continue from this point
    dest: BasicBlockIdx
  },
}

pub struct BasicBlock<'ast, 'hir, HA: Allocator + Clone> {
  statements: Vec<Statement<'ast, 'hir, HA>>,
  terminator: Terminator<'ast, 'hir, HA>,
}

pub struct MirFunc<'ast, 'hir, HA: Allocator + Clone> {
  root: BasicBlock<'ast, 'hir, HA>,
}

pub enum MirModuleEntry<'ast, 'hir, HA: Allocator + Clone> {
  Constant {
    toeval: BasicBlock<'ast, 'hir, HA>,
  },
  Function(MirFunc<'ast, 'hir, HA>),
}

#[derive(Debug)]
pub enum IntOp {
  Add,         // (u, u) -> u
  AddOverflow, // (u, u) -> (u, u)
  Sub,         // (u, u) -> u
  SubOverflow, // (u, u) -> (u, u)
  Mul,         // (u, u) -> u
  MulOverflow, // (u, u) -> (u, u)
  Div,         // (u, u) -> u
  Rem,         // (u, u) -> u
  DivRem,      // (u, u) -> (u, u)
  ShlL,        // (u, u) -> u
  ShrL,        // (u, u) -> u
  ShrA,        // (u, u) -> u
  Rol,         // (u, u) -> u
  Ror,         // (u, u) -> u
  And,         // (u, u) -> u
  Or,          // (u, u) -> u
  Xor,         // (u, u) -> u
  Inv,         // u -> u
  Neg,         // u -> u
}

#[derive(Debug)]
pub enum FloatOp {
  Add,    // (f, f) -> f
  Sub,    // (f, f) -> f
  Mul,    // (f, f) -> f
  Div,    // (f, f) -> f
  Rem,    // (f, f) -> f
  DivRem, // (f, f) -> (f, f)
  Neg,    // f -> f
}

#[derive(Debug)]
pub enum RoundingMode {
  RNE, // round to nearest even
  RTZ, // round to zero
  RDN, // round down
  RUP, // round up
}

#[derive(Debug)]
pub struct IntTy {
  pub signed: bool,
  pub size: u8,
}

#[derive(Debug)]
pub struct FloatTy {
  pub size: u8,
}

#[derive(Debug)]
pub enum Builtin {
  UnitTy,
  NeverTy,
  BoolTy,
  // Math with bools
  BoolNot,
  // Ints
  IntTy(IntTy),
  // Floats
  FloatTy(FloatTy),
  // Math with ints
  IntOp {
    ty: IntTy,
    op: IntOp,
  },
  // Math with floats
  FloatOp {
    ty: FloatTy,
    op: FloatOp,
  },
  // Convert one kind of type to another
  ConvIntIntOp {
    src: IntTy,
    dest: IntTy,
  },
  ConvIntFloatOp {
    src: IntTy,
    dest: FloatTy,
  },
  ConvFloatIntOp {
    src: FloatTy,
    dest: IntTy,
    mode: RoundingMode,
  },
  ConvFloatFloatOp {
    src: FloatTy,
    dest: FloatTy,
  },
}
