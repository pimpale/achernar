use super::hir;
use hashbrown::HashMap;
use num_bigint::BigInt;
use std::alloc::Allocator;

pub enum Ty<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Ty,
  Nil,
  Bool,
  BitVec {
    // INVARIANT: bits is 8 | 16 | 32 | 64
    bits: u8,
    signed: bool,
  },
  Float {
    // INVARIANT: bits is 32 | 64
    bits: u8,
  },
  Cons {
    left: &'mir Ty<'ast, 'hir, 'mir, MA, HA>,
    right: &'mir Ty<'ast, 'hir, 'mir, MA, HA>,
  },
  Struct {
    fields: HashMap<Vec<u8, MA>, Ty<'ast, 'hir, 'mir, MA, HA>, MA>,
  },
  Enum {
    fields: HashMap<Vec<u8, MA>, Ty<'ast, 'hir, 'mir, MA, HA>, MA>,
  },
  Func {
    inType: &'mir Ty<'ast, 'hir, 'mir, MA, HA>,
    outType: &'mir Ty<'ast, 'hir, 'mir, MA, HA>,
  },
  // IDK why i have this...
  _dummy(
    std::marker::PhantomData<&'ast HA>,
    std::marker::PhantomData<&'hir HA>,
  ),
}

pub enum Val<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Unit,
  Bool(bool),
  BitVecU8(u8),
  BitVecU16(u16),
  BitVecU32(u32),
  BitVecU64(u64),
  BitVecI8(i8),
  BitVecI16(i16),
  BitVecI32(i32),
  BitVecI64(i64),
  Float32(f32),
  Float64(f64),
  Cons {
    left: &'mir Val<'ast, 'hir, 'mir, MA, HA>,
    right: &'mir Val<'ast, 'hir, 'mir, MA, HA>,
  },
  Struct {
    fields: HashMap<Vec<u8, MA>, Val<'ast, 'hir, 'mir, MA, HA>, MA>,
  },
  Enum {
    fields: HashMap<Vec<u8, MA>, Val<'ast, 'hir, 'mir, MA, HA>, MA>,
  },
  Func {
    code: MirFunc<'ast, 'hir, 'mir, MA, HA>,
  },
  Ty(Ty<'ast, 'hir, 'mir, MA, HA>),
  Never {
    returned: &'mir Val<'ast, 'hir, 'mir, MA, HA>,
    levelsUp: u32,
  },
}

pub enum Place<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Local(i32),
}

pub enum Operand<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Move(Place<'ast, 'hir, 'mir, MA, HA>),
  Copy(Place<'ast, 'hir, 'mir, MA, HA>),
}

pub struct Statement<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  source: &'hir hir::ValExpr<'hir, 'ast, HA>,
  kind: StatementKind<'ast, 'hir, 'mir, MA, HA>,
}

pub enum StatementKind<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  /// pushes a local onto the stack
  DeclareLocal {
    value: RValue<'ast, 'hir, 'mir, MA, HA>,
  },
  /// Mutates a place
  Mutate {
    target: Place<'ast, 'hir, 'mir, MA, HA>,
    value: RValue<'ast, 'hir, 'mir, MA, HA>,
  },
}

pub enum RValue<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Builtin(hir::Builtin),
  Use(Operand<'ast, 'hir, 'mir, MA, HA>),
  GetRef {
      mutable: bool,
      place: Place<'ast, 'hir, 'mir, MA, HA>
  },

}

pub enum ValPatExpr<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Ignore,
  Unit,
  Bool(bool),
  Int(&'ast BigInt),
  Pair(
    &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
    &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
  ),
  And(
    &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
    &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
  ),
  Or(
    &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
    &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
  ),
  Local(Place<'ast, 'hir, 'mir, MA, HA>),
}

pub enum Terminator<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  // switch
  CaseOf {
    place: Place<'ast, 'hir, 'mir, MA, HA>,
    cases: (
      &'mir ValPatExpr<'ast, 'hir, 'mir, MA, HA>,
      &'mir BasicBlock<'ast, 'hir, 'mir, MA, HA>,
    ),
  },
  // leaves the program
  Exit,
  // Calls another function
  Call {
    // the function to call
    function: Operand<'ast, 'hir, 'mir, MA, HA>,
    // the argument provided to the call
    arg: Operand<'ast, 'hir, 'mir, MA, HA>,
    // the call will write its return value in this place
    write_result: Place<'ast, 'hir, 'mir, MA, HA>,
    // execution will continue from this point
    dest: &'mir BasicBlock<'ast, 'hir, 'mir, MA, HA>,
  },
}

pub struct BasicBlock<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  statements: Vec<Statement<'ast, 'hir, 'mir, MA, HA>, MA>,
  terminator: Terminator<'ast, 'hir, 'mir, MA, HA>,
}

pub struct MirFunc<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  root: BasicBlock<'ast, 'hir, 'mir, MA, HA>,
}

pub enum MirModuleEntry<'ast, 'hir, 'mir, MA: Allocator + Clone, HA: Allocator + Clone> {
  Constant { to_eval: BasicBlock },
  Function(MirFunc),
}
