use std::alloc::Allocator;
use hashbrown::HashMap;
use num_bigint::BigInt;
use num_rational::BigRational;
use super::hir;


struct ContextEntry<'ast, 'hir, 'mir, A:Allocator, HA:Allocator>{
  source: i8
}

struct Context<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
  types: HashMap<Vec<u8, A>, Ty<'ast, 'hir, 'mir, A, HA>, A>,
}


enum Ty<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
  Ty,
  Nil,
  Bool,
  Int,
  Rational,
  BitVec {
      // INVARIANT: bits is 8 | 16 | 32 | 64
      bits:u8,
      signed:bool,
  },
  Float {
      // INVARIANT: bits is 32 | 64
      bits:u8,
  },
  Cons {
      left: &'mir Ty<'ast, 'hir, 'mir, A, HA>,
      right: &'mir Ty<'ast, 'hir, 'mir, A, HA>,
  },
  Struct {
      fields: HashMap<Vec<u8, A>, Ty<'ast, 'hir, 'mir, A, HA>>
  },
  Enum {
      fields: HashMap<Vec<u8, A>, Ty<'ast, 'hir, 'mir, A, HA>>
  },
  Func {
      inType: &'mir Ty<'ast, 'hir, 'mir, A, HA>,
      outType: &'mir Ty<'ast, 'hir, 'mir, A, HA>,
  },
  Never
}

enum Val<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
  Nil,
  Bool(bool),
  Int(BigInt),
  Rational(BigRational),
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
      left: &'mir Val<'ast, 'hir, 'mir, A, HA>,
      right: &'mir Val<'ast, 'hir, 'mir, A, HA>,
  },
  Struct {
      fields: HashMap<Vec<u8, A>, Val<'ast, 'hir, 'mir, A, HA>, A>
  },
  Enum {
      fields: HashMap<Vec<u8, A>, Val<'ast, 'hir, 'mir, A, HA>, A>
  },
  Func {
      code: MirFunc<'ast, 'hir, 'mir, A, HA>
  },
  Ty(Ty<'ast, 'hir, 'mir, A, HA>),
  Never {
      returned: &'mir Val<'ast, 'hir, 'mir, A, HA>,
      levelsUp: u32
  }
}

pub enum Place<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
  Local {
      source: Option<&'hir hir::Pat<'hir, 'ast, HA>>,
      ty: Ty<'ast, 'hir, 'mir, A, HA>
  }

}

pub enum Operand<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
    Move(Place<'ast, 'hir, 'mir, A, HA>),
    Copy(Place<'ast, 'hir, 'mir, A, HA>),
}

pub struct BasicBlock<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
    statements: Vec<Statement<'ast, 'hir, 'mir, A, HA>, A>,
    terminator: Terminator<'ast, 'hir, 'mir, A, HA>
}



pub struct Statement<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
    source: &'hir hir::Expr<'hir, 'ast, HA>,
    kind: StatementKind<'ast, 'hir, 'mir, A, HA>
}

pub enum StatementKind<'ast, 'hir, 'mir, A:Allocator, HA:Allocator>{
  Write {
      target: Place<'ast, 'hir, 'mir, A, HA>,
      value: Operand<'ast, 'hir, 'mir, A, HA>,
  }
}

pub enum Terminator<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
  // leaves the program
  Exit,
  // Calls another function
  Call {
      // the function to call
      function: Operand<'ast, 'hir, 'mir, A, HA>,
      // the argument provided to the call
      arg: Operand<'ast, 'hir, 'mir, A, HA>,
      // the call will write its return value in this place
      write_result: Place<'ast, 'hir, 'mir, A, HA>,
      // execution will continue from this point
      dest: BasicBlock<'ast, 'hir, 'mir, A, HA>
  },
}

pub struct MirFunc<'ast, 'hir, 'mir, A:Allocator, HA:Allocator> {
    root: BasicBlock<'ast, 'hir, 'mir, A, HA>
}
