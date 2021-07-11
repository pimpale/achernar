use super::dlogger::DiagnosticLogger;
use super::hir;
use bumpalo::Bump;
use std::alloc::Allocator;
use hashbrown::HashMap;
use num_bigint::BigInt;
use num_rational::BigRational;


struct Context<'hir, A:Allocator> {
  vars: HashMap<Vec<u8, A>, u8, A>,
}

enum Ty<'mir, A:Allocator> {
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
      left: &'mir Ty<'mir, A>,
      right: &'mir Ty<'mir, A>,
  },
  Struct {
      fields: HashMap<Vec<u8, A>, Ty<'mir, A>>
  },
  Enum {
      fields: HashMap<Vec<u8, A>, Ty<'mir, A>>
  },
  Fn {
      inType: &'mir Ty<'mir, A>,
      outType: &'mir Ty<'mir, A>,
  },
  Never
}

enum Val<'mir, A:Allocator> {
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
      left: &'mir Val<'mir, A>,
      right: &'mir Val<'mir, A>,
  },
  Struct {
      fields: HashMap<Vec<u8, A>, Val<'mir, A>>
  },
  Enum {
      fields: HashMap<Vec<u8, A>, Val<'mir, A>>
  },
  Fn {
      code: CodeChunk<'mir, A>
  },
  Ty(Ty<'mir, A>),
  Never {
      returned: &'mir Val<'mir, A>,
      levelsUp: u32
  }
}

pub fn eval<'ast, 'hir, 'mir, HirAllocator: Allocator>(
  hir: &'hir hir::Expr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {
  
}

pub fn construct_thir<'ast, 'hir, 'mir, HirAllocator: Allocator>(
  hir: &'hir hir::Expr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {
  
}


pub fn construct_mir<'ast, 'hir, 'mir, HirAllocator: Allocator>(
  hir: &'hir hir::Expr<'hir, 'ast, HirAllocator>,
  allocator: &'mir Bump,
  mut dlogger: DiagnosticLogger,
) {
  
}
