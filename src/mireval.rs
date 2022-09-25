
use hashbrown::HashMap;
use num_bigint::BigInt;


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

