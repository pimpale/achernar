use std::alloc::Allocator;

// Most of the MIR code comes from
// rustc compiler...

pub enum Place<'mir, A:Allocator> {
  Local {
  }
}

pub enum Operand<'mir, A:Allocator> {
    Move(Place<'mir, A>),
    Copy(Place<'mir, A>),
}

pub struct BasicBlock<'mir, A:Allocator> {
    statements: Vec<Statement<'mir, A>, A>,
    terminator: Terminator<'mir, A>
}


pub enum Statement<'mir, A:Allocator> {
  Write {
      target: Place<'mir, A>,
      value: Operand<'mir, A>,
  }
}

pub enum Terminator<'mir, A:Allocator> {
  // leaves the program
  Exit,
  // Calls another function
  Call {
      function: Operand<'mir, A>,
  },


}
