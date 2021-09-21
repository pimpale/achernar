use super::thir;
use std::alloc::Allocator;



pub fn alpha_equivalent<'thir, TA: Allocator + Clone>(
  a: &thir::Ty<'thir, TA>,
  b: &thir::Ty<'thir, TA>,
) -> bool {



  // TODO validate recursively on
  true
}


pub fn get_ty<'thir, 'ast, TA: Allocator + Clone>(
  val: &thir::Val<'thir, 'ast, TA>,
) -> thir::Ty<'thir, TA> {

}
