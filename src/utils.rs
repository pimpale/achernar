use std::alloc::Allocator;

pub fn clone_in<A: Allocator, T: Clone>(allocator: A, slice: &[T]) -> Vec<T, A> {
  let mut v = Vec::new_in(allocator);
  v.extend_from_slice(slice);
  v
}

pub fn new_vec_from<A: Allocator, I, T>(allocator: A, iter: I) -> Vec<T, A>
where
  I: Iterator<Item = T>,
{
  let mut v = Vec::new_in(allocator);
  v.extend(iter);
  v
}

pub fn update<T, F>(x: &mut T, replace_fn: F) -> T
where F: FnOnce(&mut T) -> T {
    let replacement = replace_fn(x);
    std::mem::replace(x, replacement)
}
