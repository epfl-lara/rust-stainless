// TODO: Ensure we only allow creation of Ref<T> where T is owned?

mod int {
  #[derive(Clone, Copy, Debug)]
  pub struct Ref<'a, T: ?Sized>(&'a T);

  impl<'a, T> Ref<'a, T> {
    pub fn new(r: &'a T) -> Self {
      Ref(r)
    }
  }

  impl<'a, T> PartialEq for Ref<'a, T> {
    fn eq(&self, other: &Self) -> bool {
      std::ptr::eq(self.0, other.0)
    }
  }
  impl<'a, T> Eq for Ref<'a, T> {}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Point(u32, u32);

fn main() {
  use int::Ref;

  let p1 = Point(1, 2);
  let p2 = Point(1, 2);
  let p3 = Point(3, 4);
  
  assert!(p1 == p2);
  assert!(p1 != p3);
  assert!(&p1 == &p2);

  assert!(std::ptr::eq(&p1, &p1));
  assert!(!std::ptr::eq(&p1, &p2));
  assert!(!std::ptr::eq(&p1, &p3));

  assert!(Ref::new(&p1) == Ref::new(&p1));
}
