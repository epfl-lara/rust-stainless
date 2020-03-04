#![allow(dead_code)]

pub struct Point {
  pub x: i32, pub y: i32
}

// #[ensures="p.x == old(p.x) + s"]
// #[ensures="p.y == old(p.y)"]
fn shift_x(p: &mut Point, s: i32) {
  p.x = p.x + s
}

pub fn align(
    mut segm: (Box<Point>, Box<Point>)
  ) -> (Box<Point>, Box<Point>)
{
  let diff = (*segm.0).x - (*segm.1).x;
  // let diff = segm.0.x - segm.1.x;
  shift_x(&mut segm.1, diff);
  // assert!((*segm.0).x == (*segm.1).x);
  segm
}

fn main() {
  let p1 = Box::new(Point { x: 1, y: 2 });
  let p2 = Box::new(Point { x: 3, y: 4 });
  let (p1, p2) = align((p1, p2));
  assert_eq!(p1.x, p2.x);
}
