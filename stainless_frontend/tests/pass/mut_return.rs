extern crate stainless;

#[allow(dead_code)]
pub struct Thing<T> {
  field: T,
}

pub fn fresh_copy_match<T>(
  arg: Option<Thing<T>>,
  error_type: Option<&str>,
) -> Result<Thing<T>, &str> {
  if let Some(thing) = arg {
    Ok(thing)
  } else if let Some(err) = error_type {
    return Err(err);
  } else {
    Err("this is even worse")
  }
}

pub fn fresh_copy_if<T>(arg: Option<Thing<T>>, int: i32) -> Option<Thing<T>> {
  if int > 0 {
    arg
  } else {
    None
  }
}

pub fn fresh_copy_if_return<T>(arg: Option<Thing<T>>, int: i32) -> Option<Thing<T>> {
  if int > 0 {
    arg
  } else {
    return None;
  }
}

pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

pub fn fresh_copy_recursive(list: List<i32>, e: i32) -> List<i32> {
  match list {
    List::Cons(head, tail) if head <= e => {
      List::Cons(head, Box::new(fresh_copy_recursive(*tail, e)))
    }
    _ => List::Cons(e, Box::new(list)),
  }
}
