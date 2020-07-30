enum Maybe<T> {
  Nothing,
  Just(T),
}

fn nothing<T>() -> Maybe<T> {
  Maybe::Nothing
}

fn just<T>(value: T) -> Maybe<T> {
  Maybe::Just(value)
}

fn get_or_else<T>(maybe: Maybe<T>, default: T) -> T {
  match maybe {
    Maybe::Nothing => default,
    Maybe::Just(value) => value,
  }
}

fn flatten<T>(maybemaybe: Maybe<Maybe<T>>) -> Maybe<T> {
  match maybemaybe {
    Maybe::Nothing => Maybe::Nothing,
    Maybe::Just(maybe) => maybe
  }
}

pub fn main() -> () {
  let x = 123;
  let maybe_x = just(x);
  get_or_else(maybe_x, 0);

  let maybemaybe_x = just(nothing());
  get_or_else(flatten(maybemaybe_x), 0);
}
