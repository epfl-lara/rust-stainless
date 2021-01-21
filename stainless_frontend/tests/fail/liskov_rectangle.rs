extern crate stainless;
use stainless::*;

/// The typical violation of the **Liskov substitution principle** can be
/// checked and guarded against with type classes and laws.
trait Rectangle: Sized {
  fn width(&self) -> u32;
  fn height(&self) -> u32;

  fn set_width(&self, width: u32) -> Self;
  fn set_height(&self, height: u32) -> Self;

  #[law]
  fn preserve_height(&self, any: u32) -> bool {
    self.set_width(any).height() == self.height()
  }

  #[law]
  fn preserve_width(&self, any: u32) -> bool {
    self.set_height(any).width() == self.width()
  }
}

struct Rect {
  width: u32,
  height: u32,
}

impl Rectangle for Rect {
  fn width(&self) -> u32 {
    self.width
  }

  fn height(&self) -> u32 {
    self.height
  }

  fn set_width(&self, width: u32) -> Self {
    Rect {
      width,
      height: self.height,
    }
  }

  fn set_height(&self, height: u32) -> Self {
    Rect {
      width: self.width,
      height,
    }
  }
}

struct Square {
  width: u32,
}

impl Rectangle for Square {
  fn width(&self) -> u32 {
    self.width
  }

  fn height(&self) -> u32 {
    self.width
  }

  fn set_width(&self, width: u32) -> Self {
    Square { width }
  }

  fn set_height(&self, height: u32) -> Self {
    Square { width: height }
  }
}
