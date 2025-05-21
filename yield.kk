module kkv/yield

// Effect for yielding values.
pub effect yield<a>
  // Yield a single value.
  fun yield(x : a) : ()

// Runs the yielding `action` and returns the resulting value and all of the
// yielded values in a list.
pub fun result-and-list(action : () -> <div,yield<a>|e> r) : <div|e> (r, list<a>)
  var result := []
  with handler
    return(x) (x, result.reverse)
    fun yield(x)
      result := Cons(x, result)
  action()

// Runs the yielding `action` and returns all of the yielded values in a list.
pub fun list(action : () -> <div,yield<a>|e> r) : <div|e> list<a>
  result-and-list(action).snd
