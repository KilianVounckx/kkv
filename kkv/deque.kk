module kkv/deque

// Double ended queue based on head-tail linked list.
pub struct deque<a>
  cons-list : list<a>
  snoc-list : list<a>

// The empty deque.
pub val empty : forall<a> deque<a> = Deque([], [])

// Convert a deque to a list.
pub fun list(xs : deque<a>) : <> list<a>
  xs.cons-list.append(xs.snoc-list.reverse)

// Convert a list to a deque.
pub fun list/deque(xs : list<a>) : <> deque<a>
  Deque(cons-list = xs, snoc-list = [])

// Add `x` to `xs` in the beginning.
pub fun cons(x : a, xs : deque<a>) : <> deque<a>
  xs(cons-list = Cons(x, xs.cons-list))

// Add `x` to `xs` at the end.
pub fun snoc(xs : deque<a>, x : a) : <> deque<a>
  xs(snoc-list = Cons(x, xs.snoc-list))

// Get `xs`'s first element and deque without it if it's not empty.
pub fun uncons(xs : deque<a>) : maybe<(a, deque<a>)>
  match xs.cons-list
    Cons(x, xx) -> Just((x, xs(cons-list = xx)))
    Nil ->
      match xs.snoc-list.reverse
        Cons(x, xx) -> Just((x, Deque(cons-list = xx, snoc-list = [])))
        Nil -> Nothing

// Get `xs`'s last element and deque without it if it's not empty.
pub fun unsnoc(xs : deque<a>) : maybe<(deque<a>, a)>
  match xs.snoc-list
    Cons(x, xx) -> Just((xs(snoc-list = xx), x))
    Nil ->
      match xs.cons-list.reverse
        Cons(x, xx) -> Just((Deque(cons-list = [], snoc-list = xx), x))
        Nil -> Nothing
