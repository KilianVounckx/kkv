module kkv/list

import std/core/undiv

pub fun map-adjacent(xs : list<a>, f : (a, a) -> <|e> b) : <|e> list<b>
  match xs
    [] -> []
    [_] -> []
    Cons(x, Cons(y, ys)) -> Cons(f(x, y), Cons(y, ys).pretend-decreasing.map-adjacent(f))
