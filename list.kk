module kkv/list

import std/core/undiv

pub fun map-adjacent(xs : list<a>, f : (a, a) -> <|e> b) : <|e> list<b>
  match xs
    [] -> []
    [_] -> []
    Cons(x, Cons(y, ys)) -> Cons(f(x, y), Cons(y, ys).pretend-decreasing.map-adjacent(f))

pub fun transpose(xs : list<list<a>>) : <> list<list<a>>
  match xs
    [] -> []
    Cons([], xss) -> xss.transpose
    Cons(Cons(x, xx), xss) ->
      val (hds, tls) = xss.filter-map fn(ys) { match ys { [] -> Nothing; Cons(y, yy) -> Just((y, yy)) } }.unzip
      fun combine(y, h, ys, t)
        Cons(Cons(y, h), Cons(ys, t).pretend-decreasing.transpose)
      combine(x, hds, xx, tls)
