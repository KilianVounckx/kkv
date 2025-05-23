module kkv/list

import std/core/undiv

// Takes every two adjacent elements of `xs`, applies `f` and puts the results
// back in a list.
pub fun map-adjacent(xs : list<a>, f : (a, a) -> <|e> b) : <|e> list<b>
  match xs
    [] -> []
    [_] -> []
    Cons(x, Cons(y, ys)) -> Cons(f(x, y), Cons(y, ys).pretend-decreasing.map-adjacent(f))

// Transposes the rows and columns of `xs`.
pub fun transpose(xs : list<list<a>>) : <> list<list<a>>
  match xs
    [] -> []
    Cons([], xss) -> xss.transpose
    Cons(Cons(x, xx), xss) ->
      val (hds, tls) = xss.filter-map fn(ys) { match ys { [] -> Nothing; Cons(y, yy) -> Just((y, yy)) } }.unzip
      fun combine(y, h, ys, t)
        Cons(Cons(y, h), Cons(ys, t).pretend-decreasing.transpose)
      combine(x, hds, xx, tls)

// Splits `xs` into length-`n` pieces.
//
// The last piece will be shorter if `n` does not evenly divides the length of
// `xs`.
pub fun chunks(xs : list<a>, n : int) : <> list<list<a>>
  assert("negative size chunks", n > 0)
  val (chunk, rest) = xs.split(n)
  match rest
    [] -> []
    _ -> Cons(chunk, rest.pretend-decreasing.chunks(n))
