module kkv/vector

import std/core/undiv

// Sorts `v` with the quicksort algorithm.
pub fun sort(v : vector<a>, ?cmp : (a, a) -> <|e> order) : <|e> vector<a>
  val n = v.length
  v.quicksort(0, v.length - 1)

fun quicksort(v : vector<a>, low : int, high : int, ?cmp : (a, a) -> <|e> order) : <|e> vector<a>
  if low >= high then
    return v
  val (partition-index, partitioned) = v.partition(low, high)
  partitioned
    .quicksort(low, (partition-index - 1).pretend-decreasing)
    .quicksort((partition-index + 1).pretend-decreasing, high)

fun partition(v : vector<a>, low : int, high : int, ?cmp : (a, a) -> <|e> order) : <|e> (int, vector<a>)
  match v.at(high)
    Just(pivot) ->
      val (new-index, new-v) = v.partition-help(low, low, high, pivot)
      (new-index, new-v.swap(new-index, high))
    Nothing -> (low, v)

fun partition-help(v : vector<a>, i : int, j : int, high : int, pivot : a, ?cmp : (a, a) -> <|e> order) : <|e> (int, vector<a>)
  if j >= high then
    return (i, v)
  match v.at(j)
    Just(value) ->
      match cmp(value, pivot)
        Gt -> v.partition-help(i, (j + 1).pretend-decreasing, high, pivot)
        _ -> v.swap(i, j).partition-help((i + 1).pretend-decreasing, (j + 1).pretend-decreasing, high, pivot)
    Nothing -> (i, v)

// Swaps the two elements of `v` at indices `i` and `j`.
//
// Returns `v` unchanged if either index is out of bounds.
pub fun swap(v : vector<a>, i : int, j : int) : <> vector<a>
  with pretend-no-div
  catch {
    var v' := v
    val tmp = v'[i]
    v'[i] := v'[j]
    v'[j] := tmp
    v'
  } fn(_)
    v
