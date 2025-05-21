module kkv/dict

// `dict<k,v>` represents a finite dictionary from keys of type `k` to values of type `v`.
pub type dict<k,v>
  Node(color : color, left : dict<k,v>, key : k, value : v, right : dict<k,v>)
  Leaf

type color
  Red
  Black

// The empty dict.
pub val empty : forall<k,v> dict<k,v> = Leaf

// Build a dict from a list of key-value pairs.
pub fun list/dict(xs : list<(k,v)>, ?cmp : (k, k) -> <|e> order) : <|e> dict<k,v>
  xs.foldl(empty) fn(acc, (key, value)) acc.insert(key, value)

// Show a dict.
pub fun show(d : dict<k,v>, ?key/show : (k) -> <|e>string, ?value/show : (v) -> <|e> string) : <|e> string
  val data = d.list.map fn((key, value)) { key.show ++ ": " ++ value.show }.join(",")
  "{" ++ data ++ "}"

// Builds a list of key-value pairs from a dict.
pub fun list(d : dict<k,v>) : <> list<(k,v)>
  d.foldr([]) fn(key, value, acc) Cons((key, value), acc)

// Fold the elements of a tree from the left.
pub fun foldl(d : dict<k,v>, init : a, f : (a, k, v) -> <|e> a) : <|e> a
  match d
    Node(_, l, k, v, r) ->
      val left = l.foldl(init, f)
      val mid = f(left, k, v)
      r.foldl(mid, f)
    Leaf -> init

// Fold the elements of a tree from the right.
pub fun foldr(d : dict<k,v>, init : a, f : (k, v, a) -> <|e> a) : <|e> a
  match d
    Node(_, l, k, v, r) ->
      val right = r.foldr(init, f)
      val mid = f(k, v, right)
      l.foldr(mid, f)
    Leaf -> init

// Lookup the value of `key` in `d`.
pub fun lookup(d : dict<k,v>, key : k, ?cmp : (k, k) -> <|e> order) : <|e> maybe<v>
  match d
    Node(_, l, got, value, r) ->
      match cmp(key, got)
        Lt -> l.lookup(key)
        Eq -> Just(value)
        Gt -> r.lookup(key)
    Leaf -> Nothing

// Checks if `d` contains a value with key `key`.
pub fun contains-key(d : dict<k,v>, key : k, ?cmp : (k, k) -> <|e> order) : <|e> bool
  d.lookup(key).is-just

// Inserts the `key`-`value` pair into `m`.
pub fun insert(d : dict<k,v>, key : k, value : v, ?cmp : (k, k) -> <|e> order) : <|e> dict<k,v>
  if d.is-red
    then ins(d, key, value).set-black
    else ins(d, key, value)

fun ins(t : dict<k,v>, key : k, value : v, ?cmp : (k, k) -> <|e> order) : <|e> dict<k,v>
  match t
    Node(Red, l, kx, vx, r) ->
      match cmp(key, kx)
        Lt -> Node(Red, ins(l, key, value), kx, vx, r)
        Eq -> Node(Red, l, key, value, r)
        Gt -> Node(Red, l, kx, vx, ins(r, key, value))
    Node(Black, l, kx, vx, r) ->
      match cmp(key, kx)
        Lt -> if is-red(l) then balance-left(ins(l, key, value), kx, vx, r) else Node(Black, ins(l, key, value), kx, vx, r)
        Eq -> Node(Black, l, key, value, r)
        Gt -> if is-red(r) then balance-right(l, kx, vx, ins(r, key, value)) else Node(Black, l, kx, vx, ins(r, key, value))
    Leaf -> Node(Red, Leaf, key, value, Leaf)

fun balance-left(l : dict<k,v>, key : k, value : v, r : dict<k,v>) : dict<k,v>
  match l
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, lx, kx, vx, rx), ky, vy, Node(Black, ry, key, value, r))
    Node(_, ly, ky, vy, Node(Red, lx, kx, vx, rx))
      -> Node(Red, Node(Black, ly, ky, vy, lx), kx, vx, Node(Black, rx, key, value, r))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, Node(Red, lx, kx, vx, rx), key, value, r)
    Leaf -> Leaf

fun balance-right(l : dict<k,v>, key : k, value : v, r : dict<k,v>) : dict<k,v>
  match r
    Node(_, Node(Red, lx, kx, vx, rx), ky, vy, ry)
      -> Node(Red, Node(Black, l, key, value, lx), kx, vx, Node(Black, rx, ky, vy, ry))
    Node(_, lx, kx, vx, Node(Red, ly, ky, vy, ry))
      -> Node(Red, Node(Black, l, key, value, lx), kx, vx, Node(Black, ly, ky, vy, ry))
    Node(_, lx, kx, vx, rx)
      -> Node(Black, l, key, value, Node(Red, lx, kx, vx, rx))
    Leaf -> Leaf

fun tree/is-red(d : dict<k,v>) : bool
  match d
    Node(Red) -> True
    _         -> False

fun set-black(d : dict<k,v>) : dict<k,v>
  match d
    Node(_, l, k, v, r) -> Node(Black, l, k, v, r)
    _ -> d
