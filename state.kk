module kkv/state

// State effect.
pub effect state<s>
  // Get the current state.
  fun get() : s
  // Set the state to `x`.
  fun set(x : s) : ()

// Update the state according to `f`.
pub fun update(f : (s) -> <state<s>|e> s) : <state<s>|e> ()
  get().f.set

// Runs the stateful `action` with an initial state `init` and returns the
// resulting value with the final state.
pub fun state(init : s, action : () -> <div,state<s>|e> a) : <div|e> (a, s)
  var state := init
  with handler
    return(x) (x, state)
    fun get() state
    fun set(state')
      state := state'
  action()

// Runs the stateful `action` with an initial state `init` and returns the
// resulting value.
pub fun eval(init : s, action : () -> <div,state<s>|e> a) : <div|e> a
  init.state(action).fst

// Runs the stateful `action` with an initial state `init` and returns the
// final state.
pub fun exec(init : s, action : () -> <div,state<s>|e> a) : <div|e> s
  init.state(action).snd
