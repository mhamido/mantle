module Peano with

data Nat = Zero | Succ Nat

fun fromInt (n: Int): Nat = 
  if n <= 0 then Zero
  else Succ (fromInt (n - 1))

fun toInt (n: Nat): Int = 
  case n of { 
    Zero   -> 0;
    Succ m -> toInt m + 1
  }

fun add (m: Nat) (n: Nat): Nat = 
  case m of {
    Zero   -> n;
    Succ o -> add o (Succ n)
  }

/* TODO: Monus */

fun mul (m: Nat) (n: Nat): Nat =
  fromInt (toInt m * toInt n)