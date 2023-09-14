module Peano with

datatype Nat = Zero | Succ Nat

fun fromInt (n: Int): Nat = 
  if n <= 0 then Zero
  else Succ (fromInt (n - 1))

fun toInt (n: Nat): Int = 
  match n with
  | Zero   => 0
  | Succ m => toInt m + 1
  end

fun add (m: Nat) (n: Nat): Nat = 
  match m with
  | Zero   => n
  | Succ o => add o (Succ n)
  end

/* TODO: Monus */

fun mul (m: Nat) (n: Nat): Nat =
  fromInt (toInt m * toInt n)
