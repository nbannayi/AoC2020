﻿namespace AoC2020.Common.Utilities

// Maths module for AoC 2020 day 13.

module Maths = 

    /// Work out greatest common divisor of two large numbers.
    let rec gcd (a : int64) (b : int64) =
      if b = 0L then 
        abs a
      else 
        gcd b (a % b)

    /// Returns modular inverse of two large numbers.
    let ModularInverse (n : int64) (g : int64) =
      let rec fN n i g e l a =
        match e with
        | 0L -> g
        | _ -> let o = n/e
               fN e l a (n-o*e) (i-o*l) (g-o*a) 
      (n + (fN n 1L 0L g 0L 1L)) % n

    /// Chinese remainder - provides solution x such that x (mod n) = g for each element in passed arrays
    /// where the n's are coprime.
    let ChineseRemainder (n : int64 array) (g : int64 array) =
      match Seq.fold(fun n g -> if (gcd n g) = 1L then n*g else 0L) 1L g with
      |0L -> None
      |fN-> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(ModularInverse g ((fN/g)%g))) 0L n g)%fN)