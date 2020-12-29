namespace AoC2020.Common.Utilities

// Maths module for AoC 2020 day 13.

open System

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

    /// Convert a passed in int64 to binary padded by given number of 0s. 
    let int64ToBinary (padLength : int) (n: int64) =

        let rec convert (n: int64) : string =
            match n with
            | 0L | 1L -> string n
            | _ ->
                let bit = string (n % 2L)
                (convert (n / 2L)) + bit

        let n' = convert n
        n'.PadLeft(padLength, '0')

    /// Convert a passed in binary string to an int64 value.
    let binaryToInt64 (n : string) : int64 =
        n 
        |> Seq.rev
        |> Seq.mapi (fun i b -> (string >> float) b * 2. ** float i)
        |> Seq.sum
        |> int64

    /// Calculate the result of a^b (mod c).
    let rec modularExp (a : int64) (b : int64) (c : int64) =

        let mutable (m : int64) = 0L

        if a = 0L then
            m <- 0L
        elif b = 0L then
            m <- 1L
        elif b % 2L = 0L then                
            m <- modularExp a (b/2L) c  
            m <- (m * m) % c        
        else
            m <- a % c  
            m <- (m * (modularExp a (b-1L) c) % c) % c
      
        (m + c) % c 
    