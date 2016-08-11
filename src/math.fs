module Math

// ----------------------------------------------------------------------------
// Very complex parser combinator library
// ----------------------------------------------------------------------------

module Parsec = 
  type Parser<'T> = P of (char list -> option<'T * char list>)

  let (<|>) (P p1) (P p2) = P (fun c ->
    match p1 c with 
    | Some r -> Some r
    | _ -> p2 c)

  let (<*>) (P p1) (P p2) = P (fun c ->
    match p1 c with
    | Some(r1, c1) ->
        match p2 c1 with
        | Some(r2, c2) -> Some((r1, r2), c2)
        | _ -> None
    | _ -> None)

  let unit v = P (fun c -> Some(v, c))

  let sat f = P (function
    | c::cs when f c -> Some(c, cs)
    | _ -> None) 

  let parse (str:string) (P p) = 
    match p (List.ofSeq str) with
    | Some(res, []) -> Some res
    | _ -> None

  let map f (P p) = P (fun c -> 
    p c |> Option.map (fun (r, c) -> f r, c))

  let rec zeroOrMore p = P (fun c -> 
    let (P f) = oneOrMore p <|> unit [] 
    f c)

  and oneOrMore p = 
    p <*> zeroOrMore p |> map (fun (f, r) -> f::r)


// ----------------------------------------------------------------------------
// Silly bonus problem
// ----------------------------------------------------------------------------

open System
open Parsec

// This is a silly bonus problem. If you want to write something more fancy,
// you obviously need parser combinators. This is using a minimal demo 
// implementation in 'parsec.fs'. You can use it to implement simple evaluator
// that can deal with binary operations (woohoo!)

let eval str = 
  let num = 
    Parsec.oneOrMore (Parsec.sat (fun c -> c >= '0' && c <= '9')) 
    |> Parsec.map (fun n -> int (String(Array.ofList n)))
  let white =
    Parsec.zeroOrMore (Parsec.sat Char.IsWhiteSpace)
  let op = 
    (Parsec.sat ((=) '+') |> Parsec.map (fun _ -> (+))) <|>
    (Parsec.sat ((=) '*') |> Parsec.map (fun _ -> (*))) <|>
    (Parsec.sat ((=) '/') |> Parsec.map (fun _ -> (/))) <|>
    (Parsec.sat ((=) '-') |> Parsec.map (fun _ -> (-))) 
  
  let expr = 
    ( num <*> white <*> op <*> white <*> num )
    |> Parsec.map (fun ((((n1, _), op), _), n2) -> op n1 n2)

  match Parsec.parse str expr with
  | Some res -> sprintf "The result is: %d" res 
  | _ -> "Sorry, this is too hard for me!"

  