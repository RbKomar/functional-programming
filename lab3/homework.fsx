let SHOW x = printf "%A\n" x
//### Exercise 3.1
//##Implement `parseScore`.

//#### --------------- Your code goes below --------------- *)
let (|Digit|_|) char =
    let zero = System.Convert.ToInt32 '0'
    if System.Char.IsDigit char then Some(System.Convert.ToInt32 char - zero) else None

type frame = 
    | STRIKE of int
    | SPARE of int
    | POINTS of int * int

let rec parseScoreToFrame (chars: char list): frame option list = 
    match chars with
    | [] -> []
    | x :: tail when x= 'X' -> [Some(STRIKE 10)] @ parseScoreToFrame tail
    | '-' :: Digit y :: tail  -> [Some(POINTS (0, y))] @ parseScoreToFrame tail
    | Digit x :: '/' :: tail -> [Some(SPARE(x))] @ parseScoreToFrame tail
    | Digit x :: Digit y :: tail -> [Some(POINTS (x,y) )] @ parseScoreToFrame tail
    | Digit x :: '-' :: tail -> [Some(POINTS (x, 0) )] @ parseScoreToFrame tail
    | _ :: tail-> [None] @ parseScoreToFrame tail
    
let rec parseScore (chars: char list): int option list = 
    match chars with
    | [] -> []
    | x :: tail when x= 'X' -> [Some(10)] @ parseScore tail
    | '-' :: '/' ::tail  -> [Some(0)] @ [Some(10)] @ parseScore tail
    | '-' :: tail  -> [Some(0)] @ parseScore tail
    | Digit x :: '/' :: tail -> [Some(x)] @ [Some(10-x)] @ parseScore tail
    | Digit x :: tail -> [Some(x)] @ parseScore tail
    | _ :: tail-> [None] @ parseScore tail

let ``exercise 3.1`` =
    parseScore [ 'X'
                 '4'
                 '/'
                 '2'
                 '-'
                 'N' ]
//** #### Value of ``exercise 3.1`` *)
SHOW ``exercise 3.1``

//### Exercise 3.2
//##Implement `countScore`

//#### --------------- Your code goes below --------------- *)
let rec countScore (scores: int list): int = 
  match scores with
  | [] -> 0
  | 10 :: x :: y :: tail when List.length tail = 0 -> 10 + x + y
  | 10 :: (x :: y :: _ as tail) -> 10 + x + y + countScore tail
  | x :: y :: z ::  tail when x + y = 10 && List.length tail = 0 -> x+y+z 
  | x :: y :: (z :: _ as tail) when x + y = 10 && x <> 10 && y <> 10 -> x +  y + z + countScore tail
  | x :: tail -> x + countScore tail

let ``exercise 3.2`` =
    [ [ 10
        10
        10
        10
        10
        10
        10
        10
        10
        10
        10
        10 ]
      [ 9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0 ]
      [ 5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5 ]
      [ 10
        9
        1
        5
        5
        7
        2
        10
        10
        10
        9
        0
        8
        2
        9
        1
        10 ] ]
    |> List.map countScore
//** #### Value of ``exercise 3.2`` *)
SHOW ``exercise 3.2``

//### sequenceOpts function *)
let sequenceOpts (optionals: 'a option list): 'a list option =
    let rec sequence' acc optionals =
        match optionals, acc with
        | [], _ -> Option.map List.rev acc
        | Some h :: t, Some acc -> sequence' (Some(h :: acc)) t
        | _ -> None

    sequence' (Some []) optionals


//### Homework 1
//##Implement `bowlingScore`.

//###Hint: Use `sequenceOpts` to convert from list of options to option of list
let GetValue (o: int list option ): int list =
  if o.IsSome then o.Value
  else [-1]


let bowlingScore (score: string): int option = 
  let parsedScore = parseScore (Seq.toList score)
  let flag_calculate = parsedScore |> List.forall(fun x -> x.IsSome) 
  if flag_calculate then 
    parsedScore 
    |> sequenceOpts
    |> GetValue 
    |> countScore
    |> Some
  else 
    None


let ``homework 1`` =
    [ "XXXXXXXXXXXX"
      "9-9-9-9-9-9-9-9-9-9-"
      "9--/9-9-9-9-9-9-9-9-"
      "X-/9-9-9-9-9-9-9-9-"
      "9-X9-9-X--9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-XXX"
      "5/5/5/5/5/5/5/5/5/5/5"
      "5/5/5/5/5/5/5/5/5/5/X"
      "X9/5/72XXX9-8/9/X" ]
    |> List.map bowlingScore

//** #### Value of ``homework 1`` *)
SHOW ``homework 1``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]


//### Homework 2
//###Write new, **tail-recursive** versions of `parseScore` and `countScore`.
//###Implement `bowlingScoreTail` to use those 2 new functions

let rec parseScoreTail (chars: char list) (acc: int option list): int option list = 
   match chars with
    | [] -> acc
    | x :: tail when x= 'X' ->  parseScoreTail tail (acc @ [Some(10)])
    | '-' :: '/' ::tail  ->  parseScoreTail tail (acc @ [Some(0)] @ [Some(10)] )
    | '-' :: tail  -> parseScoreTail tail (acc @ [Some(0)])
    | Digit x :: '/' :: tail ->  parseScoreTail tail (acc @ [Some(x)] @ [Some(10-x)])
    | Digit x :: tail ->  parseScoreTail tail (acc @ [Some(x)])
    | _ :: tail->  parseScoreTail tail (acc @ [None])

let rec countScoreTail (scores: int list) (acc: int): int = 
 match scores with
  | [] -> acc
  | 10 :: x :: y :: tail when List.length tail = 0 -> acc + 10 + x + y
  | 10 :: (x :: y :: _ as tail) ->  countScoreTail tail acc + 10 + x + y
  | x :: y :: z ::  tail when x + y = 10 && List.length tail = 0 -> acc + x+y+z 
  | x :: y :: (z :: _ as tail) when x + y = 10 && x <> 10 && y <> 10 -> countScoreTail tail acc + x +  y + z 
  | x :: tail -> countScoreTail tail acc + x

let countScoreTail0acc (scores: int list) = countScoreTail scores 0

let bowlingScoreTail (score: string): int option = 
  let parsedScoreByTailRecursion = parseScoreTail (Seq.toList score) []
  let flag_calculate = parsedScoreByTailRecursion |> List.forall(fun x -> x.IsSome) 
  if flag_calculate then 
    parsedScoreByTailRecursion 
    |> sequenceOpts
    |> GetValue 
    |> countScoreTail0acc 
    |> Some
  else 
    SHOW parsedScoreByTailRecursion
    None
 


let ``homework 2`` =     
    [ "XXXXXXXXXXXX"
      "9-9-9-9-9-9-9-9-9-9-"
      "9--/9-9-9-9-9-9-9-9-"
      "X-/9-9-9-9-9-9-9-9-"
      "9-X9-9-X--9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-XXX"
      "5/5/5/5/5/5/5/5/5/5/5"
      "5/5/5/5/5/5/5/5/5/5/X"
      "X9/5/72XXX9-8/9/X" ]
    |> List.map bowlingScoreTail 
//** #### Value of ``homework 2`` *)
SHOW ``homework 2``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]

//////////////////////////////////////////////////////////////
/// Indeks: 
/// Imię:  
/// Nazwisko: 
/// 
/// Podsumowanie zalizowanych zadan: