let SHOW x = printf "%A\n" x
// ### Exercise 2.1
// ## Implement `parseNumber` function.
// ## You might find following functions useful:
// ## `ToCharArray()` (String member), `Array.forall`, `System.Char.IsDigit`, `System.Int32.Parse`.

// #### --------------- Your code goes below ---------------
let parseNumber (value: string): int option = 
    let is_number = value.ToCharArray()
                    |> Array.forall System.Char.IsDigit
    if is_number 
        then Some (System.Int32.Parse value)
    else 
        None


let ``exercise 2.1`` = parseNumber "42"

// #### Value of ``exercise 2.1``

SHOW ``exercise 2.1``

// ### Exercise 2.2
// ## Declare `splitBy` function - a wrapper function arround `Split` method from `String` object.
// ## Hints: Use `Split` method from `String` and `Array.toList` function to convert array to list type.
// #### --------------- Your code goes below ---------------
let splitBy (separator: char) (str: string): string list = 
    str.Split(separator)
    |> Array.toList

let ``exercise 2.2`` = "1,3,5,8,10" |> splitBy ','

// #### Value of ``exercise 2.2``
SHOW ``exercise 2.2``

// ### Exercise 3.1
// ##Define `Operator` and `Symbol` Discriminated Union Types.
// ##
// ##`Symbol` should use `Operator` as field in one case
// ##

// #### --------------- Your code goes below ---------------
// `Int` is used here only so that the code compiles.
// Remove it and instead define proper Discriminated Union cases:
// Operator might be one of the following: Plus, Minus, Multiply or Divide
type Operator = 
| Plus
| Minus
| Multiply
| Divide
| LeftBracket
| RightBracket

// Same as above:
// Symbol might be either a NumSymbol (with int) or OpSymbol (with Operator)

type Symbol = 
| NumSymbol of int
| OpSymbol of Operator

// ### Exercise 3.2
// # With help of pattern matching, implement `apply` function.

// #### --------------- Your code goes below ---------------
let apply (operator: Operator) (left: int) (right: int): int = 
    match operator with
    | Plus -> left + right
    | Minus -> left - right
    | Multiply -> left * right
    | Divide -> left / right

// test the function, e.g. `apply Divide 15 4`
let ``exercise 3.2`` = apply Divide 4 2

// #### Value of ``exercise 3.2``
SHOW ``exercise 3.2``

// ### Exercise 3.3
// ##Implement `parseSymbol` - try parse all operators first, and then in nested `match` expression use `parseNumber` function

// #### --------------- Your code goes below ---------------
let parseSymbol (token: string): Symbol option =
    match token with
    | "+" -> Some (OpSymbol Plus)
    | "-" -> Some (OpSymbol Minus)
    | "/" -> Some (OpSymbol Divide)
    | "*" -> Some (OpSymbol Multiply)
    | "(" -> Some (OpSymbol LeftBracket)
    | ")" -> Some (OpSymbol RightBracket)
    | x -> 
            match (parseNumber x).IsSome with
            | true -> Some (NumSymbol (parseNumber x).Value)
            | false -> None

let ``exercise 3.3`` =
    List.map parseSymbol [ "+"; "/"; "12"; "uups" ]
// #### Value of ``exercise 3.3``
SHOW ``exercise 3.3``

// ### Helper function "sequenceOpts"
// ##if all elements are Some values, return Some of those values
// ##otherwise if there's at least one None, return None

let rec sequenceOpts (optionals: 'a option list): 'a list option =
    match optionals with
    | [] -> Some []
    | None :: _ -> None
    | Some h :: t -> sequenceOpts t |> Option.map (fun t -> h :: t)

// ### Exercise 3.4
// ##Implement `parseSymbols`. Useful functions: `List.map`, `sequenceOpts` as well as `splitBy` and `parseSymbol`

// #### --------------- Your code goes below ---------------
let parseSymbols (expression: string): Symbol list option = 
    expression
    |> splitBy ' '
    |> List.map parseSymbol
    |> sequenceOpts 

let ``exercise 3.4`` = "1 2 / +" |> parseSymbols

// #### Value of ``exercise 3.4``
SHOW ``exercise 3.4``

// ### Homework 4.1
// ##Implement `computeonp` function (AiSD or [Wiki](https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)). Hint: `::` is "right-associative"

// #### --------------- Your code goes below ---------------
let rec computeonp (stack: int list) (symbols: Symbol list): int option = 
    match symbols with 
    | [] -> Some stack.Head
    | NumSymbol h :: tail -> computeonp (h :: stack) tail
    | OpSymbol h :: tail -> computeonp (apply h (stack.Tail).Head stack.Head :: stack.Tail.Tail) tail
// test the function, e.g. `computeonp [] [NumSymbol 4; NumSymbol 2; OpSymbol Multiply]`
// Important!!!! Replace the None with commented out assignment o computeonp
let ``exercise 4.1``: int option = computeonp [] [NumSymbol 4; NumSymbol 2; OpSymbol Minus]

// #### Value of ``exercise 4.1``
SHOW ``exercise 4.1``

// ### Homework 4.2
// ##Using `parseSymbols` and `compute`, write `onp` function

// #### --------------- Your code goes below ---------------
let onp (expression: string): int option = 
    let symbols = expression |> parseSymbols
    match symbols with
    | None -> None
    | Some x -> computeonp [] x


let ``exercise 4.2`` = onp "2 7 + 3 / 14 3 - 4 * + 3 +"

// #### Value of ``exercise 4.2``
SHOW ``exercise 4.2``

// ### Homework 4.3
// ##Implement `conv2onp` function (AiSD or (https://pl.wikipedia.org/wiki/Odwrotna_notacja_polska)).

// #### --------------- Your code goes below ---------------
let conv2onp (expression: string): Symbol list option = 
    let symbols = expression |> parseSymbols
    let rec findLeftBracket (res:Symbol list) (stack: Symbol list) = 
        match stack with
        | [] -> [|res; stack|]
        | h :: t when h = OpSymbol LeftBracket -> [|res; t|]
        | h :: t -> findLeftBracket (res @ [h]) t

    let rec conv (x: Symbol list) (result: Symbol list) (stack: Symbol list): Symbol list = 
        match x with 
        | [] -> result @ stack
        | NumSymbol h :: tail -> conv tail (result @ [NumSymbol h]) stack
        | OpSymbol h :: tail -> 
                                match h with 
                                | Plus | Minus when stack.Length > 0 && 
                                    (stack.Head = OpSymbol Multiply  || stack.Head = OpSymbol Divide) ->  conv tail (result @ [stack.Head]) (OpSymbol h:: stack.Tail)
                                | Plus | Minus | LeftBracket ->  conv tail result (OpSymbol h :: stack)
                                | Multiply | Divide when stack.Length = 0 || (stack.Head <> OpSymbol Multiply && stack.Head <> OpSymbol Divide) -> conv tail result (OpSymbol h :: stack)
                                | Multiply | Divide  -> conv tail (result @ [stack.Head]) (OpSymbol h :: stack.Tail)
                                | RightBracket -> 
                                                let operands = findLeftBracket result stack
                                                conv tail operands.[0] operands.[1]
                                                            
    match symbols with 
    | None -> None
    | Some x -> Some (conv x [] [])

let ``exercise 4.3`` =
    conv2onp "( 2 + 5 ) * 3 - 4 * ( 16 + 5 )"

// #### Value of ``exercise 4.3``

SHOW ``exercise 4.3``

// ### Homework 4.4

// #### --------------- Your code goes below ---------------
let compute (expression: string): int option = 
    let onp_expression = expression |> conv2onp
    match onp_expression.IsSome with
    | false -> None
    | true -> computeonp [] onp_expression.Value

let ``exercise 4.4`` =
    compute "( 2 + 5 ) * 3 - 4 * ( ( 16 - 1 ) * 2 + 5 )"

// #### Value of ``exercise 4.4``
SHOW ``exercise 4.4``

// __________________________________________________________________________________________________________________________________________
// Robert Komar 173237 - implementacja obliczen z wykorzystaniem odwrotnej polskiej notacji oraz konwersji z postaci infiksowej na postac postfiksowa
// W tym ćwiczeniu dosyć dokładnie przećwiczony został pattern matching i rekurencja :D