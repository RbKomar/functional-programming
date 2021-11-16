let SHOW x = printf "%A\n" x

type Tree =
    | Empty
    | Node of int * Tree * Tree

let tree =
    Node
        (8,
         Node(3, Node(1, Empty, Empty), Node(6, Node(4, Empty, Empty), Node(7, Empty, Empty))),
         Node(10, Empty, Node(14, Node(13, Empty, Empty), Empty)))

//#### --------------- Your code goes below --------------- *)
let rec sumLeaves (tree: Tree): int = 
    match tree with
    | Empty -> 0
    | Node (leaf_val, Empty, Empty) -> leaf_val
    | Node (leaf_val, left, right) -> sumLeaves left + sumLeaves right

let ``exercise 1.1`` = sumLeaves tree
(** #### Value of ``exercise 1.1`` *)
SHOW ``exercise 1.1``

//### Exercise 1.2
//##Collect **all values** from tree into a list in-order
let rec collectInOrder (tree: Tree): int list = 
    match tree with
    | Empty -> []
    | Node (v, Empty, Empty) -> [v]
    | Node (v, left, right) -> collectInOrder left @ [v] @ collectInOrder right

let ``exercise 1.2`` = collectInOrder tree
(** #### Value of ``exercise 1.2`` *)
SHOW ``exercise 1.2``


//### Exercise 1.3
//##Check if tree is sorted
let rec isSortedList (l : list<int>) (is_sorted: bool): bool =
    match l with
    | [] | [_] -> true
    | head::(x::y as tail) -> isSortedList tail (head <= x)

let isSorted (tree: Tree): bool = 
    let l = collectInOrder tree
    isSortedList l true
    
let ``exercise 1.3`` = isSorted tree
(** #### Value of ``exercise 1.3`` *)
SHOW ``exercise 1.3``

//### Exercise 1.4
//##Insert element into Binary Search Tree
let rec insertBST (value: int) (tree: Tree): Tree = 
    match tree with
    | Empty ->  Node(value, Empty, Empty)
    | Node(v, left, right) when v < value -> Node(v, left, insertBST value right)
    | Node(v, left, right) when v > value -> Node(v, insertBST value left, right)
    | Node(v, left, right) -> Node(v, left, right)

let ``exercise 1.4`` = insertBST 5 tree |> collectInOrder
//** #### Value of ``exercise 1.4`` *)
SHOW ``exercise 1.4``