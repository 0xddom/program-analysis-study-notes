(*
This OCaml script was exported from a Jupyter notebook
using an open-source software (under the MIT License) written by @Naereen
from https://github.com/Naereen/Jupyter-Notebook-OCaml
This software is still in development, please notify me of a bug at
https://github.com/Naereen/Jupyter-Notebook-OCaml/issues/new if you find one
*)

(* # The While language

The While language is a procedural language that is commonly used for learning program analysis. Is the barebones of an imperative language that can be extended for reasoning about other properties of programs.

The abstract syntax of the While language is as follows *)

(* In[1]: *)


type var = string
type label = int

type expr = Var of var 
          | Const of int
          | Op of string * expr * expr
type bool_expr = True | False
               | Not of bool_expr
               | BoolOp of string * bool_expr * bool_expr
               | RelOp of string * expr * expr
type stmt = Skip of label
          | Assign of var * expr * label
          | Seq of stmt * stmt
          | If of bool_expr * label * stmt * stmt
          | While of bool_expr * label * stmt;;

(* We can define an example program with this syntax:

```python
y = x          # 1
z = 1          # 2
while y > 1:   # 3
    z = z * y  # 4
    y = y - 1  # 5
y = 0          # 6
```

Each number represent the label of that part of the program. In OCaml syntax this program will look like this *)

(* In[25]: *)


module Examples = struct
    let factorial = Seq (
                            Assign ("y", (Var "x"), 1),
                            Seq (
                                    Assign ("z", (Const 1), 2),
                                    Seq (
                                            While (
                                                    RelOp (">", (Var "y"), (Const 1)), 
                                                    3, 
                                                    Seq (
                                                            Assign ("z", Op ("*", (Var "z"), (Var "y")), 4),
                                                            Assign ("y", Op ("-", (Var "y"), (Const 1)), 5)
                                                        )
                                                  ),
                                            Assign ("y", (Const 0), 6)
                                        )
                                )
                        )
end;;

(* For the analysis of the programs we will need mostly three things from the program:

- The variables in the program
- The labels defined in the program
- The call graph

We can write some utility functions that will give us this information *)

(* In[52]: *)


module VarsSet = Set.Make(String)

let rec vars_of_expr = function
| Var v -> VarsSet.singleton v
| Const _ -> VarsSet.empty
| Op (_, e1, e2) -> VarsSet.union (vars_of_expr e1) (vars_of_expr e2)

let rec vars_of_bool_expr = function
| True | False -> VarsSet.empty
| Not b -> vars_of_bool_expr b
| BoolOp (_, b1, b2) -> VarsSet.union (vars_of_bool_expr b1) (vars_of_bool_expr b2)
| RelOp (_, e1, e2) -> VarsSet.union (vars_of_expr e1) (vars_of_expr e2)

let rec vars_of_stmt = function
| Skip _ -> VarsSet.empty
| Assign (v, e, _) -> VarsSet.union (VarsSet.singleton v) (vars_of_expr e)
| Seq (s1, s2) -> VarsSet.union (vars_of_stmt s1) (vars_of_stmt s2)
| If (b, _, s1, s2) -> VarsSet.union (VarsSet.union (vars_of_bool_expr b) (vars_of_stmt s1)) (vars_of_stmt s2)
| While (b, _, s) -> VarsSet.union (vars_of_bool_expr b) (vars_of_stmt s)

module LabelsSet = Set.Make(struct 
   type t = int 
   let compare = compare
end)
 
let rec labels_of_stmt = function
| Skip l -> LabelsSet.singleton l
| Assign (_, _, l) -> LabelsSet.singleton l
| Seq (s1, s2) -> LabelsSet.union (labels_of_stmt s1) (labels_of_stmt s2)
| If (_, l, s1, s2) -> LabelsSet.union (LabelsSet.union (LabelsSet.singleton l) (labels_of_stmt s1)) (labels_of_stmt s2)
| While (_, l, s) -> LabelsSet.union (LabelsSet.singleton l) (labels_of_stmt s)

type graph_node = Begin | End
                | Assign of var * expr * label
                | BoolExpr of bool_expr * label
type graph_edge = graph_node * graph_node

module GraphNodesSet = Set.Make(struct
    type t = graph_node
    let compare = compare
end)

module GraphEdgesSet = Set.Make(struct
    type t = graph_edge
    let compare = compare
end)

type cfg = GraphNodesSet.t * GraphEdgesSet.t * graph_node

let empty_cfg = (GraphNodesSet.of_list [Begin; End]), (GraphEdgesSet.singleton (Begin, End)), Begin

let build_cfg cfg stmt = "TODO"

type program_info = VarsSet.t * LabelsSet.t * cfg;;

(* In[ ]: *)


;;
