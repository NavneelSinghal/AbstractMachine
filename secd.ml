(* type for expressions *)

type exp =
    | Var of string                             (* variable *)
    | Abst of string * exp                      (* function abstraction *)
    | Appl of exp * exp                         (* function application *)
    | Bool of bool                              (* boolean values *)
    | If of exp * exp * exp                     (* if then else *)
    | Int of int                                (* integer values *)
    | Add of exp * exp                          (* add *)
    | Mul of exp * exp                          (* multiply *)
    | Ge of exp                                 (* >= 0 *)
    | Gt of exp                                 (* > 0 *)
    | Le of exp                                 (* <= 0 *)
    | Lt of exp                                 (* < 0 *)
    | Eq of exp                                 (* = 0 *)
    | Ne of exp                                 (* != 0 *)
;;

(* type for opcodes *)

type opcode = 
    | LOOKUP of string                          (* find variable from environment *)
    | LDFUNC of string * (opcode list)          (* load function *)
    | RETURN                                    (* return *)
    | CALL                                      (* call function *)
    | LDBOOL of bool                            (* load bool *)
    | LDINT of int                              (* load integer *)
    | ADD                                       (* add *)
    | MUL                                       (* multiply *)
    | GE                                        (* >= 0 *)
    | GT                                        (* > 0 *)
    | LE                                        (* <= 0 *)
    | LT                                        (* < 0 *)
    | EQ                                        (* = 0 *)
    | NE                                        (* != 0 *)
    | COND of (opcode list) * (opcode list)     (* conditional *)
;;

(* types for secd machine *)

type codes = opcode list;;
type environment = (string * answer) list
    and answer = 
    | Integer of int
    | Boolean of bool
    | VariableClosure of environment * string * codes;;
type stack = answer list;;
type dump = (stack * environment * codes) list;;

(* exceptions *)

exception Not_found;;
exception Bad_program;;

(* looking up a variable in an environment *)

let rec lookup x env = 
    match env with 
    | [] -> raise Not_found
    | (var, value) :: envs ->
            if x = var
            then value
            else lookup x envs
;;

(* compile function *)

let rec compile e = 
    match e with
    
    (* variable lookup *)
    | Var v -> [LOOKUP(v)]

    (* function abstraction *)
    | Abst (x, e') -> [LDFUNC(x, (compile e') @ [RETURN])]
    
    (* function application *)
    | Appl (e1, e2) -> (compile e1) @ (compile e2) @ [CALL]
    
    (* load boolean *)
    | Bool b -> [LDBOOL(b)]
    
    (* conditional *)
    | If (e1, e2, e3) -> (compile e1) @ [COND(compile e2, compile e3)]
    
    (* load integer *)
    | Int i -> [LDINT(i)]

    (* addition *)
    | Add (e1, e2) -> (compile e1) @ (compile e2) @ [ADD]
    
    (* multiplication *)
    | Mul (e1, e2) -> (compile e1) @ (compile e2) @ [MUL]
    
    (* >= 0 *)
    | Ge e' -> (compile e') @ [GE]
    
    (* > 0 *)
    | Gt e' -> (compile e') @ [GT]
    
    (* <= 0 *)
    | Le e' -> (compile e') @ [LE]
    
    (* < 0 *)
    | Lt e' -> (compile e') @ [LT]
    
    (* = 0 *)
    | Eq e' -> (compile e') @ [EQ]
    
    (* != 0 *)
    | Ne e' -> (compile e') @ [NE]
;;

(* stack machine *)

let rec stkmc s e c d =

    match (s, e, c, d) with
    
    (* empty opcode list *)
    | (x :: _, _, [], _) -> x  
    
    (* variable lookup *)
    | (_, _, LOOKUP(x) :: c', _) -> stkmc ((lookup x e) :: s) e c' d

    (* closure *)
    | (_, _, LDFUNC(x, c_) :: c', _) -> stkmc (VariableClosure(e, x, c_) :: s) e c' d

    (* return *)
    | (x :: s', _, RETURN :: c', (s_, e_, c_) :: d') -> stkmc (x :: s_) e_ c_ d'

    (* call *)
    | (x :: VariableClosure(e_, x_, c_) :: s', _, CALL :: c', _) -> stkmc [] ((x_, x) :: e_) c_ ((s', e, c') :: d)

    (* load boolean *)
    | (_, _, LDBOOL(b) :: c', _) -> stkmc (Boolean(b) :: s) e c' d

    (* load integer *)
    | (_, _, LDINT(a) :: c', _) -> stkmc (Integer(a) :: s) e c' d

    (* add *)
    | (Integer(b) :: Integer(a) :: s', _, ADD :: c', _) -> stkmc (Integer(a + b) :: s') e c' d

    (* mul *)
    | (Integer(b) :: Integer(a) :: s', _, MUL :: c', _) -> stkmc (Integer(a * b) :: s') e c' d

    (* >= 0 *)
    | (Integer(a) :: s', _, GE :: c', _) -> stkmc (Boolean(a >= 0) :: s') e c' d

    (* > 0 *)
    | (Integer(a) :: s', _, GT :: c', _) -> stkmc (Boolean(a > 0) :: s') e c' d
    
    (* <= 0 *)
    | (Integer(a) :: s', _, LE :: c', _) -> stkmc (Boolean(a <= 0) :: s') e c' d
    
    (* < 0 *)
    | (Integer(a) :: s', _, LT :: c', _) -> stkmc (Boolean(a < 0) :: s') e c' d
    
    (* = 0 *)
    | (Integer(a) :: s', _, EQ :: c', _) -> stkmc (Boolean(a = 0) :: s') e c' d
    
    (* != 0 *)
    | (Integer(a) :: s', _, NE :: c', _) -> stkmc (Boolean(a <> 0) :: s') e c' d

    (* conditional *)
    | (Boolean(b) :: s', _, COND(c1, c2) :: c', _) -> stkmc s' e ((if b then c1 else c2) @ c') d

    (* invalid case *)
    | (_, _, _, _) -> raise Bad_program
;;

(* add test cases *)

let run p env = stkmc [] env (compile p) [];;

let cur_env = [("x", Integer 3); ("y", Integer 5); ("z", Boolean true)];;

let p1 = Add(Int(1), Int(2));;
let p2 = If(Gt(Int(1)), Int(1), Int(2));;
let p3 = If(Gt(Int(0)), Int(1), Int(2));;
let p4 = Appl(Abst("x", Add(Var "x", (Mul(If(Gt(Var "x"), Int 16, Int (-1)), Var "x")))), Int 3);;
let p5 = Appl(Abst("x", Add(Var "x", (Mul(If(Gt(Var "x"), Int 16, Int (-1)), Var "x")))), Int (-3));;
let p6 = Abst("x", Add(Var "x", Int 7));;
let p7 = Appl(Abst("x", Add(Var "x", Int 4)), Appl(Abst("x", Add(Var "x", Int 4)), Appl(Abst("x", Add(Var "x", Int 4)), Appl(Abst("x", Add(Var "x", Int 4)), Appl(Abst("x", Add(Var "x", Int 4)), Int 1)))));;

run p1 cur_env;;
run p2 cur_env;;
run p3 cur_env;;
run p4 cur_env;;
run p5 cur_env;;
run p7 cur_env;;
run p6 cur_env;;


