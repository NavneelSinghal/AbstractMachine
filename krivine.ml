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

(* type for answers and environments *)

type environment = (string * closure) list
    and closure = Closure of exp * environment;;
type stack = closure list;;

(* exceptions *)

exception Not_found;;
exception Bad_program;;

(* lookup function for variables *)

let rec lookup x env = 
    match env with 
    | (e, c) :: env' -> 
            if e = x then c
            else lookup x env'
    | _ -> raise Not_found
;;

(* eval function *)

let rec eval c s =

    match c with
    
    (* variable lookup *)
    | Closure (Var v, env) -> eval (lookup v env) s
    
    (* function abstraction *)
    | Closure (Abst(x, e), env) -> 
            begin
            match s with 
            | [] -> raise Bad_program
            | t :: s' -> eval (Closure(e, (x, t) :: env)) s'
            end

    (* function application *)
    | (Closure (Appl(e1, e2), env)) -> eval (Closure(e1, env)) ((Closure(e2, env)) :: s)
    
    (* boolean value *)
    | (Closure (Bool b, env)) -> c
    
    (* conditional *)
    | (Closure (If(e1, e2, e3), env)) ->
            begin
            match (eval (Closure(e1, env)) []) with
            | Closure (Bool b, env) -> if b then (eval (Closure(e2, env)) s) else (eval (Closure(e3, env)) s)
            | _ -> raise Bad_program
            end
    
    (* integer value *)
    | (Closure (Int a, env)) -> c

    (* add *)
    | (Closure (Add(e1, e2), env)) ->
            begin
            match (eval (Closure(e1, env)) []), (eval (Closure(e2, env)) []) with
            | Closure (Int a, env1), Closure (Int b, env2) -> Closure (Int (a + b), [])
            | _ -> raise Bad_program
            end

    (* multiply *)
    | (Closure (Mul(e1, e2), env)) ->
            begin
            match (eval (Closure(e1, env)) []), (eval (Closure(e2, env)) []) with
            | Closure (Int a, env1), Closure (Int b, env2) -> Closure (Int (a * b), [])
            | _ -> raise Bad_program
            end

    (* >= 0 *)
    | (Closure (Ge e, env)) ->
            begin
            match (eval (Closure(e, env)) []) with
            | Closure (Int a, env1) -> Closure (Bool (a >= 0), [])
            | _ -> raise Bad_program
            end

    (* > 0 *)
    | (Closure (Gt e, env)) ->
            begin
            match (eval (Closure(e, env)) []) with
            | Closure (Int a, env1) -> Closure (Bool (a > 0), [])
            | _ -> raise Bad_program
            end

    (* <= 0 *)
    | (Closure (Le e, env)) ->
            begin
            match (eval (Closure(e, env)) []) with
            | Closure (Int a, env1) -> Closure (Bool (a <= 0), [])
            | _ -> raise Bad_program
            end

    (* < 0 *)
    | (Closure (Lt e, env)) ->
            begin
            match (eval (Closure(e, env)) []) with
            | Closure (Int a, env1) -> Closure (Bool (a < 0), [])
            | _ -> raise Bad_program
            end

    (* = 0 *)
    | (Closure (Eq e, env)) ->
            begin
            match (eval (Closure(e, env)) []) with
            | Closure (Int a, env1) -> Closure (Bool (a = 0), [])
            | _ -> raise Bad_program
            end

    (* != 0 *)
    | (Closure (Ne e, env)) ->
            begin
            match (eval (Closure(e, env)) []) with
            | Closure (Int a, env1) -> Closure (Bool (a <> 0), [])
            | _ -> raise Bad_program
            end
;;

let run p env = eval (Closure(p, env)) [];;

let cur_env = [("x", Closure (Int 3, [])); ("y", Closure (Int 5, [])); ("z", Closure (Bool true, []))];;

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


