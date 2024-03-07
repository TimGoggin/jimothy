(* COMP 360H Project 1:  an interpreter for an imperative language.
 *
 * N. Danner
 *)

 module E = Ast.Expression
 module S = Ast.Stm
 module P = Ast.Program
 
 (* 'a IdentMap.t:  the type of maps from identifiers to 'a.
  *)
 module IdentMap = Map.Make(Ast.Id)
 
 (* MultipleDeclaration x is raised when x is declared more than once in a
  * block.
  *)
 exception MultipleDeclaration of Ast.Id.t
 
 (* UnboundVariable x is raised when x is used but not declared.
  *)
 exception UnboundVariable of Ast.Id.t
 
 (* UndefinedFunction f is raised when f is called but has not been defined.
  *)
 exception UndefinedFunction of Ast.Id.t
 
 (* TypeError s is raised when an operator or function is applied to operands
  * of the incorrect type.  s is any (hopefuly useful) message.
  *)
 exception TypeError of string
 
 (* Values.
  *)
 module Value = struct
   type t = 
     | V_Undefined
     | V_None
     | V_Int of int
     | V_Bool of bool
     | V_Str of string
     [@@deriving show]
 
   (* to_string v = a string representation of v (more human-readable than
    * `show`.
    *)
   let to_string (v : t) : string =
     match v with
     | V_Undefined -> "?"
     | V_None -> "None"
     | V_Int n -> Int.to_string n
     | V_Bool b -> Bool.to_string b
     | V_Str s -> s
 end
 
 (* An implementation of the I/O API.  This is a little bit complex, because
  * this one implementation allows for a few variations:
  * - The input and output channel can be set by the client (default to
  *   standard input and output).
  * - The display of prompts (for the prompt_* functions) can be turned off
  *   (default on).
  * These variations let us use this module for interactive use (use the
  * defaults) and testing (redirect the i/o channels to a programmatic stream
  * and turn off the display of prompts.
  *
  * A client makes changes to the defaults by setting `in_channel`,
  * `out_channel`, and `show_prompts`.
  *)
 module Api = struct
 
   (* Raised when a function is invoked that is not in the API.
    *)
   exception ApiError of string
 
   (* in_channel:  input channel (for get_*, prompt_* ).
    *)
   let in_channel : Scanf.Scanning.in_channel ref = 
     ref Scanf.Scanning.stdin
 
   (* out_channel:  output channel (for print_*, prompt_* when prompts are
    * displayed).
    *)
   let out_channel : Out_channel.t ref = ref Out_channel.stdout
 
   (* show_prompts:  true to display prompts, false to not display.
    *)
   let show_prompts : bool ref = ref true
 
   (* output oc s:  output `s` to `oc` and flush `oc`.
    *)
   let output (oc : Out_channel.t) (s : string) : unit =
     Out_channel.output_string oc s ;
     Out_channel.flush oc
 
   (* outputnl oc s = output `s ^ '\n'` to `oc` and flush `oc`.
    *)
   let outputnl (oc : Out_channel.t) (s : string) : unit =
     output oc (s ^ "\n")
 
   (* The API definition.  The API is specified by a
    * (string*(Value.t->Value.t)) list.  Each element names an API function
    * and provides the code to be executed when the function is called.
    *)
   let api : (Value.t list -> Value.t) IdentMap.t =
     [
       ("print_bool", fun vs ->
         match vs with
         | [Value.V_Bool n] -> 
           outputnl (!out_channel) (Bool.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_bool"
       )
     ; ("get_bool", fun vs ->
         match vs with
         | [] -> Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for get_bool"
       )
     ; ("prompt_bool", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
         | _ -> raise @@ TypeError "Bad argument type for prompt_bool"
       )
     ; ("print_int", fun vs ->
         match vs with
         | [Value.V_Int n] -> 
           outputnl (!out_channel) (Int.to_string n) ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_int"
       )
     ; ("get_int", fun vs ->
         match vs with
         | [] -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for get_int"
       )
     ; ("prompt_int", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
         | _ -> raise @@ TypeError "Bad argument type for prompt_int"
       )
     ; ("print_str", fun vs ->
          match vs with
          | [Value.V_Str s] -> 
            outputnl (!out_channel) s ; Value.V_None
          | _ -> raise @@ TypeError "Bad argument type for print_s"
       )
     ; ("get_str", fun vs ->
         match vs with
         | [] -> Value.V_Str (Scanf.bscanf !in_channel "%s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for get_str"
       )
     ; ("prompt_str", fun vs ->
         match vs with
         | [Value.V_Str s] ->
           if !show_prompts then output (!out_channel) s else () ;
             Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
         | _ -> raise @@ TypeError "Bad argument type for prompt_str"
       )
     ] |> List.to_seq |> IdentMap.of_seq
 
   (* do_call f vs invokes the API function corresponding to `f` with argument
    * list `vs`.
    *
    * Raises ApiError f: if f is not an API function.
    *)
   let do_call (f : string) (vs : Value.t list) : Value.t =
     try
       IdentMap.find f api vs
     with
     | Not_found -> raise @@ ApiError f
 
 
 end
 
 (* Environments.
  *
  * A value of type t is a map from identifiers to values.  We use σ to range
  * over environments and standard function notation when describing them.
  *)
 module Env = struct
 (* The type of environments.
   *)
   type t = Value.t IdentMap.t
 
   (*  lookup σ x = σ(x).
     *)
   let lookup (sigma : t) (x : Ast.Id.t) : Value.t =
     IdentMap.find x sigma
 
   (*  update σ x v = σ{x → v}.
     *)
   let update (sigma : t) (x : Ast.Id.t) (v : Value.t) : t =
     IdentMap.add x v sigma
 
   (*  empty = σ, where dom σ = ∅.
     *)
   let empty : t = IdentMap.empty
 
 end
 
 (* Frames.
  * 
  * Frames can either be lists of environments, or they can be return frames,
  * the output of functions. We use two constructors here to represent them both.
  *)
 module Frame = struct
 
   type t = 
     | Ret of Value.t
     | E_list of Env.t list
 
     (**
   Both of these functions need to have the type of sigmas changed to E_list but this is proving harder than it should be
  *)
 
   let push (sigma : Env.t) (sigmas : t) : t = 
     match sigmas with
     | E_list sigmas -> E_list (sigma :: sigmas)
     | Ret v -> failwith(Value.to_string v)
 
   let insert (sigmas : t) (id : Ast.Id.t)(v : Value.t) : t  = 
     match sigmas with 
       | E_list [] -> failwith("") 
       | Ret v -> failwith(Value.to_string v)
       | E_list (h :: tail) -> E_list ((Env.update h id v) :: tail)
 
   let pop (sigmas : t) : Env.t list =
     match sigmas with 
     | E_list [] -> failwith("")
     | Ret v -> failwith(Value.to_string v)
     | E_list (_h :: tail) -> tail
 
   let rec lookup (sigmas : t) (id : Ast.Id.t) : Value.t =
     match sigmas with
     | Ret _ -> failwith("")
     | E_list [] -> raise(UnboundVariable id)
     | E_list (h :: tail) -> 
         try Env.lookup h id
         with Not_found -> lookup (E_list tail) id
 
   let update (sigmas : t) (id : Ast.Id.t) (v : Value.t) : t =
     match sigmas with
     | Ret _ -> failwith("")
     | E_list [] -> failwith("")
     | E_list (h :: tail) -> 
       begin match Env.lookup h id with
         | exception Not_found -> E_list ((Env.update h id v) :: tail)
         | _ -> raise (MultipleDeclaration id)
       end
 
   let get_value (sigmas : t) : Value.t =
     match sigmas with
       | Ret v -> v
       | E_list _sigmas -> failwith("no")
 
   let emptyE : t = E_list [ Env.empty ]
   let emptyF : t = Ret Value.V_Undefined
     
 end
 
 (*  binop op v v' = v'', where v'' is the result of applying the semantic
  *  denotation of `op` to `v` and `v''`.
  *)
  let binop (op : E.binop) (v : Value.t) (v' : Value.t) : Value.t =
   match (op, v, v') with
   | (E.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
   | (E.Plus, _, _) -> raise (TypeError "UNEXPECTED TYPE: + takes input of int and int")
 
   | (E.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n - n')
   | (E.Minus, _, _) -> raise (TypeError "UNEXPECTED TYPE: - takes input of int and int")
 
   | (E.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
   | (E.Times, _, _) -> raise (TypeError "UNEXPECTED TYPE: * takes input of int and int")
   
   | (E.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n / n')
   | (E.Div, _, _) -> raise (TypeError "UNEXPECTED TYPE: / takes input of int and int")
 
   | (E.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')
   | (E.Mod, _, _) -> raise (TypeError "UNEXPECTED TYPE: % takes input of int and int")
 
   | (E.And, Value.V_Bool b, V_Bool b') -> Value.V_Bool (b && b')
   | (E.And, _, _) -> raise (TypeError "UNEXPECTED TYPE: && takes input of bool and bool")
 
   | (E.Or, Value.V_Bool b, V_Bool b') -> Value.V_Bool (b || b')
   | (E.Or, _, _) -> raise (TypeError "UNEXPECTED TYPE: || takes input of bool and bool")
   
   | (E.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n == n')
   | (E.Eq, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b == b')
   | (E.Eq, Value.V_Str s, Value.V_Str s') -> Value.V_Bool (s == s')
   | (E.Eq, _, _) -> raise (TypeError "UNEXPECTED TYPE: == takes input of bool and bool, int and int, or str and str ")
 
   | (E.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n != n')
   | (E.Ne, Value.V_Bool b, Value.V_Bool b') -> Value.V_Bool (b != b')
   | (E.Ne, Value.V_Str s, Value.V_Str s') -> Value.V_Bool (s != s')
   | (E.Ne, _, _) -> raise (TypeError "UNEXPECTED TYPE: != takes input of bool and bool, int and int, or str and str ")
 
   | (E.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n < n')
   | (E.Lt, _, _) -> raise (TypeError "UNEXPECTED TYPE: < takes input of int and int ")
 
   | (E.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
   | (E.Le, _, _) -> raise (TypeError "UNEXPECTED TYPE: <= takes input of int and int ")
 
   | (E.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n > n')
   | (E.Gt, _, _) -> raise (TypeError "UNEXPECTED TYPE: > takes input of int and int ")
 
   | (E.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
   | (E.Ge, _, _) -> raise (TypeError "UNEXPECTED TYPE: >= takes input of int and int ")
 
 (*  eval σ e = v, where σ ├ e ↓ v according to our evaluation rules.
  *)
 (*! eval header !*)
 let rec eval (sigmas : Frame.t) (e : E.t) (pgrm : P.t) : (Value.t * Frame.t) =
 (*! end !*)
   match e with
   | E.Var x -> Frame.lookup sigmas x, sigmas
   | E.Num n -> Value.V_Int n, sigmas
   | E.Bool b -> Value.V_Bool b, sigmas
   | E.Str s -> Value.V_Str s, sigmas
   | E.Binop (op, e, e') ->
     let v, sigmas = eval sigmas e pgrm in
     let v', sigmas = eval sigmas e' pgrm in
     binop op v v', sigmas
   | E.Assign (id, e) -> 
     let v, sigmas = eval sigmas e pgrm in 
     v, Frame.update sigmas id v
   | E.Not e -> 
     let v, frame = eval sigmas e pgrm in
     begin match v with 
       | Value.V_Bool true -> Value.V_Bool false, frame
       | Value.V_Bool false -> Value.V_Bool true, frame
       | _ -> raise (TypeError "UNEXPECTED TYPE: ! takes in an expression of type bool")
     end
   | E.Neg e ->
     let n, frame = eval sigmas e pgrm in
     begin match n with 
       | Value.V_Int x -> Value.V_Int (-x), frame
       | _ -> raise (TypeError "UNEXPECTED TYPE: ~ takes in an expression of type int")
     end
   | E.Call (x, pl) -> 
     try let P.FunDef (id, p, sl) = funLookup pgrm x in
       let newEnv, newFrame = functionEnvironmentMaker (P.FunDef (id, p, sl)) Env.empty pl sigmas pgrm in 
         (Frame.get_value (statement (Frame.push newEnv sigmas) (S.Block sl) pgrm)), newFrame
     with UndefinedFunction _ -> let vl, f = evalList sigmas pl pgrm [] in Api.do_call x vl, f
 (*! eval let !*)
 
and evalList (sigmas : Frame.t) (e : E.t list) (pgrm : P.t) (vl : Value.t list) : Value.t list * Frame.t =
  match e with 
  | [] -> failwith("")
  | h :: [] -> let v, f = eval sigmas h pgrm in v :: vl, f
  | h :: tail -> let v, f = eval sigmas h pgrm in let vl', f' = (evalList f tail pgrm (v :: vl)) in vl', f'

 and statement (sigmas : Frame.t) (s : S.t) (pgrm : P.t) : Frame.t =
   match s with 
   | S.Skip -> sigmas
   | S.VarDec [] -> failwith("ERROR: this should never happen")
   | S.VarDec (h :: []) -> 
     begin match h with
       | id, Some e -> 
         let v, f = eval sigmas e pgrm in
         Frame.insert f id v
       | id, None -> Frame.insert sigmas id Value.V_Undefined
     end
   | S.VarDec (h :: tail) -> 
     begin match h with
       | id, Some e -> 
         let v, f = eval sigmas e pgrm in
         statement (Frame.insert f id v) (S.VarDec tail) pgrm
       | id, None -> statement (Frame.insert sigmas id Value.V_Undefined) (S.VarDec tail) pgrm
     end
   | S.Expr e -> 
     let _v, sigmas = eval sigmas e pgrm in sigmas
   | S.Block [] -> failwith("ERROR: Empty block -- did you mean to use a skip statement?")
   | S.Block (h :: []) -> statement sigmas h pgrm
   | S.Block (h :: tail) -> statement (statement sigmas h pgrm) (S.Block tail) pgrm
   | S.If (e, s, s') -> 
     let v, frame = eval sigmas e pgrm in
       begin match v with
         | Value.V_Bool true -> statement frame s pgrm
         | Value.V_Bool false -> statement frame s' pgrm
         | _ -> failwith("ERROR: If expects an input of type bool")
       end
   | S.While (_e, _s) -> failwith("Unimplemented")
   | S.Return Some e -> 
     let v, _frame = eval sigmas e pgrm in
       Frame.Ret v
   | S.Return _ -> 
       Frame.Ret Value.V_None
 
 
 and funLookup (pgrm : P.t) (id : Ast.Id.t) : P.fundef  =
   match pgrm with 
     | Pgm [] -> raise(UndefinedFunction id)
     | Pgm (P.FunDef (name,l,sl) :: tail) -> if name = id then P.FunDef (name,l,sl) else funLookup(P.Pgm tail) (id)
 
 and functionEnvironmentMaker (func : P.fundef) (sigma : Env.t) (paramValues : E.t list) (sigmas : Frame.t) (pgrm : P.t) : Env.t * Frame.t =
   match func with 
   | (P.FunDef (_, [], _)) -> sigma, sigmas
   | (P.FunDef (id, h :: t, sl)) -> begin
     match paramValues with 
     | [] -> failwith("ERROR: Function " ^ id ^ " takes in more arguments than you put in!")
     | head :: tail -> 
       let v, newFrame = eval sigmas head pgrm in 
         functionEnvironmentMaker (P.FunDef (id, t, sl)) (Env.update sigma h v) tail newFrame pgrm
   end
 
 (*  eval e = v, where _ ├ e ↓ v.
  *
  *  Because later declarations shadow earlier ones, this is the `eval`
  *  function that is visible to clients.
  *)
 let eval (e : E.t) (pgrm : P.t) : (Value.t * Frame.t) =
   eval Frame.emptyE e pgrm
 
 let statement (s : S.t) (pgrm : P.t) : Frame.t =
   statement Frame.emptyE s pgrm
 
 (* exec p :  execute the program p according to the operational semantics
  * provided as a handout.
  *)
 let exec (p : Ast.Program.t) : unit =
   let _ = eval (E.Call ("main", [])) p in 
     ()
