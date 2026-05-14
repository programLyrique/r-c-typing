open Tree_sitter_c
open Tree_sitter_run
open CST
module U = Parser_utils
module A = PAst

module StrMap = Map.Make(String)

type value = VInt of int | VFloat of float

let constants : value StrMap.t ref = ref StrMap.empty

let reset () = constants := StrMap.empty
let remember name v = constants := StrMap.add name v !constants
let lookup name = StrMap.find_opt name !constants

type promoted =
  | BothInt of int * int
  | BothFloat of float * float

let promote2 l r =
  match l, r with
  | VInt li, VInt ri -> BothInt (li, ri)
  | VInt li, VFloat rf -> BothFloat (float_of_int li, rf)
  | VFloat lf, VInt ri -> BothFloat (lf, float_of_int ri)
  | VFloat lf, VFloat rf -> BothFloat (lf, rf)

let to_float = function
  | VInt i -> float_of_int i
  | VFloat f -> f

(* Determine how a cast type_descriptor should coerce a value.
   Returns `Int, `Float, or `Unknown (pass through). *)
let cast_kind_of_type_descriptor (ty : type_descriptor) =
  let (_, type_spec, _, _) = ty in
  let name = match type_spec with
    | `Prim_type (_, s) -> s
    | `Id (_, s) -> s
    | `Sized_type_spec _ -> "int"
    | _ -> ""
  in
  match name with
  | "int" | "unsigned" | "short" | "long" | "char" | "signed"
  | "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t"
  | "int8_t" | "int16_t" | "int32_t" | "int64_t"
  | "size_t" | "ssize_t" | "ptrdiff_t" | "intptr_t" | "uintptr_t"
  | "off_t" | "pid_t" | "mode_t" | "time_t" | "clock_t" -> `Int
  | "float" | "double" -> `Float
  | _ -> `Unknown

let rec eval_c_expression ?(local_env = StrMap.empty) (expr : expression) : value option =
  match expr with
  | `Choice_cond_exp e -> eval_not_binary ~local_env e
  | `Bin_exp bin -> eval_binary ~local_env bin

and eval_not_binary ~local_env (expr : expression_not_binary) : value option =
  match expr with
  | `Num_lit (_, s) ->
      begin try
        match U.parse_number_literal s with
        | A.CInt i -> Some (VInt i)
        | A.CFloat f -> Some (VFloat f)
        | _ -> None
      with Failure _ -> None
      end
  | `Char_lit cl -> Some (VInt (U.int_of_char_literal cl))
  | `True _ -> Some (VInt 1)
  | `False _ -> Some (VInt 0)
  | `Null _ -> Some (VInt 0)
  | `Paren_exp (_, `Exp e, _) -> eval_c_expression ~local_env e
  | `Paren_exp _ -> None
  | `Un_exp (op, e) ->
      begin match eval_c_expression ~local_env e with
      | None -> None
      | Some v ->
          begin match op with
          | `PLUS _ -> Some v
          | `DASH _ ->
              begin match v with
              | VInt i -> Some (VInt (-i))
              | VFloat f -> Some (VFloat (Float.neg f))
              end
          | `BANG _ ->
              let is_zero = match v with VInt 0 -> true | VFloat f -> f = 0.0 | _ -> false in
              Some (VInt (if is_zero then 1 else 0))
          | `TILDE _ ->
              begin match v with
              | VInt i -> Some (VInt (lnot i))
              | VFloat _ -> None
              end
          end
      end
  | `Cast_exp (_, ty, _, e) ->
      begin match eval_c_expression ~local_env e with
      | None -> None
      | Some v ->
          begin match cast_kind_of_type_descriptor ty with
          | `Int ->
              begin match v with
              | VInt _ -> Some v
              | VFloat f -> Some (VInt (int_of_float f))
              end
          | `Float ->
              begin match v with
              | VFloat _ -> Some v
              | VInt i -> Some (VFloat (float_of_int i))
              end
          | `Unknown -> Some v
          end
      end
  | `Cond_exp (cond, _, then_opt, _, else_e) ->
      let then_v () = match then_opt with
        | None -> None
        | Some (`Exp te) -> eval_c_expression ~local_env te
        | Some (`Comma_exp _) -> None
      in
      begin match eval_c_expression ~local_env cond with
      | None ->
          let tv = then_v () in
          let ev = eval_c_expression ~local_env else_e in
          if tv = ev then tv else None
      | Some cv ->
          let truthy = match cv with
            | VInt 0 -> false
            | VFloat f when f = 0.0 -> false
            | _ -> true
          in
          if truthy then
            (match then_opt with
             | None -> Some cv
             | Some (`Exp te) -> eval_c_expression ~local_env te
             | Some (`Comma_exp _) -> None)
          else
            eval_c_expression ~local_env else_e
      end
  | `Id (_, name) ->
      begin match StrMap.find_opt name local_env with
      | Some v -> Some v
      | None -> lookup name
      end
  | `Exte_exp (_, e) -> eval_c_expression ~local_env e
  | _ -> None

and eval_binary ~local_env (bin : binary_expression) : value option =
  let eval e = eval_c_expression ~local_env e in
  let arith f_int f_float l r =
    match eval l, eval r with
    | Some lv, Some rv ->
        begin match promote2 lv rv with
        | BothInt (a, b) ->
            (try Some (VInt (f_int a b)) with Division_by_zero -> None)
        | BothFloat (a, b) -> Some (VFloat (f_float a b))
        end
    | _ -> None
  in
  let int_only f l r =
    match eval l, eval r with
    | Some (VInt a), Some (VInt b) ->
        (try Some (VInt (f a b)) with Division_by_zero -> None)
    | _ -> None
  in
  let cmp f_int f_float l r =
    match eval l, eval r with
    | Some lv, Some rv ->
        let result = match promote2 lv rv with
          | BothInt (a, b) -> f_int a b
          | BothFloat (a, b) -> f_float a b
        in
        Some (VInt (if result then 1 else 0))
    | _ -> None
  in
  let is_truthy v = to_float v <> 0.0 in
  let lazy_and l r =
    match eval l with
    | Some lv when not (is_truthy lv) -> Some (VInt 0)
    | Some _ ->
        begin match eval r with
        | Some rv -> Some (VInt (if is_truthy rv then 1 else 0))
        | None -> None
        end
    | None ->
        begin match eval r with
        | Some rv when not (is_truthy rv) -> Some (VInt 0)
        | _ -> None
        end
  in
  let lazy_or l r =
    match eval l with
    | Some lv when is_truthy lv -> Some (VInt 1)
    | Some _ ->
        begin match eval r with
        | Some rv -> Some (VInt (if is_truthy rv then 1 else 0))
        | None -> None
        end
    | None ->
        begin match eval r with
        | Some rv when is_truthy rv -> Some (VInt 1)
        | _ -> None
        end
  in
  match bin with
  | `Exp_PLUS_exp (l, _, r) -> arith ( + ) ( +. ) l r
  | `Exp_DASH_exp (l, _, r) -> arith ( - ) ( -. ) l r
  | `Exp_STAR_exp (l, _, r) -> arith ( * ) ( *. ) l r
  | `Exp_SLASH_exp (l, _, r) -> arith ( / ) ( /. ) l r
  | `Exp_PERC_exp (l, _, r) -> int_only ( mod ) l r
  | `Exp_LTLT_exp (l, _, r) -> int_only ( lsl ) l r
  | `Exp_GTGT_exp (l, _, r) -> int_only ( asr ) l r
  | `Exp_AMP_exp (l, _, r) -> int_only ( land ) l r
  | `Exp_BAR_exp (l, _, r) -> int_only ( lor ) l r
  | `Exp_HAT_exp (l, _, r) -> int_only ( lxor ) l r
  | `Exp_AMPAMP_exp (l, _, r) -> lazy_and l r
  | `Exp_BARBAR_exp (l, _, r) -> lazy_or l r
  | `Exp_EQEQ_exp (l, _, r) -> cmp ( = ) ( = ) l r
  | `Exp_BANGEQ_exp (l, _, r) -> cmp ( <> ) ( <> ) l r
  | `Exp_LT_exp (l, _, r) -> cmp ( < ) ( < ) l r
  | `Exp_LTEQ_exp (l, _, r) -> cmp ( <= ) ( <= ) l r
  | `Exp_GT_exp (l, _, r) -> cmp ( > ) ( > ) l r
  | `Exp_GTEQ_exp (l, _, r) -> cmp ( >= ) ( >= ) l r

(* Evaluate a raw #define value string by re-parsing it as a C expression. *)
let eval_define_string (raw : string) : value option =
  let tu_text = "int __ce__ = (" ^ raw ^ ");" in
  match Parse.string tu_text with
  | { Parsing_result.program = Some (item :: _); _ } ->
      begin match item with
      | `Decl (_, `Init_decl (_, _, `Exp expr), _, _) ->
          eval_c_expression expr
      | _ -> None
      end
  | _ -> None


(* Plug ourselves into [PAst]'s enum-int lookup hook. [PAst.process_call]
   uses it to rewrite SEXPTYPE-int arguments to their named binding (e.g.
   [Rf_allocVector(R_TYPE_integer, n)] -> [Rf_allocVector(INTSXP, n)]). *)
let () =
  A.set_enum_int_lookup (fun name ->
    match lookup name with
    | Some (VInt n) -> Some n
    | _ -> None)


let%test "VInt arithmetic" =
  let raw_add = eval_define_string "1 + 2" in
  let raw_shift = eval_define_string "1 << 3" in
  let raw_or = eval_define_string "(1 << 0) | (1 << 1)" in
  raw_add = Some (VInt 3)
  && raw_shift = Some (VInt 8)
  && raw_or = Some (VInt 3)

let%test "VFloat arithmetic" =
  let raw_float = eval_define_string "1.0 / 2.0" in
  let raw_mixed = eval_define_string "(double)1 / 2" in
  raw_float = Some (VFloat 0.5)
  && raw_mixed = Some (VFloat 0.5)

let%test "identifier lookup in global env" =
  reset ();
  remember "X" (VInt 5);
  let res = eval_define_string "X + 3" in
  reset ();
  res = Some (VInt 8)

let%test "sizeof returns None" =
  eval_define_string "sizeof(int)" = None
