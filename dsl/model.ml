
(************************************************************************************
  model type
 ************************************************************************************)

type basic_type =
  | Uint
  | String
  | Double
  | Date
  | Tez

type ident = string

(* struct ****************************************************************************)

type field_type =
  | Basic of basic_type
  | Ref   of ident

type field = ident * field_type

(* state machine ********************************************************************)

type state_typ = SInitial | SBasic | STerminal

type state = ident * state_typ

type transition = {
    mId        : ident;
    mFromState : ident;
    mToState   : ident;
    (* add roles; conditions; actions; ... *)
  }

let empty_transition = { mId = ""; mFromState = ""; mToState = ""; }

type state_machine = {
    mStates      : state list;
    mTransitions : transition list;
    mArgs        : field list;
  }

let empty_machine = { mStates = []; mTransitions = []; mArgs = []; }

(* entity and model ******************************************************************)

type entity =
  | Const    of ident * basic_type
  | Asset    of ident * field list
  | Machine  of ident * state_machine

type model = entity list

(************************************************************************************
  Dump model
 ************************************************************************************)

let dump_type = function
  | Uint   -> "uint"
  | String -> "string"
  | Double -> "double"
  | Date   -> "date"
  | Tez    -> "tez"

let dump_field (id,ft) =
  match ft with
  | Basic t -> id ^ " : " ^ (dump_type t)
  | Ref   t -> id ^ " : " ^ t ^ " ref"

let dump_fields fs = String.concat "\n\t" (List.map dump_field fs)

let dump_statety = function
  | SBasic -> ""
  | STerminal -> "terminal"
  | SInitial -> "initial"

let dump_state (s,st) = s^"\t"^(dump_statety st)

let dump_transition tr =
  "  transition "^(tr.mId)^" from "^(tr.mFromState)^" to "^(tr.mToState)


let dump_machine m =
  "\n  states\n\t | "
  ^(String.concat "\n\t | " (List.map dump_state m.mStates))
  ^"\n\n"
  ^(String.concat "\n\n" (List.map dump_transition m.mTransitions))
  ^"\n"

let dump_entity = function
  | Const (id,t)  -> "constant "^id^" of "^(dump_type t)
  | Asset (id,fs) -> "asset "^id^" {\n\t"^(dump_fields fs)^"\n}"
  | Machine (id,m) -> "machine "^id^" {\n\t"^(dump_machine m)^"\n}"

let dump_model m = String.concat "\n\n" (List.map dump_entity m)

let empty = []

(***********************************************************************************
  Helpers
 ***********************************************************************************)

(* kind of bind ... *)
let (>>) (m : model) (f : model -> model) : model = f m

let mk_cons id typ = fun m -> m @ [Const (id, typ)]

let mk_asset id fds = fun m -> m @ [Asset (id, fds)]

let mk_field id typ = (id, Basic typ)

let mk_field_ref id typ = (id, Ref typ)

let mk_state_machine id states =
  fun m -> m @ [ Machine (id, { empty_machine with mStates = states; })]

let mk_transition smid trid tr =
  List.map (fun e ->
      match e with
      | Machine (id,m) when compare id smid = 0 ->
         Machine (
             id,
             { m with
               mTransitions = m.mTransitions @ [ { tr with mId = trid } ]
             }
           )
      | _ -> e
    )

(***********************************************************************************
  test
 ***********************************************************************************)

(* built-in *)
type uint = int

type account

(* smart contract specification *)
module type [@smartcontract] Escrow = sig

  val symbol : string

  val name   : string

  val total  : uint

  type [@asset] tokenHolder = {
      holder  : account ref;
      balance : uint;
    }

  type [@transaction] transfer = {
      amount   : uint;
      toHolder : account ref;
    }

  type [@sm] states =
    | Created    [@initial]
    | Aborted    [@terminal]
    | Confirmed
    | Failed     [@terminal]
    | Transfered [@terminal]

  (* How to pass properties to transaction ? *)
  type [@transition
        fromState = Created;
        toState   = Aborted;
        roles     = ANY;
       ] abort

end

(**
   constant symbol of string

   constant name of string

   constant total of uint

   asset tokenHolder {
      holder  : account ref;
      balance : uint;
   }
 *)
let _ =
  (* TODO : implement extension let%model m = ... as let m = empty >> ... *)
  let m =
    empty >>
      mk_cons "symbol" String >>
      mk_cons "name"   String >>
      mk_cons "total"  Uint >>
      mk_asset "tokenHolder" [
          mk_field_ref "holder" "account";
          mk_field     "balance" Uint
        ] >>
      mk_state_machine "sm" [
          "Created",    SInitial;
          "Aborted",    STerminal;
          "Confirmed",  SBasic;
          "Failed",     STerminal;
          "Transfered", STerminal;
        ] >>
      (* use extension to rm "default_transition with" ... *)
      mk_transition "sm" "abort" { empty_transition with
          mFromState = "Created";
          mToState   = "Aborted";
        } in
  print_endline (dump_model m)
