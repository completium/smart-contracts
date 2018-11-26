
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

(* state machine ********************************************************************)

type state_typ = SInitial | SBasic | STerminal

type state = ident * state_typ

type transition = {
    mFromState : state;
    mToState   : state;
    (* add roles; conditions; actions; ... *)
  }

type state_machine = {
    mStates      : state list;
    mTransitions : transition list
  }

let empty_machine = { mStates = []; mTransitions = []; }

(* struct ****************************************************************************)

type field_type =
  | Basic of basic_type
  | Ref   of ident

type field = ident * field_type

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

let dump_cmd = function
  | Const (id,t)  -> "constant "^id^" of "^(dump_type t)
  | Asset (id,fs) -> "asset "^id^" {\n\t"^(dump_fields fs)^"\n}"
  | _ -> "not implemented"

let dump_model m = String.concat "\n\n" (List.map dump_cmd m)

let empty = []

(***********************************************************************************
  Helpers
 ***********************************************************************************)

(* kind of bind ... *)
let (>>) (m : model) (d : entity) : model = m @ [ d ]

let mk_cons id typ = Const (id, typ)

let mk_asset id fds = Asset (id, fds)

let mk_field id typ = (id, Basic typ)

let mk_field_ref id typ = (id, Ref typ)

let mk_state_machine id states = (id, { empty_machine with mStates = states; })

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
        ] in
  print_endline (dump_model m)
