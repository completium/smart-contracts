
(************************************************************************************
  model type
 ************************************************************************************)

type mtype =
  | Uint
  | String
  | Date


(* state machine *)
type state_typ = SInitial | SBasic | STerminal

type state = string * state_typ

type transition = {
    mId : string;
    mFromState : state;
    mToState : state;
    (* add roles; conditions; actions; ... *)
  }

type state_machine = {
    mStates : state list;
    mTransitions : transition list
  }

type ident = string

type field_type =
  | Basic of mtype
  | Ref   of ident

type field = ident * field_type

type entity =
  | Const of ident * mtype
  | Asset of ident * field list
  | Machine of state_machine

type model = entity list

(************************************************************************************
  Dump model
 ************************************************************************************)

let dump_type = function
  | Uint   [@id 1] -> "uint"
  | String [@id 2] -> "string"
  | Date   [@id 3] -> "date"

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
  Constructors
 ***********************************************************************************)

(* kind of bind *)
let (>>) (m : model) (d : entity) : model = m @ [ d ]

let constant id typ = Const (id, typ)
let asset id fds = Asset (id, fds)
let field id typ = (id, Basic typ)
let field_ref id typ = (id, Ref typ)


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
      constant "symbol" String >>
      constant "name"   String >>
      constant "total"  Uint >>
      asset "tokenHolder" [
          field_ref "holder" "account";
          field     "balance" Uint
        ] in
  print_endline (dump_model m)
