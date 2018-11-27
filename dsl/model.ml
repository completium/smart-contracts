
(************************************************************************************
  model type
 ************************************************************************************)

type basic_type =
  | Uint
  | String
  | Double
  | Date
  | Address
  | Tez

type ident = string

(* struct ****************************************************************************)

type field_type =
  | FIdentifier of basic_type
  | FBasic      of basic_type
  | FRef        of ident

type field = ident * field_type

type arg_type =
  | ABasic of basic_type
  | ARef   of ident

type arg   = ident * arg_type

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
    mArgs        : arg list;
  }

let empty_machine = { mStates = []; mTransitions = []; mArgs = []; }

(* entity and model ******************************************************************)

type entity =
  | Const       of ident * basic_type
  | Asset       of ident * field list
  | Machine     of ident * state_machine
  | Transaction of ident * arg list

type model = entity list

(************************************************************************************
  Dump model
 ************************************************************************************)

let dump_type = function
  | Uint   -> "uint"
  | String -> "string"
  | Double -> "double"
  | Address -> "address"
  | Date   -> "date"
  | Tez    -> "tez"

let dump_field (id,ft) =
  match ft with
  | FIdentifier t -> id ^ " : " ^ (dump_type t)^" (identifier)"
  | FBasic      t -> id ^ " : " ^ (dump_type t)
  | FRef        t -> id ^ " : " ^ t ^ " ref"

let dump_fields fs = String.concat "\n\t" (List.map dump_field fs)

let dump_arg (id,arg) =
  match arg with
  | ABasic      t -> id ^ " : " ^ (dump_type t)
  | ARef        t -> id ^ " : " ^ t ^ " ref"

let dump_args fs = String.concat "\n\t" (List.map dump_arg fs)

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
  | Transaction (id,fs) -> "transaction "^id^" {\n\t"^(dump_args fs)^"\n}"

let dump_model m = String.concat "\n\n" (List.map dump_entity m)

let empty = []

(***********************************************************************************
  Helpers
 ***********************************************************************************)

(* kind of bind ... *)
let (>>) (m : model) (f : model -> model) : model = f m

let mk_cons id typ = fun m -> m @ [Const (id, typ)]

let mk_asset id fds = fun m -> m @ [Asset (id, fds)]

let mk_identifier id typ = (id, FIdentifier typ)

let mk_field id typ = (id, FBasic typ)

let mk_field_ref id typ = (id, FRef typ)

let mk_arg id typ = (id, ABasic typ)

let mk_arg_ref id typ = (id, ARef typ)

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

let mk_transaction id fds = fun m -> m @ [Transaction (id, fds)]

(***********************************************************************************
  test
 ***********************************************************************************)

let _ =
  (* TODO : implement extension let%model m = ... as let m = empty >> ... *)
  print_endline "\nERC20\n";
  let erc20_model =
    empty >>
      mk_cons "symbol" String >>
      mk_cons "name"   String >>
      mk_cons "total"  Uint >>
      mk_asset "tokenHolder" [
          mk_identifier   "holder"  Address;
          mk_field        "balance" Uint;
        ] >>
      mk_transaction "transfer" [
          mk_arg "toAddress"  Address;
          mk_arg "nbTokens"   Uint;
        ]
  in
  print_endline (dump_model erc20_model);
  print_endline "\nEscrow\n";
  let escrow_model =
    empty >>
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
  print_endline (dump_model escrow_model)

(*******************************************************************************
  Transactions (aka logic)
 *******************************************************************************)

(* basic real types for transaction logic *)
type uint    = int
type double
type date
type address
type tez

type sender = {
    address : address;
    balance : uint;
  }

type assets

(* dynamically generated structs *)

(* identifier is not generated *)
type tokenHolder = {
    balance : uint;
  }

let getTokenHolder : assets -> address -> tokenHolder =
  fun _ _ -> { balance = 0; }

let setTokenHolder : assets -> tokenHolder -> assets = fun a _ -> a

(* actual transation code ****************************************************)

let[@Escrow transfer] transfer iAssets iSender ~toAddress:iTo ~nbTokens:iQty =
  let lFromholder = getTokenHolder iAssets iSender.address in
  let lFromholder = { balance = lFromholder.balance - iQty; } in
  let lToholder   = getTokenHolder iAssets iTo in
  let lToholder   = { balance = lToholder.balance + iQty; } in
  setTokenHolder (setTokenHolder iAssets lToholder) lFromholder
