
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

(* transfer *************************************************************************)

type transfer_policy =
  | Direct
  | Indirect

type transfer_data = {
    mAsset  : ident;
    mFrom   : ident;
    mTo     : ident;
    mField  : ident;
    mPolicy : transfer_policy;
  }

(* transaction data *****************************************************************)

type tx_data = {
    mRoles : ident list;
  }

let empty_txd = { mRoles = [] }

(* state machine ********************************************************************)

type state_typ = SInitial | SBasic | STerminal

type state = ident * state_typ

type transition = {
    mId          : ident;
    mFromState   : ident;
    mToState     : ident;
    mTransaction : tx_data;
  }

let empty_transition = { mId = ""; mFromState = ""; mToState = ""; mTransaction = empty_txd }

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
  | Transaction of ident * tx_data * arg list
  | Transfer    of ident * transfer_data
  | Role        of ident

type model = ident * entity list

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
  | SBasic    -> ""
  | STerminal -> "terminal"
  | SInitial  -> "initial"

let dump_state (s,st) = s^"\t"^(dump_statety st)

let dump_transition tr =
  "   transition "^(tr.mId)^" from "^(tr.mFromState)^" to "^(tr.mToState)

let dump_machine m =
  "    states\n\t | "
  ^(String.concat "\n\t | " (List.map dump_state m.mStates))
  ^"\n\n"
  ^(String.concat "\n\n" (List.map dump_transition m.mTransitions))
  ^"\n"

let dump_trpol = function
  | Direct   -> "direct"
  | Indirect -> "indirect"

let dump_trd trd =
  trd.mAsset^" "^trd.mFrom^" "^trd.mTo^" "^trd.mField^" "
  ^(dump_trpol trd.mPolicy)

let dump_entity = function
  | Const (id,t)             -> "constant "   ^id^" of "  ^(dump_type t)
  | Asset (id,fs)            -> "asset "      ^id^" {\n\t"^(dump_fields fs)^"\n  }"
  | Machine (id,m)           -> "machine "    ^id^" {\n\n"^(dump_machine m)^"\n  }"
  | Transaction (id,txd,fs)  -> "transaction "^id^" {\n\t"^(dump_args fs)  ^"\n  }"
  | Transfer (id,trd)        -> "transfer "   ^id^" {\n\t"^(dump_trd trd)  ^"\n  }"
  | Role id                  -> "role "       ^id

let dump_model (id,m) =
  "model "^id^" {\n\n  "
  ^(String.concat "\n\n  " (List.map dump_entity m))
  ^"\n}"

let empty = []

(***********************************************************************************
  Helpers
 ***********************************************************************************)

(* kind of bind ... *)
let (>>) (m : model) (f : model -> model) : model = f m

let cons ~id:id ~typ:typ = fun (mid,m) -> (mid,m @ [Const (id, typ)])

let asset ~id:id ~fields:fds = fun (mid,m) -> (mid,m @ [Asset (id, fds)])

let identifier ~id:id ~typ:typ = (id, FIdentifier typ)

let field ~id:id ~typ:typ = (id, FBasic typ)

let field_ref ~id:id ~typ:typ = (id, FRef typ)

let arg ~id:id ~typ:typ = (id, ABasic typ)

let arg_ref ~id:id ~typ:typ = (id, ARef typ)

let state_machine ~id:id ~states:states =
  fun (mid,m) -> (mid, m @ [ Machine (id, { empty_machine with mStates = states; })])

let transition ~machine:smid ~id:trid ~args:tr = fun (mid, m) ->
  (mid, List.map (fun e ->
      match e with
      | Machine (id,m) when compare id smid = 0 ->
         Machine (
             id,
             { m with
               mTransitions = m.mTransitions @ [ { tr with mId = trid } ]
             }
           )
      | _ -> e
    ) m)

let transaction ~id:id ~args:fds = fun (mid,m) -> (mid, m @ [Transaction (id, empty_txd, fds)])

let role ~id:id = fun (mid, m) -> (mid, m @ [Role id])

let transfer ~id:id ~asset:a ~from:f ~tom:t ~field:fi ?policy:(p=Indirect) = fun (mid, m) ->
  (mid, m @ [Transfer (id, { mAsset = a; mFrom = f; mTo = t; mField = fi; mPolicy = p; })] )

let model ~name:id = (id, [])

(***********************************************************************************
  test
 ***********************************************************************************)

let _ =
  (* TODO : implement extension let%model m = ... as let m = empty >> ... *)
  let erc20_model =
    model ~name:"ERC20"                                            >>
      cons ~id:"symbol" ~typ:String                                >>
      cons ~id:"name"   ~typ:String                                >>
      cons ~id:"total"  ~typ:Uint                                  >>
      asset ~id:"tokenHolder" ~fields:[
          identifier   "holder"  Address;
          field        "balance" Uint;
        ]                                                          >>
      transaction ~id:"transfer" ~args:[
          arg "toAddress"  Address;
          arg "nbTokens"   Uint;
        ]
  in
  print_endline (dump_model erc20_model);
  let escrow_model =
    model ~name:"Escrow"                                            >>
      role ~id:"buyer"                                              >>
      role ~id:"seller"                                             >>
      state_machine ~id:"sm" ~states:[
          "Created",    SInitial;
          "Aborted",    STerminal;
          "Confirmed",  SBasic;
          "Failed",     STerminal;
          "Transfered", STerminal;
        ]                                                          >>
      (* use extension to rm "default_transition with" ... *)
      transition ~machine:"sm" ~id:"abort" ~args:{ empty_transition with
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

let transfer iAssets iSender ~toAddress:iTo ~nbTokens:iQty =
  let lFromholder = getTokenHolder iAssets iSender.address in
  let lFromholder = { balance = lFromholder.balance - iQty; } in
  let lToholder   = getTokenHolder iAssets iTo in
  let lToholder   = { balance = lToholder.balance + iQty; } in
  setTokenHolder (setTokenHolder iAssets lToholder) lFromholder
