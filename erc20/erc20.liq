[%%version 0.4]

type balances = (address, nat) map
type allowances = (address, balances) map

(*
  (Pair "" (Pair "" (Pair (nat 0) (Pair _ _))))
*)
type storage = {
    mSymbol      : string;
    mName        : string;
    mTotalSupply : nat;
    mBalances    : balances;
    mAllowed     : allowances;
  }

let[@inline] gEmptyOp = ([] : operation list)

let%init storage (iSymbol : string) (iName : string) (iTotalSupply : nat) (iOwner : address)=
  let lBalances = Map.add iOwner iTotalSupply (Map : balances) in
  {
    mSymbol         = iSymbol;
    mName           = iName;
    mTotalSupply    = iTotalSupply;
    mBalances       = lBalances;
    mAllowed        = (Map : allowances);
  }

let getBalance (iAddr : address) (iBalances : balances) =
  match Map.find iAddr iBalances with
  | Some b -> b
  | None -> Current.failwith ("Sender not found in balances.")

let getBalanceInit (iAddr : address) (iBalances : balances) =
  match Map.find iAddr iBalances with
  | Some b -> b
  | None -> (0 : nat)

let nat_sub (a : nat) (b : nat) =
  match%nat a - b with
  | Plus v -> v
  | Minus _ -> Current.failwith "nat_sub result is not a nat."

let execTransfer (iFrom : address) (iTo : address) (iQty : nat) (iBalances : balances) =
 (* update from balance *)
  let lFromBalance = getBalance iFrom iBalances in
  let lNewFromBalance = nat_sub lFromBalance iQty in
  let lNewBalances = Map.update iFrom (Some lNewFromBalance) iBalances in
  (* update to balance *)
  let lToBalance = getBalanceInit iTo iBalances in
  let lNewToBalance = lToBalance + iQty in
  let lNewBalances = Map.update iTo (Some lNewToBalance) lNewBalances in
  lNewBalances

let%entry transfer (iParam : address * nat) iStorage =
  let lFrom  = Current.sender () in
  let lTo    = iParam.(0) in
  let lQty   = iParam.(1) in
  let lNewBalances = execTransfer lFrom lTo lQty iStorage.mBalances in
  (* OCAML
  let lStorage = { iStorage with
                   mBalances    = lNewBalances;
                 } in
   *)
  let lStorage = { mSymbol      = iStorage.mSymbol;
                   mName        = iStorage.mName;
                   mTotalSupply = iStorage.mTotalSupply;
                   mBalances    = lNewBalances;
                   mAllowed     = iStorage.mAllowed;
                 } in
  (gEmptyOp, lStorage)

(* NB :
   1/ anyone can register in allowed map ?
   2/ no check whether the allowed amount is greater than balance ?
 *)
let%entry approve (iParam : address * nat) iStorage =
  let lFrom  = Current.sender () in
  let lTo    = iParam.(0) in
  let lQty   = iParam.(1) in
  let lFromAllowed =
    match Map.find lFrom iStorage.mAllowed with
    | Some a -> a
    | None -> (Map : balances) in
  let lNewFromAllowed = Map.update lTo (Some lQty) lFromAllowed in
  let lNewAllowed = Map.update lFrom (Some lNewFromAllowed) iStorage.mAllowed in
  (* OCAML
  let lStorage = { iStorage with
                   mAllowed    = lNewAllowed;
                 } in
   *)
  let lStorage = { mSymbol      = iStorage.mSymbol;
                   mName        = iStorage.mName;
                   mTotalSupply = iStorage.mTotalSupply;
                   mBalances    = iStorage.mBalances;
                   mAllowed     = lNewAllowed;
                 } in
  (gEmptyOp, lStorage)

let%entry transferFrom (iParam : address * address * nat) iStorage =
  let lFrom  = iParam.(0) in
  let lTo    = iParam.(1) in
  let lQty   = iParam.(2) in
  (* update allowed *)
  let lFromAllowed =
    match Map.find lFrom iStorage.mAllowed with
    | Some q -> q
    | None -> Current.failwith "From address not found in allowed quantities." in
  let lAllowedQty =
    match Map.find lTo lFromAllowed with
    | Some q -> q
    | None -> Current.failwith "To address not found in From allowed quantities." in
  let lNewAllowedQty = nat_sub lAllowedQty lQty in
  let lNewFromAllowed = Map.update lTo (Some lNewAllowedQty) lFromAllowed in
  let lNewAllowed = Map.update lFrom (Some lNewFromAllowed) iStorage.mAllowed in
  (* execute transfer *)
  let lNewBalances = execTransfer lFrom lTo lQty iStorage.mBalances in
  (* OCAML
  let lStorage = { iStorage with
                   mBalances   = lNewBalances;
                   mAllowed    = lNewAllowed;
                 } in
   *)
  let lStorage = { mSymbol      = iStorage.mSymbol;
                   mName        = iStorage.mName;
                   mTotalSupply = iStorage.mTotalSupply;
                   mBalances    = lNewBalances;
                   mAllowed     = lNewAllowed;
                 } in
  (gEmptyOp, lStorage)
