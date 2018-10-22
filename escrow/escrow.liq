[%%version 0.4]

(* ************************************************************************************
    Tools
   ************************************************************************************)

let[@inline] gEmptyOp = ([] : operation list)

let[@inline] keyh_to_addr (iKeyh : key_hash) = Contract.address (Account.default iKeyh)

(* ************************************************************************************
    Storage
   ************************************************************************************)

(*type state =
  | TransferringPenalty                     (* waiting for penalty transfer from seller *)
  | Created                                 (* initial state *)
  | Confirmed                               (* confirmed, cannot be aborted *)
  | Aborted                                 (* transaction aborted before confirmation  *)
  | Succeeded                               (* quantity transferred to seller *)
  | Failed                                  (* quantity returned to buyer, penalty lost *)

type penalty_to_state =
  | PTTransferred                           (* penalty recipient is set*)
  | PTTransferring                          (* penalty recipeint awaits confirmation *)
 *)

type state = string

type storage = {
    mBuyer          : key_hash;             (* transfer quantity from *)
    mBuyerPKey      : key;                  (* buyer's key *)
    mSeller         : key_hash;             (* transfer quantity to *)
    mPenaltyTo      : key_hash;             (* tranfer penalty to; defaulted to buyer *)
    mQuantity       : tez;                  (* exchanged quantity *)
    mPenalty        : tez;                  (* seller's penalty *)
    mIncentive      : tez;                  (* buyer's penalty *)
    mState          : state;                (* contract's state *)
  }

let%init storage
      (iBuyer     : key_hash)
      (iBuyerPKey : key)
      (iSeller    : key_hash)
      (iPenaltyTo : key_hash option)
      (iQuantity  : tez)
      (iPenalty   : tez)
      (iIncentive : tez) =
  (* iPenaltyTo's address is defaulted to buyer's address *)
  let lPenaltyTo =
    match iPenaltyTo with
    | Some a -> a
    | None -> iBuyer
  in
  let lState =
    if iPenalty = 0tz
    then "Created"
    else "TransferringPenalty"
  in
  {
    mBuyer          = iBuyer;
    mBuyerPKey      = iBuyerPKey;
    mSeller         = iSeller;
    mPenaltyTo      = lPenaltyTo;
    mQuantity       = iQuantity;
    mPenalty        = iPenalty;
    mIncentive      = iIncentive;
    mState          = lState;
  }

(* ************************************************************************************
    State utilities
   ************************************************************************************)

let isTransferring (iState : state) =
(*  match iState with
  | TransferringPenalty -> true
  | _ -> false *)
  iState = "TransferringPenalty"

let isCreated (iState : state) =
(*  match iState with
  | Created -> true
  | _ -> false *)
  iState = "Created"

let isConfirmed (iState : state) =
(*  match iState with
  | Confirmed -> true
  | _ -> false *)
  iState = "Confirmed"

let setState (iState : state) (iStorage : storage) =
  let lStorage = iStorage.mState <- iState in
  lStorage

(* ************************************************************************************
    Seller's penalty initialisation
   ************************************************************************************)

let%entry transferPenalty (iParam : unit) iStorage =
  if not (isTransferring iStorage.mState)
  then failwith "Penalty is already set."
  else
    let lSender = Current.sender () in
    let lSeller = keyh_to_addr iStorage.mSeller in
    if lSender <> lSeller
    then failwith "transferPenaly entry can only be called by seller."
    else
      let lAmount = Current.amount () in
      if lAmount > iStorage.mPenalty
      then failwith "Too much tez for penalty."
      else if lAmount < iStorage.mPenalty
      then failwith "Not enough tez for penalty."
      else (gEmptyOp, setState "Created" iStorage)

(* ************************************************************************************
    Abort contract
   ************************************************************************************)

let transferBack (iStorage : storage) =
  (* transfer quantity back to buyer *)
  let lBuyer = Account.default iStorage.mBuyer in
  let lTotal = iStorage.mQuantity + iStorage.mIncentive in
  let lOps = [Contract.transfer ~dest:lBuyer ~amount:lTotal] in
  (* transfer penalty to penalty recipient *)
  let lOps =
    if iStorage.mPenalty > 0tz
    then
      let lPenaltyTo = Account.default iStorage.mSeller in
      (Contract.transfer ~dest:lPenaltyTo ~amount:(iStorage.mPenalty))::lOps
    else lOps in
  lOps

let%entry abortContract (iParam : unit) iStorage =
  if not (isCreated iStorage.mState) || not (isTransferring iStorage.mState)
  then Current.failwith "Abortion is possible only in Created state."
  else
    let lOps = transferBack iStorage in
    (gEmptyOp, setState "Aborted" iStorage)

(* ************************************************************************************
    Confirm contract
   ************************************************************************************)

let%entry confirmContract (iParam : unit) iStorage =
  if not (isCreated iStorage.mState)
  then Current.failwith "Invalid contract state for confirmation."
  else
    let lBalance = Current.balance () in
    let lTotal = iStorage.mIncentive + iStorage.mQuantity + iStorage.mPenalty in
    if lBalance > lTotal
    then failwith "Too much tez sent to contract."
    else if lBalance < lTotal
    then failwith "Not enough tez sent to contract."
    else (gEmptyOp, setState "Confirmed" iStorage)

(* ************************************************************************************
    Payment utilities
   ************************************************************************************)

let transferAmounts (iStorage : storage) =
  (* transfer quantity to seller *)
  let lSeller = Account.default iStorage.mSeller in
  let lOps = [Contract.transfer ~dest:lSeller ~amount:(iStorage.mQuantity)] in
  (* transfer penalty to penalty recipient *)
  let lOps =
    if iStorage.mPenalty > 0tz
    then
      let lPenaltyTo = Account.default iStorage.mPenaltyTo in
      (Contract.transfer ~dest:lPenaltyTo ~amount:(iStorage.mPenalty))::lOps
    else lOps in
  (* transfer incentive back to buyer *)
  let lOps =
    if iStorage.mIncentive > 0tz
    then
      let lBuyer = Account.default iStorage.mBuyer in
      (Contract.transfer ~dest:lBuyer ~amount:(iStorage.mIncentive))::lOps
    else lOps in
  lOps

(* ************************************************************************************
    Transfer payment
   ************************************************************************************)

let%entry transferPayment (iParam : unit) iStorage =
  if not (isConfirmed iStorage.mState)
  then Current.failwith "Payment is only possible in Confirmed state."
  else
    let lSender = Current.sender () in
    let lBuyer = keyh_to_addr iStorage.mBuyer in
    if lSender <> lBuyer
    then Current.failwith "confirmPayment can only be called by buyer."
    else
      let lOps = transferAmounts iStorage in
      (lOps, setState "Succeeded" iStorage)

(* ************************************************************************************
    Receive payment
   ************************************************************************************)

(* checkSignature makes data from:
 - contract's address
 - tez quantity
 - nonce *)
let%entry receivePayment (iParam : signature * nat) iStorage =
  if not (isConfirmed iStorage.mState)
  then Current.failwith "Payment is only possible in Confirmed state."
  else
    let lSender = Current.sender () in
    let lSeller = keyh_to_addr iStorage.mSeller in
    if lSender <> lSeller
    then Current.failwith "receivePayment can only be called by seller."
    else
      let lSig = iParam.(0) in
      let lNonce = iParam.(1) in
      let lContractAddr = Contract.address (Contract.self ()) in
      let lData = Crypto.blake2b (Bytes.pack (lContractAddr, iStorage.mQuantity, lNonce)) in
      if not (Crypto.check iStorage.mBuyerPKey lSig lData)
      then Current.failwith "Signature not checked."
      else
        let lOps = transferAmounts iStorage in
        (lOps, setState "Succeeded" iStorage)
