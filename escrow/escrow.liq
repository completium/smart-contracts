[%%version 0.5]

(* ************************************************************************************
    Tools
   ************************************************************************************)

let[@inline] gEmptyOp = ([] : operation list)

let[@inline] keyh_to_addr (iKeyh : key_hash) = Contract.address (Account.default iKeyh)

(* ************************************************************************************
    Storage
   ************************************************************************************)

type state =
  | TransferringPenalty                     (* waiting for penalty transfer from seller *)
  | Created                                 (* initial state                            *)
  | Confirmed                               (* confirmed, cannot be aborted             *)
  | Aborted                                 (* transaction aborted before confirmation  *)
  | Succeeded                               (* quantity transferred to seller           *)
  | Failed                                  (* quantity returned to buyer, penalty lost *)

type storage = {
    mBuyer          : key_hash;             (* transfer quantity from                   *)
    mBuyerPKey      : key;                  (* buyer's key                              *)
    mSeller         : key_hash;             (* transfer quantity to                     *)
    mPenaltyTo      : key_hash;             (* tranfer penalty to; defaulted to buyer   *)
    mQuantity       : tez;                  (* exchanged quantity                       *)
    mPenalty        : tez;                  (* seller's penalty                         *)
    mIncentive      : tez;                  (* buyer's penalty                          *)
    mState          : state;                (* contract's state                         *)
    mDeadline       : timestamp option      (* can trigger failContract after deadline  *)
  }

let%init storage
      (iBuyer     : key_hash)
      (iBuyerPKey : key)
      (iSeller    : key_hash)
      (iPenaltyTo : key_hash option)
      (iQuantity  : tez)
      (iPenalty   : tez)
      (iIncentive : tez)
      (iDeadline  : timestamp option) =
  (* iPenaltyTo's address is defaulted to buyer's address *)
  let lPenaltyTo =
    match iPenaltyTo with
    | Some a -> a
    | None -> iSeller
  in
  let lState =
    if iPenalty = 0tz
    then Created
    else TransferringPenalty
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
    mDeadline       = iDeadline;
  }

(* ************************************************************************************
    State utilities
   ************************************************************************************)

let isTransferring (iStorage : storage) =
  match iStorage.mState with
  | TransferringPenalty -> ()
  | _ -> Current.failwith "State should be 'TransferringPenalty'."

let isCreated (iStorage : storage) =
  match iStorage.mState with
  | Created -> ()
  | _ -> Current.failwith "State should be 'Created'."

let isConfirmed (iStorage : storage) =
  match iStorage.mState with
  | Confirmed -> ()
  | _ -> Current.failwith "State should be 'Confirmed'."

let isTransferringPenaltyOrCreated (iStorage : storage) =
  match iStorage.mState with
  | TransferringPenalty -> ()
  | Created             -> ()
  | _ -> Current.failwith "State should be 'TransferringPenalty' or 'Created'."

let setState ((iState : state), (iStorage : storage)) = iStorage.mState <- iState

(* ************************************************************************************
    Roles checking
   ************************************************************************************)

let isSeller (iStorage : storage) =
  let lSender = Current.sender () in
  let lSeller = keyh_to_addr iStorage.mSeller in
  if lSender <> lSeller
  then Current.failwith "Can only be called by seller."

let isBuyer (iStorage : storage) =
  let lSender = Current.sender () in
  let lBuyer = keyh_to_addr iStorage.mBuyer in
  if lSender <> lBuyer
  then Current.failwith "Can only be called by buyer."

(* ************************************************************************************
    Seller's penalty initialisation
   ************************************************************************************)

(* asserts that amount of tez in call is equal to amount specified at
   contract creation *)
let checkPenalty (iStorage : storage) =
  let lAmount = Current.amount () in
  if lAmount > iStorage.mPenalty
  then failwith "Too much tez for penalty."
  else if lAmount < iStorage.mPenalty
       then failwith "Not enough tez for penalty."

let%entry transferPenalty (iParam : unit) iStorage =
  (* Pre condition(s) ******************************************)
  isTransferring iStorage;
  isSeller iStorage;
  checkPenalty iStorage;
  (* Actions ***************************************************)
  (gEmptyOp, setState (Created, iStorage))

(* ************************************************************************************
    Abort contract
   ************************************************************************************)

let transferBack (iStorage : storage) =
  (* transfer quantity back to buyer *)
  let lBuyer = iStorage.mBuyer in
  let lTotal = iStorage.mQuantity + iStorage.mIncentive in
  let lOps = [Account.transfer ~dest:lBuyer ~amount:lTotal] in
  (* transfer penalty back to seller *)
  let lOps =
    if iStorage.mPenalty > 0tz
    then
      let lSeller = iStorage.mSeller in
      (Account.transfer ~dest:lSeller ~amount:(iStorage.mPenalty))::lOps
    else lOps in
  lOps

let%entry abortContract (iParam : unit) iStorage =
  (* Pre condition(s) ******************************************)
  isTransferringPenaltyOrCreated iStorage;
  (* Actions ***************************************************)
  (transferBack iStorage, setState (Aborted, iStorage))

(* ************************************************************************************
    Confirm contract
   ************************************************************************************)

(* Checks that total amount of tez owned by the contract is equal to amount
   specified at contract creation *)
let checkBalance (iStorage : storage) =
  let lBalance = Current.balance () in
  let lTotal = iStorage.mIncentive + iStorage.mQuantity + iStorage.mPenalty in
  if lBalance > lTotal
  then failwith "Too much tez sent to contract."
  else if lBalance < lTotal
  then failwith "Not enough tez sent to contract."
  else ()

let%entry confirmContract (iParam : unit) iStorage =
  (* Pre condition(s) ******************************************)
  isCreated iStorage;
  isSeller iStorage;
  checkBalance iStorage;
  (* Actions ***************************************************)
  (gEmptyOp, setState (Confirmed, iStorage))

(* ************************************************************************************
    Success payment operations
   ************************************************************************************)

let transferAmounts (iStorage : storage) =
  (* transfer quantity and penalty to seller *)
  let lSeller = iStorage.mSeller in
  let lAmount = iStorage.mQuantity + iStorage.mPenalty in
  let lOps = [Account.transfer ~dest:lSeller ~amount:lAmount] in
  (* transfer incentive back to buyer *)
  let lOps =
    if iStorage.mIncentive > 0tz
    then
      let lBuyer = iStorage.mBuyer in
      (Account.transfer ~dest:lBuyer ~amount:(iStorage.mIncentive))::lOps
    else lOps in
  lOps

(* ************************************************************************************
    Transfer payment
   ************************************************************************************)

let%entry transferPayment (iParam : unit) iStorage =
  (* Pre condition(s) ******************************************)
  isConfirmed iStorage;
  isBuyer iStorage;
  (* Actions ***************************************************)
  (transferAmounts iStorage, setState (Succeeded, iStorage))

(* ************************************************************************************
    Receive payment
   ************************************************************************************)

let checkSignature ((iParam : signature * nat), (iStorage : storage)) =
  let lSig   = iParam.(0) in
  let lNonce = iParam.(1) in
  let lContractAddr = Contract.address (Contract.self ()) in
  (* Shall we include buyer address in data ? *)
  (* Shall we store used nonce ? (see double-spend attack) *)
  let lData = (lContractAddr, iStorage.mQuantity, lNonce) in
  let lHData = Crypto.blake2b (Bytes.pack lData) in
  if not (Crypto.check iStorage.mBuyerPKey lSig lHData)
  then Current.failwith "Signature not checked."

(* checkSignature makes data from:
 - contract's address
 - tez quantity
 - nonce *)
let%entry receivePayment (iParam : signature * nat) iStorage =
  (* Pre condition(s) ******************************************)
  isConfirmed iStorage;
  isSeller iStorage;
  checkSignature (iParam, iStorage);
  (* Actions ***************************************************)
  (transferAmounts iStorage, setState (Succeeded, iStorage))

(* ************************************************************************************
    Fail contract
   ************************************************************************************)

let isAfterDeadline (iStorage : storage) =
  match iStorage.mDeadline with
  | Some t ->
     if Current.time () < t
     then failwith "Cannot fail before deadline."
  | None -> ()

let transferFailedAmounts (iStorage : storage) =
  (* transfer quantity back to buyer *)
  let lBuyer = iStorage.mBuyer in
  let lTotal = iStorage.mQuantity + iStorage.mIncentive in
  let lOps = [Account.transfer ~dest:lBuyer ~amount:lTotal] in
  (* transfer penalty to penaltyTo address *)
  let lOps =
    if iStorage.mPenalty > 0tz
    then
      let lPenaltyTo = iStorage.mPenaltyTo in
      (Account.transfer ~dest:lPenaltyTo ~amount:(iStorage.mPenalty))::lOps
    else lOps in
  lOps

let%entry failContract (iParam : unit) iStorage =
  (* Pre condition(s) ******************************************)
  isConfirmed iStorage;
  isBuyer iStorage;
  isAfterDeadline iStorage;
  (* Actions ***************************************************)
  (transferFailedAmounts iStorage, setState (Failed, iStorage))
