[%%version 0.4]

type term_state =
  | Aborted         (* transaction aborted before confirmation  *)
  | Succeded        (* quantity transferred to seller *)
  | Failed          (* quantity returned to buyer *)

type state =
  | Created                    (* initial state *)
  | Confirmed                  (* confirmed, cannot be aborted *)
  | Terminated of term_state

type penalty_to_state =
  | Transferred           (* penalty recipient is set*)
  | Transferring          (* penalty recipeint awaits confirmation *)

type storage = {
    mBuyer        : address;          (* transfer quantity from *)
    mSeller       : address;          (* transfer quantity to *)
    mPenaltyTo    : address;          (* tranfer penalty to; defaulted to buyer *)
    mQuantity     : nat;              (* exchanged quantity *)
    mPenalty      : nat;              (* seller's penalty *)
    mIncentive    : nat;              (* buyer's penalty *)
    mState        : state;            (* contract's state *)
    mConfDate     : timestamp option; (* start of confirmation state *)
    mTermDate     : timestamp option; (* end of confirmation phase *)
  }

let[@inline] gEmptyOp = ([] : operation list)

let%init storage
      (iSeller    : address)
      (iPenaltyTo : address option)
      (iQuantity  : nat)
      (iPenalty   : nat)
      (iIncentive : nat)
      (iConfDate  : timestamp option)
      (iTermDate  : timestamp option) =
  (* It is assumed that contract is initiated by buyer *)
  let lBuyer = Current.sender () in
  (* iPenaltyTo's address is defaulted to buyer's address *)
  let lPenaltyTo =
    match iPenaltyTo with
    | Some a -> a
    | None -> lBuyer
  in
  {
    mBuyer      = lBuyer;
    mSeller     = iSeller;
    mPenaltyTo  = lPenaltyTo;
    mQuantity   = iQuantity;
    mPenalty    = iPenalty;
    mIncentive  = iIncentive;
    mState      = Created;
    mConfDate   = iConfDate;
    mTermDate   = iTermDate;
  }

(*
    State transitions
 *)

let isCreated (iState : state) =
  match iState with
  | Created -> true
  | _ -> false

let isConfirmed (iState : state) =
  match iState with
  | Confirmed -> true
  | _ -> false

(* Created to Aborted *)
let%entry abort (iParam : unit) iStorage =
  if isCreated iStorage.mState
  then
    let lStorage = {
        mBuyer      = iStorage.mBuyer;
        mSeller     = iStorage.mSeller;
        mPenaltyTo  = iStorage.mPenaltyTo;
        mQuantity   = iStorage.mQuantity;
        mPenalty    = iStorage.mPenalty;
        mIncentive  = iStorage.mIncentive;
        mState      = Terminated Aborted;
        mConfDate   = iStorage.mConfDate;
        mTermDate   = iStorage.mTermDate;
      } in
    (gEmptyOp, lStorage)
  else
    Current.failwith "Abortion is possible only in Created state."
