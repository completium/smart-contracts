[%%version 0.4]

let[@inline] gEmptyOp = ([] : operation list)


type storage = {
    mTotal           : nat;
    mTokenOwners     : (nat, address) map;
    mOwnerCounter    : (address, nat) map;
    mTokenApprovals  : (nat, address) map;
    mOwnerOperators  : (address, address) map;
}

let%init storage (iTotal : nat) (iOwner : address) =
  let rec f (iParam : nat * ((nat, address) map)) : ((nat, address) map) =
  let lCounter = iParam.(0) in
  let lMap = iParam.(1) in
  if lCounter = 0p
  then lMap
  else
  (
  let i = (lCounter - 1p) in
  let n = match%nat i with
  | Plus x -> x
  | Minus x -> 0p in
  f (n, (Map.add lCounter iOwner lMap))) in
  let lTotalTokens = f (iTotal, (Map : (nat, address) map)) in
  let lOwnerCounter = Map.add iOwner iTotal (Map : (address, nat) map) in
  {
    mTotal           = iTotal;
    mTokenOwners     = lTotalTokens;
    mOwnerCounter    = lOwnerCounter;
    mTokenApprovals  = (Map : (nat, address) map);
    mOwnerOperators  = (Map : (address, address) map);
  }

let nat_sub (a : nat) (b : nat) =
  match%nat a - b with
  | Plus v -> v
  | Minus _ -> Current.failwith "nat_sub result is not a nat."

let balanceOf (iOwner : address) (iStorage : storage) =
  match Map.find iOwner iStorage.mOwnerCounter with
  | Some b -> b
  | None -> 0p

let ownerOf (iTokenId : nat) (iStorage : storage) =
  match Map.find iTokenId iStorage.mTokenOwners with
  | Some b -> b
  | None -> Current.failwith ("Token id not found.")

let%entry safeTransferFrom (iParam : address * address * nat) iStorage =
  let lFrom    = iParam.(0) in
  let lTo      = iParam.(1) in
  let lTokenId = iParam.(2) in
  let lStorage = iStorage in
  let c = Contract.sender in
  let p = "hello" in
  let a = 1tz in
  Contract.call ~dest:c ~entry:main ~parameter:p ~amount:a;
  (gEmptyOp, lStorage)

let%entry transferFrom (iParam : address * address * nat) iStorage =
  let lFrom    = iParam.(0) in
  let lTo      = iParam.(1) in
  let lTokenId = iParam.(2) in

  let lCFrom = balanceOf lFrom in
  let lCTo   = balanceOf lTo in

  let lNewCFrom = nat_sub lCFrom 1p in
  let lNewCTo   = lCTo + 1p in

  Map.update lTokenId lTo       iStorage.mTokenOwners;
  Map.update lFrom    lNewCFrom iStorage.mOwnerCounter;
  Map.update lTo      lNewCTo   iStorage.mOwnerCounter;

  Map.remove lTokenId iStorage.mTokenApprovals;

  (gEmptyOp, iStorage)

let%entry approve (iParam : address * nat) (iStorage : storage) =
  let lApproved  = iParam.(0) in
  let lTokenId   = iParam.(1) in
  let lSender    = Current.sender () in

  Map.update lTokenId lApproved iStorage.mTokenApprovals;

  (gEmptyOp, iStorage)

let%entry setApprovalForAll (iParam : address * bool) iStorage =
  let lOperator  = iParam.(0) in
  let lApproved  = iParam.(1) in
  let lStorage = iStorage in
  (gEmptyOp, lStorage)

let getApproved (iTokenId : nat) iStorage = ()

let isApprovedForAll (iOwner : address) (iOperator : address) iStorage = ()
