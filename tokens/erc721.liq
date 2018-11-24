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
  {
    mTotal           = iTotal;
    mTokenOwners     = lTotalTokens;
    mOwnerCounter    = (Map : (address, nat) map);
    mTokenApprovals  = (Map : (nat, address) map);
    mOwnerOperators  = (Map : (address, address) map);
  }


let balanceOf (iOwner : address) (iStorage : storage) =
  match Map.find iOwner iStorage.mOwnerCounter with
  | Some b -> b
  | None -> Current.failwith ("Owner not found.")

let ownerOf (iTokenId : nat) (iStorage : storage) =
  match Map.find iTokenId iStorage.mTokenOwners with
  | Some b -> b
  | None -> Current.failwith ("Token id not found.")

let%entry safeTransferFrom (iParam : address * address * nat) iStorage =
  let lFrom  = iParam.(0) in
  let lTo    = iParam.(1) in
  let lQty   = iParam.(2) in
  let lStorage = iStorage in
  (gEmptyOp, lStorage)

let%entry transferFrom (iParam : address * address * nat) iStorage =
  let lFrom  = iParam.(0) in
  let lTo    = iParam.(1) in
  let lQty   = iParam.(2) in
  let lStorage = iStorage in
  (gEmptyOp, lStorage)

let%entry approve (iParam : address * nat) iStorage =
  let lApproved  = iParam.(0) in
  let lTokenId   = iParam.(1) in
  let lStorage = iStorage in
  (gEmptyOp, lStorage)

let%entry setApprovalForAll (iParam : address * bool) iStorage =
  let lOperator  = iParam.(0) in
  let lApproved  = iParam.(1) in
  let lStorage = iStorage in
  (gEmptyOp, lStorage)

let getApproved (iTokenId : nat) iStorage = ()

let isApprovedForAll (iOwner : address) (iOperator : address) iStorage = ()
