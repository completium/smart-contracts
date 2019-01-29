[%%version 0.5]

let[@inline] empty_operations = ([] : operation list)

type state =
  | Created
  | Confirmed
  | Transferred


type storage = {
  state    : state;
  issuer   : address;
  owner    : address;
  price    : tez;
  payment  : tez;
  maturity : timestamp;
}
(*
let%init storage (params : address * address * tez * tez) =
  {
    state    = Created;
    issuer   = params.(0);
    owner    = params.(1);
    price    = params.(2);
    payment  = params.(3);
    maturity = 2000-01-01;
  }
*)

let%entry confirm (_params : unit) storage =
  let caller = Current.sender () in
  let transferred = Current.amount() in
  let ops = empty_operations in

  if (not (transferred = storage.price))
  then Current.failwith "confirm: called by fails.";

  let ops = (Account.transfer ~dest:storage.issuer ~amount:(storage.prize))::ops in
  let storage = storage.state <- Create in

  (ops, storage)

let%entry repay (_params : unit) storage =
  let caller = Current.sender () in
  let transferred = Current.amount() in

  if (not (caller = storage.issuer))
  then Current.failwith "repay: called by fails.";

  if (not (transferred = storage.payment))
  then Current.failwith "repay: condition 1 fails.";

  (ops, storage)

let%entry collect (_params : unit) storage =
  let caller = Current.sender () in
  let now = Current.time() in
  let balance = Current.balance() in
  let transferred = Current.amount() in
  let ops = empty_operations in

  if (not (caller = storage.issuer))
  then Current.failwith "collect: called by fails.";

  if (not (transferred = storage.payment))
  then Current.failwith "collect: condition 1 fails.";

  if (balance >= storage.payment)
  then (
     let ops = (Account.transfer ~dest:storage.issuer ~amount:(storage.prize))::ops in
     let storage = storage.state <- Transferred in
     (ops, storage))
  else (ops, storage)
