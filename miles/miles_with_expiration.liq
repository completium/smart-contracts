[%%version 0.5]

let[@inline] empty_operations = ([] : operation list)

type mile = {
  id         : string;
  amount     : nat;
  expiration : timestamp;
}

type owner = {
  addr  : address;
  miles : mile list;
}

type storage = {
  admin  : address;
  owners : (address, owner) map
}

let nat_sub (a : nat) (b : nat) =
  match%nat a - b with
  | Plus v -> v
  | Minus _ -> Current.failwith "nat_sub result is not a nat."

let%entry add (iParam : address * string * nat * timestamp) storage =
let caller     = Current.sender () in

let owner      = iParam.(0) in
let id         = iParam.(1) in
let amount     = iParam.(2) in
let expiration = iParam.(3) in

if (caller <> storage.admin)
then (Current.failwith ("Caller is not allowed to call this function."));

let storage, owner_asset =
  match Map.find owner storage.owners with
    | Some o -> storage, o
    | None -> (
                let new_owner = {addr = owner; miles = []} in
                let storage = storage.owners <- Map.update owner (Some new_owner ) storage.owners in
                storage, new_owner
              ) in

List.iter (fun mile ->
      if mile.id = id then Current.failwith ("Mile already exists.")
    ) owner_asset.miles;


let new_mile = {id = id; amount = amount; expiration = expiration} in

let _, miles = List.fold (fun (elt, (c, l)) ->
        if c && elt.expiration > expiration
        then (false, new_mile::elt::l)
        else (c, elt::l)
    ) owner_asset.miles (true, []) in

let owner_asset = owner_asset.miles <- miles in

let storage = storage.owners <- Map.update owner (Some owner_asset) storage.owners in

(empty_operations, storage)


let%entry consume (iParam : address * nat) storage =
let caller = Current.sender () in
let now    = Current.time() in

if (caller <> storage.admin)
then (Current.failwith ("Caller is not allowed to call this function."));

let owner  = iParam.(0) in
let amount = iParam.(1) in

let owner_asset =
  match Map.find owner storage.owners with
    | Some o -> o
    | None -> Current.failwith ("Owner not found.")  in

let available_miles_value = List.fold (fun (elt, a) ->
        if elt.expiration > now
        then a + elt.amount
        else a
    ) owner_asset.miles (0 : nat)
  in

if (available_miles_value < amount)
then Current.failwith ("Not enough miles.");

let available_miles = List.fold (fun (elt, acc) ->
        if elt.expiration > now
        then elt::acc
        else acc
    ) owner_asset.miles []
  in

let _remainder, miles = List.fold (fun (elt, (remainder, l)) ->
    if (remainder = (0 : nat))
    then (remainder, elt::l)
    else if (remainder > elt.amount)
         then (
           let new_elt = elt.amount <- nat_sub elt.amount remainder in
           ((0 : nat), new_elt::l)
         )
         else if (elt.amount = remainder)
              then ((0 : nat), l)
              else (nat_sub remainder elt.amount, l)
    ) available_miles (amount, []) in

(* assert (_remainder = 0) *)

let owner_asset = owner_asset.miles <- miles in

let storage = storage.owners <- Map.update owner (Some owner_asset) storage.owners in

(empty_operations, storage)
