[%%version 0.5]

let[@inline] empty_operations = ([] : operation list)

type state =
  | Created
  | Voting
  | Buried


type storage = {
  state            : state;
  chairperson      : address;
  chairperson_tmp  : address;
  start_date       : timestamp;
  end_date         : timestamp;
  voters           : address set;
  voters_has_voted : (address, bool) map;
  ballot           : (string, nat) map;
  winners          : string list;
}

let check_state_created s =
  match s with
  | Created -> ()
  | _ -> Current.failwith ("Wrong state, state Created expected.")

let check_state_voting s =
  match s with
  | Voting -> ()
  | _ -> Current.failwith ("Wrong state, state Voting expected.")

let%init storage (chairperson : address) =
  let max_timestamp = 2999-12-31T23:59:59+00:00 in
  {
    state            = Created;
    chairperson      = chairperson;
    chairperson_tmp  = chairperson;
    start_date       = max_timestamp;
    end_date         = max_timestamp;
    voters           = (Set : address set);
    voters_has_voted = (Map : (address, bool) map);
    ballot           = (Map : (string, nat) map);
    winners          = [];
  }

let%entry register (params : address) storage =
  let caller = Current.sender () in

  if (caller <> storage.chairperson)
  then (Current.failwith ("Caller is not allowed to call this function."));

  check_state_created storage.state;

  let voter = params in

  let storage = storage.voters <- Set.add voter storage.voters in
  let storage = storage.voters_has_voted <- Map.update voter (Some false) storage.voters_has_voted in

  (empty_operations, storage)


let%entry start (_params : unit) storage =
  let now = Current.time() in

  check_state_created storage.state;

  if (now > storage.start_date)
  then (Current.failwith ("Wrong condition."));

  let storage = storage.state <- Voting in

  (empty_operations, storage)



let%entry vote (params : string) storage =
  let caller = Current.sender () in

  check_state_voting storage.state;

  let condition =
    match Map.find caller storage.voters_has_voted with
    | Some v -> v
    | None -> false in

  if (not condition)
  then (Current.failwith ("Condition is not valid."));

  let value = params in

  let storage = storage.voters_has_voted <- Map.update caller (Some true) storage.voters_has_voted in
  let storage = storage.ballot <- Map.update value
    (Some (match Map.find value storage.ballot with
    | Some n -> n + (1 : nat)
    | None   -> (0 : nat))) storage.ballot in

  (empty_operations, storage)



let%entry bury (_params : unit) storage =
  let now = Current.time() in

  check_state_voting storage.state;

  if (now > storage.end_date)
  then (Current.failwith ("Wrong condition."));

  let storage = storage.state <- Buried in

  let nbVoteMax = Map.fold (fun ((_, v), acc) ->
    if (v > acc)
    then v
    else acc
  ) storage.ballot (0 : nat) in

  let winners = Map.fold (fun ((k, v), acc) ->
    if (v = nbVoteMax)
    then k::acc
    else acc
  ) storage.ballot [] in

  let storage = storage.winners <- winners in
  (empty_operations, storage)


let%entry assign_new_chairperson (params : address) storage =
  let caller = Current.sender () in

  if (caller <> storage.chairperson)
  then (Current.failwith ("Caller is not allowed to call this function."));

  let storage = storage.chairperson_tmp <- params in
  (empty_operations, storage)



let%entry confirm_chairperson (_param : unit) storage =
  let caller = Current.sender () in

  if (caller <> storage.chairperson_tmp)
  then (Current.failwith ("Caller is not allowed to call this function."));

  let storage = storage.chairperson <- storage.chairperson_tmp in
  (empty_operations, storage)



let%entry set_start_date (param : timestamp) storage =
  let caller = Current.sender () in

  if (caller <> storage.chairperson)
  then (Current.failwith ("Caller is not allowed to call this function."));

  check_state_created storage.state;

  let value = param in

  let storage = storage.start_date <- value in

  if (not (storage.start_date < storage.end_date))
  then Current.failwith ("Cannot set this value");

  (empty_operations, storage)



let%entry set_end_date (param : timestamp) storage =
  let caller = Current.sender () in

  if (caller <> storage.chairperson)
  then (Current.failwith ("Caller is not allowed to call this function."));

  check_state_created storage.state;

  let value = param in

  let storage = storage.end_date <- value in

  if (not (storage.start_date < storage.end_date))
  then Current.failwith ("Cannot set this value");

  (empty_operations, storage)
