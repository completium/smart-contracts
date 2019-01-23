[%%version 0.5]

let[@inline] empty_operations = ([] : operation list)

type state =
  | Created
  | InProgress
  | Done

type storage = {
  state             : state;
  organiser         : address;
  organiser_tmp     : address;
  oracle            : address;
  start_date        : timestamp;
  deadline          : timestamp;
  prize             : nat;
  submissions       : (address * nat * timestamp) list;
}

let sort_list cmp l =
  let rec insert v l =
    match l with
    | [] -> [ v ]
    | e::t -> if (cmp e v) then v::e::l else e::(insert v t) in
  let rec sort l =
    match l with
    | [] -> []
    | e::t -> insert e (sort t) in
    sort l

let nat_sub (a : nat) (b : nat) =
  match%nat a - b with
  | Plus v -> v
  | Minus _ -> Current.failwith "nat_sub result is not a nat."

let[@inline] nth l idx =
  if idx < 0 then Current.failwith "Error nth.";
  let rec nth_aux l idx =
    match l with
    | [] -> Current.failwith "Error nth."
    | e::q -> if (idx = 0) then e else nth_aux q (idx - 1) in
  nth_aux l idx

let check_state_created s =
  match s with
  | Created -> ()
  | _ -> Current.failwith ("Wrong state, state Created expected.")

let check_state_in_progress s =
  match s with
  | InProgress -> ()
  | _ -> Current.failwith ("Wrong state, state InProgress expected.")

let%init storage (params : address * address * nat) =
  let organiser = params.(0) in
  let oracle    = params.(1) in
  let prize     = params.(2) in

  let max_timestamp = 2999-12-31T23:59:59+00:00 in
  {
    state            = Created;
    organiser        = organiser;
    organiser_tmp    = organiser;
    oracle           = oracle;
    start_date       = max_timestamp;
    deadline         = max_timestamp;
    prize            = prize;
    submissions      = [];
  }

let%entry confirm (_params : unit) storage =
  let now = Current.time() in

  check_state_created storage.state;

  if (not (now > storage.start_date))
  then Current.failwith ("Wrong condition.");

  let storage = storage.state <- InProgress in

  (empty_operations, storage)


let%entry submit (params : address * nat * timestamp) storage =
  let caller = Current.sender () in

  if (caller <> storage.oracle)
  then (Current.failwith ("Caller is not allowed to call this function."));

  check_state_in_progress storage.state;

  let competitor = params.(0) in
  let score      = params.(1) in
  let date       = params.(2) in

  let storage = storage.submissions <- (competitor, score, date)::storage.submissions in

  (empty_operations, storage)



let%entry decide (params : address * nat * timestamp) storage =
  let now = Current.time() in

  let length = List.length storage.submissions in

  let cmp = fun a b ->
    if a.(1) = b(1)
    then a.(2) > b.(2)
    else a.(1) > b.(1) in
  let sorted_submissions = sort_list cmp storage.submissions in

  let operations =
    if (length >= (3 : nat))
    then (
      let first  = nth sorted_submissions 0 in
      let second = nth sorted_submissions 1 in
      let third  = nth sorted_submissions 2 in

      [Account.transfer ~dest:first.(0)  ~amount:(0.5 * storage.prize);
       Account.transfer ~dest:second.(0) ~amount:(0.3 * storage.prize);
       Account.transfer ~dest:third.(0)  ~amount:(0.2 * storage.prize) ]
    )
    else (
      if (length = (2 : nat))
      then (
        let first  = nth sorted_submissions 0 in
        let second = nth sorted_submissions 1 in

        [ Account.transfer ~dest:first.(0)  ~amount:(0.6 * storage.prize);
          Account.transfer ~dest:second.(0) ~amount:(0.4 * storage.prize) ]
      )
      else (
        if (length = (1 : nat))
        then (
          let first = nth sorted_submissions 0 in
          [ Account.transfer ~dest:first.(0) ~amount:(storage.prize) ]
        )
        else []
      )
    ) in

  let storage = storage.state <- Done in

  (operations, storage)



let%entry assign_new_organiser (params : address) storage =
  let caller = Current.sender () in

  if (caller <> storage.organiser)
  then (Current.failwith ("Caller is not allowed to call this function."));

  let storage = storage.organiser_tmp <- params in
  (empty_operations, storage)



let%entry confirm_organiser (_param : unit) storage =
  let caller = Current.sender () in

  if (caller <> storage.organiser_tmp)
  then (Current.failwith ("Caller is not allowed to call this function."));

  let storage = storage.organiser <- storage.organiser_tmp in
  (empty_operations, storage)



let%entry set_start_date (param : timestamp) storage =
  let caller = Current.sender () in

  if (caller <> storage.organiser)
  then (Current.failwith ("Caller is not allowed to call this function."));

  check_state_created storage.state;

  let value = param in

  let storage = storage.start_date <- value in

  if (not (storage.start_date < storage.deadline))
  then Current.failwith ("Cannot set this value");

  (empty_operations, storage)



let%entry set_deadline (param : timestamp) storage =
  let caller = Current.sender () in

  if (caller <> storage.organiser)
  then (Current.failwith ("Caller is not allowed to call this function."));

  check_state_created storage.state;

  let value = param in

  let storage = storage.deadline <- value in

  if (not (storage.start_date < storage.deadline))
  then Current.failwith ("Cannot set this value");

  (empty_operations, storage)
