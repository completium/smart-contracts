[%%version 1.01]

let[@inline] empty_operations = ([] : operation list)

let[@inline] keyh_to_addr (iKeyh : key_hash) = Contract.address (Account.default iKeyh)

type certificate = {
  description : string;
  formula : string;
  url : string;
  hash : string;
}

type storage =
  {
    admin            : key_hash;
    certicates       : (string, certificate) map;
  }

let%init storage = {
  admin      = tz1aazS5ms5cbGkb6FN1wvWmN7yrMTTcr6wB;
  certicates = (Map : (string, certificate) map);
}

let%entry add (p : string * string * string * string * string) s =
  let caller = Current.sender () in
  if (not (caller = (keyh_to_addr s.admin)))
  then Current.failwith "caller is not authorized to call this entry";
  let id = get p 0 in
  let description = get p 1 in
  let formula = get p 2 in
  let url = get p 3 in
  let hash = get p 4 in
  let certificate = {
    description = description;
    formula = formula;
    url = url;
    hash = hash;
  } in

  let s = s.certicates <- Map.update id (Some certificate) s.certicates in
  (empty_operations, s)
