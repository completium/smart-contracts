[%%version 0.5]

let[@inline] empty_operations = ([] : operation list)

type storage = string

let%init storage (param : string) = param

let%entry write (param : string) storage =
let storage = param in
(empty_operations, storage)
