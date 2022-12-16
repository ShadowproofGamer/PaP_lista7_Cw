(* Zad 1 *)
module type QUEUE_FUN =
sig
 type 'a t
 exception Empty of string
 val empty: unit -> 'a t
 val enqueue: 'a * 'a t -> 'a t
 val dequeue: 'a t -> 'a t
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
end;;

module QueueList : QUEUE_FUN =
struct
    type 'a t = 'a list
    exception Empty of string

    let empty() = []
    let enqueue(e, queue) = queue @ [e]
    let dequeue queue =
        match queue with
        | h::t -> t
        | _ -> []
    let first queue =
        match queue with
        | h::t -> h
        | _ -> raise (Empty "module QueueList empty! Couldn't get first.")
    let isEmpty queue =
        match queue with
        | [] -> true
        | _ -> false
end;;

module QueueListPair : QUEUE_FUN =
struct
    type 'a t = 'a list * 'a list
    exception Empty of string

    let empty() = ([], [])
    let enqueue(e, queue) = 
        match queue with
        | ([], yl) -> (List.rev yl, [e])
        | (xl, yl) -> (xl, e::yl)
    let dequeue queue =
        match queue with
        | ([], []) -> ([], [])
        | ([], yl) -> (List.tl (List.rev yl), [])
        | (xlh::xlt, yl) -> (xlt, yl)
    let first queue =
        match queue with
        | ([], []) -> raise (Empty "module QueueListPair empty! Couldn't get first.")
        | ([], yl) -> List.hd (List.rev yl)
        | (xlh::xlt, _) -> xlh
    let isEmpty queue =
        match queue with
        | ([], []) -> true
        | _ -> false
end;;



(*testy QueueList*)
let qu = QueueList.(enqueue(3, empty()));;
let qu1 = QueueList.(enqueue(4, enqueue(5,qu)));;
QueueList.first qu;;
QueueList.first qu1;;
QueueList.(isEmpty (dequeue qu));;
QueueList.(first (dequeue qu));;
QueueList.(first (dequeue qu1));;

(*testy QueueListPair*)
let qu = QueueListPair.(enqueue(3, empty()));;
let qu1 = QueueListPair.(enqueue(4, enqueue(5,qu)));;
QueueListPair.first qu;;
QueueListPair.first qu1;;
QueueListPair.(isEmpty (dequeue qu));;
QueueListPair.(first (dequeue qu));;
QueueListPair.(first (dequeue qu1));;
