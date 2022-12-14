(* Zad 2*)
module type QUEUE_MUT =
sig
 type 'a t
 exception Empty of string
 exception Full of string 
 val empty: int -> 'a t
 val enqueue: 'a * 'a t -> unit
 val dequeue: 'a t -> unit
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
 val isFull: 'a t -> bool
end;;

module QueueCyclicArrayMutable : QUEUE_MUT =
struct
    type 'a t = {mutable f : int; mutable r : int; mutable size : int; mutable arr : 'a option array }
    exception Empty of string
    exception Full of string

    let empty(s) = {f=0; r=0; size = s; arr = Array.make s None}

    let enqueue(elem, q) =
        if q.f = q.r && q.arr.(q.f) != None then 
            raise (Full "module QueueCyclicArrayMutable: enqueue")
        else
            q.arr.(q.r) <- Some elem;
            q.r <- ((q.r + 1) mod q.size)
     
    let dequeue(q) =
        q.arr.(q.f) <- None;
        q.f <- ((q.f + 1) mod q.size)
        
    let first(q) = 
        match q.arr.(q.f) with
        | Some e -> e
        | None -> raise (Empty "module QueueCyclicArrayMutable: first")

    let isEmpty(q) = q.arr.(q.f) = None
    let isFull(q) = q.f = q.r && q.arr.(q.f) != None
end;;

