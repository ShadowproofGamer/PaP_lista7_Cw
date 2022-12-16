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
    type 'a t = {mutable front : int; mutable rear : int; mutable size : int; mutable arr : 'a option array }
    exception Empty of string
    exception Full of string

    let empty(s) = {front=0; rear=0; size = s; arr = Array.make s None}

    let enqueue(elem, q) =
        if q.front = q.rear && q.arr.(q.front) != None then 
            raise (Full "module QueueCyclicArrayMutable full! Couldn't enqueue.")
        else
            q.arr.(q.rear) <- Some elem;
            q.rear <- ((q.rear + 1) mod q.size)
     
    let dequeue(q) =
        q.arr.(q.front) <- None;
        q.front <- ((q.front + 1) mod q.size)
        
    let first(q) = 
        match q.arr.(q.front) with
        | Some e -> e
        | None -> raise (Empty "module QueueCyclicArrayMutable empty! Couldn't get first.")

    let isEmpty(q) = q.arr.(q.front) = None
    let isFull(q) = q.front = q.rear && q.arr.(q.front) != None
end;;



(*testy QueueCyclicArrayMutable*)
let qu = QueueCyclicArrayMutable.empty(3);;
QueueCyclicArrayMutable.isEmpty(qu);;
QueueCyclicArrayMutable.enqueue(3,qu);;
QueueCyclicArrayMutable.enqueue(4,qu);;
QueueCyclicArrayMutable.enqueue(5,qu);;
QueueCyclicArrayMutable.isFull(qu);;
QueueCyclicArrayMutable.enqueue(2,qu);;

QueueCyclicArrayMutable.dequeue(qu);;
QueueCyclicArrayMutable.dequeue(qu);;
QueueCyclicArrayMutable.dequeue(qu);;
QueueCyclicArrayMutable.isEmpty(qu);;
QueueCyclicArrayMutable.dequeue(qu);;
QueueCyclicArrayMutable.isFull(qu);;
QueueCyclicArrayMutable.isEmpty(qu);;
