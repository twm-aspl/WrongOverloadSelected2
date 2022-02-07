module Shared

open Optics
open Optics.Operators


type TestResults = int option * int option * int option * decimal option

type Grandchild =
    { x : int option }
    static member x_ : Prism<Grandchild, int> =
        (fun p -> p.x),
        (fun x p ->
            match p with
            | p when p.x.IsSome -> { p with x = Some x }
            | p -> p)

type Child =
    { g : Grandchild }
    static member g_ : Lens<Child, Grandchild> = (fun p -> p.g), (fun x p -> { p with g = x })

type Parent =
    { c : Child option }
    static member c_ : Prism<Parent, Child> =
        (fun p -> p.c),
        (fun x p ->
            match p with
            | p when p.c.IsSome -> { p with c = Some x }
            | p -> p)

let fableOpticsTest () : TestResults =
    let x : Parent = { c = Some { g = { x = Some 5 } } }
    let y : Parent = { c = None }
    let z : Parent = { c = Some { g = { x = None } } }
    let o : Parent option = Some { c = Some { g = { x = None } } }

    let grandchild = Parent.c_ >?> Child.g_ >?> Grandchild.x_
    let test1 = x ^. grandchild
    match test1 with
    | Some x -> printfn "test1: Some %A" x
    | None -> printfn "test1: None"

    let test2 = y ^. grandchild
    match test2 with
    | Some x -> printfn "test2: Some %A" x
    | None -> printfn "test2: None"

    let test3 = z ^. grandchild
    match test3 with
    | Some x -> printfn "test3: Some %A" x
    | None -> printfn "test3: None"

    let test4 = AttemptRepro.AttemptRepro None
    match test4 with
    | Some x -> printfn "test4: Some %A" x
    | None -> printfn "test4: None"

    test1, test2, test3, test4
