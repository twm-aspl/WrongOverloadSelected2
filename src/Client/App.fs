module App

open Elmish
open Elmish.React
open Fable.React
open Thoth.Fetch
open Shared

type Model = {
    ServerResult : TestResults option
    ClientResult : TestResults
}

type Msg =
    | ServerResultLoaded of TestResults

let getServerResult () = Fetch.fetchAs<unit, TestResults> "/api/init"

let init () : Model * Cmd<Msg> =
    let initialModel = {
        ServerResult = None
        ClientResult = fableOpticsTest()
    }
    let loadCountCmd =
        Cmd.OfPromise.perform getServerResult () ServerResultLoaded
    initialModel, loadCountCmd

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with
    | ServerResultLoaded res ->
        { currentModel with ServerResult = Some res }, Cmd.none

let printResult ((test1, test2, test3, test4) : TestResults) =
    let toString o =
        match o with
        | Some v -> sprintf "Some %A" v
        | None -> sprintf "None"
    let inline pf x = (toString >> str) x

    div [] [
        p [] [ pf test1 ]
        p [] [ pf test2 ]
        p [] [ pf test3 ]
        p [] [ pf test4 ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
          h1 [] [ str "Client Result" ]
          printResult model.ClientResult
          h1 [] [ str "Server Result" ]
          match model.ServerResult with
          | Some r -> printResult r
          | None -> str "Loading..."
    ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
