module Server

open Saturn
open FSharp.Control.Tasks
open Giraffe

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let serverTestResults = Shared.fableOpticsTest()
            return! json serverTestResults next ctx
        })
}

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
        use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    }

run app
