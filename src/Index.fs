module Index

open Elmish
open Feliz
open Browser

open Mainloop

type State = { Incr:int }
type Msg =
    | Update

let init () =
    let state =
        {
            Incr = 0
        }
    state, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | Update ->
        let state =
            { state with
                Incr = state.Incr + 1 }
        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        Columns.columns [] [
            Column.column [
            ] [
                Html.canvas [
                    prop.style [
                        Feliz.style.border(1, borderStyle.solid, "red")
                    ]
                    let w, h = 556 - 10, 264 - 10
                    prop.width w
                    prop.height h

                    prop.tabIndex -1
                    prop.ref (fun canvas ->
                        if isNull canvas then ()
                        else
                            let canvas = canvas :?> Types.HTMLCanvasElement

                            let duckSprite = document.getElementById("duck") :?> Browser.Types.HTMLImageElement
                            let foxSprite = document.getElementById("fox") :?> Browser.Types.HTMLImageElement
                            let x = FoxEscape.start duckSprite foxSprite canvas
                            mainloop.setUpdate (fun _ -> x.Update ()) |> ignore
                            mainloop.setDraw (fun _ -> x.Draw ()) |> ignore
                            mainloop.setEnd (fun fps panic ->
                                // TODO: fpsCounter.textContent <- sprintf "%A FPS" (round fps)
                                if panic then
                                    let discardedTime = round(mainloop.resetFrameDelta())
                                    printfn "Main loop panicked, probably because the browser tab was put in the background. Discarding %A ms" discardedTime
                            ) |> ignore
                            mainloop.start () |> ignore
                    )
                ]
            ]
            Column.column [
            ] [
                Column.column [] [
                    Button.button [
                        Button.OnClick (fun e ->
                            Update
                            |> dispatch
                        )
                    ] [
                        str "update"
                    ]
                ]
            ]
        ]
    ]

let navBrand =
    Navbar.Brand.div [] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/fable.ico"
                Alt "Logo"
            ]
        ]
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.IsFullHeight
    ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [
                    navBrand
                ]
            ]
        ]

        Hero.body [] [
            Container.container [] [
                Column.column [
                    // Column.Width (Screen.All, Column.Is6)
                    // Column.Offset (Screen.All, Column.Is3)
                ] [
                    // Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "FoxEscape" ]
                    containerBox state dispatch
                ]
            ]
        ]
    ]
