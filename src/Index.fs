module Index

open Elmish
open Feliz
open Browser

open Mainloop

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type Img = Result<Types.HTMLImageElement, unit>
type ImgState =
    {
        InputImageSrc: string
        ImageSrc: string

        Img:Deferred<Img>
    }

type State =
    {
        FoxImgState: ImgState
        DuckImgState: ImgState
    }
type Msg =
    | UpdateFoxState of Img
    | UpdateDuckState of Img
    | SetFoxImgState of ImgState
    | SetDuckImgState of ImgState

let init () =
    let imgStateEmpty =
        {
            InputImageSrc = ""
            ImageSrc = ""
            Img = HasNotStartedYet
        }
    let state =
        {
            FoxImgState = imgStateEmpty
            DuckImgState = imgStateEmpty
        }
    state, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | UpdateFoxState img ->
        let state =
            { state with
                FoxImgState =
                    { state.FoxImgState with
                        Img = Resolved img } }
        state, Cmd.none
    | UpdateDuckState img ->
        let state =
            { state with
                DuckImgState =
                    { state.DuckImgState with
                        Img = Resolved img } }
        state, Cmd.none
    | SetFoxImgState img ->
        let state =
            { state with
                FoxImgState = img }
        state, Cmd.none
    | SetDuckImgState img ->
        let state =
            { state with
                DuckImgState = img }
        state, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

let spinner =
    div [ Class ("block " + Fa.Classes.Size.Fa3x) ]
        [ Fa.i [ Fa.Solid.Spinner
                 Fa.Spin ]
            []
        ]

let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        let f state update =
            if not <| System.String.IsNullOrEmpty state.ImageSrc then
                Html.img [
                    prop.hidden true
                    // prop.custom("crossOrigin", "anonymous")
                    prop.onLoad (fun e ->
                        if isNull e then ()
                        else
                            let imgElement = e.currentTarget :?> Types.HTMLImageElement
                            dispatch (update (Ok imgElement))
                    )
                    prop.onError (fun e ->
                        dispatch (update (Error ()))

                    )

                    prop.src state.ImageSrc
                ]
                |> Some
            else None

        match f state.FoxImgState UpdateFoxState with
        | Some img -> img
        | None -> ()

        match f state.DuckImgState UpdateDuckState with
        | Some img -> img
        | None -> ()

        Columns.columns [] [
            Column.column [
            ] [
                match state.FoxImgState.Img with
                | Resolved (Ok foxSprite) ->
                    match state.DuckImgState.Img with
                    | Resolved (Ok duckSprite) ->
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
                    | HasNotStartedYet ->
                        Html.div [str "Set the source for the duck image"]
                    | InProgress -> spinner
                    | Resolved (Error ()) ->
                        Html.div [str "Error with duck image"]
                | HasNotStartedYet ->
                    Html.div [str "Set the source for the fox image"]
                | InProgress -> spinner
                | Resolved (Error ()) ->
                    Html.div [str "Error with fox image"]
            ]
            Column.column [
            ] [
                let f description state cmd =
                    let isError = state.Img = Resolved (Error ())
                    Field.div [] [
                        Label.label [] [ str description ]
                        Control.div [] [
                            Field.div [ Field.HasAddons ] [
                                Control.p [
                                    Control.IsExpanded
                                    if isError then
                                        Control.HasIconRight
                                ] [
                                    Input.input [
                                        if isError then
                                            Input.Color IsDanger
                                        Input.Placeholder description
                                        Input.Value state.InputImageSrc
                                        Input.OnChange (fun e ->
                                            { state with
                                                InputImageSrc = e.Value }
                                            |> cmd
                                            |> dispatch
                                        )
                                    ]
                                    if isError then
                                        Icon.icon [ Icon.Size IsSmall; Icon.IsRight ]
                                            [ Fa.i [ Fa.Solid.ExclamationTriangle ] [] ]
                                ]
                                Control.p [] [
                                    Button.button [
                                        Button.Disabled (state.ImageSrc = state.InputImageSrc)
                                        Button.IsLoading (state.Img = InProgress)
                                        Button.OnClick (fun e ->
                                            { state with
                                                ImageSrc = state.InputImageSrc
                                                Img = InProgress }
                                            |> cmd
                                            |> dispatch
                                        )
                                    ] [
                                        Fa.i [ Fa.Solid.Check ] []
                                    ]
                                ]
                            ]
                        ]
                        if isError then
                            Help.help [ Help.Color IsDanger ]
                                [ str "Something wrong with this image" ]
                    ]

                f "Url for the fox image" state.FoxImgState SetFoxImgState
                f "Url for the duck image" state.DuckImgState SetDuckImgState
            ]
        ]
    ]

let navBrand =
    Navbar.Brand.div [] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://gretmn102.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            Fa.i [ Fa.Solid.Home ] []
        ]

        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://github.com/gretmn102/FoxEscape" ]
            Navbar.Item.IsActive true
        ] [
            Fa.i [ Fa.Brand.Github ] []
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
