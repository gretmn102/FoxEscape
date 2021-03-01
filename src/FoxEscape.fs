module FoxEscape

let width = 800
let height = 650
let radius = 300.0
let mutable fox = 0.0
let mutable duckx = 0.1
let mutable ducky = 0.0
let bspeed = 1.0
let gspeeds = [3.5; 4.0; 4.2; 4.4; 4.6]
let gspeed_ix = 0
let speed_mult = 3.0

type IsWin = bool
let mutable gameOver : IsWin option = None

let restart () =
    fox <- 0.0
    duckx <- 0.1
    ducky <- 0.0
    gameOver <- None

open Browser.Types

module CanvasRenderingContext2D =
    let circle (x, y) r lineWidth strokeStyle fillStyle (ctx:CanvasRenderingContext2D) =
        ctx.beginPath()
        ctx.lineWidth <- lineWidth
        ctx.arc(x, y, r, 0., 2. * System.Math.PI, false)
        match fillStyle with
        | Some fillStyle ->
            ctx.fillStyle <- fillStyle
            ctx.fill()
        | _ -> ()
        match strokeStyle with
        | Some strokeStyle ->
            ctx.strokeStyle <- strokeStyle
            ctx.stroke()
        | _ -> ()

open Fable.Core
let rgb (r:byte, g:byte, b:byte) = U3.Case1 (sprintf "rgb(%d, %d, %d)" r g b)
let clear (ctx:CanvasRenderingContext2D) =
    let radius_mult = bspeed / gspeeds.[gspeed_ix]

    ctx.clearRect(0., 0., float width, float height);

    ctx
    |> CanvasRenderingContext2D.circle (float width/2., float height/2.) (radius * 1.0) 0. (Some (U3.Case1 "green")) None
    // ctx
    // |> CanvasRenderingContext2D.circle (float width/2., float height/2.) (radius*radius_mult) 1. (Some(rgb (200uy, 200uy, 200uy))) None

let redraw (duckSprite:HTMLCanvasElement) (foxSprite:HTMLCanvasElement) ctx =
    clear ctx

    ctx.drawImage(U3.Case2 duckSprite,
                  float width/2. + duckx - duckSprite.width / 2.,
                  float height/2. + ducky - duckSprite.height / 2.)

    ctx.drawImage(U3.Case2 foxSprite,
                  float width/2. + radius * cos fox - foxSprite.width / 2.,
                  float height/2. + radius * sin fox - foxSprite.height / 2.)

    // let duckr = 6.
    // let foxr = 6.
    // ctx
    // |> CanvasRenderingContext2D.circle
    //     (float width/2. + duckx, float height/2. + ducky)
    //     duckr
    //     1.
    //     (Some (U3.Case1 "black"))
    //     None
    // ctx
    // |> CanvasRenderingContext2D.circle
    //     (float width/2. + radius * cos fox, float height/2. + radius * sin fox)
    //     foxr
    //     1.
    //     (Some (U3.Case1 "red"))
    //     None

    match gameOver with
    | Some isWin ->
        ctx.font <- "bold 48px serif"
        let centerX, centerY = float width / 2., float height / 2.

        if isWin then
            ctx.fillStyle <- U3.Case1 "grey"
            let str = "Escaped!"
            let m = ctx.measureText str
            ctx.fillText(str, centerX - m.width / 2.0, centerY)
        else
            let str = "You Were Eaten"
            ctx.fillStyle <- U3.Case1 "red"
            let m = ctx.measureText str
            ctx.fillText(str, centerX - m.width / 2.0, centerY)
    | None -> ()

open Fable.Core.JS

// Задана произвольная точка и окружность через центр и радиус. Прямая проходит через центр окружности и точку. Нужно найти пересечение окружности и прямой... два пересечения.
// Из этого:
// ```maple
// line := y = (a__x*b__y-a__y*b__x+a__y*x-b__y*x)/(-b__x+a__x);
// circleParametric := { x = r*sin(t) - a__x, y = r*cos(t) - a__y }
// t = map(x -> simplify(x, 'size'), [solve(eval(line, circleParametric), t)]);
// ```
// получилось два гигантских уравнения, которые даже приводить сюда не хочется, не то что использовать. Ума не приложу, как можно иначе решить. Да, это аналитическое решение, а численное черт знает, как получить.
// Собственно, а зачем нам задавать центр окружности, если можно сказать, что она лежит в центре координат?
// ```maple
// line := y = x*a__y/a__x
// t = map(x -> simplify(x, 'size'), [solve(eval(line, { x = r*sin(t) - a__x, y = r*cos(t) - a__y }), t)]);
// ```
// Дает одно единственное решение: `t = [arctan(a__x/a__y)]`!
// Отлично, что дальше?

let updateFox radius speed_mult (duckx, ducky) gspeed fox =
    let newang = atan2 ducky duckx // <=> atan (duckx / ducky)
    let diff = newang - fox

    let diff = diff + if diff < System.Math.PI then System.Math.PI * 2. else 0.
    let diff = diff - if diff > System.Math.PI then System.Math.PI * 2. else 0.
    let fox' =
        if abs diff * radius <= gspeed * speed_mult then
            newang
        else
            if diff > 0.0 then
                fox + gspeed * speed_mult / radius
            else
                fox - gspeed * speed_mult / radius
    let fox' = fox' + if fox' < System.Math.PI then System.Math.PI * 2. else 0.
    let fox' = fox' - if fox' > System.Math.PI then System.Math.PI * 2. else 0.
    fox'

let test () =
    let gspeeds = 3.5
    let width, height = 1024, 768
    let duckx, ducky = float width / 2., float height / 2.
    let radius = 300.0
    let speed_mult = 3.0
    let fox = 0.0

    fox
    |> Seq.unfold (fun fox ->
        let x = updateFox radius speed_mult (duckx, ducky) gspeeds fox
        Some(x, x)
        )
    |> Seq.take 100
    |> List.ofSeq

let moveDuck (x, y) =
    let dx, dy = x - duckx, y - ducky
    let mag = sqrt (dx*dx + dy*dy)
    if mag <= bspeed * speed_mult then
        duckx <- x
        ducky <- y
    else
        duckx <- duckx + bspeed * speed_mult * dx/mag
        ducky <- ducky + bspeed * speed_mult * dy/mag

open Browser
open Browser.Dom
let maxSpriteSize = 80.
let start (duckImg:HTMLImageElement) (foxImg:HTMLImageElement) (canvas:HTMLCanvasElement) =
    canvas.width <- float width
    canvas.height <- float height

    let ctx = canvas.getContext_2d()
    let mutable mouseX, mouseY = 0., 0.
    let mutable isMouseButtonDown = false
    // TODO: canvas.ontouchmove
    canvas.onmousemove <- fun e ->
        mouseX <- e.offsetX; mouseY <- e.offsetY

    canvas.onmousedown <- fun _ -> isMouseButtonDown <- true
    canvas.onmouseup <- fun _ -> isMouseButtonDown <- false

    let f (img:HTMLImageElement) =
        let w, h = img.width, img.height
        let ratio =
            if w > h then
                w / maxSpriteSize
            else
                h / maxSpriteSize
        let w, h = w / ratio, h / ratio

        let sprite = document.createElement "canvas" :?> Types.HTMLCanvasElement
        sprite.width <- w
        sprite.height <- h
        let ctx2 = sprite.getContext_2d()
        ctx2.drawImage(U3.Case1 img, 0., 0., w, h)

        sprite

    let duckSprite = f duckImg
    let foxSprite = f foxImg

    {|
        Update = fun () ->
            if Option.isSome gameOver then
                if isMouseButtonDown then
                    restart()
            elif duckx*duckx + ducky*ducky > radius*radius then
                let diff = atan2 ducky duckx - fox
                let diff = diff + if diff < System.Math.PI then System.Math.PI * 2. else 0.
                let diff = diff - if diff > System.Math.PI then System.Math.PI * 2. else 0.

                let is_win = abs(diff) > 0.000001
                gameOver <- Some is_win
                isMouseButtonDown <- false
            else
                if isMouseButtonDown then
                    moveDuck(mouseX - float width / 2., mouseY - float height / 2.)
                fox <- updateFox radius speed_mult (duckx, ducky) gspeeds.[gspeed_ix] fox

        Draw = fun () ->
            redraw duckSprite foxSprite ctx
    |}