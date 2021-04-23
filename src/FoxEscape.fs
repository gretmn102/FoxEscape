module FoxEscape

let mutable width = 800
let mutable height = 650
let mutable radius = 300.0
let mutable fox = 0.0
let mutable duckX = 0.1
let mutable duckY = 0.0
let mutable duckSpeed = 1.0
let mutable foxSpeed = 4.0
let mutable speedMult = 0.20

type IsWin = bool
let mutable gameOver : IsWin option = None

let restart () =
    fox <- 0.0
    duckX <- 0.1
    duckY <- 0.0
    gameOver <- None

open Browser.Types

module CanvasRenderingContext2DExt =
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
    ctx.clearRect(0., 0., float width, float height);

    // let radius_mult = duckSpeed / foxSpeed
    // ctx
    // |> CanvasRenderingContext2DExt.circle (float width/2., float height/2.) (radius*radius_mult) 1. (Some(rgb (200uy, 200uy, 200uy))) None

    ctx
    |> CanvasRenderingContext2DExt.circle (float width/2., float height/2.) (radius * 1.0) 0. (Some (U3.Case1 "green")) None


let mutable maxSpriteSize = 60.

open Browser
open Browser.Dom

let makeSprite isRounded (img:HTMLImageElement) =
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
    let ctx = sprite.getContext_2d()

    let x, y = 0., 0.
    if isRounded then
        ctx.save()

        let w' = w / 2.
        let h' = h / 2.
        ctx.beginPath()
        ctx.arc(x + w', y + h', w', h', 2. * Fable.Core.JS.Math.PI , true)
        ctx.closePath()

        ctx.clip()
        ctx.drawImage(U3.Case1 img, 0., 0., w, h)

        ctx.restore()
    else
        ctx.drawImage(U3.Case1 img, 0., 0., w, h)

    sprite

type T =
    {
        Sprite: HTMLCanvasElement
        IsRounded: bool
        Img: HTMLImageElement
    }
let makeSprite2 isRounded image =
    {
        Sprite = makeSprite isRounded image
        IsRounded = isRounded
        Img = image
    }

let mutable duckSprite = None
let updateDuckSprite isRounded duckImage =
    duckSprite <- Some (makeSprite2 isRounded duckImage)

let mutable foxSprite = None
let updateFoxSprite isRounded foxImage =
    foxSprite <- Some (makeSprite2 isRounded foxImage)

let redraw ctx =
    clear ctx

    match duckSprite with
    | Some duckSprite ->
        let duckSprite = duckSprite.Sprite
        ctx.drawImage(U3.Case2 duckSprite,
                      float width/2. + duckX - duckSprite.width / 2.,
                      float height/2. + duckY - duckSprite.height / 2.)
    | None ->
        let duckr = maxSpriteSize / 2.
        ctx
        |> CanvasRenderingContext2DExt.circle
            (float width/2. + duckX, float height/2. + duckY)
            duckr
            1.
            (Some (U3.Case1 "gray"))
            None

    match foxSprite with
    | Some foxSprite ->
        let foxSprite = foxSprite.Sprite
        ctx.drawImage(U3.Case2 foxSprite,
                      float width/2. + radius * cos fox - foxSprite.width / 2.,
                      float height/2. + radius * sin fox - foxSprite.height / 2.)
    | None ->
        let foxr = maxSpriteSize / 2.
        ctx
        |> CanvasRenderingContext2DExt.circle
            (float width/2. + radius * cos fox, float height/2. + radius * sin fox)
            foxr
            1.
            (Some (U3.Case1 "red"))
            None

    match gameOver with
    | Some isWin ->
        ctx.font <- sprintf "bold %dpx serif" (int (float width * 48. / 520.))
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

let updateFox radius (duckX, duckY) foxSpeed fox delta =
    let newAngle = atan2 duckY duckX // <=> atan (duckx / ducky)
    let diff = newAngle - fox

    let diff = diff + if diff < System.Math.PI then System.Math.PI * 2. else 0.
    let diff = diff - if diff > System.Math.PI then System.Math.PI * 2. else 0.
    let fox' =
        if abs diff * radius <= foxSpeed * delta then
            newAngle
        else
            if diff > 0.0 then
                fox + foxSpeed * delta / radius
            else
                fox - foxSpeed * delta / radius
    let fox' = fox' + if fox' < System.Math.PI then System.Math.PI * 2. else 0.
    let fox' = fox' - if fox' > System.Math.PI then System.Math.PI * 2. else 0.
    fox'

let test () =
    let foxSpeeds = 3.5
    let width, height = 1024, 768
    let duckx, ducky = float width / 2., float height / 2.
    let radius = 300.0
    let speedMult = 3.0
    let fox = 0.0

    fox
    |> Seq.unfold (fun fox ->
        let x = updateFox radius (duckx, ducky) foxSpeeds fox speedMult
        Some(x, x)
        )
    |> Seq.take 100
    |> List.ofSeq

let moveDuck (x, y) speedMult =
    let dx, dy = x - duckX, y - duckY
    let mag = sqrt (dx*dx + dy*dy)
    if mag <= duckSpeed * speedMult then
        duckX <- x
        duckY <- y
    else
        duckX <- duckX + duckSpeed * speedMult * dx/mag
        duckY <- duckY + duckSpeed * speedMult * dy/mag

let updateSize width' height' =
    width <- int width'
    height <- int height'
    // 60.              520.
    // maxSpriteSize    size
    maxSpriteSize <- width' * 60. / 520.
    radius <-
        if width' > height' then
            width' / 2. - (maxSpriteSize / 2.)
        else
            height' / 2. - (maxSpriteSize / 2.)
    let f =
        Option.map (fun x ->
            { x with
                Sprite = makeSprite x.IsRounded x.Img
            }
        )
    foxSprite <- f foxSprite
    duckSprite <- f duckSprite

let start (canvas:HTMLCanvasElement) gameOverEvent =
    updateSize canvas.width canvas.height

    let ctx = canvas.getContext_2d()
    let mutable mouseX, mouseY = 0., 0.
    let mutable isMouseButtonDown = false

    canvas.ontouchmove <- fun e ->
        let rect = canvas.getBoundingClientRect();
        let cssX = e.touches.[0].clientX - rect.left
        let cssY = e.touches.[0].clientY - rect.top
        let pixelX = cssX * canvas.width  / rect.width
        let pixelY = cssY * canvas.height / rect.height
        mouseX <- pixelX; mouseY <- pixelY
    canvas.ontouchstart <- fun _ ->
        isMouseButtonDown <- true
    canvas.ontouchend <-
        fun _ -> isMouseButtonDown <- false

    canvas.onmousemove <- fun e ->
        mouseX <- e.offsetX; mouseY <- e.offsetY

    canvas.onmousedown <- fun _ -> isMouseButtonDown <- true
    canvas.onmouseup <- fun _ -> isMouseButtonDown <- false

    {|
        Update = fun delta ->
            let delta = delta * speedMult
            if Option.isSome gameOver then
                if isMouseButtonDown then
                    restart()
            elif duckX*duckX + duckY*duckY > radius*radius then
                let diff = atan2 duckY duckX - fox
                let diff = diff + if diff < System.Math.PI then System.Math.PI * 2. else 0.
                let diff = diff - if diff > System.Math.PI then System.Math.PI * 2. else 0.

                let isWin = abs diff > 0.000001
                gameOver <- Some isWin
                isMouseButtonDown <- false

                gameOverEvent isWin
            else
                if isMouseButtonDown then
                    moveDuck (mouseX - float width / 2., mouseY - float height / 2.) delta
                fox <- updateFox radius (duckX, duckY) foxSpeed fox delta

        Draw = fun () ->
            redraw ctx
    |}