module colorwheel

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import


type Color =
    | RGB of int * int * int
    | XYZ of x:float * y:float * z:float
    | LAB of l:float * a:float * b:float


//type Color = int * int * int
type Results =
    | Primary of Color
    | Filled of Color



let refX = 95.047
let refY = 100.
let refZ = 108.883


let round (x:float) = int (System.Math.Round(x))

let toXyz (color: Color): Color =
    match color with
    | RGB (r, g, b) ->
        let calc x =
            let v = (float x) / 255.
            match v with
            | y when y > 0.04045 -> ((y + 0.055) / 1.055) ** 2.4
            | _ -> v / 12.92

        let R = (calc r) * 100.
        let G = (calc g) * 100.
        let B = (calc b) * 100.

        //Observer. = 2°, Illuminant = D65
        XYZ(R * 0.4124 + G * 0.3576 + B * 0.1805,
            R * 0.2126 + G * 0.7152 + B * 0.0722,
            R * 0.0193 + G * 0.1192 + B * 0.9505
        )
    | XYZ _ -> color
    | LAB(l, a, b) ->
        let y = (l + 16.) / 116.
        let x = a / 500. + y
        let z = y - b / 200.

        let normalise v =
            match v ** 3. with
            | r when r > 0.008856 -> r
            | _ -> (v - 16. / 116.) / 7.787

        let X = (normalise x) * refX
        let Y= (normalise y) * refY
        let Z = (normalise z) * refZ
        XYZ (X, Y, Z)


(*
var_Y = ( CIE-L* + 16 ) / 116
var_X = CIE-a* / 500 + var_Y
var_Z = var_Y - CIE-b* / 200

if ( var_Y^3  > 0.008856 ) var_Y = var_Y^3
else                       var_Y = ( var_Y - 16 / 116 ) / 7.787
if ( var_X^3  > 0.008856 ) var_X = var_X^3
else                       var_X = ( var_X - 16 / 116 ) / 7.787
if ( var_Z^3  > 0.008856 ) var_Z = var_Z^3
else                       var_Z = ( var_Z - 16 / 116 ) / 7.787

X = var_X * Reference-X
Y = var_Y * Reference-Y
Z = var_Z * Reference-Z*)





  (*
      var_R = parseFloat( R / 255 )        //R from 0 to 255
    var_G = parseFloat( G / 255 )        //G from 0 to 255
    var_B = parseFloat( B / 255 )        //B from 0 to 255

    if ( var_R > 0.04045 ) var_R = ( ( var_R + 0.055 ) / 1.055 ) ^ 2.4
    else                   var_R = var_R / 12.92
    if ( var_G > 0.04045 ) var_G = ( ( var_G + 0.055 ) / 1.055 ) ^ 2.4
    else                   var_G = var_G / 12.92
    if ( var_B > 0.04045 ) var_B = ( ( var_B + 0.055 ) / 1.055 ) ^ 2.4
    else                   var_B = var_B / 12.92

    var_R = var_R * 100
    var_G = var_G * 100
    var_B = var_B * 100


    X = var_R * 0.4124 + var_G * 0.3576 + var_B * 0.1805
    Y = var_R * 0.2126 + var_G * 0.7152 + var_B * 0.0722
    Z = var_R * 0.0193 + var_G * 0.1192 + var_B * 0.9505
    return [X, Y, Z]
    *)

let rec toLab color =
    match color with
    | LAB _ -> color
    | RGB _ -> color |> toXyz |> toLab
    | XYZ (x, y, z) ->

        let normalisedX = x / refX
        let normalisedY = y / refY
        let normalisedZ = z / refZ

        let calc x =
            match x with
            | y when y > 0.008856 -> y ** (1. /3.)
            | _ -> (7.787 * x) + (16./116.)

        let X = calc normalisedX
        let Y = calc normalisedY
        let Z = calc normalisedZ

        LAB(
            (116. * Y) - 16.,
            500. * (X - Y),
            200. * (Y - Z)
        )

(*
   var ref_X =  95.047;
    var ref_Y = 100.000;
    var ref_Z = 108.883;

    var_X = x / ref_X          //ref_X =  95.047   Observer= 2°, Illuminant= D65
    var_Y = y / ref_Y          //ref_Y = 100.000
    var_Z = z / ref_Z          //ref_Z = 108.883

    if ( var_X > 0.008856 ) var_X = var_X ^ ( 1/3 )
    else                    var_X = ( 7.787 * var_X ) + ( 16 / 116 )
    if ( var_Y > 0.008856 ) var_Y = var_Y ^ ( 1/3 )
    else                    var_Y = ( 7.787 * var_Y ) + ( 16 / 116 )
    if ( var_Z > 0.008856 ) var_Z = var_Z ^ ( 1/3 )
    else                    var_Z = ( 7.787 * var_Z ) + ( 16 / 116 )

    CIE_L = ( 116 * var_Y ) - 16
    CIE_a = 500 * ( var_X - var_Y )
    CIE_b = 200 * ( var_Y - var_Z )
    *)

let rec toRgb color =
    match color with
    | RGB _ -> color
    | LAB _ -> color |> toXyz |> toRgb
    | XYZ(x, y, z) ->
        let X = x / 100.
        let Y = y / 100.
        let Z = z / 100.

        let R = X *  3.2406 + Y * -1.5372 + Z * -0.4986
        let G = X * -0.9689 + Y *  1.8758 + Z *  0.0415
        let B = X *  0.0557 + Y * -0.2040 + Z *  1.0570

        let normalise v =
            match v with
            | v when v > 0.0031308 -> 1.055 * ( v ** ( 1. / 2.4 ) ) - 0.055
            | _ -> 12.92 * v

        RGB(
            ((R |> normalise) * 255.) |> round,
            ((G |> normalise) * 255.) |> round,
            ((B |> normalise) * 255.) |> round
        )

(*
var_X = X / 100
var_Y = Y / 100
var_Z = Z / 100

var_R = var_X *  3.2406 + var_Y * -1.5372 + var_Z * -0.4986
var_G = var_X * -0.9689 + var_Y *  1.8758 + var_Z *  0.0415
var_B = var_X *  0.0557 + var_Y * -0.2040 + var_Z *  1.0570

if ( var_R > 0.0031308 ) var_R = 1.055 * ( var_R ^ ( 1 / 2.4 ) ) - 0.055
else                     var_R = 12.92 * var_R
if ( var_G > 0.0031308 ) var_G = 1.055 * ( var_G ^ ( 1 / 2.4 ) ) - 0.055
else                     var_G = 12.92 * var_G
if ( var_B > 0.0031308 ) var_B = 1.055 * ( var_B ^ ( 1 / 2.4 ) ) - 0.055
else                     var_B = 12.92 * var_B

sR = var_R * 255
sG = var_G * 255
sB = var_B * 255

*)

let mutable colors: Color list = [RGB(217, 64, 37); RGB(203, 232, 143); RGB(183, 206, 228)]

let mutable renderer = fun () -> ()

let parseColor (raw:string) =
    let s =
        match raw.StartsWith("#") with
        | x when x -> raw.Substring(1)
        | _ -> raw
    let convert (x:string) = System.Convert.ToInt32(x, 16)
    let r = s.Substring(0, 2) |> convert
    let g = s.Substring(2, 2) |> convert
    let b = s.Substring(4, 2) |> convert

    RGB(r, g, b)


let refresh() =
    let inputs = Browser.document.getElementsByClassName("primary")
    let length = inputs.length |> int
    let mutable newColors: Color list = List.empty
    for i in 0..length - 1  do
        let input = inputs.[i].getElementsByTagName_input().[0]
        let value = input.value
        newColors <- (parseColor value) :: newColors

    colors <- newColors |> List.rev
    renderer()
    null

let toHex color =
    let rgb = toRgb color
    match rgb with
    | RGB(r, g, b) -> sprintf "#%X%X%X" r g b
    | _ -> failwith "shit happended"


let createColorDiv color =
    let (color, css, disabled) =
        match color with
        | Primary c -> (toHex c), "primary", false
        | Filled c -> (toHex c), "filled", true
    let div = Browser.document.createElement_div()
    let txtBox = Browser.document.createElement_input()
    txtBox.value <- color
    txtBox.disabled <- disabled
    txtBox.addEventListener_blur (fun _ -> refresh())
    div.className <- css
    div.style.backgroundColor <- color
    div.appendChild(txtBox) |> ignore
    div


let render() =

    let pointsBetween = List.init 3 (fun i -> i + 3)

    let zipped = colors |> List.zip pointsBetween

    let lastIdx = (colors |> List.length) - 1

    let result =
        zipped |>
        List.mapi (fun i (num, color) ->
            let lab c =
                match (toLab c) with
                | LAB(l, a, b) -> (l, a, b)
                | _ -> failwith "this should never happen"

            let (l, a, b) = lab color
            let (opL, opA, opB) =
                match i with
                | x when x = lastIdx -> colors |> List.head |> lab
                | _ -> colors |> List.item (i + 1) |> lab


            let calcDistance c o =
                c - o
            let (dL, dA, dB) = (calcDistance opL l, calcDistance opA a, calcDistance opB b)

            printfn "color: %A; oponent: %A; distance: %A" (l, a, b) (opL, opA, opB) (dL, dA, dB)
            let n = num |> float

            let producer (i: int) =
                let i = i + 1 |> float
                Filled (LAB(l + (dL / n) * i, a + (dA / n) * i, b + (dB /n) * i))
            Primary(LAB(l, a, b)) :: List.init num producer
        )
        |> List.collect id
    let div = Browser.document.getElementById "wheel" :?> Browser.HTMLDivElement
    div.innerHTML <- ""
    result |> List.map createColorDiv |> List.iter (fun it -> div.appendChild(it) |> ignore)


renderer <- render
render()