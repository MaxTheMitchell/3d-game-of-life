-- Render a spinning cube.
--
-- Dependencies:
--   elm install elm-explorations/linear-algebra
--   elm install elm-explorations/webgl
--
module Main exposing(main)

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Set exposing (Set)


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = {
  angle : Float
  ,cubes : List(Cube)
  ,x : Int
  ,y : Int
  ,z : Int
  ,gridSize : Int
  }


init : () -> (Model, Cmd Msg)
init () =
  ( Model 0 thing2 0 0 0 10, Cmd.none)

line : List(Cube)
line = [
      Cube 1 0 0
      ,Cube 0 0 0
      ,Cube -1 0 0
    ]

thing : List(Cube)
thing = [
  Cube 1 0 0
  ,Cube 0 1 0
  ,Cube -1 0 0
  ,Cube 0 -1 0]

thing2 : List(Cube)
thing2 = [
  Cube 0 1 0 
  ,Cube 0 0 1 
  ,Cube 0 -1 0 
  ,Cube 0 0 -1 
  , Cube 0 1 1 
  , Cube 0 -1 1 
  , Cube 0 1 -1 
  , Cube 0 -1 -1 ]

-- UPDATE


type Msg
  = TimeDelta Float
  | Update 
  | Clear
  | AddCube
  | SetX String
  | SetY String
  | SetZ String
  | SetGridSize String

type Cube 
  = Cube Int Int Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TimeDelta dt ->
      ({model | angle = model.angle + dt / 5000}, Cmd.none )
    Update ->
      (updateCubes model, Cmd.none)
    Clear -> 
      ({model | cubes = []}, Cmd.none)
    AddCube -> 
      (addCube model, Cmd.none)
    SetX str ->
      ({model | x = parseAxisStr model.gridSize str}, Cmd.none)
    SetY str ->
      ({model | y = parseAxisStr model.gridSize str}, Cmd.none)
    SetZ str ->
      ({model | z = parseAxisStr model.gridSize str}, Cmd.none)
    SetGridSize str -> 
      ({model | gridSize = parseGridStr model.gridSize str}, Cmd.none)

parseAxisStr : Int -> String -> Int
parseAxisStr gridSize str = 
  let 
    val = Maybe.withDefault 0 (String.toInt str)
  in 
    if val > gridSize
      then gridSize 
    else if val < -1 * gridSize 
      then -1 * gridSize 
    else val

parseGridStr : Int -> String -> Int 
parseGridStr prevSize str =
  let 
    val = Maybe.withDefault prevSize (String.toInt str)
  in 
    if val < gridMin
      then gridMin
    else if val > gridMax
      then gridMax
    else val

updateInterval : Int 
updateInterval = 100

updateCubes : Model -> Model
updateCubes model = 
  let
      gridRange = List.range (model.gridSize * -1) model.gridSize
  in
    List.map 
      (\x ->
        List.map
          (\y ->
            List.map (Cube x y) gridRange 
          ) gridRange 
      ) gridRange
    |> List.foldl (++) []
    |> List.foldl (++) []
    |> List.filter (\cube -> 
        case adjacentCubeCount cube model.cubes of 
          2 -> List.member cube model.cubes 
          3 -> True 
          _ -> False
      )
    |> (\newCubes -> {model | cubes = newCubes})

adjacentCubeCount : Cube -> List(Cube) -> Int
adjacentCubeCount cube cubes =
  findAdjacentCubes cube cubes 
    |> List.length

findAdjacentCubes : Cube -> List(Cube) -> List(Cube) 
findAdjacentCubes cube cubes =
  List.filter (areCubesAdjacent cube) cubes


areCubesAdjacent : Cube -> Cube -> Bool
areCubesAdjacent (Cube x1 y1 z1) (Cube x2 y2 z2) = 
  let 
    axisDiff v1 v2 = abs (v1 - v2)
    axisDiffByOneOrZero v1 v2 = 
      case axisDiff v1 v2 of 
        0 -> True
        1 -> True
        _ -> False
  in
    axisDiffByOneOrZero x1 x2
    && axisDiffByOneOrZero y1 y2 
    && axisDiffByOneOrZero z1 z2
    && (axisDiff x1 x2) + (axisDiff y1 y2) + (axisDiff z1 z2) /= 0 
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta


-- VIEW


view : Model -> Html Msg
view model =
  Html.div[style "background-color" "black", style "display" "flex"]
  [
    WebGL.toHtml
    [ width animationWidth, height windowHeight, style "display" "block"
    ]
    (drawCubes model)
    , Html.div[ 
      width (windowWidth - animationWidth)
      ,height windowHeight
      ,style "background-color" "pink"
      ,style "width" "100%"
      ,style "text-align" "center"
      ,style "line-height" "2"
      ][
        Html.h1[][Html.text "Controls"]
        ,Html.button[Html.Events.onClick Update][Html.text "Update"]
        ,Html.button[Html.Events.onClick Clear][Html.text "Clear"]
        ,Html.div[] (
          (axisInput "x" model.gridSize model.x SetX) 
          ++ (axisInput "y" model.gridSize model.y SetY) 
          ++ (axisInput "z" model.gridSize model.z SetZ) 
          ++ [Html.button[Html.Events.onClick AddCube ][Html.text "Add Cube"]] 
          )
        ,Html.div[] (numberInput "Grid Size" gridMin gridMax model.gridSize SetGridSize)
        ]
  ]

axisInput : String -> Int -> Int -> (String -> Msg) -> List(Html Msg)
axisInput axis gridSize val msg =
  numberInput axis (gridSize * -1) gridSize val msg

numberInput : String -> Int -> Int -> Int -> (String -> Msg) -> List(Html Msg)
numberInput label min max val msg =
  [Html.label[Html.Attributes.for label][Html.text label]
  ,Html.input[
    Html.Attributes.name label
    ,Html.Attributes.type_ "number"
    ,Html.Attributes.min (String.fromInt min)
    ,Html.Attributes.max (String.fromInt max)
    ,Html.Attributes.value (String.fromInt val)
    ,Html.Events.onInput msg
  ][]]

type alias Uniforms =
  { rotation : Mat4
  , perspective : Mat4
  , camera : Mat4
  }


uniforms : Float -> Uniforms
uniforms angle =
  { rotation =
      Mat4.mul
        (Mat4.makeRotate (3 * angle) (vec3 0 1 0))
        (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
  , perspective = Mat4.makePerspective 45 1 0.01 100
  , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  }


-- MESH

type alias Vertex =
  { color : Vec3
  , position : Vec3
  }

drawCubes : Model -> List(WebGL.Entity)
drawCubes model =
  model.cubes 
    |> List.map (cubeMesh model.gridSize)
    |> List.concat
    |> WebGL.triangles
    |> (\c -> [WebGL.entity vertexShader fragmentShader c (uniforms model.angle)])

addCube : Model -> Model
addCube model =
  let 
    cube = Cube model.x model.y model.z
  in
    if List.member cube model.cubes 
    then model
    else {model | cubes = cube :: model.cubes}


cubeMesh : Int -> Cube ->  List ( Vertex, Vertex, Vertex ) --WebGL.Mesh Vertex
cubeMesh gridSize (Cube x y z) =
  let
    cubesize = cubeSize gridSize
    create_corner xc yc zc = vec3 (xc*cubesize*0.5 + cubesize*toFloat x) (yc*cubesize*0.5 + toFloat y*cubesize) (zc*cubesize*0.5 + toFloat z*cubesize) 
    rft = create_corner 1 1 1
    lft = create_corner -1 1 1
    lbt = create_corner -1 -1 1
    rbt = create_corner 1 -1 1
    rbb = create_corner 1 -1 -1
    rfb = create_corner 1 1 -1
    lfb = create_corner -1 1 -1
    lbb = create_corner -1 -1 -1
  in
  --WebGL.triangles <|
   List.concat <|
    [ face (vec3 115 210 22 ) rft rfb rbb rbt -- green
    , face (vec3 52  101 164) rft rfb lfb lft -- blue
    , face (vec3 237 212 0  ) rft lft lbt rbt -- yellow
    , face (vec3 204 0   0  ) rfb lfb lbb rbb -- red
    , face (vec3 117 80  123) lft lfb lbb lbt -- purple
    , face (vec3 245 121 0  ) rbt rbb lbb lbt -- orange
    ]


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
  let
    vertex position =
      Vertex (Vec3.scale (1 / 255) color) position
  in
  [ ( vertex a, vertex b, vertex c )
  , ( vertex c, vertex d, vertex a )
  ]


-- SHADERS

vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 rotation;
    varying vec3 vcolor;
    void main () {
        gl_Position = perspective * camera * rotation * vec4(position, 1.0);
        vcolor = color;
    }
  |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
  [glsl|
    precision mediump float;
    varying vec3 vcolor;
    void main () {
        gl_FragColor = 0.8 * vec4(vcolor, 1.0);
    }
  |]

-- Conts
cubeSize :  Int -> Float
cubeSize gridSize = 1 / toFloat gridSize

animationWidth : Int 
animationWidth = 1080

windowWidth : Int 
windowWidth = 1920

windowHeight : Int 
windowHeight = 1080

gridMin : Int 
gridMin = 1

gridMax : Int 
gridMax = 20