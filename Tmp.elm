-- Start of ./src/Main.elm
-- module Main exposing(..)


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Browser.Dom
import Browser.Events
import Dict
import Set
import Json.Decode
import Random
import Array


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { w : Float
  , h : Float
  , tmp : TmpModel
  }


defaultW = 256
defaultH = 128


init : () -> (Model, Cmd Msg)
init _ =
  ( Model defaultW defaultH tmpInit
  , Cmd.batch
    [ Task.attempt
      getSizeFromViewport
      Browser.Dom.getViewport
    , Cmd.map MsgTmp cmdTmpInit
    ]
  )


getSizeFromViewport resultViewport =
  case resultViewport of
    Err err ->
      SetSizeFromViewport
        defaultW
        defaultH
        
    Ok viewport ->
      SetSizeFromViewport
        (viewport.viewport.width - 50)
        (viewport.viewport.height - 55)


-- UPDATE


type Msg
  = SetSizeFromViewport Float Float
  | MsgTmp TmpMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MsgTmp tmpMsg ->
      let
        ( tmp, cmd ) =
          tmpUpdate
            tmpMsg
            model.tmp
      in
      ( { model | tmp = tmp }
      , Cmd.map MsgTmp cmd
      )
    
    SetSizeFromViewport w h ->
      ( { model
          | w = w
          , h = h
        }
      , Cmd.none
      )


-- VIEW


view : Model -> Html Msg
view model =
  svg
    [ -- viewBox parameters are:
      --   minXValue
      --   minYValue
      --   width
      --   height
      viewBox ( "-25 -25 "
        ++ String.fromFloat (model.w + 50)
        ++ " "
        ++ String.fromFloat (model.h + 50)
        )
    , width (String.fromFloat (model.w + 50))
    , height (String.fromFloat (model.h + 50))
    ]
    [ rect
      [ x "-15"
      , y "-15"
      , width (String.fromFloat (model.w + 30))
      , height (String.fromFloat (model.h + 30))
      , rx "15"
      , ry "15"
      , fill "rgb(50,50,50)"
      ]
      []
    , rect
      [ x "-5"
      , y "-5"
      , width (String.fromFloat (model.w + 10))
      , height (String.fromFloat (model.h + 10))
      , rx "5"
      , ry "5"
      , fill "LightBlue"
      ]
      []
    , tmpView 0 0 model.w model.h
      model.tmp
        |> Svg.map MsgTmp
    ]


viewLine strokeC strokeW x1Float y1Float x2Float y2Float =
  line
    [ x1 <| String.fromFloat x1Float
    , y1 <| String.fromFloat y1Float
    , x2 <| String.fromFloat x2Float
    , y2 <| String.fromFloat y2Float
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewRectFill fillC = viewRect fillC "none" 0
viewRectLine strokeC strokeW =
  viewRect "none" strokeC strokeW
viewRect fillC strokeC strokeW xFloat yFloat widthFloat heightFloat =
  rect
    [ x <| String.fromFloat xFloat
    , y <| String.fromFloat yFloat
    , width <| String.fromFloat widthFloat
    , height <| String.fromFloat heightFloat
    , fill fillC
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewCircleFill fillC = viewCircle fillC "none" 0
viewCircleLine strokeC strokeW =
  viewCircle "none" strokeC strokeW
viewCircle fillC strokeC strokeW cxFloat cyFloat rFloat =
  circle
    [ cx <| String.fromFloat cxFloat
    , cy <| String.fromFloat cyFloat
    , r <| String.fromFloat rFloat
    , fill fillC
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewEllipseFill fillC = viewEllipse fillC "none" 0
viewEllipseLine strokeC strokeW =
  viewEllipse "none" strokeC strokeW
viewEllipse fillC strokeC strokeW cxFloat cyFloat wFloat hFloat =
  ellipse
    [ cx <| String.fromFloat cxFloat
    , cy <| String.fromFloat cyFloat
    , rx <| String.fromFloat wFloat
    , ry <| String.fromFloat hFloat
    , fill fillC
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewText fontC fontSizeFloat xFloat yFloat str =
  text_
    [ x <| String.fromFloat xFloat
    , y <| String.fromFloat yFloat
    , fontSize
      <| String.fromFloat
        fontSizeFloat
    , fontFamily "tahoma"
    , fill fontC
    ]
    [ text str ]


lettersWithTails =
  "gjpyq;,/\\$()[]{}"
    |> String.toList
    |> Set.fromList


tallLetters =
  ( "ABCDEFTHIJKLMNOPQRSTUVWXYZ"
    ++ "df/lkjibht<>?/'\"{}[])(*&&^%$#@!~`"
    ++ "1234567890"
  )
    |> String.toList
    |> Set.fromList


extraTallLetters =
  ( "`{}[]()"
  )
    |> String.toList
    |> Set.fromList


fontWs =
  -- These measurements are for viewFillText
  [("!",0.33),("\"",0.40),("#",0.72),("$",0.54),("%",0.98)
  ,("&",0.675),("'",0.21),("(",0.38),(")",0.38),("*",0.54)
  ,("+",0.72),(",",0.30),("-",0.36),(".",0.3),("/",0.38)
  ,("0",0.54),("1",0.54),("2",0.54),("3",0.54),("4",0.54)
  ,("5",0.54),("6",0.54),("7",0.54),("8",0.54),("9",0.54)
  ,(":",0.35),(";",0.35),("<",0.739),("=",0.72),(">",0.73)
  ,("?",0.47),("@",0.90),("A",0.60),("B",0.59),("C",0.60)
  ,("D",0.68),("E",0.56),("F",0.52),("G",0.67),("H",0.675)
  ,("I",0.37),("J",0.41),("K",0.59),("L",0.5),("M",0.77)
  ,("N",0.67),("O",0.7),("P",0.55),("Q",0.7),("R",0.62)
  ,("S",0.55),("T",0.54),("U",0.66),("V",0.6),("W",0.9)
  ,("X",0.58),("Y",0.58),("Z",0.56),("[",0.38),("]",0.38)
  ,("^",0.72),("_",0.55),("`",0.54),("a",0.52),("b",0.55)
  ,("c",0.46),("d",0.55),("e",0.52),("f",0.31),("g",0.55)
  ,("h",0.56),("i",0.23),("j",0.28),("k",0.5),("l",0.23)
  ,("m",0.84),("n",0.56),("o",0.54),("p",0.55),("q",0.55)
  ,("r",0.36),("s",0.45),("t",0.33),("u",0.56),("v",0.5)
  ,("w",0.74),("x",0.5),("y",0.5),("z",0.44),("{",0.48)
  ,("}",0.48),("~",0.72)
  ]

fontWAvg =
  ( fontWs
    |> List.map Tuple.second
    |> List.foldl (+) 0
  )
  / (fontWs |> List.length |> toFloat)
    
  
fontW =
  fontWs |> Dict.fromList


viewFillText fillC left top w h str =
  let
    ws =
      str
        |> String.toList
        |> List.map
          (\ch ->
            Dict.get
              (String.fromChar ch)
              fontW
              |> Maybe.withDefault
                fontWAvg
          )
        |> List.foldl (+) 0

    fontSizeByW =
      w / ws
    
    hasTails =
      str
        |> String.toList
        |> List.any
          (\ch ->
            Set.member ch lettersWithTails
          )
    
    hasTall =
      str
        |> String.toList
        |> List.any
          (\ch ->
            Set.member ch tallLetters
          )
    
    
    fontSizeByH =
      if hasTails then
        if hasTall then
          h * 1
        else
          h * 1.3
      else if hasTall then
        h * 1.27
      else
        h * 1.7
    
    fontS =
      Basics.min
        fontSizeByW
        fontSizeByH
    
    yOffset =
      if hasTails then
        if hasTall then
        fontS * 0.74
        else
        fontS * 0.53
      else if hasTall then
        fontS * 0.77
      else
        fontS * 0.57
    
    textH =
      if hasTails then
        if hasTall then
        fontS * 0.95
        else
        fontS * 0.7
      else if hasTall then
        fontS * 0.77
      else
        fontS * 0.57
        
    textW = fontS * ws
  in
  viewText fillC
    fontS
    (left + (w / 2) - (textW / 2))
    (top + ((h-textH)/2) + yOffset)
    str


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize
      setSizeFromViewport
    , Sub.map
      MsgTmp
      (tmpSubscriptions model.tmp)
    ]

setSizeFromViewport w h =
  SetSizeFromViewport
    (toFloat (w - 50))
    (toFloat (h - 55))
    
    
-- End of ./src/Main.elm


-- Start of ./src/Tmp.elm
-- module Tmp exposing(..)
-- Copy "import" lines from the top of Main.elm


-- Model


type alias TmpModel = {}


tmpInit =
  TmpModel


-- Update


type TmpMsg
  = TmpMsg


cmdTmpInit =
  Cmd.none


tmpUpdate msg model =
  ( model, Cmd.none )


-- View


tmpView left top w h model =
  g [] []


-- Subscriptions


tmpSubscriptions model =
  Sub.none


-- Functions



-- End of ./src/Tmp.elm
