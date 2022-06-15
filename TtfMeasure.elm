-- Start of ./src/Main.elm
-- module Main exposing(..)


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Browser.Dom
import Browser.Events
import Json.Decode
import Dict
import Set


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
  , ch : String
  , fontSize : Float
  , fontW : Dict.Dict String Float
  }


defaultW = 256
defaultH = 128


init : () -> (Model, Cmd Msg)
init _ =
  ( Model defaultW defaultH "A" 20 fontW
  , Task.attempt
    getSizeFromViewport
    Browser.Dom.getViewport
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
  | MouseMove Float
  | Ch String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MouseMove pageX ->
      ( { model | fontSize = (pageX / 3) + 20 }
      , Cmd.none
      )
    Ch ch ->
      ( { model | ch = ch
        , fontSize =
          Dict.get ch model.fontW
            |> Maybe.map (\fs -> model.w / (fs * 10) )
            |> Maybe.withDefault 20
        , fontW =
          Dict.insert model.ch (model.w / (model.fontSize * 10) ) model.fontW
      }
      , Cmd.none
      )
    SetSizeFromViewport w h ->
      ( { model
          | w = w
          , h = 100
        }
      , Cmd.none
      )


-- VIEW


view : Model -> Html Msg
view model =
  Html.div []
    [ svg
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
      , viewRectLine "rgb(50,50,50)"
        1
        0
        0
        model.w
        model.h
      , viewFillText "rgb(50,50,50)"
        --model.fontSize
        0
        0
        model.w
        model.h
        "SSSaa"
        --(String.repeat 10 model.ch)
      ]
    , Html.p []
      [ model.fontW
        |> Debug.toString
        |> Html.text
      ]
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
  [("A",0.6047619047619047),("S",0.5442857142857142),("Y",0.5772727272727273),("a",0.5193103448275861),("d",0.5456521739130434),("f",0.3161825726141078),("j",0.27567567567567564),("p",0.5482014388489209),("q",0.5482014388489209),("s",0.4456140350877192),("y",0.4980392156862745)]

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
    , Browser.Events.onMouseMove
      ( Json.Decode.field "pageX" Json.Decode.float
        |> Json.Decode.map MouseMove
      )
    , Browser.Events.onKeyDown
      ( Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.map Ch
      )
    ]

setSizeFromViewport w h =
  SetSizeFromViewport
    (toFloat (w - 50))
    (toFloat (h - 55))
    
    
-- End of ./src/Main.elm
