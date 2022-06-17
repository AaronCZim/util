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
  [(" ",0.35),("!",0.3281767955801105),("\"",0.400390625),("#",0.7243902439024391),("$",0.5449541284403671),("%",0.981818181818182),("&",0.675),("'",0.21119505494505497),("(",0.3832258064516129),(")",0.38076923076923075),("*",0.5449541284403671),("+",0.7243902439024391),(",",0.300759493670886),("-",0.3632),(".",0.29849246231155774),("/",0.38151364764268),("0",0.5449541284403671),("1",0.5449541284403671),("2",0.5449541284403671),("3",0.5449541284403671),("4",0.5449541284403671),("5",0.5449541284403671),("6",0.5449541284403671),("7",0.5449541284403671),("8",0.5449541284403671),("9",0.5449541284403671),(":",0.3494318181818181),(";",0.3504424778761062),("<",0.7252358490566039),("=",0.724468085106383),(">",0.7252358490566039),("?",0.4730769230769231),("@",0.9068702290076335),("A",0.6026548672566371),("B",0.591044776119403),("C",0.6030456852791881),("D",0.6788571428571428),("E",0.5603773584905661),("F",0.5233480176211455),("G",0.6674157303370786),("H",0.675),("I",0.37124999999999997),("J",0.4139372822299651),("K",0.591044776119403),("L",0.4991596638655461),("M",0.7714285714285716),("N",0.6674157303370786),("O",0.7071428571428572),("P",0.55),("Q",0.7071428571428573),("R",0.6219895287958116),("S",0.5474654377880184),("T",0.5399999999999999),("U",0.656353591160221),("V",0.6),("W",0.9068702290076335),("X",0.5823529411764707),("Y",0.5766990291262137),("Z",0.5551401869158877),("[",0.38151364764268),("]",0.38056930693069296),("^",0.7243902439024391),("_",0.55),("`",0.5351351351351351),("a",0.5211864406779662),("b",0.5510752688172043),("c",0.458955223880597),("d",0.5510752688172043),("e",0.5247440273037541),("f",0.31441717791411045),("g",0.5510752688172043),("h",0.5570652173913043),("i",0.22643593519882177),("j",0.27903811252268607),("k",0.4991883116883115),("l",0.22811572700296737),("m",0.8401639344262294),("n",0.5550541516245486),("o",0.5413732394366196),("p",0.5510752688172043),("q",0.5491071428571429),("r",0.3609154929577465),("s",0.4456140350877192),("t",0.3278251599147121),("u",0.5550541516245486),("v",0.4975728155339806),("w",0.7427536231884058),("x",0.49596774193548393),("y",0.4991883116883115),("z",0.444364161849711),("{",0.47897196261682246),("}",0.48046874999999994),("~",0.7243902439024391)]

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
    , Sub.map MsgTmp (tmpSubscriptions model.tmp)
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
