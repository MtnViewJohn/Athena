module Main exposing (Model, Msg(..), initModel, main, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Browser
import Browser.Navigation as Nav
import Maybe.Extra exposing (isNothing)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Url.Builder as UB
import Url
import Dict exposing (Dict)
import Markdown

main =
  Browser.application
  { init = initModel
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = LinkClicked
  , onUrlChange = UrlChanged
  }




-- MODEL

type alias CheckedField =
  { value : Float
  , text : String
  , error : Maybe String
  , convertFn : String -> Result String Float
  }


initField : CheckedField -> Maybe String -> (String -> Result String Float) -> CheckedField
initField defs urls cvt =
  let
    s = Maybe.withDefault defs.text urls
  in
    case cvt s of
      Ok f2 -> CheckedField f2 s Nothing cvt
      Err e -> CheckedField 0.0 s (Just e) cvt


updateField : CheckedField -> String -> CheckedField
updateField cf str =
  case cf.convertFn str of
    Ok f -> {cf | value = f, text = str, error = Nothing}
    Err s -> {cf | text = str, error = Just s}


positive : String -> Result String Float
positive s =
  case String.toFloat s of
    Nothing -> Err "not valid"
    Just f -> 
      if f > 0.0 then
        Ok f
      else
        Err "must be positive"


notNegative : String -> Result String Float
notNegative s =
  case String.toFloat s of
    Nothing -> Err "not valid"
    Just f -> 
      if f >= 0.0 then
        Ok f
      else
        Err "must not be negative"


isInteger : String -> Result String Float
isInteger s =
  case String.toFloat s of
    Nothing -> Err "not valid"
    Just f -> 
      if f == (toFloat <| floor f) then
        Ok f
      else
        Err "must be an integer"



type alias Model =
  { project : String
  , metric : Bool
  , length : CheckedField
  , count : CheckedField
  , fringeLength : CheckedField
  , samplingLength : CheckedField
  , loomWaste : CheckedField
  , lengthTakeup : CheckedField
  , lengthShrinkage : CheckedField
  , width : CheckedField
  , widthTakeup : CheckedField
  , widthShrinkage : CheckedField
  , warpSett : CheckedField
  , floatingSelvedge : Bool
  , ppi : CheckedField
  , warpAdjust : CheckedField
  , lengthAdjust : CheckedField
  , initUrl : Url.Url
  , key : Nav.Key
  }

emptyUrl = Url.Url Url.Https "" Nothing "/" Nothing Nothing

globalDefaultModel : Nav.Key -> Model
globalDefaultModel navkey = 
  Model
    ""                                                  -- Project name
    False                                               -- Metric?
    (CheckedField 30 "30" Nothing positive)             -- Finished length
    (CheckedField  1  "1" Nothing positive)             -- Count
    (CheckedField  0  "0" Nothing notNegative)          -- Fringe length
    (CheckedField  0  "0" Nothing notNegative)          -- Sampling length
    (CheckedField 24 "24" Nothing notNegative)          -- Loom waste
    (CheckedField 10 "10" Nothing notNegative)          -- Length take-up %
    (CheckedField 10 "10" Nothing notNegative)          -- Length shrinkage %
    (CheckedField 10 "10" Nothing positive)             -- Finished width
    (CheckedField 10 "10" Nothing notNegative)          -- Width take-up %
    (CheckedField 10 "10" Nothing notNegative)          -- Width shrinkage %
    (CheckedField 10 "10" Nothing positive)             -- Sett
    False                                               -- Floating selvedge?
    (CheckedField 10 "10" Nothing positive)             -- ppi
    (CheckedField  0  "0" Nothing isInteger)            -- Warp ends adjust
    (CheckedField  0  "0" Nothing notNegative)          -- Warp length adjust
    emptyUrl                                            -- URL
    navkey                                              -- Navigation key


type alias ParamDict = Dict String (Maybe String)

{- Quick and dirty url query parser. Does not percent-decode or handle
   escaped & or =. Adequate for this problem domain.
-}
parseParams : String -> ParamDict
parseParams stringWithAmpersands =
  let
    eachParam = (String.split "&" stringWithAmpersands)
    eachPair  = List.map (splitAtFirst '=') eachParam
  in
    Dict.fromList eachPair

splitAtFirst : Char -> String -> (String, Maybe String)
splitAtFirst c s =
  case firstOccurrence c s of
    Nothing -> (s, Nothing)
    Just i  -> ( String.left i s
               , let right = String.dropLeft (i + 1) s
                 in if right == ""
                    then Nothing
                    else Just right
               )


firstOccurrence : Char -> String -> Maybe Int
firstOccurrence c s =
  case String.indexes (String.fromChar c) s of
    []        -> Nothing
    head :: _ -> Just head


getQueryPart : String -> ParamDict -> Maybe String
getQueryPart field qd =
  case Dict.get field qd of
    Nothing -> Nothing
    Just q -> q




getQueryBool : String -> ParamDict -> Bool
getQueryBool field qd =
  case Dict.get field qd of
    Nothing -> False        -- not there -> False
    Just ms -> case ms of
      Nothing -> True       -- there, but no value -> True
      Just s -> List.member s ["true", "1"]


processUrl : Model -> Url.Url -> Model
processUrl defaults url = 
  let
    qd = case url.query of
      Nothing -> Dict.empty
      Just q -> parseParams q
    name =  Maybe.withDefault "" 
            <| Url.percentDecode 
            <| Maybe.withDefault "" (getQueryPart "project" qd)
  in
    case url.query of
      Nothing -> {defaults | initUrl = url}
      Just "" -> {defaults | initUrl = url}
      _ ->
        { project           = name
        , metric            = getQueryBool "metric" qd
        , length            = initField defaults.length (getQueryPart "length" qd) positive
        , count             = initField defaults.count (getQueryPart "count" qd) positive
        , fringeLength      = initField defaults.fringeLength (getQueryPart "fringe" qd) notNegative
        , samplingLength    = initField defaults.samplingLength (getQueryPart "sample" qd) notNegative
        , loomWaste         = initField defaults.loomWaste (getQueryPart "waste" qd) notNegative
        , lengthTakeup      = initField defaults.lengthTakeup (getQueryPart "ltakeup" qd) notNegative
        , lengthShrinkage   = initField defaults.lengthShrinkage (getQueryPart "lshrink" qd) notNegative
        , width             = initField defaults.width (getQueryPart "width" qd) positive
        , widthTakeup       = initField defaults.widthTakeup (getQueryPart "wtakeup" qd) notNegative
        , widthShrinkage    = initField defaults.widthShrinkage (getQueryPart "wshrink" qd) notNegative
        , warpSett          = initField defaults.warpSett (getQueryPart "sett" qd) positive
        , floatingSelvedge  = getQueryBool "floating" qd
        , ppi               = initField defaults.ppi (getQueryPart "ppi" qd) positive
        , warpAdjust        = initField defaults.warpAdjust (getQueryPart "adjust" qd) isInteger
        , lengthAdjust      = initField defaults.lengthAdjust (getQueryPart "warpAdjust" qd) notNegative
        , initUrl           = url
        , key               = defaults.key
        } 

initModel : String -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
initModel localDefaults initUrl navkey = 
  let
    mUrl = Url.fromString localDefaults
    localUrl = Maybe.withDefault emptyUrl mUrl
    localDefaultModel = processUrl (globalDefaultModel navkey) localUrl
  in
    (processUrl localDefaultModel initUrl, Cmd.none)

isValid : Model -> Bool
isValid model = 
     isNothing model.length.error
  && isNothing model.count.error
  && isNothing model.fringeLength.error
  && isNothing model.samplingLength.error
  && isNothing model.loomWaste.error
  && isNothing model.lengthTakeup.error
  && isNothing model.lengthShrinkage.error
  && isNothing model.width.error
  && isNothing model.widthTakeup.error
  && isNothing model.widthShrinkage.error
  && isNothing model.warpSett.error
  && isNothing model.ppi.error
  && isNothing model.warpAdjust.error
  && isNothing model.lengthAdjust.error


  -- UPDATE

type Msg
  = LengthChange String
  | CountChange String
  | FringeChange String
  | SamplingChange String
  | WasteChange String
  | LTakeupChange String
  | LShrinkChange String
  | WidthChange String
  | WTakeupChange String
  | WShrinkChange String
  | SettChange String
  | FloatingSelvedgeChange Bool
  | PPIChange String
  | WarpAdjustChange String
  | LengthAdjustChange String
  | UnitsChange Bool
  | ProjectChange String
  | UrlChanged Url.Url
  | LinkClicked Browser.UrlRequest


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
        LengthChange l -> {model | length = updateField model.length l}
        CountChange c -> {model | count = updateField model.count c}
        FringeChange l -> {model | fringeLength = updateField model.fringeLength l}
        SamplingChange l -> {model | samplingLength = updateField model.samplingLength l}
        WasteChange l -> {model | loomWaste = updateField model.loomWaste l}
        LTakeupChange l -> {model | lengthTakeup = updateField model.lengthTakeup l}
        LShrinkChange l -> {model | lengthShrinkage = updateField model.lengthShrinkage l}
        WidthChange w -> {model | width = updateField model.width w}
        WTakeupChange w -> {model | widthTakeup = updateField model.widthTakeup w}
        WShrinkChange w -> {model | widthShrinkage = updateField model.widthShrinkage w}
        SettChange s -> {model | warpSett = updateField model.warpSett s}
        FloatingSelvedgeChange f -> {model | floatingSelvedge = f}
        PPIChange p -> {model | ppi = updateField model.ppi p}
        WarpAdjustChange w -> {model | warpAdjust = updateField model.warpAdjust w}
        LengthAdjustChange l -> {model | lengthAdjust = updateField model.lengthAdjust l}
        UnitsChange m -> {model | metric = m}
        ProjectChange n -> {model | project = n}
        _ -> model
    newCmd = 
      case msg of
        UrlChanged _ -> Cmd.none
        LinkClicked req ->
          case req of
            Browser.Internal _ -> Cmd.none
            Browser.External href -> Nav.load href
        _ -> Nav.replaceUrl newModel.key <| Url.toString <| makeQuery newModel
  in
    (newModel, newCmd)


-- VIEW

type FieldType = Real | Integer | Percent


intLocale = {usLocale | decimals = 0, positivePrefix = "+"}


type alias Calculation =
  { shrinkW : Float
  , shrunkWidth : Float
  , takeupW : Float
  , reedWidth : Float
  , ends : Int
  , endsAdjusted : Int
  , endsFloat : Int
  , actualReedWidth : Float
  , shrinkL : Float
  , lengthWeave : Float
  , lengthWeaveT : Float
  , takeupL : Float
  , fringe : Float
  , lengthItem : Float
  , lengthItems : Float
  , lengthWarp : Float
  , lengthWarpYards : Int
  , lengthWarpInches : Int
  , lengthWarpAlt : Int
  , lengthWarpYarn : Float
  , lengthWarpYarnYards : Int
  , lengthWeftYarn : Float
  , lengthWeftSample : Float
  , lengthWeftYarnYards : Int
  }

roundf : Float -> Float
roundf f =
  toFloat <| floor (f + 0.9)

calculateWarp : Model -> Calculation
calculateWarp model =
  let
    smallperlarge = if model.metric then 100 else 36
    fringeoverlap = if model.metric then 15.0 else 6.0
    shrinkW = model.width.value * (model.widthShrinkage.value/100.0)
    shrunkWidth = model.width.value + shrinkW
    takeupW = shrunkWidth * (model.widthTakeup.value/100.0)
    reedWidth = shrunkWidth + takeupW
    ends = round <| reedWidth * model.warpSett.value
    endsAdjusted = ends + floor model.warpAdjust.value
    endsFloat = if model.floatingSelvedge then (endsAdjusted + 2) else endsAdjusted
    actualReedWidth = (toFloat endsFloat) / model.warpSett.value
    shrinkL = model.length.value * (model.lengthShrinkage.value/100.0)
    lengthWeave = roundf <| model.length.value + shrinkL
    takeupL = lengthWeave * (model.lengthTakeup.value/100.0)
    lengthWeaveT = roundf <| model.length.value + shrinkL + takeupL
    fringe = 2 * model.fringeLength.value
    frontFringe =
      if model.samplingLength.value <= 0 then
        Basics.max 0 (model.fringeLength.value - fringeoverlap)    -- steal up to 6" fring from waste, unless sampling
      else
        model.fringeLength.value
    backFringe = Basics.max 0 (model.fringeLength.value - fringeoverlap)
    lengthItem = lengthWeaveT + fringe
    lengthItems = roundf <| lengthItem * model.count.value - fringe + frontFringe + backFringe  -- loom waste is the outermost fringe
    lengthWarp = lengthItems + model.loomWaste.value + model.samplingLength.value + model.lengthAdjust.value
    lengthWarpAlt = if model.metric then
                      floor <| lengthWarp / 2.54 + 0.5
                    else
                      floor <| lengthWarp * 2.54 + 0.5
    lengthWarpLarge = floor (lengthWarp / smallperlarge)
    lengthWarpSmall = floor lengthWarp - (smallperlarge * lengthWarpLarge)
    lengthWarpYarn = lengthWarp * (toFloat endsFloat)
    lengthWarpYarnLarge = floor ((lengthWarpYarn / smallperlarge) + 0.9)
    lengthWeftYarn = actualReedWidth * model.ppi.value * 
                    (lengthWeaveT * model.count.value + model.samplingLength.value)
    lengthWeftSample = actualReedWidth * model.ppi.value * model.samplingLength.value
    lengthWeftYarnLarge = floor ((lengthWeftYarn / smallperlarge) + 0.9)
  in
    Calculation shrinkW shrunkWidth takeupW reedWidth ends endsAdjusted endsFloat
                actualReedWidth shrinkL lengthWeave lengthWeaveT takeupL fringe lengthItem
                lengthItems lengthWarp lengthWarpLarge lengthWarpSmall lengthWarpAlt lengthWarpYarn
                lengthWarpYarnLarge lengthWeftYarn lengthWeftSample lengthWeftYarnLarge

makeMarkup : Model -> Calculation -> String
makeMarkup model calc = 
  let
    smallunit = if model.metric then " cm" else "\""
    largeunit = if model.metric then " m" else " yards"
    altunit = if model.metric then "\"" else " cm"
    endsunit = if model.metric then " epcm" else " epi" 
    picksunit = if model.metric then " ppcm" else " ppi"
    picktext = if model.metric then "|× Picks per cm|" else "|× Picks per inch|"
    header = if String.isEmpty model.project then "" else "# " ++ model.project ++ "\n"
  in
    String.concat
    [ header
    , "|Warp Ends|Calculation|\n|:---|---:|\n|    Finished width|"
    , "   " ++ (format usLocale model.width.value) ++ smallunit ++ "|\n"
    , "|+ Shrinkage (" ++ (format usLocale model.widthShrinkage.value) ++ "%)|"
    , "+ " ++ (format usLocale calc.shrinkW) ++ smallunit ++ "|\n"
    , "|+ Take-up (" ++ (format usLocale model.widthTakeup.value) ++ "%)|"
    , "+ " ++ (format usLocale calc.takeupW) ++ smallunit ++ "|\n"
    , "|= Width at reed|"
    , "= " ++ (format usLocale calc.reedWidth) ++ smallunit ++ "|\n"
    , "|× Warp Sett|"
    , "× " ++ (format usLocale model.warpSett.value) ++ endsunit ++ "|\n"
    , "|= Warp ends|"
    , "= " ++ (String.fromInt calc.ends) ++ " ends|\n"
    , (if calc.endsAdjusted /= calc.ends then
          "|+ Adjustment for pattern|" ++ (format intLocale <| toFloat (calc.endsAdjusted - calc.ends)) ++ "|\n"
        else
          ""
      )
    , (if model.floatingSelvedge then 
          "|+ Floating selvedge|+2|\n"
        else
          ""
      )
    , ( if calc.endsFloat /= calc.ends then
           "|= Total warp ends|= " ++ (String.fromInt calc.endsFloat) ++ " ends|\n"
         else
           ""
      )
    , "\n\n|Warp Length|Calculation|\n|:---|---:|\n|    Finished length|" 
    , "   " ++ (format usLocale model.length.value) ++ smallunit ++ "|\n"
    , "|+ Shrinkage (" ++ (format usLocale model.lengthShrinkage.value) ++ "%)|"
    , "+ " ++ (format usLocale calc.shrinkL) ++ smallunit ++ "|\n"
    , "|+ Take-up (" ++ (format usLocale model.lengthTakeup.value) ++ "%)|"
    , "+ " ++ (format usLocale calc.takeupL) ++ smallunit ++ "|\n"
    , "|= Length to weave (relaxed)|" 
    , "= " ++ (format usLocale calc.lengthWeave) ++ smallunit ++ "|\n"
    , "|= Length to weave (under tension)|" 
    , "= " ++ (format usLocale calc.lengthWeaveT) ++ smallunit ++ "|\n"
    , "|+ Fringe|"
    , "+ " ++ (format usLocale calc.fringe) ++ smallunit ++ "|\n"
    , "|= Length of item|"
    , "= " ++ (format usLocale calc.lengthItem) ++ smallunit ++ "|\n"
    , "|× Item count|"
    , "× " ++ (format usLocale model.count.value) ++ "|\n"
    , "|= Length of weaving|"
    , "= " ++ (format usLocale calc.lengthItems) ++ smallunit ++ "|\n"
    , "|+ Loom waste|"
    , "+ " ++ (format usLocale model.loomWaste.value) ++ smallunit ++ "|\n"
    , ( if model.samplingLength.value > 0.0 then
          "|+ Sampling warp|+ " ++ (format usLocale model.samplingLength.value) ++ smallunit ++ "|\n"
        else
          ""
      )
    , ( if model.lengthAdjust.value > 0.0 then
          "|+ Warp length adjustment|+ " ++ (format usLocale model.lengthAdjust.value) ++ smallunit ++ "|\n"
        else
          ""
      )
    , "|= Total warp length|"
    , "= " ++ (format usLocale calc.lengthWarp) ++ smallunit ++ "|\n"
    , "|  |= " ++ (String.fromInt calc.lengthWarpYards) ++ largeunit ++ " " ++ (String.fromInt calc.lengthWarpInches) ++ smallunit ++ "|\n"
    , "|  |= " ++ (String.fromInt calc.lengthWarpAlt) ++ altunit ++ "|\n\n"
    , "|Yarn|Amount|\n|:---|---:|\n|    Total warp ends|"
    , "   " ++ (String.fromInt calc.endsFloat) ++ " ends|\n"
    , "|× Total warp length|"
    , "× " ++ (format usLocale calc.lengthWarp) ++ smallunit ++ "|\n"
    , "|= Total warp yarn required|"
    , "= " ++ (String.fromInt calc.lengthWarpYarnYards) ++ largeunit ++ "|\n"
    , "|    Actual width at reed|"
    , "   " ++ (format usLocale calc.actualReedWidth) ++ smallunit ++ "|\n"
    , picktext
    , "× " ++ (format usLocale model.ppi.value) ++ picksunit ++ "|\n"
    , "|× Length to weave|"
    , "× " ++ (format usLocale (calc.lengthWeave + calc.takeupL)) ++ smallunit ++ "|\n"
    , "|× Item count|"
    , "× " ++ (format usLocale model.count.value) ++ "|\n"
    , ( if model.samplingLength.value > 0.0 then
          "|+ Sampling weft|+ " ++ (format usLocale calc.lengthWeftSample) ++ smallunit ++ "|\n"
        else
          ""
      )
    , "|= Total weft yarn required|"
    , "= " ++ (String.fromInt calc.lengthWeftYarnYards) ++ largeunit ++ "|\n"
    , "|= Total yarn required|"
    , "= " ++ (String.fromInt (calc.lengthWeftYarnYards + calc.lengthWarpYarnYards)) ++ largeunit ++ "|\n"
    , "\n\nClick [Here]("
    , Url.toString <| makeQuery model
    , ") to edit this weave calculation."
    ]


makeQuery : Model -> Url.Url
makeQuery model =
  let
    oldUrl = model.initUrl
    query = String.dropLeft 1 <| UB.toQuery
      [ UB.string "project" model.project
      , UB.string "length" model.length.text
      , UB.string "count" model.count.text
      , UB.string "fringe" model.fringeLength.text
      , UB.string "sample" model.samplingLength.text
      , UB.string "waste" model.loomWaste.text
      , UB.string "ltakeup" model.lengthTakeup.text
      , UB.string "lshrink" model.lengthShrinkage.text
      , UB.string "width" model.width.text
      , UB.string "wtakeup" model.widthTakeup.text
      , UB.string "wshrink" model.widthShrinkage.text
      , UB.string "sett" model.warpSett.text
      , UB.string "floating" (if model.floatingSelvedge then "true" else "false")
      , UB.string "ppi" model.ppi.text
      , UB.string "adjust" model.warpAdjust.text
      , UB.string "warpAdjust" model.lengthAdjust.text
      , UB.string "metric" (if model.metric then "true" else "false")
      ]
  in
    { oldUrl | query = Just query }
      


myOptions : Markdown.Options
myOptions =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = False
    , smartypants = False
    }


viewField : Html Msg -> String -> CheckedField -> (String -> Msg) -> FieldType -> Html Msg
viewField desc unit cf umsg fieldType =
  tr []
  [ td [] [desc]
  , td [] [ input 
            [ type_ (if fieldType == Integer then "number" else "text")
            , onInput umsg
            , value cf.text
            ] []
          , text unit
          ]
  , td [class "issue"] [text <| Maybe.withDefault "" cf.error]
  ]

viewNote : String -> String -> Html Msg
viewNote symbol notetext =
  div [ class "footnote" ]
  [ span [ class "glyph" ] [text symbol]
  , p [] [text notetext]
  ]

viewSymbol : String -> String -> Html Msg
viewSymbol desc symbol =
  span []
  [ text desc
  , sup [] [text symbol]
  ]

view : Model -> Browser.Document Msg
view model =
  let
    smallunit = if model.metric then " cm" else " \""
    endsunit = if model.metric then " epcm" else " epi" 
    picksunit = if model.metric then " ppcm" else " ppi"
    picktext = if model.metric then "Picks per cm:" else "Picks per inch:"
  in
    Browser.Document "Athena Warp & Weft Calculator"
    [ h1 [] [ text "Athena Warp & Weft Calculator" ]
    , text "This calculation can be inserted into a Ravelry post or project "
    , text "note. The 'Copy results to Clipboard' button below puts your "
    , text "calculation onto the clipboard formatted for Ravelry or for any "
    , text "other site that accepts Github-flavored markdown."
    , hr [][]
    , label []
      [ text "Units: US/Imperial "
      , input 
        [ class "toggle-switch"
        , type_ "checkbox"
        , checked model.metric
        , onCheck UnitsChange
        ]
        []
      , text " Metric"
      ]
    , table []
      [ thead []
        [ tr []
          [ th [style "text-align" "left"] [text "Weaving"]
          , th [style "text-align" "right"] [text "Parameters"]
          ]
        ]
      , tbody []
        [ tr []
          [ td [] [text "Project name:"]
          , td []
            [ input 
              [ type_ "text"
              , onInput ProjectChange
              , value model.project
              ] []
            ]
          ]
        , viewField (text "Number of items:") "" model.count CountChange Integer
        , viewField (text "Finished length:") smallunit model.length LengthChange Real
        , viewField (text "Fringe length:") smallunit model.fringeLength FringeChange Real
        , viewField (text "Sampling length:") smallunit model.samplingLength SamplingChange Real
        , viewField (text "Loom waste length:") smallunit model.loomWaste WasteChange Real
        , viewField (text "Length take-up amount:") " %" model.lengthTakeup LTakeupChange Percent
        , viewField (text "Length shrinkage amount:") " %" model.lengthShrinkage LShrinkChange Percent
        , viewField (viewSymbol "Warp length adjustment:" "†") smallunit model.lengthAdjust LengthAdjustChange Real
        , viewField (text "Finished width:") smallunit model.width WidthChange Real
        , viewField (text "Width take-up amount:") " %" model.widthTakeup WTakeupChange Percent
        , viewField (text "Width shrinkage amount:") " %" model.widthShrinkage WShrinkChange Percent
        , viewField (text "Warp sett:") endsunit model.warpSett SettChange Real
        , tr []
          [ td [] [text "Floating selvedge:"]
          , td [] 
            [ input 
              [ type_ "checkbox"
              , onCheck FloatingSelvedgeChange
              , checked model.floatingSelvedge
              ] []
            ]
          ]
        , viewField (viewSymbol "Warp count adjustment:" "‡") "" model.warpAdjust WarpAdjustChange Integer
        , viewField (text picktext) picksunit model.ppi PPIChange Real
        ]
      ]
    , viewNote "†" "If your warping frame or warping mill cannot measure the calculated warp length then add more length here to get to a measurable length."
    , viewNote "‡" "If your design has restrictions on the number of warp ends and the calculated number of warp ends does not meet the restriction then adjust the count up or down until it does."
    , hr [] []
    , div [class "result"]
      ( if isValid model then 
        let
          calc = calculateWarp model
          md = makeMarkup model calc
          q = Url.toString <| makeQuery model
        in
          [ Markdown.toHtmlWith myOptions [] md
          , button 
            [ class "copy-button"
            , attribute "data-clipboard-text" md
            ] 
            [ img [ src "clippy.svg", width 13, class "clippy"] []
            , text "Copy results to Clipboard"
            ]
          , button 
            [ class "copy-button"
            , attribute "data-clipboard-text" q
            ] 
            [ img [ src "clippy.svg", width 13, class "clippy"] []
            , text "Copy link to Clipboard"
            ]
          ]
        else
          [ text "" ]
      )
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


