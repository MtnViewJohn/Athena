import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Browser
import Maybe.Extra exposing (isNothing)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Url.Builder as UB
import Url
import Dict exposing (Dict)
import Markdown

main =
  Browser.element
  { init = initModel
  , view = view
  , update = update
  , subscriptions = subscriptions
  }




-- MODEL

type alias CheckedField =
  { value : Float
  , text : String
  , error : Maybe String
  , convertFn : String -> Result String Float
  }


initField : String -> Maybe String -> (String -> Result String Float) -> CheckedField
initField defs urls cvt =
  let
    s = Maybe.withDefault defs urls
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
  { length : CheckedField
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
  , initUrl : Url.Url
  }

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


initModel : String -> (Model, Cmd Msg)
initModel initUrl = 
  let
    murl = Url.fromString initUrl
    defUrl = Url.Url Url.Https "" Nothing "/" Nothing Nothing
    url = Maybe.withDefault defUrl murl
    qd = case url.query of
      Nothing -> Dict.empty
      Just q -> parseParams q
  in
    ( { length                = initField "30" (getQueryPart "length" qd) positive
      , count                 = initField "1"  (getQueryPart "count" qd) positive
      , fringeLength          = initField "0"  (getQueryPart "fringe" qd) notNegative
      , samplingLength        = initField "0"  (getQueryPart "sample" qd) notNegative
      , loomWaste             = initField "24" (getQueryPart "waste" qd) notNegative
      , lengthTakeup          = initField "10" (getQueryPart "ltakeup" qd) notNegative
      , lengthShrinkage       = initField "10" (getQueryPart "lshrink" qd) notNegative
      , width                 = initField "10" (getQueryPart "width" qd) positive
      , widthTakeup           = initField "10" (getQueryPart "wtakeup" qd) notNegative
      , widthShrinkage        = initField "10" (getQueryPart "wshrink" qd) notNegative
      , warpSett              = initField "10" (getQueryPart "sett" qd) positive
      , floatingSelvedge      = getQueryBool "floating" qd
      , ppi                   = initField "10" (getQueryPart "ppi" qd) positive
      , warpAdjust            = initField "0"  (getQueryPart "adjust" qd) isInteger
      , initUrl               = url
      } 
    , Cmd.none
    )

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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LengthChange l -> ({model | length = updateField model.length l}, Cmd.none)
    CountChange c -> ({model | count = updateField model.count c}, Cmd.none)
    FringeChange l -> ({model | fringeLength = updateField model.fringeLength l}, Cmd.none)
    SamplingChange l -> ({model | samplingLength = updateField model.samplingLength l}, Cmd.none)
    WasteChange l -> ({model | loomWaste = updateField model.loomWaste l}, Cmd.none)
    LTakeupChange l -> ({model | lengthTakeup = updateField model.lengthTakeup l}, Cmd.none)
    LShrinkChange l -> ({model | lengthShrinkage = updateField model.lengthShrinkage l}, Cmd.none)
    WidthChange w -> ({model | width = updateField model.width w}, Cmd.none)
    WTakeupChange w -> ({model | widthTakeup = updateField model.widthTakeup w}, Cmd.none)
    WShrinkChange w -> ({model | widthShrinkage = updateField model.widthShrinkage w}, Cmd.none)
    SettChange s -> ({model | warpSett = updateField model.warpSett s}, Cmd.none)
    FloatingSelvedgeChange f -> ({model | floatingSelvedge = f}, Cmd.none)
    PPIChange p -> ({model | ppi = updateField model.ppi p}, Cmd.none)
    WarpAdjustChange w -> ({model | warpAdjust = updateField model.warpAdjust w}, Cmd.none)

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
  , lengthWarpYarn : Float
  , lengthWarpYarnYards : Int
  , lengthWeftYarn : Float
  , lengthWeftYarnYards : Int
  }

roundf : Float -> Float
roundf f =
  toFloat <| floor (f + 0.9)

calculateWarp : Model -> Calculation
calculateWarp model =
  let
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
        Basics.max 0 model.fringeLength.value - 6    -- steal up to 6" fring from waste, unless sampling
      else
        model.fringeLength.value
    backFringe = Basics.max 0 model.fringeLength.value - 6
    lengthItem = lengthWeaveT + fringe
    lengthItems = roundf <| lengthItem * model.count.value - fringe + frontFringe + backFringe  -- loom waste is the outermost fringe
    lengthWarp = lengthItems + model.loomWaste.value + model.samplingLength.value
    lengthWarpYards = floor (lengthWarp / 36.0)
    lengthWarpInches = floor lengthWarp - (36 * lengthWarpYards)
    lengthWarpYarn = lengthWarp * (toFloat endsFloat)
    lengthWarpYarnYards = floor ((lengthWarpYarn / 36.0) + 0.9)
    lengthWeftYarn = actualReedWidth * model.ppi.value * 
                    (lengthWeaveT * model.count.value + model.samplingLength.value)
    lengthWeftYarnYards = floor ((lengthWeftYarn / 36.0) + 0.9)
  in
    Calculation shrinkW shrunkWidth takeupW reedWidth ends endsAdjusted endsFloat
                actualReedWidth shrinkL lengthWeave lengthWeaveT takeupL fringe lengthItem
                lengthItems lengthWarp lengthWarpYards lengthWarpInches lengthWarpYarn
                lengthWarpYarnYards lengthWeftYarn lengthWeftYarnYards

makeMarkup : Model -> Calculation -> String
makeMarkup model calc = 
  String.concat
  [ "|Warp Ends|Calculation|\n|:---|---:|\n|    Finished width|"
  , "   " ++ (format usLocale model.width.value) ++ "\"|\n"
  , "|+ Shrinkage (" ++ (format usLocale model.widthShrinkage.value) ++ "%)|"
  , "+ " ++ (format usLocale calc.shrinkW) ++ "\"|\n"
  , "|+ Take-up (" ++ (format usLocale model.widthTakeup.value) ++ "%)|"
  , "+ " ++ (format usLocale calc.takeupW) ++ "\"|\n"
  , "|= Width at reed|"
  , "= " ++ (format usLocale calc.reedWidth) ++ "\"|\n"
  , "|× Warp Sett|"
  , "× " ++ (format usLocale model.warpSett.value) ++ " epi|\n"
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
  , "   " ++ (format usLocale model.length.value) ++ "\"|\n"
  , "|+ Shrinkage (" ++ (format usLocale model.lengthShrinkage.value) ++ "%)|"
  , "+ " ++ (format usLocale calc.shrinkL) ++ "\"|\n"
  , "|+ Take-up (" ++ (format usLocale model.lengthTakeup.value) ++ "%)|"
  , "+ " ++ (format usLocale calc.takeupL) ++ "\"|\n"
  , "|= Length to weave (relaxed)|" 
  , "= " ++ (format usLocale calc.lengthWeave) ++ "\"|\n"
  , "|= Length to weave (under tension)|" 
  , "= " ++ (format usLocale calc.lengthWeaveT) ++ "\"|\n"
  , "|+ Fringe|"
  , "+ " ++ (format usLocale calc.fringe) ++ "\"|\n"
  , "|= Length of item|"
  , "= " ++ (format usLocale calc.lengthItem) ++ "\"|\n"
  , "|× Item count|"
  , "× " ++ (format usLocale model.count.value) ++ "|\n"
  , "|= Length of weaving|"
  , "= " ++ (format usLocale calc.lengthItems) ++ "\"|\n"
  , "|+ Loom waste|"
  , "+ " ++ (format usLocale model.loomWaste.value) ++ "\"|\n"
  , ( if model.samplingLength.value > 0.0 then
        "|+ Sampling|+ " ++ (format usLocale model.samplingLength.value) ++ "\"|\n"
      else
        ""
    )
  , "|= Total warp length|"
  , "= " ++ (format usLocale calc.lengthWarp) ++ "\"|\n"
  , "|  |= " ++ (String.fromInt calc.lengthWarpYards) ++ " yards " ++ (String.fromInt calc.lengthWarpInches) ++ "\"|\n\n"
  , "|Yarn|Amount|\n|:---|---:|\n|    Total warp ends|"
  , "   " ++ (String.fromInt calc.endsFloat) ++ " ends|\n"
  , "|× Total warp length|"
  , "× " ++ (format usLocale calc.lengthWarp) ++ "\"|\n"
  , "|= Total warp yarn required|"
  , "= " ++ (String.fromInt calc.lengthWarpYarnYards) ++ " yards|\n"
  , "|    Actual width at reed|"
  , "   " ++ (format usLocale calc.actualReedWidth) ++ "\"|\n"
  , "|× Picks per inch|"
  , "× " ++ (format usLocale model.ppi.value) ++ " ppi|\n"
  , "|× Length to weave|"
  , "× " ++ (format usLocale (calc.lengthWeave + calc.takeupL)) ++ "\"|\n"
  , "|× Item count|"
  , "× " ++ (format usLocale model.count.value) ++ "|\n"
  , "|= Total weft yarn required|"
  , "= " ++ (String.fromInt calc.lengthWeftYarnYards) ++ " yards|\n"
  ]


makeQuery : Model -> Url.Url
makeQuery model =
  let
    oldUrl = model.initUrl
    query = String.dropLeft 1 <| UB.toQuery
      [ UB.string "length" model.length.text
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


viewField : String -> CheckedField -> (String -> Msg) -> FieldType -> Html Msg
viewField desc cf umsg fieldType =
  tr []
  [ td [] [text desc]
  , td [] [ input 
            [ type_ (if fieldType == Integer then "number" else "text")
            , onInput umsg
            , value cf.text
            ] []
          , text <| if fieldType == Percent then "%" else ""
          ]
  , td [class "issue"] [text <| Maybe.withDefault "" cf.error]
  ]


view : Model -> Html Msg
view model =
  div []
  [ h1 [] [ text "Warp Calculator" ]
  , table []
    [ thead []
      [ tr []
        [ th [style "text-align" "left"] [text "Weaving"]
        , th [style "text-align" "right"] [text "Parameters"]
        ]
      ]
    , tbody []
      [ viewField "Number of items:" model.count CountChange Integer
      , viewField "Finished length:" model.length LengthChange Real
      , viewField "Fringe length:" model.fringeLength FringeChange Real
      , viewField "Sampling length:" model.samplingLength SamplingChange Real
      , viewField "Loom waste length:" model.loomWaste WasteChange Real
      , viewField "Length take-up amount:" model.lengthTakeup LTakeupChange Percent
      , viewField "Length shrinkage amount:" model.lengthShrinkage LShrinkChange Percent
      , viewField "Finished width:" model.width WidthChange Real
      , viewField "Width take-up amount:" model.widthTakeup WTakeupChange Percent
      , viewField "Width shrinkage amount:" model.widthShrinkage WShrinkChange Percent
      , viewField "Warp sett:" model.warpSett SettChange Real
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
      , viewField "Warp count adjustment:" model.warpAdjust WarpAdjustChange Integer
      , viewField "Picks per inch:" model.ppi PPIChange Real
      ]
    ]
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
          , text "Copy Mark-up to Clipboard"
          ]
        , button 
          [ class "copy-button"
          , attribute "data-clipboard-text" q
          ] 
          [ img [ src "clippy.svg", width 13, class "clippy"] []
          , text "Copy parameters-url to Clipboard"
          ]
        ]
      else
        [ text "" ]
    )
  ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none


