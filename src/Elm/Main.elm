module Main exposing (main)

import Array
import BrainFuck exposing (RunningState(..), Runtime, RuntimeMsg(..))
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Parser


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { runtime : Maybe Runtime
    , state : Maybe RunningState
    , error : List Parser.DeadEnd
    , code : String
    , isFinish : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { runtime = Nothing
      , state = Nothing
      , error = []
      , code = ""
      , isFinish = False
      }
    , Cmd.none
    )


type Msg
    = Parse
    | NextStep
    | RunAll
    | InputCode String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            case Parser.run BrainFuck.parser model.code of
                Ok cmds ->
                    ( { model
                        | runtime = Just <| BrainFuck.createRuntime cmds
                        , state = Nothing
                        , isFinish = False
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | error = err
                      }
                    , Cmd.none
                    )

        NextStep ->
            case ( model.runtime, model.state ) of
                ( Just runtime, Nothing ) ->
                    update NextStep { model | state = Just <| Run runtime }

                ( _, Just state ) ->
                    case BrainFuck.step state of
                        Running st ->
                            ( { model
                                | state = Just st
                              }
                            , Cmd.none
                            )

                        Error err ->
                            ( model, Cmd.none )

                        Finish _ ->
                            ( { model
                                | state = Nothing
                                , isFinish = True
                              }
                            , Cmd.none
                            )

                ( Nothing, _ ) ->
                    update Parse model

        RunAll ->
            case ( model.runtime, model.state ) of
                ( Just runtime, Nothing ) ->
                    update RunAll { model | state = Just <| Run runtime }

                ( _, Just state ) ->
                    case run state of
                        Ok (Finish rt) ->
                            ( { model
                                | runtime = Just rt
                                , isFinish = True
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                ( Nothing, _ ) ->
                    update Parse model

        InputCode str ->
            ( { model | code = str }, Cmd.none )


run : RunningState -> Result String RuntimeMsg
run state =
    case BrainFuck.step state of
        Running st ->
            run st

        Error err ->
            Err err

        Finish rt ->
            Ok <| Finish rt


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ class "container mx-auto"
        , class "bg-gray-200"
        , class "p-4"
        , class "flex"
        , class "flex-col"
        , class "space-y-4"
        ]
        [ div
            [ class "text-4xl" ]
            [ text "BrainFuck \u{1F92F}" ]
        , div []
            [ subTitle [] [ text "code" ]
            , codeArea [ onInput InputCode ] [ text model.code ]
            ]
        , div [] <|
            if model.runtime == Nothing then
                [ primitiveButton
                    [ onClick Parse
                    , class "rounded"
                    ]
                    [ text "load" ]
                ]

            else if model.isFinish then
                [ primitiveButton
                    [ onClick Parse
                    , class "rounded"
                    ]
                    [ text "reload" ]
                ]

            else
                [ primitiveButton
                    [ onClick NextStep
                    , class "rounded-l"
                    ]
                    [ text "run" ]
                , primitiveButton
                    [ onClick RunAll
                    , disabled (model.runtime == Nothing)
                    , class "rounded-r"
                    ]
                    [ text "run all" ]
                ]
        , if model.isFinish then
            div
                []
                [ subTitle [] [ text "Result" ]
                , div []
                    [ model.runtime
                        |> Maybe.map .output
                        |> Maybe.withDefault ""
                        |> text
                    ]
                ]

          else
            case
                model.state
                    |> Maybe.map BrainFuck.getRuntime
                    |> Maybe.map memoryView
            of
                Just v ->
                    v

                Nothing ->
                    model.runtime
                        |> Maybe.map memoryView
                        |> Maybe.withDefault (text "")
        ]


subTitle : List (Attribute msg) -> List (Html msg) -> Html msg
subTitle attrs =
    div
        (attrs
            ++ [ class "font-bold"
               , class "text-xl"
               ]
        )


codeArea : List (Attribute msg) -> List (Html msg) -> Html msg
codeArea attrs =
    textarea
        (attrs
            ++ [ class "border"
               , class "rounded"
               , class "resize-x-none"
               , class "focus:shadow-outline"
               , class "focus:outline-none"
               , class "p-1"
               , class "w-full"
               , rows 10
               ]
        )


primitiveButton : List (Attribute msg) -> List (Html msg) -> Html msg
primitiveButton attrs =
    button
        (attrs
            ++ [ class "px-4"
               , class "py-2"
               , class "bg-teal-500"
               , class "hover:bg-teal-700"
               , class "text-white"
               , class "disabled:opacity-50"
               , class "disabled:cursor-not-allowed"
               ]
        )


memoryView : Runtime -> Html msg
memoryView runtime =
    div
        []
        [ subTitle
            []
            [ text "Memory" ]
        , runtime.memory
            |> Array.indexedMap Tuple.pair
            |> Array.map (memoryCell runtime.ptr)
            |> Array.toList
            |> div
                [ class "flex"
                , class "flex-row"
                , class "space-x-4"
                , class "overflow-x-scroll"
                , class "py-4"
                ]
        ]


memoryCell : Int -> ( Int, Int ) -> Html msg
memoryCell currentMem ( i, n ) =
    div
        [ class "shadow"
        , class "min-w-xs"
        , class "p-4"
        , class "bg-white"
        , class "rounded"
        , class "transition-shadow"
        , class "duration-300"
        , class "ease-in-out"
        , classList [ ( "shadow-lg", currentMem == i ) ]
        ]
        [ text <| String.fromInt n ]
