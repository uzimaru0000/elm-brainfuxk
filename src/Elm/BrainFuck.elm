module BrainFuck exposing (..)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser)


type BCmd
    = NextPtr
    | PrevPtr
    | Increment
    | Decrement
    | Input
    | Output
    | Loop (List BCmd)


parser : Parser (List BCmd)
parser =
    Parser.loop [] <|
        \list ->
            Parser.oneOf
                [ comment
                    |> Parser.map (\_ -> Parser.Loop list)
                , cmdParser
                    |> Parser.map (\cmd -> Parser.Loop (cmd :: list))
                , Parser.end
                    |> Parser.map (\_ -> Parser.Done (List.reverse list))
                ]


cmdParser : Parser BCmd
cmdParser =
    Parser.oneOf
        [ nextPtr
        , prevPtr
        , increment
        , decrement
        , input
        , output
        , loop
        ]


comment : Parser ()
comment =
    let
        isBFToken c =
            [ '+', '-', '>', '<', '.', ',', '[', ']' ]
                |> List.any ((==) c)
    in
    Parser.succeed ()
        |. Parser.chompIf (not << isBFToken)
        |. Parser.chompWhile (not << isBFToken)


nextPtr : Parser BCmd
nextPtr =
    Parser.succeed NextPtr
        |. Parser.symbol ">"


prevPtr : Parser BCmd
prevPtr =
    Parser.succeed PrevPtr
        |. Parser.symbol "<"


increment : Parser BCmd
increment =
    Parser.succeed Increment
        |. Parser.symbol "+"


decrement : Parser BCmd
decrement =
    Parser.succeed Decrement
        |. Parser.symbol "-"


input : Parser BCmd
input =
    Parser.succeed Input
        |. Parser.symbol ","


output : Parser BCmd
output =
    Parser.succeed Output
        |. Parser.symbol "."


loop : Parser BCmd
loop =
    Parser.succeed Loop
        |. Parser.symbol "["
        |= Parser.loop [] loopHelper


loopHelper : List BCmd -> Parser (Parser.Step (List BCmd) (List BCmd))
loopHelper inLoop =
    Parser.oneOf
        [ comment
            |> Parser.map (\_ -> Parser.Loop inLoop)
        , cmdParser
            |> Parser.map (\cmd -> Parser.Loop (cmd :: inLoop))
        , Parser.symbol "]"
            |> Parser.map (\_ -> Parser.Done (List.reverse inLoop))
        , Parser.end
            |> Parser.andThen (\_ -> Parser.problem "invalid loop")
        ]


type alias Runtime =
    { ptr : Int
    , cmds : List BCmd
    , memory : Array Int
    , input : String
    , output : String
    }


type RuntimeMsg
    = Running RunningState
    | Error String
    | Finish Runtime


type RunningState
    = Run Runtime
    | InLoop (List BCmd) RunningState Runtime


getRuntime : RunningState -> Runtime
getRuntime state =
    case state of
        Run rt ->
            rt

        InLoop _ st _ ->
            getRuntime st


update : List BCmd -> RunningState -> RunningState
update tail state =
    case state of
        Run rt ->
            Run { rt | cmds = tail }

        InLoop cmds st rt ->
            InLoop cmds st { rt | cmds = tail }


memorySize : Int
memorySize =
    3000


createRuntime : List BCmd -> Runtime
createRuntime cmds =
    { ptr = 0
    , cmds = cmds
    , memory = Array.initialize memorySize (always 0)
    , input = ""
    , output = ""
    }


step : RunningState -> RuntimeMsg
step state =
    case state of
        Run rt ->
            run rt

        InLoop cmds st rt ->
            runLoop cmds st rt


run : Runtime -> RuntimeMsg
run runtime =
    case runtime.cmds of
        h :: tl ->
            case runCmd h runtime of
                Ok st ->
                    update tl st
                        |> Running

                Err e ->
                    Error e

        [] ->
            Finish runtime


runLoop : List BCmd -> RunningState -> Runtime -> RuntimeMsg
runLoop cmds state runtime =
    case step state of
        Finish rt ->
            case Array.get rt.ptr rt.memory of
                Just val ->
                    if val == 0 then
                        Running <| Run { rt | cmds = runtime.cmds }

                    else
                        case step (Run { rt | cmds = cmds }) of
                            Running st ->
                                Running <|
                                    InLoop cmds st runtime

                            Finish rt_ ->
                                Running <| Run rt_

                            Error e ->
                                Error e

                Nothing ->
                    Error "Out of range"

        Running st ->
            Running <| InLoop cmds st runtime

        Error e ->
            Error e


runCmd : BCmd -> Runtime -> Result String RunningState
runCmd cmd runtime =
    case cmd of
        NextPtr ->
            if runtime.ptr + 1 < memorySize then
                Ok <|
                    Run
                        { runtime
                            | ptr = runtime.ptr + 1
                        }

            else
                Err "Over memory pointer"

        PrevPtr ->
            if runtime.ptr - 1 >= 0 then
                Ok <|
                    Run
                        { runtime
                            | ptr = runtime.ptr - 1
                        }

            else
                Err "Over memory pointer"

        Increment ->
            case Array.get runtime.ptr runtime.memory of
                Just val ->
                    Ok <|
                        Run
                            { runtime
                                | memory = Array.set runtime.ptr (val + 1) runtime.memory
                            }

                Nothing ->
                    Err "Out of range"

        Decrement ->
            case Array.get runtime.ptr runtime.memory of
                Just val ->
                    Ok <|
                        Run
                            { runtime
                                | memory = Array.set runtime.ptr (val - 1) runtime.memory
                            }

                Nothing ->
                    Err "Out of range"

        Loop cmds ->
            case Array.get runtime.ptr runtime.memory of
                Just val ->
                    if val == 0 then
                        Ok <| Run runtime

                    else
                        case step (Run { runtime | cmds = cmds }) of
                            Running st ->
                                Ok <| InLoop cmds st runtime

                            Finish rt ->
                                Ok <| Run rt

                            Error e ->
                                Err e

                Nothing ->
                    Err "Out of range"

        Output ->
            Array.get runtime.ptr runtime.memory
                |> Maybe.map (Char.fromCode >> String.fromChar)
                |> Maybe.map (\x -> { runtime | output = runtime.output ++ x })
                |> Maybe.withDefault runtime
                |> Run
                |> Ok

        _ ->
            Ok <| Run runtime
