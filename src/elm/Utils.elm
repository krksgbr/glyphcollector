module Utils exposing (send, timeout, timeoutMsg)

import Process
import Task exposing (Task)


send : a -> Cmd a
send msg =
    Task.perform (always msg) (Task.succeed ())


timeout : Float -> (Result a b -> msg) -> Task a b -> Cmd msg
timeout time msg task =
    Process.sleep time
        |> Task.andThen (always task)
        |> Task.attempt msg


timeoutMsg : Float -> msg -> Cmd msg
timeoutMsg time msg =
    timeout time (always msg) (Task.succeed ())
