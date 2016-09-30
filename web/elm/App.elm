module App exposing (main)


import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Decode exposing ((:=))
import Html exposing (..)
import Html.App as App


-- MAIN


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { numbers : List Int
  , socket : Phoenix.Socket.Socket Msg
  }


init : (Model, Cmd Msg)
init =
  let
    (initialSocket, joinCmd) = initSocket
  in
    ({numbers = [ ], socket = initialSocket}, Cmd.map PhoenixMsg joinCmd)


initSocket : (Phoenix.Socket.Socket Msg, Cmd (Phoenix.Socket.Msg Msg))
initSocket =
  let
    socket =
      Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
        |> Phoenix.Socket.on "new_number" "chart:feed" ReceiveNumber
    channel =
      Phoenix.Channel.init "chart:feed"
  in
    Phoenix.Socket.join channel socket

-- UPDATE


type Msg
  = ReceiveNumber Json.Decode.Value
  | PhoenixMsg (Phoenix.Socket.Msg Msg)


type alias NumberMessage = {number : Int}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveNumber json ->
      case Json.Decode.decodeValue numberMessageDecoder json of
        Ok numberMessage ->
          let
            newNumbers =
              numberMessage.number :: model.numbers
                |> List.take 10
          in
            ({model | numbers = newNumbers}, Cmd.none)
        Err err ->
          (model, Cmd.none)
    PhoenixMsg msg ->
      let
        (newSocket, cmd) = Phoenix.Socket.update msg model.socket
      in
        ({model | socket = newSocket}, Cmd.map PhoenixMsg cmd)


numberMessageDecoder : Json.Decode.Decoder NumberMessage
numberMessageDecoder =
  Json.Decode.object1 NumberMessage ("number" := Json.Decode.int)


-- VIEW


view : Model -> Html Msg
view model =
  div [ ] (viewNumbers model.numbers)


viewNumbers : List Int -> List (Html Msg)
viewNumbers numbers =
  numbers
    |> List.reverse
    |> List.map (\number -> p [ ] [text (toString number)])


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.socket PhoenixMsg
