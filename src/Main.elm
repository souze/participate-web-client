port module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Browser.Navigation
import Dict
import Element exposing (..)
import Element.Font as Font
import Element.Input
import Expect
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Test
import Test.Runner.Failure exposing (Reason(..))
import Url
import Url.Parser as UrlParser exposing ((</>))



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- PORTS
-- From ELM


port tryConnect : String -> Cmd msg


port sendMessage : String -> Cmd msg



-- To ELM


port connectionEstablished : (String -> msg) -> Sub msg


port connectionClosed : (String -> msg) -> Sub msg


port connectionFailure : (String -> msg) -> Sub msg


port messageReceiver : (String -> msg) -> Sub msg


tryConnectToChannel : String -> Cmd msg
tryConnectToChannel channel =
    tryConnect ("ws://participate-server.herokuapp.com/channel/" ++ channel)



-- MODEL


type Model
    = NotConnected String
    | Connected ConnectedState


type alias ConnectedState =
    { draft : String
    , messages : List String
    , reactions : List ( String, Int )
    , showDebug : Bool
    , customReaction : CustomReaction
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        route : Maybe Route
        route =
            UrlParser.parse routeParser url
    in
    case route of
        Nothing ->
            ( NotConnected "Please input channel url (TODO add channel selection here)", Cmd.none )

        Just (Topic name) ->
            ( NotConnected "Landed on a topic, nice", Cmd.none )

        Just (Channel name) ->
            ( NotConnected ("Trying to connect to " ++ name), tryConnectToChannel name )


genUrlString : Url.Url -> String
genUrlString url =
    UrlParser.parse routeParser url
        |> Maybe.map genTopicStr
        |> Maybe.withDefault "Url parsing failed"


genTopicStr : Route -> String
genTopicStr r =
    case r of
        Topic s ->
            "Topic: " ++ s

        Channel name ->
            "Channel: " ++ name


type Route
    = Topic String
    | Channel String


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Topic (UrlParser.s "topic" </> UrlParser.string)
        , UrlParser.map Channel (UrlParser.s "client" </> UrlParser.s "channel" </> UrlParser.string)
        ]


justConnectedState : Model
justConnectedState =
    Connected { draft = "", messages = [], reactions = [], showDebug = False, customReaction = emptyCustomReaction }


emptyCustomReaction : CustomReaction
emptyCustomReaction =
    { showText = False, text = "", focused = False }


type alias CustomReaction =
    { showText : Bool
    , text : String
    , focused : Bool
    }


type alias ReactionWithDuration =
    { reaction : String
    , duration : Int
    }



-- UPDATE


{-| Should encode to:

    {"React":{"reaction":"😊","duration":10}}

-}
jsonEncodeReaction : ReactionWithDuration -> String
jsonEncodeReaction reaction =
    Json.Encode.encode 0 <|
        Json.Encode.object
            [ ( "React"
              , Json.Encode.object
                    [ ( "reaction", Json.Encode.string reaction.reaction )
                    , ( "duration", Json.Encode.int reaction.duration )
                    ]
              )
            ]


type Msg
    = DraftChanged String
    | CustomTextChanged String
    | SendTypedText
    | SendReaction ReactionWithDuration
    | ShowCustomReactionTextField
    | Recv String
    | KeyboardEventReceived KeyboardEvent
    | OnWebsocketConnected String
    | OnWebsocketDisconnected String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( _, UrlChanged _ ) ->
            ( model, Cmd.none )

        ( _, LinkClicked _ ) ->
            ( model, Cmd.none )

        ( NotConnected _, OnWebsocketConnected _ ) ->
            ( justConnectedState, Cmd.none )

        ( _, OnWebsocketDisconnected reason ) ->
            ( NotConnected reason, Cmd.none )

        ( NotConnected m, _ ) ->
            ( model, Cmd.none )

        ( Connected m, _ ) ->
            Tuple.mapFirst Connected <| updateConnected msg m


updateConnected : Msg -> ConnectedState -> ( ConnectedState, Cmd Msg )
updateConnected msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }
            , Cmd.none
            )

        CustomTextChanged text ->
            ( setCustomReactionText text model
            , Cmd.none
            )

        SendTypedText ->
            ( { model | draft = "" }
            , sendMessage <| jsonEncodeReaction { reaction = model.draft, duration = 5 }
            )

        SendReaction r ->
            ( model
            , sendMessage <| jsonEncodeReaction r
            )

        Recv message ->
            ( { model
                | messages = model.messages ++ [ message ]
                , reactions = parse_reactions message
              }
            , Cmd.none
            )

        ShowCustomReactionTextField ->
            ( { model | customReaction = { showText = True, text = model.customReaction.text, focused = False } }, Cmd.none )

        KeyboardEventReceived e ->
            case e.key of
                Just "~" ->
                    ( { model | showDebug = not model.showDebug }, Cmd.none )

                Just "Enter" ->
                    ( { model | customReaction = emptyCustomReaction }
                    , sendMessage <| jsonEncodeReaction { reaction = model.customReaction.text, duration = 10 }
                    )

                _ ->
                    ( model, Cmd.none )

        OnWebsocketConnected _ ->
            ( model, Cmd.none )

        OnWebsocketDisconnected _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )


setCustomReactionText : String -> ConnectedState -> ConnectedState
setCustomReactionText t m =
    { m | customReaction = setCustomReactionText_ t m.customReaction }


setCustomReactionText_ : String -> CustomReaction -> CustomReaction
setCustomReactionText_ t cr =
    { cr | text = t }


parse_reactions : String -> List ( String, Int )
parse_reactions s =
    D.decodeString active_reactions_decoder s
        |> Result.withDefault emptyActiveReactions
        |> .reactions


test_active_reactions_decoder : Test.Test
test_active_reactions_decoder =
    Test.describe "test json parsing"
        [ Test.test "empty reactions" <|
            \_ ->
                Expect.equal
                    (Ok { reactions = [] })
                    (D.decodeString active_reactions_decoder """{"reactions":{}}""")
        , Test.test "sample reactions" <|
            \_ ->
                Expect.equal
                    (Ok { reactions = [ ( "frown", 3 ), ( "smil", 1 ) ] })
                    (D.decodeString active_reactions_decoder
                        """{"reactions":{"smil":1,"frown":3}}"""
                    )
        ]


active_reactions_decoder : D.Decoder ActiveReactions
active_reactions_decoder =
    D.map ActiveReactions (D.field "reactions" reactions_decoder)


reactions_decoder : D.Decoder (List ( String, Int ))
reactions_decoder =
    D.map Dict.toList (D.dict D.int)


type alias ActiveReactions =
    { reactions : List ( String, Int )
    }


emptyActiveReactions : ActiveReactions
emptyActiveReactions =
    ActiveReactions []



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ messageReceiver Recv
        , connectionEstablished OnWebsocketConnected
        , connectionClosed OnWebsocketDisconnected
        , connectionFailure OnWebsocketDisconnected
        , onKeyPress <| D.map KeyboardEventReceived decodeKeyboardEvent
        ]



-- VIEW


view : Model -> Browser.Document Msg
view m =
    { title = "Channel happy"
    , body =
        [ case m of
            NotConnected text ->
                Element.layout [] <| Element.text ("Not connected: " ++ text)

            Connected connectedState ->
                Element.layout [] (mainLayout connectedState)
        ]
    }


mainLayout : ConnectedState -> Element Msg
mainLayout m =
    column [ Element.spacing 500, Element.padding 20 ]
        [ reactionLayout m
        , maybeViewDebugFields m
        ]


maybeViewDebugFields : ConnectedState -> Element Msg
maybeViewDebugFields m =
    if m.showDebug then
        viewDebugFields m

    else
        Element.text ""


reactionLayout : ConnectedState -> Element Msg
reactionLayout m =
    column [ Element.spacing 20 ] <| viewReactionList m.reactions



-- Not currently used. Will show a button that can be used to add new emotes --


viewCustom : ConnectedState -> Element Msg
viewCustom m =
    if m.customReaction.showText then
        viewCustomText m

    else
        viewCustomButton


viewCustomText : ConnectedState -> Element Msg
viewCustomText m =
    Element.Input.text []
        { onChange = CustomTextChanged
        , text = m.customReaction.text
        , placeholder = Nothing
        , label = Element.Input.labelHidden ""
        }


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnterKey keyCode =
            if keyCode == 13 then
                D.succeed msg

            else
                D.fail "silent failure :)"
    in
    on "keyup" <|
        D.andThen isEnterKey Html.Events.keyCode


viewCustomButton : Element Msg
viewCustomButton =
    Element.Input.button [ Element.height (px 30), Font.size 20 ]
        { onPress = Just ShowCustomReactionTextField
        , label = Element.text "Custom"
        }


viewReactionList : List ( String, Int ) -> List (Element Msg)
viewReactionList =
    List.map viewReaction


viewReaction : ( String, Int ) -> Element Msg
viewReaction ( s, i ) =
    Element.Input.button [ Element.height (px (70 + i * 15)), Font.size (50 + i * 15) ]
        { onPress = Just (SendReaction { reaction = s, duration = 10 })
        , label = Element.text (s ++ " " ++ String.fromInt i)
        }


viewDebugFields : ConnectedState -> Element Msg
viewDebugFields m =
    column [] <|
        [ Element.text "Literal text send"
        , Element.text ""
        ]
            ++ viewReceivedMessageList m
            ++ [ Element.Input.text []
                    { onChange = DraftChanged
                    , text = m.draft
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden ""
                    }
               , Element.Input.button []
                    { onPress = Just SendTypedText
                    , label = Element.text "Send"
                    }
               ]


viewReceivedMessageList : ConnectedState -> List (Element msg)
viewReceivedMessageList m =
    List.map ((++) "* " >> Element.text) m.messages



{--view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "Echo Chat Chit x" ]
    , ul []
        (List.map (\msg -> li [] [ text msg ]) model.messages)
    , input
        [ type_ "text"
        , placeholder "Draft"
        , onInput DraftChanged
        , on "keydown" (ifIsEnter Send)
        , value model.draft
        ]
        []
    , button [ onClick Send ] [ text "Send now pls" ]
    ]--}
-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )
