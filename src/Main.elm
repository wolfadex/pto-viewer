port module Main exposing
    ( main
    , Pto
    )

{-| An app for comparing PTO within a year, as a means for encouraging people to use their PTO. Especially when you're at a company that has unlimited PTO.

@docs main

@docs Pto

-}

import Browser exposing (Document)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Html
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Ui
import Ui.Color as Color
import Ui.Icon as Icon


{-| -}
main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



---- TYPES ----


type Model
    = Unauthenticated Year
    | Authenticated AuthModel


type alias AuthModel =
    { user : User
    , allPto : Request (Dict Id Pto) String
    , addPto : Maybe Int
    , displayNameForm : Maybe String
    , currentYear : Year
    , showSettings : Bool
    , showStats : Bool
    }


type Request d e
    = NotYetAsked
    | Loading
    | Success d
    | Failure e


type alias User =
    { id : Id
    , name : String
    , email : String
    }


decodeUser : Decoder User
decodeUser =
    Json.Decode.map3 User
        (Json.Decode.field "uid" Json.Decode.string)
        (Json.Decode.field "displayName" Json.Decode.string)
        (Json.Decode.field "email" Json.Decode.string)


type alias Id =
    String


{-| Example

    { name = Nothing
    , years =
        Dict.fromList
            [ ( 2018, 17 )
            , ( 2019, 5 )
            ]
    }

-}
type alias Pto =
    { name : Maybe String
    , years : Dict Year Day
    }


type alias Year =
    Int


type alias Day =
    Int


decodeAllPto : Decoder (Dict Id Pto)
decodeAllPto =
    Json.Decode.dict decodePto


decodePto : Decoder Pto
decodePto =
    Json.Decode.map2 Pto
        (Json.Decode.maybe (Json.Decode.field "name" Json.Decode.string))
        (Json.Decode.field "years" decodePtoYears)


decodePtoYears : Decoder (Dict Year Day)
decodePtoYears =
    Json.Decode.dict decodeYear
        |> Json.Decode.andThen
            (\dictString ->
                Json.Decode.succeed <|
                    Dict.foldl
                        (\k v b ->
                            case String.toInt k of
                                Nothing ->
                                    b

                                Just n ->
                                    Dict.insert n v b
                        )
                        Dict.empty
                        dictString
            )


decodeYear : Decoder Int
decodeYear =
    Json.Decode.field "days" Json.Decode.int


type Msg
    = NoOp
    | LoggedOut
    | Logout
    | LoggedIn User
    | ShowAddPtoForm
    | HideAddPtoForm
    | SetAddPtoDays Int
    | SubmitPto
    | SetPto (Dict Id Pto)
    | ShowNameForm
    | HideNameForm
    | SetName String
    | SubmitName
    | RemoveName
    | ShowSettings
    | HideSettings
    | RefreshData
    | ShowStats
    | HideStats



---- INIT ----


init : Int -> ( Model, Cmd Msg )
init currentYear =
    ( Unauthenticated currentYear, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ loggedOut (\_ -> LoggedOut)
        , login handleLogin
        , setPto handleSetPto
        ]



---- PORTS ----
-- INCOMING


port login : (Value -> msg) -> Sub msg


handleLogin : Value -> Msg
handleLogin value =
    case Json.Decode.decodeValue decodeUser value of
        Ok user ->
            LoggedIn user

        Err _ ->
            NoOp


port loggedOut : (() -> msg) -> Sub msg


port setPto : (Value -> msg) -> Sub msg


handleSetPto : Value -> Msg
handleSetPto value =
    case Json.Decode.decodeValue decodeAllPto value of
        Ok pto ->
            SetPto pto

        Err _ ->
            NoOp



-- OUTGOING


port logout : () -> Cmd msg


port updatePto : ( Id, Value ) -> Cmd msg


port getPto : () -> Cmd msg


port setName : ( Id, String ) -> Cmd msg


port createSelf : Id -> Cmd msg


port removeName : Id -> Cmd msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Unauthenticated data ->
            updateUnauthenticated msg data model

        Authenticated data ->
            updateAuthenticated msg data model


updateUnauthenticated : Msg -> Year -> Model -> ( Model, Cmd Msg )
updateUnauthenticated msg currentYear model =
    case msg of
        --  Auth
        LoggedIn user ->
            ( Authenticated
                { user = user
                , allPto = Loading
                , addPto = Nothing
                , displayNameForm = Nothing
                , currentYear = currentYear
                , showSettings = False
                , showStats = False
                }
            , getPto ()
            )

        -- ALL OTHER
        _ ->
            ( model, Cmd.none )


updateAuthenticated : Msg -> AuthModel -> Model -> ( Model, Cmd Msg )
updateAuthenticated msg authModel model =
    case msg of
        -- Auth
        LoggedOut ->
            ( Unauthenticated authModel.currentYear, Cmd.none )

        Logout ->
            ( model, logout () )

        -- SETTINGS
        ShowSettings ->
            ( Authenticated { authModel | showSettings = True }, Cmd.none )

        HideSettings ->
            ( Authenticated { authModel | showSettings = False }, Cmd.none )

        -- DISPLAY NAME
        ShowNameForm ->
            ( Authenticated
                { authModel
                    | displayNameForm =
                        Just <|
                            case authModel.allPto of
                                Success pto ->
                                    case Dict.get authModel.user.id pto of
                                        Nothing ->
                                            ""

                                        Just { name } ->
                                            Maybe.withDefault "" name

                                _ ->
                                    ""
                }
            , Cmd.none
            )

        HideNameForm ->
            ( Authenticated { authModel | displayNameForm = Nothing }, Cmd.none )

        SetName name ->
            ( Authenticated { authModel | displayNameForm = Just name }, Cmd.none )

        SubmitName ->
            case authModel.displayNameForm of
                Just name ->
                    ( Authenticated { authModel | displayNameForm = Nothing }, setName ( authModel.user.id, name ) )

                Nothing ->
                    ( model, Cmd.none )

        RemoveName ->
            ( model, removeName authModel.user.id )

        -- PTO CRUD
        RefreshData ->
            ( Authenticated { authModel | allPto = Loading }, getPto () )

        ShowAddPtoForm ->
            ( Authenticated { authModel | addPto = Just 1 }, Cmd.none )

        HideAddPtoForm ->
            ( Authenticated { authModel | addPto = Nothing }, Cmd.none )

        SetAddPtoDays days ->
            let
                maxDays =
                    maxDaysInYear authModel.currentYear
            in
            ( Authenticated { authModel | addPto = Just (days |> min maxDays |> max -maxDays) }
            , Cmd.none
            )

        SubmitPto ->
            case authModel.addPto of
                Just days ->
                    ( Authenticated { authModel | addPto = Nothing }
                    , case encodeMyPto authModel days of
                        Nothing ->
                            Cmd.none

                        Just encodedYears ->
                            updatePto ( authModel.user.id, encodedYears )
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetPto pto ->
            ( Authenticated { authModel | allPto = Success pto }
            , case Dict.get authModel.user.id pto of
                Just _ ->
                    Cmd.none

                Nothing ->
                    createSelf authModel.user.id
            )

        ShowStats ->
            ( Authenticated { authModel | showStats = True }, Cmd.none )

        HideStats ->
            ( Authenticated { authModel | showStats = False }, Cmd.none )

        _ ->
            ( model, Cmd.none )


encodeMyPto : AuthModel -> Int -> Maybe Value
encodeMyPto { user, allPto, currentYear } days =
    case allPto of
        Success pto ->
            Just <|
                Json.Encode.object <|
                    let
                        maxDays =
                            maxDaysInYear currentYear
                    in
                    case Dict.get user.id pto of
                        Just { years } ->
                            years
                                |> Dict.update
                                    currentYear
                                    (\d ->
                                        Maybe.withDefault 0 d
                                            + days
                                            |> max 0
                                            |> min maxDays
                                            |> Just
                                    )
                                |> Dict.toList
                                |> List.map
                                    (\( y, d ) ->
                                        ( String.fromInt y, Json.Encode.object [ ( "days", Json.Encode.int d ) ] )
                                    )

                        Nothing ->
                            [ ( String.fromInt currentYear, Json.Encode.object [ ( "days", Json.Encode.int (days |> max 0 |> min maxDays) ) ] ) ]

        _ ->
            Nothing



---- VIEW ----


view : Model -> Document Msg
view model =
    case model of
        Unauthenticated _ ->
            { title = "PTO Viewer - Login"
            , body =
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.padding 16
                    ]
                    viewUnauthenticated
                ]
            }

        Authenticated data ->
            { title = "PTO Viewer"
            , body =
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.padding 16
                    , Element.inFront <|
                        if data.showStats then
                            viewStats data

                        else
                            Element.none
                    , Element.inFront <|
                        case data.addPto of
                            Nothing ->
                                Element.none

                            Just n ->
                                viewAddPto n
                    , Element.inFront <|
                        if data.showSettings then
                            viewSettings data

                        else
                            Element.none
                    , Element.inFront <|
                        case data.displayNameForm of
                            Nothing ->
                                Element.none

                            Just n ->
                                viewNameForm n
                    ]
                    (viewAuthenticated data)
                ]
            }


viewUnauthenticated : Element Msg
viewUnauthenticated =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 800)
        , Element.spacing 16
        ]
        [ Element.el
            [ Region.heading 1
            , Font.size 28
            , Border.solid
            , Border.widthEach
                { bottom = 2
                , left = 0
                , right = 0
                , top = 0
                }
            , Element.width Element.fill
            ]
            (Element.text "PTO Viewer")
        , Element.paragraph
            []
            [ Element.text "It sucks to feel like you're taking too much PTO, when in reality "
            , Element.el [ Font.bold ] <| Element.text "you're probably not taking enough!"
            , Element.text " This app is a way to share how much PTO you're taking so that others know that "
            , Element.el [ Font.bold ] <| Element.text "it's ok to take more."
            ]
        , Element.paragraph
            []
            [ Element.text "Sharing is defaulted to anonymous. You can "
            , Element.el [ Font.bold ] <| Element.text "optionally"
            , Element.text " share your name if you want."
            ]
        , Element.el
            [ Element.centerX
            , Element.width Element.fill
            ]
            (Element.html <| Html.node "firebase-auth" [] [])
        ]


viewAuthenticated : AuthModel -> Element Msg
viewAuthenticated { user, allPto, currentYear } =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 800)
        , Element.height Element.fill
        , Element.spacing 16
        ]
        [ Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.scrollbarY
            ]
            (case allPto of
                NotYetAsked ->
                    [ Element.none ]

                Loading ->
                    [ Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Loading PTO..")
                    ]

                Success pto ->
                    case Dict.toList pto of
                        [] ->
                            [ Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                                (Element.text "No one has uploaded PTO yet. You should be the first!")
                            ]

                        listPto ->
                            [ Element.column
                                [ Element.width Element.fill
                                , Element.spacing 16
                                ]
                                [ Element.row
                                    [ Element.width Element.fill ]
                                    [ Element.text ("Year: " ++ String.fromInt currentYear)
                                    , Ui.button
                                        [ Element.alignRight ]
                                        { onPress = Just RefreshData
                                        , label = Element.text "Refresh"
                                        }
                                    ]
                                , Element.row
                                    [ Element.centerX
                                    ]
                                    (let
                                        ( names, days ) =
                                            listPto
                                                |> List.map (viewPto currentYear user.id)
                                                |> List.unzip
                                     in
                                     [ Keyed.column
                                        [ Border.solid
                                        , Border.widthEach
                                            { bottom = 1
                                            , top = 1
                                            , left = 1
                                            , right = 1
                                            }
                                        ]
                                        (tableHeader "Who" :: names)
                                     , Keyed.column
                                        [ Border.solid
                                        , Border.widthEach
                                            { bottom = 1
                                            , top = 1
                                            , left = 0
                                            , right = 1
                                            }
                                        ]
                                        (tableHeader "Days of PTO" :: days)
                                     ]
                                    )
                                ]
                            ]

                Failure error ->
                    [ Element.el
                        [ Element.centerX
                        , Element.centerY
                        ]
                        (Element.text <| "Failed to load PTO: " ++ error)
                    ]
            )
        , viewSettingsBar
        ]


tableHeader : String -> ( String, Element msg )
tableHeader title =
    ( "header" ++ title
    , Element.el
        [ Border.solid
        , Border.widthEach
            { bottom = 1
            , top = 0
            , left = 0
            , right = 0
            }
        , Element.width Element.fill
        , Element.padding 8
        ]
        (Element.text title)
    )



{-
   Year ____ <- dropdown
   --------------------
   | Who       | Days |
   | Wolfgang  |   23 |
   | Anonymous |    4 |
   | Anonymous |   16 |
   | Will      |   12 |
-}


viewPto : Int -> Id -> ( Id, Pto ) -> ( ( String, Element Msg ), ( String, Element Msg ) )
viewPto year selfId ( ptoId, { name, years } ) =
    ( ( "name-" ++ ptoId
      , Element.el
            [ Element.padding 8 ]
            (Element.text <|
                (if selfId == ptoId then
                    "> "

                 else
                    ""
                )
                    ++ Maybe.withDefault "Anonymous" name
            )
      )
    , ( "days-" ++ ptoId
      , years
            |> Dict.get year
            |> Maybe.withDefault 0
            |> String.fromInt
            |> Element.text
            |> Element.el
                [ Element.alignRight
                , Element.padding 8
                ]
      )
    )


viewAddPto : Int -> Element Msg
viewAddPto days =
    Ui.modal <|
        Element.column
            [ Element.spacing 16 ]
            [ Ui.numberInt
                []
                { onChange = SetAddPtoDays
                , value = days
                , label = Input.labelAbove [] <| Element.paragraph [] [ Element.text "Days of PTO (vacation, sick, etc) to add:" ]
                }
            , Element.row
                [ Element.spacing 16 ]
                [ Ui.button
                    [ Background.color Color.complement ]
                    { onPress = Just HideAddPtoForm
                    , label = Element.text "Cancel"
                    }
                , Ui.button
                    [ Background.color Color.primary ]
                    { onPress = Just SubmitPto
                    , label = Element.text "Submit PTO"
                    }
                ]
            ]


viewNameForm : String -> Element Msg
viewNameForm name =
    Ui.modal <|
        Element.column
            [ Element.spacing 16 ]
            [ Ui.text
                []
                { onChange = SetName
                , value = name
                , label = Input.labelAbove [] <| Element.text "Name for other to see"
                }
            , Element.row
                [ Element.spacing 16 ]
                [ Ui.button
                    [ Background.color Color.complement ]
                    { onPress = Just HideNameForm
                    , label = Element.text "Cancel"
                    }
                , Ui.button
                    [ Background.color Color.primary ]
                    { onPress = Just SubmitName
                    , label = Element.text "Submit Name"
                    }
                ]
            ]


viewSettingsBar : Element Msg
viewSettingsBar =
    Element.row
        [ Element.spacing 16
        , Element.padding 8
        , Element.width Element.fill
        , Border.solid
        , Border.widthEach
            { bottom = 0
            , top = 1
            , left = 0
            , right = 0
            }
        ]
        [ Ui.button
            []
            { onPress = Just ShowAddPtoForm
            , label = Element.text "PTO"
            }
        , Ui.button
            []
            { onPress = Just ShowStats
            , label = Element.text "Stats"
            }
        , Ui.button
            [ Background.color Color.white
            , Element.alignRight
            ]
            { onPress = Just ShowSettings
            , label = Icon.hamburger
            }
        ]


viewSettings : AuthModel -> Element Msg
viewSettings { user, allPto } =
    let
        displayName =
            case allPto of
                Success pto ->
                    getDisplayName user.id pto

                _ ->
                    Nothing
    in
    Ui.modal <|
        Element.column
            [ Element.spacing 16
            , Element.padding 8
            ]
            [ Element.paragraph
                []
                [ label "Logged In As"
                , Element.text user.name
                ]
            , Element.paragraph
                []
                [ label "Email"
                , Element.text user.email
                ]
            , Element.paragraph
                []
                [ label "Display Name"
                , Element.text <| Maybe.withDefault "Anonymous" displayName
                ]
            , case displayName of
                Just _ ->
                    Ui.button
                        []
                        { onPress = Just RemoveName
                        , label = Element.text "Remove Display Name"
                        }

                Nothing ->
                    Ui.button
                        []
                        { onPress = Just ShowNameForm
                        , label = Element.text "Set Display Name"
                        }
            , Element.el
                [ Border.solid
                , Border.widthEach
                    { left = 0
                    , right = 0
                    , top = 1
                    , bottom = 0
                    }
                , Element.width Element.fill
                ]
                Element.none
            , Element.newTabLink
                [ Font.color Color.linkBlue ]
                { url = "https://github.com/wolfadex/pto-viewer"
                , label =
                    Element.row
                        []
                        [ Element.image
                            [ Element.height (Element.px 28) ]
                            { src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"
                            , description = "GitHub logo"
                            }
                        , Element.text "GitHub repo"
                        ]
                }
            , Element.row
                [ Element.width Element.fill ]
                [ Element.newTabLink
                    [ Font.size 12
                    , Font.color Color.linkBlue
                    ]
                    { url = "/tos.html"
                    , label = Element.text "Terms of Service"
                    }
                , Element.newTabLink
                    [ Font.size 12
                    , Font.color Color.linkBlue
                    , Element.alignRight
                    ]
                    { url = "/privacypolicy.html"
                    , label = Element.text "Privacy Policy"
                    }
                ]
            , Element.el [ Element.height (Element.px 32) ] Element.none
            , Element.row
                [ Element.width Element.fill ]
                [ Ui.button
                    [ Background.color Color.complement ]
                    { onPress = Just HideSettings
                    , label = Element.text "Cancel"
                    }
                , Ui.button
                    [ Background.color Color.secondary2
                    , Element.alignRight
                    ]
                    { onPress = Just Logout
                    , label = Element.text "Logout"
                    }
                ]
            ]


getDisplayName : Id -> Dict Id Pto -> Maybe String
getDisplayName selfId =
    Dict.get selfId >> Maybe.andThen (\{ name } -> name)


label : String -> Element msg
label str =
    Element.el [ Font.bold ] (Element.text (str ++ ": "))


maxDaysInYear : Year -> Int
maxDaysInYear year =
    if modBy 4 year == 0 then
        if modBy 100 year == 0 then
            if modBy 400 year == 0 then
                366

            else
                365

        else
            366

    else
        365


viewStats : AuthModel -> Element Msg
viewStats { allPto, currentYear, user } =
    let
        ( listOfDays, yourPto ) =
            case allPto of
                Success pto ->
                    ( pto
                        |> Dict.toList
                        |> List.map (Tuple.second >> .years >> Dict.get currentYear >> Maybe.withDefault 0)
                    , pto
                        |> Dict.get user.id
                        |> Maybe.map (.years >> Dict.get currentYear >> Maybe.withDefault 0)
                        |> Maybe.withDefault 0
                    )

                _ ->
                    ( [], 0 )
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color Color.white
        , Element.padding 16
        , Element.spacing 16
        ]
        [ Element.column
            [ Element.spacing 16
            , Element.height Element.fill
            ]
            [ Element.paragraph
                [ Border.solid
                , Border.widthEach
                    { bottom = 1
                    , top = 0
                    , left = 0
                    , right = 0
                    }
                ]
                [ label "Your PTO"
                , yourPto
                    |> String.fromInt
                    |> addDays
                    |> Element.text
                ]
            , Element.paragraph
                []
                [ label "Mean"
                , (List.sum listOfDays // List.length listOfDays)
                    |> String.fromInt
                    |> addDays
                    |> Element.text
                ]
            , Element.paragraph
                []
                [ label "Median"
                , listOfDays
                    |> List.sort
                    |> List.drop (List.length listOfDays // 2)
                    |> List.head
                    |> Maybe.withDefault 0
                    |> String.fromInt
                    |> addDays
                    |> Element.text
                ]
            , Element.paragraph
                []
                [ label "Range"
                , let
                    largest =
                        listOfDays
                            |> List.maximum
                            |> Maybe.withDefault 0

                    smallest =
                        listOfDays
                            |> List.minimum
                            |> Maybe.withDefault 0
                  in
                  (largest - smallest)
                    |> String.fromInt
                    |> addDays
                    |> Element.text
                ]
            ]
        , Ui.button
            [ Element.alignRight
            , Background.color Color.complement
            ]
            { onPress = Just HideStats
            , label = Element.text "Back"
            }
        ]


addDays : String -> String
addDays value =
    value ++ " Days"
