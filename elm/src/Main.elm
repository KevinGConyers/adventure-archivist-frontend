module Main exposing (..)
import Html exposing(..)
import Http
import Browser
import Json.Decode as Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , int
        , list
        , map
        , string
        )
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Html.Events exposing (onClick)
import String exposing (cons)
import Json.Decode exposing (bool)

type alias ListApiResponse = 
    {
        status: String
        , campaigns: List Campaign
    }
type alias GetApiResponse = 
    {
        status: String
        , campaign: Campaign
    }

type alias Campaign =
    {       id: Int
            , name:String
            , recruitment:Bool
            , judge:String
            , timekeeping:String
            , cadence:String
            , created_at:String
            , updated_at:String
            , last_adventure: String
            }
type alias  Model =
    { campaigns: List Campaign
    , displayCampaign: Campaign
    , errorMessage : Maybe String
    }


view : Model -> Html Msg
view model =
      div []
        [ button [ onClick ListCampaigns ]
            [ text "Get data from server" ]
        , viewCampaignsOrError model
        , viewDisplayCampaign model
        ]

viewCampaignsOrError : Model -> Html Msg
viewCampaignsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message
        Nothing ->
            viewCampaigns model.campaigns
        
viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
    
viewCampaigns : List Campaign -> Html Msg
viewCampaigns campaigns =
    div []
    [ h3 [] [text "Campaigns"]
    , table []
        ( List.map viewCampaignBlurb campaigns )
    ]

viewTableHeader : Campaign -> Html Msg
viewTableHeader campaign =
    tr []
        [ th []
            [ text campaign.name ]
        ]
viewTableBody : Campaign -> List (Html Msg)
viewTableBody campaign =
    [ tr []
        [ td []
            [text campaign.judge]
        , td []
            [text campaign.cadence]
        ]
    , tr []
    [
        td []
            [text campaign.last_adventure]
    ]
    ]
viewCampaignBlurb : Campaign -> Html Msg
viewCampaignBlurb campaign =
    button [onClick (GetCampaignById campaign.id)] 
    [
        table [] 
    
        ( viewTableHeader campaign  :: viewTableBody campaign)
    ]

viewDisplayCampaign : Model -> Html Msg
viewDisplayCampaign model =
    div [] [
        h3 [] [text model.displayCampaign.name]
        , table [] (viewTableBody model.displayCampaign)
    ]
type Msg
    = ListCampaigns
    | ListRecieved (Result Http.Error (ListApiResponse))
    | GetCampaignById (Int)
    | CampaignReceived (Result Http.Error (GetApiResponse))
    
        
listResponseDecoder : Decoder ListApiResponse
listResponseDecoder =
    Decode.succeed ListApiResponse
        |> required "status" string
        |> required "data" (list campaignDecoder)
getResponseDecoder : Decoder GetApiResponse
getResponseDecoder =
    Decode.succeed GetApiResponse
        |> required "status" string
        |> required "data"  campaignDecoder

        
campaignDecoder : Decoder Campaign
campaignDecoder =
    Decode.succeed Campaign
            |> required "id" int
            |> required "name" string
            |> required "recruitment" bool
            |> required "judge" string
            |> required "timekeeping" string
            |> required "cadence" string
            |> required "created_at" string
            |> required "updated_at" string
            |> required "last_adventure" string


    
getCampaigns : Cmd Msg
getCampaigns =
    Http.request {
        method ="GET"
    , body = Http.emptyBody
        , headers =             [ Http.header "Access-Control-Allow-Origin" "http://localhost"
            ]
        , url = "http://localhost:9090/campaigns"
        , expect = Http.expectJson ListRecieved listResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
    }

getCampaignById : Int -> Cmd Msg
getCampaignById id =
    let 
        url_string = "http://localhost:9090/campaigns/" ++ String.fromInt(id)
    in
    Http.request {
        method ="GET"
    , body = Http.emptyBody
        , headers =             [ Http.header "Access-Control-Allow-Origin" "http://localhost"
            ]
        , url = url_string 
        , expect = Http.expectJson CampaignReceived getResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ListCampaigns ->
            (model, getCampaigns)
        ListRecieved (Ok campaignPayload) ->
            ( {model 
                | campaigns = campaignPayload.campaigns
                , errorMessage = Nothing
                }
                , Cmd.none
            )
        ListRecieved (Err httpError) ->
                    ( { model
                        | errorMessage = Just (buildErrorMessage httpError)
                        }
                        , 
                        Cmd.none
                        )
        GetCampaignById (id) ->
            (model, getCampaignById id)
        CampaignReceived (Ok campaignPayload) ->
            ( {model 
                | displayCampaign = campaignPayload.campaign
                , errorMessage = Nothing
                }
                , Cmd.none
            )
        CampaignReceived (Err httpError) ->
                ( { model
                        | errorMessage = Just (buildErrorMessage httpError)
                        }
                        , 
                        Cmd.none
                        )

        
buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message

init : () -> ( Model, Cmd Msg )
init _ =
    ( { campaigns = []
      , displayCampaign = {
        id = -1
            , name = ""
            , recruitment = False
            , judge = ""
            , timekeeping = ""
            , cadence = ""
            , created_at = ""
            , updated_at = ""
            , last_adventure =  ""

      }
      , errorMessage = Nothing
      }
    , getCampaigns
    )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

