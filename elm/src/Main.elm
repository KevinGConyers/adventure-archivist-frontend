module Main exposing(..)
import Html exposing(..)
import Html.Events exposing (onClick)
import Http
import Browser
import Campaign as C exposing(..)

type Model 
    = Loading 
    | LoadedCampaign Campaign
    | LoadedCampaigns (List Campaign)
    | Success Campaign
    | Success (List Campaign)
    | Error Problem


view : Model -> Html Msg
view model =
      div []
        [ button [ onClick C.ListCampaigns ]
            [ text "Get data from server" ]
        , viewCampaignsOrError model
        , C.viewDisplayCampaign model
        ]

viewCampaignsOrError : Model -> Html Msg
viewCampaignsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message
        Nothing ->
            C.viewCampaigns model.campaigns
        
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
        
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        C.ListCampaigns ->
            (model, C.getCampaigns)
        C.ListRecieved (Ok campaignPayload) ->
            ( {model 
                | campaigns = campaignPayload.campaigns
                , errorMessage = Nothing
                }
                , Cmd.none
            )
        C.ListRecieved (Err httpError) ->
                    ( { model
                        | errorMessage = Just (buildErrorMessage httpError)
                        }
                        , 
                        Cmd.none
                        )
        C.GetCampaignById (id) ->
            (model, C.getCampaignById id)
        C.CampaignReceived (Ok campaignPayload) ->
            ( {model 
                | displayCampaign = campaignPayload.campaign
                , errorMessage = Nothing
                }
                , Cmd.none
            )
        C.CampaignReceived (Err httpError) ->
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


