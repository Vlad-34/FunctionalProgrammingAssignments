module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as De
import Model exposing (..)
import Model.Event as Event
import Model.Event.Category as EventCategory
import Model.PersonalDetails as PersonalDetails
import Model.Repo as Repo


type Msg
    = GetRepos
    | GotRepos (Result Http.Error (List Repo.Repo))
    | SelectEventCategory EventCategory.EventCategory
    | DeselectEventCategory EventCategory.EventCategory


getRepos : List Repo.Repo -> List (Cmd Msg)
getRepos repos =
    case repos of
        [] ->
            [ Cmd.none ]

        x :: xs ->
            Http.get
                { url = x.url
                , expect = Http.expectJson GotRepos (De.list Repo.decodeRepo)
                }
                :: getRepos xs


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetRepos ->
            ( model, Cmd.batch (getRepos model.repos) )

        GotRepos _ ->
            ( { model | repos = model.repos }, Cmd.none )

        SelectEventCategory category ->
            ( { model | selectedEventCategories = EventCategory.set category True model.selectedEventCategories }, Cmd.none )

        DeselectEventCategory category ->
            ( { model | selectedEventCategories = EventCategory.set category False model.selectedEventCategories }, Cmd.none )


eventCategoryToMsg : ( EventCategory.EventCategory, Bool ) -> Msg
eventCategoryToMsg ( event, selected ) =
    if selected then
        SelectEventCategory event

    else
        DeselectEventCategory event


repoNoDescriptionJsonStr : String
repoNoDescriptionJsonStr =
    """{
                 "name": "compiler",
                 "description": null,
                 "url": "https://github.com/elm/compiler",
                 "pushedAt": "2021-09-18T14:00:17Z",
                 "stars": 6625
           }"""


view : Model -> Html Msg
view model =
    let
        eventCategoriesView =
            EventCategory.view model.selectedEventCategories |> Html.map eventCategoryToMsg

        eventsView =
            model.events
                |> List.filter (.category >> (\cat -> EventCategory.isEventCategorySelected cat model.selectedEventCategories))
                |> List.map Event.view
                |> div []
                |> Html.map never

        reposView =
            model.repos
                |> Repo.sortByStars
                |> List.take 5
                |> List.map Repo.view
                |> div []
    in
    div []
        [ PersonalDetails.view model.personalDetails
        , h2 [] [ text "Experience" ]
        , eventCategoriesView
        , eventsView
        , h2 [] [ text "My top repos" ]
        , reposView
        ]
