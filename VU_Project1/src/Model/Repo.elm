module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as Dec


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


{-| Repos have class repo.
The name has class repo-name.
The description has class repo-description.
The url has class repo-url, and contain an a tag with a href attribute that links to the repo.
The stars have class repo-stars.
-}
view : Repo -> Html msg
view repo =
    div [ class "repo" ]
        [ p [ class "repo-name" ] [ text <| "Name:" ++ repo.name ]
        , p [ class "repo-description" ] [ text <| "Description:" ++ Maybe.withDefault "" repo.description ]
        , div [ class "repo-url" ] [ p [] [ text "URL: " ], a [ href repo.url ] [ text repo.url ] ]
        , p [ class "repo-stars" ] [ text <| "Stars:" ++ String.fromInt repo.stars ]
        ]


{-| Sorts a list of repos by stars.
-}
sortByStars : List Repo -> List Repo
sortByStars repos =
    List.reverse (List.sortBy .stars repos)


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : Dec.Decoder Repo
decodeRepo =
    Dec.map5 Repo
        (Dec.field "name" Dec.string)
        (Dec.field "description" (Dec.nullable Dec.string))
        (Dec.field "html_url" Dec.string)
        (Dec.field "pushed_at" Dec.string)
        (Dec.field "stargazers_count" Dec.int)
