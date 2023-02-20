module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, id)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }


{-| The name is in a h1 tag and id name.
The intro is in an em tag and id intro.
Each contact detail has class contact-detail.
Each social media link has class social-link and use the a tag with a href attribute for the links.
-}
view : PersonalDetails -> Html msg
view details =
    div []
        [ h1 [ id "name" ] [ text details.name ]
        , em [ id "intro" ] [ text details.intro ]
        , List.map (\l -> li [ class "contact-detail" ] [ text (l.name ++ ": " ++ l.detail) ]) details.contacts |> p []
        , List.map (\l -> li [ class "social-link" ] [ a [ href l.detail ] [ text l.name ] ]) details.socials |> p []
        ]
