module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Model.Date exposing (..)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


{-| Sorts a list using the compareEvents function
-}
sortByInterval : List Event -> List Event
sortByInterval events =
    List.sortWith compareEvents events


{-| Establishes an order relation between two events.
-}
compareEvents : Event -> Event -> Order
compareEvents e1 e2 =
    Interval.compare e1.interval e2.interval


{-| Events have class event.
The title has class event-title.
The description has class event-description.
The category has class event-category.
The url has class event-url.
If the important field is True, the event is class event-important.
-}
view : Event -> Html Never
view event =
    div
        [ class
            (if event.important == True then
                "event event-important"

             else
                "event"
            )
        ]
        [ h1 [ class "event-title" ] [ text event.title ]
        , p [ class "event-category" ] [ text (Debug.toString event.category) ]
        , div [ class "event-interval" ]
            [ Interval.view event.interval
            ]
        , div [ class "event-description" ] [ event.description ]
        , p [ class "event-url" ] [ text (Debug.toString event.url) ]
        ]


{-| Sorts a list using the comparing function
-}
sortByWith : (a -> comparable) -> (comparable -> comparable -> Order) -> List a -> List a
sortByWith accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list


{-| Establishes an order relation between two comparable elements.
-}
orderBy : (a -> comparable) -> (comparable -> comparable -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
    orderFunc (accessor a) (accessor b)
