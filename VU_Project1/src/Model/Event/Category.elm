module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, noneSelected, set, view)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories : List EventCategory
eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type alias SelectedEventCategories =
    { isAcademic : Bool, isWork : Bool, isProject : Bool, isAward : Bool }


{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected =
    { isAcademic = True, isWork = True, isProject = True, isAward = True }


{-| Returns an instance of `SelectedEventCategories` with no categories selected

-- isEventCategorySelected Academic noneSelected --> False

-}
noneSelected : SelectedEventCategories
noneSelected =
    { isAcademic = False, isWork = False, isProject = False, isAward = False }


{-| Given a current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
    case category of
        Academic ->
            if current.isAcademic == True then
                True

            else
                False

        Work ->
            if current.isWork == True then
                True

            else
                False

        Project ->
            if current.isProject == True then
                True

            else
                False

        Award ->
            if current.isAward == True then
                True

            else
                False


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value selected =
    case category of
        Academic ->
            { isAcademic = value, isWork = selected.isWork, isProject = selected.isProject, isAward = selected.isAward }

        Work ->
            { isAcademic = selected.isAcademic, isWork = value, isProject = selected.isProject, isAward = selected.isAward }

        Project ->
            { isAcademic = selected.isAcademic, isWork = selected.isWork, isProject = value, isAward = selected.isAward }

        Award ->
            { isAcademic = selected.isAcademic, isWork = selected.isWork, isProject = selected.isProject, isAward = value }


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state ] []
        , text name
        ]


{-| It shows 4 checkboxes using the given checkbox function.
-}
view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
    div []
        [ checkbox "Academic" model.isAcademic Academic
        , checkbox "Work" model.isWork Work
        , checkbox "Project" model.isProject Project
        , checkbox "Award" model.isAward Award
        ]
