module Model exposing (..)

import Html exposing (div, p, text)
import Model.Date as Date
import Model.Event as Event exposing (Event)
import Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, noneSelected)
import Model.Interval as Interval
import Model.PersonalDetails exposing (DetailWithName, PersonalDetails)
import Model.Repo as Repo


type alias Model =
    { personalDetails : PersonalDetails
    , events : List Event
    , selectedEventCategories : SelectedEventCategories
    , repos : List Repo.Repo
    }


academicEvents : List Event
academicEvents =
    [ { title = "Technical University of Cluj-Napoca"
      , interval = Interval.withDurationYears (Date.onlyYear 2020) 4
      , description = p [] [ text "Computers and Information Technology" ]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = True
      }
    , { title = "\"Unirea\" National College Focsani"
      , interval = Interval.withDurationYears (Date.onlyYear 2016) 4
      , description = div [] []
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


workEvents : List Event
workEvents =
    [ { title = "Stratec Biomedical"
      , interval = Interval.withDurationMonths 2021 Date.Apr 7
      , description = text "Internship"
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


projectEvents : List Event
projectEvents =
    [ { title = "MIPS Single-Cycle CPU"
      , interval = Interval.oneYear 2021
      , description = text ""
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Food Delivery System"
      , interval = Interval.oneYear 2021
      , description = text ""
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Queue Simulator"
      , interval = Interval.oneYear 2021
      , description = text ""
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Polynomial Calculator"
      , interval = Interval.oneYear 2021
      , description = text ""
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Operating Systems Assignments"
      , interval = Interval.oneYear 2021
      , description = text "Max grade"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Library Database Project"
      , interval = Interval.oneYear 2021
      , description = text "Max grade"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "ATM \"Team\" Project"
      , interval = Interval.oneYear 2021
      , description = text "Max grade"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "windOS Calculator"
      , interval = Interval.oneYear 2021
      , description = text "Max grade asm x86 calculator"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


awardEvents : List Event
awardEvents =
    [ { title = "Build IT @ Home"
      , interval = Interval.oneYear 2021
      , description = text "Stratec Biomedical contest - 2nd Place"
      , category = Award
      , url = Nothing
      , tags = []
      , important = True
      }
    , { title = "International Informatics Olympiad in Teams"
      , interval = Interval.oneYear 2019
      , description = text "National phases participation"
      , category = Award
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


personalDetails : PersonalDetails
personalDetails =
    { name = "Vlad Ursache"
    , intro = "Team Player, Fast Learner, Critical Thinker and Problem Solver"
    , contacts = [ DetailWithName "email" "Ursache.Co.Vlad@student.utcluj.ro" ]
    , socials = [ DetailWithName "github" "https://github.com/Vlad-34" ]
    }


initModel : Model
initModel =
    let
        repoList =
            [ Repo.Repo "Operating Systems Assignment" (Just "Max grade") "https://github.com/Vlad-34/OperatingSystemsAssignments" "" 1 ]
    in
    { personalDetails = personalDetails
    , events = Event.sortByInterval <| academicEvents ++ workEvents ++ projectEvents ++ awardEvents
    , selectedEventCategories = noneSelected
    , repos = repoList
    }
