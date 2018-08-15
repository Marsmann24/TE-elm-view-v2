module Model exposing (..)

import Term exposing (..)
import Topic exposing (..)
import Document exposing (..)

import Material
import Material.Options exposing (Property, cs, css)
import Material.Color as Color
import Material.Icon as Icon
import Http
import Html exposing (Html)
import Array exposing (Array)
import Dict exposing (Dict)
import Maybe exposing (Maybe, withDefault)
import ContainerCache exposing (ContainerModel)

topicNumber : Int
topicNumber = 30

type alias Model =
    { topics : List Topic           -- all topics
    , docs : List Doc                -- ranked articles
    , terms : List Term             -- current term list
    , settings : Settings           -- which views are shown
    , slots : List View
    , answer :
        { terms : Maybe (List Term)
        , docs : Maybe (List Doc)
        }
    , termsCache : ContainerModel (List Term)
    , termsDict : Dict String Int
    , docsCache : ContainerModel (List Doc)
    , docsDict : Dict String Int
    , mdl : Material.Model
    }

type alias Settings =
    { error : String
    , mobile : Bool
    , showRelevance : Bool
    , search : Bool
    , search4 : String
    , searchResult : SearchResult
    , frame : Frame
    }

type SearchResult
    = TopicResult (List Topic)
    | TermResult (List Term)
    | DocumentResult Doc

type Frame
    = Startpage
    | Mobile
    | ViewTopics
    | Custom

type View
    = TermsView String (List Term)
    -- WordlistView (List String)
    | TopicsView String (List Topic)
    | DocumentsView String (List Doc)
    | TermsDocumentsView String (List Term) (List Doc)
    | ShowdocumentView Document
    | CombinedView String View View
    | TermsContainerSlot String Topic
    | DocsContainerSlot String
    | Empty String
    | ErrorSlot
