module Init exposing (init, initSettings)

import Model exposing (..)
import Msg exposing (Msg(..))
import Topic exposing (Topic, defaultTopic)
import Term exposing (Term, defaultTerm, termsDecoder)
import Document exposing (Doc, defaultDoc, Document, defaultDocument)
import Request exposing (loadTopics)
import ContainerCache

import Material
import Material.Layout as Layout
import Array
import Dict

init : (Model, Cmd Msg)
init =
    ({ topics = []
    , docs = []
    , terms = []
    , settings = initSettings
    , slots = [Empty "300px"]
    , answer =
        { terms = Nothing
        , docs = Nothing
        }
    , termsCache = (ContainerCache.newContainerModel Array.empty 10 ContainerCache.defaultContainer)
    , termsDict = Dict.empty
    , mdl = Material.model
    } , Cmd.batch [(loadTopics 0), (Layout.sub0 Mdl)])

initSettings : Settings
initSettings =
    { error = ""
    , mobile = False
    , showRelevance = True
    , search = False
    , search4 = ""
    , searchResult = TopicResult []
    , frame = Startpage
    }

initDocument : Int -> Document
initDocument id =
    { defaultDocument | id = id}

initDoc : Int -> Doc
initDoc id =
    { defaultDoc | id = id}

initTopic : Int -> Topic
initTopic id =
    { defaultTopic | id = id}

initTerm : Int -> Term
initTerm id =
    { defaultTerm | id = id}
