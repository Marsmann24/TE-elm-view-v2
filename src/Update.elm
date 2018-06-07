module Update exposing (update)

import Model exposing (..)
import Msg exposing (..)
import Init exposing (initSettings)
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)
import Request

import Material
import Dispatch
import Delay
import Time
import Platform.Cmd
import Maybe exposing (withDefault)
import Array
import Set
import Dom.Scroll
import Task

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Open frame ->
            let cmd =
                    if (frame == ViewTopics)
                    then
                        if (List.length model.topics) < topicNumber
                        then (Request.loadTopics 0)
                        else Cmd.none
                    else Cmd.none
                settings = model.settings
                newSlots =
                    if frame == ViewTopics
                    then
                        if (List.length model.topics) < topicNumber
                        then [ Empty "300px"]
                        else [ TopicsView "Topics" model.topics]
                    else model.slots
            in
            ({ model
                | slots = newSlots
                , settings = { settings | frame = frame}
            }, cmd)
        Toggle settings ->
            ({ model | settings = settings}, Cmd.none)
        Search searchterm ->
            let oldSettings = model.settings
            in
            if not (searchterm == "")
            then
                ({ model
                    | settings =
                        { oldSettings
                            | search = not (searchterm == "")
                            , search4 = searchterm
                        }
                }, Request.loadSearchTerms searchterm)
            else
                ( model, Cmd.none)
        AdvancedSearch string ->
            let oldSettings = model.settings
                searchterm = String.toLower string
            in
            if not (searchterm == "")
            then
                let request =
                        (if (String.startsWith "topic:" searchterm)
                        then
                            let search4topic = (String.dropLeft 6 searchterm)
                            in
                            (Request.loadSearchTopics search4topic)
                        else (if (String.startsWith "term:" searchterm)
                        then
                            let search4term = (String.dropLeft 5 searchterm)
                            in
                            (Request.loadSearchTerms search4term)
                        else (if (String.startsWith "document:" searchterm)
                        then
                            let search4doc = (String.dropLeft 9 searchterm)
                                docId = String.toInt search4doc
                            in
                            case docId of
                                Ok id ->
                                    let doc = Document.defaultDoc
                                    in
                                    (Request.loadDoc 1 { doc | id = id})
                                Err _ ->
                                    (Request.loadSearchDocs search4doc False "RELEVANCE")
                        else
                            (Request.loadSearchTerms searchterm)
                        )))
                in
                ({ model
                    | settings =
                        { oldSettings
                            | search = False
                            , search4 = searchterm
                        }
                }, request)
            else
                ( model, Cmd.none)
        ResetSettings ->
            ({ model | settings = {initSettings | frame = model.settings.frame}}, Cmd.none)
        Found view ->
                let oldSettings = model.settings
                in
                ({ model
                    | settings = { oldSettings
                                    | search = False
                                    , search4 = ""
                                }
                    , slots = [ view]
                    }
                , Cmd.none)
        DeleteSlot slotId ->
            let oldSettings = model.settings
            in
            ({ model
                | slots =  List.take slotId model.slots
                }
            , Cmd.none)
        UpdateSlot view slotId ->
            ({ model
                | slots = List.append (List.take slotId model.slots) [ view]
            }, scroll2Right)
        NewTopics name slotId result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newTopics ->
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ TopicsView name newTopics]
                        , topics = newTopics
                        , settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        NewTerms name slotId result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newTerms ->
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ TermsView name newTerms]
                        , terms = newTerms
                        , settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        NewDocs name slotId result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newDocs ->
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ DocumentsView name newDocs]
                        , docs = newDocs
                        ,settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        NewDocTokens name slotId result ->
            let oldSettings = model.settings
                allTerms = model.terms
            in
            case result of
                Ok document ->
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ TermsView name (Document.documentTerms document allTerms)]
                        ,settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        NewDocument slotId result ->
            let oldSettings = model.settings
            in
            case result of
                Ok document ->
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ ShowdocumentView document]
                        , settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = "Document not found"}}, scroll2Right)
        NewFrames name slotId result ->
            (model, scroll2Right)
        NewTermTopics termName slotId result ->
            let oldSettings = model.settings
                fetchTerm : List Term -> Maybe Term
                fetchTerm terms =
                    List.head (List.filter (\x -> x.name == termName) terms)
                maybeTerm2TopicList : Maybe Term -> List Int
                maybeTerm2TopicList terms =
                    case terms of
                        Just term ->
                            term.top_topic
                        Nothing ->
                            []
                isMember : List Term -> Topic -> Bool
                isMember terms topics=
                    List.member
                        topics.id
                        (maybeTerm2TopicList
                            (fetchTerm terms)
                        )
                newTopics : List Term -> List Topic
                newTopics terms =
                    List.filter
                        (isMember terms)
                        model.topics
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots =  List.append (List.take slotId model.slots) [ TopicsView ("Topics with " ++ termName) (newTopics termList)]
                        , settings =
                            { oldSettings | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        NewSearchTopics name result ->
            let oldSettings = model.settings
                concatTopTopics : List Term -> List Int
                concatTopTopics terms =
                    Set.toList
                        (Set.fromList
                            (List.concat
                                (List.map .top_topic terms)
                            )
                        )
                isMember : List Term -> Topic -> Bool
                isMember terms topics=
                    List.member
                        topics.id
                        (concatTopTopics terms)
                newTopics : List Term -> List Topic
                newTopics terms =
                    List.filter
                        (isMember terms)
                        model.topics
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots = [ TopicsView name (newTopics termList)]
                        , settings =
                            { oldSettings
                                | search = False
                                , error = ""
                                , frame = Custom
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model
                        | settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString err
                            }
                    }, scroll2Right)
        NewSearchTerms name result ->
            let oldSettings = model.settings
            in
            case result of
                Ok termList ->
                    ({ model
                        | slots = [ TermsView name termList]
                        , settings =
                            { oldSettings
                                | searchResult = TermResult termList
                                , error = ""
                                , frame = Custom
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model
                        | settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString err
                            }
                    }, scroll2Right)
        NewSearchDocs name result ->
            let oldSettings = model.settings
            in
            case result of
                Ok docList ->
                    ({ model
                        | slots = [ DocumentsView name docList]
                        , settings =
                            { oldSettings
                                | search = False
                                , error = ""
                                , frame = Custom
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model
                        | settings =
                            { oldSettings
                                | search = False
                                , search4 = ""
                                , error = toString err
                            }
                    }, scroll2Right)
        ReturnDocs topic name slotId result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newDocs ->
                    let view =
                            case model.answer.terms of
                                Just terms ->
                                    TermsDocumentsView ("Details for Topic" ++ (toString topic.id)) terms newDocs
                                Nothing ->
                                    TermsDocumentsView "loading" [] newDocs
                        newAnswer =
                            case model.answer.terms of
                                Just terms ->
                                    { docs = Nothing, terms = Nothing}
                                Nothing ->
                                    { docs = Just newDocs, terms = Nothing}
                    in
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ view]
                        , answer = newAnswer
                        , docs = newDocs
                        ,settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        ReturnTerms topic name slotId result ->
            let oldSettings = model.settings
            in
            case result of
                Ok newTerms ->
                    let view =
                            case model.answer.docs of
                                Just docs ->
                                    TermsDocumentsView ("Details for Topic" ++ (toString topic.id)) newTerms docs
                                Nothing ->
                                    TermsDocumentsView "loading" newTerms []
                        newAnswer =
                            case model.answer.docs of
                                Just docs ->
                                    { docs = Nothing, terms = Nothing}
                                Nothing ->
                                    { docs = Nothing, terms = Just newTerms}
                    in
                    ({ model
                        | slots = List.append (List.take slotId model.slots) [ view]
                        , terms = newTerms
                        , answer = newAnswer
                        , settings =
                            { oldSettings
                                | error = ""
                            }
                    }, scroll2Right)
                Err err ->
                    ({ model | settings = { oldSettings | error = toString err}}, scroll2Right)
        ExecCmd slotId width cmd ->
            let oldSettings = model.settings
            in
            ({ model
                | slots =  List.append (List.take slotId model.slots) [ Empty width]
            }, cmd)
        Batch msg_ ->
            model ! [ Dispatch.forward msg_ ]
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)

scroll2Right : Cmd Msg
scroll2Right =
    Task.attempt (\a -> None) (Dom.Scroll.toRight "board")
