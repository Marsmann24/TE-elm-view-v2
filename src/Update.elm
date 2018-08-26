module Update exposing (update, scroll2Top, scroll2Bottom, scroll2Right)

import Model exposing (..)
import Msg exposing (..)
import Init exposing (initSettings)
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)
import Request

import Debug
import Dict
import ContainerCache
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
import Window
import Http

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenCheckMobile frame mobileframe ->
            let size2Msg task =
                    case task of
                        Ok deviceSize ->
                            if (deviceSize.width < 612)
                            then Open mobileframe
                            else Open frame
                        Err error ->
                            Open frame
            in
            ( model, (Task.attempt size2Msg Window.size))
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
            if ((String.length searchterm) > 2)
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
            if ((String.length searchterm) > 2)
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
                    (update (OpenCheckMobile Custom Mobile)
                        { model
                            | slots = [ TopicsView name (newTopics termList)]
                            , settings =
                                { oldSettings
                                    | search = False
                                    , error = ""
                                }
                        })
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
                    (update (OpenCheckMobile Custom Mobile)
                        { model
                            | slots = [ TermsView name termList]
                            , settings =
                                { oldSettings
                                    | searchResult = TermResult termList
                                    , error = ""
                                }
                        })
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
                    (update (OpenCheckMobile Custom Mobile)
                        { model
                            | slots = [ DocumentsView name docList]
                            , settings =
                                { oldSettings
                                    | search = False
                                    , error = ""
                                }
                        }
                    )
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
                newSlot =
                    if slotId >= 0
                    then List.append (List.take slotId model.slots) [ Empty width]
                    else model.slots
            in
            ({ model
                | slots = newSlot
            }, cmd)
        CombineTopic id termName result ->
            let fetchTerm : List Term -> Maybe Term
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
                Ok newTerms ->
                    update (Combine id ("Search Result for " ++ termName) (TopicsView "" (newTopics newTerms))) model
                Err err ->
                    ((includeError model err), Cmd.none)
        CombineDoc id name result ->
            case result of
                Ok newDocs ->
                    update (Combine id ("Search Result for " ++ name) (DocumentsView "" newDocs)) model
                Err err ->
                    ((includeError model err), Cmd.none)
        Combine slotId name view ->
            let oldSettings = model.settings
                newSlots =
                    if slotId >= 0
                    then List.append (List.take slotId model.slots) [ newView]
                    else model.slots
                newView =
                    case oldView1 of
                        (Empty "300px") ->
                            CombinedView name view oldView2
                        _ ->
                            CombinedView name oldView1 view
                (oldView1, oldView2) =
                    case List.head (List.drop slotId model.slots) of
                        Just (CombinedView _ oldView1 oldView2) ->
                            (oldView1, oldView2)
                        _ ->
                            ((Empty "300px"), (Empty "300px"))

            in
            ({ model
                | slots = newSlots
            }, Cmd.none)
        NewTermsContainerSlot combine name parent slotId containerId msg->
            let oldSettings = model.settings
                oldDict = model.termsDict
                newView = TermsContainerSlot name parent
                newSlots =
                    if combine
                    then model_.slots
                    else List.append (List.take slotId model.slots) [ newView]
                (model_, msg_) = update (Combine slotId name newView) model
            in
            if Dict.member name oldDict
            then
                ({ model
                    | slots = newSlots
                }, Cmd.none)
            else
                (update msg
                    { model
                        | slots =  newSlots
                        , termsDict = Dict.insert name containerId oldDict
                    })
        ManageTermsCache cachemsg ->
            let (newdata, cmd) =
                    ContainerCache.update cachemsg model.termsCache
                oldSettings = model.settings
            in
            ({ model
                | termsCache = newdata
                , settings = { oldSettings | error = ""}
            }, Platform.Cmd.map ManageTermsCache cmd )
        NewDocsContainerSlot combine name slotId containerId msg->
            let oldSettings = model.settings
                oldDict = model.docsDict
                newView = DocsContainerSlot name
                newSlots =
                    if combine
                    then model_.slots
                    else List.append (List.take slotId model.slots) [ newView]
                (model_, msg_) = update (Combine slotId name newView) model
            in
            if Dict.member name oldDict
            then
                ({ model
                    | slots = newSlots
                }, Cmd.none)
            else
                (update msg
                    { model
                        | slots = newSlots
                        , docsDict = Dict.insert name containerId oldDict
                    })
        ManageDocsCache cachemsg ->
            let (newdata, cmd) =
                    ContainerCache.update cachemsg model.docsCache
                oldSettings = model.settings
            in
            ({ model
                | docsCache = newdata
                , settings = { oldSettings | error = ""}
            }, Platform.Cmd.map ManageDocsCache cmd )
        Batch msg_ ->
            model ! [ Dispatch.forward msg_ ]
        BatchCmd cmd_ ->
            model ! cmd_
        Mdl msgmdl ->
            (Material.update Mdl msgmdl model)
        _ ->
            ( model, Cmd.none)

includeError : Model -> Http.Error -> Model
includeError model err =
    let oldSettings = model.settings
    in
    { model | settings = { oldSettings | error = toString err}}

scroll2Right : Cmd Msg
scroll2Right =
    Task.attempt (\a -> None) (Dom.Scroll.toRight "board")

scroll2Top : String -> Cmd Msg
scroll2Top id =
    Task.attempt (\a -> None) (Dom.Scroll.toTop id)

scroll2Bottom : String -> Cmd Msg
scroll2Bottom id =
    Task.attempt (\a -> None) (Dom.Scroll.toBottom id)
