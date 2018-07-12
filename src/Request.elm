module Request exposing (..)

import Msg exposing (..)
import Model exposing (Model)
--, Command(..))
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)

import Task
import ContainerCache
import Array
import Http
import Json.Decode exposing (Decoder)

baseURL : String
baseURL = "https://topicexplorer.informatik.uni-halle.de/09sdfjglikqw3bret5cp84vqyolrfiksefgdakyuheas/webapp/ZEIT0614_3_te/JsonServlet?Command="

loadData : Decoder a -> (Result Http.Error a -> Msg) -> String -> Cmd Msg
loadData decoder msg arguments =
    let url = (baseURL ++ arguments)
        request = Http.get url decoder
    in
    Http.send msg request

loadTopics : Int -> Cmd Msg
loadTopics slotId =
    loadData Topic.decodeTopics (NewTopics "Topics" slotId) "getTopics"

loadDoc : Int -> Doc -> Cmd Msg
loadDoc slotId doc =
    loadData Document.documentDecoder (NewDocument slotId) ("getDoc&DocId=" ++ (toString doc.id))

loadDocTokens : Doc -> Int -> Cmd Msg
loadDocTokens doc slotId =
    loadData Document.documentDecoder (NewDocTokens (("Terms in \"" ++ doc.title) ++ "\"") slotId) ("getDoc&DocId=" ++ (toString doc.id))

loadBestDocs : (String -> Int -> (Result Http.Error (List Doc)) -> Msg) -> Topic -> Maybe Term -> String -> Int -> Cmd Msg
loadBestDocs onResult topic maybeterm sorting slotId =
    let command =
            String.concat
                [ "bestDocs&TopicId="
                , (toString topic.id)
                , termArgument
                , "&sorting="
                , sorting
                ]
        termArgument =
            case maybeterm of
                Just term ->
                    "&term=" ++ term.name
                _ ->
                    ""
        name =
            case maybeterm of
                Just term ->
                    "Docs with " ++ term.name
                _ ->
                    "Docs in Topic " ++ (toString topic.id)
    in
    loadData Document.bestDocsDecoder (onResult name slotId) command

loadTerms : (String -> Int -> (Result Http.Error (List Term)) -> Msg) -> Topic -> Int -> Int -> Cmd Msg
loadTerms onResult topic offset slotId =
    let command =
            String.concat
                [ "getTerms&TopicId="
                , (toString topic.id)
                , "&offset="
                , (toString offset)
                ]
    in
    loadData Term.termsDecoder (onResult ("Terms in Topic " ++ (toString topic.id)) slotId) command

loadBestTerms : Int -> Cmd Msg
loadBestTerms slotId =
    loadData Term.bestTermsDecoder (NewTerms "Terms" slotId) "getBestTerms"

loadAutocompleteTerms : String  -> Int-> Cmd Msg
loadAutocompleteTerms termName slotId=
    let command = "autocomplete&SearchWord=" ++ termName
    in
    loadData Term.searchTermDecoder (NewTermTopics termName slotId) command

loadBestFrames : Int -> Cmd Msg
loadBestFrames slotId =
    loadData Term.bestTermsDecoder (NewFrames "Frames" slotId) "getBestFrames"

loadSearchTopics : String -> Cmd Msg
loadSearchTopics search =
    let command =
            "autocomplete&SearchWord=" ++ search
        name = "Search Result for " ++ search
    in
    loadData Term.searchTermDecoder (NewSearchTopics name) command

loadSearchTerms : String -> Cmd Msg
loadSearchTerms search =
    let command =
            "autocomplete&SearchWord=" ++ search
        name = "Search Result for " ++ search
    in
    loadData Term.searchTermDecoder (NewSearchTerms name) command

loadSearchDocs : String -> Bool -> String -> Cmd Msg
loadSearchDocs search strict sorting =
    let command =
            String.concat
                [ "search&SearchWord="
                , search
                , "&SearchStrict="
                , if strict
                    then "true"
                    else "false"
                , "&sorting="
                , sorting
                ]
        name = "Search Result for " ++ search
    in
    loadData Document.bestDocsDecoder (NewSearchDocs name) command

getCombineTopics : Int -> String -> Cmd Msg
getCombineTopics slotId termName =
    let command = "autocomplete&SearchWord=" ++ termName
    in
    loadData Term.searchTermDecoder (CombineTopic slotId termName) command

getCombineDocs : Int -> String -> Bool -> String -> Cmd Msg
getCombineDocs slotId name strict sorting =
    let command =
            String.concat
                [ "search&SearchWord="
                , name
                , "&SearchStrict="
                , if strict
                    then "true"
                    else "false"
                , "&sorting="
                , sorting
                ]
    in
    loadData Document.bestDocsDecoder (CombineDoc slotId name) command


docsPageRequest : Topic -> String -> ContainerCache.Meta (List Doc) -> Int -> ContainerCache.Page (List Doc)
docsPageRequest topic termArgument meta pagenumber =
    let command =
            String.concat
                [ "bestDocs&TopicId="
                , (toString topic.id)
                , termArgument
                , "&sorting="
                , "RELEVANCE"
                , "&offset="
                , (toString (pagenumber * meta.itemsPerPage))
                ]
        url = baseURL ++ command
    in
    ContainerCache.ToLoad meta.identifier
        (Http.send (ContainerCache.LoadCheckPage meta pagenumber)
            (Http.get url
                (meta.decoder)
            )
        )

createNewDocsContainer : Bool -> Model -> Topic -> Maybe Term -> Int -> Msg
createNewDocsContainer combine model topic maybeterm slotId =
    let containerId = (Array.length model.docsCache.arrayOfContainer)
        (termArgument, termName) =
            case maybeterm of
                Just term ->
                    ("&term=" ++ term.name, " with " ++ term.name)
                _ ->
                    ("", "")
    in
    NewDocsContainerSlot combine (("Docs in Topic " ++ (toString topic.id)) ++ termName) slotId containerId
        (ManageDocsCache
            (ContainerCache.CreateNewContainer
                (ContainerCache.LoadNewContainer ("docslot" ++ (toString slotId))
                    500
                    20
                    1
                    containerId
                    Document.bestDocsDecoder
                    (docsPageRequest topic termArgument)
                )
            )
        )

termsPageRequest : Topic -> ContainerCache.Meta (List Term) -> Int -> ContainerCache.Page (List Term)
termsPageRequest topic meta pagenumber =
    let command =
            String.concat
                [ "getTerms&TopicId="
                , (toString topic.id)
                , "&offset="
                , (toString (pagenumber * meta.itemsPerPage))
                ]
        url = baseURL ++ command
    in
    ContainerCache.ToLoad meta.identifier
        (Http.send (ContainerCache.LoadCheckPage meta pagenumber)
            (Http.get url
                (meta.decoder)
            )
        )

createNewTermsContainer : Bool -> Model -> Topic -> Int -> Msg
createNewTermsContainer combine model topic slotId =
    let containerId = (Array.length model.termsCache.arrayOfContainer)
    in
    NewTermsContainerSlot combine ("Terms in Topic " ++ (toString topic.id)) topic slotId containerId
        (ManageTermsCache
            (ContainerCache.CreateNewContainer
                (ContainerCache.LoadNewContainer ("termslot" ++ (toString slotId))
                    450
                    30
                    1
                    containerId
                    Term.termsDecoder
                    (termsPageRequest topic)
                )
            )
        )
