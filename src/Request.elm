module Request exposing (..)

import Msg exposing (..)
--, Command(..))
import Topic exposing (Topic)
import Term exposing (Term)
import Document exposing (Doc, Document)

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
                    "&term" ++ (toString term)
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
