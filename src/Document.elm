module Document exposing (..)

import Topic exposing (..)
import Term exposing (..)

import Json.Decode exposing (Decoder, int, string, list, map2, map4, map8, field, keyValuePairs, maybe)
import Decoderhelper exposing (int2, float2, map12, pseudolist, listheadwithdefault)

-- Types
type alias Token =
    { topic_id : Int
    , posintion_in_document : Int
    , term : String
    , parent_topic_ids : List Int
    }

type alias Doc =
    { id : Int
    , keyword_snippet : String
    , keyword_title : String
    , top_topic : List Int
    , linkurl : String
    , time_stamp : Int
    , title : String
    , snippet : String
    , topic_id : Maybe Int
    , document_count : Maybe String
    , relevance : Maybe Float
    , isino : Maybe Int
    }

defaultDoc : Doc
defaultDoc =
    Doc 0 "" "default" [] "url" 0 "default" "" Nothing Nothing Nothing Nothing

type alias Document =
    { id : Int
    , linkurl : String
    , time_stamp : Int
    , title : String
    , fulltext : String
    , search_test : String
    , frame_list :  List String
    , word_list : List Token
    }

defaultDocument : Document
defaultDocument =
    Document 0 "url" 0 "default" "" "" [] []

-- Mapper and Checker
termInDocument : Term -> Document -> Bool
termInDocument term document =
    List.member term.name (List.map .term document.word_list)

documentTerms : Document -> List Term -> List Term
documentTerms document terms =
    List.filter ((flip termInDocument) document) terms

documentId2Document : List Document -> Int -> Maybe Document
documentId2Document documents documentId =
    (List.head (List.filter (\x -> x.id == documentId) documents))

docId2Doc : List Doc -> Int -> Maybe Doc
docId2Doc docs docId =
    (List.head (List.filter (\x -> x.id == docId) docs))

-- Decoders :
tokenDecoder : Decoder Token
tokenDecoder =
    map4 Token
        (field "TOPIC_ID" int2)
        (field "POSITION_OF_TOKEN_IN_DOCUMENT" int2)
        (field "TOKEN" string)
        (field "HIERARCHICAL_TOPIC$PARENT_IDS" (list int))

documentDecoder : Decoder Document
documentDecoder =
    field "DOCUMENT"
        (map8 Document
            (field "DOCUMENT_ID" int2)
            (field "LINK$URL" string)
            (field "TIME$TIME_STAMP" int2)
            (field "TEXT$TITLE" string)
            (field "TEXT$FULLTEXT" string)
            (field "SEARCH_TEXT" string)
            (field "FRAME_LIST" (listheadwithdefault [] (pseudolist (list string))))
            (field "WORD_LIST" (list tokenDecoder))
        )

bestDocsDecoder : Decoder (List Doc)
bestDocsDecoder =
    let -- Decoders :
        documents : Decoder (List Doc)
        documents =
            field "DOCUMENT"
                (pseudolist
                    (map12 Doc
                        (field "DOCUMENT_ID" int2)
                        (field "KEYWORD_SNIPPET" string)
                        (field "KEYWORD_TITLE" string)
                        (field "TOP_TOPIC" (list int))
                        (field "LINK$URL" string)
                        (field "TIME$TIME_STAMP" int2)
                        (field "TEXT$TITLE" string)
                        (field "TEXT$SNIPPET" string)
                        (maybe (field "TOPIC_ID" int2))
                        (maybe (field "PR_DOCUMENT_GIVEN_TOPIC" string))
                        (maybe (field "RELEVANCE" float2))
                        (maybe (field "ISIN0" int2))
                    )
                )

        sorting : Decoder (List Int)
        sorting =
            field "DOCUMENT_SORTING" (list int2)

        -- Combinefunction :
        getDoc : List Doc -> Int -> Maybe Doc
        getDoc docs docId =
            List.head
                (List.filter
                    (\a -> a.id == docId)
                    docs
                )

        applySorting : List Doc -> List Int -> List Doc
        applySorting docs order =
            List.filterMap (getDoc docs) order
    in
    -- combine Decoders :
    map2 applySorting documents sorting
