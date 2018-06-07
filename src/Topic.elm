module Topic exposing (..)

import Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map, map3, map4, field, keyValuePairs, maybe, succeed)
import Decoderhelper exposing (int2, intDictDecoder)
import Dict exposing (Dict)

type alias RawTopic =
    { id : Int
    , hirarchical_topic : TopicHirarchie
    , color_topic : String
    , top_terms : TermSorting
    }

defaultRawTopic : RawTopic
defaultRawTopic =
    RawTopic -1 defaultTopicHirarchie "" []

type alias Topic =
    { id : Int
    , hirarchical_topic : TopicHirarchie
    , color_topic : String
    , top_terms : List Term
    }

defaultTopic : Topic
defaultTopic =
    Topic 0 defaultTopicHirarchie "" []

type alias TopicHirarchie =
    { start : Int
    , end : Int
    , depth : Int
    , cluster : Maybe String
    }

defaultTopicHirarchie : TopicHirarchie
defaultTopicHirarchie =
    TopicHirarchie 0 0 0 Nothing

--type alias TopicResult =
--    { topics : List Topic
--    , sorting : List Int
--    , terms : List Term
--    , topicsBestItemLimit : Int
--    }

-- Mapper and Checker
termInTopic : Term -> Topic -> Bool
termInTopic term topic =
    List.member term.id (List.map (\x -> x.id) topic.top_terms)

topicId2Topic : List Topic -> Int -> Maybe Topic
topicId2Topic topics topicId =
    (List.head (List.filter (\x -> x.id == topicId) topics))

matchRawTopicsById : Dict Int RawTopic -> List Int -> List RawTopic
matchRawTopicsById topics sorting =
    let sortedList =
            List.map
                (\x -> Maybe.withDefault defaultRawTopic (Dict.get x topics))
                sorting
    --    unsortedList =
    --        List.filter
    --            (not << ((flip List.member) sortedList))
    --            (Dict.values topics)
    in
    --List.append
        sortedList
    --    unsortedList

makeTopicsList : Dict Int RawTopic -> List Int -> Dict Int Term -> List Topic
makeTopicsList topics sorting terms =
    let matchTerms : Int -> TermSorting -> List Term
        matchTerms topic topic_terms =
            matchTermsortingById { topic = (topic, topic_terms) , terms = terms}
        rawTopic2Topic : RawTopic -> Topic
        rawTopic2Topic raw =
            Topic raw.id raw.hirarchical_topic raw.color_topic (matchTerms raw.id raw.top_terms)
    in
    List.map rawTopic2Topic (matchRawTopicsById topics sorting)

-- Decoders
topicDecoder : Decoder RawTopic
topicDecoder =
    map4 RawTopic
        (field "TOPIC_ID" int2)
        (map4 TopicHirarchie
            (field "HIERARCHICAL_TOPIC$START" int2)
            (field "HIERARCHICAL_TOPIC$END" int2)
            (field "HIERARCHICAL_TOPIC$DEPTH" int2)
            (maybe (field "HIERARCHICAL_TOPIC$CLUSTER_MEMBERSHIP" string))
        )
        (field "COLOR_TOPIC$COLOR" string)
        (field "Top_Terms" termSortingDecoder)

decodeTopics : Decoder (List Topic)
decodeTopics =
    map3 makeTopicsList
        (field "Topic" (intDictDecoder defaultRawTopic topicDecoder))
        (field "TOPIC_SORTING" (list int))
        (field "Term" (intDictDecoder defaultTerm (termDecoder "TERM" int2)))
        --(field "TopicBestItemLimit" int)
