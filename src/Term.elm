module Term exposing (..)

import Json.Decode exposing (Decoder, string, int, list, map, map2, map6, field, keyValuePairs, maybe)
import Decoderhelper exposing (int2, intDictDecoder, listheadwithdefault, pseudolist)
import Dict exposing (Dict)

type alias Term =
    { id : Int
    , name : String
    , wordtype : Maybe Int
    , count : Maybe Int
    , relevance : Maybe Int
    , top_topic : List Int
    }

defaultTerm : Term
defaultTerm =
    (Term -1 "Error: Not matching." Nothing Nothing Nothing [])

type alias TermSorting =
    List { id : Int, relevance : Int}

type alias TermsResult =
    { topic : (Int, TermSorting)
    , terms : Dict Int Term
    }

-- Mapper and Checker
termId2Term : List Term -> Int -> Maybe Term
termId2Term terms termId =
    (List.head (List.filter (\x -> x.id == termId) terms))

matchTermsById : { items : Dict Int Term, sorting : List Int} -> List Term
matchTermsById termsorting =
    let toTermSortingItem id =
            { id = id
            , relevance = 0
            }
        sorting =
            { items = termsorting.items
            , sorting = List.map toTermSortingItem termsorting.sorting
            }
    in
    matchTermsBySorting sorting

matchTermsBySorting : { items : Dict Int Term, sorting : TermSorting} -> List Term
matchTermsBySorting termsorting =
    let term : { id : Int, relevance : Int} -> Term
        term x =
            Maybe.withDefault
                defaultTerm
                (Dict.get x.id termsorting.items)
        matchTerm : { id : Int, relevance : Int} -> Term
        matchTerm x =
            let term_ = term x
            in
            { term_ | relevance = Just x.relevance}
    in
    List.map
        matchTerm
        termsorting.sorting

matchTermsortingById : TermsResult -> List Term
matchTermsortingById termsresult =
    let termsorting : { items : Dict Int Term, sorting : TermSorting}
        termsorting =
            { items = termsresult.terms
            , sorting = Tuple.second termsresult.topic
            }
    in
    matchTermsBySorting termsorting

-- Decoders
termDecoder : String -> Decoder Int -> Decoder Term
termDecoder itemName intDecoder =
    map6 Term
        (field (itemName ++ "_ID") intDecoder)
        (field (itemName ++ "_NAME") string)
        (maybe (field "WORDTYPE$WORDTYPE" intDecoder))
        (maybe (field (itemName ++ "_COUNT") intDecoder))
        (maybe (field "RELEVANCE" intDecoder))
        (map
            (Maybe.withDefault [])
            (maybe (field "TOP_TOPIC" (list int)))
        )

termSortingDecoder : Decoder TermSorting
termSortingDecoder =
    list
        (map2 (\x y -> {id = x, relevance = y})
            (field "TermId" int2)
            (field "relevance" int2)
        )

termsDecoder : Decoder (List Term)
termsDecoder =
    map matchTermsortingById
        (map2 TermsResult
            (field "Topic"
                (map
                    (Tuple.mapFirst (\x -> Result.withDefault -1 (String.toInt x)))
                    (listheadwithdefault
                        ("0", [{id = 0, relevance = 0}])
                        (keyValuePairs (field "Top_Terms" termSortingDecoder))
                    )
                )
            )
            (field "Term" (intDictDecoder defaultTerm (termDecoder "TERM" int)))
        )

bestTermsDecoder : Decoder (List Term)
bestTermsDecoder =
    --pseudolist
    map matchTermsById
        (field "61"
            (listheadwithdefault { sorting = [], items = Dict.empty}
                (field "ITEMS"
                    (pseudolist
                        (map2 (\x y->{ sorting = x, items = y})
                            (field "SORTING" (list int))
                            (intDictDecoder
                                defaultTerm
                                (termDecoder "ITEM" int)
                            )
                        )
                    )
                )
            )
        )


searchTermDecoder : Decoder (List Term)
searchTermDecoder =
    list (termDecoder "TERM" int2)
