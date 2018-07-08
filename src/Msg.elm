module Msg exposing (..)

import Model exposing (..)
import Term exposing (Term)
import Topic exposing (Topic)
import Document exposing (Document, Doc)

import ContainerCache
import Material
import Http

type Msg
    = Open Frame
    | ResetSettings
    | Search String
    | AdvancedSearch String
    | Found View
    | DeleteSlot Int
    | RemoveSlotFromOther Int
    | SlotToLastFromOther Int
    | ChoseSlotDialog Int
    | UpdateSlot View Int
    | Toggle Settings
    | NewTopics String Int (Result Http.Error (List Topic))
    | NewDocument Int (Result Http.Error Document)
    | NewDocs String Int (Result Http.Error (List Doc))
    | NewDocTokens String Int (Result Http.Error Document)
    | NewTerms String Int (Result Http.Error (List Term))
    | NewFrames String Int (Result Http.Error (List Term))
    | NewTermTopics String Int (Result Http.Error (List Term))
    | NewSearchTopics String (Result Http.Error (List Term))
    | NewSearchTerms String (Result Http.Error (List Term))
    | NewSearchDocs String (Result Http.Error (List Doc))
    | ReturnTerms Topic String Int (Result Http.Error (List Term))
    | ReturnDocs Topic String Int (Result Http.Error (List Doc))
    | NewTermsContainerSlot String Topic Int Int Msg
    | ManageTermsCache (ContainerCache.ContainerModelMsg (List Term))
    | NewDocsContainerSlot String Int Int Msg
    | ManageDocsCache (ContainerCache.ContainerModelMsg (List Doc))
    | ExecCmd Int String (Cmd Msg)
    | Batch (List Msg)
    | Mdl (Material.Msg Msg)
    | None -- zum Testen, damit update immer einen "_ ->"-Zweig haben kann
