module ContainerCache exposing (ContainerModel, Container, Meta, Page(Loaded, ToLoad, HandleError), ContainerModelMsg(CreateNewContainer, PageUpdate), Msg(LoadNewContainer, LoadCheckPage), PageMsg(NextPage, PrevPage), defaultContainer, newContainerModel, update, getCurrPageDataFromContainer)

import Json.Decode as Decode exposing (Decoder)
import Array exposing (Array, empty, indexedMap)
import Platform.Cmd
import LruCache
import Http


-- Type alias


type alias ContainerModel a =
    { arrayOfContainer : Array (Container a)
    , cache : LruCache.LruCache ( Int, Int ) (Page a)
    , newcontainer : Container a
    , log : ContainerError
    }


type alias Container a =
    { meta : Meta a
    , data : Array (Page a)
    , requestmaker : Meta a -> Int -> Page a
    , log : ContainerError
    }


type alias Meta a =
    { name : String
    , numOfItemsInContainer : Int
    , itemsPerPage : Int
    , numOfPages : Int
    , identifier : Int
    , windowSize : Int
    , currPage : Int
    , decoder : Decode.Decoder a
    }



-- Getters


getCurrIndex : ContainerModel a -> Int
getCurrIndex conmod =
    (Array.length conmod.arrayOfContainer) - 1


getCurrPageDataFromContainer : ContainerModel a -> Int -> Maybe a
getCurrPageDataFromContainer conmod index =
    let
        targetcontainer =
            Maybe.withDefault defaultContainer (Array.get index conmod.arrayOfContainer)
    in
        case (Maybe.withDefault (HandleError "") (Array.get targetcontainer.meta.currPage targetcontainer.data)) of
            Loaded data ->
                Just data

            _ ->
                Nothing



-- Types


type Page a
    = Loaded a
    | ToLoad Int (Cmd (Msg a))
    | HandleError String


type ContainerModelMsg a
    = CreateNewContainer (Msg a)
    | PageUpdate Int PageMsg
    | LoadNewPage Int (Cmd (Msg a)) (Msg a)


type Msg a
    = LoadNewContainer String Int Int Int Int (Decode.Decoder a) (Meta a -> Int -> Page a)
    | LoadCheckPage (Meta a) Int (Result Http.Error a)
    | UpdatePage PageMsg (Maybe (Page a))


type PageMsg
    = NextPage
    | PrevPage


type ContainerError
    = NoError
    | ManageContainer_Error Result
    | ContainerError Http.Error
    | Container_Error ContainerError



-- Defaults


defaultContainer : Container a
defaultContainer =
    (createContainer (createMetadata "DefaultContainer - Error" -1 -1 -1 -1 (Decode.fail "DefaultContainer - Decoder Error")) Array.empty requestmaker)


requestmaker : Meta a -> Int -> Page a
requestmaker meta pagenumtodelete =
    HandleError "Default pagerequest"



-- Inits


createContainer : Meta a -> Array (Page a) -> (Meta a -> Int -> Page a) -> Container a
createContainer meta data req =
    { meta = meta, data = data, requestmaker = req, log = NoError }


createMetadata : String -> Int -> Int -> Int -> Int -> Decode.Decoder a -> Meta a
createMetadata name numOfElem ipp cs iden decoder =
    { name = name
    , numOfItemsInContainer = numOfElem
    , itemsPerPage = ipp
    , numOfPages = (ceiling ((toFloat numOfElem) / (toFloat ipp)))
    , windowSize = cs
    , currPage = 0
    , identifier = iden
    , decoder = decoder
    }


newContainerModel : Array (Container a) -> Int -> Container a -> ContainerModel a
newContainerModel array cachesize newcontainer =
    { arrayOfContainer = array
    , cache = LruCache.empty cachesize
    , newcontainer = newcontainer
    , log = NoError
    }



-- Updates


update : ContainerModelMsg a -> ContainerModel a -> ( ContainerModel a, Cmd (ContainerModelMsg a) )
update msg model =
    case msg of
        CreateNewContainer updateMsg ->
            let
                ( container, command ) =
                    updatecontainer updateMsg model.newcontainer
            in
                case
                    (Maybe.withDefault (HandleError "CreateNewContainer")
                        (Array.get (0) container.data)
                    )
                of
                    Loaded _ ->
                        ( { model | arrayOfContainer = Array.push container model.arrayOfContainer }, Cmd.none )

                    _ ->
                        ( { model | newcontainer = container }, Platform.Cmd.map CreateNewContainer command )

        PageUpdate containernum pagemsg ->
            let
                targetcontainer =
                    (Maybe.withDefault (defaultContainer) (Array.get containernum model.arrayOfContainer))

                pagetoload =
                    getpagetoload targetcontainer pagemsg

                ( newcache, maybepageincache ) =
                    LruCache.get ( containernum, pagetoload ) model.cache

                updateMsg =
                    addpagetorequest pagemsg maybepageincache

                ( container, command, ( pageforcachenum, pageforcache ) ) =
                    updatepage updateMsg targetcontainer

                nextpagenum =
                    if pagemsg == NextPage then
                        targetcontainer.meta.currPage + 1
                    else
                        targetcontainer.meta.currPage - 1

                targetpage =
                    Maybe.withDefault (HandleError "PageUpdate targetpage") (Array.get (nextpagenum) targetcontainer.data)

                newcontainerarray =
                    if (pageforcachenum >= 0) then
                        ( { model
                            | arrayOfContainer = Array.set containernum container model.arrayOfContainer
                            , cache = LruCache.insert ( containernum, pageforcachenum ) pageforcache newcache
                          }
                        , Platform.Cmd.map (LoadNewPage containernum command) command
                        )
                    else
                        ( { model
                            | arrayOfContainer = Array.set containernum container model.arrayOfContainer
                          }
                        , Platform.Cmd.map (LoadNewPage containernum command) command
                        )
            in
                case targetpage of
                    ToLoad int httpreq ->
                        if targetcontainer.meta.windowSize > 0 then
                            let
                                ( newcontainer, reloadcommand ) =
                                    loadpage targetpage targetcontainer
                            in
                                if (pageforcachenum >= 0) then
                                    ( { model
                                        | arrayOfContainer = Array.set containernum newcontainer model.arrayOfContainer
                                        , cache = LruCache.insert ( containernum, pageforcachenum ) pageforcache newcache
                                      }
                                    , Platform.Cmd.map (LoadNewPage containernum reloadcommand) reloadcommand
                                    )
                                else
                                    ( { model
                                        | arrayOfContainer = Array.set containernum newcontainer model.arrayOfContainer
                                      }
                                    , Platform.Cmd.map (LoadNewPage containernum reloadcommand) reloadcommand
                                    )
                        else
                            newcontainerarray

                    _ ->
                        newcontainerarray

        LoadNewPage containernum command updateMsg ->
            let
                ( container, cmdnone ) =
                    updatecontainer updateMsg (Maybe.withDefault (defaultContainer) (Array.get containernum model.arrayOfContainer))
            in
                if container.log == NoError then
                    ( { model
                        | arrayOfContainer = Array.set containernum container model.arrayOfContainer
                        , log = NoError
                      }
                    , Cmd.none
                    )
                else
                    ( { model | log = Container_Error container.log }, Platform.Cmd.map (LoadNewPage containernum command) command )


updatecontainer : Msg a -> Container a -> ( Container a, Cmd (Msg a) )
updatecontainer msg model =
    case msg of
        LoadNewContainer containername numOfItems itemsPerPage windowSize numofitemsinarrayOfContainer decoder requestmaker ->
            let
                newMeta =
                    createMetadata containername numOfItems itemsPerPage windowSize numofitemsinarrayOfContainer decoder

                loadedContainer =
                    initializenewContainer newMeta requestmaker
            in
                ( (createContainer newMeta loadedContainer requestmaker)
                , Cmd.batch (Array.toList (Array.indexedMap (fillcache newMeta) loadedContainer))
                )

        LoadCheckPage meta pagenum result ->
            case result of
                Ok newEntry ->
                    ( createContainer meta (Array.set (pagenum) (Loaded newEntry) model.data) model.requestmaker
                    , Cmd.none
                    )

                Err a ->
                    ( { model | log = ContainerError a }, Cmd.none )

        _ ->
            ( model, Cmd.none )



--Determines the next page


updatepage : Msg a -> Container a -> ( Container a, Cmd (Msg a), ( Int, Page a ) )
updatepage msg model =
    case msg of
        UpdatePage NextPage page ->
            if model.meta.currPage + 1 < model.meta.numOfPages then
                switchpage 1 model page
            else
                ( model, Cmd.none, ( -1, HandleError "updatepage NextPage" ) )

        UpdatePage PrevPage page ->
            if 0 < model.meta.currPage then
                switchpage (-1) model page
            else
                ( model, Cmd.none, ( -1, HandleError "updatepage PrevPage" ) )

        _ ->
            ( model, Cmd.none, ( -1, HandleError "updatepage No UpdatePage" ) )



-- Function to go to next page and delete the one that's not in the window anymore


switchpage : Int -> Container a -> Maybe (Page a) -> ( Container a, Cmd (Msg a), ( Int, Page a ) )
switchpage factor model maybepage =
    let
        metahelp =
            model.meta

        newmeta =
            { metahelp | currPage = metahelp.currPage + (1 * factor) }

        pagenumtodelete =
            model.meta.currPage - (model.meta.windowSize * factor)

        pagenumtoload =
            model.meta.currPage + ((model.meta.windowSize + 1) * factor)

        newdeletemeta =
            { metahelp | currPage = pagenumtodelete + (metahelp.windowSize * factor) }

        nextpage =
            Maybe.withDefault (HandleError "switchpage nextpage") (Array.get pagenumtoload model.data)

        pagetodelete =
            model.requestmaker newdeletemeta pagenumtodelete

        deletedpage =
            Maybe.withDefault (HandleError "switchpage deletedpage") (Array.get pagenumtodelete model.data)
    in
        case nextpage of
            ToLoad int requestfornewpage ->
                case maybepage of
                    Just page ->
                        ( createContainer newmeta (Array.set pagenumtoload page (Array.set pagenumtodelete pagetodelete model.data)) model.requestmaker
                        , Cmd.none
                        , ( pagenumtodelete, deletedpage )
                        )

                    Nothing ->
                        ( createContainer newmeta (Array.set pagenumtodelete pagetodelete model.data) model.requestmaker
                        , requestfornewpage
                        , ( pagenumtodelete, deletedpage )
                        )

            _ ->
                ( createContainer newmeta model.data model.requestmaker, Cmd.none, ( -1, HandleError "switchpage not ToLoad" ) )



-- Helper
-- Turns a PageMsg into a Msg


addpagetorequest : PageMsg -> Maybe (Page a) -> Msg a
addpagetorequest msg page =
    UpdatePage msg page



-- Calculates the page to load


getpagetoload : Container a -> PageMsg -> Int
getpagetoload container msg =
    case msg of
        NextPage ->
            container.meta.currPage + container.meta.windowSize + 1

        PrevPage ->
            container.meta.currPage - container.meta.windowSize - 1



-- Initializes Container on load


initializenewContainer : Meta a -> (Meta a -> Int -> Page a) -> Array (Page a)
initializenewContainer meta requestmaker =
    Array.initialize (meta.numOfPages)
        (\n ->
            if n > (meta.windowSize) then
                let
                    metanew =
                        { meta | currPage = n - meta.windowSize }
                in
                    requestmaker metanew n
            else
                requestmaker meta n
        )



-- Sends cmd to load target page


loadpage : Page a -> Container a -> ( Container a, Cmd (Msg a) )
loadpage targetpage targetcontainer =
    case targetpage of
        ToLoad int a ->
            ( targetcontainer, a )

        _ ->
            ( targetcontainer, Cmd.none )



-- FIlls cache on init


fillcache : Meta a -> Int -> Page a -> Cmd (Msg a)
fillcache meta index page =
    case page of
        ToLoad int a ->
            if (index <= meta.windowSize) then
                a
            else
                Cmd.none

        _ ->
            Cmd.none
