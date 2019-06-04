module Image exposing
    ( Promise(..)
    , catResolved
    , imageStoreToList
    )

import IPC.Types as IPC
    exposing
        ( Image
        , ImageStore
        , PendingImage
        )


type Promise
    = Pending PendingImage
    | Resolved IPC.Image


catResolved : List Promise -> List Image
catResolved input =
    input
        |> List.foldl
            (\p imgs ->
                case p of
                    Pending _ ->
                        imgs

                    Resolved img ->
                        imgs ++ [ img ]
            )
            []


imageStoreToList : ImageStore -> List Promise
imageStoreToList imageStore =
    let
        pendingImages =
            imageStore.pending
                |> Maybe.map (List.map Pending)
                |> Maybe.withDefault []

        importedImages =
            imageStore.imported
                |> List.map Resolved
    in
    pendingImages
        ++ importedImages
        |> List.sortBy
            (\img ->
                case img of
                    Pending { fileName } ->
                        fileName

                    Resolved { name } ->
                        name
            )



--
--thumbnail : Image -> Element msg
--thumbnail image =
--    column []
--        [ Element.image [ height <| fill ]
--            { src = File.pathToUrl image.thumbnail
--            , description = ""
--            }
--        , row []
--            [ text image.name
--            , el [] <| text "x"
--            ]
--        ]
