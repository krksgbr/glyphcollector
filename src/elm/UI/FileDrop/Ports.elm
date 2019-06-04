port module UI.FileDrop.Ports exposing (dropHandled, windowDragEnd, windowDragStart)

-- TODO
-- These ports are for the implementation of the transparent file-drop UI.
-- We cannot simply render it as an invisible overlay, because
-- it would not let through click-events to the underlying elements.
-- "pointer-events: none" would also disable drag and drop events, so that's no solution.
-- Instead, we listen to drag events on window on the js side and use these ports to set
-- {isWindowDraggedOver: Bool} in FileDrop.Model, then conditionally render the transparent overlay
-- based on this state.
-- dropHandled is needed because, the drop event will not be triggered on window,
-- because the DropZone library cancels propagation, but we still want to know about it, so that
-- other FileDrop instances can reset `isWindowDraggedOver` to False.
-- Look at FileDrop.elm and index.js for more details.
-- Perhaps there is a neater solution.


port windowDragStart : (() -> msg) -> Sub msg


port windowDragEnd : (() -> msg) -> Sub msg


port dropHandled : () -> Cmd msg
