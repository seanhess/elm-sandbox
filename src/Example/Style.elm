module Example.Style where

-- styles ------------------------------------------

type alias Style = List (String, String)

background : Style
background = [("background-color", "red")]

clickable : Style
clickable = [("cursor", "pointer")]
