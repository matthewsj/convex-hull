module Polygon exposing (..)


type Point
    = Point Int Int


getX : Point -> Int
getX (Point x _) =
    x


getY : Point -> Int
getY (Point _ y) =
    y


type alias Polygon =
    List Point


type alias Edge =
    ( Point, Point )


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        x1 :: x2 :: xs_ ->
            ( x1, x2 ) :: (pairs (x2 :: xs_))

        _ ->
            []


edges : Polygon -> List Edge
edges xs =
    case ( xs, last xs ) of
        ( a :: rest, Just z ) ->
            ( a, z ) :: (pairs xs)

        _ ->
            []


last : List a -> Maybe a
last xs =
    case xs of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            last xs



{-
   pointInPolygon : Point -> Polygon -> Bool
   pointInPolygon point polygon =
       let edgeList = edges polygon
                      candidates = List.filter (\(p1, p2) ->
       in
-}
