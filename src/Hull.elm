module Hull exposing (findHull)

import List
import List.Extra
import Polygon exposing (..)


type Turn
    = Left
    | Right
    | Straight


turn : Point -> Point -> Point -> Turn
turn (Point ax ay) (Point bx by) (Point cx cy) =
    let
        x =
            (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    in
        if x < 0 then
            Right
        else if x > 0 then
            Left
        else
            Straight


type alias Comparator a =
    a -> a -> Order


natural : Comparator comparable
natural x y =
    if x < y then
        LT
    else if x > y then
        GT
    else
        EQ


compareFieldWith : (a -> b) -> Comparator b -> Comparator a
compareFieldWith fieldSelector fieldComparator x y =
    fieldComparator (fieldSelector x) (fieldSelector y)


compareField : (a -> comparable) -> Comparator a
compareField fieldSelector =
    compareFieldWith fieldSelector natural


breakTiesWith : Comparator a -> Comparator a -> Comparator a
breakTiesWith tiebreaker mainComparator x y =
    case mainComparator x y of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            tiebreaker x y


minWith : Comparator a -> a -> a -> a
minWith comparator a b =
    case comparator a b of
        LT ->
            a

        GT ->
            b

        EQ ->
            a


minimumWith : Comparator a -> List a -> Maybe a
minimumWith comparator elements =
    let
        -- minimumWith' : a -> List a -> a
        minimumWithInternal prev elts =
            case elts of
                [] ->
                    prev

                elt :: restElts ->
                    minimumWithInternal (minWith comparator prev elt) restElts
    in
        case elements of
            [] ->
                Nothing

            element :: restElements ->
                Just <| minimumWithInternal element restElements


addPointToHull : Point -> List Point -> List Point
addPointToHull pt hull =
    case hull of
        y :: x :: rest ->
            case turn x y pt of
                Left ->
                    pt :: y :: x :: rest

                _ ->
                    addPointToHull pt (x :: rest)

        _ ->
            pt :: hull


findHull : List Point -> Maybe (List Point)
findHull points =
    if List.length points < 3 then
        Nothing
    else
        case minimumWith (compareField getY |> breakTiesWith (compareField getX)) points of
            Nothing ->
                Nothing

            Just minPoint ->
                List.Extra.remove minPoint points
                    |> List.sortWith (compareField (angle minPoint) |> breakTiesWith (compareField getY))
                    |> List.foldl addPointToHull [ minPoint ]
                    |> \hull ->
                        if List.length hull > 2 then
                            Just hull
                        else
                            Nothing


angle : Point -> Point -> Float
angle (Point ax ay) (Point bx by) =
    atan2 (toFloat (by - ay)) (toFloat (bx - ax))
