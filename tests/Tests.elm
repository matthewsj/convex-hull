module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (Fuzzer, list, int, tuple, string)
import Hull exposing (..)
import Polygon exposing (..)


point : Fuzzer Point
point =
    Fuzz.map2 Point int int


kdescribe : String -> a -> Test
kdescribe text stuff =
    describe text []


all : Test
all =
    describe "Convex hull tests"
        [ describe "Unit test examples (found by fuzz test)"
            [ test "removes collinear points" <|
                \_ ->
                    let
                        pts =
                            [ Point -1 0, Point -2 0, Point 0 0, Point 0 -1 ]
                    in
                        Expect.equal
                            (Just [ Point -2 0, Point 0 0, Point 0 -1 ])
                            (findHull pts)
            , test "Another collinear case" <|
                \_ ->
                    let
                        pts =
                            [ Point 0 0, Point 1 0, Point 0 -1, Point 0 -2 ]
                    in
                        Expect.equal
                            (Just [ Point 0 0, Point 1 0, Point 0 -2 ])
                            (findHull pts)
            ]
        , describe "Property tests"
            [ fuzz (list point) "Hull is idempotent" <|
                \points ->
                    let
                        hull =
                            findHull points
                    in
                        case hull of
                            Nothing ->
                                Expect.true "" True

                            Just hullPoints ->
                                case findHull hullPoints of
                                    Nothing ->
                                        Expect.true "Expected points to be produced" False

                                    Just pts2 ->
                                        Expect.equal hullPoints pts2
            ]
        ]
