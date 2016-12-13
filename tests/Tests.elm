module Tests exposing (..)

import Test exposing (..)
import Expect exposing (equal)
import String
import Statistics exposing (..)
import Round exposing (..)


quantileTestData =
    [ ( [ -86.1781991553
        , 99.8557203449
        , 34.7726002801
        , 17.7950058132
        , 98.3702570666
        , 16.3121790159
        , 70.3816040885
        , -8.2830874715
        , 83.6835207883
        , 76.3281899039
        ]
      , [ ( 0, -86.17819916 )
        , ( 0.1, -16.07259864 )
        , ( 0.25, 16.68288572 )
        , ( 0.33, 17.75052101 )
        , ( 0.5, 52.57710218 )
        , ( 0.75, 81.84468807 )
        , ( 0.99, 99.72202865 )
        , ( 1, 99.85572034 )
        ]
      )
    ]


all : Test
all =
    describe "Tests"
        [ describe "divWithDefault"
            [ test "divWithDefault" <|
                \() -> Expect.equal 2 (divWithDefault 0 6 3)
            , test "divWithDefault" <|
                \() -> Expect.equal 2 (divWithDefault 1 6 3)
            , test "divWithDefault" <|
                \() -> Expect.equal 0 (divWithDefault 0 6 0)
            , test "divWithDefault" <|
                \() -> Expect.equal 1 (divWithDefault 1 6 0)
            , test "divWithDefault" <|
                \() -> Expect.equal 0 (divWithDefault 0 0 6)
            , test "divWithDefault" <|
                \() -> Expect.equal 0 (divWithDefault 1 0 6)
            , test "divWithDefault" <|
                \() -> Expect.equal 0 (6 /. 0)
            , test "divWithDefault" <|
                \() -> Expect.equal 0 (0 /. 6)
            ]
        , describe "quantileR7" <|
            List.concat <|
                List.map
                    (\( values, results ) ->
                        List.map
                            (\( p, r ) ->
                                test ("quantileR7 " ++ toString p) <|
                                    \() -> Expect.equal r (roundNum 8 <| quantileR7 p values)
                            )
                            results
                    )
                    quantileTestData
        ]
