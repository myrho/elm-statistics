module Tests where

import ElmTest exposing (..)

import Statistics exposing (..)

all : Test
all =
  suite "All"
    [ test "divWithDefault"
      <| assertEqual 2 (divWithDefault 0 6 3)
    , test "divWithDefault"
      <| assertEqual 2 (divWithDefault 1 6 3)
    , test "divWithDefault"
      <| assertEqual 0 (divWithDefault 0 6 0)
    , test "divWithDefault"
      <| assertEqual 1 (divWithDefault 1 6 0)
    , test "divWithDefault"
      <| assertEqual 0 (divWithDefault 0 0 6)
    , test "divWithDefault"
      <| assertEqual 0 (divWithDefault 1 0 6)
    , test "divWithDefault"
      <| assertEqual 0 (6 /. 0)
    , test "divWithDefault"
      <| assertEqual 0 (0 /. 6)
    ]

