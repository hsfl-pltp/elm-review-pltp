module SinglePatternCase exposing (test1, test2)


type One = One Int

test1 : One -> Int
test1 one =
  case one of
      One n -> n

test2 : One -> Int
test2 (One n) = n
