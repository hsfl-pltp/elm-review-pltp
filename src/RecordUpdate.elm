module RecordUpdate exposing (..)


type alias Vector3 =
    { x : Int
    , y : Int
    , z : Int
    }


type alias Vector2 =
    { x : Int
    , y : Int
    }


allDifferent : Vector3 -> Vector3
allDifferent v =
    { x = v.x + 1, y = v.y - 1, z = v.z + 1 }


oneSame : Vector3 -> Vector3
oneSame v =
    { x = v.x + 1, y = v.y - 1, z = v.z }


twoSame : Vector3 -> Vector3
twoSame { x, y, z } =
    { x = x + 1, y = y, z = z }


alsoAllDifferent : Int -> Vector3 -> Vector3
alsoAllDifferent x v =
    if v.x < 3 then
        { x = x, y = v.y + 1, z = v.z + 1 }

    else
        v


destructedDifferent : Vector3 -> Vector3
destructedDifferent { x, y, z } =
    { x = y, y = x, z = z + 1 }


diffRecordsDifferent : Vector2 -> Vector3 -> Vector3
diffRecordsDifferent v2 v3 =
    { x = v2.x, y = v2.y, z = v3.x + v3.y + v3.z }


diffDestructDifferent : Vector2 -> Vector3 -> Vector3
diffDestructDifferent { x, y } v3 =
    { x = x, y = y, z = v3.x + v3.y + v3.z }


twoDifferent : Vector3 -> Vector3 -> Vector3
twoDifferent v1 v2 =
    { x = v1.x + v1.z + v2.x + v2.y, y = v1.y, z = v2.z }
