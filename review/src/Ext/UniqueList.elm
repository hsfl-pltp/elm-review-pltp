{--
TODO: !!!!!!!!!!!!!!!!!!!!!!
TODO: Dokumentation anpassen
TODO: !!!!!!!!!!!!!!!!!!!!!!
--}


module Ext.UniqueList exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import AssocList as Dict
import Basics exposing (Bool, Int)
import List exposing ((::))


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set t
    = S (Dict.Dict t ())


{-| Create an empty set.
-}
empty : Set a
empty =
    S Dict.empty


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton key =
    S (Dict.singleton key ())


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert key (S dict) =
    S (Dict.insert key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove key (S dict) =
    S (Dict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (S dict) =
    Dict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member key (S dict) =
    Dict.member key dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (S dict) =
    Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : Set a -> Set a -> Set a
union (S dict1) (S dict2) =
    S (Dict.union dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set a -> Set a -> Set a
intersect (S dict1) (S dict2) =
    S (Dict.intersect dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set a -> Set a -> Set a
diff (S dict1) (S dict2) =
    S (Dict.diff dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (S dict) =
    Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> Set a
fromList list =
    List.foldl insert empty list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (S dict) =
    Dict.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (S dict) =
    Dict.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> b) -> Set a -> Set b
map func set =
    fromList (foldl (\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
        Set.fromList [ -2, -1, 0, 1, 2 ]

    positives : Set Int
    positives =
        Set.filter (\x -> x > 0) numbers

    -- positives == Set.fromList [1,2]

-}
filter : (a -> Bool) -> Set a -> Set a
filter isGood (S dict) =
    S (Dict.filter (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (a -> Bool) -> Set a -> ( Set a, Set a )
partition isGood (S dict) =
    let
        ( dict1, dict2 ) =
            Dict.partition (\key _ -> isGood key) dict
    in
    ( S dict1, S dict2 )
