module Fuzzy exposing (Config, Result, match, matchWithConfig, sort)


type alias Result =
    { score : Int
    , matches : List Int
    }


type alias Matches =
    List Int


type alias Config =
    { maxRecursionDepth : Int
    , unmatchedLetterPenalty : Int
    , sequentialBonus : Int
    , separatorBonus : Int
    , camelCaseBonus : Int
    , firstLetterBonus : Int
    , leadingLetterPenalty : Int
    , maxLeadingLetterPenalty : Int
    }


type alias Data =
    { needle : String
    , haystack : String
    , needleIndex : Int
    , haystackIndex : Int
    , matches : List Matches
    , recursionDepth : Int
    }


defaultConfig : Config
defaultConfig =
    { maxRecursionDepth = 10
    , sequentialBonus = 30
    , separatorBonus = 30
    , camelCaseBonus = 30
    , firstLetterBonus = 15
    , unmatchedLetterPenalty = -1
    , leadingLetterPenalty = -5
    , maxLeadingLetterPenalty = -15
    }


matchWithConfig : Config -> String -> String -> Result
matchWithConfig config needle haystack =
    let
        matches =
            findMatches
                config
                { needle = String.toLower needle
                , haystack = String.toLower haystack
                , needleIndex = 0
                , haystackIndex = 0
                , matches = []
                , recursionDepth = 0
                }
    in
    matches
        |> List.map
            (\m ->
                { score = score defaultConfig haystack m
                , matches = m
                }
            )
        |> List.sortBy .score
        |> List.reverse
        |> List.head
        |> Maybe.withDefault { score = 0, matches = [] }


match : String -> String -> Result
match =
    matchWithConfig defaultConfig


sort : List String -> String -> List String
sort elements needle =
    let
        getScore needle_ hay =
            match needle_ hay |> .score
    in
    elements
        |> List.sortBy (getScore needle)
        |> List.reverse


findMatches : Config -> Data -> List Matches
findMatches config { needle, haystack, needleIndex, haystackIndex, matches, recursionDepth } =
    if
        needleIndex
            >= String.length needle
            || haystackIndex
            >= String.length haystack
            || recursionDepth
            >= config.maxRecursionDepth
    then
        matches

    else if
        String.slice needleIndex (needleIndex + 1) needle
            == String.slice haystackIndex (haystackIndex + 1) haystack
    then
        {- It's a match!

           needle   =  "c[a]rs"
           haystack = "cl[a]ssic cars"

           Save the current index and continue with matching.
        -}
        let
            currentMatches =
                case matches of
                    [] ->
                        [ [ haystackIndex ] ]

                    x :: xs ->
                        (x ++ [ haystackIndex ]) :: xs

            {- We found a match, but there might be a better one waiting
               around the corner.

               needle   = "cars"
               haystack = "classic cars"

               If we always take the first match we get:
                   "[c]l[a]ssic ca[r][s]"

               With searching for next matches we match the whole word:
                   "classic [c][a][r][s]"

               ... and get a better score.
            -}
            nextMatches =
                let
                    previousMatches =
                        case matches of
                            [] ->
                                []

                            x :: _ ->
                                [ x ]
                in
                findMatches config
                    { needle = needle
                    , haystack = haystack
                    , needleIndex = needleIndex
                    , haystackIndex = haystackIndex + 1
                    , matches = previousMatches
                    , recursionDepth = recursionDepth + 1
                    }
        in
        findMatches config
            { needle = needle
            , haystack = haystack
            , needleIndex = needleIndex + 1
            , haystackIndex = haystackIndex + 1
            , matches = currentMatches ++ nextMatches
            , recursionDepth = recursionDepth
            }

    else
        {- No match :(

           needle   =    "ca[r]s"
           haystack = "class[i]c cars"

           Move haystack index.

        -}
        findMatches config
            { needle = needle
            , haystack = haystack
            , needleIndex = needleIndex
            , haystackIndex = haystackIndex + 1
            , matches = matches
            , recursionDepth = recursionDepth
            }


score : Config -> String -> Matches -> Int
score config haystack matches =
    let
        {- Give penalty for first n unmatched chars -}
        leadingPenalty =
            let
                penalty =
                    config.leadingLetterPenalty * (List.head matches |> Maybe.withDefault 0)
            in
            max config.maxLeadingLetterPenalty penalty

        {- Give penalty for number of unmatched chars -}
        unmatchedPenalty =
            let
                unmatchedLetters =
                    String.length haystack - List.length matches
            in
            config.unmatchedLetterPenalty * unmatchedLetters

        {- Add bonus if first letter is a match -}
        matchFromStartBonus =
            if List.head matches == Just 0 then
                config.firstLetterBonus

            else
                0

        {- Add bonus for each sequential match -}
        sequentialBonus =
            let
                skipFirst =
                    Maybe.withDefault [] (List.tail matches)

                skipLast =
                    List.reverse matches |> List.tail |> Maybe.withDefault [] |> List.reverse

                pairs =
                    List.map2 Tuple.pair skipLast skipFirst

                addBonus ( first, second ) =
                    if (second - first) == 1 then
                        config.sequentialBonus

                    else
                        0
            in
            pairs |> List.map addBonus |> List.sum

        {- Add bonus if match occurs after a separator -}
        separatorBonus =
            let
                addBonus index =
                    if index > 0 then
                        if String.slice (index - 1) index haystack == " " then
                            config.separatorBonus

                        else
                            0

                    else
                        0
            in
            matches |> List.map addBonus |> List.sum

        {- Add bonus if match occurs in start of camel[C]ase word -}
        camelCaseBonus =
            let
                addBonus index =
                    if index > 0 then
                        let
                            previousLetter =
                                String.slice (index - 1) index haystack

                            currentLetter =
                                String.slice index (index + 1) haystack
                        in
                        if
                            previousLetter
                                == String.toLower previousLetter
                                && previousLetter
                                /= " "
                                && currentLetter
                                == String.toUpper currentLetter
                        then
                            config.camelCaseBonus

                        else
                            0

                    else
                        0
            in
            matches |> List.map addBonus |> List.sum
    in
    leadingPenalty
        + unmatchedPenalty
        + matchFromStartBonus
        + sequentialBonus
        + separatorBonus
        + camelCaseBonus
