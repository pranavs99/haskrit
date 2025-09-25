module Phonology.Phoneme.Feature where

import Phonology.Utilities as Utils


{-  phonological feature
    def) a property of a Phoneme
-}
data Feature
    = Length LengthType
    | Voicing VoicingType
    | Aspiration AspirationType
    | Place PlaceType
    | Manner MannerType
    | VowelClass VowelClassType
    deriving (Show, Eq)

{-
    length
    def) whether a sound is sustained longer

    (only defined for vowels)
-}
data LengthType =
    Short | Long
    deriving (Show, Eq, Enum)

lengthOf :: [Feature] -> Feature
lengthOf [] = error "invariant violated: no length found"
lengthOf (curr : rest) = case curr of
    Length _    -> curr
    _           -> lengthOf rest

isLong :: [Feature] -> Bool
isLong features =
    lengthOf features == Length Long

{-
    voicing
    def) whether the larynx is vibrating
-}
data VoicingType =
    Unvoiced | Voiced
    deriving (Show, Eq, Enum)

voicingOf :: [Feature] -> Feature
voicingOf [] = error "invariant violated: no voicing found"
voicingOf (curr : rest) = case curr of
    Voicing _   -> curr
    _           -> voicingOf rest

isVoiced :: [Feature] -> Bool
isVoiced features =
    voicingOf features == Voicing Voiced

{-
    aspiration
    def) whether a puff of air follows the sound
-}
data AspirationType =
    Unaspirated | Aspirated
    deriving (Show, Eq)

aspirationOf :: [Feature] -> Feature
aspirationOf [] = error "invariant violated: no aspiration found"
aspirationOf (curr : rest) = case curr of
    Aspiration _    -> curr
    _               -> aspirationOf rest

isAspirated :: [Feature] -> Bool
isAspirated features =
    aspirationOf features == Aspiration Aspirated

{-
    place of articulation
    def) which articulators make a constriction
-}
data PlaceType =
    Velar | Palatal | Retroflex | Dental | Labial | Bilabial
    deriving (Show, Eq)

placeOf :: [Feature] -> Feature
placeOf [] = error "invariant violated: no place found"
placeOf (curr : rest) = case curr of
    Place _ -> curr
    _       -> placeOf rest

{-
    manner of articulation
    def) degree of constriction in oral cavity
-}
data MannerType =
    Vocalic | Plosive | Nasal | Approximant | Fricative
    deriving (Show, Eq)

mannerOf :: [Feature] -> Feature
mannerOf [] = error "invariant violated: no manner found"
mannerOf (curr : rest) = case curr of
    Manner _    -> curr
    _           -> mannerOf rest

{-
    vowel class
    def) type of vowel

    (only defined for vowels)
-}
data VowelClassType =
    Monophthong | Composite | Diphthong
    deriving (Show, Eq)

vowelClassOf :: [Feature] -> Feature
vowelClassOf [] = error "invariant violated: no vowel class found"
vowelClassOf (curr : rest) = case curr of
    VowelClass _    -> curr
    _               -> vowelClassOf rest

{-
    feature accessor by ID

    input:  - feature ID as String
            - list of Features to search in
    output: a confirmed Feature object of matching constructor

    errors: on invalid feature ID
-}
findFeatureById :: String -> [Feature] -> Feature
findFeatureById "length" features       = lengthOf features
findFeatureById "voicing" features      = voicingOf features
findFeatureById "aspiration" features   = aspirationOf features
findFeatureById "place" features        = placeOf features
findFeatureById "manner" features       = mannerOf features
findFeatureById "class" features        = vowelClassOf features
findFeatureById otherId features = Utils.errorMessages
    [ "invariant violated, no feature for ID: " ++ show otherId
    , "found in features: " ++ show features ]

{-
    "multiplexed" function to check whether two given Feature objects are of
    the same constructor

    input:  two Feature objects
    output: whether they use the same constructor
-}
featuresMatch :: Feature -> Feature -> Bool
featuresMatch new old = case (new, old) of
    (Length _, Length _)            -> True
    (Voicing _, Voicing _)          -> True
    (Aspiration _, Aspiration _)    -> True
    (Place _, Place _)              -> True
    (Manner _, Manner _)            -> True
    (VowelClass _, VowelClass _)    -> True
    _                               -> False

{-
    singular Feature mutator

    input:  a new Feature to add / replace
    output: list of Features with new Feature either added anew or replaced
-}
updateFeature :: Feature -> [Feature] -> [Feature]
updateFeature newValue oldFeatures =
    reverse (update newValue oldFeatures []) where
    update :: Feature -> [Feature] -> [Feature] -> [Feature]
    update new [] featureAcc = new : featureAcc
    update new (currOld : restOlds) featureAcc
        | featuresMatch new currOld = new : featureAcc
        | otherwise                 = update new restOlds (currOld : featureAcc)

{-
    multiple Feature mutator

    input:  list of Features to add / replace, list of Features to mutate
    output: a list with Features either mutated or added new if not found
-}
updateFeatures :: [Feature] -> [Feature] -> [Feature]
updateFeatures [] olds = olds
updateFeatures (currNew : restNew) olds =
    updateFeatures restNew (updateFeature currNew olds)
