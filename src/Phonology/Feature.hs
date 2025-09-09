module Phonology.Feature where


data Feature =
    Length      LengthType |
    Voicing     VoicingType |
    Aspiration  AspirationType |
    Place       PlaceType |
    Manner      MannerType
    deriving (Show, Eq)

data LengthType =
    Long
    deriving (Show, Eq)

data VoicingType =
    Unvoiced | Voiced
    deriving (Show, Eq)

data AspirationType =
    Unaspirated | Aspirated
    deriving (Show, Eq)

data PlaceType =
    Velar | Palatal | Retroflex | Dental | Labial
    deriving (Show, Eq)

data MannerType =
    Plosive | Nasal | Approximant | Fricative
    deriving (Show, Eq)
