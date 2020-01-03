module Card where

data Color 
    = Red
    | Green
    | Blue
    | Black
    | White
    | NoColor 

data CastingCost = CastingCost [([Color] Int)]

newtype  Power = Power Int
newtype  Toughness  = Power Int

data Ability
    
data Card 
    = Creature CastingCost Power Toughness 
    | Instant CastingCost
    | Sorcery CastingCost
    | Land [[Color], Int)]

data PlayerNum = PlayerNum Int

data Deck = Deck [Card]
data Hand = Hand [Card]
data Graveyard = Graveyard [Card]

--data Stack = Stack [(Card, PlayerNum)]
--data Battlefield = [(Card, PlayerNum)]


-- data Deck = Deck    
{-    
data Space
    = Gaveyard
    | Deck
    | Hand
    | Battlefield
    | Stack
-}

--data Ability
--    = TriggeredAbility
  
{-
data CardInGame
    { cardType :: CardType

    }    
-}
