import qualified Data.ByteString.Char8 as C

data TSuit = H | D | C | S deriving (Eq,Read,Show)

data TCard = Card { suit :: TSuit, value :: Int } deriving (Eq,Show)

instance Ord TCard where
    (Card _ a) `compare` (Card _ b) = a `compare` b

data TCombination = HighCard [TCard] | 
                    Pair Int |
                    TwoPairs Int Int |
                    Three Int |
                    Straight [TCard] | 
                    Flush [TCard] |
                    FullHouse TCombination TCombination |
                    Four Int |
                    StraightFlush [TCard] |
                    RoyalFlush deriving (Eq,Ord)

data THand = Hand [TCombination]

parseCard :: String -> TCard
parseCard (v:s:[]) = Card (read [s]) (read [v])
parseCard (_:_:s:[]) = Card (read [s]) 10

readHands :: C.ByteString -> ([TCard],[TCard])
