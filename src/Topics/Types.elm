module Topics.Types where

-- simplified topic model with only 
type alias TopicTerm =
  { name : String
  , topicFrequency : Float
  , overallFrequency : Float
  }

