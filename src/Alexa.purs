module Alexa
  ( ALEXA
  , Alexa
  , Event
  , Context
  , HandlerThis
  , handler
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, runEffFn1, runEffFn2, runEffFn3)
import Data.Newtype (class Newtype)

type AlexaEffects eff = (alexa :: ALEXA, console :: CONSOLE | eff)

-- | Type of effects performed by Alexa handlers, and associated types used
-- | by the SDK.
foreign import data ALEXA       :: Effect
foreign import data Alexa       :: Type
foreign import data Event       :: Type
foreign import data Context     :: Type
foreign import data HandlerThis :: Type

-- | Newtype wrappers for common arguments

newtype IntentLabel = IntentLabel String
derive instance ntIntentLabel :: Newtype IntentLabel _

newtype Say = Say String
derive instance ntSay :: Newtype Say _

newtype Listen = Listen String
derive instance ntListen :: Newtype Listen _

-- | Given an `Event` and `Context` from AWS Lambda, return an initialized
-- | Alexa handler.
foreign import _init
  :: ∀ eff. EffFn2 (AlexaEffects eff) Event Context Alexa
init :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Alexa
init = runEffFn2 _init

-- | Given an initialized Alexa handler, register an intent function.
foreign import _registerHandler
  :: ∀ eff.
     EffFn3 (AlexaEffects eff)
       Alexa
       String
       (HandlerThis -> Eff (AlexaEffects eff) Unit)
       Unit
registerHandler
  :: ∀ eff
   . Alexa
  -> IntentLabel
  -> (HandlerThis -> (Eff (AlexaEffects eff) Unit))
  -> (Eff (AlexaEffects eff) Unit)
registerHandler alexa (IntentLabel label) fn =
  runEffFn3 _registerHandler alexa label fn

-- | Execute the given Alexa handler.
foreign import _execute :: ∀ eff. EffFn1 (AlexaEffects eff) Alexa Unit
execute :: ∀ eff. Alexa -> (Eff (AlexaEffects eff) Unit)
execute = runEffFn1 _execute

-- | Register an intent that just speaks.
foreign import _speak :: ∀ eff. EffFn2 (AlexaEffects eff) HandlerThis String Unit
speak :: ∀ eff. HandlerThis -> Say -> (Eff (AlexaEffects eff) Unit)
speak this (Say say) = runEffFn2 _speak this say

-- | Register an intent that speaks and listens for a user response.
foreign import _speakAndListen
  :: ∀ eff.
     EffFn3 (AlexaEffects eff)
       HandlerThis
       String
       String
       Unit
speakAndListen
  :: ∀ eff
   . HandlerThis
  -> Say
  -> Listen
  -> (Eff (AlexaEffects eff) Unit)
speakAndListen this (Say say) (Listen listen) =
  runEffFn3 _speakAndListen this say listen

registerHelpIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerHelpIntent alexa =
  let label  = IntentLabel "AMAZON.HelpIntent"
      say    = Say "Please refer to the readme for example invocations and try again."
      listen = Listen "Please refer to the readme and try again."
      fn     = \this -> speakAndListen this say listen
  in do
    log "Registered help intent"
    registerHandler alexa label fn

registerCancelIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerCancelIntent alexa =
  let label  = IntentLabel "AMAZON.CancelIntent"
      say    = Say "Goodbye."
      fn     = \this -> speak this say
  in do
    log "Registered cancel intent"
    registerHandler alexa label fn

registerStopIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerStopIntent alexa =
  let label  = IntentLabel "AMAZON.StopIntent"
      say    = Say "Goodbye."
      fn     = \this -> speak this say
  in do
    log "Registered stop intent"
    registerHandler alexa label fn

registerSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerSpeakIntent alexa =
  let label  = IntentLabel "SpeakIntent"
      say    = Say "Hello"
      fn     = \this -> speak this say
  in do
    log "Registered speak intent"
    registerHandler alexa label fn

handler :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Unit
handler event ctx = do
  alexa <- init event ctx

  registerHelpIntent alexa
  registerCancelIntent alexa
  registerStopIntent alexa
  registerSpeakIntent alexa

  execute alexa
