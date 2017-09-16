module Alexa
  ( ALEXA
  , Alexa
  , Event
  , Context
  , This
  , handler
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn4, runEffFn1, runEffFn2, runEffFn3, runEffFn4)
import Data.Newtype (class Newtype, unwrap)

type AlexaEffects eff = (alexa :: ALEXA, console :: CONSOLE | eff)

-- | Type of effects performed by Alexa handlers, and associated types used
-- | by the SDK.
foreign import data ALEXA   :: Effect
foreign import data Alexa   :: Type
foreign import data Event   :: Type
foreign import data Context :: Type
foreign import data This    :: Type

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

-- | Register an intent handler that speaks to the user.
foreign import _registerSay
  :: ∀ eff.
     EffFn3 (AlexaEffects eff)
       Alexa
       String
       String
       Unit
registerSay
  :: ∀ eff
   . Alexa
  -> IntentLabel
  -> Say
  -> (Eff (AlexaEffects eff) Unit)
registerSay alexa (IntentLabel label) (Say say) =
  runEffFn3 _registerSay alexa label say

-- | Register an intent handler that speaks to the user and listens for a response.
foreign import _registerSayAndListen
  :: ∀ eff.
     EffFn4 (AlexaEffects eff)
       Alexa
       String
       String
       String
       Unit
registerSayAndListen
  :: ∀ eff
   . Alexa
  -> IntentLabel
  -> Say
  -> Listen
  -> (Eff (AlexaEffects eff) Unit)
registerSayAndListen alexa (IntentLabel label) (Say say) (Listen listen) =
  runEffFn4 _registerSayAndListen alexa label say listen

-- | Execute the given Alexa handler.
foreign import _execute :: ∀ eff. EffFn1 (AlexaEffects eff) Alexa Unit
execute :: ∀ eff. Alexa -> (Eff (AlexaEffects eff) Unit)
execute = runEffFn1 _execute

--------------------------------------------------------------------------------
-- This works
--------------------------------------------------------------------------------

registerHelpIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerHelpIntent alexa =
  let label  = IntentLabel "AMAZON.HelpIntent"
      say    = Say "Please refer to the readme for example invocations and try again."
      listen = Listen "Please refer to the readme and try again."
  in do
    log "Registered help intent"
    registerSayAndListen alexa label say listen

registerCancelIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerCancelIntent alexa =
  let label = IntentLabel "AMAZON.CancelIntent"
      say   = Say "Goodbye."
  in do
    log "Registered cancel intent"
    registerSay alexa label say

registerStopIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerStopIntent alexa =
  let label = IntentLabel "AMAZON.StopIntent"
      say   = Say "Goodbye."
  in do
    log "Registered stop intent"
    registerSay alexa label say

registerSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerSpeakIntent alexa =
  let label = IntentLabel "SpeakIntent"
      say   = Say "Hello"
  in do
    log "Registered speak intent"
    registerSay alexa label say

handler :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Unit
handler event ctx = do
  alexa <- init event ctx

  registerHelpIntent alexa
  registerCancelIntent alexa
  registerStopIntent alexa
  -- registerSpeakIntent alexa
  failRegisterSpeakIntent alexa

  execute alexa

--------------------------------------------------------------------------------
-- This doesn't work
--------------------------------------------------------------------------------

-- | Register an intent handler.
foreign import _registerHandler
  :: ∀ eff.
     EffFn3 (AlexaEffects eff)
       Alexa
       String
       (This -> Eff (AlexaEffects eff) Unit)
       Unit
registerHandler
  :: ∀ eff
   . Alexa
  -> IntentLabel
  -> (This -> Eff (AlexaEffects eff) Unit)
  -> (Eff (AlexaEffects eff) Unit)
registerHandler alexa (IntentLabel label) fn =
  runEffFn3 _registerHandler alexa label fn

foreign import _speak
  :: ∀ eff.
    EffFn2 (AlexaEffects eff)
      String
      This
      Unit
speak :: ∀ eff. Say -> This -> Eff (AlexaEffects eff) Unit
speak = runEffFn2 _speak <<< unwrap

failRegisterSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
failRegisterSpeakIntent alexa =
  let label = IntentLabel "SpeakIntent"
      say   = Say "Hello"
      fn    = speak say
  in do
    log "Registered speak intent"
    registerHandler alexa label fn
