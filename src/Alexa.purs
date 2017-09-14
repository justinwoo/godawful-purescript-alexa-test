module Alexa
  (ALEXA, Alexa, Event, Context, Handler, handler
  ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Newtype (class Newtype)

type AlexaEffects eff = (alexa :: ALEXA | eff)

newtype IntentLabel = IntentLabel String
derive instance ntIntentLabel :: Newtype IntentLabel _

newtype Say = Say String
derive instance ntSay :: Newtype Say _

newtype Listen = Listen String
derive instance ntListen :: Newtype Listen _

foreign import data ALEXA   :: Effect
foreign import data Alexa   :: Type
foreign import data Event   :: Type
foreign import data Context :: Type
foreign import data Handler :: Type

foreign import _init
  :: ∀ eff. Fn2 Event Context (Eff (AlexaEffects eff) Alexa)
init :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Alexa
init = runFn2 _init

foreign import _registerHandler
  :: ∀ eff. Fn3 Alexa String (Eff (AlexaEffects eff) Unit) (Eff (AlexaEffects eff) Unit)
registerHandler
  :: ∀ eff. Alexa -> IntentLabel
  -> (Eff (AlexaEffects eff) Unit)
  -> (Eff (AlexaEffects eff) Unit)
registerHandler alexa (IntentLabel label) =
  runFn3 _registerHandler alexa label

foreign import _execute
  :: ∀ eff. Fn1 Alexa (Eff (AlexaEffects eff) Unit)
execute :: ∀ eff. Alexa -> (Eff (AlexaEffects eff) Unit)
execute = runFn1 _execute

foreign import _speak
  :: ∀ eff. Fn2 Alexa String (Eff (AlexaEffects eff) Unit)
speak :: ∀ eff. Alexa -> Say -> (Eff (AlexaEffects eff) Unit)
speak alexa (Say say) = runFn2 _speak alexa say

foreign import _speakAndListen
  :: ∀ eff. Fn3 Alexa String String (Eff (AlexaEffects eff) Unit)
speakAndListen
  :: ∀ eff. Alexa -> Say -> Listen
  -> (Eff (AlexaEffects eff) Unit)
speakAndListen alexa (Say say) (Listen listen) =
  runFn3 _speakAndListen alexa say listen

registerHelpIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerHelpIntent alexa =
  let label  = IntentLabel "AMAZON.HelpIntent"
      say    = Say "Please refer to the Skyfall readme for example invocations and try again."
      listen = Listen "Please refer to the readme and try again."
      fn     = speakAndListen alexa say listen
  in registerHandler alexa label fn

registerCancelIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerCancelIntent alexa =
  let label  = IntentLabel "AMAZON.CancelIntent"
      say    = Say "Please refer to the Skyfall readme for example invocations and try again."
      fn     = speak alexa say
  in registerHandler alexa label fn

registerStopIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerStopIntent alexa =
  let label  = IntentLabel "AMAZON.StopIntent"
      say    = Say "Please refer to the Skyfall readme for example invocations and try again."
      fn     = speak alexa say
  in registerHandler alexa label fn

registerSpeakIntent :: ∀ eff. Alexa -> Eff (AlexaEffects eff) Unit
registerSpeakIntent alexa =
  let label  = IntentLabel "SpeakIntent"
      say    = Say "Hello"
      fn     = speak alexa say
  in registerHandler alexa label fn

handler :: ∀ eff. Event -> Context -> Eff (AlexaEffects eff) Unit
handler event ctx = do
  alexa <- init event ctx
  registerHelpIntent alexa
  registerCancelIntent alexa
  registerStopIntent alexa
  registerSpeakIntent alexa
  execute alexa
