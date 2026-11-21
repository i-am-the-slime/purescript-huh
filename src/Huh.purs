-- | A PureScript wrapper for `github.com/charmbracelet/huh` — interactive
-- | forms for the terminal. This binding exposes the three workhorse
-- | prompts (input, confirm, select) as `Aff` actions.
-- |
-- | Each prompt blocks until the user submits, so don't run them from
-- | inside an already-running Bubbletea program — they'll fight for the
-- | TTY. Use them for one-shot flows (init scripts, CLI prompts, etc.).
module Huh
  ( runInput
  , runConfirm
  , runSelect
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error, error)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)

type InputOpts =
  { title :: String
  , prompt :: String
  , placeholder :: String
  }

type ConfirmOpts =
  { title :: String
  , affirmative :: String
  , negative :: String
  }

type SelectOpts =
  { title :: String
  , options :: Array String
  }

foreign import runInputImpl
  :: EffectFn2 InputOpts (EffectFn1 (Either String String) Unit) Unit

foreign import runConfirmImpl
  :: EffectFn2 ConfirmOpts (EffectFn1 (Either String Boolean) Unit) Unit

foreign import runSelectImpl
  :: EffectFn2 SelectOpts (EffectFn1 (Either String String) Unit) Unit

runInput :: InputOpts -> Aff String
runInput opts = makeAff \resolve -> do
  runEffectFn2 runInputImpl opts (mkEffectFn1 (resolveEither resolve))
  pure nonCanceler

runConfirm :: ConfirmOpts -> Aff Boolean
runConfirm opts = makeAff \resolve -> do
  runEffectFn2 runConfirmImpl opts (mkEffectFn1 (resolveEither resolve))
  pure nonCanceler

runSelect :: SelectOpts -> Aff String
runSelect opts = makeAff \resolve -> do
  runEffectFn2 runSelectImpl opts (mkEffectFn1 (resolveEither resolve))
  pure nonCanceler

resolveEither
  :: forall a
   . (Either Error a -> Effect Unit)
  -> Either String a
  -> Effect Unit
resolveEither resolve = case _ of
  Left msg -> resolve (Left (error msg))
  Right a -> resolve (Right a)
