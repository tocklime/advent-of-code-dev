-- |
-- Module      : AOC.Prelude
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Custom Prelude while developing challenges.  Ideally, once challenges
-- are completed, an import to this module would be replaced with explicit
-- ones for future readers.
--
module AOC.MinimalPrelude
  ( module P
  ) where

import           AOC.Common as P
import           AOC.Solver as P
import           AOC.Util   as P
