-------------------------------------------------------------------
-- |
-- Module    :  Semantics.Types
-- Copyright :  (c) Zach Kimberg 2020
-- License   :  MIT
-- Maintainer:  zachary@kimberg.com
-- Stability :  experimental
-- Portability: non-portable
--
-- This module defines the 'Type' which are used to represent
-- Catln types. It also includes functions for the various set
-- operations implemented for types.
--------------------------------------------------------------------


module Semantics.Types (
  module Semantics.Types.Name,
  module Semantics.Types.Data,
  module Semantics.Types.Memo,
  module Semantics.Types.Construct,
  module Semantics.Types.Preds,
  module Semantics.Types.ArgModes,
  module Semantics.Types.Vars,
  module Semantics.Types.SatGoal,
  module Semantics.Types.Algebra
  ) where

import           Semantics.Types.Algebra
import           Semantics.Types.ArgModes
import           Semantics.Types.Construct
import           Semantics.Types.Data
import           Semantics.Types.Instances ()
import           Semantics.Types.Memo
import           Semantics.Types.Name
import           Semantics.Types.Preds
import           Semantics.Types.SatGoal
import           Semantics.Types.Vars
