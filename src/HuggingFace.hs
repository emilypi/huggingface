-- |
-- Module      : HuggingFace
-- Description : Haskell client for the Hugging Face Hub and hosted inference
-- Copyright   : (c) 2026 Emily Pillmore
-- License     : BSD-3-Clause
-- Maintainer  : emilypi@cohomolo.gy
-- Stability   : experimental
-- Portability : GHC
--
-- Create a reusable 'Client' with 'newClient' and 'defaultClientConfig'.
-- Parse repository names with 'parseRepoId', then call 'modelInfo',
-- 'listModels', or 'chatCompletion'. See the README for complete examples.
-- Compose operations in 'HuggingFaceM' and run them with 'runHuggingFaceM'.
module HuggingFace
    ( module HuggingFace.Client
    , module HuggingFace.Hub
    , module HuggingFace.Inference
    , module HuggingFace.Monad
    , module HuggingFace.Types
    ) where

import HuggingFace.Client
import HuggingFace.Hub
import HuggingFace.Inference
import HuggingFace.Monad
import HuggingFace.Types
