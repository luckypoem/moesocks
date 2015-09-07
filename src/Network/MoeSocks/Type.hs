{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.MoeSocks.Type 
(
  module Network.MoeSocks.Type.Runtime
, module Network.MoeSocks.Type.Common
)
where

import Network.MoeSocks.Type.Runtime hiding (forbidden_IPs, obfuscation)
import Network.MoeSocks.Type.Common


