module Compile where

import Prelude
import Chanterelle (compileMain)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)

main :: forall eff.
      Eff
        ( console :: CONSOLE
        , fs :: FS
        , process :: PROCESS
        , exception :: EXCEPTION
        , now :: NOW
        | eff
        )
        Unit
main = compileMain
