-- XMonad Tiling Window Manager Configuration.
-- P.C. Shyamshankar <sykora@lucentbeing.com>

import IO
import System.Exit

import qualified Data.Map as M

import XMonad

import qualified XMonad.StackSet as W

-- Amalgamation of settings.

myConfig = defaultConfig

-- Run it.

main = do
    xmonad $ myConfig
