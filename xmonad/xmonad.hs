-- XMonad Tiling Window Manager Configuration.
-- P.C. Shyamshankar <sykora@lucentbeing.com>

import IO
import System.Exit

import qualified Data.Map as M

import XMonad

import qualified XMonad.StackSet as W

-- Keys

myModMask = mod4Mask -- The Windows Key, aka "Super"

myKeys config@(XConfig {XMonad.modMask = m}) = M.fromList $

    [
        -- The Terminal
        ((m, xK_c), spawn $ XMonad.terminal config), -- Start a new terminal.

        -- Window Navigation
        ((m, xK_t), windows W.focusDown), -- Focus next window.
        ((m, xK_s), windows W.focusUp), -- Focus previous window.

        -- Window Management
        ((m, xK_x), kill), -- Kill the window.

        ((m .|. shiftMask, xK_t), windows W.swapDown), -- Swap with next.
        ((m .|. shiftMask, xK_s), windows W.swapUp), -- Swap with previous.

        -- Layout Management
        ((m, xK_space), sendMessage NextLayout), -- Rotate to next layout.
        ((m, xK_comma), sendMessage (IncMasterN 1)), -- Increment number of master windows.
        ((m, xK_period), sendMessage (IncMasterN (-1)), -- Decrement number of master windows.

        -- XMonad Control
        ((m, xK_q), restart "xmonad" True), -- Restart XMonad.
        ((m .|. shiftMask, xK_F12), io (exitWith ExitSuccess)), -- Quit XMonad.

    ]
    ++

    -- Map the workspace access keys.
    -- mod + xK_0 .. xK_9 -> Switch to the corresponding workspace (greedyView) 
    -- mod + shift + xK_0 .. xK_9 -> Move current window to corresponding workspace. 
    [((m .|. shiftMask', numberKey), windows $ windowAction workspace)
        | (workspace, numberKey) <- zip (XMonad.workspaces config) [xK_1 .. xK_9]
        , (shiftMask', windowAction) <- [(0, W.greedyView), (shiftMask, W.shift)]]

-- Amalgamation of settings.

myConfig = defaultConfig {
    modMask = myModMask,
    keys    = myKeys
}

-- Run it.

main = do
    xmonad $ myConfig
