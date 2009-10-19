-- XMonad Tiling Window Manager Configuration.
-- P.C. Shyamshankar <sykora@lucentbeing.com>

import IO
import System.Exit

import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS

import qualified XMonad.StackSet as W

-- Appearance

myBorderWidth = 0 -- No borders around windows, I think I can manage.

-- Default Applications

myTerminal = "urxvtc"
myDMenu = "x=$(dmenu_path | dmenu -i " ++
          "-fn xft:'Envy Code R':pixelsize=18 " ++
          "-nb \"#000000\" " ++
          "-nf \"#AFAFAF\" " ++
          "-sb \"#ECAB00\" " ++
          "-sf \"#FFFFFF\" " ++
          ") && eval \"exec $x\""

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
        ((m, xK_period), sendMessage (IncMasterN (-1))), -- Decrement number of master windows.

        -- Application Shortcuts
        ((m, xK_p), spawn myDMenu),

        -- XMonad Control
        ((m, xK_q), restart "xmonad" True), -- Restart XMonad.
        ((m .|. shiftMask, xK_F12), io (exitWith ExitSuccess)) -- Quit XMonad.
    ]
    ++

    -- Map the workspace access keys.
    -- mod + xK_0 .. xK_9 -> Switch to the corresponding workspace (greedyView) 
    -- mod + shift + xK_0 .. xK_9 -> Move current window to corresponding workspace. 
    [((m .|. shiftMask', numberKey), windows $ windowAction workspace)
        | (workspace, numberKey) <- zip (XMonad.workspaces config) [xK_1 .. xK_9]
        , (shiftMask', windowAction) <- [(0, W.greedyView), (shiftMask, W.shift)]]

myMouseBindings (XConfig {XMonad.modMask = m}) = M.fromList $

    [
        ((m, button1), (\w -> focus w >> mouseMoveWindow w)), -- Float and move while dragging.
        ((m, button2), (\w -> focus w >> windows W.swapMaster)), -- Raise window to top of stack.
        ((m, button3), (\w -> focus w >> mouseResizeWindow w)), -- Float and resize while dragging.
        ((m, button4), (\_ -> prevWS)), -- Switch to previous workspace.
        ((m, button5), (\_ -> nextWS)) -- Switch to next workspace.
    ]

-- Amalgamation of settings.

myConfig = defaultConfig {
    terminal      = myTerminal,

    modMask       = myModMask,
    keys          = myKeys,
    mouseBindings = myMouseBindings,

    borderWidth   = myBorderWidth
}

-- Run it.

main = do
    xmonad $ myConfig
