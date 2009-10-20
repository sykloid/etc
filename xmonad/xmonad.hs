-- XMonad Tiling Window Manager Configuration.
-- P.C. Shyamshankar <sykora@lucentbeing.com>

import System.IO
import System.Exit

import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

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

-- Statusbar

myDzenBar = "dzen2 -x 0 -y 0 -h 18 -w 900 -p -ta l -fn \"Envy Code R:size=11\" -bg \"#000000\" -fg \"#AFAFAF\""

myDzenMPDBar = "dzen2 -x 900 -y 0 -h 18 -w 540 -p -ta r -fn \"Envy Code R:size=11\" -bg \"#000000\" -fg \"#FF00AA\""

myDzenDateBar = "dzen2 -x 1440 -y 0 -h 18 -w 240 -p -ta r -fn \"Envy Code R:size=11\" -bg \"#000000\" -fg \"#6294CF\""

dzenStatusLogger handle = dynamicLogWithPP $ defaultPP {
    ppOutput  = hPutStrLn handle,
    ppCurrent = (\wsID -> "^fg(#FFAF00)[" ++ wsID ++ "]^fg()"),
    ppSep = " | ",
    ppTitle = (\title -> "^fg(#92FF00)" ++ title ++ "^fg()"),
    ppLayout = (\layout -> case layout of
                    "Tall" -> "Tall ^i(/home/sykora/.icons/dzen2/tall.xbm)"
                    "Mirror Tall" -> "Mirror Tall ^i(/home/sykora/.icons/dzen2/mirror_tall.xbm)"
                    "Full" -> "Full ^i(/home/sykora/.icons/dzen2/full.xbm)"
                )
}


-- Workspaces

myWorkspaces = ["main", "web", "3", "4", "5", "6", "7", "8", "irc"]

-- Layouts

-- A constructed default tiling layout, 2 panes of windows.
tiledLayout = Tall masterCapacity resizeDelta defaultRatio
    where
        masterCapacity = 1 -- Number of master windows by default.
        resizeDelta    = 3/100 -- Percent to increase the size by each time.
        defaultRatio   = 1/2 -- Default screen ratio of master : others.

-- avoidStruts makes room for the status bars.
myLayoutHook = avoidStruts $ tiledLayout ||| Mirror tiledLayout ||| Full

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

        -- Mouse Management.
        ((m, xK_b), banish LowerRight), -- Banish mouse to the lower right corner of the screen.

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

-- Run it.

main = do
    dzenBar <- spawnPipe myDzenBar
    dzenDateBar <- spawnPipe $ "/home/sykora/_etc/xmonad/dzen_date_bar.zsh | " ++ myDzenDateBar
    dzenMPDBar <- spawnPipe $ "/home/sykora/_etc/xmonad/dzen_mpd_bar.zsh | " ++ myDzenMPDBar
    xmonad $ defaultConfig {

        -- Basics
        terminal      = myTerminal,
        workspaces    = myWorkspaces,

        -- Appearance
        borderWidth   = myBorderWidth,

        -- Interaction
        keys          = myKeys,
        modMask       = myModMask,
        mouseBindings = myMouseBindings,

        -- Hooks
        layoutHook    = myLayoutHook,
        logHook       = dzenStatusLogger dzenBar,
        manageHook    = manageDocks
    }
