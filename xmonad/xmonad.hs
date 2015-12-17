-- XMonad Tiling Window Manager Configuration.
-- P.C. Shyamshankar <sykora@lucentbeing.com>

{-# LANGUAGE NoMonomorphismRestriction #-}

import System.IO
import System.Exit

import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Util.Run

import qualified XMonad.StackSet as W

-- Appearance
-------------

-- Window Appearance
myBorderWidth :: Integral a => a
myBorderWidth = 0 -- No borders around windows, I think I can manage.

-- XMonad.Prompt Appearance
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig {
    font = "xft:Pragmata Pro:pixelsize=14",
    fgHLight = "#FFCC00",
    bgHLight = "#000000",
    bgColor = "#000000",
    borderColor = "#222222",
    height = 24,

    historyFilter = deleteConsecutive
}

-- XMonad.GridSelect Configuration.
myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig {
        gs_font = "xft:Pragmata Pro:pixelsize=14",
        gs_cellheight = 40,
        gs_cellwidth = 512,
        gs_navigate = myGSNavigation
    }
  where
    myGSNavigation :: TwoD a (Maybe a)
    myGSNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
    navKeyMap = M.fromList [
            ((0, xK_Escape), cancel),
            ((0, xK_q),      cancel),
            ((0, xK_Return), select),
            ((0, xK_slash),  substringSearch myGSNavigation),
            ((0, xK_space),  setPos (0, 0) >> myGSNavigation),

            ((0, xK_n), move (-1, 0) >> myGSNavigation),
            ((0, xK_e), move (0, 1)  >> myGSNavigation),
            ((0, xK_i), move (0, -1) >> myGSNavigation),
            ((0, xK_o), move (1, 0)  >> myGSNavigation)
        ]
    navDefaultHandler = const myGSNavigation

-- XMonad.Layout.Tabbed appearance
myTabTheme :: Theme
myTabTheme = defaultTheme {
    fontName = "xft:Envy Code R:pixelsize=12",
    inactiveColor = "#333333",
    activeColor = "#FFCC00",
    activeTextColor = "#000000",
    inactiveTextColor = "#BDBDBD"

}

-- XMobar pretty printing configuration.
myXMobarLogger :: Handle -> X ()
myXMobarLogger handle = workspaceNamesPP defaultPP {
    ppOutput    = hPutStrLn handle,
    ppCurrent   = \wsID -> "<fc=#FFAF00>[" ++ wsID ++ "]</fc>",
    ppUrgent    = \wsID -> "<fc=#FF0000>" ++ wsID ++ "</fc>",
    ppSep       = " | ",
    ppTitle     = \wTitle -> "<fc=#92FF00>" ++ wTitle ++ "</fc>"
} >>= dynamicLogWithPP

-- Applications
---------------

myTerminal :: String
myTerminal = "urxvtc"

myEditor :: String
myEditor = "emacsclient -c"

-- Workspaces
-------------

myWorkspaces :: [String]
myWorkspaces = map show [1..9] ++ [show 0] ++ map (("F"++) . show) [1..12]

-- Layouts
----------

-- A constructed default tiling layout, 2 panes of windows.
myTiledLayout :: Tall a
myTiledLayout = Tall masterCapacity resizeDelta defaultRatio
    where
        masterCapacity = 1 -- Number of master windows by default.
        resizeDelta    = 1/100 -- Percent to increase the size by each time.
        defaultRatio   = 1/2 -- Default screen ratio of master : others.

-- avoidStruts makes room for the status bars.
myLayoutHook = windowNavigation $ avoidStruts $ maximize $ minimize $ boringWindows $ myTiledLayout ||| Mirror myTiledLayout ||| tabbedLayout ||| Full
    where
        tabbedLayout = named "Tabbed" $ tabbed shrinkText myTabTheme

-- Keys
-------

-- Global Modifier
myModMask = mod4Mask -- The Windows Key, aka "Super"

-- Keymap
myKeys xconfig@(XConfig {XMonad.modMask = m}) = M.fromList $

    [
        -- Applications
        ((m, xK_c), spawn $ XMonad.terminal xconfig), -- Start a new terminal.
        ((m, xK_v), spawn myEditor), -- Start a new terminal.

        -- Window Navigation
        ((m, xK_t), focusDown), -- Focus next window.
        ((m, xK_s), focusUp), -- Focus previous window.

        ((m, xK_Return), windows W.focusMaster), -- Focus master window.

        -- Window Management
        ((m, xK_x), kill), -- Kill the window.

        ((m .|. shiftMask, xK_t), windows W.swapDown), -- Swap with next.
        ((m .|. shiftMask, xK_s), windows W.swapUp), -- Swap with previous.

        ((m .|. shiftMask, xK_Return), windows W.swapMaster), -- Focus master window.

        ((m, xK_j), withFocused $ windows . W.sink), -- Bring floating windows back to tile.
        ((m, xK_backslash), withFocused $ sendMessage . maximizeRestore),
        ((m, xK_minus), withFocused minimizeWindow >> markBoring),
        ((m .|. shiftMask, xK_minus), sendMessage RestoreNextMinimizedWin >> clearBoring),

        -- Layout Management
        ((m, xK_space), sendMessage NextLayout), -- Rotate to next layout.
        ((m .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook xconfig), -- Reset layout.
        ((m, xK_k), sendMessage ToggleStruts), -- Toggle Docks

        ((m, xK_comma), sendMessage (IncMasterN 1)), -- Increment number of master windows.
        ((m, xK_period), sendMessage (IncMasterN (-1))), -- Decrement number of master windows.

        ((m .|. shiftMask, xK_comma), sendMessage Shrink),
        ((m .|. shiftMask, xK_period), sendMessage Expand),

        -- Mouse Management.
        ((m, xK_b), warpToWindow 0.98 0.95), -- Banish mouse to the lower right corner of the screen.

        -- Application Shortcuts
        ((0, xK_Print), spawn "scrot"),
        ((m, xK_m), spawn "urxvtc -e ncmpcpp"),
        ((m, xK_p), shellPrompt myXPConfig),

        -- XMonad Prompts.
        ((m, xK_semicolon), appendFilePrompt myXPConfig "/home/sykora/.notes"),

        -- XMonad Control
        ((m, xK_d), goToSelected myGSConfig),
        ((m, xK_q), restart "xmonad" True), -- Restart XMonad.
        ((m, xK_l), renameWorkspace myXPConfig),
        ((m .|. controlMask, xK_F8), spawn "xautolock -locknow"), -- Start the screen saver, lock the screen.
        ((m .|. controlMask, xK_F12), io exitSuccess) -- Quit XMonad.
    ]
    ++

    -- Map the workspace access keys.
    -- mod + xK_0 .. xK_9 -> Switch to the corresponding workspace (greedyView)
    -- mod + shift + xK_0 .. xK_9 -> Move current window to corresponding workspace.
    [((m .|. shiftMask', numberKey), windows $ windowAction workspace)
        | (workspace, numberKey) <- zip (XMonad.workspaces xconfig) ([xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F12])
        , (shiftMask', windowAction) <- [(0, W.greedyView), (shiftMask, W.shift)]
    ]
    ++

    [((m .|. shiftMask', screenKey), screenWorkspace sc >>= flip whenJust (windows . f))
        | (screenKey, sc) <- zip [xK_n, xK_e] [0..]
        , (f, shiftMask') <- [(W.view, 0), (W.shift, shiftMask)]
    ]

myMouseBindings :: XConfig a -> M.Map (KeyMask, Button) (Window -> X())
myMouseBindings (XConfig {XMonad.modMask = m}) = M.fromList

    [
        ((m, button1), \w -> focus w >> mouseMoveWindow w), -- Float and move while dragging.
        ((m, button2), \w -> focus w >> windows W.swapMaster), -- Raise window to top of stack.
        ((m, button3), \w -> focus w >> mouseResizeWindow w), -- Float and resize while dragging.
        ((m, button4), const prevWS), -- Switch to previous workspace.
        ((m, button5), const nextWS) -- Switch to next workspace.
    ]

-- Managing everything.
myManageHook = manageDocks

-- Run it.
main :: IO ()
main = do
    xmobarPipe <- spawnPipe "xmobar -x 1 ~/etc/xmonad/xmobar.config"
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {

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
        handleEventHook = fullscreenEventHook,
        layoutHook    = myLayoutHook,
        logHook       = myXMobarLogger xmobarPipe >> updatePointer (Relative 0.98 0.95),
        manageHook    = myManageHook
    }
