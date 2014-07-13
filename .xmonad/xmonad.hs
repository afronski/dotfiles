--------------------------------------------------------------------------
-- afronski                   February 2014
-- xmonad config used by Wojciech Gawroński
--------------------------------------------------------------------------

import System.IO
import System.Exit

import Data.List
import Data.Ratio ((%))

import XMonad

import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace(onWorkspace)

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Actions.SpawnOn
import XMonad.Actions.PhysicalScreens

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import qualified XMonad.StackSet as StackSet
import qualified Data.Map        as Map

--------------------------------------------------------------------------
-- Terminal name.
--------------------------------------------------------------------------

terminalName = "urxvtc"

--------------------------------------------------------------------------
-- Windows key as a modifier.
--------------------------------------------------------------------------

modifierName = mod4Mask

--------------------------------------------------------------------------
-- Workspaces
--------------------------------------------------------------------------

workspacesList = [ "Term", "Web", "Code", "VM", "Media", "IM", "GIMP" ] ++ map show [ 8 .. 9 ]

--------------------------------------------------------------------------
-- Management hooks
--------------------------------------------------------------------------

customizedManagementHooks = composeAll [
    className =? "Chromium"                                       --> doShift "Web",

    className =? "sublime-text"                                   --> doShift "Code",
    className =? "emacs"                                          --> doShift "Code",

    className =? "feh"                                            --> doShift "Media",

    className =? "Gimp"                                           --> doShift "GIMP",
    className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role      --> doShift "GIMP",

    className =? "XClock"                                         --> doFloat,

    className =? "Pidgin" <&&> role =? "conversation"             --> doShift "IM",
    className =? "Pidgin" <&&> role =? "buddy_list"               --> doShift "IM",

    className =? "Skype" <&&> role =? "ConversationsWindow"       --> doShift "IM",
    className =? "Skype" <&&> role =? ""                          --> doShift "IM",

    className =? "stalonetray"                                    --> doIgnore,

    isFullscreen                                                  --> (doF StackSet.focusDown <+> doFullFloat)
  ] where
      role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
------------------------------------------------------------------------

xmobarTitleColor = "#FFB6B0"
xmobarCurrentWorkspaceColor = "#CEFFAC"

customizedNormalBorderColor  = "#7c7c7c"
customizedFocusedBorderColor = "#ffb6b0"

customizedTabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
  }

customizedBorderWidth = 1

----------------------------------------------------------------------
-- Key bindings
----------------------------------------------------------------------

customizedKeys conf@(XConfig { XMonad.modMask = modMask }) = Map.fromList $ [
    --------------------------------------------------------------------
    -- Customized xmonad key bindings
    --------------------------------------------------------------------

    -- Start a terminal.  Terminal to start is specified by myTerminal variable.
    ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf),

    -- Lock the screen using slock.
    ((modMask .|. controlMask, xK_l), spawn "slock"),

    -- Launch dmenu via yeganesh.
    -- Use this to launch programs without a key binding.
    ((modMask, xK_p), spawn "customized-yeganesh"),

    -- Mute volume.
    ((modMask .|. controlMask, xK_m), spawn "amixer -q set Master toggle"),

    -- Decrease volume.
    ((modMask .|. controlMask, xK_j), spawn "amixer -q set Master 10%-"),

    -- Increase volume.
    ((modMask .|. controlMask, xK_k), spawn "amixer -q set Master 10%+"),

    --------------------------------------------------------------------
    -- "Standard" xmonad key bindings
    --------------------------------------------------------------------

    -- Close focused window.
    ((modMask .|. shiftMask, xK_c), kill),

    -- Cycle through the available layout algorithms.
    ((modMask, xK_space), sendMessage NextLayout),

    --  Reset the layouts on the current workspace to default.
    ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),

    -- Resize viewed windows to the correct size.
    ((modMask, xK_n), refresh),

    -- Move focus to the next window.
    ((modMask, xK_Tab), windows StackSet.focusDown),

    -- Move focus to the next window.
    ((modMask, xK_j), windows StackSet.focusDown),

    -- Move focus to the previous window.
    ((modMask, xK_k), windows StackSet.focusUp),

    -- Move focus to the master window.
    ((modMask, xK_m), windows StackSet.focusMaster),

    -- Swap the focused window and the master window.
    ((modMask, xK_Return), windows StackSet.swapMaster),

    -- Swap the focused window with the next window.
    ((modMask .|. shiftMask, xK_j), windows StackSet.swapDown),

    -- Swap the focused window with the previous window.
    ((modMask .|. shiftMask, xK_k), windows StackSet.swapUp),

    -- Shrink the master area.
    ((modMask, xK_h), sendMessage Shrink),

    -- Expand the master area.
    ((modMask, xK_l), sendMessage Expand),

    -- Push window back into tiling.
    ((modMask, xK_t), withFocused $ windows . StackSet.sink),

    -- Increment the number of windows in the master area.
    ((modMask, xK_comma), sendMessage (IncMasterN 1)),

    -- Decrement the number of windows in the master area.
    ((modMask, xK_period), sendMessage (IncMasterN (-1))),

    -- Quit xmonad.
    ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess)),

    -- Restart xmonad.
    ((modMask, xK_q), restart "xmonad" True),

    -- Showing windows on screens - previous.
    ((modMask, xK_a), onPrevNeighbour StackSet.view),

    -- Showing windows on screens - next.
    ((modMask, xK_o), onNextNeighbour StackSet.view),

    -- Moving windows around screens - previous.
    ((modMask .|. shiftMask, xK_a), onPrevNeighbour StackSet.shift),

    -- Moving windows around screens - next.
    ((modMask .|. shiftMask, xK_o), onNextNeighbour StackSet.shift)
  ]

  ++

  --------------------------------------------------------------------
  -- Workspaces
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --------------------------------------------------------------------
  [
    ((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [ xK_1 .. xK_9 ]
      , (f, m) <- [ (StackSet.greedyView, 0), (StackSet.shift, shiftMask) ]
  ]

  ++

  --------------------------------------------------------------------
  -- Screens
  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --------------------------------------------------------------------
  [
    ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [ xK_w, xK_e, xK_r ] [ 0 .. ]
      , (f, m) <- [ (StackSet.view, 0), (StackSet.shift, shiftMask) ]
  ]

--------------------------------------------------------------------
-- Layout
--------------------------------------------------------------------
defaultLayouts =  avoidStruts (
                    Tall 1 (3 / 100) (1 / 2) |||
                    Mirror (Tall 1 (3 / 100) (1 / 2)
                  ) |||
                  tabbed shrinkText customizedTabConfig |||
                  Grid |||
                  Full |||
                  spiral (6 / 7)) |||
                  noBorders (fullscreenFull Full)

customizedLayout =  onWorkspace "IM" (avoidStruts $ simpleFloat) $
                    onWorkspace "Media" (avoidStruts $ simpleFloat) $
                    onWorkspace "Term" (avoidStruts $ Grid) $
                    onWorkspace "GIMP" gimpLayout $
                    defaultLayouts
                      where
                        gimpLayout = avoidStruts (
                                      withIM (0.15) (Role "gimp-toolbox") $
                                      reflectHoriz $
                                      withIM (0.2) (Role "gimp-dock") Grid
                                    )

--------------------------------------------------------------------
-- Startup
--------------------------------------------------------------------

startup = do
  spawnOn "Term" terminalName
  spawnOn "Term" terminalName
  spawnOn "Term" terminalName
  spawnOn "Term" terminalName

--------------------------------------------------------------------
-- Log hook.
--------------------------------------------------------------------

customizedLogHook destination = dynamicLogWithPP xmobarPP {
    ppOutput = hPutStrLn destination,
    ppTitle = xmobarColor xmobarTitleColor "" . shorten 100,
    ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "",
    ppSep = "   "
  }

--------------------------------------------------------------------
-- Main
--------------------------------------------------------------------

main = do
  xmproc <- spawnPipe "sleep 3 && xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaults {
    logHook = customizedLogHook xmproc,
    manageHook = manageDocks <+> customizedManagementHooks,
    startupHook = startup <+> setWMName "LG3D"
  }

--------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------

defaults = defaultConfig {
    terminal            = terminalName,
    modMask             = modifierName,

    normalBorderColor   = customizedNormalBorderColor,
    focusedBorderColor  = customizedFocusedBorderColor,
    borderWidth         = customizedBorderWidth,

    workspaces          = workspacesList,
    layoutHook          = smartBorders $ customizedLayout,
    manageHook          = customizedManagementHooks,

    keys                = customizedKeys
  }