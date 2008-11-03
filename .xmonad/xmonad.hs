import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myNormalBorderColor  = "#888888"
myFocusedBorderColor = "#FFFFFF"
myDmenuBGColor = "black"
myDmenuFGColor = "green"
myDmenuAltFGColor = "cyan"
myDmenuSelBGColor = "green"
myDmenuSelFGColor = "black"
myDmenuFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"

myDmenuLaunchString = "exe=`dmenu_path | dmenu -fn " ++ myDmenuFont ++ " -nb " ++ myDmenuBGColor ++ " -nf " ++ myDmenuFGColor ++ " -sb " ++ myDmenuSelBGColor ++ " -sf " ++ myDmenuSelFGColor ++ "` && eval \"exec $exe\""

-- mask to use for switching workspaces with <mask>-[0..9]
workspaceMask = mod4Mask


myManageHook = composeAll [
  className =? "Gimp" --> doFloat,
  className =? "Vncviewer" --> doFloat,
  className =? "KiCad" --> doFloat
  ]


myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- launch a terminal
  [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  -- launch dmenu
  , ((modMask,               xK_p     ), spawn myDmenuLaunchString)
  -- launch gmrun
  , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
  -- close focused window
  , ((modMask .|. shiftMask, xK_c     ), kill)
   -- Rotate through the available layout algorithms
  , ((modMask,               xK_space ), sendMessage NextLayout)
  --  Reset the layouts on the current workspace to default
  , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to the correct size
  , ((modMask,               xK_n     ), refresh)
  -- Move focus to the next window
  , ((modMask,               xK_Tab   ), windows W.focusDown)
  -- Move focus to the next window
  , ((modMask,               xK_j     ), windows W.focusDown)
  -- Move focus to the previous window
  , ((modMask,               xK_k     ), windows W.focusUp  )
  -- Move focus to the master window
  , ((modMask,               xK_m     ), windows W.focusMaster  )
  -- Swap the focused window and the master window
  , ((modMask,               xK_Return), windows W.swapMaster)
  -- Swap the focused window with the next window
  , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
  -- Swap the focused window with the previous window
  , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
  -- Shrink the master area
  , ((modMask,               xK_h     ), sendMessage Shrink)
  -- Expand the master area
  , ((modMask,               xK_l     ), sendMessage Expand)
  -- Push window back into tiling
  , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
  -- Increment the number of windows in the master area
  , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
  -- Deincrement the number of windows in the master area
  , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
  ]
  ++

  --
  -- <mod>-[1..9], Switch to workspace N
  -- <mod>-shift-[1..9], Move client to workspace N
  --
  --
  [((m .|. workspaceMask, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myConfig = defaultConfig {
    manageHook = manageDocks <+> myManageHook <+>
                 manageHook defaultConfig,
    layoutHook = avoidStruts  $  layoutHook defaultConfig,
    modMask = mod1Mask,
    keys = myKeys
  }


main = do xmonad myConfig
