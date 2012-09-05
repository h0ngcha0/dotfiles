import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import XMonad.Layout.NoBorders
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ThreeColumns
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace

myNormalBorderColor  = "#000000" 
myFocusedBorderColor = "#93c91d"

myManageHook :: [ManageHook]
myManageHook = 
    [ resource  =? "Do"   --> doIgnore
    , className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , isFullscreen --> doFullFloat
    , manageDocks
    ]

myLayout = avoidStruts $ windowNavigation $ subTabbed (tiled ||| threeCol ||| Full)
    where
      tiled = ResizableTall nmaster delta ratio []
      threeCol = ThreeCol nmaster delta ratio
      nmaster = 1
      ratio = 50/100
      delta = 3/100
curLayout :: X String
curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

main = xmonad $	gnomeConfig 
     { terminal		    = "gnome-terminal"
     , normalBorderColor    = myNormalBorderColor
     , focusedBorderColor   = myFocusedBorderColor
     , borderWidth 	    = 2
     , modMask		    = mod4Mask	
     , manageHook	    = manageHook defaultConfig <+> composeAll myManageHook	
     , layoutHook           = smartBorders (myLayout)
     , focusFollowsMouse    = True
     } `additionalKeysP` myKeys
     
myKeys =
  [ ("M-S-z", spawn "gnome-screensaver-command --lock") 
  , ("M-C-h", sendMessage $ pullGroup L)
  , ("M-C-l", sendMessage $ pullGroup R)
  , ("M-C-k", sendMessage $ pullGroup U)
  , ("M-C-j", sendMessage $ pullGroup D)
  , ("M-C-m", withFocused (sendMessage . MergeAll))
  , ("M-C-u", withFocused (sendMessage . UnMerge))
    -- Cycle WS
  , ("M-<Right>",     nextWS)
  , ("M-<Left>",      prevWS)
  , ("M-S-<Right>",   shiftToNext >> nextWS)
  , ("M-S-<Left>",    shiftToPrev >> prevWS)
  , ("M-z",           toggleWS)
  , ("M-g",           moveTo Next HiddenNonEmptyWS)
  , ("M-n",           moveTo Next EmptyWS)
  , ("M-S-n",         tagToEmptyWorkspace)
  , ("M-c",           kill) -- Close the focused window
  , ("M-<Down>",      windows W.focusDown)
  , ("M-<Up>",        windows W.focusUp)
  , ("M-S-<Down>",    windows W.swapDown)
  , ("M-S-<Up>",      windows W.swapUp)
  ]
  ++
  [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
       | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
       , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
  ]     
     