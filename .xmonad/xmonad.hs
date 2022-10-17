import XMonad
import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.EZConfig (additionalKeys)
import Data.List (isInfixOf, isPrefixOf)
import qualified XMonad.StackSet as W

{-
main = xmonad def
  { terminal = "xterm"
  , modMask = mod4Mask -- Rebind Mod to the Windows key
  }
-}

myModMask            = mod4Mask

myFocusedBorderColor = "#ffffff"
myNormalBorderColor  = "#cccccc"

myKeys = [ ((myModMask, xK_f), spawn "firefox")
         , ((myModMask, xK_e), spawn "emacs")
         , ((myModMask, xK_g), spawn "ghostwriter")
         , ((myModMask .|. controlMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
         , ((myModMask, xK_F6), spawn "amixer -q sset Master 5%-")
         , ((myModMask, xK_F7), spawn "amixer -q sset Master 5%+")
         ]

manageZoomHook =
  composeAll $
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

myStartupHook = do
  startupHook defaultConfig
  spawn "xrandr --output HDMI-0 --auto --set audio off &"
  --spawn "xcompmgr -cfF -t-9 -l-11 -r9 -o.95 -D4 &"
  spawn "stalonetray &"


myConfig = def
  { terminal = "xterm"
  , modMask = myModMask
  , layoutHook = smartBorders $ layoutHook defaultConfig
  , manageHook = (isFullscreen --> doFullFloat) <+> (fmap ("mpv" `isPrefixOf`) title --> doFullFloat) <+> (className =? "sdl" --> doFullFloat) <+> manageZoomHook <+> manageDocks <+> manageHook defaultConfig
  , focusedBorderColor = myFocusedBorderColor
  , normalBorderColor  = myNormalBorderColor
  , startupHook = myStartupHook
  } `additionalKeys` myKeys

main = xmonad =<< xmobar myConfig
--main = xmonad . ewmh =<< xmobar myConfig
{-

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ defaultConfig
        { startupHook = setWMName "LG3D"
        , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
        
-}
