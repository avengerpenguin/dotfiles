import           XMonad
import           XMonad.Hooks.DynamicLog
--import XMonad.Hooks.EwmhDesktops
import           Data.List                    (isInfixOf, isPrefixOf)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders      (smartBorders)
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig         (additionalKeys)


myModMask            = mod4Mask

myFocusedBorderColor = "#ffffff"
myNormalBorderColor  = "#cccccc"

myKeys = [ ((myModMask, xK_f), spawn "firefox")
         , ((myModMask, xK_e), spawn "emacs")
         , ((myModMask, xK_g), gotoMenuArgs ["-i"])
         , ((myModMask, xK_r), bringMenuArgs ["-i"])
         , ((myModMask, xK_F6), spawn "amixer -q sset Master 5%-")
         , ((myModMask, xK_F7), spawn "amixer -q sset Master 5%+")
         , ((myModMask, xK_o), swapNextScreen)
         , ((myModMask .|. shiftMask, xK_o), shiftNextScreen)
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
  spawn "xrandr --output HDMI-0 --auto --set audio on &"
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
