import XMonad
import Data.Maybe

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar 
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Loggers
import XMonad.Util.Font
import XMonad.Util.SpawnOnce
import XMonad.Layout.Spacing
import XMonad.Util.ClickableWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Layout.LayoutModifier
import Graphics.X11.ExtraTypes.XF86

myWorkspaces = ["1:\xebca ", "2:\xe745 ", "3:\xf0d6e ", "4:\xf15c ", "5:\xe217 "]
myModMask = mod4Mask;
defaults = def {
    modMask = myModMask,
    layoutHook = myLayout,
    manageHook = manageDocks,
    terminal = "alacritty",
    workspaces = myWorkspaces,
    focusedBorderColor = "#222222",
    normalBorderColor = "#333333",
    startupHook = myStartupHook 
}

myManageHook = composeAll 
    [
        [className =? "stalonetray" --> doIgnore]
    ]
myStartupHook = do
    spawnOnce "nitrogen --restore &" 
    spawnOnce "compton &"
    spawnOnce "stalonetray &"
    spawnOnce "nm-applet --sm-disable &"
    spawnOnce "xscreensaver &"       
    spawnOnce "pasystray &"
    spawnOnce "numlockx"


myLayout = avoidStruts $ spacing 10 $  tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta "| "
    , ppHiddenNoWindows = red . pad
    , ppWsSep           = " "
    , ppCurrent         = currentFormatted
    , ppHidden          = white . pad
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, t] -> [ws,t]
    , ppExtras          = [logTitleFormat]
    }
  where
    currentFormatted :: String->String  
    currentFormatted x = wrap "<fc=#000000,#AAAAAA:0>" "</fc>" $ pad x
    
    logTitleFormat :: Logger
    logTitleFormat =  logDefault (xmobarColorL "#333333" "#AAAAAA" $ padL $ shortenL 75 logTitle) $ logConst "\xf05b0"
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh . withEasySB(statusBarProp "~/.cabal/bin/xmobar ~/.config/xmobar/xmobarrc" (clickablePP myXmobarPP)) defToggleStrutsKey $ defaults `additionalKeys`
        [ ((0, xK_Print), spawn "flameshot gui")
        , ((myModMask, xK_f), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled >> sendMessage ToggleStruts)
        , ((0, xF86XK_MonBrightnessUp), spawn "/home/izz/.config/xmonad/brightness.sh 0.1")
        , ((0, xF86XK_MonBrightnessDown), spawn "/home/izz/.config/xmonad/brightness.sh -0.1")
        , ((myModMask .|. shiftMask .|. controlMask, xK_s), spawn "systemctl suspend" )
        , ((myModMask,xK_q), spawn "xmonad--restart")
        ]
    



