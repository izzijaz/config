import XMonad
import Data.Maybe

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Loggers
import XMonad.Util.Font
import XMonad.Util.SpawnOnce
import XMonad.Layout.Spacing
import XMonad.Actions.SpawnOn
import XMonad.Layout.LayoutModifier
import Graphics.X11.ExtraTypes.XF86

myWorkspaces = ["Term", "wWw", "Code", "TxT"]
myModMask = mod4Mask;
defaults = def {
    modMask = myModMask,
    layoutHook = myLayout,
    terminal = "alacritty",
    handleEventHook = fullscreenEventHook,
    workspaces = myWorkspaces,
    manageHook = myManageHook,
    focusedBorderColor = "#222222",
    normalBorderColor = "#333333",
    startupHook = myStartupHook 
}

myStartupHook = do
    spawnOnce "nitrogen --restore &" 
    spawnOnce "compton &"
    spawnOnce "stalonetray &"
    spawnOnce "nm-applet --sm-disable &"
    spawnOnce "xscreensaver &"       
    spawnOnce "pasystray &"
    spawnOnce "numlockx"

myManageHook = composeAll [
    className =? "Code" --> (doShift "Code")
    ,manageSpawn
    ]


myLayout = spacing 10 $ avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta "|"
    , ppHiddenNoWindows = red . pad
    , ppCurrent         = currentFormatted
    , ppHidden          = white . pad
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, t] -> [ws,t]
    , ppExtras          = [xmobarColorL "#bd93f9" "#111111" $ padL $ shortenL 55 logTitle]
    }
  where
    currentFormatted :: String->String  
    currentFormatted x =  wrap "<box>" "</box>" $ xmobarColor "#bd93f9" "#333355" $ pad x

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

main :: IO ()
main = xmonad . docks . ewmh =<< (statusBar "xmobar ~/.config/xmobar/xmobarrc" myXmobarPP toggleStrutsKey) (defaults `additionalKeys`
        [ ((myModMask .|. shiftMask, xK_z), spawnOn "VSCode" "code" >> spawnOn "VSCode" "firefox")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "flameshot gui")
        , ((myModMask, xK_f), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled >> sendMessage ToggleStruts)
        , ((0, xF86XK_MonBrightnessUp), spawn "/home/izz/.xmonad/brightness.sh 0.1")
        , ((0, xF86XK_MonBrightnessDown), spawn "/home/izz/.xmonad/brightness.sh -0.1")
        , ((myModMask .|. shiftMask .|. controlMask, xK_s), spawn "systemctl suspend" )
        ])
    where
        toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)
    



