import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import System.Exit
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = do
    spawn myTrayCommand
    spawn myInfoBar
    xmonad =<< statusBar cmd pp kb conf
      where
        uhook = withUrgencyHook NoUrgencyHook
        cmd = xmonadHome ++ "blinker.pl | dzen2 -ta l -w 1130 " ++ myDzenOptions
        -- cmd = "polybar"
        -- cmd = "dzen2 -ta l -w 1280 " ++ myDzenOptions
        pp = myPP
        kb = toggleStrutsKey
        -- conf = ewmhFullscreen . ewmh $ myConfig
        conf = ewmhFullscreen . ewmh $ uhook myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = def { workspaces = myWorkspaces
               , modMask    = myModMask
               , terminal   = myTerminal
               , logHook    = myLogHook
               , manageHook = myManageHook <+> manageHook def
               , layoutHook = myLayout
               , startupHook = myStartup >> setWMName "LG3D"
               , normalBorderColor = myNormalBorderColor
               , focusedBorderColor = myFocusedBorderColor
               } `removeKeys` myKeysToRemove
                 `additionalKeys` myKeysToAdd

myUserName = "limansky"
myUserHome = "/home/" ++ myUserName ++ "/"
xmonadHome = myUserHome ++ ".xmonad/"

-- Bars, etc
myTrayCommand = "killall stalonetray ; stalonetray -i 24 --icon-gravity SE --geometry 5x1-0+0 -bg '" ++ barBgColor ++ "' --sticky --skip-taskbar &"
myInfoBar = "killall conky ; conky --config=" ++ xmonadHome ++ "conkyrc | dzen2 -x -790 -w 670 -ta r " ++ myDzenOptions

-- Colors --
barBgColor = "#111111"
barFgColor = "#AFAF87"
myNormalBorderColor = "#222222"
myFocusedBorderColor = "#666666"

myXPConfig = def { font = "xft:Monospace:size=8"
                 , fgColor = barFgColor
                 , bgColor = barBgColor
                 , height  = 32
                 }

-------------------------------------------------------------------------------
-- Looks --
-- bar

myDzenOptions = "-h 24 -bg '" ++ barBgColor ++ "' -fg '" ++ barFgColor ++ "' -fn '-*-fixed-*-*-*-*-14-*-*-*-*-*-*-u'" ++ " -e ''"

myPP = dzenPP { ppCurrent = dzenColor barFgColor "#4d4d4d" . pad
              , ppHidden = dzenColor barFgColor barBgColor . pad
              , ppUrgent = (\s -> "^blink(1)" ++ (dzenColor barFgColor "dark red" $ dzenStrip s) ++ "^blink(0)")
              --, ppUrgent = dzenColor barFgColor "dark red" . dzenStrip
              , ppHiddenNoWindows = dzenColor "#333333" "" . pad
              , ppTitle = dzenColor "#87af87" "" . shorten 200
              , ppSep = " "
--              , ppWsSep = " "
              , ppLayout = dzenColor barFgColor "" .
                    (\x -> case x of
                        "Tall"         -> wrapIcon "tall.xbm"
                        "Mirror Tall"  -> wrapIcon "tallmirr.xbm"
                        "Full"         -> wrapIcon "full.xbm"
                        "ReflectX IM Grid" -> ""
                        _ -> x
                    )
              }
    where wrapIcon pic = "^i(" ++ xmonadHome ++ "icons/" ++ pic ++ ")"

-- workspaces
myWorkspaces = ["1:main", "2:web", "3:work", "4:im", "5:skype", "6:IRC", "7:mail", "8", "9:music" ]

-------------------------------------------------------------------------------
-- Terminal --
myTerminal = "kitty"

myLogHook = (dynamicLogWithPP $ myPP) >> fadeInactiveLogHook 0.8

myManageHook = composeAll [
        className =? "stalonetray"      --> doIgnore,
        className =? "Firefox"          --> doShift "2:web",
        className =? "jetbrains-idea-ce"--> doShift "3:work",
        className =? "TelegramDesktop"         --> doShift "4:im",
        className =? "Skype"            --> doShift "5:skype",
        className =? "Slack"            --> doShift "6:IRC",
        className =? "Gimp"             --> doFloat,
        className =? "MPlayer"          --> doFloat,
        isFullscreen                    --> doFullFloat
    ]

-- Layouting

--myLayout = ewmhDesktopsLayout . dwmStyle shrinkText myTheme . windowNavigation . avoidStruts . maximize . mkToggle (single ACCORDION) . mkToggle (NOBORDERS ?? FULL ?? EOT) $ onWorkspace "IM" tiledIM (Mirror tiled) ||| onWorkspace "IM" (Mirror tiled) tiledIM

defaultLayouts = smartBorders tiled ||| smartBorders ( Mirror tiled ) ||| noBorders Full
  where
       -- default tiling algorithm partitions the screen into two panes
       tiled   = Tall nmaster delta ratio

       -- The default number of windows in the master pane
       nmaster = 1

       -- Default proportion of screen occupied by master pane
       ratio   = 1/2

       -- Percent of screen to increment by when resizing panes
       delta   = 3/100

myLayout = onWorkspace "4:im" fsLayout $ onWorkspace "5:skype" fsLayout $ defaultLayouts
    where fsLayout     = noBorders Full

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.

myModMask = mod4Mask

myKeysToRemove = [ (myModMask,               xK_p     )
                 ]

myKeysToAdd =
    [ ((myModMask,               xK_Return), spawn $ XMonad.terminal myConfig)
    , ((myModMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((myModMask,               xK_Left  ), prevWS)
    , ((myModMask,               xK_Right ), nextWS)

    --, ((myModMask,               xK_r     ), spawn $ "exe=`dmenu_path | dmenu -nb '" ++ barBgColor ++ "' -nf '" ++ barFgColor ++ "'` && eval \"exec $exe\"")
--    , ((myModMask,               xK_f     ), sendMessage ToggleStruts)
    -- MPD keys
    , ((myModMask,               xK_c     ), spawn "ncmpcpp toggle")
    , ((0,         xF86XK_AudioMute       ), spawn "pactl set-sink-mute 0 toggle")
    , ((0,         xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
    , ((0,         xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
    , ((myModMask,               xK_r     ), shellPrompt myXPConfig)
    , ((myModMask,               xK_u     ), focusUrgent)
    , ((controlMask .|. altMask, xK_l     ), safeSpawn "xautolock" ["-locknow"])
    , ((0,                       xK_Print ), spawn "scrot -e 'xclip -selection clipboard -t image/png -i $f && rm $f")
    ]
        where altMask = mod1Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_f)

myStartup = io setWallpaper >>
            spawn ("compton &") >>
            spawn "xautolock -time 10" >>
            spawn "tinymount --iconTheme=Tango"

setWallpaper = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (_, month, _) = toGregorian $ localDay zoneNow
    let filename = (showFixed month) ++ ".jpg"
    spawn ("feh --bg-scale " ++ xmonadHome ++ "wallpaper/" ++ filename)

showFixed n = if n < 10 then "0" ++ show n else show n
