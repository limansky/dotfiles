import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts, ToggleStruts(..))

import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn, spawnPipe, hPutStrLn)
import XMonad.Util.NamedWindows (getName)

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

import Data.List (sortBy, intercalate)
import Data.Maybe (isJust, catMaybes)
import Data.Ord (comparing)
import Control.Monad (zipWithM_)
import Codec.Binary.UTF8.String (encodeString)

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = do 
    spawn myTrayCommand
    spawn myInfoBar
    dzenl <- spawnPipe $ cmd ++ " -w 1280"
    dzenr <- spawnPipe $ cmd ++ " -x 1280 -w 1232"
    xmonad $ uhook $ myConfig { logHook = myLogHook [ myPP { ppOutput = hPutStrLn dzenl }
                                                    , myPP { ppOutput = hPutStrLn dzenr }
                                                    ] }
      where 
        uhook = withUrgencyHook NoUrgencyHook
--        cmd = "/home/" ++ myUserName ++ "/.xmonad/blinker.pl | dzen2 -ta l -w 1280 " ++ myDzenOptions
        cmd = "dzen2 -ta l " ++ myDzenOptions

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = myWorkspaces
                         , modMask    = myModMask
                         , terminal   = myTerminal
                         , manageHook = myManageHook <+> manageHook defaultConfig <+> manageDocks
                         , layoutHook = avoidStruts myLayout
                         , startupHook = myStartup
                         , normalBorderColor = myNormalBorderColor
                         , focusedBorderColor = myFocusedBorderColor
                         } `removeKeys` myKeysToRemove 
                           `additionalKeys` myKeysToAdd

myUserName = "mlimansk"

-- Bars, etc
myTrayCommand = "killall stalonetray ; stalonetray -i 16 --max-width 48 --icon-gravity E --geometry 48x16-0+0 -bg '" ++ barBgColor ++ "' --sticky --skip-taskbar &"
myInfoBar = "killall conky ; conky --config=/home/" ++ myUserName ++ "/.xmonad/conkyrc | dzen2 -y -1 -ta r -w 1280 " ++ myDzenOptions

-- Colors --
barBgColor = "#111111"
barFgColor = "#AFAF87"
myNormalBorderColor = "#222222"
myFocusedBorderColor = "#666666"

myXPConfig = defaultXPConfig { fgColor = barFgColor
                             , bgColor = barBgColor
                             }

-------------------------------------------------------------------------------
-- Looks --
-- bar

myDzenOptions = "-h 16 -bg '" ++ barBgColor ++ "' -fg '" ++ barFgColor ++ "' -fn '-*-fixed-*-*-*-*-12-*-*-*-*-*-*-u'" ++ " -e ''"

myPP = dzenPP { ppCurrent = dzenColor barFgColor "#4d4d4d" . pad
              , ppHidden = dzenColor barFgColor barBgColor . pad
              , ppVisible = dzenColor barFgColor "#2d2d2d" . pad
              --, ppUrgent = (\s -> "^blink(1)" ++ (dzenColor barFgColor "dark red" $ dzenStrip s) ++ "^blink(0)")
              , ppUrgent = dzenColor barFgColor "dark red" . dzenStrip
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
    where wrapIcon pic = "^i(/home/" ++ myUserName ++ "/.xmonad/icons/" ++ pic ++ ")"

-- workspaces
myWorkspaces = ["1:main", "2:web", "3:work", "4:im", "5:skype", "6:IRC", "7:mail", "8", "9:music" ]

-------------------------------------------------------------------------------
-- Terminal --
myTerminal = "urxvt"

myManageHook = composeAll [
        className =? "stalonetray"      --> doIgnore,
        className =? "Firefox"          --> doShift "2:web",
        className =? "Skype"            --> doShift "5:skype",
        className =? "Qutim"            --> doShift "4:im",
        className =? "Gimp"             --> doFloat,
        className =? "MPlayer"          --> doFloat
    ]

-- Layouting

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

myLayout = onWorkspace "4:im" qutimLayout $ onWorkspace "5:skype" skypeLayout $ defaultLayouts
    where skypeLayout = reflectHoriz $ withIM (1/6) skypeRoster Grid
          skypeRoster = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
          qutimLayout = reflectHoriz $ withIM (1/7) qutimRoster Grid
          qutimRoster = (ClassName "Pidgin") `And` (Role "buddy_list")
          --qutimRoster = Role "contactlist"

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

    , ((myModMask,               xK_f     ), sendMessage ToggleStruts)
    -- MPD keys
    , ((myModMask,               xK_c     ), spawn "ncmpcpp toggle")
    , ((myModMask,               xK_b     ), spawn "ncmpcpp next")
    , ((myModMask,               xK_z     ), spawn "ncmpcpp prev")
    , ((myModMask,               xK_r     ), shellPrompt myXPConfig)
    , ((myModMask,               xK_u     ), focusUrgent)
    , ((controlMask .|. altMask, xK_l     ), safeSpawn "xautolock" ["-locknow"])
    ]
        where altMask = mod1Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_f)

myStartup = spawn ("feh --bg-tile /home/" ++ myUserName ++ "/.xmonad/theme.jpg")
            >> spawn "xautolock -time 10"
            >> spawn "tinymount --iconTheme=Tango"

myLogHook pps = do
  screens <- (sortBy (comparing W.screen) . W.screens) `fmap` gets windowset
  zipWithM_ dynamicLogWithPP' screens pps

-- Extract the focused window from the stack of windows on the given screen.
-- Return Just that window, or Nothing for an empty stack.
focusedWindow = maybe Nothing (return . W.focus) . W.stack . W.workspace

-- The functions dynamicLogWithPP', dynamicLogString', and pprWindowSet' below
-- are similar to their undashed versions, with the difference being that the
-- latter operate on the current screen, whereas the former take the screen to
-- operate on as the first argument.

dynamicLogWithPP' screen pp = dynamicLogString' screen pp >>= io . ppOutput pp

dynamicLogString' screen pp = do

  winset <- gets windowset
  urgents <- readUrgents
  sort' <- ppSort pp

  -- layout description
  let ld = description . W.layout . W.workspace $ screen

  -- workspace list
  let ws = pprWindowSet' screen sort' urgents pp winset

  -- window title
  wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

  -- run extra loggers, ignoring any that generate errorW.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
             [ ws
             , ppLayout pp ld
             , ppTitle pp wt
             ]
             ++ catMaybes extras


pprWindowSet' screen sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $ W.workspaces s
    where this = W.tag . W.workspace $ screen
          visibles = map (W.tag . W.workspace) (W.current s : W.visible s)

          fmt w = printer pp (W.tag w)
              where printer | W.tag w == this = ppCurrent
                            | W.tag w `elem` visibles = ppVisible
                            | any (\x -> maybe False (== W.tag w) (W.findTag x s)) urgents = \ppC -> ppUrgent ppC . ppHidden ppC
                            | isJust (W.stack w) = ppHidden
                            | otherwise = ppHiddenNoWindows


sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)
