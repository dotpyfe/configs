import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Prompt
import Control.Monad (liftM2)

import qualified XMonad.StackSet as W

main = do
    dzen <- spawnPipe myStatusBar
    dzenTime <- spawnPipe myTimeBar
--    conkytop <- spawnPipe myTopBar
--    conkympd <- spawnPipe myMpdBar
--    conkyhdd <- spawnPipe myHddBar
    xmonad $ myUrgencyHook $ defaultConfig 
       { terminal = myTerm
       , workspaces = myWorkspaces
       , modMask = mod4Mask
       , manageHook = myManageHook <+> manageDocks
       , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
       , logHook = dynamicLogWithPP $ myDzenPP dzen
       } `additionalKeysP`
            [ ("<XF86AudioMute>", spawn "amixer -c 1 -q set 'Speaker' toggle")
            , ("<XF86AudioLowerVolume>", spawn "amixer -c 1 -q set 'Speaker' 1-")
            , ("<XF86AudioRaiseVolume>", spawn "amixer -c 1 -q set 'Speaker' 1+")
            , ("<XF86Sleep>", spawn "xscreensaver-command --lock")
            , ("M-p", spawn "dmenu_run")
            ]

       
myTerm = "urxvt"
myDzenFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
myWorkspaces = [ "1:dev", "2:www", "3:music", "4:irc", "5:misc", "6:", "7:", "8:", "9:" ]
myStatusBar = "dzen2 -x '0' -y '0' -h '20' -w '1296' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myDzenFont ++ "'"
myTimeBar = "/home/maciek/dzen.sh | dzen2 -x '0' -y '882' -h '18' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myDzenFont ++ "'"
--myTopBar = "conky -c ~/.conkytop | dzen2 -x '1000' -y '0' -h '20' -w '680' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myDzenFont ++ "'"
--myMpdBar = "conky -c ~/.conkympd | dzen2 -x '0' -y '1030' -h '20' -w '1330' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myDzenFont ++ "'"
--myHddBar = "conky -c ~/.conkyhdd | dzen2 -x '1330' -y '1030' -h '20' -w '350' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myDzenBGColor ++ "' -fn '" ++ myDzenFont ++ "'"

myIconDir = ""
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
mySeperatorColor = "#555555"

myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1030", "-h", "20", "-w", "1650", "-ta", "r", "-expand", "l", "-fg", myUrgentFGColor, "-bg", myUrgentBGColor, "-fn", myDzenFont ] }

myManageHook = composeAll . concat $
    [ [isDialog --> doFloat]
    , [isFullscreen --> doFullFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "1:dev" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "2:www" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "3:music" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "4:irc" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "5:misc" | x <- my5Shifts]
--    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "6:irc" | x <- my6Shifts]
--    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "7:irc" | x <- my7Shifts]
--    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "8:irc" | x <- my8Shifts]
--    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "9:irc" | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = [ "MPlayer" ]
    myTFloats = [ "Save as...", "Hulu Desktop" ]
    myRFloats = []
    myIgnores = []
    my1Shifts = [ "GVim" ]
    my2Shifts = ["Google-Chrome"]
    my3Shifts = ["Gmpc"]
    my4Shifts = []
    my5Shifts = []

myDzenPP h = defaultPP
 { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ ")^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" -- . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ ")^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" -- . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ ")") "^fg()^bg()^p()" -- . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ ")") "^fg()^bg()^p()" wsId -- . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myUrgentBGColor ++ ")^p()^i(" ++ myIconDir ++ ")^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" -- . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:dev", "2:www", "3:music", "4:irc", "5:misc"]
