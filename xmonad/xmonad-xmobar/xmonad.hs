import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
  [ className =? "VLC" --> doFloat
  ]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/maciek/.xmobarrc"
  xmonad $ defaultConfig
    { manageHook  = manageDocks <+> myManageHook
                      <+> manageHook defaultConfig
    , layoutHook  = avoidStruts  $  layoutHook defaultConfig
    , logHook     = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle  = xmobarColor "green" "" . shorten 50
                        }
    , terminal    = myTerminal
    , modMask     = myModMask
    , borderWidth = myBorderWidth
    } `additionalKeys`
    [ ((mod4Mask,                 xK_p), spawn "dmenu_run")
    , ((controlMask .|. mod4Mask, xK_Delete), spawn "xscreensaver-command --lock")
    , ((controlMask,              xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0,                        xK_Print), spawn "scrot")
    ]

myTerminal    = "urxvt"
myModMask     = mod4Mask
myBorderWidth = 3
