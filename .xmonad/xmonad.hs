import qualified XMonad.StackSet as W
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad

myModKey :: KeyMask
myModKey = mod4Mask  -- super
--myModKey = mod1Mask  -- left alt

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Zenity"                       --> doFloat
    , className =? "Steam"                        --> doShift "8"
    , className =? "Spotify Premium"              --> doShift "7"
    , wmRole    =? "gimp-message-dialog"          --> doFloat
    , wmRole    =? "gimp-toolbox-color-dialog"    --> doFloat
    ]
        where
            wmRole = stringProperty "WM_WINDOW_ROLE"

gaps size = spacingRaw False (Border size 0 size 0) True (Border 0 size 0 size) True
myLayouts = (gaps 10 $ Tall 1 (3/100) (1/2)) ||| (noBorders Full)

xK_AudioLower = 0x1008FF11
xK_AudioMute  = 0x1008FF12
xK_AudioRaise = 0x1008FF13
xK_AudioPlay  = 0x1008FF14
xK_AudioStop  = 0x1008FF15
xK_AudioPrev  = 0x1008FF16
xK_AudioNext  = 0x1008FF17
                    
main = do           
    xmproc <- spawnPipe "xmobar"
    xmonad . ewmh . docks $ def
        { borderWidth        = 3
        , modMask            = myModKey
        , focusedBorderColor = "#fb27ff"
        , terminal           = "kitty"
        --, focusFollowsMouse  = False
        --, clickJustFocuses   = False
        , layoutHook         = avoidStruts $ myLayouts
        , manageHook         = namedScratchpadManageHook scratchpads <+> myManageHook <+> manageHook def
        , handleEventHook    = fullscreenEventHook
        , logHook            = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeys`
        [ ((0,      xK_AudioLower), spawn "volume dec")
        , ((0,      xK_AudioRaise), spawn "volume inc")
        , ((0,      xK_AudioMute),  spawn "volume toggle")
        , ((0,      xK_AudioPlay),  spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ((0,      xK_AudioStop),  spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ((0,      xK_AudioPrev),  spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ((0,      xK_AudioNext),  spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        , ((myModKey, xK_space),    spawn "dmenu_run -b")
        , ((myModKey, xK_f),        sendMessage NextLayout)
        , ((myModKey, xK_b),        sendMessage ToggleStruts)
        , ((0,      xK_F11),        namedScratchpadAction scratchpads "console")
        , ((0,      xK_F12),        namedScratchpadAction scratchpads "music")
        ]

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS
        "console"
        ("kitty" ++ " --class=drop-console")
        (className =? "drop-console")
        (customFloating $ W.RationalRect 0 0 1 (1/3))
    , NS
        "music"
        ("spotify" ++ " --class=drop-console")
        (className =? "drop-console")
        (customFloating $ W.RationalRect 0 0 0.9 0.9)
    ]

-- vim: set sw=4 et sta:
