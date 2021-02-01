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

modKey :: KeyMask
modKey = mod4Mask  -- super
--modKey = mod1Mask  -- left alt

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Zenity"                       --> doFloat
    , className =? "Slack"                        --> doShift "9"
    , className =? "Signal"                       --> doShift "9"
    , wmRole    =? "gimp-message-dialog"          --> doFloat
    , wmRole    =? "gimp-toolbox-color-dialog"    --> doFloat
    ]
        where
            wmRole = stringProperty "WM_WINDOW_ROLE"

gaps size = spacingRaw False (Border size 0 size 0) True (Border 0 size 0 size) True
myLayouts = (gaps 10 $ Tall 1 (3/100) (1/2)) ||| (noBorders Full)

xK_AudioLower     = 0x1008FF11
xK_AudioMute      = 0x1008FF12
xK_AudioRaise     = 0x1008FF13
xK_BrightnessUp   = 0x1008FF02
xK_BrightnessDown = 0x1008FF03
xK_KbdLightOn     = 0x1008FF04
xK_KbdLightOff    = 0x1008FF05

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad . ewmh . docks $ def
        { borderWidth        = 3
        , modMask            = modKey
        , focusedBorderColor = "#ff0000"
        , terminal           = "kitty"
        , focusFollowsMouse  = False
        --, clickJustFocuses   = False
        , layoutHook         = avoidStruts $ myLayouts
        , manageHook         = namedScratchpadManageHook scratchpads <+> myManageHook <+> manageHook def
        , logHook            = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        }
        `additionalKeys`
        [ ((0,                    xK_AudioLower),     spawn "volume dec")
        , ((0,                    xK_AudioRaise),     spawn "volume inc")
        , ((0,                    xK_AudioMute),      spawn "volume toggle")
        , ((0,                    xK_BrightnessUp),   spawn "brightness inc")
        , ((0,                    xK_BrightnessDown), spawn "brightness dec")
        , ((modKey .|. shiftMask, xK_k),              spawn "brightness inc")
        , ((modKey .|. shiftMask, xK_j),              spawn "brightness dec")
        , ((modKey,               xK_f),              sendMessage NextLayout)
        , ((modKey,               xK_b),              sendMessage ToggleStruts)
        , ((modKey .|. shiftMask, xK_p),              spawn "screenshot")
        , ((modKey,               xK_Tab),            spawn "rofi -show window")
        , ((modKey,               xK_space),          spawn "rofi -show drun")
        , ((modKey .|. shiftMask, xK_space),          spawn "rofi -show run")
        , ((modKey,               xK_Escape),         spawn "kblayouts")
        , ((0,                    xK_F11),            namedScratchpadAction scratchpads "console")
        , ((modKey .|. shiftMask, xK_F12),            spawn "nitrogen --set-auto --random $HOME/Pictures/wallpapers")
        ]

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS
        "console"
        ("kitty" ++ " --class=drop-console")
        (className =? "drop-console")
        (customFloating $ W.RationalRect 0 0.015 1 (1/2))
    ]

-- vim: set sw=4 et sta:
