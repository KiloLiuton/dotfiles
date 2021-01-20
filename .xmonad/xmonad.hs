--import XMonad.Hooks.EwmhDesktops
import System.IO
import XMonad
import XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste
import XMonad.Util.Run (spawnPipe)

modKey :: KeyMask
--modKey = mod4Mask  -- super
modKey = mod1Mask  -- left alt

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Zenity"                       --> doFloat
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
    xmonad . docks $ def
        { borderWidth        = 2
        , modMask            = modKey
        , focusedBorderColor = "#22D05F"
        , terminal           = "kitty"
        --, focusFollowsMouse  = False
        --, clickJustFocuses   = False
        , layoutHook         = avoidStruts $ myLayouts
        , manageHook         = myManageHook <+> manageHook def
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
        , ((modKey,               xK_f),              sendMessage NextLayout)
        , ((modKey,               xK_b),              sendMessage ToggleStruts)
        , ((modKey,               xK_Tab),            spawn "rofi -show window")
        , ((modKey,               xK_space),          spawn "rofi -show drun")
        , ((modKey .|. shiftMask, xK_space),          spawn "rofi -show run")
        , ((0,                    xK_F11),            namedScratchpadAction scratchpads "console")
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
