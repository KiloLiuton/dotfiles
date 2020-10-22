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

modKey :: KeyMask
modKey = mod4Mask  -- super
--modKey = mod1Mask  -- left alt

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

xK_AudioLower     = 0x1008FF11
xK_AudioMute      = 0x1008FF12
xK_AudioRaise     = 0x1008FF13

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad . docks $ def
        { borderWidth        = 3
        , modMask            = modKey
        , focusedBorderColor = "#fb27ff"
        , terminal           = "kitty"
        --, focusFollowsMouse  = False
        --, clickJustFocuses   = False
        , layoutHook         = avoidStruts $ myLayouts
        , manageHook         = myManageHook <+> manageHook def
        , handleEventHook    = fullscreenEventHook
        , logHook            = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        } `additionalKeys`
        [ ((0, xK_AudioLower),     spawn "volume dec")
        , ((0, xK_AudioRaise),     spawn "volume inc")
        , ((0, xK_AudioMute),      spawn "volume toggle")
        , ((modKey, xK_space),     spawn "dmenu_run -b")
        , ((modKey, xK_f),         sendMessage NextLayout)
        , ((modKey, xK_b),         sendMessage ToggleStruts)
        ]

-- vim: set sw=4 et sta:
