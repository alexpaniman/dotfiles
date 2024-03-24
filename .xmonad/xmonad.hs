import XMonad

import XMonad.Config.Desktop
import System.Exit (exitSuccess)

import XMonad.Layout.Spacing (spacing)

import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig

import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.EqualSpacing
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.MultiToggle.Instances

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.NoBorders (toggleBorder)

import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.CycleWS
import Data.Map
  

myTerminal = "alacritty"
myMusicPlayer = "cmus"
myTop = "gotop"
myBrowser = "firefox"
myFileManager = "/home/alex/.config/vifm/scripts/vifmrun"
myEditor = "emacsclient -nc -a ''"

myStartupHook = do
    setWMName "LG3D"

myLayoutHook = toggleLayouts (noBorders Full) $ mkToggle (single MIRROR) $ smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ equalSpacing 24 6 0 6 $
    ResizableTall 1 (3/100) (1/2) [] ||| ThreeColMid 1 (3/100) (1/2)

myManageHook = namedScratchpadManageHook myScratchpads

myScratchpads = [
      NS "terminal" spawnTerminal findTerminal manageScratchpad
    , NS "music-player" spawnMusicPlayer findMusicPlayer manageScratchpad
    ]
    where
        spawnTerminal = myTerminal ++ " --title terminal-scratchpad --class scratchpad"
        findTerminal = resource =? "scratchpad"

        spawnMusicPlayer = myTerminal ++ " --title music-player-scratchpad --class music-player -e " ++ myMusicPlayer
        findMusicPlayer = resource =? "music-player"

        manageScratchpad = customFloating $ W.RationalRect 0.05 0.05 0.9 0.9


myWorkspaces = [show i | i <- [0..9]]
  

main = xmonad $ ewmh desktopConfig
    { borderWidth = 2
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , modMask = mod4Mask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , focusFollowsMouse = False
    , normalBorderColor = "#18081A"
    , focusedBorderColor = "#fa0574" -- "#6f00cc"
    } `additionalKeysP` [

    -- XMonad
      ("M-S-r"            , spawn "xmonad --restart")
    , ("M-S-q"            , io exitSuccess)
    , ("M-S-C-q"          , io exitSuccess                                           )

    -- Terminal
    , ("M-<Return>"       , spawn $ myTerminal                                       )

    -- Apps
    , ("M-i"              , spawn $ myBrowser                                        )
    , ("M-a"              , spawn $ myEditor                                         )

    , ("M-S-s"            , shiftNextScreen >> nextScreen                            )
    , ("M-s"              , nextScreen                                               )


    -- Debug
    , ("M-; e"            , spawn "ps aux | grep -ie emacs | grep -v grep | awk '{print $2}' | xargs kill -SIGUSR2")
    , ("M-; t"            , spawn $ myTerminal ++ " -e " ++ myTop                    )
    , ("M-; f"            , spawn $ myTerminal ++ " -e " ++ myFileManager            )

    -- Fullscreen
    , ("M-f"              , (sendMessage (TL.Toggle "Full"))                         )

    -- Mirror
    , ("M-m"              , sendMessage $ MT.Toggle MIRROR                           )

    -- DMenu
    , ("M-o"              , spawn $ "rofi -show run"                                 )

    -- Volume
    , ("M-<Up>"           , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"         )
    , ("M-<Down>"         , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"         )

    -- Scratchpads
    , ("M-C-<Return>"     , namedScratchpadAction myScratchpads "terminal"           )
    , ("M-C-m"            , namedScratchpadAction myScratchpads "music-player"       )

    -- Close Windows
    , ("M-d"              , kill1                                                    )
    , ("M-S-d"            , killAll                                                  )

    -- Screenshot
    , ("S-<Print>"        , spawn "flameshot gui"                                    )
    , ("C-<Print>"        , spawn "maim -s | xclip -selection clipboard -t image/png")
    , ("<Print>"          , spawn "maim | xclip -selection clipboard -t image/png"   )

    -- Scripts
    , ("M-'"              , sendMessage (TL.Toggle "Full")                           )
    ]
    ++
    [ ("M-1"              , windows $ lazyView $ myWorkspaces !! 0                   )
    , ("M-2"              , windows $ lazyView $ myWorkspaces !! 1                   )
    , ("M-3"              , windows $ lazyView $ myWorkspaces !! 2                   )
    , ("M-4"              , windows $ lazyView $ myWorkspaces !! 3                   )
    , ("M-5"              , windows $ lazyView $ myWorkspaces !! 4                   )
    , ("M-6"              , windows $ lazyView $ myWorkspaces !! 5                   )
    , ("M-7"              , windows $ lazyView $ myWorkspaces !! 6                   )
    , ("M-8"              , windows $ lazyView $ myWorkspaces !! 7                   )
    , ("M-9"              , windows $ lazyView $ myWorkspaces !! 8                   )
    ]
    ++
    [ ("M-S-1"            , windows $ W.shift  $ myWorkspaces !! 0                   )
    , ("M-S-2"            , windows $ W.shift  $ myWorkspaces !! 1                   )
    , ("M-S-3"            , windows $ W.shift  $ myWorkspaces !! 2                   )
    , ("M-S-4"            , windows $ W.shift  $ myWorkspaces !! 3                   )
    , ("M-S-5"            , windows $ W.shift  $ myWorkspaces !! 4                   )
    , ("M-S-6"            , windows $ W.shift  $ myWorkspaces !! 5                   )
    , ("M-S-7"            , windows $ W.shift  $ myWorkspaces !! 6                   )
    , ("M-S-8"            , windows $ W.shift  $ myWorkspaces !! 7                   )
    , ("M-S-9"            , windows $ W.shift  $ myWorkspaces !! 8                   )
    ]



isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)
lazyView w ws = if isVisible w ws then ws else W.view w ws
