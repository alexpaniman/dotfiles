import XMonad

import XMonad.Config.Desktop
import System.Exit (exitSuccess)

import XMonad.Layout.IndependentScreens (withScreens)

import XMonad.Layout.Spacing (spacing)

import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeysP)

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

myTerminal = "alacritty"
myMusicPlayer = "cmus"
myTop = "gotop"
myBrowser = "firefox"
myFileManager = "/home/alex/.config/vifm/scripts/vifmrun"
myEditor = "emacsclient -nc -a ''"

myStartupHook = do
    spawnOnce "/home/alex/.profile &"

    spawnOnce "flameshot &"
    
    spawnOnce "nitrogen --restore &"

    spawnOnce $ "picom" ++
        " --experimental-backends" ++
        " --backend glx" ++
        " --vsync" ++
        " --config ~/.config/picom/picom.conf &"

    spawnOnce "/usr/lib/xfce4/notifyd/xfce4-notifyd &"
    spawnOnce "udiskie &"

    spawnOnce "emacs --daemon &"

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

main = xmonad $ ewmh desktopConfig
    { borderWidth = 2
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , modMask = mod4Mask
    , terminal = myTerminal
    , workspaces = withScreens 2 [show i | i <- [0..9]]
    , focusFollowsMouse = False
    , normalBorderColor = "#18081A"
    , focusedBorderColor = "#fa0574" -- "#6f00cc"
    } `additionalKeysP` [
    -- XMonad
      ("M-S-r", spawn "xmonad --restart")
    , ("M-S-q", io exitSuccess)
    -- Terminal
    , ("M-<Return>", spawn $ myTerminal)
    -- Apps
    , ("M-i", spawn $ myBrowser)
    , ("M-a", spawn $ myEditor)
    , ("M-s", spawn $ "~/.emacs_anywhere/bin/run")
    , ("M-; t", spawn $ myTerminal ++ " -e " ++ myTop)
    , ("M-; f", spawn $ myTerminal ++ " -e " ++ myFileManager)
    -- Debug
    , ("M-; e", spawn "ps aux | grep -ie emacs | grep -v grep | awk '{print $2}' | xargs kill -SIGUSR2")
    -- Fullscreen
    , ("M-f", -- (withFocused toggleBorder) <+> (sendMessage $ Toggle FULL)
       (sendMessage (TL.Toggle "Full"))
      )
    -- , ("M-b", withFocused toggleBorder)
    -- Mirror
    , ("M-m", sendMessage $ MT.Toggle MIRROR)
    -- DMenu
    , ("M-o", spawn $ "dmenu_run -p 'Run anything:'")
    -- Volume
    , ("M-<Up>", spawn "amixer set Master 5%+")
    , ("M-<Down>", spawn "amixer set Master 5%-")
    -- Scratchpads
    , ("M-C-<Return>", namedScratchpadAction myScratchpads "terminal")
    , ("M-C-m", namedScratchpadAction myScratchpads "music-player")
    -- Close Windows
    , ("M-d", kill1)
    , ("M-S-d", killAll)
    -- Screenshot
    , ("S-<Print>", spawn "flameshot gui")
    , ("C-<Print>", spawn "maim -s | xclip -selection clipboard -t image/png")
    , ("<Print>", spawn "maim | xclip -selection clipboard -t image/png")
    -- Music
    , ("M-p <Space>", spawn "cmus-remote --pause")
    , ("M-p n", spawn "cmus-remote --next")
    , ("M-p p", spawn "cmus-remote --prev")
    , ("M-p r", spawn "cmus-remote --repeat")
    , ("M-p s", spawn "cmus-remote --shuffle")
    , ("M-p h", spawn "cmus-remote --seek -15")
    , ("M-p l", spawn "cmus-remote --seek +15")
    , ("M-C-<Up>", spawn "~/.local/bin/change-volume 'ALSA plug-in [cmus]' +5")
    , ("M-C-<Down>", spawn "~/.local/bin/change-volume 'ALSA plug-in [cmus]' -5")
    , ("M-v t <Up>", spawn "~/.local/bin/change-volume 'WEBRTC VoiceEngine' +15")
    , ("M-v t <Down>", spawn "~/.local/bin/change-volume 'WEBRTC VoiceEngine' -15")
    -- Scripts
    , ("M-x d", spawn "~/.local/bin/mount-android-device")
    , ("M-]", spawn "~/.local/bin/dmenu_latex")
    , ("M-[", spawn "~/.local/bin/read-screen")
    , ("M-g", spawn "~/.local/bin/search-clipboard")
    , ("M-S-g", spawn "~/.local/bin/search-selection")
    -- , ("M-/", spawn "~/.local/bin/send-screenshot")
    , ("M-'", sendMessage (TL.Toggle "Full"))
    -- , ("M-<Left>", spawn "~/.local/bin/photo-previous")
    -- , ("M-<Right>", spawn "~/.local/bin/photo-next")
    ]
