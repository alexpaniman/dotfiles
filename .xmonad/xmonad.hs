import XMonad

import XMonad.Config.Desktop
import System.Exit (exitSuccess)

import XMonad.Layout.Spacing (spacing)

import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeysP)

import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.MultiToggle.Instances

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)

import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Hooks.SetWMName (setWMName)

myTerminal = "alacritty"
myMusicPlayer = "cmus"
myTop = "gotop"
myBrowser = "brave"
myFileManager = "vifm"
myEditor = "nvim"

myStartupHook = do
    spawnOnce "nitrogen --restore &"

    spawnOnce $ "picom" ++
        " --experimental-backends" ++
        " --backend glx" ++
        " --vsync" ++
        " --config ~/.config/picom/picom.conf &"

    spawnOnce "/usr/lib/xfce4/notifyd/xfce4-notifyd &"
    spawnOnce "udiskie &"

    setWMName "LG3D"

myLayoutHook = mkToggle (single MIRROR) $ smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ spacing 6 $ 
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

main = xmonad $ desktopConfig
    { borderWidth = 2
    , startupHook = myStartupHook
    , manageHook = myManageHook
    , layoutHook = myLayoutHook
    , modMask = mod4Mask
    , terminal = myTerminal
    , normalBorderColor = "#cccccc"
    , focusedBorderColor = "#d79921" -- "#6f00cc"
    } `additionalKeysP` [
    -- XMonad
      ("M-S-r", spawn "xmonad --restart")
    , ("M-S-q", io exitSuccess)
    -- Terminal
    , ("M-<Return>", spawn $ myTerminal)
    -- Apps
    , ("M-; i", spawn myBrowser)
    , ("M-; t", spawn $ myTerminal ++ " -e " ++ myTop)
    , ("M-; e", spawn $ myTerminal ++ " -e " ++ myEditor)
    , ("M-; f", spawn $ myTerminal ++ " -e " ++ myFileManager)
    -- Fullscreen
    , ("M-f", sendMessage $ Toggle FULL)
    -- Mirror
    , ("M-m", sendMessage $ Toggle MIRROR)
    -- DMenu
    , ("M-o", spawn $ "dmenu_run -fn 'Fira Code Medium:size=12'" ++
                   " -nb '#282828'" ++
				   " -nf '#cccccc'" ++
				   " -sb '#d79921'" ++
				   " -sf '#282828'" ++
                   " -p 'Run anything:'")
    -- Volume
    , ("M-<Up>", spawn "amixer set Master 5%+")
    , ("M-<Down>", spawn "amixer set Master 5%-")
    -- Scratchpads
    , ("M-s <Return>", namedScratchpadAction myScratchpads "terminal")
    , ("M-s m", namedScratchpadAction myScratchpads "music-player")
    -- Close Windows
    , ("M-d", kill1)
    , ("M-S-d", killAll)
    -- Screenshot
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
    ]
