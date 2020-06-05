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

import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Hooks.SetWMName (setWMName)

myTerminal = "alacritty"
myMusicPlayer = "cmus"
myTop = "gotop"
myBrowser = "firefox"

myStartupHook = do
    spawnOnce "nitrogen --restore &"
    spawnOnce "picom --config ~/.config/picom/picom.conf &"
    spawnOnce "/usr/lib/xfce4/notifyd/xfce4-notifyd &"
    spawnOnce "udiskie &"

myLayoutHook = mkToggle (single MIRROR) $ smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ spacing 6 $ 
    ResizableTall 1 (3/100) (1/2) [] ||| ThreeColMid 1 (3/100) (1/2)

myManageHook = composeAll [
      namedScratchpadManageHook myScratchpads
  --, appName =? "jetbrains-idea-ce" --> doFloat
    ]

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
    -- Apps
    , ("M-M1-b", spawn myBrowser)
    , ("M-M1-t", spawn $ myTerminal ++ " -e " ++ myTop)
    -- Fullscreen
    , ("M-S-f", sendMessage $ Toggle FULL)
    -- Mirror
    , ("M-S-\\", sendMessage $ Toggle MIRROR)
    -- DMenu
    , ("M-<Return>", spawn $ "dmenu_run -fn 'Fira Code Medium:size=10'" ++
                   " -nb '#292d3e'" ++
				   " -nf '#bbc5ff'" ++
				   " -sb '#82AAFF'" ++
				   " -sf '#292d3e'" ++
                   " -p 'Run anything:'")
    -- Volume
    , ("M-<Up>", spawn "amixer set Master 5%+")
    , ("M-<Down>", spawn "amixer set Master 5%-")
    -- Scratchpads
    , ("M-C-<Return>", namedScratchpadAction myScratchpads "terminal")
    , ("M-C-m", namedScratchpadAction myScratchpads "music-player")
    -- Screenshot
    , ("C-<Print>", spawn "maim -s | xclip -selection clipboard -t image/png")
    , ("<Print>", spawn "maim | xclip -selection clipboard -t image/png")
    ]
