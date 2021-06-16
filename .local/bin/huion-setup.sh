#!/bin/sh
STYLUS_ID="$(xsetwacom --list | grep stylus | cut -f 2 | cut -d " " -f 2)"
PAD_ID="$(xsetwacom --list | grep 'Pad pad' | cut -f 2 | cut -d " " -f 2)"

SCREEN_WIDTH=1920
SCREEN_HEIGHT=1080

TABLET_WIDTH="$(xsetwacom --get "$STYLUS_ID" Area | cut -d " " -f 3)"
TABLET_HEIGHT="$(xsetwacom --get "$STYLUS_ID" Area | cut -d " " -f 4)"

NEW_TABLET_HEIGHT="$(echo "scale=0; $SCREEN_HEIGHT * $TABLET_WIDTH / $SCREEN_WIDTH" | bc)"

TABLET_OFFSET_Y="$(echo "scale=0; $TABLET_HEIGHT - $NEW_TABLET_HEIGHT" | bc)"
TABLET_OFFSET_Y="$(echo "scale=0; $TABLET_OFFSET_Y / 2" | bc)"

SCREEN_ID="$(xrandr | grep primary | cut -d " " -f 1)"
if [ ! $# -eq 0 ]; then
    case "$1" in
        "primary")
            ;;

        *)
            SCREEN_ID="$(xrandr | grep connected | sed "$1q;d" | cut -d " " -f 1)"
            ;;
    esac
fi

xsetwacom --set "$STYLUS_ID" ResetArea
xsetwacom --set "$PAD_ID" RawSample 4
xinput map-to-output "$STYLUS_ID" "$SCREEN_ID"
xsetwacom --set "$STYLUS_ID" Area 0 "$TABLET_OFFSET_Y" "$TABLET_WIDTH" "$NEW_TABLET_HEIGHT"
