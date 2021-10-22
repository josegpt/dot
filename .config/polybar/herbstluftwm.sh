#!/bin/env bash

MON_IDX="0"
mapfile -t MONITOR_LIST < <(polybar --list-monitors | cut -d":" -f1)
for (( i=0; i<$((${#MONITOR_LIST[@]})); i++ )); do
  [[ ${MONITOR_LIST[${i}]} == "$MONITOR" ]] && MON_IDX="$i"
done;

herbstclient --idle "tag_*" 2>/dev/null | {
    while true; do
        # Read tags into $tags as array
        IFS=$'\t' read -ra tags <<< "$(herbstclient tag_status "${MON_IDX}")"
        {
            for tag in $(herbstclient tag_status "$1"); do
                # Read the prefix from each tag and render them according to that prefix
                name=${tag#?}
                state=${tag%$name}
                case "$state" in
                    '.')
                        # Empty
                        printf ' %s ' "$name"
                        ;;
                    ':')
                        # Not empty
                        printf "%%{B${COLOR_WEAKER} F${COLOR_FG}} %s %%{B- F-}" "$name"
                        ;;
                    '+')
                        # View in monitor but not focused
                        printf "%%{B${COLOR_WEAK} F${COLOR_BG}} %s %%{B- F-}" "$name"
                        ;;
                    '#')
                        # View in monitor and focused
                        printf "%%{B${COLOR_HL} F${COLOR_FG}} %s %%{B- F-}" "$name"
                        ;;

                    '-')
                        # View in another monitor but not focused
                        printf "%%{B${COLOR_S} F${COLOR_BG}} %s %%{B- F-}" "$name"
                        ;;
                    '%')
                        # View in another monitor and focused
                        printf "%%{B${COLOR_SCS} F${COLOR_BG}} %s %%{B- F-}" "$name"
                        ;;
                    '!')
                        # Urgent
                        printf "%%{B${COLOR_WG} F${COLOR_BG}} %s %%{B- F-}" "$name"
                        ;;
                    *)
                        printf ' %s ' "$name"
                esac
            done
        } | tr -d "\n"

        echo

        # wait for next event from herbstclient --idle
        read -r || break
    done
} 2>/dev/null
