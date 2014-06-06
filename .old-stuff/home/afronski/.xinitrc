#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Update fonts directories.
xset +fp /usr/share/fonts/artwiz-latin1-fonts
xset fp rehash

# Compiz Manager, XScreenSaver and Xresources.
exec xcompmgr -cF &
xrdb ~/.Xresources &

# Autostart.
sleep 0.8 && urxvt -T Terminal &
sleep 0.8 && urxvt -T Terminal &
sleep 0.8 && urxvt -T Terminal &
chromium &

BROWSER=links
if [ -n "$DISPLAY" ]; then
	BROWSER=chromium
fi

# Fixing cursors.
sleep 0.6 && xsetroot -cursor_name left_ptr &

# Dropbox.
sleep 1.0 && ./.dropbox-dist/dropboxd &

DEFAULT_SESSION=awesome
case $1 in
	awesome|wmii|xmonad) exec $1 ;;
	*) exec $DEFAULT_SESSION ;;
esac
