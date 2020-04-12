# Applying Xresources
xrdb ~/.Xresources

# Setting cursor
xsetroot -cursor_name left_ptr

# Setting sessions
case %SESSION in
bspwm | *)
	exec bspwm
;;
esac


