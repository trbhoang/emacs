import XMonad
import XMonad.Config.Gnome
import XMonad.ManageHook


myManageHook :: [ManageHook]
myManageHook =
    [ resource  =? "Do"   --> doIgnore ]

main = xmonad gnomeConfig
	{ modMask = mod4Mask }
    { manageHook = manageHook gnomeConfig <+> composeAll myManageHook}

