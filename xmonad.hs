import XMonad 
import Codec.Binary.UTF8.String (encodeString)
import Data.Maybe
import XMonad.Util.WorkspaceCompare
import Data.List
import System.IO
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks 
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Mosaic
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedWindows
import qualified XMonad.StackSet as W

defaults = defaultConfig 
    { terminal           = "st"
    , manageHook         = manageDocks
        <+>  manageHook defaultConfig
    , workspaces         = myWorkspaces
    , modMask            = mod4Mask
    , layoutHook         = avoidStruts myLayoutHook
    , handleEventHook    = handleEventHook defaultConfig
        <+> fullscreenEventHook
        <+> docksEventHook
        <+> ewmhDesktopsEventHook 
    , borderWidth        = 1
    , normalBorderColor  = "black"
    , focusedBorderColor = "orange" }

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

myLayoutHook = toggleLayouts (noBorders Full)
    $ smartBorders
    $ Full ||| mosaic 1 []

myLogHook :: (String -> IO ()) -> X ()

myLogHook outPut = do
    winset <- gets windowset
    sort' <- getSortByIndex
    let current = W.workspace $ W.current winset
        workspacesView = concatMap workSpaceFormat
                       $ sort'
                       $ W.workspaces winset
        workSpaceFormat ws | W.tag current == W.tag ws = "[" ++ workSpaceColor ws ++ "]"
                           | otherwise                 = " " ++ workSpaceColor ws ++ " "
        workSpaceColor ws | isNothing $ W.stack ws = W.tag ws
                          | otherwise              = xmobarColor "orange" "black" $ W.tag ws
        tabbedNames = showStack $ W.stack current
        show'       = concatMap mShow
        mShow name  = "[" ++ show name                                ++ "]"
        mShowC name = "[" ++ xmobarColor "orange" "black" (show name) ++ "]"
        showStack Nothing = return "" 
        showStack (Just a) = do
            prvWs    <- mapM getName $ W.up a
            currentW <- getName      $ W.focus a
            nxtWs    <- mapM getName $ reverse $ W.down a
            return (show' nxtWs ++ mShowC currentW ++ show' prvWs)
    answer <- tabbedNames
    (io . outPut) $ encodeString $ workspacesView ++ "   " ++ answer

main = do
    spawn "find /home/askhat/Dropbox/covers -type f -name '*.*' -print0 | shuf -n1 -z | xargs -0 feh --bg-max"
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ defaults
        { logHook = myLogHook $ System.IO.hPutStrLn xmproc }
