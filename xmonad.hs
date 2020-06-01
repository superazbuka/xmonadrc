import XMonad 
import Codec.Binary.UTF8.String (encodeString)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Util.WorkspaceCompare
import XMonad.Layout.SimplestFloat
import Data.List
import System.IO
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks 
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import XMonad.Layout.NoBorders
import XMonad.Layout.Mosaic
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedWindows
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Util.NamedScratchpad
import XMonad.Config
import XMonad.Util.Timer
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Layout.SimplestFloat

myHandleEventHook = handleEventHook def

myStartUpHook = startupHook def

myTerminal = "alacritty"

myKeys = [ ("M-n", namedScratchpadAction myScratchpads "terminal"),
           ("M-c", kill),
           ("M-<Return>", spawn myTerminal)
         ]

defaults = def 
    { terminal           = myTerminal
    , manageHook         = manageHook def
                       <+> (isFullscreen --> doFullFloat)
		       <+> namedScratchpadManageHook myScratchpads
    , workspaces         = myWorkspaces
    , modMask            = mod4Mask
    , handleEventHook    = myHandleEventHook 
                       <+> fullscreenEventHook
                       <+> docksEventHook
                       <+> ewmhDesktopsEventHook
    , layoutHook         = myLayoutHook
    , borderWidth        = 1
    , normalBorderColor  = "black"
    , startupHook        = myStartUpHook
    , focusedBorderColor = "orange"
    } `additionalKeysP` myKeys

myWorkspaces :: [String]
myWorkspaces = map show [1..9]

myLayoutHook = avoidStruts
             $ smartBorders
             $ Full ||| Tall 1 (3/100) (1/2)

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
        mShow name  = "[" ++ takeWhile (\x -> (x /= ' ')) (show name)                                ++ "]"
        mShowC name = "[" ++ xmobarColor "orange" "black" (takeWhile (\x -> (x /= ' '))  (show name)) ++ "]"
        showStack Nothing = return "" 
        showStack (Just a) = do
            prvWs    <- mapM getName $ W.up a
            currentW <- getName      $ W.focus a
            nxtWs    <- mapM getName $ reverse $ W.down a
            return (show' nxtWs ++ mShowC currentW ++ show' prvWs)
    answer <- tabbedNames
    (io . outPut) $ encodeString $ workspacesView ++ "   " ++ answer

myScratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                ]
    where
    termName = "scratchpad"
    spawnTerm = "st" ++ " -n " ++ termName
    findTerm  = resource =? termName
    manageTerm = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar.hs"
    xmonad $ ewmh $ defaults
        { logHook = myLogHook $ System.IO.hPutStrLn xmproc } `removeKeysP` ["M-S-<Return>", "M-S-c"]
