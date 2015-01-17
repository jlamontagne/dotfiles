-- Heavily modified version of Vic Fryzel's xmonad.hs with various pieces from:
--
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey's_darcs_xmonad.hs
-- http://github.com/vicfryzel/xmonad-config
-- http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Arossato.html#arossatoConfig
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo's_xmonad.hs
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html
--
-- TODO
--
-- dynamic workspaces for code\d (See byorgey's config)
-- per-workspace layouts (maybe not necessary with fullscreen managehook for
--     xine, etc)
-- search

import Data.List
import Data.Maybe
import Data.Ord
import System.IO
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
-- import XMonad.Hooks.Place
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = do
    checkTopicConfig myTopicNames myTopicConfig
    xmonad =<< statusBar "xmobar" myPrettyPrinter toggleStrutsKey myConfig

myConfig = withUrgencyHook NoUrgencyHook defaultConfig
    { workspaces         = myTopicNames
    , manageHook         = manageSpawn <+> floatNextHook <+> myManageHook
    , layoutHook         = smartBorders $ myLayout
    , logHook = fadeInactiveLogHook 0.5
    , handleEventHook    = docksEventHook
    , terminal           = "urxvt"
    -- , normalBorderColor  = "#586e75"
    , normalBorderColor  = "#111111"
    , focusedBorderColor = "#cccccc"
    , borderWidth = 1
    , focusFollowsMouse  = False
    , modMask            = mod4Mask
    , startupHook        = return () >> checkKeymap myConfig myKeys -- as prescribed by docs
    -- , startupHook        = composeAll
        -- [ setWMName "LG3Dverify"
        -- , return () >> checkKeymap myConfig myKeys -- as prescribed by docs
        -- ]
    } `additionalKeysP` myKeys
    -- } `additionalKeysP` myKeys `removeKeys` myRemoveKeys

-- myRemoveKeys = [ (mod1Mask, xK_Tab) ]
-- , (button8        , (\w -> focus w >> mouseResizeWindow w))

myKeys =
    [ ("M-S-<Delete>" , spawnShell)
    , ("M-<Delete>"   , windows W.swapMaster)
    , ("M-<Backspace>", sendMessage NextLayout)
    , ("M-p"          , shellPromptHere myXPConfig)
    , ("M-S-a"        , currentTopicAction myTopicConfig)
    , ("M-s"          , promptedGoto)
    , ("M-S-s"        , promptedShift)
    , ("M-<Page_Up>"  , focusUrgent)
    , ("M-S-<Page_Up>", clearUrgents)
    , ("M-a"          , sendMessage MirrorShrink)
    , ("M-z"          , sendMessage MirrorExpand)
    , ("M-f"          , toggleFloatAllNew)

    -- Used to switch between floating EVE clients
    -- focus the next window (which should be floating), then swap it to master
    -- to pop it to the top of the view
    -- , ("C-t"          , (windows W.focusDown) >> (windows W.swapMaster))
    ]
    ++
    [ ("M-"++m++[k], a i)
        | (a, m) <- [ (gotoNthLastFocused,"")
                    , (shiftNthLastFocused, "S-")
                    ]
        , (i, k) <- zip [1..] "123456789"
    ]
    -- ++
    -- mod-{w,f,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,f,r} %! Move client to screen 1, 2, or 3
    -- [(("M-"++m++[key]), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip "wfr" [0..]
    --     , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

myLayout = (
    reflectHoriz $ ResizableTall 1 (3/100) (1/2) [] |||
    Mirror (ResizableTall 1 (3/100) (1/2) []) |||
    ThreeColMid 1 (3/100) (1/3) |||
    noBorders (fullscreenFull Full))

myXPConfig = greenXPConfig { font = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15" }

-- Nice refactorization of TopicSpace config from Brent Yorgey's xmonad.hs
data TopicItem = TI { topicName   :: Topic
                    , topicDir    :: Dir
                    , topicAction :: X ()
                    }

myTopics =
    --   Name           Directory           Default Action
    [ TI "web"          ""                  (spawn "google-chrome-stable")
    , TI "mail"         ""                  (runInTerm "" "ssh 10.8.0.1 -t mutt")
    , TI "src"          "src"               (spawnShell >*> 2)
    , TI "src0"         "src"               (spawnShell >*> 2)
    , TI "medrem"       "src/medrem"        (spawnShell >*> 3)
    , TI "townlobby"    "src/townlobby"     (spawnShell >*> 3)
    , TI "dotfiles"     ".dotfiles"         (vim "")
    , TI "xm"           ".dotfiles/xmonad"  (vim "xmonad.hs")
    , TI "music"        "music"             (runInTerm "" "ncmpcpp")
    , TI "torrent"      ""                  (spawn "transmission-gtk")
    , TI "skype"        ""                  (spawn "ALSA_PCM=\"dmix\" skype")
    ]
    where
        -- Helper for topics that just need a shell
        ti t d = TI t d spawnShell

-- Open file in vim, relative to topic's directory
vim :: String -> X ()
vim file = do
    dir <- currentTopicDir myTopicConfig
    let opts = "-name vim -cd $HOME/" ++ dir
    runInTerm opts $ "vim " ++ file

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
    { topicDirs          = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics
    , defaultTopicAction = const (return ())
    , defaultTopic       = "src"
    , maxTopicHistory    = 10
    , topicActions       = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics
    }

myPrettyPrinter = xmobarPP
                    { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                    , ppTitle   = xmobarColor "green"  "" . shorten 40
                    , ppVisible = wrap "(" ")"
                    , ppUrgent  = xmobarColor "red" "" . xmobarStrip
                    , ppSort    = ppSortTS
                    }

-- $ xprop | grep WM_CLASS
-- doSideFloat SE, NW, NE, etc
myManageHook = composeAll
    [ className =? "Gimp"  --> doShift "*" -- may be "Gimp" or "Gimp-2.4" instead
    , (className =? "Gimp" <&&> fmap ("tool" `isSuffixOf`) role) --> doFloat
    , className =? "MPlayer"        --> doFloat
    , resource  =? "skype"          --> doFloat
    -- , className =? "Chromium"       --> doShift "2:web"
    -- , className =? "Google-chrome"  --> doShift "2:web"
    -- , className =? "VirtualBox"     --> doShift "4:vm"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    -- , name =? "Kerbal Space Program" --> doFullFloat
    ]
    where role = stringProperty "WM_WINDOW_ROLE"
    -- where name = stringProperty "WM_NAME"

goto :: Topic -> X ()
goto topic = do
    winset <- gets windowset
    let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
        currentTopic = (W.tag . W.workspace . W.current $ winset)

    -- Push current, then topic to get: [topic, currentTopic, ...]
    --                                          ^- goto(N=1)thLastFocused
    setLastFocusedTopic currentTopic (`notElem` empty_workspaces)
    setLastFocusedTopic topic (`notElem` empty_workspaces)
    switchTopic myTopicConfig topic

-- Adapted from XMonad.Actions.TopicSpace.switchNthLastFocused
gotoNthLastFocused :: Int -> X()
gotoNthLastFocused depth = do
    lastWs <- getLastFocusedTopics
    goto $ (lastWs ++ repeat (defaultTopic myTopicConfig)) !! depth

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = do
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t

-- Copied from XMonad.Hooks.DynamicLog
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)

-- Adapted from XMonad.Actions.TopicSpace.pprWindowSet
ppSortTS :: X ([WindowSpace] -> [WindowSpace])
ppSortTS = do
    lastWs <- getLastFocusedTopics
    let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
        maxDepth = maxTopicHistory myTopicConfig
    return $ take maxDepth . sortBy (comparing $ depth . W.tag)
