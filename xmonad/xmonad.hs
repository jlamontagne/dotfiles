-- Heavily modified version of Vic Fryzel's xmonad.hs with ideas from:
--
-- http://github.com/vicfryzel/xmonad-config
-- http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Arossato.html#arossatoConfig
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/adamvo's_xmonad.hs
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html

import System.IO
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = do
    checkTopicConfig myTopics myTopicConfig
    xmonad =<< statusBar "xmobar" myPrettyPrinter toggleStrutsKey myConfig

myConfig = defaultConfig
        { workspaces = myTopics
        , manageHook = myManageHook
        , layoutHook = smartBorders $ myLayout
        , handleEventHook = docksEventHook
        , startupHook = composeAll
            [ setWMName "LG3Dverify"
            , return () >> checkKeymap myConfig myKeys -- as prescribed by docs
            ]
        , terminal = "urxvt"
        , normalBorderColor = "white"
        , focusedBorderColor = "black"
        , focusFollowsMouse = True
        , modMask = mod4Mask
        } `additionalKeysP` myKeys

myKeys =
    [ ("M-S-<Return>", spawnShell)
    , ("M-p", shellPromptHere myXPConfig)
    , ("M-S-a", currentTopicAction myTopicConfig)
    , ("M-s", promptedGoto)
    , ("M-S-s", promptedShift)
    ]
    ++
    [ ("M-"++m++[k], a i)
        | (a, m) <- [(switchNthLastFocused myTopicConfig,""),(shiftNthLastFocused, "S-")]
        , (i, k) <- zip [1..] "123456789"
    ]

myLayout = (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText defaultTheme |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

myXPConfig = greenXPConfig { font = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15" }

-- The order is important, new topics must be inserted at the end of the
-- list if you want hot-restarting to work.
myTopics = [ "dotfiles", "admin", "conf", "web", "tv", "music" ]

myTopicConfig = defaultTopicConfig {
    topicDirs = M.fromList $
        [ ("dotfiles", ".dotfiles")
        , ("tv", "/mnt/percy/torrent/seeding")
        ]
    , topicActions = M.fromList $
        [ ("dotfiles",   spawnShell)
        -- , ("darcs",      spawnShell >*> 3)
        -- , ("haskell",    spawnShell >*> 2 >>
        --                  spawnShellIn "wd/dev-haskell/ghc")
        -- , ("xmonad",     spawnShellIn "wd/x11-wm/xmonad" >>
        --                  spawnShellIn ".xmonad")
        -- , ("mail",       mailAction)
        -- , ("dashboard",  spawnShell)
        -- , ("web",        spawn browserCmd)
        -- , ("documents",  spawnShell >*> 2 >>
        --                  spawnShellIn "Documents" >*> 2)
        ]
    , defaultTopicAction = const $ spawnShell >*> 3
    , defaultTopic = "dotfiles"
}

goto = switchTopic myTopicConfig
promptedGoto = workspacePrompt myXPConfig goto
promptedShift = workspacePrompt myXPConfig $ windows . W.shift
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
spawnShellIn dir = do
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t

-- Copied from XMonad.Hooks.DynamicLog
toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myPrettyPrinter = xmobarPP
                    { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                    , ppTitle   = xmobarColor "green"  "" . shorten 40
                    , ppVisible = wrap "(" ")"
                    }

-- $ xprop | grep WM_CLASS
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "2:web"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Galculator"     --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Google-chrome"  --> doShift "2:web"
    , className =? "MPlayer"        --> doFloat
    , resource  =? "skype"          --> doFloat
    , className =? "VirtualBox"     --> doShift "4:vm"
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]
