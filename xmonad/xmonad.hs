-- Author: Vic Fryzel
-- modified by: Josh Lamontagne
-- with ideas from:
--      http://xmonad.org/xmonad-docs/xmonad-contrib/src/XMonad-Config-Arossato.html#arossatoConfig
--      http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-TopicSpace.html
-- http://github.com/vicfryzel/xmonad-config

import System.IO
import System.Exit
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
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = xmonad =<< myConfig

myConfig = do
    checkTopicConfig myTopics myTopicConfig
    return =<< statusBar "xmobar" myPrettyPrinter toggleStrutsKey $ defaultConfig
        { workspaces = myTopics
        , manageHook = myManageHook
        , layoutHook = smartBorders $ myLayout
        , handleEventHook = docksEventHook
        , startupHook = setWMName "LG3D"
        , terminal = "urxvt"
        , normalBorderColor = "white"
        , focusedBorderColor = "black"
        , focusFollowsMouse = True
        , modMask = mod4Mask
        } `additionalKeysP` myKeys
    where
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
            -- TODO: Merge these.. maybe
            -- ,("M-s"        , warpToCentre >> promptedGoto )
            -- ,("M-S-s"      , warpToCentre >> promptedShift)
            -- workaround
            -- ,("M-<Tab>"    , switchNthLastFocused myTopicConfig . succ . length . W.visible . windowset =<< get )
            -- ,("M-a"        , warpToCentre >> goToSelected gsConfig)
            --

        -- The list of all topics/workspaces of your xmonad configuration.  The order is
        -- important, new topics must be inserted at the end of the list if you want
        -- hot-restarting to work.
        myTopics :: [Topic]
        myTopics =
            [ "dotfiles"
            , "admin"
            , "conf"
            , "web"
            , "tv"
            , "music"
            ]

        myTopicConfig :: TopicConfig
        myTopicConfig = defaultTopicConfig {
            topicDirs = M.fromList $
                [ ("dotfiles", ".dotfiles")
                , ("tv", "/mnt/percy/torrent/seeding")
                ]
            , topicActions = M.fromList $
                [ ("dotfiles",   spawnShell)
                -- , ("darcs",      spawnShell >*> 3)
                -- , ("yi",         spawnShell >*> 3)
                -- , ("haskell",    spawnShell >*> 2 >>
                --                  spawnShellIn "wd/dev-haskell/ghc")
                -- , ("xmonad",     spawnShellIn "wd/x11-wm/xmonad" >>
                --                  spawnShellIn "wd/x11-wm/xmonad/contrib" >>
                --                  spawnShellIn "wd/x11-wm/xmonad/utils" >>
                --                  spawnShellIn ".xmonad" >>
                --                  spawnShellIn ".xmonad")
                -- , ("mail",       mailAction)
                -- , ("irc",        ssh somewhere)
                -- , ("admin",      ssh somewhere >>
                --                  ssh nowhere)
                -- , ("dashboard",  spawnShell)
                -- , ("twitter",    spawnShell)
                -- , ("web",        spawn browserCmd)
                -- , ("movie",      spawnShell)
                -- , ("documents",  spawnShell >*> 2 >>
                --                  spawnShellIn "Documents" >*> 2)
                -- , ("pdf",        spawn pdfViewerCmd)
                ]
            , defaultTopicAction = const $ spawnShell >*> 3
            , defaultTopic = "dotfiles"
        }

        spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

        spawnShellIn dir = do
            t <- asks (terminal . config)
            spawnHere $ "cd " ++ dir ++ " && " ++ t

        goto = switchTopic myTopicConfig

        promptedGoto = workspacePrompt myXPConfig goto

        promptedShift = workspacePrompt myXPConfig $ windows . W.shift

        -- xmobar
        --
        -- Helper function which provides ToggleStruts keybinding. This is copied from
        -- XMonad.Hooks.DynamicLog and probably needs to be exported with statusBar.
        --
        toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
        toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )

        myPrettyPrinter = xmobarPP
                            { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
                            , ppTitle   = xmobarColor "green"  "" . shorten 40
                            , ppVisible = wrap "(" ")"
                            }

        ------------------------------------------------------------------------
        -- To find the property name associated with a program, use
        -- > xprop | grep WM_CLASS
        -- and click on the client you're interested in.
        --
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

        myLayout = (
            Tall 1 (3/100) (1/2) |||
            Mirror (Tall 1 (3/100) (1/2)) |||
            tabbed shrinkText defaultTheme |||
            Full |||
            spiral (6/7)) |||
            noBorders (fullscreenFull Full)

        myXPConfig :: XPConfig
        myXPConfig = greenXPConfig { font = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15" }
