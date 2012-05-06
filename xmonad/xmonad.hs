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
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = xmonad =<< myConfig

myConfig = do
    checkTopicConfig myTopics myTopicConfig
    return =<< statusBar "xmobar" myPrettyPrinter toggleStrutsKey defaultConfig
        { workspaces = myTopics
        , manageHook = myManageHook
        , layoutHook = smartBorders $ myLayout
        , handleEventHook = docksEventHook
        , startupHook = setWMName "LG3D"
        , terminal = "urxvt"
        , normalBorderColor = "white"
        , focusedBorderColor = "black"
        , keys = myKeys
        , focusFollowsMouse = True
        , modMask = mod4Mask
        }
    where
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
            , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

        myLayout = (
            Tall 1 (3/100) (1/2) |||
            Mirror (Tall 1 (3/100) (1/2)) |||
            tabbed shrinkText defaultTheme |||
            Full |||
            spiral (6/7)) |||
            noBorders (fullscreenFull Full)

        myXPConfig :: XPConfig
        myXPConfig = greenXPConfig { font = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15" }

        myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
            -- Start a terminal.
            [ ((modMask .|. shiftMask, xK_Return), spawnShell)
            -- TODO: Merge these.. maybe
            -- ,("M-s"        , warpToCentre >> promptedGoto )
            -- ,("M-S-s"      , warpToCentre >> promptedShift)
            -- workaround
            -- ,("M-<Tab>"    , switchNthLastFocused myTopicConfig . succ . length . W.visible . windowset =<< get )
            -- ,("M-a"        , warpToCentre >> goToSelected gsConfig)
            --
            , ((modMask, xK_p), shellPromptHere myXPConfig)
            -- Close focused window.
            , ((modMask .|. shiftMask, xK_c), kill)
            -- Cycle through the available layout algorithms.
            , ((modMask, xK_space), sendMessage NextLayout)
            --  Reset the layouts on the current workspace to default.
            , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
            -- Resize viewed windows to the correct size.
            , ((modMask, xK_n), refresh)
            -- Move focus to the next window.
            , ((modMask, xK_Tab), windows W.focusDown)
            -- Move focus to the next window.
            , ((modMask, xK_j), windows W.focusDown)
            -- Move focus to the previous window.
            , ((modMask, xK_k), windows W.focusUp  )
            -- Move focus to the master window.
            , ((modMask, xK_m), windows W.focusMaster  )
            -- Swap the focused window and the master window.
            , ((modMask, xK_Return), windows W.swapMaster)
            -- Swap the focused window with the next window.
            , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )
            -- Swap the focused window with the previous window.
            , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )
            -- Shrink the master area.
            , ((modMask, xK_h), sendMessage Shrink)
            -- Expand the master area.
            , ((modMask, xK_l), sendMessage Expand)
            -- Push window back into tiling.
            , ((modMask, xK_t), withFocused $ windows . W.sink)
            -- Increment the number of windows in the master area.
            , ((modMask, xK_comma), sendMessage (IncMasterN 1))
            -- Decrement the number of windows in the master area.
            , ((modMask, xK_period), sendMessage (IncMasterN (-1)))
            -- Quit xmonad.
            , ((modMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
            -- Restart xmonad.
            , ((modMask, xK_q), spawn "xmonad --recompile; xmonad --restart")
            -- TopicSpace binds
            , ((modMask .|. shiftMask, xK_a     ), currentTopicAction myTopicConfig)
            , ((modMask              , xK_s     ), promptedGoto)
            , ((modMask .|. shiftMask, xK_s     ), promptedShift)
            ]
            ++
            [((modMask, k), switchNthLastFocused myTopicConfig i)
                | (i, k) <- zip [1..] [xK_1 .. xK_9]]
            -- ++
            -- mod-[1..9], Switch to workspace N
            -- mod-shift-[1..9], Move client to workspace N
            -- [((m .|. modMask, k), windows $ f i)
            --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
            -- ++
            -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
            -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
            -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
            --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
