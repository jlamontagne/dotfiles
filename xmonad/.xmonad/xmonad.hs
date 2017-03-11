import Data.List
import Data.Maybe
import Data.Ord
import System.IO
import XMonad hiding (Tall)
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
-- import XMonad.Hooks.Place
import XMonad.Layout.Accordion
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Reflect
import XMonad.Layout.LimitWindows
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutHints
import XMonad.Layout.HintedTile
import XMonad.Prompt.Window
import XMonad.Prompt
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.Search
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM

workspaces' = ["dev","review","chat","stack"] ++ map show [4..9]

layoutHook' = smartBorders $ avoidStruts $
    onWorkspace "deploy" (simpleDeco shrinkText defaultTheme (Accordion)) $
    (HintedTile 1 (3/100) (1/2) TopLeft Tall |||
    simpleDeco shrinkText defaultTheme (Accordion) |||
    -- (reflectHoriz $ ResizableTall 1 (3/100) (1/2) []) |||
    -- Mirror (ResizableTall 1 (3/100) (1/2) []) |||
    -- ThreeColMid 1 (3/100) (1/3) |||
    noBorders (fullscreenFull Full))

-- $ xprop | grep WM_CLASS
-- doSideFloat SE, NW, NE, etc
manageHook' = composeAll
    [ className =? "Gimp" --> doF (W.shift "dev")
    , isFullscreen --> doFullFloat
    ]

myXPConfig = greenXPConfig
    { font = "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15" }

additionalKeys' =
    [ ("M-<Delete>"   , windows W.swapMaster)
    , ("M-<Backspace>", sendMessage NextLayout)
    , ("M-p"          , shellPromptHere myXPConfig)
    , ("M-a"          , sendMessage MirrorShrink)
    , ("M-z"          , sendMessage MirrorExpand)
    , ("M-s"          , workspacePrompt defaultXPConfig (windows . W.view))
    , ("M-S-s"        , workspacePrompt defaultXPConfig (windows . W.shift))
    , ("M-b"          , sendMessage ToggleStruts)
    , ("M-S-z"        , spawn "xscreensaver-command --lock")
    , ("M-g"          , S.promptSearchBrowser myXPConfig "google-chrome-stable" S.google)
    ]
    ++
    -- mod-{w,f,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,f,r} %! Move client to screen 1, 2, or 3
    [(("M-"++m++[key]), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip "wfr" [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ ewmh defaultConfig
        { manageHook = manageHook' <+> manageDocks <+> manageHook defaultConfig
        , layoutHook = layoutHook'
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
            , ppVisible = wrap "(" ")"
            }
        , modMask = mod4Mask
        , terminal = "urxvt"
        , borderWidth = 1
        , workspaces = workspaces'
        , normalBorderColor  = "#004400"
        , focusedBorderColor = "#00AA00"
        }
        `additionalKeysP` additionalKeys'
