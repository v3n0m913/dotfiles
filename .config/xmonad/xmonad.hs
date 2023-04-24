import XMonad
 -- import XMonad.Config.Desktop
import Control.Monad (liftM2)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Cursor
import XMonad.Util.Paste
import XMonad.Util.SpawnOnce
import XMonad.Actions.PhysicalScreens

    -- Data
import Data.Monoid
import Data.Maybe (isJust, fromJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ServerMode
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work
import XMonad.Hooks.InsertPosition

    -- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.GridSelect
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Accordion
import XMonad.Layout.OneBig
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

    -- Prompts
import XMonad.Prompt
import XMonad.Prompt.OrgMode (orgPrompt)

    -- Colorscheme
import Colors.DoomOne

myFont :: String
myFont = "xft:Ubuntu Mono Nerd Font Mono:regular:size=12:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "kitty"    -- Sets default terminal

myBrowser :: String
myBrowser = "qutebrowser "  -- Sets qutebrowser as browser

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME

myWorkspaces :: [String]
--myWorkspaces = ["home", "www", "sys", "dev", "doc", "virt", "gfx", "music", "kodi"]
myWorkspaces = [" \xf303 ", " \xf312 ", " \xf31b ", " \xf316 ", " \xf306 ", " \xf327 ", " \xf30a ", " \xf30d ", " \xf31a "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..]

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
             where
               myDefaultLayout =     tall
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| grid
                                 ||| spirals
                                 ||| threeCol
                                 ||| threeRow
                                 ||| tallAccordion
                                 ||| wideAccordion

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 1
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ mySpacing 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)
tallAccordion  = renamed [Replace "tallAccordion"]
           $ Accordion
wideAccordion  = renamed [Replace "wideAccordion"]
           $ Mirror Accordion

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
        [ className =? "Kodi"         --> doShift   ( myWorkspaces !! 8 )
        --, className =? "qutebrowser"  --> viewShift ( myWorkspaces !! 1 )
        --, className =? "firefox"      --> viewShift ( myWorkspaces !! 1 )
        --, className =? "chromium"     --> viewShift ( myWorkspaces !! 1 )
        , className =? "Virt-manager" --> viewShift ( myWorkspaces !! 5 )
        , className =? "VirtualBox Manager"   --> viewShift ( myWorkspaces !! 5 )
        , className =? "Gimp-2.10"    --> viewShift ( myWorkspaces !! 6 )
        , className =? "Gimp-2.10"    --> doFloat
        , (className =? "Gimp-2.10" <&&> resource =? "Dialog") --> doFloat   -- Float Gimp Dialog
        , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat   -- Float Firefox Dialog
        , manageDocks
        , isDialog --> doFloat <+> doF W.focusDown
        ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

myStartupHook = do
        spawn "killall trayer"

        spawnOnce "lxsession &"
        spawnOnce "feh --bg-fill ~/.config/wall.png &"
        spawnOnce "picom --experimental-backends &"
        spawnOnce "xfce4-power-manager &"
        spawnOnce "nm-applet &"
        spawnOnce "/usr/bin/emacs --daemon &"
        spawnOnce "cbatticon &"
        spawnOnce "volumeicon &"
        -- spawnOnce "xrandr --output HDMI-1 --transform 0.80,0,-40,0,1.04,-40,0,0,1 &"
        spawnOnce "xrandr --output HDMI-1 --transform 1.07,0,-50,0,1.12,-34,0,0,1 &"
        spawnOnce "xmobar -x1 .config/xmobar/xmobarrc &"

        spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0  --transparent true --alpha 0 " ++ colorTrayer ++ " --height 28")
        -- spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 24 &"

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Windows
        , ("M-S-c", kill1)                           -- Kill the currently focused client
        , ("M-S-z", killAll)                         -- Kill all the windows on current workspace

    -- Floating windows
        , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
        , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.

    -- Windows navigation
        , ("M-m", windows W.focusMaster)             -- Move focus to the master window
        , ("M-j", windows W.focusDown)               -- Move focus to the next window
        , ("M-k", windows W.focusUp)                 -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster)            -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
        , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
        , ("M-<Backspace>", promote)                 -- Moves focused window to master, all others maintain order
        , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
        , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3, these work automatically
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3, these work automatically
      -- mod-{1-9}, Move to workplace, these work automatically
      -- mod-shift {1-9}. Move client to workplace, these work automatically
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
        , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
        , ("M-<Right>", sendMessage (MoveRight 10))       --  Move focused window to right
        , ("M-<Left>", sendMessage (MoveLeft 10))         --  Move focused window to left
        , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
        , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
        , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
        , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
        , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
        , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
        , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
        , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left

    -- Emacs (CTRL-e followed by a key)
        , ("M-e e", spawn "emacsclient -c -a 'emacs'")    -- start emacs
        , ("M-e t", spawn (myTerminal ++ " -e emacsclient -t"))    -- start emacs
        -- , ("M-e e", spawn "/usr/bin/emacs")
        , ("M-t w", spawn (myTerminal ++ " -e sxiv -t ~/wallpapers/"))    -- start sxiv

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)                              -- Switch to next layout
        , ("M-S-<Space>", sendMessage ToggleStruts)                          -- Toggles struts
        , ("M-S-n", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
        , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-f", sendMessage (T.Toggle "float"))
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile
    --    , ("M-S-x", sendMessage $ Toggle REFLECTX)
    --    , ("M-S-y", sendMessage $ Toggle REFLECTY)
    --    , ("M-S-u", sendMessage $ Toggle MIRROR)
        , ("M-S-x", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
        , ("M-S-y", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
        , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
        , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-C-j", sendMessage MirrorShrink)
        , ("M-C-k", sendMessage MirrorExpand)
        , ("M-S-;", sendMessage zoomReset)
        , ("M-;", sendMessage ZoomFullToggle)

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

    -- Workspaces
        , ("M-.", nextScreen)                           -- Switch focus to next monitor
        , ("M-,", prevScreen)                           -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next workspace
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to previous workspace

    -- Open My Preferred Terminal.
        , ("M-<Return>", spawn (myTerminal))
        , ("M-<Space>", spawn (myTerminal))
        , ("M-C-<Return>", spawn (myTerminal ++ " -e ranger"))

    --- Rofi and Dmenu Scripts
        , ("M-S-<Return>", spawn "rofi -modi drun -show drun -show-icons")
        , ("M-C-<Space>", spawn "rofi -show run -show-icons")

    -- Other Dmenu Prompts
    -- In Xmonad and many tiling window managers, M-p is the default keybinding to
    -- launch dmenu_run, so I've decided to use M-p plus KEY for these dmenu scripts.
        , ("M-p c", spawn "/usr/bin/dm-colors")  -- pick color from our scheme
        , ("M-p e", spawn "/usr/bin/dm-confedit")   -- edit config files
        , ("M-p i", spawn "/usr/bin/dm-maim")  -- screenshots (images)
        -- , ("M-p k", spawn "/usr/bin/dm-kill")   -- kill processes
        , ("M-p m", spawn "/usr/bin/dm-man")     -- manpages
        , ("M-p o", spawn "/usr/bin/dm-bookman")   -- qutebrowser bookmarks/history
        , ("M-p p", spawn "passmenu")                    -- passmenu
        , ("M-p q", spawn "/usr/bin/dm-logout") -- logout menu
        , ("M-p r", spawn "/usr/bin/dm-reddit")    -- reddio (a reddit viewer)
        , ("M-p s", spawn "/usr/bin/dm-websearch") -- search various search engines
        , ("M-p k", spawn "/usr/bin/theme_choose") -- search various search engines
        , ("M-p w", spawn "/usr/bin/dm-setbg")   -- sets wallpaper

    --- My Applications
    --  My Preferred Web Browsers
        , ("M-b c", spawn "/usr/bin/chromium")
        , ("M-b q", spawn "/usr/bin/qutebrowser")
        , ("M-b f", spawn "/usr/bin/firefox")
        , ("M-b t", spawn $ "sh -c '/home/andrew/tor-browser/Browser/start-tor-browser'")

    -- Other Commonly Used Applications
        , ("M-a v", spawn "/usr/bin/virtualbox")
        , ("M-a f", spawn "/usr/bin/pcmanfm")
        , ("M-a k", spawn "/usr/bin/kodi")
        , ("M-a q", spawn "/usr/bin/virt-manager")

    -- Org TODO prompt
        , ("M-C-o", orgPrompt def "TODO" "~/Org/todos.org")

    -- Grid Select
        , ("M-g", spawnSelected def ["kitty","chromium","qutebrowser","pcmanfm","xterm","emacs","firefox","lxappearance","sxiv","virtualbox","termite","qt5ct","gimp"])

    -- Take a Screenshot
        , ("<Print>", spawn $ "scrot 'archlinux-%Y-%m-%d-%s_screenshot_$wx%h.jpg' -e 'mv $f /home/andrew/Images/screenshots'")

    -- Paste X-selection buffer
        , ("<Insert>", pasteSelection)

    -- Multimedia Keys
        --, ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        --, ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
        , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
        , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

myPP = xmobarPP { ppCurrent = xmobarColor color15 "" . wrap              -- Current workspace in xmobar
       			("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
       		, ppVisible = xmobarColor color13 ""  -- Visible but not current workspace
       		, ppHidden = xmobarColor color06 ""  -- Hidden workspaces in xmobar
       		, ppHiddenNoWindows = xmobarColor color13 ""  -- Hidden workspaces (no windows)
       		, ppTitle = xmobarColor color16 "" . shorten 60          -- Title of active window in xmobar
       		, ppSep =  "<fc=" ++ color16 ++ "> | </fc>"              -- Separators in xmobar
       		, ppUrgent = xmobarColor color02 "" . wrap "!" "!"       -- Urgent workspace
       		, ppOrder  = \(ws:l:t:ex) -> [ws]++ex++[t]
       		}

mySB = statusBarProp "xmobar" (clickablePP myPP)

main = xmonad . withSB mySB . ewmh . docks $ def
        { manageHook = ( isFullscreen --> doFullFloat ) <+> insertPosition Below Newer <+> myManageHook <+> manageHook def <+> manageDocks
        , logHook            = dynamicLogWithPP xmobarPP
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook <+> setDefaultCursor xC_left_ptr
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = color14
        , focusedBorderColor = color05
        } `additionalKeysP`         myKeys
