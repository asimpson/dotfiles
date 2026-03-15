--# selene: allow(unused_variable)
---@diagnostic disable: unused-local

-- Simple window movement and resizing, focusing on half- and third-of-screen sizes
--
-- Download: [https://github.com/Hammerspoon/Spoons/raw/master/Spoons/WindowHalfsAndThirds.spoon.zip](https://github.com/Hammerspoon/Spoons/raw/master/Spoons/WindowHalfsAndThirds.spoon.zip)
---@class spoon.WindowHalfsAndThirds
local M = {}
spoon.WindowHalfsAndThirds = M

-- Binds hotkeys for WindowHalfsAndThirds
--
-- Parameters:
--  * mapping - A table containing hotkey objifier/key details for the following items:
--   * left_half, right_half, top_half, bottom_half - resize to the corresponding half of the screen
--   * third_left, third_right - resize to one horizontal-third of the screen and move left/right
--   * third_up, third_down - resize to one vertical-third of the screen and move up/down
--   * max - maximize the window
--   * max_toggle - toggle maximization
--   * left_third, middle_third_h, right_third - resize and move the window to the corresponding horizontal third of the screen
--   * top_third, middle_third_v, bottom_third - resize and move the window to the corresponding vertical third of the screen
--   * top_left, top_right, bottom_left, bottom_right - resize and move the window to the corresponding quarter of the screen
--   * undo - restore window to position before last move
--   * center - move window to center of screen
--   * larger - grow window larger than its current size
--   * smaller - shrink window smaller than its current size
--
-- Returns:
--  * the WindowHalfsAndThirds object
function M:bindHotkeys(mapping, ...) end

-- Center window on screen
--
-- Parameters:
--  * win - hs.window to use, defaults to hs.window.focusedWindow()
--
-- Returns:
--  * the WindowHalfsAndThirds object
function M:center(win, ...) end

-- We don't want our undo frame cache filling all available memory. Let's clear it after it hasn't been used for a while.
M.clear_cache_after_seconds = nil

-- Table containing a sample set of hotkeys that can be
-- assigned to the different operations. These are not bound
-- by default - if you want to use them you have to call:
-- `spoon.WindowHalfsAndThirds:bindHotkeys(spoon.WindowHalfsAndThirds.defaultHotkeys)`
-- after loading the spoon. Value:
-- ```
--  {
--     left_half   = { {"ctrl",        "cmd"}, "Left" },
--     right_half  = { {"ctrl",        "cmd"}, "Right" },
--     top_half    = { {"ctrl",        "cmd"}, "Up" },
--     bottom_half = { {"ctrl",        "cmd"}, "Down" },
--     third_left  = { {"ctrl", "alt"       }, "Left" },
--     third_right = { {"ctrl", "alt"       }, "Right" },
--     third_up    = { {"ctrl", "alt"       }, "Up" },
--     third_down  = { {"ctrl", "alt"       }, "Down" },
--     top_left    = { {"ctrl",        "cmd"}, "1" },
--     top_right   = { {"ctrl",        "cmd"}, "2" },
--     bottom_left = { {"ctrl",        "cmd"}, "3" },
--     bottom_right= { {"ctrl",        "cmd"}, "4" },
--     max_toggle  = { {"ctrl", "alt", "cmd"}, "f" },
--     max         = { {"ctrl", "alt", "cmd"}, "Up" },
--     undo        = { {        "alt", "cmd"}, "z" },
--     center      = { {        "alt", "cmd"}, "c" },
--     larger      = { {        "alt", "cmd", "shift"}, "Right" },
--     smaller     = { {        "alt", "cmd", "shift"}, "Left" },
--  }
-- ```
M.defaultHotkeys = nil

-- Make win larger than its current size
--
-- Parameters:
--  * win - hs.window to use, defaults to hs.window.focusedWindow()
--
-- Returns:
--  * the WindowHalfsAndThirds object
function M:larger(win, ...) end

-- Resize to the left half of the screen.
--
-- Parameters:
--  * win - hs.window to use, defaults to hs.window.focusedWindow()
--
-- Returns:
--  * the WindowHalfsAndThirds object
--
-- Notes:
--  * Variations of this method exist for other operations. See WindowHalfsAndThirds:bindHotkeys for details:
--    * .leftHalf .rightHalf .topHalf .bottomHalf .thirdLeft .thirdRight .leftThird .middleThirdH .rightThird
--    * .thirdUp .thirdDown .topThird .middleThirdV .bottomThird .topLeft .topRight .bottomLeft .bottomRight
--    * .maximize
function M:leftHalf(win, ...) end

-- Logger object used within the Spoon. Can be accessed to set the default log level for the messages coming from the Spoon.
M.logger = nil

-- Make win smaller than its current size
--
-- Parameters:
--  * win - hs.window to use, defaults to hs.window.focusedWindow()
--
-- Returns:
--  * the WindowHalfsAndThirds object
function M:smaller(win, ...) end

-- Toggle win between its normal size, and being maximized
--
-- Parameters:
--  * win - hs.window to use, defaults to hs.window.focusedWindow()
--
-- Returns:
--  * the WindowHalfsAndThirds object
function M:toggleMaximized(win, ...) end

-- Undo window size changes for win if there've been any in WindowHalfsAndThirds.clear_cache_after_seconds
--
-- Parameters:
--  * win - hs.window to use, defaults to hs.window.focusedWindow()
--
-- Returns:
--  * the WindowHalfsAndThirds object
function M:undo(win, ...) end

-- If `true`, set [setFrameCorrectness](http://www.hammerspoon.org/docs/hs.window.html#setFrameCorrectness) for some resizing operations which fail when the window extends beyonds screen boundaries. This may cause some jerkiness in the resizing, so experiment and determine if you need it. Defaults to `false`
M.use_frame_correctness = nil

