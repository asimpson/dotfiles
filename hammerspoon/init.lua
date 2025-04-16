hs.loadSpoon("WindowHalfsAndThirds")
spoon.WindowHalfsAndThirds:bindHotkeys(spoon.WindowHalfsAndThirds.defaultHotkeys)

hs.hotkey.bind({"cmd", "shift", "ctrl"}, "left", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen():frame()
  
  -- Calculate distance to screen edge
  local distanceToEdge = f.x - screen.x
  
  -- Move by 15px or to edge, whichever is smaller
  local moveAmount = math.min(50, distanceToEdge)
  
  -- Only move if there's space
  if moveAmount > 0 then
    f.x = f.x - moveAmount
    win:setFrame(f)
  end
end)

-- Move current window right by 15px or until screen edge
hs.hotkey.bind({"cmd", "shift", "ctrl"}, "right", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen():frame()
  
  -- Calculate distance to screen edge
  local distanceToEdge = screen.x + screen.w - (f.x + f.w)
  
  -- Move by 15px or to edge, whichever is smaller
  local moveAmount = math.min(50, distanceToEdge)
  
  -- Only move if there's space
  if moveAmount > 0 then
    f.x = f.x + moveAmount
    win:setFrame(f)
  end
end)
