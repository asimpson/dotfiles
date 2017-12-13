choices =  {
   {
      ['text'] = 'Toggle Zoom',
      ['func'] = 'toggleZoomMute'},
   {
      ['text'] = 'Toggle Caffeine',
      ['func'] = 'toggleCaffine'
   },
   {
      ['text'] = 'Org capture',
      ['func'] = 'capture'
   },
   {
      ['text'] = 'Emacs frame',
      ['func'] = 'frame'
   },
   {
      ['text'] = 'View Desktop in Alfred',
      ['func'] = 'desktop'
   }
}

function frame()
   os.execute("/usr/local/bin/emacsclient -c -n &")
end

function capture()
   os.execute("/usr/local/bin/emacsclient -c -n -e '(org-capture)' &")
   hs.timer.doAfter(0.2, function() hs.application.find('Emacs'):findWindow("*Org Select*"):focus() end)
end

function toggleZoomMute()
    hs.application.launchOrFocus("zoom.us")
    local zoom = hs.appfinder.appFromName("zoom.us")

    local muteAudio = {"Meeting", "Mute Audio"}
    local unMuteAudio = {"Meeting", "Unmute Audio"}
    -- close the extra zoom window
    local close = {"Window", "Close"}
    local free = {"Window", "Zoom - Free Account"}

    local mute = zoom:findMenuItem(muteAudio)
    local unmute = zoom:findMenuItem(unMuteAudio)
    local freeWindow = zoom:findMenuItem(free)

    if (freeWindow) then
        zoom:selectMenuItem(free)
        zoom:selectMenuItem(close)
    end
    if (mute) then
        zoom:selectMenuItem(muteAudio)
    end
    if (unmute) then
        zoom:selectMenuItem(unMuteAudio)
    end
end

function toggleCaffine()
   local state = hs.caffeinate.toggle('displayIdle')
   if (state) then
    hs.alert.show('caffeinated')
   else
    hs.alert.show('decaffeinated')
   end
end

function chooser()
   local choose = hs.chooser.new(function(selection)
         if (selection) then
          _G[selection['func']]()
         end
   end)
   choose:choices(choices)
   choose:rows(3)
   choose:width(30)
   choose:show()
end

function desktop()
  hs.eventtap.keyStroke('cmd', 'space')
  hs.eventtap.keyStrokes('~desktop')
  hs.eventtap.keyStroke('cmd', hs.keycodes.map['down'])
end
hs.hotkey.bind({"cmd, shift"}, ".", chooser)
