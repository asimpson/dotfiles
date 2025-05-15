-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Dracula'
config.font = wezterm.font 'Hack'
config.font_size = 14.0
--config.enable_tab_bar = false
config.default_cwd = wezterm.home_dir
config.keys = {
    -- Override the default Ctrl+Shift+n (or CMD+n on macOS) shortcut
    {
      key = 'n',
      mods = 'CMD', -- Use 'CMD' instead on macOS
      action = wezterm.action.SpawnCommandInNewWindow {
        cwd = wezterm.home_dir
      }
    },
  }

-- Tab title customization to include current working directory
wezterm.on('format-tab-title', function(tab, tabs, panes, config, hover, max_width)
    local pane = tab.active_pane

    -- Get CWD
    local cwd_uri = pane.current_working_dir
    local cwd = cwd_uri.file_path or cwd_uri.path or ""
    if cwd_uri then
        -- Get just the last directory name
        cwd = cwd:gsub("^.*[/\\]([^/\\]+)[/\\]?$", "%1")
    end

    -- Get process name
    local process = pane.foreground_process_name or ""
    -- Format title: "directory - process"
    return cwd .. " - " .. process
end)

-- and finally, return the configuration to wezterm
return config
