-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action

-- This will hold the configuration.
local config = wezterm.config_builder()
local modkey = 'ALT'

config.color_scheme = 'Dracula'
config.font = wezterm.font 'Hack'
config.font_size = 12.0
config.enable_tab_bar = false
config.default_cwd = wezterm.home_dir
config.scrollback_lines = 20000
config.inactive_pane_hsb = {
  saturation = 0.4,
  --brightness = 0.1,
}
config.keys = {
    -- Override the default Ctrl+Shift+n (or CMD+n on macOS) shortcut
    {
      key = 'n',
      mods = 'CMD', -- Use 'CMD' instead on macOS
      action = act.SpawnCommandInNewWindow {
        cwd = wezterm.home_dir
      }
    },
    --i3 pane movement
    {
      key = 'h',
      mods = modkey,
      action = act.ActivatePaneDirection 'Left',
    },
    {
      key = 'l',
      mods = modkey,
      action = act.ActivatePaneDirection 'Right',
    },
    {
      key = 'k',
      mods = modkey,
      action = act.ActivatePaneDirection 'Up',
    },
    {
      key = 'j',
      mods = modkey,
      action = act.ActivatePaneDirection 'Down',
    },
    {
      key = 'f',
      mods = modkey,
      action = act.TogglePaneZoomState,
    },
    --easier new pane combo
    {
      key = '\\',
      mods = modkey,
      action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    {
      key = '-',
      mods = modkey,
      action = act.SplitVertical { domain = 'CurrentPaneDomain' },
    },
    {
        key = ',',
        mods = modkey,
        action = act.SpawnCommandInNewTab {
          cwd = os.getenv('WEZTERM_CONFIG_DIR'),
          set_environment_variables = {
            PATH = os.getenv('PATH') .. ":/Users/adam/.nix-profile/bin/",
          },
          args = {
            'vim',
            os.getenv('WEZTERM_CONFIG_FILE'),
          },
        },
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
