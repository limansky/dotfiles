local wezterm = require('wezterm')
local act = wezterm.action

local config = wezterm.config_builder()

-- config.default_cursor_style = 'BlinkingBlock'

-- config.color_scheme = 'MaterialOcean'
config.color_scheme = 'Spacegray Eighties (Gogh)'
-- config.color_scheme = 'tokyonight'

config.hide_tab_bar_if_only_one_tab = true
config.enable_scroll_bar = true
config.scrollback_lines = 20000

config.disable_default_key_bindings = true

config.keys = {
  { key = 'LeftArrow', mods = 'SHIFT', action = act.ActivateTabRelative(-1) },
  { key = 'LeftArrow', mods = 'CTRL', action = act.MoveTabRelative(-1) },
  { key = 'RightArrow', mods = 'SHIFT', action = act.ActivateTabRelative(1) },
  { key = 'RightArrow', mods = 'CTRL', action = act.MoveTabRelative(1) },
  { key = 'DownArrow', mods = 'SHIFT', action = act.SpawnTab('DefaultDomain') },
  { key = 'v', mods = 'ALT|CTRL', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }},
  { key = 'h', mods = 'ALT|CTRL', action = act.SplitVertical { domain = 'CurrentPaneDomain' }},
  { key = 'f', mods = 'ALT|CTRL', action = act.Search('CurrentSelectionOrEmptyString')},
  { key = '=', mods = 'ALT|CTRL', action = act.IncreaseFontSize },
  { key = '-', mods = 'ALT|CTRL', action = act.DecreaseFontSize },
  { key = 'v', mods = 'CTRL|SHIFT', action = act.PasteFrom('Clipboard') },
  { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-1) },
  { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(1) },
}
for i = 1, 9 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'ALT',
    action = act.ActivateTab(i - 1),
  })
end

return config
