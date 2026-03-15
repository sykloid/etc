local wezterm = require("wezterm")

local config = wezterm.config_builder()

local font = wezterm.font("Iosevka Custom Term", { weight = "Medium" })

-- Font
config.font = font
config.dpi = 140
config.freetype_load_target = "Light"

-- Colors
config.color_scheme = "Ayu Mirage"
config.colors = {
  background = "#0A0A0A",
}

-- Window
config.window_decorations = "RESIZE"
config.window_padding = { bottom = 0, top = 0, left = 0, right = 0 }

-- Tab bar
config.use_fancy_tab_bar = false
config.tab_max_width = 128

-- Quick select
config.quick_select_patterns = {
  "[0-9a-f]{7,40}",
  "[a-zA-Z0-9][a-zA-Z0-9-]+",
}

-- Per-screen overrides

local screen_overrides = {
  ["Built-in Retina Display"] = {
    font_size = 11,
    command_palette_font_size = 11,
    char_select_font_size = 11,
    window_frame = { font = font, font_size = 11 },
  },
  ["Odyssey G95NC"] = {
    font_size = 7,
    command_palette_font_size = 7,
    char_select_font_size = 7,
    window_frame = { font = font, font_size = 7 },
  },
}

wezterm.on("window-config-reloaded", function(window)
  local screen = wezterm.gui.screens().active
  local overrides = screen_overrides[screen.name]
  if overrides then
    window:set_config_overrides(overrides)
  end
end)

-- Keybindings
config.leader = { key = "Space", mods = "CTRL", timeout_milliseconds = 1000 }

local act = wezterm.action

config.keys = {
  -- Pane navigation
  { key = "n", mods = "LEADER", action = act.ActivatePaneDirection("Left") },
  { key = "e", mods = "LEADER", action = act.ActivatePaneDirection("Down") },
  { key = "i", mods = "LEADER", action = act.ActivatePaneDirection("Up") },
  { key = "o", mods = "LEADER", action = act.ActivatePaneDirection("Right") },

  -- Pane management
  { key = "s", mods = "LEADER", action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
  { key = "v", mods = "LEADER", action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
  { key = "x", mods = "LEADER", action = act.CloseCurrentPane({ confirm = true }) },

  -- Selection
  { key = "d", mods = "LEADER", action = act.QuickSelect },

  -- General
  { key = "p", mods = "SUPER", action = act.ActivateCommandPalette },
}

config.key_tables = {
  copy_mode = {
    -- Movement (Colemak neio)
    { key = "n", mods = "NONE", action = act.CopyMode("MoveLeft") },
    { key = "e", mods = "NONE", action = act.CopyMode("MoveDown") },
    { key = "i", mods = "NONE", action = act.CopyMode("MoveUp") },
    { key = "o", mods = "NONE", action = act.CopyMode("MoveRight") },

    -- Page movement
    { key = "E", mods = "SHIFT", action = act.CopyMode("PageDown") },
    { key = "I", mods = "SHIFT", action = act.CopyMode("PageUp") },

    -- Word movement
    { key = "w", mods = "NONE", action = act.CopyMode("MoveForwardWord") },
    { key = "b", mods = "NONE", action = act.CopyMode("MoveBackwardWord") },
    { key = "f", mods = "NONE", action = act.CopyMode("MoveForwardWordEnd") },

    -- Line movement
    { key = "N", mods = "SHIFT", action = act.CopyMode("MoveToStartOfLineContent") },
    { key = "O", mods = "SHIFT", action = act.CopyMode("MoveToEndOfLineContent") },
    { key = "0", mods = "NONE", action = act.CopyMode("MoveToStartOfLine") },

    -- Viewport
    { key = "H", mods = "SHIFT", action = act.CopyMode("MoveToViewportTop") },
    { key = "M", mods = "SHIFT", action = act.CopyMode("MoveToViewportMiddle") },
    { key = "L", mods = "SHIFT", action = act.CopyMode("MoveToViewportBottom") },

    -- Scrollback
    { key = "g", mods = "NONE", action = act.CopyMode("MoveToScrollbackTop") },
    { key = "G", mods = "SHIFT", action = act.CopyMode("MoveToScrollbackBottom") },

    -- Selection
    { key = "v", mods = "NONE", action = act.CopyMode({ SetSelectionMode = "Cell" }) },
    { key = "V", mods = "SHIFT", action = act.CopyMode({ SetSelectionMode = "Line" }) },
    { key = "v", mods = "CTRL", action = act.CopyMode({ SetSelectionMode = "Block" }) },

    -- Search
    { key = "/", mods = "NONE", action = act.Search({ CaseInSensitiveString = "" }) },

    -- Jump (find character)
    { key = "j", mods = "NONE", action = act.CopyMode({ JumpForward = { prev_char = false } }) },
    { key = "J", mods = "SHIFT", action = act.CopyMode({ JumpBackward = { prev_char = false } }) },
    { key = ";", mods = "NONE", action = act.CopyMode("JumpAgain") },
    { key = ",", mods = "NONE", action = act.CopyMode("JumpReverse") },

    -- Copy and exit
    { key = "k", mods = "NONE", action = act.Multiple({
      act.CopyTo("ClipboardAndPrimarySelection"),
      act.CopyMode("Close"),
    }) },

    -- Exit
    { key = "Escape", mods = "NONE", action = act.CopyMode("Close") },
    { key = "q", mods = "NONE", action = act.CopyMode("Close") },
  },
}

return config
