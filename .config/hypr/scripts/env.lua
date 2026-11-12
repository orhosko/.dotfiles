-- Environment helpers, used by ../hyprland.lua
-- Loaded with require("scripts.env"); a config reload loads it again.

local M = {}

-- PATH from a clean login shell, or nil if it can't be read.
-- start-hyprland doesn't go through a login shell (niri-session does), so
-- Hyprland only gets PATH=/usr/local/bin:/usr/bin and misses ~/.nix-profile/bin,
-- ~/.local/bin, etc. env -i keeps PATH from growing on every config reload.
function M.loginShellPath()
    local shell = os.getenv("SHELL") or "/bin/sh"
    local cmd = string.format(
        [[env -i HOME="$HOME" USER="$USER" SHELL=%q %q -lc 'printf "\n__PATH__=%%s\n" "$PATH"' 2>/dev/null]],
        shell, shell)
    local handle = io.popen(cmd)
    if not handle then
        return nil
    end

    local output = handle:read("a")
    handle:close()
    return output:match("__PATH__=([^\n]+)")
end

return M
