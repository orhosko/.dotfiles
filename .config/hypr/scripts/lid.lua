-- Lid helpers, used by ../hyprland.lua
-- Loaded with require("scripts.lid"); a config reload loads it again.

local M = {}

-- True when the lid is closed and a non-internal display is connected.
-- Hyprland doesn't turn the laptop screen off itself, and logind ignores the lid when docked.
function M.closedWithExternalDisplay()
    -- Prints "yes" only when both are true.
    -- Uses the output because os.execute always fails inside Hyprland ("No child processes").
    local check = io.popen([[
        grep -q closed /proc/acpi/button/lid/*/state &&
        grep -l '^connected' /sys/class/drm/card*-*/status | grep -qv eDP &&
        echo yes
    ]])
    if not check then
        return false
    end

    local output = check:read("a")
    check:close()
    return output:find("yes") ~= nil
end

return M
