-- Scrolling layout column helpers, used by ../hyprland.lua
-- Loaded with require("scripts.column"); a config reload loads it again
-- (and forgets the remembered widths).

local M = {}

local widthBeforeWiden = {}

-- Like niri's maximize-column: widen to full width, run again to restore the previous width.
function M.toggleWidth()
    local window = hl.get_active_window()
    if not window or window.floating then
        return
    end

    local previous = widthBeforeWiden[window.address]
    if previous then
        widthBeforeWiden[window.address] = nil
        -- Rounded so that repeated toggles don't drift because of gaps
        local ratio = math.floor(previous / window.size.x * 100 + 0.5) / 100
        hl.dispatch(hl.dsp.layout("colresize " .. ratio))
    elseif window.size.x >= window.monitor.width / window.monitor.scale * 0.95 then
        -- Already full width with nothing remembered, e.g. after a config reload
        hl.dispatch(hl.dsp.layout("colresize 0.5"))
    else
        widthBeforeWiden[window.address] = window.size.x
        hl.dispatch(hl.dsp.layout("colresize 1.0"))
    end
end

return M
