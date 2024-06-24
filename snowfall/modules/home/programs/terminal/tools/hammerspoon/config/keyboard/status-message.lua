local drawing = require 'hs.drawing'
local geometry = require 'hs.geometry'
local screen = require 'hs.screen'
local styledtext = require 'hs.styledtext'
local alpha = 1.0
local statusmessage = {}
statusmessage.new = function(messageText)
  local buildParts = function(messageText)
    local frame = screen.primaryScreen():frame()

    local styledTextAttributes = {
      font = { name = 'Monaco', size = 24 },
      color = hs.drawing.color.white
    }

    local styledText = styledtext.new('🔨 ' .. messageText, styledTextAttributes)

    local styledTextSize = drawing.getTextDrawingSize(styledText)
    local textRect = {
      x = frame.w/2 - 40,
      y = frame.h - styledTextSize.h,
      w = styledTextSize.w + 40,
      h = styledTextSize.h + 40,
    }
    local text = drawing.text(textRect, styledText):setAlpha(alpha)

    local background = drawing.rectangle(
      {
        x = 0,
        y = frame.h - styledTextSize.h,
        w = frame.w,
        h = styledTextSize.h + 20
      }
    )
    background:setRoundedRectRadii(10, 10)
    background:setFillColor({ red = 0, green = 0, blue = 0, alpha=alpha})

    return background, text
  end

  return {
    _buildParts = buildParts,
    show = function(self)
      self:hide()

      self.background, self.text = self._buildParts(messageText)
      self.background:show()
      self.text:show()
    end,
    hide = function(self)
      if self.background then
        self.background:delete()
        self.background = nil
      end
      if self.text then
        self.text:delete()
        self.text = nil
      end
    end,
    notify = function(self, seconds)
      local seconds = seconds or 1
      self:show()
      hs.timer.delayed.new(seconds, function() self:hide() end):start()
    end
  }
end

return statusmessage
