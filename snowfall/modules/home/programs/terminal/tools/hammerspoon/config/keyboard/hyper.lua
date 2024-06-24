local status, hyperModeAppMappings = pcall(require, 'keyboard.hyper-apps')

function hyperHelp()
  hs.logger.new('hyper'):e('Call for help')
end

if not status then
  hyperModeAppMappings = require('keyboard.hyper-apps-defaults')
end

for i, mapping in ipairs(hyperModeAppMappings) do
  local key = mapping[1]
  local app = mapping[2]
  hs.application.enableSpotlightForNameSearches(true)
  hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, key, function()
    if (type(app) == 'string') then
      hs.application.open(app)
    elseif (type(app) == 'function') then
      app()
    else
      hs.logger.new('hyper'):e('Invalid mapping for Hyper +', key)
    end
  end)
end

