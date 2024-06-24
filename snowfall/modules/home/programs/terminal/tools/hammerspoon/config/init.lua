-- hammerspoon init file
require('hs.ipc')
require('keyboard')
local images = require('images')
local yabai = require('yabai')
local hints = require('hs.hints')

--# constants
super = { 'ctrl', 'alt', 'cmd' }
hyper = { 'shift', 'ctrl', 'alt', 'cmd' }
empty_table = {}
windowCornerRadius = 10

local log = hs.logger.new('config', 'debug')
