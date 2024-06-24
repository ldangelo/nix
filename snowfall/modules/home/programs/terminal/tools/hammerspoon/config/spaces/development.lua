if hs.application.pathForBundleID('com.figma.Desktop') then
  table.insert(Config.spaces, {
    text = "Development",
    subText = "Work on development related issues",
    image = hs.image.imageFromAppBundle('org.gnu.Emacs'),
    whitelist = {'design', 'research'},
    togglProj = Config.projects.development,
    intentRequired = true
  })
end
