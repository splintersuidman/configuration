# Autogenerated config.py
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'file://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# Padding (in pixels) around text for tabs.
# Type: Padding
c.tabs.padding = { 'bottom': 5, 'left': 5, 'right': 5, 'top': 5 }

# Page to open if :open -t/-b/-w is used without URL. Use `about:blank`
# for a blank page.
# Type: FuzzyUrl
c.url.default_page = 'https://duckduckgo.com'

# Page(s) to open at the start.
# Type: List of FuzzyUrl, or FuzzyUrl
c.url.start_pages = 'https://duckduckgo.com'

# Hide the window decoration.  This setting requires a restart on
# Wayland.
# Type: Bool
c.window.hide_decoration = True

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
# Type: List of QtColor, or QtColor
c.colors.completion.fg = '#d5c4a1'

# Background color of the completion widget for odd rows.
# Type: QssColor
c.colors.completion.odd.bg = '#333333'

# Background color of the completion widget for even rows.
# Type: QssColor
c.colors.completion.even.bg = '#333333'

# Background color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.bg = '#8ec07c'

# Top border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.item.selected.border.top = '#1d2021'

# Bottom border color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.border.bottom = '#1d2021'

# Foreground color of the matched text in the completion.
# Type: QssColor
c.colors.completion.match.fg = '#fb4934'

# Foreground color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.fg = '#d5c4a1'

# Background color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.bg = '#1d2021'

# Foreground color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.fg = '#d4c5a1'

# Background color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.bg = '#1d2021'

# Default foreground color of the URL in the statusbar.
# Type: QssColor
c.colors.statusbar.url.fg = '#d5c4a1'

# Foreground color of the URL in the statusbar on error.
# Type: QssColor
c.colors.statusbar.url.error.fg = '#fb4934'

# Foreground color of the URL in the statusbar on successful load
# (http).
# Type: QssColor
c.colors.statusbar.url.success.http.fg = '#83a598'

# Foreground color of the URL in the statusbar on successful load
# (https).
# Type: QssColor
c.colors.statusbar.url.success.https.fg = '#8ec07c'

# Foreground color of the URL in the statusbar when there's a warning.
# Type: QssColor
c.colors.statusbar.url.warn.fg = '#fabd2f'

# Background color of the tab bar.
# Type: QtColor
c.colors.tabs.bar.bg = '#1d2021'

# Foreground color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.fg = '#928374'

# Background color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.bg = '#1d2021'

# Foreground color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.fg = '#928374'

# Background color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.bg = '#1d2021'

# Foreground color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.fg = '#d5c4a1'

# Background color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.bg = '#333333'

# Foreground color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.fg = '#d5c4a1'

# Background color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.bg = '#333333'

# Position of the tab bar.
c.tabs.position = 'bottom'

# Always restore open sites when qutebrowser is reopened.
c.auto_save.session = True

# Fonts.
c.fonts.tabs = '11pt Fira Code'
c.fonts.monospace = 'Fira Code'
