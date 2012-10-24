require("awful")
require("awful.autofocus")
require("awful.rules")

require("beautiful")

require("naughty")

vicious = require("vicious")

-- {{{ Variable definitions
-- Load theme.
beautiful.init("/home/afronski/.config/awesome/themes/theme.lua")

-- Set terminal name.
terminal = "urxvt"

-- Modifier is a Windows Key.
modkey = "Mod4"

-- Set layout order.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating
}
-- }}}

-- {{{ Tags
tags = 	{
		names  = { "➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑" }, 
	  	layout = { layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
       	}

for s = 1, screen.count() do
	tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
mymainmenu = awful.menu({ 
				items = {
                   	{ " terminal", terminal },
					{ " chromium", "chromium" }, 
					{ " canary", "google-chrome" }, 
					{ " firefox", "firefox" }, 
					{ " fx-nightly", "firefox-nightly" }, 
					{ " opera", "opera" }, 
					{ " opera-next", "opera-next" },
					{ " skype", "skype" },
					{ " pidgin", "pidgin" },
					{ " pcmamfm", "pcmanfm" },
					{ " xarchiver", "xarchiver" },
					{ " gimp", "gimp" },	
					{ " ncmpcpp", "urxvt -e ncmpcpp" },
					{ " evince", "evince" },
					{ " calibre", "calibre" },		
					{ " nixnote", "nixnote" },
					{ " deluge", "deluge" },
					{ " sublime", "subl" },
					{ " eclipse", "run-eclipse.sh" },
                               	},
				width = 150,
				height = 10
			  
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon), menu = mymainmenu })
-- }}}

-- Separators
spacer = widget({ type = "textbox" })
seperator = widget({ type = "textbox" })
dash = widget({ type = "textbox" })
spacer.text = " "
seperator.text = "|"
dash.text = "-"

-- {{{ Wibox

-- MPD Widget
mpdicon = widget({ type = "imagebox" })
mpdicon.image = image("/usr/share/awesome/themes/icons/him/mpd.png")
mpdwidget = widget({ type = "textbox" })
vicious.register(mpdwidget, vicious.widgets.mpd, "<span color='#FFFFFF'> ♪</span> ${Title}, ${Artist}", 13)

-- Volume widget
volumewidget = widget({ type = "textbox" })
vicious.register(volumewidget, vicious.widgets.volume,
		 function(widget, args)
			return "<span color='#FFFFFF'>" ..  args[2] .. "</span>" .. " (" .. args[1] .. "%)"
		 end,
		 2, "Master")

-- HDD temperature widget.
hddtempwidget = widget({ type = "textbox" })
vicious.register(hddtempwidget, vicious.widgets.hddtemp, "HDD: ${/dev/sda}°C", 19)

--Seperator
spicon = widget({ type = "imagebox" })
spicon.image = image("/usr/share/awesome/themes/icons/him/separator.png")

-- Memory widget.
memwidget = widget({ type = "textbox" })
vicious.cache(vicious.widgets.mem)
vicious.register(memwidget, vicious.widgets.mem, "$1% ($2MB/4GB)", 10)

-- Create a netwidget (usage)
dnicon = widget({ type = "imagebox" })
upicon = widget({ type = "imagebox" })
dnicon.image = image("/usr/share/awesome/themes/icons/him/down.png")
upicon.image = image("/usr/share/awesome/themes/icons/him/up.png")
netwidget = widget({ type = "textbox" })
vicious.register(netwidget, vicious.widgets.net, "${eth0 down_kb} / ${eth0 up_kb}", 1)

-- Create a battery widget
baticon = widget({ type = "imagebox" })
baticon.image = image("/usr/share/awesome/themes/icons/him/bat.png")
batwidget = widget({ type = "textbox" })

--Create a cpuwidget
vicious.register(batwidget, vicious.widgets.bat, " $1$2", 32, "BAT1")
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image("/usr/share/awesome/themes/icons/him/cpuinfo.png")

cpuwidget = widget({ type = "textbox" })
vicious.register(cpuwidget, vicious.widgets.cpu, "$1%", 2)

cpufreqwidget = widget({ type = "textbox" })
vicious.register(cpufreqwidget, vicious.widgets.cpufreq, "$2GHz", 2, "cpu0")

cputempwidget = widget({ type = "textbox" })
vicious.register(cputempwidget, vicious.widgets.thermal, "$1°C", 2, { "coretemp.0", "core" })

-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Google Calendar widget
local gcal = nil
local cal = nil

function remove_gcal()
    if gcal~= nil then
        naughty.destroy(gcal)
        gcal = nil
    end

    if cal~= nil then
	    naughty.destroy(cal)
	    cal = nil
    end
end

function add_gcal()
    remove_gcal()

    local calinfo = awful.util.pread("cal")
    local gcalcinfo = awful.util.pread("gcalcli --nc agenda")

    gcalcinfo = string.gsub(gcalcinfo, "%$(%w+)", "%1")
    calinfo = string.gsub(calinfo, "%$(%w+)", "%1")

    cal = naughty.notify({ text = calinfo, timeout = 0 })
    gcal = naughty.notify({ text = gcalcinfo, timeout = 0 })
end

-- mytextclock:add_signal("mouse::enter", add_gcal)
-- mytextclock:add_signal("mouse::leave", remove_gcal)

-- CoveraArt tooltip
local coverart_nf = nil

function coverart_show()
    -- Destroy old popup, needed when bound to a key.
    coverart_hide()
    local img = awful.util.pread("/home/afronski/.coverart/coverart.sh")
    local ico = image(img)
    local txt = awful.util.pread("/home/afronski/.coverart/musicinfo.sh")

    -- Set desired position of popup during creation.
    coverart_nf = naughty.notify({icon = ico, icon_size = 100, text = txt, position = "bottom_left"})
end

function coverart_hide()
    if coverart_nf ~= nil then
	    naughty.destroy(coverart_nf)
        coverart_nf = nil
    end
end

-- mpdwidget:add_signal("mouse::enter", function() coverart_show() end)
-- mpdwidget:add_signal("mouse::leave", function() coverart_hide() end)

-- Create a wibox for each screen and add it.
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}

mytaglist.buttons = awful.util.table.join(
                    	awful.button({ }, 1, awful.tag.viewonly),
                    	awful.button({ modkey }, 1, awful.client.movetotag),
                    	awful.button({ }, 3, awful.tag.viewtoggle),
                    	awful.button({ modkey }, 3, awful.client.toggletag),
                    	awful.button({ }, 4, awful.tag.viewnext),
                    	awful.button({ }, 5, awful.tag.viewprev)
                    )

mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Set a screen margin for borders.
    awful.screen.padding( screen[s], { top = 0 } )

    -- Create a promptbox for each screen.
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright, prompt = "" })

    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))

    -- Create a taglist widget.
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget.
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox.
    mywibox[s] = awful.wibox({ position = "top", screen = s, border_width = 0, border_color = "#FFFFFF" })

    -- Add widgets to the wibox - order matters.
    mywibox[s].widgets = {
        {
            mytaglist[s], spacer, mpdwidget, spacer,
	    mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        
        s == 1 and mysystray or nil, mytextclock,
        seperator, spacer, 
	cpufreqwidget, spacer,
	cputempwidget, spacer,
	cpuwidget, spacer,
	cpuicon, spacer, seperator, spacer,
	memwidget, spacer, seperator, 
	upicon, netwidget, 
	dnicon, seperator, spacer, 
	volumewidget, spacer, seperator, spacer, 
	hddtempwidget, spacer, seperator, 
	spacer, batwidget, baticon,
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey }, "slash", 
        function () 
            if coverart_nf ~= nil then
                coverart_hide()
            else
                coverart_show() 
            end
        end),
    awful.key({ modkey }, "c", 
        function () 
            if gcal ~= nil then
                remove_gcal()
            else
                add_gcal() 
            end
        end),

    awful.key({ modkey,           }, ",",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),

    awful.key({ modkey,           }, ".",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    awful.key({ modkey,           }, "w", function () mymainmenu:show({ keygrabber = true }) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),

    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()

            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ "Control"         }, "l",     function () awful.util.spawn("slock") end),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),

    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),

    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),

    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey, "Control" }, "Next",  function () awful.client.moveresize(0, 0, -5, 0) end),
    awful.key({ modkey, "Control" }, "Prior", function () awful.client.moveresize(0, 0,  5, 0) end),

    awful.key({ modkey, "Control", "Shift" }, "Next",  function () awful.client.moveresize(0, 0, 0, -5) end),
    awful.key({ modkey, "Control", "Shift" }, "Prior", function () awful.client.moveresize(0, 0, 0,  5) end),

    awful.key({ modkey, "Control" }, "Down",  function () awful.client.moveresize(  0,  5,   0,   0) end),
    awful.key({ modkey, "Control" }, "Up",    function () awful.client.moveresize(  0, -5,   0,   0) end),
    awful.key({ modkey, "Control" }, "Left",  function () awful.client.moveresize( -5,  0,   0,   0) end),
    awful.key({ modkey, "Control" }, "Right", function () awful.client.moveresize(  5,  0,   0,   0) end),

    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),

    awful.key({ modkey   	  }, "F4",     function (c) c:kill()                         end),

    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),

    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),

    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),

    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { 
	rule = { },
      	properties = {
			border_width = beautiful.border_width,
                     	border_color = beautiful.border_normal,
		     	size_hints_honor = false,
                     	focus = true,
                     	keys = clientkeys,
                     	buttons = clientbuttons 
		     } 
    },

    -- Flash container rule.
    { rule = { class = "Exe" }, properties = { floating = true } },

    { rule = { class = "MPlayer" }, properties = { floating = true } },
    { rule = { class = "feh" }, properties = { floating = true } },
    { rule = { class = "Xchm" }, properties = { floating = true } },
    { rule = { class = "Evince" }, properties = { floating = true } },

    { rule = { class = "Chromium" }, callback = function(class) awful.client.movetotag(tags[1][2], class) end },
    
    { rule = { class = "sublime-text" }, callback = function(class) awful.client.movetotag(tags[1][3], class) end },

    { rule = { class = "Pcmanfm" }, callback = function(class) awful.client.movetotag(tags[1][4], class) end },

    { rule = { class = "Firefox" }, callback = function(class) awful.client.movetotag(tags[1][8], class) end },
    { rule = { class = "Opera" }, callback = function(class) awful.client.movetotag(tags[1][8], class) end },
    { rule = { class = "Google-chrome" }, callback = function(class) awful.client.movetotag(tags[1][4], class) end },

    -- Nixnote rule.
    { 
	rule = { class = "Qt Jambi application" }, 
	callback = function(class) awful.client.movetotag(tags[1][5], class) end, 
	properties = { floating = true } 
    },
    { 
	rule = { class = "Deluge" }, 
	callback = function(class) awful.client.movetotag(tags[1][5], class) end, 
	properties = { floating = true } 
    },
    {
	rule = { class = "Gimp" },
	callback = function(class) awful.client.movetotag(tags[1][5], class) end, 
	properties = { floating = true } 
    },
    {
	rule = { class = "Pidgin" },
	callback = function(class) awful.client.movetotag(tags[1][6], class) end, 
	properties = { floating = true } 
    },
    { 	
	rule = { class = "Skype" }, 
	callback = function(class) awful.client.movetotag(tags[1][7], class) end,
	properties = { floating = true }
    }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- {{{ Tag signal handler - selection
-- - ASCII tags 1 [2] 3 4...
--   - start with tag 1 named [1] in tag setup
for s = 1, screen.count() do
    for t = 1, #tags[s] do
        tags[s][t]:add_signal("property::selected", function ()
           if tags[s][t].selected then
                tags[s][t].name = "[" .. tags[s][t].name .. "]"
            else
                tags[s][t].name = tags[s][t].name:gsub("[%[%]]", "")
            end
        end)
    end
end
-- }}}

--- {{{ Add titlebar to floating apps

client.add_signal("manage", function (c, prop)
  -- Remove the titlebar if fullscreen
  if c.fullscreen then
     awful.titlebar.remove(c)
  elseif not c.fullscreen then
    -- Add title bar for floating apps
    if c.titlebar == nil and awful.client.floating.get(c) then
       awful.titlebar.add(c, { modkey = modkey })
    -- Remove title bar, if it's not floating
    elseif c.titlebar and not awful.client.floating.get(c) then
       awful.titlebar.remove(c)
    end
  end
end)

--- }}}
