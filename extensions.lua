function jid_escape(s)
	-- TODO: the class for escaping backslash is overbroad at the moment
	return s
		:gsub("\\([2345][0267face0c])", "\\5c%1")
		:gsub(" ", "\\20")
		:gsub("\"", "\\22")
		:gsub("&", "\\26")
		:gsub("'", "\\27")
		:gsub("/", "\\2f")
		:gsub(":", "\\3a")
		:gsub("<", "\\3c")
		:gsub(">", "\\3e")
		:gsub("@", "\\40")
end

function make_jid(extension, from_header)
	return (
		jid_escape(extension)
		.. "@sip.cheogram.com/"
		.. jid_escape(from_header:gsub("^[^<]*<sip:", ""):gsub(">.*$", ""))
	):gsub("\\", "\\\\")
end

extensions = {
	public = {
		["_X!"] = function(context, extension)
			app.log("NOTICE", "Call from '' (" .. channel.CHANNEL("peerip"):get() .. ":0) to extension '" .. extension .. "' rejected because extension not found in context 'public'.")
			app.goto("i", 1)
		end;

		["i"] = function(context, extension)
			app.goto("default", "i", 1)
		end;

		["h"] = function(context, extension)
			app.goto("default", "h", 1)
		end;

		["_."] = function(context, extension)
			if channel.CHANNEL("channeltype"):get() == "Message" then
				local jid = make_jid(extension, channel.MESSAGE("from"):get())

				app.MessageSend("xmpp:" .. jid, "xmpp:asterisk")
			else
				local jid = make_jid(extension, channel.SIP_HEADER("From"):get())

				app.dial("Motif/jingle-endpoint/" .. jid)
			end
		end;
	};
}
