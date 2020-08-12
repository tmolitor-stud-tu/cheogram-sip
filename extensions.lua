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

extensions = {
	public = {
		["_X!"] = function(context, extension)
			app.hangup(1)
		end;

		["_[a-z]"] = function(context, extension)
			app.hangup(1)
		end;

		["_[a-z][a-z]"] = function(context, extension)
			app.hangup(1)
		end;

		["_."] = function(context, extension)
			local jid = (
				jid_escape(extension)
				.. "@sip.cheogram.com/"
				.. jid_escape(channel.SIP_HEADER("From"):get():gsub("^[^<]*<sip:", ""):gsub(">.*$", ""))
			):gsub("\\", "\\\\")

			app.dial(
				"Motif/jingle-endpoint/" .. jid
			)
		end;
	};
}
