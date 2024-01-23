function textBase10Decode(digits)
	if digits:sub(0, 2) == "99" then
		result = ""
		for i = 3,digits:len(),3
		do
			result = result .. string.char(tonumber(digits:sub(i, i+2)))
		end
		return result
	else
		result = ""
		for i = 1,digits:len(),2
		do
			result = result .. string.char(tonumber(digits:sub(i, i+1)) + 30)
		end
		return result
	end
end

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

function jid_unescape(s)
	return s
		:gsub("\\20", " ")
		:gsub("\\22", "\"")
		:gsub("\\26", "&")
		:gsub("\\27", "'")
		:gsub("\\2f", "/")
		:gsub("\\3a", ":")
		:gsub("\\3c", "<")
		:gsub("\\3e", ">")
		:gsub("\\40", "@")
		:gsub("\\5c", "\\")
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
		["i"] = function(context, extension)
			app.goto("default", "i", 1)
		end;

		["h"] = function(context, extension)
			app.goto("default", "h", 1)
		end;

		["_."] = function(context, extension)
			local from = channel.SIP_HEADER("From"):get()
			channel.original_extension = extension

			if not extension:match("[^0-9]") then
				from = from:gsub("^[^<]*<sip:", ""):gsub(">.*$", "")
				extension = textBase10Decode(extension)
				if from:match("^%+?[0-9]*@") then
					if not extension:match("@cheogram%.com$") then
						extension = jid_escape(extension) .. "@cheogram.com"
					end
					if from:byte(1) ~= 43 then
						from = "+" .. from
					end
				end
				from = "<sip:" .. from .. ">"
			end

			if not extension:find("%.") then
				app.log("NOTICE", "Call from '' (" .. channel.CHANNEL("peerip"):get() .. ":0) to extension '" .. extension .. "' rejected because extension not found in context 'public'.")
				app.goto("i", 1)
				return
			end

			if channel.CHANNEL("channeltype"):get() == "Message" then
				local jid = make_jid(extension, channel.MESSAGE("from"):get())

				app.MessageSend("xmpp:" .. jid, "xmpp:asterisk")
				app.hangup()
			else
				local jid = make_jid(extension, from)

				app.dial("Motif/jingle-endpoint/" .. jid, 300, "r")
			end
		end;
	};

	jingle = {
		["jingle-endpoint"] = function(context, extension)
			local jid = channel.CALLERID("name"):get()
			local from = jid_unescape(jid:sub(0, jid:find("@") - 1))
			local to = jid_unescape(jid:sub(jid:find("/") + 1))
			-- outbound calls
			channel.CALLERID("all"):set(from .. "<" .. from .. ">")
			app.dial("SIP/" .. to:gsub("\\", "\\\\"):gsub("&", "") .. "!!" .. from:gsub("\\", "\\\\") .. "@sip.cheogram.com")
		end;
	};

	xmpp = {
		["s"] = function(context, extension)
			local jid = channel.MESSAGE("from"):get():sub(6)
			local from = jid_unescape(jid:sub(0, jid:find("@") - 1))
			local to = jid_unescape(jid:sub(jid:find("/") + 1))
			app.MessageSend("sip:" .. to, from .. "<sip:" .. channel.URIENCODE(from):get() .. "@sip.cheogram.com>")
		end;
	};
}
