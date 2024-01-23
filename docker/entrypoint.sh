#!/bin/sh

sed -i "s/^serverhost=.*/serverhost=$CONNECT_IP/" /etc/asterisk/xmpp.conf
sed -i "s/^port=.*/port=$CONNECT_PORT/" /etc/asterisk/xmpp.conf
sed -i "s/^username=.*/username=$ASTERISK_COMPONENT_DOMAIN/" /etc/asterisk/xmpp.conf
sed -i "s/sip.cheogram.com/$COMPONENT_DOMAIN/g" /etc/asterisk/extensions.lua
sed -i "s/^secret=.*/secret=$ASTERISK_COMPONENT_SECRET/" /etc/asterisk/xmpp.conf
if [ -n "$SIP_HOST" ]; then
	 if ! grep "@$SIP_HOST" /etc/asterisk/sip.conf; then
		sed -i "/^;register => 1234/a register => $SIP_USER:$SIP_PASSWORD@$SIP_HOST/\"$SIP_JID\"" /etc/asterisk/sip.conf
		echo "[$SIP_HOST]" >> /etc/asterisk/sip.conf
		echo "type=peer" >> /etc/asterisk/sip.conf
		echo "insecure=invite" >> /etc/asterisk/sip.conf
		echo "nat=yes" >> /etc/asterisk/sip.conf
		echo "srvlookup=yes" >> /etc/asterisk/sip.conf
		echo "host=$SIP_HOST" >> /etc/asterisk/sip.conf
		echo "realm=$SIP_HOST" >> /etc/asterisk/sip.conf
		echo "secret=$SIP_PASSWORD" >> /etc/asterisk/sip.conf
		echo "username=$SIP_USER" >> /etc/asterisk/sip.conf
		echo "fromuser=$SIP_USER" >> /etc/asterisk/sip.conf
		echo "fromdomain=$SIP_HOST" >> /etc/asterisk/sip.conf

		sed -i "s/-- outbound calls/if from ~= \"$SIP_JID\" then return end/" /etc/asterisk/extensions.lua
	 fi
fi
/usr/sbin/asterisk &

if [ -n "$SIP_HOST" ]; then
	/bin/cheogram-sip "$COMPONENT_DOMAIN" "$CONNECT_IP" "$CONNECT_PORT" "$COMPONENT_SECRET" "$REDIS_URL" "$ASTERISK_COMPONENT_DOMAIN" "$SIP_HOST"
else
	/bin/cheogram-sip "$COMPONENT_DOMAIN" "$CONNECT_IP" "$CONNECT_PORT" "$COMPONENT_SECRET" "$REDIS_URL" "$ASTERISK_COMPONENT_DOMAIN"
fi
