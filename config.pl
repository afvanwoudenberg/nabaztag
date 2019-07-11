% config.pl
% Aswin van Woudenberg

public_host('<<PUBLIC_HOST_NAME_OR_IP_OF_SERVER>>').
public_port(<<PUBLIC_PORT_OF_SERVER>>).
server_port(<<PORT_OF_SERVER>>).

ping('<<NABAZTAG_MAC_ADDRESS>>',10).
ambient('<<NABAZTAG_MAC_ADDRESS>>',[],0,0,0).
ifttt('<<NABAZTAG_MAC_ADDRESS>>','<<IFTTT_WEBHOOK_KEY>>').
packets('<<NABAZTAG_MAC_ADDRESS>>',[]).


