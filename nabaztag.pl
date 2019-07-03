% nabaztag.pl
% Aswin van Woudenberg

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_log)).
:- use_module(library(socket)).
:- use_module(library(sgml)).
:- use_module(library(prolog_server)).

:- consult(config).
:- consult(choreographies).
:- consult(colors).
:- consult(ifttt).
:- consult(tts).

:- multifile http:location/3.
:- dynamic http:location/3.

:- dynamic ping/2.
:- dynamic ambient/5.
:- dynamic ifttt/2.
:- dynamic packets/2.

http:location(vl,root(vl),[]).
http:location(record,vl(record),[]).
http:location(tts,vl(tts),[]).
http:location(sounds,vl(sounds),[]).
http:location(api,vl(api),[]).

:- http_handler(vl('bc.jsp'),bc,[]).
:- http_handler(vl('locate.jsp'),locate,[]).
:- http_handler(vl('p4.jsp'),p4,[]).
:- http_handler(vl('record.jsp'),record,[]).
:- http_handler(vl('rfid.jsp'),rfid,[]).
:- http_handler(api('tts.jsp'),tts,[method(post)]).
:- http_handler(api('play.jsp'),play,[method(post)]).
:- http_handler(api('chor.jsp'),chor,[method(post)]).
:- http_handler(api('stream.jsp'),stream,[method(post)]).
:- http_handler(api('ambient.jsp'),ambient,[method(post)]).
:- http_handler(api('reboot.jsp'),reboot,[method(post)]).
:- http_handler(record(.),http_reply_from_files(record,[]),[prefix]).
:- http_handler(sounds(.),http_reply_from_files(sounds,[]),[prefix]).
:- http_handler(tts(.),http_reply_from_files(tts,[]),[prefix]).

server_start :- 
	server_port(Port), http_server(http_dispatch, [port(Port)]).
server_stop :- 
	server_port(Port), http_stop_server(Port, []).
server_restart :- 
	server_port(Port), 
	http_stop_server(Port, []), 
	http_server(http_dispatch, [port(Port)]).

bc(Request) :-
	catch(http_parameters(Request, [m(M,[])]), _E, fail),
	atom_codes(M, P),
	subtract(P, [58], Q), atom_codes(S, Q),
	ping(S,_),
	http_reply_file('bootcode.bin', [], []).

locate(Request) :-
	catch(http_parameters(Request, [sn(SN,[])]), _E, fail),
	ping(SN,_),
	public_host(Host), public_port(Port),
	format('Content-type: text/plain~n~n'),
	format('ping ~s:~d~nbroad ~s:~d~n', [Host,Port,Host,Port]).

p4(Request) :- % Ping event
	catch(http_parameters(Request, [sn(SN,[]), sd(SD,[]), tc(TC,[])]), _E, fail),
	ping(SN,_),
	ping(Request,SN,SD,TC).

ping(_Request,SN,_SD,'0') :- % wakeup rabbit
	ping(SN,T),
	http_reply_packet([message([id(2147483647)]),ping(T)]),
	ifttt(SN,Key),
	atomic_list_concat([SN,wakeup],'_',Trigger),
	trigger_ifttt(Key,'nabaztag',Trigger),
	trigger_ifttt(Key,Trigger).
ping(_Request,SN,'0',_TC) :- % Simple ping event
	next_packet(SN,Packet),
	http_reply_packet(Packet).
ping(_Request,SN,'1',_TC) :- % Double-click event
	next_packet(SN,Packet),
	http_reply_packet(Packet), 
	ifttt(SN,Key),
	atomic_list_concat([SN,dblclick],'_',Trigger),
	trigger_ifttt(Key,'nabaztag',Trigger),
	trigger_ifttt(Key,Trigger).
ping(_Request,SN,'2',_TC) :- % End of message event
	next_packet(SN,Packet),
	http_reply_packet(Packet). 
ping(_Request,SN,'3',_TC) :- % Single-click event
	next_packet(SN,Packet),
	http_reply_packet(Packet), 
	ifttt(SN,Key),
	atomic_list_concat([SN,click],'_',Trigger),
	trigger_ifttt(Key,'nabaztag',Trigger),
	trigger_ifttt(Key,Trigger).
ping(_Request,SN,'5',_TC) :- % Single-click while playing event
	next_packet(SN,Packet),
	http_reply_packet(Packet), 
	ifttt(SN,Key),
	atomic_list_concat([SN,interrupt],'_',Trigger),
	trigger_ifttt(Key,'nabaztag',Trigger),
	trigger_ifttt(Key,Trigger).
ping(_Request,SN,SD,_TC) :- % Ears move event
	not(member(SD,['0','1','2','3','5'])),
	downcase_atom(SD,SDD),
	atom_codes(SDD,[56,Left,_M,Right]),
	atom_codes('0123456789abcdef',Codes),
	nth0(LeftEar,Codes,Left),
	nth0(RightEar,Codes,Right),
	next_packet(SN,Packet),
	http_reply_packet(Packet), 
	ifttt(SN,Key),
	atomic_list_concat([SN,earsmove],'_',Trigger),
	atom_number(LA,LeftEar), 
	atom_number(RA,RightEar),
	trigger_ifttt(Key,'nabaztag',Trigger,LA,RA),
	trigger_ifttt(Key,Trigger,LeftEar,RightEar).

record(Request) :- % Record event
	catch(http_parameters(Request, [sn(SN,[]),m(M,[])]), _E, fail),
	next_packet(SN,Packet),
	((M = '0') -> Btn=click ; Btn=dblclick),
	uuid(UUID),
	format(atom(File),'record/~s.wav',[UUID]),
	open(File,write,Out,[type(binary)]),
	http_read_data(Request,_,[to(stream(Out))]),
	flush_output(Out),
	close(Out),
	http_reply_packet(Packet), 
	ifttt(SN,Key),
	atomic_list_concat([SN,record,Btn],'_',Trigger),
	public_host(Host), public_port(Port),
        format(atom(URL),'http://~s:~d/vl/~s',[Host,Port,File]),
	trigger_ifttt(Key,'nabaztag',Trigger,URL),
	trigger_ifttt(Key,Trigger,URL).

rfid(Request) :- % Read RFID tag
	catch(http_parameters(Request, [sn(SN,[]),t(Tag,[])]), _E, fail),
	next_packet(SN,Packet),
	http_reply_packet(Packet), 
	ifttt(SN,Key),
	atomic_list_concat([SN,Tag],'_',Trigger),
	trigger_ifttt(Key,'nabaztag',Trigger),
	trigger_ifttt(Key,Trigger).

% API
tts(Request) :- % Perform tts
	catch(http_parameters(Request, [sn(SN,[]),lang(Lang,[default(en)]),text(Text,[])]), _E, fail),
	ping(SN,_),
	uuid(UUID),
	format(atom(File),'tts/~s.mp3',[UUID]),
	tts(Text,Lang,File),
	public_host(Host), public_port(Port),
	format(atom(URL),'http://~s:~d/vl/~s',[Host,Port,File]),
	add_packet(SN,[message([ms(URL),mw]),ping(3)]),
	format('Content-type: text/plain~n~n'),
        format('Congratulations! You''ve successfully called the TTS API.~n').

play(Request) :- % Play mp3 from URL
	catch(http_parameters(Request, [sn(SN,[]),url(URL,[])]), _E, fail),
	ping(SN,_),
	add_packet(SN,[message([ms(URL),mw]),ping(3)]),
	format('Content-type: text/plain~n~n'),
        format('Congratulations! You''ve successfully called the play API.~n').

stream(Request) :- % Play stream from URL
	catch(http_parameters(Request, [sn(SN,[]),url(URL,[])]), _E, fail),
	ping(SN,_),
	add_packet(SN,[message([st(URL),mw]),ping(3)]),
	format('Content-type: text/plain~n~n'),
        format('Congratulations! You''ve successfully called the stream API.~n').

reboot(Request) :- % Reboot the bunny
	catch(http_parameters(Request, [sn(SN,[])]), _E, fail),
	ping(SN,_),
	add_packet(SN,[reboot,ping(3)]),
	format('Content-type: text/plain~n~n'),
        format('Congratulations! You''ve successfully called the reboot API.~n').

chor(Request) :- % Play choreography
	catch(http_parameters(Request, [sn(SN,[]),chor(Chor,[optional(true)]),url(URL,[optional(true)])]), _E, fail),
	ping(SN,_),
	(nonvar(Chor) -> 
		add_packet(SN,[message([ch(Chor),mw]),ping(3)]) ; 
		(nonvar(URL) -> 
			add_packet(SN,[message([ch(URL),mw]),ping(3)]) ; 
			true 
		)
	),
	format('Content-type: text/plain~n~n'),
        format('Congratulations! You''ve successfully called the choreography API.~n').

ambient(Request) :- 
	catch(http_parameters(Request, [sn(SN,[])]), _, fail),
	ambient(SN,Services,L,R,B),
	catch(http_parameters(Request, [
		sn(SN,[]),
		leftear(LE,[default(L),between(0,15)]),
		rightear(RE,[default(R),between(0,15)]),
		nose(N,[default(B),between(0,2)]),
		weather(W,[optional(true),oneof([none,cloudy,smog,rain,snow,storm])]),
		market(M,[optional(true),oneof([none,highdown,mediumdown,littledown,stable,littleup,mediumup,highup])]),
		traffic(T,[optional(true),oneof([none,100,75,50,25,12,8,4])]),
		messages(E,[optional(true),oneof([none,0,1,2,3])]),
		airquality(A,[optional(true),oneof([none,good,medium,low])])
	]), _, fail),
	update_services(Services,weather,W,S1),
	update_services(S1,market,M,S2),
	update_services(S2,traffic,T,S3),
	update_services(S3,messages,E,S4),
	update_services(S4,airquality,A,NewServices),
	with_mutex(SN,(
		retractall(ambient(SN,Services,L,R,B)),
		assert(ambient(SN,NewServices,LE,RE,N))
	)),
	format('Content-type: text/plain~n~n'),
        format('Congratulations! You''ve successfully called the ambient API.~n').

update_services(Services,Service,Value,NewServices) :-
	nonvar(Value), Value = none,
	subtract(Services,[(Service,_)],NewServices).
update_services(Services,Service,Value,[(Service,Value)|NewServices]) :-
	nonvar(Value), Value \= none, subtract(Services,[(Service,_)],NewServices).
update_services(Services,_,Var,Services) :- var(Var).

http_reply_packet(Packet) :- 
	packet(Packet,Reply,[]), 
	format('~n~n'),
	http_reply_codelist(Reply).

http_reply_choreography(Choreography) :-
	choreography(Choreography,Reply,[]),
	format('~n~n'),
	http_reply_codelist(Reply).

http_reply_codelist([]).
http_reply_codelist([Head|Tail]) :- put_code(Head), http_reply_codelist(Tail).

add_packet(SN,Packet) :-
	with_mutex(SN,(
		retractall(packets(SN,OldPackets)),
		append(OldPackets,[Packet],NewPackets),
		assert(packets(SN,NewPackets))
	)).

next_packet(SN,Packet) :-
	with_mutex(SN,(
		packets(SN,[Packet|Tail]),
		retractall(packets(SN,[Packet|Tail])),
		assert(packets(SN,Tail))
	)).
next_packet(SN,[ambient(Services,L,R,B),ping(T)]) :-
	with_mutex(SN,(
		packets(SN,[]),
		ping(SN,T),
		ambient(SN,Services,L,R,B)
	)).

packet(Blocks) --> [0x7F], blocks(Blocks), [0xFF,0x0A].

blocks([]) --> [].
blocks([Head|Tail]) --> block(Head), blocks(Tail).

block(reboot) --> [0x09], size(0).
block(ping(I)) --> [0x03], size(1), [I].
block(ambient(A,L,R,B)) --> 
	[0x04], 
	{ S is 23 + B }, size(S), 
	[0x7f,0xff,0xff,0xff], ambient(A,8), [R,L], 
	blink(B).
block(message(Msgs)) --> [0x0a], { messages(Msgs,B,[]), obfuscate(B,O), length(O,L) }, size(L), O.

ambient(_,0) --> [].
ambient([],L) --> { L > 0, M is L - 1 }, [0,0], ambient([],M).
ambient([(K,V)|T],L) --> { number(K), L > 0, M is L - 1 }, [K,V], ambient(T,M).
ambient([(K,V)|T],L) --> { not(number(K)), L > 0, M is L - 1 }, service(K,V), ambient(T,M).

service(weather,sunny) --> [1,0].
service(weather,cloudy) --> [1,1].
service(weather,smog) --> [1,2].
service(weather,rain) --> [1,3].
service(weather,snow) --> [1,4].
service(weather,storm) --> [1,5].
service(market,highdown) --> [2,0].
service(market,mediumdown) --> [2,1].
service(market,littledown) --> [2,2].
service(market,stable) --> [2,3].
service(market,littleup) --> [2,4].
service(market,mediumup) --> [2,5].
service(market,highup) --> [2,6].
service(traffic,100) --> [3,0].
service(traffic,75) --> [3,1].
service(traffic,50) --> [3,2].
service(traffic,25) --> [3,3].
service(traffic,12) --> [3,4].
service(traffic,8) --> [3,5].
service(traffic,4) --> [3,6].
service(messages,V) --> { member(V,[0,1,2,3]) }, [6,V].
service(airquality,good) --> [7,0].
service(airquality,medium) --> [7,5].
service(airquality,low) --> [7,8].

blink(0) --> [0].
blink(1) --> [5,0].
blink(2) --> [5,5,0].

size(S) --> { C is S mod 256, 
		R is (S - C) div 256, B is R mod 256,
		Q is (R - B) div 256, A is Q mod 256 }, [A,B,C].

messages([]) --> [].
messages([Head|Tail]) --> message(Head), messages(Tail).

message(id(ID)) --> "ID ", { format(codes(A),'~d~n',[ID]) }, A.
message(mu(URL)) --> "MU ", { format(codes(A),'~s~n',[URL]) }, A.
message(ms(URL)) --> "MS ", { format(codes(A),'~s~n',[URL]) }, A.
message(st(URL)) --> "ST ", { format(codes(A),'~s~n',[URL]) }, A.
message(sp(URL)) --> "SP ", { format(codes(A),'~s~n',[URL]) }, A.
message(is(App)) --> "IS ", { format(codes(A),'~s~n',[App]) }, A.
message(ie) --> "IE\n".
message(ch(Chor)) --> { 
		catch(http_location_by_id(Chor,L),_,fail), 
		public_host(Host), public_port(Port),
		format(codes(A),'CH http://~s:~d~s~n', [Host,Port,L]) }, A.
message(ch(URL)) --> { not(catch(http_location_by_id(URL,_),_,fail)) }, 
		"CH ", { format(codes(A),'~s~n',[URL]) }, A.
message(mw) --> "MW\n".

choreography(Cmds) --> { commands(Cmds,Res,[]), length(Res,Len) }, csize(Len), Res, [0,0,0,0].

csize(S) --> { D is S mod 256, 
		R is (S - D) div 256, C is R mod 256,
		Q is (R - C) div 256, B is Q mod 256,
		P is (Q - B) div 256, A is P mod 256 }, [A,B,C,D].

commands([]) --> [].
commands([Head|Tail]) --> command(Head), commands(Tail).

command(led(TS,L,R,G,B)) --> [TS,7], led(L), [R,G,B,0,0].
command(led(TS,L,C)) --> [TS,7], led(L), { color(C,R,G,B) }, [R,G,B,0,0].
command(ear(TS,Ear,Pos,Dir)) --> [TS,8], ear(Ear), [Pos], direction(Dir).
command(tempo(TS,Tempo)) --> [TS,1,Tempo].
command(midi(TS)) --> [TS,0x10].

led(bottom) --> [0].
led(left) --> [1].
led(middle) --> [2].
led(right) --> [3].
led(nose) --> [4].

ear(right) --> [0].
ear(left) --> [1].

direction(forward) --> [0].
direction(backward) --> [1].

inv8([0x01, 0xAB, 0xCD, 0xB7, 0x39, 0xA3, 0xC5, 0xEF,
	0xF1, 0x1B, 0x3D, 0xA7, 0x29, 0x13, 0x35, 0xDF,
	0xE1, 0x8B, 0xAD, 0x97, 0x19, 0x83, 0xA5, 0xCF,
	0xD1, 0xFB, 0x1D, 0x87, 0x09, 0xF3, 0x15, 0xBF,
	0xC1, 0x6B, 0x8D, 0x77, 0xF9, 0x63, 0x85, 0xAF,
	0xB1, 0xDB, 0xFD, 0x67, 0xE9, 0xD3, 0xF5, 0x9F,
	0xA1, 0x4B, 0x6D, 0x57, 0xD9, 0x43, 0x65, 0x8F,
	0x91, 0xBB, 0xDD, 0x47, 0xC9, 0xB3, 0xD5, 0x7F,
	0x81, 0x2B, 0x4D, 0x37, 0xB9, 0x23, 0x45, 0x6F,
	0x71, 0x9B, 0xBD, 0x27, 0xA9, 0x93, 0xB5, 0x5F,
	0x61, 0x0B, 0x2D, 0x17, 0x99, 0x03, 0x25, 0x4F,
	0x51, 0x7B, 0x9D, 0x07, 0x89, 0x73, 0x95, 0x3F,
	0x41, 0xEB, 0x0D, 0xF7, 0x79, 0xE3, 0x05, 0x2F,
	0x31, 0x5B, 0x7D, 0xE7, 0x69, 0x53, 0x75, 0x1F,
	0x21, 0xCB, 0xED, 0xD7, 0x59, 0xC3, 0xE5, 0x0F,
	0x11, 0x3B, 0x5D, 0xC7, 0x49, 0x33, 0x55, 0xFF]).

obfuscate(In,[0x01|Out]) :- obfuscate(In,Out,0x47).

obfuscate([],[],_K).
obfuscate([P|PTail],[O|OTail],K) :-
	inv8(Inv),
	I is K >> 1,
	nth0(I,Inv,E),
	O is (0x2F + P * E) /\ 0xFF,
	NewK is (2 * P + 1) /\ 0xFF,
	obfuscate(PTail,OTail,NewK).

