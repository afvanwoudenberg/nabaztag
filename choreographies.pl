% choreographies.pl
% Aswin van Woudenberg

http:location(chor, vl(chor), []).

:- http_handler(chor('rainbow.chor'), rainbow, []).
:- http_handler(chor('slider.chor'), slider, []).
:- http_handler(chor('disco.chor'), disco, []).
:- http_handler(chor('larson.chor'), larson, []).
:- http_handler(chor('blink.chor'), blink, []).

rainbow(_Request) :-
	http_reply_choreography([
		tempo(0,60),
		led(0,left,red),
		led(0,middle,red),
		led(0,right,red),
		led(0,bottom,red),
		led(0,nose,red),
		led(1,left,orange),
		led(0,middle,orange),
		led(0,right,orange),
		led(0,bottom,orange),
		led(0,nose,orange),
		led(1,left,yellow),
		led(0,middle,yellow),
		led(0,right,yellow),
		led(0,bottom,yellow),
		led(0,nose,yellow),
		led(1,left,green),
		led(0,middle,green),
		led(0,right,green),
		led(0,bottom,green),
		led(0,nose,green),
		led(0,left,blue),
		led(1,middle,blue),
		led(0,right,blue),
		led(0,bottom,blue),
		led(0,nose,blue),
		led(1,left,indigo),
		led(0,middle,indigo),
		led(0,right,indigo),
		led(0,bottom,indigo),
		led(0,nose,indigo),
		led(1,left,violet),
		led(0,middle,violet),
		led(0,right,violet),
		led(0,bottom,violet),
		led(0,nose,violet)]).

slider(_Request) :-
	random_member(A,[red,green,blue,orange,purple]),
	random_member(B,[red,green,blue,orange,purple]),
	random_member(C,[red,green,blue,orange,purple]),
	http_reply_choreography([
		tempo(0,60),
		led(0,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(1,middle,A),
		led(1,right,A),
		led(1,left,black),
		led(1,middle,black),
		led(1,right,black),
		led(1,left,B),
		led(1,middle,B),
		led(1,right,B),
		led(1,left,black),
		led(1,middle,black),
		led(1,right,black),
		led(1,left,C),
		led(1,middle,C),
		led(1,right,C),
		led(1,left,black),
		led(1,middle,black),
		led(1,right,black)]).

disco(_Request) :-
	Len = 150,
	length(R,Len), length(G,Len), length(B,Len), length(L,Len),
	maplist(random(0,256),R), maplist(random(0,256),G), maplist(random(0,256),B), 
	findall(M,(member(M,L), random_member(M,[left,middle,right,bottom,nose])),Leds),
	findall(led(1,Led,Red,Green,Blue),(
		numlist(0,Len,I), member(N,I),
		nth0(N,R,Red), nth0(N,G,Green), nth0(N,B,Blue),
		nth0(N,Leds,Led)),Res),
	http_reply_choreography([tempo(0,10)|Res]).

larson(_Request) :-
	http_reply_choreography([
		tempo(0,20),
		ear(0,left,4,forward),
		ear(0,right,4,forward),
		led(0,left,255,0,0),
		led(1,middle,255,0,0),
		led(0,left,180,0,0),
		led(1,right,255,0,0),
		led(0,middle,180,0,0),
		led(0,left,40,0,0),
		led(1,middle,255,0,0),
		led(0,right,180,0,0),
		led(0,left,black),
		led(1,left,255,0,0),
		led(0,middle,180,0,0),
		led(0,right,40,0,0),
		led(1,middle,255,0,0),
		led(0,left,180,0,0),
		led(0,right,black),
		led(1,right,255,0,0), % Again
		led(0,middle,180,0,0),
		led(0,left,40,0,0),
		led(1,middle,255,0,0),
		led(0,right,180,0,0),
		led(0,left,black),
		led(1,left,255,0,0),
		led(0,middle,180,0,0),
		led(0,right,40,0,0),
		led(1,middle,255,0,0),
		led(0,left,180,0,0),
		led(0,right,black),
		led(1,right,255,0,0), % Again
		led(0,middle,180,0,0),
		led(0,left,40,0,0),
		led(1,middle,255,0,0),
		led(0,right,180,0,0),
		led(0,left,black),
		led(1,left,255,0,0),
		led(0,middle,180,0,0),
		led(0,right,40,0,0),
		led(1,middle,255,0,0),
		led(0,left,180,0,0),
		led(0,right,black),
		led(1,right,255,0,0), % Again
		led(0,middle,180,0,0),
		led(0,left,40,0,0),
		led(1,middle,255,0,0),
		led(0,right,180,0,0),
		led(0,left,black),
		led(1,left,255,0,0),
		led(0,middle,180,0,0),
		led(0,right,40,0,0),
		led(1,middle,255,0,0),
		led(0,left,180,0,0),
		led(0,right,black),
		led(1,right,255,0,0), % Again
		led(0,middle,180,0,0),
		led(0,left,40,0,0),
		led(1,middle,255,0,0),
		led(0,right,180,0,0),
		led(0,left,black),
		led(1,left,255,0,0),
		led(0,middle,180,0,0),
		led(0,right,40,0,0),
		led(1,middle,255,0,0),
		led(0,left,180,0,0),
		led(0,right,black)
	]).

blink(_Request) :-
	random_member(A,[red,green,blue,orange,purple]),
	http_reply_choreography([
		tempo(0,50),
		ear(0,left,0,forward),
		ear(0,right,0,forward),
		led(0,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black),
		led(1,left,A),
		led(0,middle,A),
		led(0,right,A),
		led(1,left,black),
		led(0,middle,black),
		led(0,right,black)
	]).

