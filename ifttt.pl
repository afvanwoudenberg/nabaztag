% ifttt.pl
% Aswin van Woudenberg

:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).

trigger_ifttt(Key,Event) :- 
	format(atom(URL),'https://maker.ifttt.com/trigger/~w/with/key/~w',[Event,Key]),
	http_open(URL,In,[method(post)]),
	open_null_stream(Out),
	copy_stream_data(In,Out),
	close(In), close(Out).

trigger_ifttt(Key,Event,Value1) :-
	format(atom(URL),'https://maker.ifttt.com/trigger/~w/with/key/~w',[Event,Key]),
	http_open(URL,In,[post(json(_{value1:Value1}))]),
	open_null_stream(Out),
	copy_stream_data(In,Out),
	close(In), close(Out).

trigger_ifttt(Key,Event,Value1,Value2) :-
	format(atom(URL),'https://maker.ifttt.com/trigger/~w/with/key/~w',[Event,Key]),
	http_open(URL,In,[post(json(_{value1:Value1, value2:Value2}))]),
	open_null_stream(Out),
	copy_stream_data(In,Out),
	close(In), close(Out).

trigger_ifttt(Key,Event,Value1,Value2,Value3) :-
	format(atom(URL),'https://maker.ifttt.com/trigger/~w/with/key/~w',[Event,Key]),
	http_open(URL,In,[post(json(_{value1:Value1, value2:Value2, value3:Value3}))]),
	open_null_stream(Out),
	copy_stream_data(In,Out),
	close(In), close(Out).

