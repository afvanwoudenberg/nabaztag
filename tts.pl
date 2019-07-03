% tts.pl
% Aswin van Woudenberg

:- use_module(library(uri)).

% curl 'https://translate.google.com/translate_tts?ie=UTF-8&q=hello&tl=en&tk=995126.592330&client=tw-ob' -H 'user-agent: stagefright/1.2 (Linux;Android 5.0)' -H 'referer: https://translate.google.com/' > google_tts.mp3

tts(Text,File) :-
	tts(Text,'en',File).
tts(Text,Lang,File) :-
	uri_query_components(QS, [ie='UTF-8', q=Text, tl=Lang, tk='995126.592330', client='tw-ob']),
	format(atom(URL),'https://translate.google.com/translate_tts?~w',[QS]),
	http_open(URL,In,[method(get), user_agent('stagefright/1.2 (Linux;Android 5.0)'), request_header(referer='https://translate.google.com/')]),
        open(File,write,Out,[type(binary)]),
        copy_stream_data(In,Out),
        flush_output(Out),
        close(In),
        close(Out).

