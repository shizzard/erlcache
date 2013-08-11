-define(LIBDIR(Dir), code:lib_dir(bigwoot, Dir)).

-define(LOG(Level, Format, Args), lager:log(Level, self(), Format, Args)).

-define(START, ?START(static)).
-define(START(Name), ?INFO("Start/~p", [Name])).

-define(DEBUG(Msg), ?DEBUG(Msg, [])).
-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).

-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Args), ?LOG(info, Msg, Args)).

-define(NOTICE(Msg), ?NOTICE(Msg, [])).
-define(NOTICE(Msg, Args), ?LOG(notice, Msg, Args)).

-define(WARNING(Msg), ?WARNING(Msg, [])).
-define(WARNING(Msg, Args), ?LOG(warning, Msg, Args)).

-define(ERROR(Msg), ?ERROR(Msg, [])).
-define(ERROR(Msg, Args), ?LOG(error, Msg, Args)).

-define(CRITICAL(Msg), ?CRITICAL(Msg, [])).
-define(CRITICAL(Msg, Args), ?LOG(critical, Msg, Args)).

-define(ALERT(Msg), ?ALERT(Msg, [])).
-define(ALERT(Msg, Args), ?LOG(alert, Msg, Args)).

-define(EMERGENCY(Msg), ?EMERGENCY(Msg, [])).
-define(EMERGENCY(Msg, Args), ?LOG(emergency, Msg, Args)).

-define(DYN_NAME(Name), ?DYN_NAME(?MODULE, Name)).
-define(DYN_NAME(Module, Name), {Module, Name}).


-define(TIMESTAMP, timer:now_diff(now(), {0,0,0}) div 1000000).
-define(TIMESTAMP_WM, (fun() ->
    {_, _, M} = now(),
    ?TIMESTAMP * 1000 + erlang:round(M / 1000)
end)()).
-define(INT_TO_BIN(Int), list_to_binary(integer_to_list(Int))).
