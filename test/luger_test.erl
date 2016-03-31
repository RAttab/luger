-module(luger_test).

-include_lib("eunit/include/eunit.hrl").

no_start_test() ->
    {error, _} = luger:alert("chan.alert", "format: ~s", ["blah"]).


stderr_test_() ->
    {foreach,
     fun() ->
             application:load(luger),
             application:set_env(luger, app_name, "luger_test"),
             application:set_env(luger, type, stderr),
             application:start(luger)
     end,
     fun(_) ->
             application:stop(luger)
     end,
     [
      {"basic stderr logging",
       fun () ->
               ok = luger:alert("chan.alert", "format: ~s", ["blah"]),
               ok = luger:error("chan.error", "format: ~B", [10]),
               ok = luger:warning("chan.warn", "format: ~p", [blah]),
               ok = luger:info("chan.info", "msg"),
               ok = luger:debug("chan.debug", "msg")
       end},
      {"trunc",
       fun () ->
               ok = luger:alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb", "msg"),
               ok = luger:alert(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb">>, "msg")
       end}
     ]}.


stderr_prio_test_() ->
    {foreach,
     fun() ->
             application:load(luger),
             application:set_env(luger, app_name, "luger_test"),
             application:set_env(luger, type, stderr),
             application:set_env(luger, stderr_min_priority, debug),
             application:start(luger)
     end,
     fun(_) ->
             application:stop(luger)
     end,
     [
      {"priority stderr logging",
       fun () ->
               ok = luger:alert("chan.alert", "format: ~s", ["blah"]),
               ok = luger:error("chan.error", "format: ~B", [10]),
               ok = luger:warning("chan.warn", "format: ~p", [blah]),
               ok = luger:info("chan.info", "msg"),
               ok = luger:debug("chan.debug", "msg")
       end}
     ]}.

syslog_test_() ->
    {foreach,
     fun() ->
             application:load(luger),
             application:set_env(luger, app_name, "luger_test"),
             application:set_env(luger, type, syslog_udp),
             application:set_env(luger, syslog_udp_host, {127, 0, 0, 1}),
             application:set_env(luger, syslog_udp_port, 514),
             application:start(luger)
     end,
     fun(_) ->
             application:stop(luger)
     end,
     [
      {"basic syslog logging",
       fun () ->
               ok = luger:alert("chan.alert", "format: ~s", ["blah"]),
               ok = luger:error("chan.error", "format: ~B", [10]),
               ok = luger:warning("chan.warn", "format: ~p", [blah]),
               ok = luger:info("chan.info", "msg"),
               ok = luger:debug("chan.debug", "msg")
       end},
      {"trunc",
       fun () ->
               ok = luger:alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb", "msg"),
               ok = luger:alert(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb">>, "msg")
       end}
     ]}.

error_logger_test_() ->
    {foreach,
     fun() ->
             application:load(luger),
             application:set_env(luger, app_name, "luger_test"),
             application:set_env(luger, type, stderr),
             application:start(luger)
     end,
     fun(_) ->
             application:stop(luger)
     end,
     [
      {"basic error logger",
       fun () ->
               error_logger:error_msg("error_msg"),
               error_logger:error_report("error_report")
       end}
     ]}.


statsd_test_() ->
    {foreach,
     fun() ->
             %% Should meck this up.
             application:load(statsderl),
             application:set_env(statsderl, hostname, {127, 0, 0, 1}),
             application:set_env(statsderl, port, 8125),
             application:start(statsderl),

             application:load(luger),
             application:set_env(luger, app_name, "luger_test"),
             application:set_env(luger, type, stderr),
             application:set_env(luger, statsd, true),
             application:start(luger)
     end,
     fun(_) ->
             application:stop(luger)
     end,
     [
      {"statsd metrics",
       fun () ->
               ok = luger:alert("chan.alert", "format: ~s", ["blah"]),
               ok = luger:error("chan.error", "format: ~B", [10]),
               ok = luger:warning("chan.warn", "format: ~p", [blah]),
               ok = luger:info("chan.info", "msg"),
               ok = luger:debug("chan.debug", "msg")
       end}
     ]}.
