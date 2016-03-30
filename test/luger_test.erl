-module(luger_test).

-include_lib("eunit/include/eunit.hrl").

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
               luger:alert("chan.alert", "format: ~s", ["blah"]),
               luger:error("chan.error", "format: ~B", [10]),
               luger:warning("chan.warn", "format: ~p", [blah]),
               luger:info("chan.info", "msg"),
               luger:debug("chan.debug", "msg")
       end},
      {"trunc",
       fun () ->
               luger:alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb", "msg"),
               luger:alert(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb">>, "msg")
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
               luger:alert("chan.alert", "format: ~s", ["blah"]),
               luger:error("chan.error", "format: ~B", [10]),
               luger:warning("chan.warn", "format: ~p", [blah]),
               luger:info("chan.info", "msg"),
               luger:debug("chan.debug", "msg")
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
               luger:alert("chan.alert", "format: ~s", ["blah"]),
               luger:error("chan.error", "format: ~B", [10]),
               luger:warning("chan.warn", "format: ~p", [blah]),
               luger:info("chan.info", "msg"),
               luger:debug("chan.debug", "msg")
       end},
      {"trunc",
       fun () ->
               luger:alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb", "msg"),
               luger:alert(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbb">>, "msg")
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
