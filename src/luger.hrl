-define(EMERGENCY, 0).
-define(ALERT, 1).
-define(CRITICAL, 2).
-define(ERROR, 3).
-define(WARNING, 4).
-define(NOTICE, 5).
-define(INFO, 6).
-define(DEBUG, 7).

-record(config, { app    :: string(),
                  host   :: string(),
                  statsd :: boolean() }).

-record(syslog_udp_config, { host     :: string(),
                             port     :: integer(),
                             facility :: integer() }).

-record(stderr_config, { min_priority :: integer() }).

-record(null_config, {}).
