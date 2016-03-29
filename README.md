# luger

A direct-to-syslog logger over udp for Erlang.

The reason for yet-another-syslog-logger is that all existing implementation
forward messages to a single process which, under heavy, load becomes a
bottleneck and tends to build up a huge backlog in it's message queue which can
take a long time to drain.

A call to `luger`'s log function will directly send the log message over UDP to a
syslog daemon. This approach offloads the queue management to the kernel which
will simply drop UDP packets if it's ever overloaded.

Note that `luger` uses the undocumented `inet_udp` module to directly send UDP
packets to the erlang `inet` driver while avoiding process queues. Use at your
own risk and peril.


## Build

```bash
make
```

Uses rebar under the hood so it can be easily included in any other rebar
project.


## Usage

Config for the application env can be one of the following:
```erlang
  {luger, [
      {type, stderr}
  ]}
```
```erlang
  {luger, [
      {type, syslog_udp},
	  {syslog_udp_host, "127.0.0.1"},
	  {syslog_udp_port, 514}
  ]}
```

Once the application is set up, logging can be done like so:
```erlang
luger:error("channel", "message: ~B", [10]),
luger:warn("channel", "message: ~p", [{blah}]),
```

That's it.
