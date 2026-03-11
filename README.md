# net_stuff

A hands-on exploration of **Erlang/OTP** networking primitives and concurrent programming patterns.

The project is structured as a deliberate learning progression: raw process message-passing →
hand-rolled OTP scaffold → standard `gen_server` behaviour → applied TCP/UDP socket programming.
Each layer builds on the previous one so you can see exactly what abstraction buys you.

## Architecture

```
                        OTP Application
                   ┌────────────────────┐
                   │      red_app       │
                   │   (entry point)    │
                   └────────┬───────────┘
                            │ starts
                   ┌────────▼───────────┐
                   │      red_sup       │
                   │  (top-level sup)   │
                   └────────────────────┘

  ─────────────────────────────────────────────────────────────────────────
  Networking modules (invoked directly from the Erlang shell):

  Custom OTP layer        Standard gen_server       Transport layer
  ┌──────────────────┐    ┌──────────────────────┐   ┌─────────────────────┐
  │  otp_interface   │    │     server.erl       │   │   tcp_server.erl    │
  │                  │    │                      │   │   tcp_client.erl    │
  │ spawn / sync /   │    │  Map-based K/V store │<--│   udp_receiver.erl  │
  │ async / reply    │    │  via gen_server      │   │   udp_sender.erl    │
  └────────┬─────────┘    └──────────────────────┘   └─────────────────────┘
           │ implements
  ┌────────▼─────────┐
  │     otp.erl      │
  │                  │
  │  Map-based K/V   │
  │  store (custom   │
  │  OTP callbacks)  │
  └──────────────────┘
```

## Prerequisites

- Erlang/OTP 25+
- [rebar3](https://rebar3.org) — the standard Erlang build tool

## Quick Start

```bash
$ rebar3 compile   # compile all modules
$ rebar3 shell     # launch an interactive Erlang shell with the project loaded
```

---

## GenServer — `src/genServer/`

A key-value store implemented with the standard `gen_server` OTP behaviour.
This is the pattern you reach for in production Erlang/Elixir projects.

```erlang
% Start the gen_server process (registers it locally as 'server')
server:start_link().

% Async write — returns immediately; state is updated inside the server process
server:store(username, <<"alice">>).

% Sync read — blocks until the server replies
server:get(username).           %=> <<"alice">>

% Async delete
server:delete(username).

% Retrieve all keys / the full map
server:get_keys().
server:get_data().

% Demonstrate long-running synchronous calls (blocks for 6 s)
server:sleep().

% Clear all entries
server:clear().

% Graceful shutdown
server:stop().
```

### Connecting multiple Erlang nodes

See [`src/genServer/connect.md`](src/genServer/connect.md) for instructions on running
a distributed Erlang cluster and accessing a `{global, ...}` registered `gen_server`
from any node in the cluster.

---

## Custom OTP — `src/otp/`

The same key-value store, but built on a hand-rolled OTP-like process scaffold
(`otp_interface`). The goal is to make the `gen_server` contract *visible*: you can
read `otp_interface.erl` and see exactly how spawn, send, receive, and
reference-based reply work before a framework hides them.

```erlang
% Spawn the store process; hold on to the Pid (no implicit name registration)
Pid = otp:start().

% Async writes and deletes
otp:save(Pid, language, erlang).
otp:delete(Pid, language).
otp:clear(Pid).

% Sync reads (1-second timeout enforced in otp_interface)
otp:get(Pid, language).         %=> erlang  (or `null` if not found)
otp:get_keys(Pid).
otp:get_data(Pid).

% Pretty-print the current state
otp:print(Pid).
```

---

## TCP Sockets — `src/tcp socket/`

A multi-client TCP server. Each accepted connection gets its own Erlang process,
which is Erlang's idiomatic answer to the C10k problem. The server maintains a live
registry of client sockets using the custom OTP store, and the listener socket is
kept in the `gen_server`.

### Server

```erlang
% Start listening on port 3000; keep the returned Pid
Pid = tcp_server:start(3000).

% Broadcast a message to every connected client
tcp_server:message(Pid, "system announcement").

% Cleanly disconnect all clients and free the port
tcp_server:stop(Pid).

% (In the shell, unbind the variable so it can be reused)
f(Pid).
```

### Client

```erlang
% Connect to the server
Pid = tcp_client:start(3000).

% Send a message — any string beginning with "exit" triggers a server-side disconnect
tcp_client:send(Pid, "hello server").
tcp_client:send(Pid, "exit goodbye").

% Close the socket from the client side
tcp_client:close_socket(Pid).
```

---

## UDP Sockets — `src/udp socket/`

A UDP fan-out hub: every datagram the receiver gets is re-broadcast to all currently
registered senders. Senders auto-discover a free local port on startup and register
themselves with the receiver on first contact.

### Receiver (hub)

```erlang
% Open a UDP socket and start the fan-out loop
udp_receiver:start(3000).

% Graceful shutdown
udp_receiver:stop().
```

### Sender (client)

```erlang
% Connect to the receiver at port 3000
% A free port >= 3001 is found automatically
udp_sender:start(3000).

% Broadcast — the receiver echoes this to every other registered sender
udp_sender:send("hello everyone").

% Deregister from the receiver and close the socket
udp_sender:stop().
```

---

## Concurrency Patterns — `src/patterns/`

Standalone demos of idiomatic Erlang concurrency patterns, runnable independently
from the shell.

### Ping-Pong (`ping_pong.erl`)

The canonical distributed Erlang example. Two processes bounce a message back and
forth a fixed number of times, then exit cleanly. Supports running `pong` on a
remote node via `spawn/4`.

```erlang
% Run locally — spawns both processes in the current node
ping_pong:start(5).

% Run distributed — pong on a remote node, ping stays local
% (both nodes must share the same cookie and be connected via net_kernel)
ping_pong:start_distributed('node2@hostname', 5).
```

### Publish / Subscribe (`pub_sub.erl`)

A lightweight in-process pub/sub broker backed by a `gen_server`. Topics are atoms;
any process can subscribe or publish without knowing who else is listening. Dead
subscribers are pruned lazily on the next publish.

```erlang
% Start the broker (registers it locally as 'pub_sub')
pub_sub:start_link().

% Subscribe the calling process to a topic
pub_sub:subscribe(news).

% Publish — every subscriber receives {pub_sub, Topic, Payload}
pub_sub:publish(news, "Erlang 28 released").

% Receive in your shell process
receive
    {pub_sub, news, Msg} -> io:format("Got: ~s~n", [Msg])
end.

% Unsubscribe
pub_sub:unsubscribe(news).
```
