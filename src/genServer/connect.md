# Distributed Erlang — Connecting Nodes

Erlang's distributed mode lets processes on different nodes communicate as if
they were on the same machine.  This is what OTP calls a "cluster", and it is
the foundation of hot-code upgrades, global process registration, and
geo-distributed fault tolerance.

---

## 1. Start each node with a short name

Open two terminal windows and start one node in each:

```bash
# Terminal 1
$ erl -sname node1

# Terminal 2
$ erl -sname node2
```

The `-sname` flag assigns a *short* name (no domain).  Use `-name` with a
fully-qualified hostname (`node1@host.example.com`) for cross-machine clusters.

---

## 2. Connect the nodes

From **node1**'s Erlang shell, ping node2:

```erlang
% Replace 'your_pc_name' with the hostname shown in node2's prompt
net_adm:ping('node2@your_pc_name').
%=> pong   (connection succeeded)
%=> pang   (connection failed — check cookie and hostname)
```

> **Cookie** — both nodes must share the same Erlang cookie.  By default
> the cookie is read from `~/.erlang.cookie`.  You can override it per-session:
> ```bash
> $ erl -sname node1 -setcookie my_secret_cookie
> ```

---

## 3. Verify the cluster

```erlang
% List all nodes connected to the current one
nodes().
%=> ['node2@your_pc_name']

% Ping is transitive — node1 ↔ node2 ↔ node3 forms a fully-connected mesh
% automatically once any two adjacent nodes connect.
```

---

## 4. Use the global gen_server across nodes

`server.erl` ships with a commented-out `{global, ?SERVER}` variant.  To share
the key-value store across the cluster:

1. Uncomment the `{global, ...}` lines in `server.erl` and recompile.
2. Start the server on **one** node:
   ```erlang
   % node1
   server:start_link().
   server:store(greeting, <<"hello cluster">>).
   ```
3. Read from **any other** node:
   ```erlang
   % node2
   server:get(greeting).
   %=> <<"hello cluster">>
   ```

Global registration uses `global:register_name/2` under the hood and
propagates across the mesh automatically.

---

## 5. Remote process spawn

You can spawn a process on a remote node directly:

```erlang
% Spawn a function on node2 and get its Pid
RemotePid = spawn('node2@your_pc_name', fun() ->
    receive
        {hello, From} -> From ! {world, self()}
    end
end).

RemotePid ! {hello, self()}.

receive
    {world, _} -> io:format("got reply from remote node~n")
end.
```

See `src/patterns/ping_pong.erl` for a complete worked example using
`ping_pong:start_distributed/2`.
