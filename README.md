Net functionalities
=====

Build
-----
    $ rebar3 compile

Run
-----
    $ rebar3 shell

## GenServer

Working with a maps data sctructure making simple operations as Storing, Reading and deleting information 
with a GenServer workflow

```Erlang
% For starting the process
server:start_link().

% For storing data as Key => Value
server:store(key_you_want_to_store, value_for_that_key).

% For reading a value using its key to access it
server:get(key_you_want_to_read).

% For removing a pair using its key 
server:delete(key_you_want_to_delete).

% For retrieving all the keys stored
server:get_keys().

% To make the process sleep for 6 seconds
server:sleep().

% To clear the maps content (Your key->value pairs)
server:clear().

% To stop the GenServer process
server:stop().
```
## Open Telecom Plataform (OTP)
Working with a maps data sctructure making simple operations as Storing, Reading and deleting information 
with a OTP workflow
```Erlang
% For starting the process and saving its identifier into Pid
Pid = otp:start().

% For storing data as Key => Value
otp:save(Pid, llave, valor).

% For reading a value using its key to access it
otp:get(Pid, key_you_want_to_store, value_for_that_key).

% For removing a pair using its key
otp:delete(Pid, key_you_want_to_delete).

% For retrieving all the keys stored
otp:get_keys(Pid).

% For printing the actual state of the maps data structure
otp:print(Pid). 

% To clear the maps content (Your key->value pairs)
otp:clear(Pid).

```
## TCP socket workflow

Server for TCP sockets and client, using the OTP and the gen_server modules to store data.

### TCP Server

```Erlang 
% Starting the server on the port you want to 
% for this tutorial we are going to use port 3000
Pid = tcp_server:start(3000).

% For sending a message to all of the clients connected
% If there is any client connected, the message will be stored in a queue waiting for a client to connect
% and when the client connects, it will send those messages
tcp_server:message(Pid, "Here goes the message you want to send")

% Disconnect every client and shut down the server
tcp_server:stop().

% If you want to run the server again without closing the actual console don't forget to do this: 
f(Pid). 
```

### TCP Client

```Erlang
% For starting the client with the port of the server you want to connect to
% For this tutorial we are going to use port 3000
Pid = tcp_client:start(3000).

% For sending a message to the server 
tcp_client:send(Pid, "Hello Server :D").
% If the string you are sending starts with 'exit', it will be processed as a unnconnection solicitude and u will disconnect from server
% Example: 
tcp_client:send(Pid, "exit from this Server").

% Closing the socket from client
tcp_client:close_socket(Pid)
```
