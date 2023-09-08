Connecting 2 nodes
===
Startig the node
-----
    $ erl -sname node1 

# Calls the node i2 (replies pong if connected successfully)
``` Erlang console
    net:ping('node2@destiny_pc_name'). 
```
# To check that all nodes are connected
``` Erlang console
    nodes().                           
``` 

