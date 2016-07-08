erlangzmq documentation
=======================

`erlangzmq` is a library written in [Erlang](https://www.erlang.org/). It implements the ZeroMQ Message Transport Protocol (ZMTP). `erlangzmq` supports ZMTP version [3.1](http://rfc.zeromq.org/spec:37/ZMTP/).

System Overview
---------------

The system map below presents an overview of the system.

![System Map](images/erlangzmq_system_map.png)

Sockets
-------

Each socket creates a peer process for each remote peer it communicates with.

![Socket Composition Diagram](images/erlangzmq_socket.png)

Entity Relationship Diagrams
----------------------------

Here we can see different types of sockets and also which relationships
the sockets participate in.

![Entity Relationship Diagram](images/erlangzmq_entities.png)