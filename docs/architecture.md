Architecture
============

This document aims to describe the architecture of the system to make it easier
for contributors to understand its structure and behavior.

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

Notation
--------

The notation used in diagrams follows the [FMC](http://www.fmc-modeling.org/notation_reference) standard.
