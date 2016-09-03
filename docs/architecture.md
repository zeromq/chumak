Architecture
============

This document aims to describe the architecture of the system to make it easier
for contributors to understand its structure and behavior.

System Overview
---------------

The system map below presents an overview of the system.

![System Map](images/system_map.png)

Sockets
-------

Each socket creates a peer process for each remote peer it communicates with.

![Socket Composition Diagram](images/socket.png)

Entity Relationship Diagrams
----------------------------

Here we can see different types of sockets and also which relationships
the sockets participate in.

![Entity Relationship Diagram](images/entities.png)

About Diagrams
--------------

1. The notation used in diagrams follows the [FMC](http://www.fmc-modeling.org/notation_reference) standard.
2. Diagrams can be modified with draw.io. The source can be found in [docs/src](src).
