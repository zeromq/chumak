"""
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

"""

import zmq
import time
context = zmq.Context()

print('PAIR client')
socket = context.socket(zmq.PAIR)
socket.connect("tcp://localhost:5555")

for request in range(100):
    message = socket.recv()
    print("Recv %d, %s" % (request, message))
    time.sleep(1)
    socket.send("I am a python man!")
