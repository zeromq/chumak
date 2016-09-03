"""
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

"""
import zmq
import time
context = zmq.Context()

print('Connection to hello world server')
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5555")
#socket.connect("tcp://localhost:5556")


for request in range(100):
    print("Sending request %s" % request)
    time.sleep(1)
    socket.send(b"Hello1")
    message = socket.recv()
    time.sleep(1)
    socket.send(b"Hello")
    message = socket.recv()
    print("Received reply %s [ %s ]" % (request, message))
