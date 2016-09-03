"""
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

"""
import zmq
import time
context = zmq.Context()

print('Connection to hello world server')
socket = context.socket(zmq.REP)
socket.setsockopt(zmq.IDENTITY, b"PEER")

socket.bind("tcp://*:5555")


while True:
    message = socket.recv()
    print("Received reply [ %s ]" % message)

    if message == 'delay':
        time.sleep(0.2)

    print("Sending response")
    socket.send(message)

