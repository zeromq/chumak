"""
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

"""
import zmq
import time
context = zmq.Context()

print('Connection to hello world server')
socket = context.socket(zmq.DEALER)
socket.connect("tcp://localhost:5555")
socket.connect("tcp://localhost:5556")


for request in range(100):
    print("Sending request %s" % request)
    socket.send_multipart([b"", b"Hello1"])
    socket.send_multipart([b"", b"Hello2"])
    socket.send_multipart([b"", b"Hello2"])
    message1 = socket.recv_multipart()
    message2 = socket.recv_multipart()
    message3 = socket.recv_multipart()
    time.sleep(1)
    print("Received reply %s [ %s ]" % (request, message1))
    print("Received reply %s [ %s ]" % (request, message2))
