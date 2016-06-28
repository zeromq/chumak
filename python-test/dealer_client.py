"""
@copyright 2016 Choven Corp.

This file is part of erlangzmq.

erlangzmq is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

erlangzmq is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with erlangzmq.  If not, see <http://www.gnu.org/licenses/>
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
