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
socket = context.socket(zmq.PUB)
socket.bind("tcp://*:5555")

for request in range(100):
    message = socket.send("oi")
    print("Received reply %s [ %s ]" % (request, message))
    time.sleep(1)
