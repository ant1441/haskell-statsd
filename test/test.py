import socket
from contextlib import closing
import sys

if __name__ == "__main__":
    with closing(socket.socket()) as s:
        s.connect(('127.0.0.1', int(sys.argv[1])))
        s.send("pyfoo:1|c")
