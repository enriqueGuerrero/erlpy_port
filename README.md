erlpy_port
======
TCP Server Socket written in Erlang and execute python code through Erlang port.

How it works?
------
The gen_server (erlpy_port.erl) creates one child (gen_server - erlpy_port_acceptor.erl) to listening a TCP connection. When a client establishes a connection to the child that was listening. erlpy_port creates another child to listening again. The child has a timeout to receive some message, if the timeout ends, this child die and close the TCP connection.

The child receives a message that included an ID. This ID is associated with a python file, this association is defined in erlpy_port.schema and then, this child opens a port and execute that file. Finally, this child collects the response by the python file and returns that response to the client.

The message received and sent is a json string format.

Dependencies
------
You will need:

Erlang OTP 18.0  or later

Python 2.7.12 or later

rebar3 to compile

Clone
------

Clone:
	
	$ git clone https://github.com/enriqueGuerrero/erlpy_port.git
	      
Into erlpy_port:

	$ cd erlpy_port
       	    
Compile:

	$ make compile
	       
How to configure?
------

See schema/erlpy_port.schema file.

Starting
------
To start erlpy_port, type:
   
	$ make start

Or you also can:
   
	$ make console

Author
------
Enrique Iván Guerrero Martínez <enrique.eigm@gmail.com>.

License
------
THIS SOFTWARE IS LICENSED UNDER BSD LICENSE.