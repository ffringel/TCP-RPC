# TCP-RPC
A Basic RPC Server that listens for TCP connections and allows the user to execute commands via
that TCP stream

USAGE

To compile the erl files in the src directory,

cd ebin
erlc ../src/*.erl


Once you do that, start a shell in the ebin directory and run the 
start command exported by the application behaviour

Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:2:2] [async-threads:10] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
1> application:start(tcp_rpc).
ok


Open another terminal and run a telnet session


fabu@fabu:~$ telnet localhost 1055

Trying 127.0.0.1...

Connected to localhost.

Escape character is '^]'.

lists:reverse([1,2,3]).

[3,2,1]

init:stop().

ok

Connection closed by foreign host.
