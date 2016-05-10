#How to compile
Run the bash script compile.sh

#How to run
To start a master run:
`erl -name master -run start start_server master`
To start a node run:
`erl -name node -run start start_server node`

Both requires a config file called server.conf.
