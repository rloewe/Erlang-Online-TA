PROJECT=master_server

mastermake: master_server.erl
	erlc master_server.erl;
	mkdir -p ../web/lib/master/ebin;
	cp master.app ../web/lib/master/ebin;
	cp master_server.beam ../web/lib/master/ebin;

