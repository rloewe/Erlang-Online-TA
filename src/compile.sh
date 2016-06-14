erlc start.erl;
cd lib/;
erl -pa . -make;
cd ../node;
erlc *.erl;
cd ../master;
erlc *.erl;
cd ../tests;
erlc *.erl;
