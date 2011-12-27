default: compile start
start:
	erl -noinput -pa ../cecho/ebin/ -eval 'interface:start()' +A 50
repl:
	erl -pa ../cecho/ebin/
compile:
	erlc interface.erl
	erlc board.erl
	erlc plot.erl
clean:
	rm *.dump
	rm *.beam
fixterm:
	stty cooked
