default: compile start
start:
	erl -noinput -pa ../cecho/ebin/ -eval 'interface:start()' +A 50
compile:
	erlc interface.erl
	erlc plot_state_machine.erl
clean:
	rm *.dump
	rm *.beam
