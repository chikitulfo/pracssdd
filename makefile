compile:
	erl -make

start: compile
	yaws --conf yaws.conf 

test: ctest
	erl -noshell -eval 'eunit:test([{dir, "ebin"}], [verbose])' -s init stop

ctest: clean
	erlc +export_all +debug_info -o ebin src/*.erl test/*.erl
   
clean:
	rm -f ebin/*.beam erl_crash.dump 

