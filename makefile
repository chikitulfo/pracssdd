compile:
	erl -make

start: compile
	yaws --conf yaws.conf 

test: ctest
	yaws --id test -D --conf yaws.conf
	erl -noshell -eval 'eunit:test([{dir, "ebin"}], [verbose])' -s init stop
	yaws --id test --stop

ctest: clean
	erlc +export_all +debug_info -o ebin src/*.erl test/*.erl
   
clean:
	rm -f ebin/*.beam erl_crash.dump 

