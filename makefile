compile:
	erl -make

start: compile
	yaws --conf yaws.conf
   
clean:
	rm -f ebin/*.beam erl_crash.dump 

