compile:
	erl -make
   
clean:
	rm -f $(EBIN)/*.beam erl_crash.dump 

