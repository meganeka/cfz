C=csc -O3
makedir=mkdir -p $(dir $@)

bin/cfz: build/cfz
	@$(makedir)
	@cp $< $@

build/cfz: src/cfz.scm build/curses-loop.o
	(cd build && $(C) -uses curses-loop -o ../$@ $(patsubst %, ../%, $^))

build/curses-loop.o: src/curses-loop.scm
	@$(makedir)
	(cd build && \
	$(C) -c -unit curses-loop -ot curses-loop.types \
	-o ../$@ -J $(patsubst %, ../%, $<))

clean:
	rm -fr build bin
