TESTSUITE=progs-test-lab2

all: ${TESTSUITE}

${TESTSUITE}: ${TESTSUITE}.hs
	ghc -threaded --make -o $@ $<

clean:
	-rm ${TESTSUITE} *.hi *.o ${TESTSUITE}.tgz

dist: lab2-testsuite.tgz

lab2-testsuite.tgz: 
	tar -cvzf ../$@ --transform 's,^,testsuite/,' \
	--exclude="*.hi" --exclude="*.o" --exclude="*~" \
	--exclude=$@ --exclude=${TESTSUITE} \
	*
