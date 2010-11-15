include $(top_srcdir)/Make-rules

KAWALIB = $(top_builddir)
KAWA = ../../../bin/kawa.sh

.SUFFIXES = .q2 .out

.q2.expected-out:
	sed -n -e 's|# Output: *\(.*$$\)|\1|p' $< > $@

.q2.out:
	$(KAWA) $< > $@

TEST_Q2_FILES = binary1.q2 comment1.q2 function1.q2 indent1.q2
EXTRA_DIST = $(TEST_Q2_FILES)
EXTRA_CLEAN = *.out *.expected-out

run-tests:
	@for f in $(TEST_Q2_FILES); do b=`echo $$f | sed -e 's|[.]q2||'`; $(MAKE) $$b.out $$b.expected-out; \
	if diff $$b.expected-out $$b.out; \
	  then echo "# test $$b passes"; \
	  else echo FAIL test $$b; fi; \
	done

check: run-tests
