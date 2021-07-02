.PHONY: build clean check coverage check-report

build:
	dune build @install

check:
	dune runtest

check-report:
	dune runtest --instrument-with bisect_ppx --force

coverage: check-report
	bisect-ppx-report html

clean:
	dune clean
