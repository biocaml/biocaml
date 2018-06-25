.PHONY: all
all:
	jbuilder build --dev @install
	jbuilder build --dev @runtest

.PHONY: clean
clean:
	jbuilder clean

.PHONY: test
test:
	jbuilder build @runtest --dev -j 1

doc:
	make doc

