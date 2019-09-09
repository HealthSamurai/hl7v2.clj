.PHONY: test

test:
	clj -A:test:runner

deploy: test
	clj -Spom
	mvn deploy

repl:
	clj -A:test:nrepl -e "(-main)" -r

push:
	mvn deploy
