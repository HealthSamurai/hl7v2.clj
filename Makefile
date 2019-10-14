.PHONY: test deploy jar repl

test:
	clojure -A:test:runner

jar:
	clojure -A:jar

clojars-push:
	clojure -A:deploy

repl:
	clj -A:test:nrepl -e "(-main)" -r
