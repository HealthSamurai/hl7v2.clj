.PHONY: test deploy jar repl

test:
	clojure -A:test:runner

jar:
	clojure -A:jar

clojars-push:
	clojure -A:deploy

repl:
	clj -A:test:nrepl -m nrepl.cmdline --middleware "[cider.nrepl/cider-middleware refactor-nrepl.middleware/wrap-refactor]"

