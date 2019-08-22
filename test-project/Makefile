.PHONY: nrepl chrome clean

chrome:
	chromium \
	  --remote-debugging-port=9222 \
	  --no-first-run \
	  --user-data-dir=chrome-user-profile

nrepl:
	clojure -R:cider -m cljs-debugger-example.nrepl

clean:
	rm -rf target .cpcache
