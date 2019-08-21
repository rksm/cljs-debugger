.PHONY: clean nrepl chrome


nrepl:
	clojure -A:dev -R:test

chrome:
	chromium \
	  --remote-debugging-port=9222 \
	  --no-first-run \
	  --user-data-dir=chrome-user-profile

clean:
	rm -rf resources/public/js target out .cpcache
