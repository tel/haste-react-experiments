
all:
	hastec hs/index.hs --with-js=hs/base.js
	rm -f index.js
	ln -s hs/index.js index.js
