all: output/amalgamation.js output/index.html

.PHONY: output/amalgamation.js
output/amalgamation.js:
	mkdir -p $(dir $@)
	pulp browserify -t $@

output/index.html: src/index.html
	mkdir -p $(dir $@)
	cp $< $@
