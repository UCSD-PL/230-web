#PANDOC=~/.cabal/bin/pandoc --columns=80  -s --mathjax --slide-level=2  
PANDOC=pandoc --columns=80  -s --mathjax --slide-level=2  
SLIDY=$(PANDOC) -t slidy
DZSLIDES=$(PANDOC) --highlight-style tango --css=slides.css -w dzslides 
HANDOUT=$(PANDOC) --highlight-style tango --css=text.css -w html5 

####################################################################

# clear out all suffixes
.SUFFIXES:
# list only those we use
.SUFFIXES: .html .markdown

slides: 
	for file in *.lhs ; do $(DZSLIDES) $$file -o $$file.slides.html; done
#	for file in *.markdown; do $(HANDOUT) $$file -o $$file.handout.html; done
#	for file in *.markdown; do $(PANDOC) $$file -o $$file.pdf; done

all:

.markdown.html:
	$(DZSLIDES) $< -o $@

clean:
	rm -f *.pdf *.html *.class
