
# called on each darcs commit
commithook: site

sitebuild site: hakyll index.html
	./hakyll build

index.html:
	ln -s README.html index.html

siteclean: hakyll
	./hakyll clean

sitepreview: hakyll
	./hakyll preview 8001

hakyll: hakyll.hs
	ghc --make -Wall hakyll.hs

# misc
tag: TAGS

TAGS: *.el *.hs *.markdown Makefile
	etags *.el *.hs *.markdown Makefile

clean:
	rm -f *.o *.hi

Clean: clean
	rm -rf _cache _site TAGS

