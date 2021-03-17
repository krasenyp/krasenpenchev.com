.PHONY: all publish

all: publish

publish: publish.el
	@echo "Compiling SCSS"
	sassc ./static/styles/base.scss ./static/styles/base.css
	@echo "Publishing with current Emacs configurations."
	emacs --batch --load publish.el --funcall org-publish-all

clean:
	@echo "Cleaning up."
	@rm -rvf *.elc
	@rm -rvf public
	@rm -rvf ~/.org-timestamps/*
	@rm -rvf org/posts/sitemap.org