.PHONY: all term termconda root remotes env github arch jammy focal win deps style lint check cran site build install rpkg tinytex latex rclean rcleanall clean termclean deepclean

all: clean deps style man/*.Rd check build install README.md site

# terminal

term: termclean
	@(cd .bash && make)
	@(cd .vim && make)

termconda: term
	@(cd .bash && make conda)

# system

root:
	@Rscript .r-set-pkg/r-set-pkg-rprojroot.R $(PWD)

remotes:
	@Rscript .r-set-pkg/r-set-pkg-remotes.R $(PWD)

env:
	@Rscript .r-set-env/r-set-env.R $(PWD)

github:
	@Rscript .r-set-pkg/r-set-pkg-dev-github.R $(PWD)
	@Rscript .r-set-pkg/r-set-pkg-proj-github.R $(PWD)

sysarch:
	@bash .sys/sys-arch/sys-arch.sh

sysubuntu:
	@bash .sys/sys-ubuntu/sys-ubuntu.sh

arch: clean env github
	@Rscript .r-set-profile/r-set-profile.R $(PWD) arch
	@Rscript .r-set-pkg/r-set-pkg-dev.R $(PWD) arch
	@Rscript .r-set-pkg/r-set-pkg-proj.R $(PWD) arch
	@Rscript .r-set-pkg/r-set-pkg-proj-version.R $(PWD) arch

jammy: clean env github
	@Rscript .r-set-profile/r-set-profile.R $(PWD) jammy
	@Rscript .r-set-pkg/r-set-pkg-dev.R $(PWD) jammy
	@Rscript .r-set-pkg/r-set-pkg-proj.R $(PWD) jammy
	@Rscript .r-set-pkg/r-set-pkg-proj-version.R $(PWD) jammy

focal: clean env github
	@Rscript .r-set-profile/r-set-profile.R $(PWD) focal
	@Rscript .r-set-pkg/r-set-pkg-dev.R $(PWD) focal
	@Rscript .r-set-pkg/r-set-pkg-proj.R $(PWD) focal
	@Rscript .r-set-pkg/r-set-pkg-proj-version.R $(PWD) focal

# R package related

deps:
	@find .r-dependencies -name \*.R -exec cp {} R \;
	@Rscript .bin/bin.R $(PWD)
	@Rscript .data-process/data-process.R

style:
	@Rscript -e "styler::style_dir(exclude_dirs = c('.library', '.notes'), filetype = c('.R', '.Rmd'))"

lint:
	@Rscript -e "lintr::lint_dir('R')"
	@Rscript -e "lintr::lint_dir('.r-dependencies')"
	@Rscript -e "lintr::lint_dir('.r-load-all')"
	@Rscript -e "lintr::lint_dir('.r-misc')"
	@Rscript -e "lintr::lint_dir('.r-set-env')"
	@Rscript -e "lintr::lint_dir('.r-set-pkg')"
	@Rscript -e "lintr::lint_dir('.r-set-profile')"
	@Rscript -e "lintr::lint_dir('.r-writeup')"
	@Rscript -e "lintr::lint_dir('tests')"
	@Rscript -e "lintr::lint_dir('.tests-benchmark')"
	@Rscript -e "lintr::lint_dir('.tests-external')"

README.md: README.Rmd R/*.R
	@Rscript -e "rmarkdown::render('README.Rmd')"

man/*.Rd: R/*.R
	@Rscript -e "devtools::document()"

check:
	@Rscript -e "devtools::check(cran = FALSE)"

cran:
	@Rscript -e "devtools::check(cran = TRUE)"

# RUN INTERACTIVELY
# rhub:
#	rhub::validate_email(email = 'r.jeksterslab@gmail.com', token = 'TOKEN')
#	rhub::check_for_cran(email = 'r.jeksterslab@gmail.com')

site:
	@Rscript -e "pkgdown::build_site()"

build:
	@Rscript -e "devtools::build(path = '.')"

install:
	@Rscript -e "devtools::install(pkg = '.')"

rpkg: deps style README.md man/*.Rd check build install site

# latex related

tinytex:
	@Rscript -e "tinytex::install_tinytex(bundle = 'TinyTeX-2', force = TRUE)"

latex:
	@Rscript -e "source('latex/r-scripts/latex-make.R'); LatexMake(clean = TRUE)"
	@rm -rf _detritus
	@echo "Run 'make tinytex' if the process failed."

# cleaning

clean:
	@rm -rf README.html
	@rm -rf README.md
	@rm -rf docs/*
	@rm -rf man/*
	@rm -rf NAMESPACE
	@rm -rf latex/pdf/*.*
	@rm -rf .data-process/*.Rds
	@find .detritus/ -type f -not -name '.gitignore' -delete

termclean:
	@rm -rf ~/.vim

rclean:
	@Rscript .r-set-pkg/r-set-pkg-clean-default.R $(PWD)

rcleanall: rclean
	@rm -rf .library/*

deepclean: clean termclean rcleanall
	@rm -rf .git*
	@rm -rf .library
	@rm -rf .notes
	@rm -rf .bash
	@rm -rf .bin
	@rm -rf .data*
	@rm -rf .detritus
	@rm -rf docs
	@rm -rf julia
	@rm -rf latex
	@rm -rf .log
	@rm -rf .r-*
	@rm -rf .sim
	@rm -rf .sys
	@rm -rf .tests-*
	@rm -rf .vim
	@rm -rf _pkgdown.yml
	@rm -rf .clone.sh
	@rm -rf .covrignore
	@rm -rf latexmkrc
	@rm -rf Makefile
	@rm -rf NEWS.md
	@rm -rf project.Rproj
	@rm -rf *.html
	@rm -rf *.Rmd
	@rm -rf *.uuid
	@rm -rf *.tar.gz
	@find . -maxdepth 1 -name '*.md' ! -name 'LICENSE.md' -type f -delete