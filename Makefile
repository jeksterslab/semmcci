.PHONY: all push root remotes dev proj env rsetup arch ubuntu win deps style lint check cran site build install rpkg cov xz data tinytex latex term hpc rclean rcleanall clean deepclean

all: clean rsetup rpkg

push: clean rsetup rpkg tinytex cran latex
	@bash .clone.sh

root:
	@Rscript r-set-pkg/r-set-pkg-rprojroot.R

remotes: root
	@Rscript r-set-pkg/r-set-pkg-remotes.R

dev: remotes
	@Rscript r-set-pkg/r-set-pkg-dev.R
	@Rscript r-set-pkg/r-set-pkg-dev-github.R

proj: dev
	@Rscript r-set-pkg/r-set-pkg-proj.R
	@Rscript r-set-pkg/r-set-pkg-proj-version.R
	@Rscript r-set-pkg/r-set-pkg-proj-github.R

env:
	@Rscript r-set-env/r-set-env.R

rsetup: proj env rclean

arch:
	@bash sys/sys-arch/sys-arch-dev.sh
	@bash sys/sys-arch/sys-arch-proj.sh

ubuntu:
	@echo "Building..."

win:
	@echo "Building..."

# the following assumes that the system is setup properly

# R package related

deps:
	@find r-dependencies -name \*.R -exec cp {} R \;
	@Rscript bin/bin.R

style:
	@Rscript -e "styler::style_dir(exclude_dirs = c('.library', '.notes'), filetype = c('.R', '.Rmd'))"

lint:
	@Rscript -e "lintr::lint_dir('R')"
	@Rscript -e "lintr::lint_dir('r-dependencies')"
	@Rscript -e "lintr::lint_dir('r-writeup')"
	@Rscript -e "lintr::lint_dir('tests')"
	@Rscript -e "lintr::lint_dir('tests-benchmark')"
	@Rscript -e "lintr::lint_dir('tests-external')"

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

rpkg: deps style README.md man/*.Rd check site build install

# R package miscellaneous

cov:
	@Rscript -e "covr::package_coverage()"

xz:
	@Rscript -e "tools::resaveRdaFiles(paths = 'data', compress = 'xz')"

data:
	@Rscript -e "tools::resaveRdaFiles(paths = 'data', version = 3)"

# latex related

tinytex:
	@Rscript -e "tinytex::install_tinytex(bundle = 'TinyTeX-2', force = TRUE)"

latex:
	@Rscript -e "source('latex/r-scripts/latex-make.R'); LatexMake(clean = TRUE)"
	@echo "Run 'make tinytex' if the process failed."

# terminal

term:
	@yes 2>/dev/null | cp -rf bash/.bash* ~
	@yes 2>/dev/null | cp -rf vim/.vim* ~
	-@bash ~/.vimplugins

# simulation related

hpc:
	@cat sim/hpc/.bashrc_hpc >> ~/.bashrc

# cleaning

rclean: root
	@Rscript r-set-pkg/r-set-pkg-clean-default.R

rcleanall: rclean
	@rm -rf .library/*

clean:
	@rm -rf hpc*.err
	@rm -rf hpc*.out
	@rm -rf local-linux*.err
	@rm -rf local-linux*.out
	@rm -rf README.html
	@rm -rf README.md
	@rm -rf docs/*
	@rm -rf man/*
	@rm -rf NAMESPACE
	@rm -rf latex/pdf/*.*
	@rm -rf data-process/*.Rds
	@find detritus/ -type f -not -name '.gitignore' -delete

termclean:
	@rm -rf ~/.vim

deepclean: clean
	@rm -rf .github
	@rm -rf .library
	@rm -rf .notes
	@rm -rf bash
	@rm -rf bin
	@rm -rf data-process
	@rm -rf data-raw
	@rm -rf detritus
	@rm -rf docs
	@rm -rf julia
	@rm -rf latex
	@rm -rf log
	@rm -rf r-dependencies
	@rm -rf r-load-all
	@rm -rf r-misc
	@rm -rf r-set-env
	@rm -rf r-set-pkg
	@rm -rf r-writeup
	@rm -rf sim
	@rm -rf sys
	@rm -rf tests-benchmark
	@rm -rf tests-external
	@rm -rf detritus
	@rm -rf vim
	@rm -rf _pkgdown.yml
	@rm -rf .clone.sh
	@rm -rf .covrignore
	@rm -rf .git
	@rm -rf .gitignore
	@rm -rf .Rbuildignore
	@rm -rf .Renviron
	@rm -rf .Rprofile
	@rm -rf latexmkrc
	@rm -rf Makefile
	@rm -rf NEWS.md
	@rm -rf project.Rproj
	@rm -rf README.Rmd
	@rm -rf *.uuid
	@rm -rf *.tar.gz
