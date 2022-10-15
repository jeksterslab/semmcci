.PHONY: r all sys sif term termconda root remotes env pkg deps style lint cov readme document check cran site build install vignettes tinytex latex manual clean termclean rclean rcleanall deepclean

r: clean pkg deps style document check build install vignettes readme site manual

all: r latex

sys: term tinytex

# apptainer

sif:
	@sudo -E bash .r-sif/apptainer.sh

# terminal

term: termclean
	@(cd .bash && make)
	@(cd .vim && make)

termconda: term
	@(cd .bash && make conda)

# R package related

root:
	@Rscript .r-set-pkg/r-set-pkg-rprojroot.R $(PWD)

remotes:
	@Rscript .r-set-pkg/r-set-pkg-remotes.R $(PWD)

env:
	@Rscript .r-set-env/r-set-env.R $(PWD)
	@Rscript .r-buildignore/r-buildignore.R
	@grep -q "\S" .Rbuildignore
	@Rscript .r-set-profile/r-set-profile.R $(PWD)
	
pkg: clean env
	@Rscript .r-set-pkg/r-set-pkg-dev.R $(PWD)
	@Rscript .r-set-pkg/r-set-pkg-dev-github.R $(PWD)
	@Rscript .r-set-pkg/r-set-pkg-proj.R $(PWD)
	@Rscript .r-set-pkg/r-set-pkg-proj-github.R $(PWD)
	@Rscript .r-set-pkg/r-set-pkg-proj-version.R $(PWD)

deps:
	@find .r-dependencies -name \*.R -exec cp {} R \;
	@Rscript .bin/bin.R $(PWD)
	@Rscript .data-process/data-process.R

style:
	@Rscript .r-lint/r-lint.R

lint:
	@Rscript .r-lint/r-lint.R

cov:
	@Rscript -e "covr::package_coverage()"

readme:
	@Rscript -e "devtools::build_readme()"

document:
	@Rscript -e "devtools::document()"

check:
	@Rscript -e "devtools::check(cran = FALSE)"

cran:
	@Rscript -e "devtools::check(cran = TRUE)"

site:
	@Rscript -e "pkgdown::build_site()"

build:
	@Rscript -e "devtools::build(path = '.', vignettes = FALSE)"

install:
	@Rscript -e "devtools::install(pkg = '.')"

vignettes:
	@Rscript .r-vignettes/precompile.R
	@rm -rf vignettes/*.orig
	@Rscript -e "devtools::build_vignettes(pkg = '.')"

# latex related

tinytex:
	@Rscript .r-tinytex/r-tinytex.R $(PWD)

latex:
	@Rscript -e "source('latexsrc/r-scripts/latex-make.R'); LatexMake(clean = TRUE)"
	@rm -rf _detritus

# manual

manual:
	@Rscript .r-hub/manual.R

# cleaning

clean:
	@rm -rf README.html
	@rm -rf README.md
	@rm -rf doc/*
	@rm -rf docs/*
	@rm -rf man/*
	@rm -rf NAMESPACE
	@rm -rf latex/pdf/*.*
	@rm -rf fig-vignettes-*
	@rm -rf vignettes/fig-vignettes-*
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
	@rm -rf doc
	@rm -rf docs
	@rm -rf julia
	@rm -rf latexsrc
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
