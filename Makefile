.PHONY: all build local localforce dotfiles project pkg tinytex clean cleanpkg cleantinytex cleanall coverage lint qmd

all: build latex

build: pkg clean
	@echo TinyTex...
	@Rscript -e "rProject::TinyTex(\"${PWD}\", force = FALSE)"
	@echo Styling...
	@Rscript -e "rProject::Style(\"${PWD}\")"
	@echo Linting...
	@Rscript -e "rProject::Lint(\"${PWD}\")"
	@echo Building dependencies...
	@Rscript -e "rProject::DataProcess(\"${PWD}\")"
	@Rscript -e "rProject::DataAnalysis(\"${PWD}\")"
	@Rscript -e "rProject::Dependencies(\"${PWD}\")"
	@echo Initial build...
	@Rscript -e "rProject::Build(\"${PWD}\")"
	@echo Precompiling vignettes...
	@Rscript -e "rProject::VignettesPrecompile(\"${PWD}\")"
	@echo Building project...
	@Rscript -e "rProject::Build(\"${PWD}\")"
	@echo Building website...
	@Rscript -e "rProject::Site(\"${PWD}\")"
	@echo Building manual...
	@Rscript -e "rProject::Manual(\"${PWD}\", project = Sys.getenv(\"PROJECT\"))"
	@echo Building CITATION.cff...
	@Rscript -e "rProject::CFF(\"${PWD}\")"

cleanall: clean cleanpkg cleantinytex

dotfiles:
	@echo Building dotfiles...
	@Rscript -e "source('tools/project.R') ; rProject::ConfigFiles(git_user)"

project:
	@echo Building project...
	@Rscript tools/make-project.R ${PWD}

pkg: project
	@echo Installing packages...
	@Rscript tools/make-packages.R ${PWD}

tinytex:
	@echo Installing TinyTex...
	@Rscript -e "rProject::TinyTex(\"${PWD}\", force = TRUE)"

local: project
	@echo Installing local applications...
	@Rscript -e "rProject::InstallLocal(all = TRUE)"
	@Rscript tools/make-config.R ${PWD}

localforce: project
	@echo Installing local applications...
	@Rscript -e "rProject::InstallLocal(all = TRUE, force = TRUE)"
	@Rscript tools/make-config.R ${PWD}

clean:
	@echo Cleaning...
	@Rscript -e "rProject::Clean(\"${PWD}\")"
	@rm -rf "${PWD}/TEMPDIR.*"

cleanpkg:
	@echo Cleaning packages...
	@Rscript -e "rProject::CleanPkg(\"${PWD}\")"

cleanproj:
	@echo Cleaning project...
	@Rscript -e "rProject::CleanProj(\"${PWD}\")"

cleantinytex:
	@echo Cleaning TinyTex...
	@Rscript -e "rProject::CleanTinyTex(\"${PWD}\")"

coverage:
	@echo Code coverage...
	@Rscript -e "rProject::Coverage(\"${PWD}\")"

lint:
	@echo Styling...
	@Rscript -e "rProject::Style(\"${PWD}\")"
	@echo Linting...
	@Rscript -e "rProject::Lint(\"${PWD}\")"

latex:
	@Rscript -e "rProject::LatexMake(\"${PWD}\")"

qmd: lint
	@Rscript qmd/r-script/prerender.R
	@quarto render ${PWD}
