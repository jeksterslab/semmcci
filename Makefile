.PHONY: all release build clean cleanall cleanpkg cleanproj cleanpush cleantinytex bib libcheck libformat bibrproject clone coverage data dependencies readme docs dotfiles aiglobal secretscan verify status spec log fixup upload sizecheck template tree conform public ai manifest install latex lint local localforce pdf pkg pkgdown project push quarto style tinytex tinytexforce vignettes upgrade

push: build docs latex coverage cleanpush

project:
	@echo "\n\nBuilding project...\n\n"
	@# a project without make-project.R gets the current one from rProject on
	@# GitHub (it installs the newest rProject first), so an older rProject in
	@# the image cannot stop the build; Template() of the image only offline
	@test -f .setup/scripts/make-project.R || Rscript -e "download.file(\"https://raw.githubusercontent.com/ijapesigan/rProject/main/inst/template/common/dot-setup/scripts/make-project.R\", \".setup/scripts/make-project.R\", quiet = TRUE)" || Rscript -e "rProject::Template(\"${PWD}\")"
	@Rscript .setup/scripts/make-project.R ${PWD} ${LOCK}

upgrade:
	@echo "\n\nUpgrading rProject (the next make auto updates the lock)...\n\n"
	@# a project without make-project.R gets the current one from rProject on
	@# GitHub (it installs the newest rProject first), so an older rProject in
	@# the image cannot stop the build; Template() of the image only offline
	@test -f .setup/scripts/make-project.R || Rscript -e "download.file(\"https://raw.githubusercontent.com/ijapesigan/rProject/main/inst/template/common/dot-setup/scripts/make-project.R\", \".setup/scripts/make-project.R\", quiet = TRUE)" || Rscript -e "rProject::Template(\"${PWD}\")"
	@Rscript .setup/scripts/make-project.R ${PWD} UPGRADE

template:
	@echo "\n\nWriting template files...\n\n"
	@Rscript -e "rProject::Template(\"${PWD}\")"

tree:
	@Rscript -e "rProject::Tree(\"${PWD}\")"

# make conform [REFERENCE=<project>] [REMOTE=TRUE] [LINT=TRUE] [TESTS=TRUE] [FIX=TRUE [TOPIC=<topic>]]
# compares the project with the standard; FIX=TRUE writes the missing files
# and removes the strays (main.tex), then updates the log and the upload folder;
# LINT=TRUE runs make lint and TESTS=TRUE runs the unit tests (findings, logs in tempdir)
conform:
	@echo "\n\nChecking the project against the standard...\n\n"
	@if [ "$(FIX)" = "TRUE" ]; then \
	Rscript -e "rProject::ConformFix(\"${PWD}\", reference = if (nzchar(\"$(REFERENCE)\")) \"$(REFERENCE)\", topic = \"$(or ${TOPIC},conform)\", remove = TRUE, lint = $(or ${LINT},FALSE), tests = $(or ${TESTS},FALSE))"; \
	else \
	Rscript -e "rProject::Conform(\"${PWD}\", reference = if (nzchar(\"$(REFERENCE)\")) \"$(REFERENCE)\", remote = $(or ${REMOTE},FALSE), lint = $(or ${LINT},FALSE), tests = $(or ${TESTS},FALSE))"; \
	fi

pkg:
	@echo "\n\nInstalling packages...\n\n"
	@Rscript .setup/scripts/make-packages.R ${PWD}

dotfiles:
	@echo "\n\nBuilding dotfiles...\n\n"
	@Rscript .setup/scripts/make-config.R ${PWD}

aiglobal:
	@echo "\n\nInstalling global AI instructions...\n\n"
	@Rscript -e "rProject::AIGlobal(\"${PWD}\")"

clean:
	@echo "\n\nCleaning...\n\n"
	@Rscript -e "rProject::Clean(\"${PWD}\")"
	@rm -rf "${PWD}/TEMPDIR.*"

cleanpush:
	@echo "\n\nCleaning in prep for push...\n\n"
	@Rscript -e "rProject::Clean(\"${PWD}\", push = TRUE)"

cleanpkg:
	@echo "\n\nCleaning packages...\n\n"
	@Rscript -e "rProject::CleanPkg(\"${PWD}\")"

cleantinytex:
	@echo "\n\nCleaning TinyTex...\n\n"
	@Rscript -e "rProject::CleanTinyTex(\"${PWD}\")"

cleanproj:
	@echo "\n\nCleaning project...\n\n"
	@Rscript -e "rProject::CleanProj(\"${PWD}\")"

# rProject (type <- "rProject" in project.R) rebuilds bib.bib
# from the public lib-* repositories at every build; git runs without
# the user's config (it may rewrite https to ssh) and never prompts
bib:
	@echo "\n\nBuilding bib.bib...\n\n"
	@GIT_CONFIG_GLOBAL=/dev/null GIT_TERMINAL_PROMPT=0 Rscript -e "rProject::Bib(\"${PWD}\", bib_lib = $$(grep -q '^type <- \"rProject\"' .setup/scripts/project.R && echo TRUE || echo FALSE))"

libcheck:
	@echo "\n\nChecking the lib-* entries...\n\n"
	@Rscript -e "print(rProject::LibCheck(\"${PWD}\", root = if (nzchar(\"${ROOT}\")) \"${ROOT}\" else NULL))"

libformat:
	@echo "\n\nPutting the lib-* entries in the house layout...\n\n"
	@Rscript -e "rProject::LibFormat(\"${PWD}\", write = TRUE)"

bibrproject:
	@echo "\n\nCleaning project...\n\n"
	@Rscript -e "rProject::Bib(\"${PWD}\", bib_lib = TRUE)"

tinytex: 
	@echo "\n\nTinyTex...\n\n"
	@Rscript -e "rProject::TinyTex(\"${PWD}\", force = FALSE)"

tinytexforce: 
	@echo "\n\nTinyTex...\n\n"
	@Rscript -e "rProject::TinyTex(\"${PWD}\", force = TRUE)"

style:
	@echo "\n\nStyling...\n\n"
	@Rscript -e "rProject::Style(\"${PWD}\")"

lint: style
	@echo "\n\nLinting...\n\n"
	@Rscript -e "rProject::Lint(\"${PWD}\")"
	@Rscript -e "if (exists('SizeCheck', envir = asNamespace('rProject'))) rProject::SizeCheck(\"${PWD}\")"

# the agents' command line tools, on the host (no R, no Apptainer): installs
# the missing ones (in ~/work/tools, linked, where ~/work exists); UPDATE=TRUE updates them
agents:
	@bash .setup/scripts/agents.sh $(if $(UPDATE),--update,)

sizecheck:
	@echo "\n\nChecking Overleaf limits...\n\n"
	@Rscript -e "rProject::SizeCheck(\"${PWD}\", error = TRUE)"

# after make clone: checks what make auto would publish
public:
	@echo "\n\nChecking the public export...\n\n"
	@Rscript -e "rProject::Public(\"${PWD}\")"

secretscan:
	@echo "\n\nScanning for secrets...\n\n"
	@Rscript -e "rProject::SecretScan(\"${PWD}\")"

# apptainer exec -B ~/work:$(readlink -f ~/work) <sif> make verify RECORD=TRUE (on Roar, add -B /scratch/ibp5092)
# pulls the private repository into .verify (where SSH works in the
# container), then checks the push; on Roar, first run on the host:
# bash .setup/scripts/verify.sh
linked:
	@echo "\n\nComparing the linked files with functions...\n\n"
	@Rscript -e "rProject::Linked(\"${PWD}\", functions = if (nzchar(\"$(FUNCTIONS)\")) \"$(FUNCTIONS)\")"

verify:
	@echo "\n\nVerifying the Overleaf push...\n\n"
	@bash .setup/scripts/verify.sh || echo "\n\nWARNING: .verify was not updated (no SSH here). Run bash .setup/scripts/verify.sh on the host, then make verify again.\n\n"
	@Rscript -e "rProject::Verify(\"${PWD}\", record = $(or ${RECORD},FALSE))"

status:
	@echo "\n\nReporting the private and public repositories...\n\n"
	@SET="$(SET)" bash .setup/scripts/repo-status.sh $(PROJECTS)

# make spec TOPIC=<topic>
spec:
	@echo "\n\nCreating a spec...\n\n"
	@Rscript -e "rProject::Spec(\"${PWD}\", topic = \"${TOPIC}\")"

# make log TOPIC=<topic> SUMMARY="<first line of the commit message>"
# make log TOPIC=<topic> UPDATE=TRUE refreshes the file lists
log:
	@echo "\n\nWriting the change-set log...\n\n"
	@Rscript -e "rProject::Log(\"${PWD}\", topic = \"${TOPIC}\", summary = \"${SUMMARY}\", update = $(or ${UPDATE},FALSE))"

# make fixup TOPIC=<topic> SUMMARY="<first line of the fix-up's commit message>"
# adds a fix-up section, a pending Pushes row, and a pending index row to the topic's log
fixup:
	@echo "\n\nAdding a fix-up to the change-set log...\n\n"
	@Rscript -e "rProject::FixUp(\"${PWD}\", topic = \"${TOPIC}\", summary = \"${SUMMARY}\")"

# make upload TOPIC=<topic> [FIXUP=TRUE] builds the upload folder from the log's lists
upload:
	@echo "\n\nBuilding the upload folder...\n\n"
	@Rscript -e "rProject::Upload(\"${PWD}\", topic = \"${TOPIC}\", fixup = $(or ${FIXUP},FALSE))"

# project AI instructions: AGENTS.md, CLAUDE.md, .github/copilot-instructions.md
ai:
	@echo "\n\nWriting the project's AI instructions...\n\n"
	@Rscript -e "rProject::AI(\"${PWD}\")"

# data manifest: .setup/ai/data.md (private)
manifest:
	@echo "\n\nWriting the data manifest...\n\n"
	@Rscript -e "rProject::DataManifest(\"${PWD}\")"

data:
	@echo "\n\nBuilding data...\n\n"
	@Rscript -e "rProject::DataProcess(\"${PWD}\")"

dependencies:
	@echo "\n\nBuilding dependencies...\n\n"
	@Rscript -e "rProject::Dependencies(\"${PWD}\")"

vignettes:
	@echo "\n\nInitial build...\n\n"
	@Rscript -e "rProject::Build(\"${PWD}\")"
	@Rscript -e "rProject::DataAnalysis(\"${PWD}\")"
	@echo "\n\nPrecompiling vignettes...\n\n"
	@Rscript -e "rProject::VignettesPrecompile(\"${PWD}\")"

build: project pkg tinytex lint data dependencies bib vignettes
	@echo "\n\nBuilding package...\n\n"
	@Rscript -e "rProject::Build(\"${PWD}\")"

coverage:
	@echo "\n\nCode coverage...\n\n"
	@Rscript -e "rProject::Coverage(\"${PWD}\")"

latex:
	@echo "\n\nCompiling latex...\n\n"
	@Rscript -e "rProject::LatexMake(\"${PWD}\")"

pdf:
	@echo "\n\nCompiling latex...\n\n"
	@Rscript -e "rProject::LatexMake(\"${PWD}\", clean = TRUE)"

pkgdown:
	@echo "\n\nBuilding pkgdown website...\n\n"
	@Rscript -e "rProject::Site(\"${PWD}\")"

quarto:
	@echo "\n\nRendering quarto...\n\n"
	@Rscript -e "rProject::Quarto(\"${PWD}\")"

readme:
	@echo "\n\nBuilding README.md...\n\n"
	@Rscript -e "rProject::ReadMe(\"${PWD}\")"

docs: readme
	@echo "\n\nBuilding manual...\n\n"
	@Rscript -e "rProject::Manual(\"${PWD}\", project = Sys.getenv(\"PROJECT\"))"
	@echo "\n\nBuilding CITATION.cff...\n\n"
	@Rscript -e "rProject::CFF(\"${PWD}\")"

all: project lint build docs latex pkgdown quarto coverage

cleanall: clean cleanpush cleanpkg cleantinytex

install: project pkg data clean
	@echo "\n\nInstalling...\n\n"
	@Rscript .setup/scripts/make-install.R ${PWD}

local: project dotfiles
	@echo "\n\nInstalling local applications...\n\n"
	@Rscript -e "rProject::InstallLocal(all = TRUE)"
	@Rscript .setup/scripts/make-config.R ${PWD}

localforce: project dotfiles
	@echo "\n\nInstalling local applications...\n\n"
	@Rscript -e "rProject::InstallLocal(all = TRUE, force = TRUE)"
	@Rscript .setup/scripts/make-config.R ${PWD}

clone: project
	@bash .setup/scripts/clone.sh

# make all, the secret scan, then make auto (commit, push, tag); it stops at the
# first failure. Then look at the runs: gh run list -R <public repository>
release: all secretscan auto
	@echo "\n\nReleased. Check that Make Project started on the pushed commit.\n\n"

auto: clone
	@Rscript -e "if (exists('Public', envir = asNamespace('rProject'))) rProject::Public(\"${PWD}\") else message('\n\nWARNING: rProject is older than 0.0.33; the public export was NOT checked. Run make project.\n\n')"
	@Rscript -e "if (exists('Lock', envir = asNamespace('rProject'))) rProject::Lock(\"${PWD}\")"
	@git add --all
	@git commit -m "Automated build."
	@git push origin main
	@V=$$(sed -n 's/^Version: *//p' DESCRIPTION 2> /dev/null); \
	if [ -n "$$V" ] && ! git rev-parse -q --verify "refs/tags/v$$V" > /dev/null; then \
		git tag -a "v$$V" -m "v$$V" && git push origin "v$$V"; \
	fi
