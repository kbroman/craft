all: doc data/font_sets.RData docs/craft.html

doc:
	R -e 'devtools::document()'

data/font_sets.RData: inst/scripts/grab_fonts.R
	cd $(<D);R -e "source('$(<F)')"

data/Rlogo.RData: inst/scripts/r_logo.R
	cd $(<D);R -e "source('$(<F)')"

docs/craft.html: vignettes/craft.Rmd
	cd $(<D);R -e "rmarkdown::render('$(<F)')"
	mv $(<D)/$(@F) $@
