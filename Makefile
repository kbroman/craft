all: doc data/font_sets.RData

doc:
	R -e 'devtools::document()'

data/font_sets.RData: inst/scripts/grab_fonts.R
	cd $(<D);R -e "source('$(<F)')"
