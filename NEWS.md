# LSTbook 0.5.0

First CRAN release

# LSTbook 0.5.1

* A request for a prediction interval for the output of logistic regression now generates an error rather than a warning.

# LSTbook 0.5.1.9001

* print.datasim(), print.model_object() were not being exported. Now they are.

# LSTbook 0.5.1.9002

* Adding documentation stubs for data sources that are too big to include in the package but are available on the web, such as `Natality_2014`. 

* Changed the name of the FEV data frame to CRDS ("childhood respiratory disease study") to avoid confusion with the FEV variable.

* model_eval() mistakenly changed name of first column to .response even when handed evaluation data directly. Fixed.

# LSTbook 0.5.1.9003

* Added random_terms() to generate random columns in a model matrix.

* Changed the name of the LSTbook function for sampling from "sample()" (which conflicts with other packages) to "take_sample()." All this just because I wanted the size parameter to be named "n" rather than "size"!

* Fixed a warning in "model_eval()" that led residuals not being calculated for zero-one response variables.

* Fix point_plot so error bars are dodged when there are multiple colors.

# LSTbook 0.6

Updated CRAN release 

# LSTbook 0.6.0.9000

* In point_plot(), categorical variables mapped to x now will show model annotations for all levels of x.

* A new argument to point_plot(), ncategorical=, controls how many levels will be shown for model annotations for color & faceting 
variables.

# LSTbook 0.6.1

* New version number so that r-universe.dev will pick up the package.

* Fixed order of colors for model values in `point_plot()`.

* Added `Natality_2014` with 10,000 randomly selected cases from the 3.99M in the `"dtkaplan/natality2014"`
package available by `remotes::install_github("dtkaplan/natality2014")`. That large data set
won't work in WebR installations.

* Ungrouped Sessions in College_database.rda and removed redundant College_grades.rda

* Added simple `shuffle()` function.

* Added `rterm()` for demonstrations of degrees of freedom.

* `trials()` avoids name conflicts (per issue #14)


