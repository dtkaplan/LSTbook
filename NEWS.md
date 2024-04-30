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

* Added random_terms() to generate random columns in a model matrix.
