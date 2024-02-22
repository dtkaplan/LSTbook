## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Per Feb 8, 2024 request from Benjamin Altmann at CRAN, 
    - addressed error in LICENSE file, removed inappropriate "cph" entry in Authors@R field, and listed an additional contributor.
    - added on.exit() in dag_draw.R to restore options before exit.
    - removed \dontrun{} and examples that had been commented out
    - added \value{} field for all exported functions
    - re-wrote reference in description field of DESCRIPTION file in proper format.

* Per Feb 21, 2024 request from Benjamin Altman at CRAN,
    - added \value{} field for point_plot() and model_plot. These had been omitted when handling the Feb 8 request.
    - corrected mis-use of expect_snapshot_file() so that png files are saved as temporary files rather than in the testthat/directory. 
