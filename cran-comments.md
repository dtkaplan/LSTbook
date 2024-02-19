## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Per Feb 8, 2024 request from Benjamin Altmann at CRAN, 
    - addressed error in LICSENSE file, removed inappropriate "cph" entry in Authors@R field, and listed an additional contributor.
    - added on.exit() in dag_draw.R to restore options before exit.
    - removed \dontrun{} and examples that had been commented out
    - added \value{} field for all exported functions
    - re-wrote reference in description field of DESCRIPTION file in proper format.
