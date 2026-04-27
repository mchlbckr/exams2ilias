## Test environments

* local macOS Tahoe 26.3.1, R 4.5.3

## Resubmission

This resubmission addresses the two points raised in the previous CRAN review:

* Added a methods reference in `Description`:
  Zeileis, Umlauf, and Leisch (2014) <doi:10.18637/jss.v058.i01>.
* Fixed the vignette example in
  `vignettes/authoring-ilias-questions.Rmd` so the `Meta-information`
  example is shown as Markdown instead of being parsed as R code.

## R CMD check results

* 0 errors | 0 warnings | 1 note
* Remaining note on the local machine:
  * `unable to verify current time`

## Package purpose

`exams2ilias` exports exercises from `R/exams` to question pools for the
learning management system `ILIAS` using an `ILIAS`-specific QTI 1.2 rendering
path validated against `ILIAS` 9.17.

## Additional checks

* win-builder (R-devel, R 4.6.0 RC, Windows Server 2022): 1 NOTE
  * The only NOTE is a CRAN incoming spelling hint for the domain-specific
    system name `ILIAS` in `DESCRIPTION`.
  * No errors or warnings were reported by win-builder.
* R-hub (`ubuntu-release`, Ubuntu 24.04.4 LTS, R 4.5.3): Status OK
  * No errors, warnings, or notes were reported by the R-hub check.
  * Workflow run: <https://github.com/mchlbckr/exams2ilias/actions/runs/24819446978>
