## Test environments

* local macOS Tahoe 26.3.1, R 4.5.3

## R CMD check results

* 0 errors | 0 warnings | 0 notes

## Package purpose

`exams2ilias` exports exercises from `R/exams` to question pools for the
learning management system `ILIAS` using an `ILIAS`-specific QTI 1.2 rendering
path validated against `ILIAS` 9.17.

## Additional checks

* win-builder (R-devel, R 4.6.0 RC, Windows Server 2022): 1 NOTE
  * The only NOTE is a CRAN incoming spelling hint for the domain-specific
    system name `ILIAS` in `DESCRIPTION`.
  * No errors or warnings were reported by win-builder.
* additional cross-platform check (e.g. R-hub): pending
