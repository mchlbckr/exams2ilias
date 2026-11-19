## Test environments

* macOS Tahoe 26.5.2 (arm64), R 4.6.2

## R CMD check results

* 0 errors | 0 warnings | 1 note
* The local HTML manual validation was skipped because the installed HTML Tidy
  was too old.

## Package purpose

`exams2ilias` exports exercises from `R/exams` to question pools for the
learning management system `ILIAS` using an `ILIAS`-specific QTI 1.2 rendering
path tested with `ILIAS` 9.20 and 10.12.

## ILIAS 10.12 validation

An 11-question test pool imported successfully using the default template.
Manual checks covered cloze scoring, numeric tolerance, and multiple-choice
partial credit. A separate text question exported with `metasolution = TRUE`
awarded 1 point for `SE` and 0 points for `SD` in the ILIAS preview.
