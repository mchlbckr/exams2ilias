# exams2ilias

`exams2ilias` is a small standalone R package that builds ILIAS question pool
exports on top of [`exams`](https://www.r-exams.org/) while keeping the
ILIAS-specific QTI rendering logic inside this package. Version `0.0.1`
focuses on the export structure that was validated against `ILIAS 9.17`.

## Install

```r
remotes::install_github("mchlbckr/exams2ilias")
```

## Minimal example

```r
library(exams2ilias)

outdir <- tempfile("ilias-")
dir.create(outdir)

exams2ilias(
  system.file("exercises/lm.Rmd", package = "exams"),
  n = 1,
  dir = outdir,
  name = "lm_ilias"
)
```

## Example with `mixed_stats_cloze.Rmd`

The package bundles a cloze example in `inst/examples`:

```r
library(exams2ilias)

outdir <- tempfile("ilias-")
dir.create(outdir)

exams2ilias(
  system.file("examples", "mixed_stats_cloze.Rmd", package = "exams2ilias"),
  n = 10,
  dir = outdir,
  name = "mixed_stats_cloze",
  xmlcollapse = FALSE,
  solutionswitch = FALSE
)
```

This produces `mixed_stats_cloze_qpl.zip` in `outdir`.
