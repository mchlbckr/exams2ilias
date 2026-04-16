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

## Bundled examples

The package bundles self-contained statistics examples for the main question
types supported by `exams2ilias`:

```r
example_dir <- system.file("examples", package = "exams2ilias")
list.files(example_dir, pattern = "\\.[Rr]md$", full.names = TRUE)
```

These include `stats_cloze.Rmd`, `stats_schoice.Rmd`, `stats_mchoice.Rmd`,
`stats_num.Rmd`, and `stats_string.Rmd`.

To export the full example set, source the bundled helper script:

```r
source(file.path(example_dir, "generate_examples.R"))

outdir <- tempfile("ilias-examples-")
dir.create(outdir)

generate_example_exports(outdir)
```

This writes one `_qpl.zip` per example and an additional combined
`stats_examples_qpl.zip` to `outdir`.

You can also export a single example directly:

```r
library(exams2ilias)

outdir <- tempfile("ilias-")
dir.create(outdir)

exams2ilias(
  file.path(example_dir, "stats_cloze.Rmd"),
  n = 1,
  dir = outdir,
  name = "stats_cloze",
  xmlcollapse = FALSE,
  solutionswitch = FALSE
)
```

The cloze example uses `exams::add_cloze()` and `format_metainfo()` so it can
serve as a template for new ILIAS-ready cloze exercises.
