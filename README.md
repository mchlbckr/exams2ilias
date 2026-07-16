# exams2ilias

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/exams2ilias)](https://CRAN.R-project.org/package=exams2ilias)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/exams2ilias)](https://CRAN.R-project.org/package=exams2ilias)
[![R-CMD-check](https://github.com/mchlbckr/exams2ilias/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mchlbckr/exams2ilias/actions/workflows/R-CMD-check.yaml)
[![License:
GPL-2 | GPL-3](https://img.shields.io/badge/License-GPL--2%20%7C%20GPL--3-blue.svg)](https://www.r-project.org/Licenses/)
[![GitHub
issues](https://img.shields.io/github/issues/mchlbckr/exams2ilias)](https://github.com/mchlbckr/exams2ilias/issues)
[![GitHub
stars](https://img.shields.io/github/stars/mchlbckr/exams2ilias?style=social)](https://github.com/mchlbckr/exams2ilias/stargazers)
<!-- badges: end -->

`exams2ilias` is a small standalone R package that builds ILIAS question pool
exports on top of [`exams`](https://www.r-exams.org/) while keeping the
ILIAS-specific QTI rendering logic inside this package. The export structure
is validated against `ILIAS 9.20`.

## Install

Install the latest development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("mchlbckr/exams2ilias")
```

The CRAN release is the stable version, but may be behind GitHub:

```r
install.packages("exams2ilias")
```

## Minimal example

```r
library(exams2ilias)

outdir <- tempfile("ilias-")
dir.create(outdir)

exams2ilias(
  "lm.Rmd",
  n = 1,
  dir = outdir,
  name = "lm_ilias"
)
```

Exercises bundled with `exams` can be addressed by file name, such as
`"lm.Rmd"`, `"ttest.Rmd"`, or `"boxplots.Rmd"`. Use `system.file()` only when
you want to reference exercises bundled with `exams2ilias` itself.

## Bundled exercises

The package bundles self-contained statistics exercises for the main question
types supported by `exams2ilias`:

```r
example_dir <- system.file("exercises", package = "exams2ilias")
list.files(example_dir, pattern = "\\.[Rr]md$", full.names = TRUE)
```

These include `stats_cloze.Rmd`, `stats_schoice.Rmd`, `stats_mchoice.Rmd`,
`stats_num.Rmd`, and `stats_string.Rmd`.

To export the full set, source the bundled helper script:

```r
source(file.path(example_dir, "generate_examples.R"))

outdir <- tempfile("ilias-examples-")
dir.create(outdir)

generate_example_exports(outdir)
```

This writes one `_qpl.zip` per example and an additional combined
`stats_examples_qpl.zip` to `outdir`.

You can also export a single exercise directly:

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

## ILIAS authoring notes

ILIAS renders dropdown labels as plain text. Avoid HTML and math markup in
choice-based cloze gaps; `exams2ilias` removes unsupported HTML tags from these
labels and emits a warning.

For cloze questions, `exams2ilias` enables ILIAS Identical Scoring by default.
This lets repeated correct answers in different gaps, such as repeated dropdown
choices, receive points independently.

For visible tables in question text, use simple Markdown tables or
`knitr::kable(df, format = "html", row.names = FALSE)`. The default
`table_strategy = "html_basic"` rewrites rendered tables to a conservative
ILIAS-compatible subset. Avoid `kableExtra`, CSS classes, inline styles,
captions, `rowspan`, `colspan`, nested tables, and layout HTML. For wide tables
or cases where visual column separation is essential, test the export with
`table_strategy = "pre"`.

Static files should be registered as supplements, for example with
`exams::include_supplement("figure.png")`. Files created during exercise
processing, such as plots and CSV files written by the exercise, are handled as
supplements automatically and embedded via Base64 where possible.
