generate_example_exports <- function(output_dir = file.path(getwd(), "ilias_examples"),
                                     n = 1,
                                     combined = TRUE) {
  if(!requireNamespace("exams2ilias", quietly = TRUE)) {
    stop("The exams2ilias package must be installed to run this script.")
  }

  example_dir <- system.file("examples", package = "exams2ilias")
  if(!nzchar(example_dir)) {
    stop("Could not locate the bundled example directory.")
  }

  example_files <- file.path(example_dir, c(
    "stats_cloze.Rmd",
    "stats_schoice.Rmd",
    "stats_mchoice.Rmd",
    "stats_num.Rmd",
    "stats_string.Rmd"
  ))

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  individual <- vapply(example_files, function(file) {
    name <- tools::file_path_sans_ext(basename(file))
    exams2ilias::exams2ilias(
      file,
      n = n,
      dir = output_dir,
      name = name,
      xmlcollapse = FALSE,
      solutionswitch = FALSE
    )
    file.path(output_dir, paste0(name, "_qpl.zip"))
  }, character(1))

  combined_zip <- NULL
  if(isTRUE(combined)) {
    exams2ilias::exams2ilias(
      example_files,
      n = n,
      dir = output_dir,
      name = "stats_examples",
      xmlcollapse = FALSE,
      solutionswitch = FALSE
    )
    combined_zip <- file.path(output_dir, "stats_examples_qpl.zip")
  }

  invisible(list(
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    example_files = example_files,
    individual = individual,
    combined = combined_zip
  ))
}
