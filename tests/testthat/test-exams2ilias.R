read_zip_xml <- function(zipfile, member) {
  readLines(unz(zipfile, member), warn = FALSE)
}

fixture_path <- function(name) {
  test_path("..", "fixtures", name)
}

example_path <- function(name) {
  system.file("examples", name, package = "exams2ilias")
}

test_that("non-placeholder cloze export uses ILIAS top-level items", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    system.file("exercises/lm.Rmd", package = "exams"),
    n = 1,
    dir = mydir,
    name = "lm_ilias",
    xmlcollapse = FALSE
  )

  lm_zip <- file.path(mydir, "lm_ilias_qpl.zip")
  expect_true(file.exists(lm_zip))

  lm_qti <- read_zip_xml(lm_zip, "lm_ilias_qpl/lm_ilias_qti.xml")
  lm_qpl <- read_zip_xml(lm_zip, "lm_ilias_qpl/lm_ilias_qpl.xml")

  expect_false(any(grepl("<assessment", lm_qti, fixed = TRUE)))
  expect_false(any(grepl("<section", lm_qti, fixed = TRUE)))
  expect_true(any(grepl("<fieldentry>CLOZE QUESTION</fieldentry>", lm_qti, fixed = TRUE)))
  expect_true(any(grepl('maxattempts="0"', lm_qti, fixed = TRUE)))
  expect_true(any(grepl('<response_str ident="gap_0"', lm_qti, fixed = TRUE)))
  expect_true(any(grepl('<response_num ident="gap_1"', lm_qti, fixed = TRUE)))
  expect_true(any(grepl("<fieldlabel>textgaprating</fieldlabel>", lm_qti, fixed = TRUE)))
  expect_false(any(grepl("<fieldlabel>AUTHOR</fieldlabel>", lm_qti, fixed = TRUE)))
  expect_false(any(grepl("<fieldlabel>fixedTextLength</fieldlabel>", lm_qti, fixed = TRUE)))
  expect_false(any(grepl("<itemfeedback", lm_qti, fixed = TRUE)))
  expect_true(any(grepl('minnumber="', lm_qti, fixed = TRUE)))
  expect_false(any(grepl('minnumber="([^"]+)" maxnumber="\\1"', lm_qti, perl = TRUE)))
  expect_true(any(grepl('<Question QRef="il_0_qst_[0-9]+"', lm_qpl, perl = TRUE)))
  expect_true(any(grepl('<TriggerQuestion Id="[0-9]+"></TriggerQuestion>', lm_qpl, perl = TRUE)))
})

test_that("metadata fields are emitted as siblings in single-choice items", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    system.file("exercises/swisscapital.Rmd", package = "exams"),
    n = 1,
    dir = mydir,
    name = "swiss_ilias",
    xmlcollapse = FALSE
  )

  swiss_zip <- file.path(mydir, "swiss_ilias_qpl.zip")
  swiss_qti <- paste(read_zip_xml(swiss_zip, "swiss_ilias_qpl/swiss_ilias_qti.xml"), collapse = "\n")

  expect_true(grepl(
    "<fieldlabel>QUESTIONTYPE</fieldlabel>\\s*<fieldentry>SINGLE CHOICE QUESTION</fieldentry>\\s*</qtimetadatafield>\\s*<qtimetadatafield>\\s*<fieldlabel>AUTHOR</fieldlabel>",
    swiss_qti,
    perl = TRUE
  ))
  expect_false(grepl(
    "<fieldlabel>identicalScoring</fieldlabel>\\s*<qtimetadatafield>",
    swiss_qti,
    perl = TRUE
  ))
  expect_true(grepl(
    "<fieldlabel>fixedTextLength</fieldlabel>\\s*<fieldentry/>\\s*</qtimetadatafield>\\s*<qtimetadatafield>\\s*<fieldlabel>identicalScoring</fieldlabel>",
    swiss_qti,
    perl = TRUE
  ))
})

test_that("placeholder cloze export keeps the simplified ILIAS structure", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    system.file("exercises/vowels.Rmd", package = "exams"),
    n = 1,
    dir = mydir,
    name = "vowels_ilias",
    xmlcollapse = FALSE
  )

  vowels_zip <- file.path(mydir, "vowels_ilias_qpl.zip")
  expect_true(file.exists(vowels_zip))

  vowels_qti <- read_zip_xml(vowels_zip, "vowels_ilias_qpl/vowels_ilias_qti.xml")

  expect_false(any(grepl("<assessment", vowels_qti, fixed = TRUE)))
  expect_true(any(grepl('maxattempts="0"', vowels_qti, fixed = TRUE)))
  expect_true(any(grepl('<response_str ident="gap_5"', vowels_qti, fixed = TRUE)))
  expect_true(any(grepl('<mattext texttype="text/xhtml"> </mattext>', vowels_qti, fixed = TRUE)))
  expect_false(any(grepl("<itemfeedback", vowels_qti, fixed = TRUE)))
})

test_that("vault mixed-type cloze fixture exports without the upstream crash", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  expect_no_error(
    exams2ilias(
      fixture_path("cloze_08_mixed_types.Rmd"),
      n = 1,
      dir = mydir,
      name = "cloze_08_mixed",
      xmlcollapse = FALSE,
      eval = list(partial = TRUE, negative = TRUE)
    )
  )

  expect_true(file.exists(file.path(mydir, "cloze_08_mixed_qpl.zip")))
})

test_that("character-heavy cloze choices are wrapped in CDATA", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("cloze_02_chars.Rmd"),
    n = 1,
    dir = mydir,
    name = "cloze_chars",
    xmlcollapse = FALSE
  )

  qti <- read_zip_xml(file.path(mydir, "cloze_chars_qpl.zip"), "cloze_chars_qpl/cloze_chars_qti.xml")

  expect_true(any(grepl('<mattext texttype="text/html"><!\\[CDATA\\[', qti, perl = TRUE)))
  expect_true(any(grepl('<varequal respident="gap_0"><!\\[CDATA\\[', qti, perl = TRUE)))
})

test_that("single-choice export uses matching response identifiers and additive scoring", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("schoice_02_chars.Rmd"),
    n = 1,
    dir = mydir,
    name = "schoice_chars",
    xmlcollapse = FALSE
  )

  qti <- read_zip_xml(file.path(mydir, "schoice_chars_qpl.zip"), "schoice_chars_qpl/schoice_chars_qti.xml")
  response_line <- qti[grep('<response_lid ident="', qti, fixed = TRUE)][1]
  response_id <- sub('^.*<response_lid ident="([^"]+)".*$', "\\1", response_line)

  expect_match(response_id, "_RESPONSE_")
  expect_true(any(grepl(paste0('<varequal respident="', response_id, '" case="Yes">'), qti, fixed = TRUE)))
  expect_true(any(grepl('<setvar varname="SCORE" action="Add">1</setvar>', qti, fixed = TRUE)))
  expect_false(any(grepl('<setvar varname="SCORE" action="Set">1</setvar>', qti, fixed = TRUE)))
})

test_that("multiple-choice export keeps partial-credit scoring without all-or-nothing fallback", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("mchoice_01_math.Rmd"),
    n = 1,
    dir = mydir,
    name = "mchoice_math",
    xmlcollapse = FALSE,
    eval = list(partial = TRUE, negative = TRUE)
  )

  qti <- read_zip_xml(file.path(mydir, "mchoice_math_qpl.zip"), "mchoice_math_qpl/mchoice_math_qti.xml")

  expect_true(any(grepl('<response_lid ident="[^"]+" rcardinality="Multiple"', qti, perl = TRUE)))
  expect_gte(sum(grepl('<setvar varname="SCORE" action="Add">1</setvar>', qti, fixed = TRUE)), 3L)
  expect_true(any(grepl('<setvar varname="SCORE" action="Add">-1\\.5</setvar>', qti, perl = TRUE)))
  expect_false(any(grepl('<setvar varname="SCORE" action="Set">3</setvar>', qti, fixed = TRUE)))
})

test_that("bundled example exercises export individually", {
  example_files <- c(
    "stats_cloze.Rmd",
    "stats_schoice.Rmd",
    "stats_mchoice.Rmd",
    "stats_num.Rmd",
    "stats_string.Rmd"
  )

  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  for(file in example_files) {
    example_file <- example_path(file)
    expect_true(file.exists(example_file), info = file)

    export_name <- tools::file_path_sans_ext(basename(file))
    expect_no_error({
      exams2ilias(
        example_file,
        n = 1,
        dir = mydir,
        name = export_name,
        xmlcollapse = FALSE,
        solutionswitch = FALSE
      )
    })

    zipfile <- file.path(mydir, paste0(export_name, "_qpl.zip"))
    expect_true(file.exists(zipfile), info = file)
  }
})

test_that("bundled generator script exports example set and combined pool", {
  script <- example_path("generate_examples.R")
  expect_true(file.exists(script))

  env <- new.env(parent = globalenv())
  source(script, local = env)
  expect_true(is.function(env$generate_example_exports))

  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  result <- env$generate_example_exports(output_dir = mydir, n = 1, combined = TRUE)

  expect_length(result$individual, 5L)
  expect_true(all(file.exists(result$individual)))
  expect_true(file.exists(result$combined))
})
