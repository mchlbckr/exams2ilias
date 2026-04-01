read_zip_xml <- function(zipfile, member) {
  readLines(unz(zipfile, member), warn = FALSE)
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

test_that("multiple-choice export uses top-level items", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    system.file("exercises/ttest.Rmd", package = "exams"),
    n = 1,
    dir = mydir,
    name = "ttest_ilias",
    xmlcollapse = FALSE
  )

  ttest_zip <- file.path(mydir, "ttest_ilias_qpl.zip")
  expect_true(file.exists(ttest_zip))

  ttest_qti <- read_zip_xml(ttest_zip, "ttest_ilias_qpl/ttest_ilias_qti.xml")

  expect_false(any(grepl("<assessment", ttest_qti, fixed = TRUE)))
  expect_false(any(grepl("<section", ttest_qti, fixed = TRUE)))
  expect_true(any(grepl('maxattempts="0"', ttest_qti, fixed = TRUE)))
  expect_true(any(grepl("<fieldentry>MULTIPLE CHOICE QUESTION</fieldentry>", ttest_qti, fixed = TRUE)))
  expect_true(any(grepl('<response_lid ident="[^"]+" rcardinality="Multiple"', ttest_qti, perl = TRUE)))
  expect_true(any(grepl("<itemcontrol ", ttest_qti, fixed = TRUE)))
  expect_true(any(grepl('<setvar varname="SCORE" action="Add">', ttest_qti, fixed = TRUE)))
})

test_that("single-choice export keeps the working 04422d2 ILIAS layout", {
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
  expect_true(file.exists(swiss_zip))

  swiss_qti <- read_zip_xml(swiss_zip, "swiss_ilias_qpl/swiss_ilias_qti.xml")

  expect_false(any(grepl("<assessment", swiss_qti, fixed = TRUE)))
  expect_false(any(grepl("<section", swiss_qti, fixed = TRUE)))
  expect_true(any(grepl("<fieldentry>SINGLE CHOICE QUESTION</fieldentry>", swiss_qti, fixed = TRUE)))
  expect_true(any(grepl('<response_lid ident="[^"]+" rcardinality="Single"', swiss_qti, perl = TRUE)))
  expect_true(any(grepl("<itemcontrol ", swiss_qti, fixed = TRUE)))
  expect_true(any(grepl('<setvar varname="SCORE" action="Set">1</setvar>', swiss_qti, fixed = TRUE)))
})
