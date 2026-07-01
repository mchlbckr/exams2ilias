read_zip_xml <- function(zipfile, member) {
  readLines(unz(zipfile, member), warn = FALSE)
}

fixture_path <- function(name) {
  test_path("..", "fixtures", name)
}

example_path <- function(name) {
  system.file("exercises", name, package = "exams2ilias")
}

test_that("non-placeholder cloze export uses ILIAS top-level items", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  expect_warning(
    exams2ilias(
      system.file("exercises/lm.Rmd", package = "exams"),
      n = 1,
      dir = mydir,
      name = "lm_ilias",
      xmlcollapse = FALSE
    ),
    "plain text only"
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
  expect_true(any(grepl("<fieldlabel>fixedTextLength</fieldlabel>", lm_qti, fixed = TRUE)))
  expect_true(any(grepl("<fieldlabel>feedback_mode</fieldlabel>", lm_qti, fixed = TRUE)))
  expect_true(any(grepl("<fieldlabel>combinations</fieldlabel>", lm_qti, fixed = TRUE)))
  expect_false(any(grepl("<itemfeedback", lm_qti, fixed = TRUE)))
  expect_true(any(grepl('minnumber="', lm_qti, fixed = TRUE)))
  expect_false(any(grepl('minnumber="([^"]+)" maxnumber="\\1"', lm_qti, perl = TRUE)))
  expect_true(any(grepl("data:text/csv;base64", lm_qti, fixed = TRUE)))
  expect_true(any(grepl("<![CDATA[x and y are not significantly correlated]]>", lm_qti, fixed = TRUE)))
  expect_false(any(grepl("<![CDATA[<code>", lm_qti, fixed = TRUE)))
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
  expect_true(grepl(
    "<fieldlabel>ILIAS_VERSION</fieldlabel><fieldentry>9\\.20\\.0</fieldentry>|<fieldlabel>ILIAS_VERSION</fieldlabel>\\s*<fieldentry>9\\.20\\.0</fieldentry>",
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

test_that("ILIAS 9.20 template alias resolves to bundled template", {
  expect_identical(ilias_resolve_template("ilias_9_20"), ilias_resolve_template("ilias"))
  expect_identical(ilias_resolve_template("ilias_9_17"), ilias_resolve_template("ilias"))
})

test_that("question pool description can be read from extags", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("description_tags.Rmd"),
    n = 1,
    dir = mydir,
    name = "description_tags",
    xmlcollapse = FALSE
  )

  description_zip <- file.path(mydir, "description_tags_qpl.zip")
  description_qpl <- paste(read_zip_xml(description_zip,
    "description_tags_qpl/description_tags_qpl.xml"), collapse = "\n")
  description_qti <- paste(read_zip_xml(description_zip,
    "description_tags_qpl/description_tags_qti.xml"), collapse = "\n")

  expect_true(grepl(
    "<Description Language=\"en\">Internal ILIAS description from extags.</Description>",
    description_qpl,
    fixed = TRUE
  ))
  expect_true(grepl(
    "<qticomment>Internal ILIAS description from extags.; v1</qticomment>",
    description_qti,
    fixed = TRUE
  ))
})

test_that("item descriptions include rendered variant numbers", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("description_tags.Rmd"),
    n = 2,
    dir = mydir,
    name = "description_variants",
    xmlcollapse = FALSE
  )

  variants_zip <- file.path(mydir, "description_variants_qpl.zip")
  variants_qti <- paste(read_zip_xml(variants_zip,
    "description_variants_qpl/description_variants_qti.xml"), collapse = "\n")

  expect_true(grepl(
    "<qticomment>Internal ILIAS description from extags.; v1</qticomment>",
    variants_qti,
    fixed = TRUE
  ))
  expect_true(grepl(
    "<qticomment>Internal ILIAS description from extags.; v2</qticomment>",
    variants_qti,
    fixed = TRUE
  ))
})

test_that("cloze intro and first prompt are emitted as separate material blocks", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("cloze_intro_split.Rmd"),
    n = 1,
    dir = mydir,
    name = "cloze_intro_split",
    xmlcollapse = FALSE
  )

  cloze_zip <- file.path(mydir, "cloze_intro_split_qpl.zip")
  cloze_qti <- read_zip_xml(cloze_zip, "cloze_intro_split_qpl/cloze_intro_split_qti.xml")
  preamble_line <- grep("Use the following values", cloze_qti)
  first_prompt_line <- grep("1\\. .*Compute x \\+ y", cloze_qti)
  first_gap_line <- grep('<response_num ident="gap_0"', cloze_qti)

  expect_length(preamble_line, 1L)
  expect_length(first_prompt_line, 1L)
  expect_length(first_gap_line, 1L)
  expect_lt(preamble_line, first_prompt_line)
  expect_lt(first_prompt_line, first_gap_line)
})

test_that("choice-based cloze gaps respect exshuffle", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("cloze_shuffle_choice.Rmd"),
    n = 1,
    dir = mydir,
    name = "cloze_shuffle_choice",
    xmlcollapse = FALSE
  )

  shuffle_zip <- file.path(mydir, "cloze_shuffle_choice_qpl.zip")
  shuffle_qti <- paste(read_zip_xml(shuffle_zip,
    "cloze_shuffle_choice_qpl/cloze_shuffle_choice_qti.xml"), collapse = "\n")

  expect_true(grepl('<render_choice shuffle="Yes">', shuffle_qti, fixed = TRUE))
})

test_that("cloze metadata follows native ILIAS identical scoring fields", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("cloze_repeated_select.Rmd"),
    n = 1,
    dir = mydir,
    name = "cloze_repeated_select",
    xmlcollapse = FALSE
  )

  qti <- paste(read_zip_xml(file.path(mydir, "cloze_repeated_select_qpl.zip"),
    "cloze_repeated_select_qpl/cloze_repeated_select_qti.xml"), collapse = "\n")

  expect_true(grepl(
    paste0(
      "<qtimetadatafield><fieldlabel>textgaprating</fieldlabel><fieldentry>ci</fieldentry></qtimetadatafield>\\s*",
      "<qtimetadatafield><fieldlabel>fixedTextLength</fieldlabel><fieldentry/></qtimetadatafield>\\s*",
      "<qtimetadatafield><fieldlabel>identicalScoring</fieldlabel><fieldentry>1</fieldentry></qtimetadatafield>\\s*",
      "<qtimetadatafield><fieldlabel>feedback_mode</fieldlabel><fieldentry>gapQuestion</fieldentry></qtimetadatafield>\\s*",
      "<qtimetadatafield><fieldlabel>combinations</fieldlabel><fieldentry>W10=</fieldentry></qtimetadatafield>"
    ),
    qti,
    perl = TRUE
  ))
})

test_that("choice-based cloze export keeps repeated correct select solutions scorable", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("cloze_repeated_select.Rmd"),
    n = 1,
    dir = mydir,
    name = "cloze_repeated_select",
    xmlcollapse = FALSE
  )

  qti <- paste(read_zip_xml(file.path(mydir, "cloze_repeated_select_qpl.zip"),
    "cloze_repeated_select_qpl/cloze_repeated_select_qti.xml"), collapse = "\n")

  expect_true(grepl("<fieldlabel>identicalScoring</fieldlabel>\\s*<fieldentry>1</fieldentry>", qti, perl = TRUE))
  expect_equal(lengths(regmatches(qti, gregexpr("<response_str ident=\"gap_[01]\"", qti, perl = TRUE))), 2L)
  expect_equal(lengths(regmatches(qti, gregexpr(
    "<varequal respident=\"gap_[01]\"><!\\[CDATA\\[nominal\\]\\]></varequal>\\s*</conditionvar>\\s*<setvar action=\"Add\">1</setvar>",
    qti,
    perl = TRUE
  ))), 2L)
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
  expect_true(any(grepl("1\\. \\[[^]]+\\] is the", vowels_qti, perl = TRUE)))
  expect_true(any(grepl("2\\. \\[[^]]+\\] is the", vowels_qti, perl = TRUE)))
  expect_false(any(grepl("&lt;ol|&lt;li|&lt;/li", vowels_qti, perl = TRUE)))
  expect_false(any(grepl("<itemfeedback", vowels_qti, fixed = TRUE)))
})

test_that("preformatted R output keeps line breaks for ILIAS", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    system.file("exercises/ttest.Rmd", package = "exams"),
    n = 1,
    dir = mydir,
    name = "ttest_ilias",
    xmlcollapse = FALSE
  )

  qti <- read_zip_xml(file.path(mydir, "ttest_ilias_qpl.zip"), "ttest_ilias_qpl/ttest_ilias_qti.xml")

  expect_true(any(grepl("<pre><code style=\"font-family: 'courier';\">", qti, fixed = TRUE)))
  expect_true(any(grepl("Two Sample t-test<br/>", qti, fixed = TRUE)))
})

test_that("default table strategy emits conservative table markup", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("table_cloze.Rmd"),
    n = 1,
    dir = mydir,
    name = "table_cloze",
    xmlcollapse = FALSE
  )

  qti <- paste(read_zip_xml(file.path(mydir, "table_cloze_qpl.zip"),
    "table_cloze_qpl/table_cloze_qti.xml"), collapse = "\n")

  expect_true(grepl("&lt;table&gt;", qti, fixed = TRUE))
  expect_true(grepl("&lt;tr&gt;&lt;th&gt;Group&lt;/th&gt;", qti, fixed = TRUE))
  expect_true(grepl("&lt;td&gt;&amp;nbsp;&lt;/td&gt;", qti, fixed = TRUE))
  expect_false(grepl("class=&quot;dataframe&quot;|class=\"dataframe\"", qti))
  expect_false(grepl("style=|align=|&lt;thead|&lt;tbody", qti))
})

test_that("table strategy keep preserves source table markup", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("table_cloze.Rmd"),
    n = 1,
    dir = mydir,
    name = "table_keep",
    xmlcollapse = FALSE,
    table_strategy = "keep"
  )

  qti <- paste(read_zip_xml(file.path(mydir, "table_keep_qpl.zip"),
    "table_keep_qpl/table_keep_qti.xml"), collapse = "\n")

  expect_true(grepl("class=\"dataframe\"", qti, fixed = TRUE))
  expect_true(grepl("style=\"border-collapse: collapse;\"", qti, fixed = TRUE))
  expect_true(grepl("&lt;thead&gt;", qti, fixed = TRUE))
})

test_that("table strategy pre emits monospace table text", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("table_cloze.Rmd"),
    n = 1,
    dir = mydir,
    name = "table_pre",
    xmlcollapse = FALSE,
    table_strategy = "pre"
  )

  qti <- paste(read_zip_xml(file.path(mydir, "table_pre_qpl.zip"),
    "table_pre_qpl/table_pre_qti.xml"), collapse = "\n")

  expect_true(grepl("&lt;pre&gt;", qti, fixed = TRUE))
  expect_true(grepl("Group | n | Mean", qti, fixed = TRUE))
  expect_false(grepl("&lt;table", qti, fixed = TRUE))
})

test_that("complex tables fall back to preformatted text", {
  html <- paste0(
    '<table><tr><th colspan="2">Header</th></tr>',
    '<tr><td>A</td><td>B</td></tr></table>'
  )

  normalized <- ilias_normalize_tables(html, "html_basic")

  expect_true(grepl("<pre>", normalized, fixed = TRUE))
  expect_true(grepl("Header", normalized, fixed = TRUE))
  expect_false(grepl("<table>", normalized, fixed = TRUE))
})

test_that("choice-based cloze tables are still reduced to plain text", {
  gap <- NULL
  expect_warning(
    gap <- ilias_gap_xml(
      "schoice",
      "gap_0",
      c("<table><tr><td>correct</td></tr></table>", "<strong>wrong</strong>"),
      c(TRUE, FALSE),
      tolerance = NA,
      points = 1,
      maxchars = c(12, NA, 12)
    ),
    "plain text only"
  )

  qti <- paste(c(gap$presentation, gap$resprocessing), collapse = "\n")

  expect_true(grepl("<![CDATA[correct]]", qti, fixed = TRUE))
  expect_true(grepl("<![CDATA[wrong]]", qti, fixed = TRUE))
  expect_false(grepl("<table|&lt;table|<strong|&lt;strong", qti))
})

test_that("boxplots exercise keeps generated images embedded", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    system.file("exercises/boxplots.Rmd", package = "exams"),
    n = 1,
    dir = mydir,
    name = "boxplots_ilias",
    xmlcollapse = FALSE
  )

  qti <- read_zip_xml(file.path(mydir, "boxplots_ilias_qpl.zip"), "boxplots_ilias_qpl/boxplots_ilias_qti.xml")

  expect_true(any(grepl("data:image/png;base64", qti, fixed = TRUE)))
})

test_that("plot cloze with spaced chunk label embeds supplements as data URIs", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("plot_space_cloze.Rmd"),
    n = 1,
    dir = mydir,
    name = "plot_space_cloze",
    xmlcollapse = FALSE
  )

  zipfile <- file.path(mydir, "plot_space_cloze_qpl.zip")
  qti <- paste(read_zip_xml(zipfile,
    "plot_space_cloze_qpl/plot_space_cloze_qti.xml"), collapse = "\n")
  members <- utils::unzip(zipfile, list = TRUE)$Name

  expect_true(grepl("data:image/png;base64", qti, fixed = TRUE))
  expect_false(grepl('src="[^"]+\\.png"', qti, perl = TRUE))
  expect_false(any(grepl("\\.png$", members)))
})

test_that("plot cloze image embedding avoids multi-variant filename collisions", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("plot_space_cloze.Rmd"),
    n = 2,
    dir = mydir,
    name = "plot_space_cloze_multi",
    xmlcollapse = FALSE
  )

  zipfile <- file.path(mydir, "plot_space_cloze_multi_qpl.zip")
  qti <- paste(read_zip_xml(zipfile,
    "plot_space_cloze_multi_qpl/plot_space_cloze_multi_qti.xml"), collapse = "\n")

  data_uri_count <- gregexpr("data:image/png;base64", qti, fixed = TRUE)[[1L]]
  expect_gte(sum(data_uri_count > 0L), 2L)
  expect_false(grepl('src="[^"]+\\.png"', qti, perl = TRUE))
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

test_that("metasolution writes ILIAS term scoring metadata", {
  mydir <- tempfile("exams2ilias-")
  dir.create(mydir)

  exams2ilias(
    fixture_path("string_metasolution.Rmd"),
    n = 1,
    dir = mydir,
    name = "string_metasolution",
    xmlcollapse = FALSE,
    metasolution = TRUE
  )

  qti_lines <- read_zip_xml(
    file.path(mydir, "string_metasolution_qpl.zip"),
    "string_metasolution_qpl/string_metasolution_qti.xml"
  )
  qti <- paste(qti_lines, collapse = "\n")

  expect_true(grepl("<fieldlabel>termscoring</fieldlabel>", qti, fixed = TRUE))
  expect_true(grepl("<fieldlabel>termrelation</fieldlabel><fieldentry>any</fieldentry>", qti, fixed = TRUE))

  term_line <- qti_lines[grep("<fieldlabel>termscoring</fieldlabel>", qti_lines, fixed = TRUE)][1L]
  encoded <- sub(".*<fieldentry>([^<]+)</fieldentry>.*", "\\1", term_line)
  decoded_raw <- base64enc::base64decode(encoded)
  decoded <- rawToChar(decoded_raw[decoded_raw != as.raw(0)])

  expect_true(grepl("ASS_AnswerMultipleResponseImage", decoded, fixed = TRUE))
  expect_true(grepl("answertext", decoded, fixed = TRUE))
  expect_true(grepl("SE", decoded, fixed = TRUE))
})

test_that("metasolution PHP string lengths use UTF-8 byte counts", {
  decoded_raw <- solustr_to_phpstruct("Größe", nitems = 1L, encode = FALSE)
  decoded <- rawToChar(decoded_raw[decoded_raw != as.raw(0)])

  expect_true(grepl('answertext";s:7:"Größe";', decoded, fixed = TRUE))
  expect_false(grepl('answertext";s:5:"Größe";', decoded, fixed = TRUE))
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
