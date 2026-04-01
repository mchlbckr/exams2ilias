.fileURI_mime_types <- matrix(c(
  "bmp", "image/bmp",
  "png", "image/png",
  "jpg", "image/jpeg",
  "jpeg", "image/jpeg",
  "gif", "image/gif",
  "heic", "image/heic",
  "svg", "image/svg+xml",
  "wmf", "image/wmf",
  "doc", "application/msword",
  "docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  "css", "text/css",
  "htm", "text/html",
  "html", "text/html",
  "pdf", "application/pdf",
  "rtf", "application/rtf",
  "tex", "application/x-tex",
  "txt", "text/plain",
  "xml", "application/xml",
  "c", "text/plain",
  "js", "text/javascript",
  "py", "text/plain",
  "r", "text/plain",
  "rmd", "text/markdown",
  "md", "text/markdown",
  "csv", "text/csv",
  "dta", "application/octet-stream",
  "ods", "application/vnd.oasis.opendocument.spreadsheet",
  "raw", "text/plain",
  "rda", "application/octet-stream",
  "rds", "application/octet-stream",
  "sav", "application/octet-stream",
  "tsv", "text/tab-separated-values",
  "xls", "application/vnd.ms-excel",
  "xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  "zip", "application/zip",
  "gh", "application/octet-stream",
  "3dm", "model/vnd.3dm"
), ncol = 2L, byrow = TRUE, dimnames = list(NULL, c("ext", "mime")))

.empty_text <- function(x) {
  is.null(x) || anyNA(x) || all(grepl("^[[:space:]]*$", x))
}

ilias_resolve_template <- function(template) {
  if(identical(template, "ilias") || identical(template, "ilias_9_17")) {
    path <- system.file("xml", "ilias_9_17.xml", package = "exams2ilias")
    if(!nzchar(path)) stop("internal ILIAS 9.17 template not found")
    return(path)
  }
  template
}

extract_qti12_items <- function(xml) {
  starts <- grep("^\\s*<item ident=", xml)
  if(length(starts) < 1L) stop("no <item> tags found in generated QTI 1.2 XML")

  rval <- vector("list", length(starts))
  names(rval) <- character(length(starts))
  end_cursor <- 1L

  for(i in seq_along(starts)) {
    ends <- grep("^\\s*</item>\\s*$", xml)
    ends <- ends[ends >= starts[i] & ends >= end_cursor]
    if(length(ends) < 1L) stop("unterminated <item> block in generated QTI 1.2 XML")
    end_i <- ends[1L]
    rval[[i]] <- xml[starts[i]:end_i]
    names(rval)[i] <- sub('^\\s*<item ident="([^"]+)".*$', "\\1", xml[starts[i]])
    end_cursor <- end_i + 1L
  }

  rval
}

ilias_flatten_exams <- function(exm) {
  rval <- list()
  k <- 1L
  for(i in seq_along(exm)) {
    for(j in seq_along(exm[[i]])) {
      rval[[k]] <- list(i = i, j = j, item = exm[[i]][[j]])
      k <- k + 1L
    }
  }
  rval
}

ilias_extract_item_title <- function(item_xml) {
  sub('^\\s*<item ident="[^"]+" title="([^"]*)".*$', "\\1", item_xml[1L])
}

ilias_question_type <- function(type) {
  switch(type,
    "schoice" = "SINGLE CHOICE QUESTION",
    "mchoice" = "MULTIPLE CHOICE QUESTION",
    "num" = "NUMERIC QUESTION",
    "cloze" = "CLOZE QUESTION",
    "string" = "TEXT QUESTION",
    stop("unsupported ILIAS question type: ", type)
  )
}

ilias_escape_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

ilias_escape_attribute <- function(x) {
  x <- ilias_escape_text(x)
  gsub('"', "&quot;", x, fixed = TRUE)
}

ilias_format_value <- function(x) {
  trimws(format(x, scientific = FALSE, trim = TRUE))
}

ilias_item_header <- function(id, title, maxattempts = NULL) {
  attr <- if(is.null(maxattempts)) "" else paste0(" ", maxattempts)
  paste0('<item ident="', ilias_escape_attribute(id),
    '" title="', ilias_escape_attribute(title), '"', attr, '>')
}

ilias_item_metadata <- function(questiontype, ilias_version = "9.17.0",
  author = "R/exams", textgaprating = "ci", include_author = TRUE,
  include_fixed_text_length = TRUE)
{
  xml <- c(
    '<qticomment></qticomment>',
    '<itemmetadata>',
    '<qtimetadata>',
    '<qtimetadatafield>',
    '<fieldlabel>ILIAS_VERSION</fieldlabel>',
    paste0('<fieldentry>', ilias_escape_text(ilias_version), '</fieldentry>'),
    '</qtimetadatafield>',
    '<qtimetadatafield>',
    '<fieldlabel>QUESTIONTYPE</fieldlabel>',
    paste0('<fieldentry>', ilias_escape_text(questiontype), '</fieldentry>'),
    '</qtimetadatafield>',
    '<qtimetadatafield>',
    '<fieldlabel>textgaprating</fieldlabel>',
    paste0('<fieldentry>', textgaprating, '</fieldentry>'),
    '</qtimetadatafield>',
    '<qtimetadatafield>',
    '<fieldlabel>identicalScoring</fieldlabel>',
    '<fieldentry>1</fieldentry>',
    '</qtimetadatafield>',
    '</qtimetadata>',
    '</itemmetadata>'
  )
  if(include_author) {
    xml <- append(xml, c(
      '<qtimetadatafield>',
      '<fieldlabel>AUTHOR</fieldlabel>',
      paste0('<fieldentry>', ilias_escape_text(author), '</fieldentry>'),
      '</qtimetadatafield>'
    ), after = 9L)
  }
  if(include_fixed_text_length) {
    pos <- grep("<fieldlabel>identicalScoring</fieldlabel>", xml, fixed = TRUE)[1L] - 1L
    xml <- append(xml, c(
      '<qtimetadatafield>',
      '<fieldlabel>fixedTextLength</fieldlabel>',
      '<fieldentry/>',
      '</qtimetadatafield>'
    ), after = pos)
  }
  xml
}

ilias_bare_qid <- function(qref) {
  sub("^.*_qst_([0-9]+)$", "\\1", qref)
}

patch_item_ilias <- function(item_xml, item_id, title, questiontype, maxattempts = 0) {
  meta_start <- grep("^\\s*<itemmetadata>\\s*$", item_xml)
  meta_end <- grep("^\\s*</itemmetadata>\\s*$", item_xml)
  if(length(meta_start) != 1L || length(meta_end) != 1L || meta_end < meta_start) {
    stop("cannot locate <itemmetadata> block in generated ILIAS item")
  }

  rval <- item_xml[-(meta_start:meta_end)]
  rval[1L] <- ilias_item_header(item_id, title,
    paste0('maxattempts="', if(is.infinite(maxattempts) || maxattempts == 0) 0 else maxattempts, '"'))
  rval <- append(rval, ilias_item_metadata(questiontype), after = 1L)

  p <- grep("^\\s*<presentation>\\s*$", rval)
  if(length(p) > 0L) {
    rval[p[1L]] <- paste0('<presentation label="', ilias_escape_attribute(title), '">')
  }

  rval
}

ilias_questionlist <- function(x) {
  questionlist <- if(!is.list(x$questionlist)) {
    if(x$metainfo$type == "cloze") {
      g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
      if(!is.null(x$questionlist)) split(x$questionlist, g) else NULL
    } else {
      list(x$questionlist)
    }
  } else {
    x$questionlist
  }

  if(length(questionlist) < 1L) {
    questionlist <- NULL
  } else {
    for(i in seq_along(questionlist)) {
      if(length(questionlist[[i]]) < 1L) questionlist[[i]] <- NA_character_
    }
  }

  questionlist
}

ilias_maxchars <- function(x, n, maxchars = 12) {
  rval <- if(is.null(x$metainfo$maxchars)) {
    if(length(maxchars) < 2L) c(maxchars, NA, NA) else maxchars[1:3]
  } else x$metainfo$maxchars

  if(!is.list(rval)) rval <- list(rval)
  rval <- rep(rval, length.out = n)
  for(i in seq_along(rval)) {
    if(length(rval[[i]]) < 2L) rval[[i]] <- c(rval[[i]], NA, NA)
  }
  rval
}

ilias_choice_solution <- function(solution) {
  if(is.logical(solution)) return(solution)
  if(is.character(solution) && length(solution) == 1L && grepl("^[01]+$", solution)) {
    return(as.logical(as.integer(strsplit(solution, "")[[1L]])))
  }
  as.logical(solution)
}

ilias_material <- function(text, keep_whitespace = FALSE) {
  if(is.null(text) || anyNA(text)) return(NULL)
  text <- paste(text, collapse = "\n")
  if(!keep_whitespace && .empty_text(text)) return(NULL)
  if(keep_whitespace && identical(text, "")) return(NULL)
  c(
    '<material>',
    paste0('<mattext texttype="text/xhtml">', ilias_escape_text(text), '</mattext>'),
    '</material>'
  )
}

ilias_gap_xml <- function(type, gap_id, choices, solution, tolerance, points, maxchars) {
  if(type %in% c("essay", "file", "verbatim")) {
    warning("ILIAS cloze export treats cloze type '", type, "' as a string gap")
    type <- "string"
  }

  if(type == "string") {
    cols <- if(!is.na(maxchars[3L])) maxchars[3L] else 0L
    presentation <- c(
      paste0('<response_str ident="', gap_id, '" rcardinality="Single">'),
      paste0('<render_fib fibtype="String" prompt="Box" columns="', cols, '">'),
      '</render_fib>',
      '</response_str>'
    )
    resprocessing <- unlist(lapply(as.character(solution), function(sol) {
      c(
        '<respcondition continue="Yes">',
        '<conditionvar>',
        paste0('<varequal respident="', gap_id, '">', ilias_escape_text(sol), '</varequal>'),
        '</conditionvar>',
        paste0('<setvar action="Add">', ilias_format_value(points), '</setvar>'),
        '</respcondition>'
      )
    }), use.names = FALSE)
    return(list(presentation = presentation, resprocessing = resprocessing))
  }

  if(type == "num") {
    values <- as.numeric(solution)
    tol <- suppressWarnings(as.numeric(tolerance))
    tol <- tol[is.finite(tol)]
    tol <- if(length(tol)) max(tol) else 0
    if(!is.finite(tol) || tol <= 0) {
      value_txt <- ilias_format_value(values[1L])
      if(grepl(".", value_txt, fixed = TRUE)) {
        decimals <- nchar(sub(".*\\.", "", value_txt))
        tol <- 0.5 * 10^(-decimals)
      } else {
        tol <- 0.1
      }
    }
    cols <- if(!is.na(maxchars[3L])) maxchars[3L] else max(2L, nchar(ilias_format_value(values[1L])))
    presentation <- c(
      paste0('<response_num ident="', gap_id, '" numtype="Decimal" rcardinality="Single">'),
      paste0('<render_fib fibtype="Decimal" prompt="Box" columns="', cols,
        '" maxchars="0" minnumber="', ilias_format_value(min(values) - tol),
        '" maxnumber="', ilias_format_value(max(values) + tol), '">'),
      '</render_fib>',
      '</response_num>'
    )
    resprocessing <- unlist(lapply(values, function(sol) {
      c(
        '<respcondition continue="Yes">',
        '<conditionvar>',
        paste0('<varequal respident="', gap_id, '">', ilias_format_value(sol), '</varequal>'),
        '</conditionvar>',
        paste0('<setvar action="Add">', ilias_format_value(points), '</setvar>'),
        '</respcondition>'
      )
    }), use.names = FALSE)
    return(list(presentation = presentation, resprocessing = resprocessing))
  }

  if(!length(choices)) choices <- as.character(seq_along(solution))
  correct <- ilias_choice_solution(solution)
  if(length(correct) != length(choices)) {
    stop("choice-based cloze gap has mismatched choices and solution length")
  }

  choice_points <- if(type == "mchoice" && any(correct)) points / sum(correct) else points
  presentation <- c(
    paste0('<response_str ident="', gap_id, '" rcardinality="',
      if(type == "mchoice") "Multiple" else "Single", '">'),
    '<render_choice shuffle="No">'
  )
  for(j in seq_along(choices)) {
    presentation <- c(presentation,
      paste0('<response_label ident="', j - 1L, '">'),
      '<material>',
      paste0('<mattext>', ilias_escape_text(choices[j]), '</mattext>'),
      '</material>',
      '</response_label>'
    )
  }
  presentation <- c(presentation, '</render_choice>', '</response_str>')

  resprocessing <- unlist(lapply(seq_along(choices), function(j) {
    pts <- if(correct[j]) choice_points else 0
    c(
      '<respcondition continue="Yes">',
      '<conditionvar>',
      paste0('<varequal respident="', gap_id, '">', ilias_escape_text(choices[j]), '</varequal>'),
      '</conditionvar>',
      paste0('<setvar action="Add">', ilias_format_value(pts), '</setvar>'),
      '</respcondition>'
    )
  }), use.names = FALSE)

  list(presentation = presentation, resprocessing = resprocessing)
}

make_item_ilias_cloze <- function(item_xml, x, item_id, title, maxattempts = 0) {
  solution <- if(!is.list(x$metainfo$solution)) list(x$metainfo$solution) else x$metainfo$solution
  n <- length(solution)
  type <- x$metainfo$clozetype
  tol <- if(!is.list(x$metainfo$tolerance)) as.list(x$metainfo$tolerance) else x$metainfo$tolerance
  tol <- rep(tol, length.out = n)
  questionlist <- ilias_questionlist(x)
  if(is.null(questionlist)) questionlist <- vector("list", n)
  maxchars <- ilias_maxchars(x, n)

  points <- if(is.null(x$metainfo$points)) rep(1, n) else x$metainfo$points
  q_points <- rep(points, length.out = n)

  presentation <- c(
    paste0('<presentation label="', ilias_escape_attribute(title), '">'),
    '<flow>'
  )
  resprocessing <- c(
    '<resprocessing>',
    '<outcomes>',
    '<decvar></decvar>',
    '</outcomes>'
  )

  question <- if(!is.null(x$question)) paste(x$question, collapse = "\n") else NULL
  has_answertags <- !is.null(question) && grepl("##ANSWER[0-9]+##", question)

  if(has_answertags) {
    parts <- strsplit(question, "##ANSWER[0-9]+##", perl = TRUE)[[1L]]
    markers <- gregexpr("##ANSWER[0-9]+##", question, perl = TRUE)
    markers <- regmatches(question, markers)[[1L]]
    gaps <- as.integer(sub("##ANSWER([0-9]+)##", "\\1", markers))

    if(length(parts) != length(gaps) + 1L) {
      stop("cannot split cloze question into text and answer gaps")
    }

    for(k in seq_along(gaps)) {
      i <- gaps[k]
      if(i < 1L || i > n) stop("invalid ##ANSWER tag in cloze question")
      presentation <- c(presentation, ilias_material(parts[k], keep_whitespace = TRUE))
      gap_xml <- ilias_gap_xml(type[i], paste0("gap_", i - 1L), questionlist[[i]],
        solution[[i]], tol[[i]], q_points[i], maxchars[[i]])
      presentation <- c(presentation, gap_xml$presentation)
      resprocessing <- c(resprocessing, gap_xml$resprocessing)
    }
    presentation <- c(presentation, ilias_material(parts[length(parts)], keep_whitespace = TRUE))
  } else {
    presentation <- c(presentation, ilias_material(question))
    for(i in seq_len(n)) {
      if(type[i] %in% c("string", "num", "essay", "file", "verbatim")) {
        presentation <- c(presentation, ilias_material(questionlist[[i]]))
      }
      gap_xml <- ilias_gap_xml(type[i], paste0("gap_", i - 1L), questionlist[[i]],
        solution[[i]], tol[[i]], q_points[i], maxchars[[i]])
      presentation <- c(presentation, gap_xml$presentation)
      resprocessing <- c(resprocessing, gap_xml$resprocessing)
    }
  }

  presentation <- c(presentation, '</flow>', '</presentation>')
  resprocessing <- c(resprocessing, '</resprocessing>')

  c(
    ilias_item_header(item_id, title,
      paste0('maxattempts="', if(is.infinite(maxattempts) || maxattempts == 0) 0 else maxattempts, '"')),
    ilias_item_metadata("CLOZE QUESTION",
      include_author = FALSE, include_fixed_text_length = FALSE),
    presentation,
    resprocessing,
    '</item>'
  )
}

ilias_collapse_xml <- function(xml, xmlcollapse) {
  if(identical(xmlcollapse, FALSE)) return(xml)
  collapse <- if(identical(xmlcollapse, TRUE)) " " else as.character(xmlcollapse)
  paste(xml, collapse = collapse)
}

make_qpl_xml <- function(name, qrefs, pool_id = paste0(name, "_qpl")) {
  xml <- c(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<!DOCTYPE Test SYSTEM "http://www.ilias.uni-koeln.de/download/dtd/ilias_co.dtd">',
    '<!--Export of ILIAS Test Questionpool 3029 of installation .-->',
    '<ContentObject Type="Questionpool_Test">',
    '<MetaData>',
    '<General Structure="Hierarchical">',
    paste0('<Identifier Catalog="ILIAS" Entry="', pool_id, '"/>'),
    paste0('<Title Language="en">', name, '</Title>'),
    '<Language Language="en"/>',
    '<Description Language="en"/>',
    '<Keyword Language="en"/>',
    '</General>',
    '</MetaData>',
    '<Settings>',
    '<ShowTaxonomies>0</ShowTaxonomies>',
    '<NavTaxonomy>0</NavTaxonomy>',
    '<SkillService>0</SkillService>',
    '</Settings>'
  )

  for(qref in qrefs) {
    xml <- c(xml,
      '<PageObject>',
      '<PageContent>',
      paste0('<Question QRef="', qref, '"/>'),
      '</PageContent>',
      '</PageObject>'
    )
  }

  xml <- c(xml, '<QuestionSkillAssignments>')
  for(qref in qrefs) {
    xml <- c(xml,
      paste0('<TriggerQuestion Id="', ilias_bare_qid(qref), '"></TriggerQuestion>')
    )
  }

  c(xml, '</QuestionSkillAssignments>', '</ContentObject>')
}

solution_to_qtimetadata <- function(name, exm, path = ".") {
  xml <- readLines(file.path(path, name, paste0(name, "_qti.xml")), warn = FALSE)

  for(i in seq_along(exm)) {
    idx <- (grep("QUESTIONTYPE", xml) + 2L)[i]
    solustr <- paste(exm[[i]][[1L]]$solution, collapse = "\n")

    if(exm[[i]][[1L]]$solution[1L] %in% c("<ol>", "<ul>")) {
      solustr <- gsub("</li>.*", "", strsplit(solustr, "<li>", fixed = TRUE)[[1L]])[-1L]
    }

    solustr <- solustr_to_phpstruct(solustr, length(solustr))

    xml <- append(xml,
      c("<qtimetadatafield>",
        paste0("<fieldlabel>termscoring</fieldlabel><fieldentry>",
          solustr, "</fieldentry>"),
        "</qtimetadatafield>",
        "<qtimetadatafield>",
        "<fieldlabel>termrelation</fieldlabel><fieldentry>any</fieldentry>",
        "</qtimetadatafield>"),
      after = idx)
  }

  writeLines(xml, file.path(path, name, paste0(name, "_qti.xml")))
}

solustr_to_phpstruct <- function(solustr, nitems, encode = TRUE) {
  items <- raw(0)
  for(i in seq_len(nitems)) {
    items <- c(items,
      c(
        charToRaw(paste0(
          'i:', i - 1L,
          ';O:31:"ASS_AnswerMultipleResponseImage":6:{',
          's:5:"image";s:0:"";s:16:"points_unchecked";s:1:"0";'
        )),
        charToRaw('s:13:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
        charToRaw(paste0('answertext";s:', nchar(solustr[i]), ':"',
          solustr[i], '";')),
        charToRaw('s:9:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
        charToRaw('points";s:1:"1";'),
        charToRaw('s:8:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
        charToRaw('order";i:0;'),
        charToRaw('s:5:"'), as.raw(0x00), charToRaw('*'), as.raw(0x00),
        charToRaw('id";i:-1;}')
      )
    )
  }
  rval <- c(charToRaw(paste0('a:', nitems, ':{')), items, charToRaw('}'))
  if(encode) {
    rval <- base64enc::base64encode(rval)
  }
  rval
}
