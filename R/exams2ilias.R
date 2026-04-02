exams2ilias <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "UTF-8",
  num = list(fix_num = FALSE, minvalue = NA),
  mchoice = list(maxchars = c(3, NA, 3), minvalue = NA),
  schoice = mchoice, string = NULL, cloze = NULL,
  template = "ilias",
  duration = NULL, stitle = "Exercise", ititle = "Question",
  adescription = "Please solve the following exercises.",
  sdescription = "Please answer the following question.",
  maxattempts = 0, cutvalue = 0, solutionswitch = TRUE, zip = TRUE,
  points = NULL, eval = list(partial = TRUE, negative = FALSE),
  converter = "pandoc-mathjax", xmlcollapse = TRUE,
  metasolution = FALSE, ...)
{
  if(is.null(num)) {
    num <- list(fix_num = FALSE, minvalue = NA)
  } else {
    num$fix_num <- FALSE
    num$minvalue <- NA
  }
  if(is.null(mchoice)) {
    mchoice <- list(maxchars = c(3, NA, 3), minvalue = NA)
  } else {
    mchoice$maxchars <- c(3, NA, 3)
    mchoice$minvalue <- NA
  }
  if(is.null(schoice)) {
    schoice <- list(maxchars = c(3, NA, 3), minvalue = NA)
  } else {
    schoice$maxchars <- c(3, NA, 3)
    schoice$minvalue <- NA
  }

  if(is.null(name)) name <- gsub("\\.xml$", "", basename(template))
  template <- ilias_resolve_template(template)

  base64 <- .fileURI_mime_types[, "ext"]

  outdir <- tools::file_path_as_absolute(dir)
  dir.create(workdir <- tempfile())
  on.exit(unlink(workdir, recursive = TRUE), add = TRUE)

  maxattempts_qti12 <- maxattempts
  maxattempts_qti12[is.infinite(maxattempts_qti12) | maxattempts_qti12 == 0] <- 1

  rval <- ilias_exams2qti12(file = file, n = n, nsamp = nsamp, dir = workdir,
    name = name, quiet = quiet, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose,
    resolution = resolution, width = width, height = height, svg = svg, encoding = encoding,
    num = num, mchoice = mchoice, schoice = schoice, string = string, cloze = cloze,
    template = template,
    duration = duration, stitle = stitle, ititle = ititle,
    adescription = adescription, sdescription = sdescription,
    maxattempts = maxattempts_qti12, cutvalue = cutvalue, solutionswitch = solutionswitch, zip = FALSE,
    points = points, eval = eval, converter = converter, xmlcollapse = FALSE,
    base64 = base64, flavor = "ilias", ...)

  qti12_path <- file.path(workdir, paste0(name, ".xml"))
  xml <- readLines(qti12_path, warn = FALSE)
  items <- extract_qti12_items(xml)
  exm <- ilias_flatten_exams(rval)
  lookup <- setNames(seq_along(exm), vapply(exm, function(z) z$item$metainfo$id, character(1)))

  if(length(items) != length(exm)) {
    stop("generated item count does not match exams item count")
  }
  if(!all(names(items) %in% names(lookup))) {
    stop("failed to match generated QTI items back to exercises")
  }

  prefix <- paste0(format(as.integer(Sys.time()), scientific = FALSE, trim = TRUE),
    sprintf("%04d", sample.int(10000L, 1L) - 1L))
  width_id <- max(1L, nchar(length(items)))
  item_xml <- vector("list", length(items))
  qrefs <- character(length(items))
  pool_id <- paste0("il_0_qpl_", prefix)
  final_maxattempts <- rep(maxattempts, length.out = length(items))

  for(k in seq_along(items)) {
    pos <- lookup[[names(items)[k]]]
    i <- exm[[pos]]$i
    j <- exm[[pos]]$j
    x <- exm[[pos]]$item
    title <- x$metainfo$name
    if(is.null(title) || identical(title, "")) {
      title <- ilias_extract_item_title(items[[k]])
    }
    item_id <- paste0("il_0_qst_", prefix, sprintf(paste0("%0", width_id, "d"), k))
    rval[[i]][[j]]$metainfo$id <- item_id
    qrefs[k] <- item_id

    if(identical(x$metainfo$type, "cloze")) {
      item_xml[[k]] <- make_item_ilias_cloze(items[[k]], rval[[i]][[j]], item_id, title,
        final_maxattempts[k])
    } else {
      item_xml[[k]] <- patch_item_ilias(items[[k]], item_id, title,
        ilias_question_type(x$metainfo$type), final_maxattempts[k])
    }
  }

  qti_xml <- c(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<!DOCTYPE questestinterop SYSTEM "ims_qtiasiv1p2p1.dtd">',
    '<questestinterop>',
    unlist(item_xml, use.names = FALSE),
    '</questestinterop>'
  )
  qti_xml <- ilias_collapse_xml(qti_xml, xmlcollapse)

  if(zip) {
    pkgdir <- file.path(workdir, name)
    dir.create(pkgdir)
    writeLines(qti_xml, file.path(pkgdir, paste0(name, "_qti.xml")))
    writeLines(make_qpl_xml(name, qrefs, pool_id), file.path(pkgdir, paste0(name, "_qpl.xml")))

    if(metasolution) solution_to_qtimetadata(name, rval, path = workdir)

    owd <- getwd()
    setwd(workdir)
    on.exit(setwd(owd), add = TRUE)
    file.rename(name, paste0(name, "_qpl"))
    utils::zip(zipfile = paste0(name, "_qpl.zip"), files = paste0(name, "_qpl"))
    file.copy(file.path(workdir, paste0(name, "_qpl.zip")),
      file.path(outdir, paste0(name, "_qpl.zip")), overwrite = TRUE)
  } else {
    writeLines(qti_xml, file.path(outdir, paste0(name, ".xml")))
  }

  invisible(rval)
}
