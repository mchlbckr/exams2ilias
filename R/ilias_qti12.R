## Adapted from exams::exams_eval() and exams:::.empty_text() to avoid
## relying on non-exported objects from the upstream package.
.ilias_exams_eval <- function(partial = TRUE, negative = FALSE,
  rule = c("false2", "false", "true", "all", "none"))
{
  rule <- match.arg(rule, c("false2", "false", "true", "all", "none"))

  if(is.logical(negative)) negative <- ifelse(negative, -1, 0)
  negative <- -abs(as.numeric(negative))

  extype <- function(correct, answer = NULL, type = NULL) {
    mchoice01 <- function(x) as.numeric(strsplit(unlist(x), "")[[1L]])

    if(is.null(type)) {
      type <- if(is.numeric(correct)) {
        "num"
      } else if(is.logical(correct)) {
        "mchoice"
      } else if(is.character(correct)) {
        if(all(strsplit(correct, "")[[1L]] %in% c("0", "1"))) "mchoice" else "string"
      } else {
        "unknown"
      }
    }
    if(!(type %in% c("num", "mchoice", "schoice", "string"))) stop("Unknown exercise type.")

    if(type != "string" && is.character(correct)) {
      correct <- if(type == "num") as.numeric(correct) else as.logical(mchoice01(correct))
    }

    if(!is.null(answer)) {
      answer <- switch(type,
        "num" = {
          if(is.character(answer)) answer <- gsub(",", ".", answer, fixed = TRUE)
          as.numeric(answer)
        },
        "mchoice" = {
          if(is.character(answer)) answer <- mchoice01(answer)
          as.logical(answer)
        },
        "schoice" = {
          if(is.character(answer)) answer <- mchoice01(answer)
          as.logical(answer)
        },
        "string" = {
          as.character(answer)
        }
      )
      if(!any(is.na(answer)) && (length(correct) != length(answer))) {
        stop("Length of 'correct' and given 'answer' do not match.")
      }
    }

    list(type = type, correct = correct, answer = answer)
  }

  checkanswer <- function(correct, answer, tolerance = 0, type = NULL) {
    type <- extype(correct, answer, type = type)
    correct <- type$correct
    answer <- type$answer
    type <- type$type

    if(is.null(answer)) return(rep.int(0, length(correct)))

    if(type == "num") {
      if(any(is.na(answer))) return(0L)
      if(all(answer >= correct - tolerance & answer <= correct + tolerance)) {
        return(1L)
      } else {
        return(-1L)
      }
    }

    if(type %in% c("mchoice", "schoice")) {
      if(any(is.na(answer))) return(0L)
      if(partial && type == "mchoice") {
        rval <- rep.int(0L, length(answer))
        if(all(!answer)) return(rval)
        rval[which(correct & answer)] <- 1L
        rval[which(!correct & answer)] <- -1L
        return(rval)
      } else {
        if(any(is.na(answer))) return(0)
        if(negative < 0 && all(!answer)) return(0)
        return(ifelse(all(correct == answer), 1L, -1L))
      }
    }

    if(type == "string") {
      if(any(is.na(answer)) || all(grepl("^[[:space:]]*$", answer))) return(NA)
      return(ifelse(correct == answer, 1L, -1L))
    }
  }

  pointvec <- function(correct = NULL, type = NULL) {
    if(!partial) return(c("pos" = 1, "neg" = negative))
    if(is.null(correct)) stop("Need 'correct' answer to compute points vector.")
    type <- extype(correct, type = type)
    if(all(!type$correct)) warning("partial credits for mchoice answer without correct answer alternatives")
    if(type$type == "mchoice") {
      n <- switch(rule,
        "false" = -1 / sum(!type$correct),
        "false2" = -1 / pmax(sum(!type$correct), 2),
        "true" = -1 / sum(type$correct),
        "all" = -1,
        "none" = 0
      )
      pv <- c("pos" = 1 / sum(type$correct), "neg" = n)
    } else {
      pv <- c("pos" = 1, "neg" = negative)
    }
    pv
  }

  pointsum <- function(correct, answer, tolerance = 0, type = NULL) {
    pts <- pointvec(correct, type = type)
    chk <- as.character(checkanswer(correct, answer, tolerance = tolerance, type = type))
    res <- rep(0, length.out = length(chk))
    res[which(chk == "1")] <- pts["pos"]
    res[which(chk == "-1")] <- pts["neg"]
    pmax(sum(res), negative)
  }

  list(
    partial = partial,
    negative = negative,
    rule = rule,
    checkanswer = checkanswer,
    pointvec = pointvec,
    pointsum = pointsum
  )
}

.ilias_empty_text <- function(x) {
  is.null(x) || anyNA(x) || all(grepl("^[[:space:]]*$", x))
}

ilias_make_id <- function(size, n = 1L) {
  if(is.null(n)) n <- 1L
  rval <- matrix(sample(0:9, size * n, replace = TRUE), ncol = n, nrow = size)
  colSums(rval * 10^((size - 1L):0L))
}

ilias_delete_NULLs <- function(x.list) {
  rval <- x.list[unlist(lapply(x.list, length) != 0)]
  if(length(rval)) rval else NULL
}

ilias_exams2qti12 <- function(file, n = 1L, nsamp = NULL, dir = ".",
  name = NULL, quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, rds = FALSE,
  seed = NULL, resolution = 100, width = 4, height = 4, svg = FALSE, encoding = "UTF-8",
  num = NULL, mchoice = NULL, schoice = mchoice, string = NULL, cloze = NULL,
  template = "qti12", duration = NULL, stitle = "Exercise", ititle = "Question",
  adescription = "Please solve the following exercises.",
  sdescription = "Please answer the following question.",
  maxattempts = 1, cutvalue = 0, solutionswitch = TRUE, zip = TRUE,
  points = NULL, eval = list(partial = TRUE, rule = "false2", negative = FALSE),
  converter = NULL, envir = NULL, engine = NULL, xmlcollapse = FALSE,
  flavor = c("plain", "openolat", "canvas", "ilias"), ...)
{
  flavor <- "ilias"

  itembody <- list(num = num, mchoice = mchoice, schoice = schoice, cloze = cloze, string = string)
  for(i in c("num", "mchoice", "schoice", "cloze", "string")) {
    if(is.null(itembody[[i]])) itembody[[i]] <- list()
    if(is.list(itembody[[i]])) {
      if(is.null(itembody[[i]]$eval)) itembody[[i]]$eval <- eval
      itembody[[i]]$flavor <- flavor
      itembody[[i]] <- do.call(ilias_make_itembody_qti12, itembody[[i]])
    }
    if(!is.function(itembody[[i]])) stop(sprintf("wrong specification of %s", sQuote(i)))
  }

  exams::exams2qti12(
    file = file, n = n, nsamp = nsamp, dir = dir,
    name = name, quiet = quiet, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose,
    rds = rds, seed = seed, resolution = resolution, width = width, height = height,
    svg = svg, encoding = encoding, num = itembody$num, mchoice = itembody$mchoice,
    schoice = itembody$schoice, string = itembody$string, cloze = itembody$cloze,
    template = template, duration = duration, stitle = stitle, ititle = ititle,
    adescription = adescription, sdescription = sdescription, maxattempts = maxattempts,
    cutvalue = cutvalue, solutionswitch = solutionswitch, zip = zip, points = points,
    eval = eval, converter = converter, envir = envir, engine = engine,
    xmlcollapse = xmlcollapse, flavor = flavor, ...
  )
}

ilias_make_itembody_qti12 <- function(rtiming = FALSE, shuffle = FALSE, rshuffle = shuffle,
  minnumber = NULL, maxnumber = NULL, defaultval = NULL, minvalue = NULL,
  maxvalue = NULL, cutvalue = NULL, enumerate = FALSE, digits = NULL, tolerance = is.null(digits),
  fix_num = FALSE, maxchars = c(32, 1, 12),
  eval = list(partial = TRUE, rule = "false2", negative = FALSE),
  flavor = c("plain", "openolat", "canvas", "ilias"))
{
  function(x) {
    flavor <- match.arg(flavor, "ilias")

    points <- if(is.null(x$metainfo$points)) 1 else x$metainfo$points

    solution <- if(!is.list(x$metainfo$solution)) {
      list(x$metainfo$solution)
    } else {
      x$metainfo$solution
    }
    n <- length(solution)

    questionlist <- if(!is.list(x$questionlist)) {
      if(x$metainfo$type == "cloze") {
        g <- rep(seq_along(x$metainfo$solution), sapply(x$metainfo$solution, length))
        split(x$questionlist, g)
      } else {
        list(x$questionlist)
      }
    } else {
      x$questionlist
    }
    if(length(questionlist) < 1L) {
      questionlist <- NULL
    } else {
      questionlist <- lapply(questionlist, function(q) {
        ifelse(grepl("<span", q, fixed = TRUE), q, paste0("<span>", q, "</span>"))
      })
    }
    if(is.null(questionlist)) {
      questionlist <- vector("list", n)
    }
    if(length(questionlist) < n) {
      questionlist <- c(questionlist, vector("list", n - length(questionlist)))
    }

    tol <- if(!is.list(x$metainfo$tolerance)) {
      if(x$metainfo$type == "cloze") as.list(x$metainfo$tolerance) else list(x$metainfo$tolerance)
    } else {
      x$metainfo$tolerance
    }
    tol <- rep(tol, length.out = n)

    if(length(points) == 1L && x$metainfo$type == "cloze") points <- points / n

    q_points <- rep(points, length.out = n)
    if(x$metainfo$type == "cloze") points <- sum(q_points)

    type <- x$metainfo$type
    type <- if(type == "cloze") x$metainfo$clozetype else rep(type, length.out = n)

    if(is.null(eval) || length(eval) < 1L) eval <- .ilias_exams_eval()
    if(!is.list(eval)) stop("'eval' needs to specify a list of partial/negative/rule")
    eval <- eval[match(c("partial", "negative", "rule"), names(eval), nomatch = 0)]
    if(x$metainfo$type %in% c("num", "string", "schoice")) eval$partial <- FALSE
    eval <- do.call(.ilias_exams_eval, eval)

    maxchars <- if(is.null(x$metainfo$maxchars)) {
      if(length(maxchars) < 2L) c(maxchars, NA, NA) else maxchars[1:3]
    } else {
      x$metainfo$maxchars
    }
    if(!is.list(maxchars)) maxchars <- list(maxchars)
    maxchars <- rep(maxchars, length.out = n)
    for(j in seq_along(maxchars)) {
      if(length(maxchars[[j]]) < 2L) maxchars[[j]] <- c(maxchars[[j]], NA, NA)
    }

    xml <- c(
      '<presentation>',
      '<flow>',
      if(!is.null(x$question)) {
        c(
          '<material>',
          '<matbreak/>',
          '<mattext texttype="text/html" charset="utf-8"><![CDATA[',
          x$question,
          ']]></mattext>',
          '<matbreak/>',
          '</material>'
        )
      } else {
        NULL
      }
    )

    letters2 <- c(letters, paste0(sort(rep(letters, length(letters))), rep(letters, length(letters))))

    ids <- pv <- vector("list", n)
    for(i in seq_len(n)) {
      iid <- x$metainfo$id

      ids[[i]] <- list(
        response = paste(iid, "RESPONSE", ilias_make_id(7), sep = "_"),
        questions = paste(iid, ilias_make_id(10, length(solution[[i]])), sep = "_")
      )

      if(type[i] %in% c("schoice", "mchoice")) {
        pv[[i]] <- eval$pointvec(solution[[i]])
      } else {
        pv[[i]] <- eval$pointvec(TRUE)
      }
      pv[[i]]["pos"] <- pv[[i]]["pos"] * q_points[i]
      if(grepl("choice", type[i])) pv[[i]]["neg"] <- pv[[i]]["neg"] * q_points[i]

      txml <- character(0)

      if(grepl("choice", type[i])) {
        choice_labels <- questionlist[[i]]
        if(length(choice_labels) < length(solution[[i]])) {
          choice_labels <- c(choice_labels, rep("", length(solution[[i]]) - length(choice_labels)))
        }

        txml <- c(
          paste(
            '<response_lid ident="', ids[[i]]$response, '" rcardinality="',
            if(type[i] == "mchoice") "Multiple" else "Single",
            '" rtiming=', if(rtiming) '"Yes"' else '"No"', '>',
            sep = ""
          ),
          paste('<render_choice shuffle="', if(shuffle) "Yes" else "No", '">', sep = "")
        )

        for(j in seq_along(solution[[i]])) {
          choice_text <- choice_labels[j]
          if(is.na(choice_text)) choice_text <- ""
          txml <- c(
            txml,
            '<flow_label class="List">',
            paste(
              '<response_label ident="', ids[[i]]$questions[j], '" rshuffle="',
              if(rshuffle) "Yes" else "No", '">',
              sep = ""
            ),
            '<material>',
            '<mattext texttype="text/html" charset="utf-8"><![CDATA[',
            paste(
              if(enumerate && n > 1L) {
                paste(
                  letters2[if(x$metainfo$type == "cloze") i else j], ".",
                  if(x$metainfo$type == "cloze" && length(solution[[i]]) > 1L) paste0(j, ".") else NULL,
                  sep = ""
                )
              } else {
                NULL
              },
              choice_text
            ),
            ']]></mattext>',
            '</material>',
            '</response_label>',
            '</flow_label>'
          )
        }

        txml <- c(txml, '</render_choice>', '</response_lid>')
      }

      if(type[i] %in% c("string", "num")) {
        for(j in seq_along(solution[[i]])) {
          soltext <- if(type[i] == "num") {
            if(!is.null(digits)) format(round(solution[[i]][j], digits), nsmall = digits) else solution[[i]][j]
          } else {
            if(!is.character(solution[[i]][j])) format(solution[[i]][j]) else solution[[i]][j]
          }

          qtxt <- if(length(questionlist[[i]]) >= j) questionlist[[i]][j] else NA_character_
          qlc <- .ilias_empty_text(qtxt)

          txml <- c(
            txml,
            if(!qlc) {
              c(
                '<material>',
                paste(
                  '<mattext><![CDATA[',
                  paste(if(enumerate && n > 1L) paste0(letters2[i], ".") else NULL, qtxt),
                  ']]></mattext>',
                  sep = ""
                ),
                '</material>',
                '<material>', '<matbreak/>', '</material>'
              )
            } else {
              NULL
            },
            paste(
              if(type[i] == "string" || !tolerance || fix_num) '<response_str ident="' else '<response_num ident="',
              ids[[i]]$response,
              if(!is.na(maxchars[[i]][3])) '" rcardinality="Ordered"' else '" rcardinality="Single"',
              if(type[i] == "num" && tolerance && !fix_num) ' numtype="Decimal"' else NULL,
              '>',
              sep = ""
            ),
            paste(
              '<render_fib',
              if(type[i] == "num" && tolerance && !fix_num) ' fibtype="Decimal"' else NULL,
              if(!is.na(maxchars[[i]][1])) paste0(' maxchars="', max(c(nchar(soltext), maxchars[[i]][1])), '"') else NULL,
              if(!is.na(maxchars[[i]][2])) paste0(' rows="', maxchars[[i]][2], '"') else NULL,
              if(!is.na(maxchars[[i]][3])) paste0(' columns="', maxchars[[i]][3], '"') else NULL,
              if(!is.na(maxchars[[i]][3])) ' fibtype="String" prompt="Box"' else NULL,
              '>',
              sep = ""
            ),
            '<flow_label class="Block">',
            paste0('<response_label ident="', ids[[i]]$response, '" rshuffle="No"/>'),
            '</flow_label>',
            '</render_fib>',
            if(type[i] == "string" || !tolerance || fix_num) '</response_str>' else '</response_num>',
            '<material>', '<matbreak/>', '</material>'
          )
        }
      }

      xml <- c(xml, txml)
    }

    xml <- c(xml, '</flow>', '</presentation>')

    if(is.null(minvalue)) {
      if(eval$negative) {
        minvalue <- sum(sapply(pv, function(x) x["neg"]))
      } else {
        minvalue <- 0
      }
    }

    xml <- c(
      xml,
      '<resprocessing>',
      '<outcomes>',
      paste(
        '<decvar varname="SCORE" vartype="Decimal" defaultval="',
        if(is.null(defaultval)) 0 else defaultval,
        '" minvalue="', if(is.null(minvalue) || is.na(minvalue)) 0 else minvalue,
        '" maxvalue="', if(is.null(maxvalue)) points else maxvalue,
        '" cutvalue="', if(is.null(cutvalue)) points else cutvalue,
        '"/>',
        sep = ""
      ),
      '</outcomes>'
    )

    correct_answers <- wrong_answers <- correct_num <- wrong_num <- vector("list", n)
    for(i in seq_len(n)) {
      if(grepl("choice", type[i])) {
        for(j in seq_along(solution[[i]])) {
          if(solution[[i]][j]) {
            correct_answers[[i]] <- c(
              correct_answers[[i]],
              paste(
                '<varequal respident="', ids[[i]]$response, '" case="Yes">',
                ids[[i]]$questions[j], '</varequal>',
                sep = ""
              )
            )
          } else {
            wrong_answers[[i]] <- c(
              wrong_answers[[i]],
              paste(
                '<varequal respident="', ids[[i]]$response, '" case="Yes">',
                ids[[i]]$questions[j], '</varequal>',
                sep = ""
              )
            )
          }
        }
      }

      if(type[i] %in% c("string", "num")) {
        for(j in seq_along(solution[[i]])) {
          if(type[i] == "string") {
            soltext <- if(!is.character(solution[[i]][j])) {
              format(round(solution[[i]][j], digits), nsmall = digits)
            } else {
              solution[[i]][j]
            }
            correct_answers[[i]] <- c(
              correct_answers[[i]],
              paste('<varequal respident="', ids[[i]]$response, '" case="No"><![CDATA[', soltext, ']]></varequal>', sep = "")
            )
          } else {
            correct_answers[[i]] <- c(
              correct_answers[[i]],
              if(!tolerance) {
                paste(
                  '<varequal respident="', ids[[i]]$response, '" case="No"><![CDATA[',
                  if(!is.null(digits)) format(round(solution[[i]][j], digits), nsmall = digits) else solution[[i]][j],
                  ']]></varequal>',
                  sep = ""
                )
              } else {
                if(fix_num) {
                  correct_num[[i]] <- c(
                    correct_num[[i]],
                    paste(
                      '<varequal respident="', ids[[i]]$response, '" case="No"><![CDATA[',
                      if(!is.null(digits)) format(round(solution[[i]][j], digits), nsmall = digits) else solution[[i]][j],
                      ']]></varequal>',
                      sep = ""
                    )
                  )
                }
                wrong_num[[i]] <- paste(
                  '<and>',
                  paste0('<vargte respident="', ids[[i]]$response, '">', solution[[i]][j] - max(tol[[i]]), '</vargte>'),
                  paste0('<varlte respident="', ids[[i]]$response, '">', solution[[i]][j] + max(tol[[i]]), '</varlte>'),
                  '</and>',
                  sep = "\n"
                )
              }
            )
          }
        }
      }

      if(!is.null(correct_answers[[i]])) {
        attr(correct_answers[[i]], "points") <- pv[[i]]
        attr(correct_answers[[i]], "type") <- type[i]
      }
      if(!is.null(wrong_answers[[i]])) attr(wrong_answers[[i]], "points") <- pv[[i]]
    }

    correct_answers <- ilias_delete_NULLs(correct_answers)
    wrong_answers <- ilias_delete_NULLs(wrong_answers)
    correct_num <- unlist(ilias_delete_NULLs(correct_num))
    wrong_num <- ilias_delete_NULLs(wrong_num)
    if(length(wrong_num)) {
      wrong_num <- unlist(sapply(wrong_num, function(x) paste('<not>', x, '</not>', collapse = '\n')))
    }

    if(x$metainfo$type == "schoice") {
      xml <- c(
        xml,
        '<respcondition title="Mastery" continue="Yes">',
        '<conditionvar>',
        unlist(correct_answers),
        '</conditionvar>',
        paste('<setvar varname="SCORE" action="Add">', points, '</setvar>', sep = ""),
        '<displayfeedback feedbacktype="Response" linkrefid="Mastery"/>',
        '</respcondition>',
        '<respcondition title="Fail" continue="Yes">',
        '<conditionvar><other/></conditionvar>',
        '<setvar varname="SCORE" action="Set">0</setvar>',
        '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
        '</respcondition>'
      )
    } else if(x$metainfo$type == "mchoice") {
      if(length(correct_answers)) {
        for(i in seq_along(correct_answers)) {
          for(j in correct_answers[[i]]) {
            pts <- if(eval$partial) attr(correct_answers[[i]], "points")["pos"] else points
            xml <- c(
              xml,
              '<respcondition continue="Yes">',
              '<conditionvar>', j, '</conditionvar>',
              paste0('<setvar varname="SCORE" action="Add">', pts, '</setvar>'),
              '</respcondition>',
              '<respcondition continue="Yes">',
              '<conditionvar><not>', j, '</not></conditionvar>',
              '<setvar varname="SCORE" action="Add">0</setvar>',
              '</respcondition>'
            )
          }
        }
      }
      if(length(wrong_answers)) {
        for(i in seq_along(wrong_answers)) {
          for(j in wrong_answers[[i]]) {
            pts <- if(eval$partial) attr(wrong_answers[[i]], "points")["neg"] else 0
            xml <- c(
              xml,
              '<respcondition continue="Yes">',
              '<conditionvar>', j, '</conditionvar>',
              paste0('<setvar varname="SCORE" action="Add">', pts, '</setvar>'),
              '</respcondition>',
              '<respcondition continue="Yes">',
              '<conditionvar><not>', j, '</not></conditionvar>',
              '<setvar varname="SCORE" action="Add">0</setvar>',
              '</respcondition>'
            )
          }
        }
      }
      xml <- c(
        xml,
        '<respcondition title="Fail" continue="Yes">',
        '<conditionvar><other/></conditionvar>',
        '<setvar varname="SCORE" action="Set">0</setvar>',
        '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
        '</respcondition>'
      )
    } else {
      if((eval$partial || x$metainfo$type == "cloze")) {
        if(length(correct_answers)) {
          for(i in seq_along(correct_answers)) {
            for(j in correct_answers[[i]]) {
              xml <- c(
                xml,
                '<respcondition continue="Yes" title="Mastery">',
                '<conditionvar>', j, '</conditionvar>',
                paste('<setvar varname="SCORE" action="Add">', attr(correct_answers[[i]], "points")["pos"], '</setvar>', sep = ""),
                '</respcondition>'
              )
            }
          }
        }
        if(length(wrong_answers)) {
          for(i in seq_along(wrong_answers)) {
            for(j in wrong_answers[[i]]) {
              xml <- c(
                xml,
                '<respcondition continue="Yes" title="Fail">',
                '<conditionvar>', j, '</conditionvar>',
                paste('<setvar varname="SCORE" action="Add">', attr(wrong_answers[[i]], "points")["neg"], '</setvar>', sep = ""),
                '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
                '</respcondition>'
              )
            }
          }
        }
      }

      if(eval$partial && x$metainfo$type == "cloze" && length(correct_answers)) {
        for(i in seq_along(correct_answers)) {
          ctype <- attr(correct_answers[[i]], "type")
          if(ctype %in% c("string", "num", "schoice")) {
            xml <- c(
              xml,
              '<respcondition title="Fail" continue="Yes">',
              '<conditionvar>', '<not>', correct_answers[[i]], '</not>', '</conditionvar>',
              paste('<setvar varname="SCORE" action="Add">', attr(correct_answers[[i]], "points")["neg"], '</setvar>', sep = ""),
              '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
              '</respcondition>'
            )
          }
        }
      }

      xml <- c(
        xml,
        '<respcondition title="Mastery" continue="Yes">',
        '<conditionvar>',
        if(length(correct_answers) > 1L || grepl("choice", x$metainfo$type)) '<and>' else NULL,
        unlist(correct_answers),
        if(length(correct_answers) > 1L || grepl("choice", x$metainfo$type)) '</and>' else NULL,
        if(length(wrong_answers)) c('<not>', '<or>', unlist(wrong_answers), '</or>', '</not>') else NULL,
        '</conditionvar>',
        if(!eval$partial) paste('<setvar varname="SCORE" action="Set">', points, '</setvar>', sep = "") else NULL,
        '<displayfeedback feedbacktype="Response" linkrefid="Mastery"/>',
        '</respcondition>'
      )

      if(length(correct_num)) {
        for(j in correct_num) {
          xml <- c(
            xml,
            '<respcondition continue="Yes" title="Mastery">',
            '<conditionvar>', j, '</conditionvar>',
            if(fix_num) {
              c(
                paste('<setvar varname="SCORE" action="Add">', 0.001, '</setvar>', sep = ""),
                paste('<setvar varname="SCORE" action="Add">', -0.001, '</setvar>', sep = "")
              )
            } else {
              NULL
            },
            '</respcondition>'
          )
        }
      }

      if(length(correct_answers)) {
        for(j in seq_along(correct_answers)) {
          if(attr(correct_answers[[j]], "type") != "num") {
            xml <- c(
              xml,
              '<respcondition continue="Yes" title="Mastery">',
              '<conditionvar>', correct_answers[[j]], '</conditionvar>',
              '</respcondition>'
            )
          }
        }
      }

      correct_answers <- unlist(correct_answers)
      wrong_answers <- c(unlist(wrong_answers), unlist(wrong_num))

      if(!eval$partial && x$metainfo$type == "cloze") {
        if(length(correct_answers)) {
          for(i in seq_along(correct_answers)) {
            xml <- c(
              xml,
              '<respcondition title="Fail" continue="Yes">',
              '<conditionvar>', '<not>', correct_answers[[i]], '</not>', '</conditionvar>',
              paste('<setvar varname="SCORE" action="Add">', -1 * n * points, '</setvar>', sep = ""),
              '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
              '</respcondition>'
            )
          }
        }

        if(length(wrong_answers)) {
          for(i in seq_along(wrong_answers)) {
            xml <- c(
              xml,
              '<respcondition continue="Yes" title="Fail">',
              '<conditionvar>', wrong_answers[[i]], '</conditionvar>',
              paste('<setvar varname="SCORE" action="Add">', -1 * n * points, '</setvar>', sep = ""),
              '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
              '</respcondition>'
            )
          }
        }
      }

      xml <- c(
        xml,
        '<respcondition title="Fail" continue="Yes">',
        '<conditionvar>',
        if(length(wrong_answers)) NULL else '<not>',
        if(length(wrong_answers)) c('<or>', wrong_answers, '</or>') else c(if(length(correct_answers) > 1L) '<and>' else NULL, correct_answers, if(length(correct_answers) > 1L) '</and>' else NULL),
        if(length(wrong_answers)) NULL else '</not>',
        '</conditionvar>',
        if(!eval$partial && !is.na(minvalue)) paste('<setvar varname="SCORE" action="Set">', minvalue, '</setvar>', sep = "") else NULL,
        '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
        '</respcondition>',
        '<respcondition title="Fail" continue="Yes">',
        '<conditionvar>', '<other/>', '</conditionvar>',
        '<setvar varname="SCORE" action="Set">0</setvar>',
        '<displayfeedback feedbacktype="Solution" linkrefid="Solution"/>',
        '</respcondition>'
      )
    }

    xml <- c(xml, '</resprocessing>')
    attr(xml, "enumerate") <- enumerate
    xml
  }
}
