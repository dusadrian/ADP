makeCodebook <- function(sav, json, lang = "sl-SI") {

    if (missing(sav)) {
        admisc::stopError("The sav file is required.")
    }

    tp_file <- DDIwR::treatPath(sav, type = "*", single = TRUE, check = FALSE)

    if (missing(json)) {
        admisc::stopError("The json file is required.")
    }

    # missings <- c(
    #     "Ne vem" = -99, # Don't know
    #     "Zavrnil" = -98, # Refused
    #     "Neustrezno" = -97, # Invalid
    #     "Ni\u010d od navedenega" = -96, # None of above
    #     "Se ne uporablja" = -2, # Not applicable
    #     "Ni odgovorjeno" = -1 # Not answered
    # )

    # Single answer = En odgovor
    # Dropdown menu - single answer = Roleta - en odgovor
    # Multiple answer = Ve&#269; odgovorov (Ve\u10d odgovorov)
    # Numeric input = &#352;tevilo (\u160tevilo)
    # Text input = Besedilo
    # Single answer table = Tabela en odgovor
    # Multiple answers (table) = Tabela ve&#269; odgovorov (Tabela ve\u10d odgovorov)
    # Number table = Tabela &#353;tevilo (Tabela \u161tevilo)
    # Text table = Tabela besedilo
    # Date input = Datum

    type <- qtext(sapply(json, function(x) x$tip))

    # question id
    qid <- qtext(sapply(json, function(x) x$variable))

    # question text
    question <- qtext(sapply(json, function(x) x$naslov))

    # instructions (subquestion)
    info <- qtext(sapply(json, function(x) x$info))

    result <- vector(mode = "list", length = length(type))
    for (i in seq(length(type))) {
        vrednosti <- json[[i]]$vrednosti
        stolpci <- json[[i]]$stolpci

        # predefined
        var_name <- qid[i]
        var_label <- question[i]

        values <- labels <- NULL

        if (length(vrednosti)) {

            values <- qtext(sapply(vrednosti, function(x) x$variable))
            if (admisc::possibleNumeric(values)) {
                values <- admisc::asNumeric(values)
            }

            labels <- qtext(sapply(vrednosti, function(x) x$naslov))

            if (any(grepl(var_name, values))) {
                var_name <- values
                var_label <- labels
                if (grepl("Multiple answer", type[i])) {
                    values <- 0:1
                    labels <- c("No", "Yes")
                } else {
                    values <- labels <- NULL
                }
            }
        }

        if (length(stolpci)) {
            ma <- grepl(
                tolower(paste(
                    c(
                        "Multiple answer", # that includes "Multiple answers" (table)
                        qtext("Ve&#269; odgovorov"), # that includes "Tabela ve&#269; odgovorov"
                        qtext("Ve\u10d odgovorov")
                    ),
                    collapse = "|"
                )),
                tolower(type[i])
            )

            nt <- grepl(
                paste(
                    c(
                        "Number table",
                        qtext("Tabela &#353;tevilo"),
                        qtext("Tabela \u161tevilo")
                    ),
                    collapse = "|"
                ),
                type[i]
            )

            tt <- grepl(
                paste(
                    c("Text table", "Tabela besedilo"),
                    collapse = "|"
                ),
                type[i]
            )

            if (ma || nt || tt) {
                sufix <- seq(length(stolpci))
                if (tt) {
                    sufix <- qtext(sapply(stolpci, function(x) x$variable))
                }
                var_name <- paste(
                    rep(var_name, each = length(stolpci)),
                    sufix,
                    sep = "_"
                )
                var_label <- paste(
                    rep(var_label, each = length(stolpci)),
                    qtext(sapply(stolpci, function(x) x$naslov)),
                    sep = ": "
                )
            } else {
                values <- qtext(sapply(stolpci, function(x) x$variable))
                if (admisc::possibleNumeric(values)) {
                    values <- admisc::asNumeric(values)
                }

                labels <- qtext(sapply(stolpci, function(x) x$naslov))
            }
        }

        if (length(var_name) == 1) {
            var_name <- qid[i]
            var_label <- question[i]
        }

        result[[i]] <- list(
            question_text = question[i],
            var_name = var_name,
            var_label = var_label,
            values = unique(values),
            labels = unique(labels)
        )
    }

    names(result) <- qid

    html    <- c("&#39;", "&#268;", "&#269;", "&#352;", "&#353;", "&#381;", "&#382;")
    unicode <- c("'",     "\u010c", "\u010d", "\u160",  "\u161",  "\u17d",  "\u17e")

    sav <- DDIwR::convert(sav)
    vars <- names(sav)

    varmap <- lapply(vars, function(x) {
        searchvar <- lapply(result, function(r) {
            equal <- r$var_name == x
            if (any(equal)) {
                return(equal)
            }

            return(
                sapply(
                    r$var_name,
                    function(y) {
                        grepl(paste0("^", y), x)
                    }
                )
            )
        })

        if (any(unlist(searchvar))) {
            whas <- which(sapply(searchvar, any))
            if (length(whas) > 1) {
                nchars <- nchar(names(whas))
                whas <- whas[nchars == max(nchars)]
            }

            if (length(whas) == 1) {
                return(unname(c(whas, which(searchvar[[whas]]))))
            } else {
                return(NULL)
            }
        }
    })

    for (i in seq(length(vars))) {
        var_label <- attr(sav[[i]], "label", exact = TRUE)

        if (is.null(var_label)) {
            # The SPSS variable does not have a label, trying to construct it from the 1ka JSON
            if (!is.null(varmap[[i]])) {
                var_label <- result[[varmap[[i]][1]]]$var_label[varmap[[i]][2]]
            }
        }

        if (!is.null(var_label)) {
            for (h in seq_along(html)) {
                var_label <- gsub(html[h], unicode[h], var_label)
            }

            attr(sav[[i]], "label") <- var_label
        }

        value_labels <- attr(sav[[i]], "labels", exact = TRUE)

        if (!is.null(value_labels)) {
            nms <- names(value_labels)
            for (h in seq_along(html)) {
                nms <- gsub(html[h], unicode[h], nms)
            }

            names(value_labels) <- nms
            attr(sav[[i]], "labels") <- value_labels
        }

    }

    tmp <- tempdir()
    DDIwR::convert(
        sav,
        to = file.path(tmp, "temp.xml"),
        embed = FALSE, xmlang = lang, monolang = FALSE
    )

    xml <- readLines(file.path(tmp, "temp.xml"))
    varpos <- which(grepl("<var ID=", xml))

    for (i in rev(seq(length(varpos)))) {
        if (!is.null(varmap[[i]])) {
            qstn <- paste(
                "      <qstn ID=\"", qid[varmap[[i]][1]],
                "\">\n        <qstnLit xml:lang=\"",
                lang, "\">",
                result[[varmap[[i]][1]]]$question_text,
                "</qstnLit>\n      </qstn>",
                sep = ""
            )
            xml <- append(xml, qstn, after = varpos[i])
        }
    }

    writeLines(xml, file.path(
        tp_file$completePath,
        paste(tp_file$filenames[1], "xml", sep = ".")
    ))
}
