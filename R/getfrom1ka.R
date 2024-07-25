getfrom1ka <- function(
    identifier = NULL, key = NULL, survey = NULL,
    action = NULL, params = NULL
) {
    if (is.null(identifier)) {
        admisc::stopError("The 1ka identifier is required.")
    }

    if (is.null(key)) {
        admisc::stopError("The 1ka private key is required.")
    }

    if (is.null(survey)) {
        admisc::stopError("The 1ka survey id is required.")
    }

    if (is.null(action)) {
        admisc::stopError("The 1ka API action is required.")
    }

    url <- paste0(
        "https://www.1ka.si/api/",
        action,
        "/survey/",
        survey,
        '?'
    )

    if (!is.null(params)) {
        url <- paste0(
            url,
            paste0(names(params), '=', params, collapse = '&')
        )
    }

    response <- httr::GET(
        paste0(
            url,
            '&identifier=', identifier,
            '&token=', openssl::sha256(paste0("GET", url), key)
        )
    )

    # check if the request was successful
    code <- httr::status_code(response)

    if (code != 200) {
        admisc::stopError(
            paste(
                "The request to the 1ka API was not successful.",
                "The status code was:",
                code
            )
        )
    }

    text <- httr::content(response, "text")
    if (text == "") {
        admisc::stopError(
            "The response from the 1ka API was empty."
        )
    }

    tc <- admisc::tryCatchWEM({
        json <- jsonlite::fromJSON(text)
    })

    if (!is.null(tc$error)) {
        admisc::stopError(
            paste(
                "An error occurred while extracting the JSON response:",
                tc$error
            )
        )
    }

    return(json)
}