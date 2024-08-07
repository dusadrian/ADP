`qtext` <- function(x) {
    admisc::trimstr(unname(sapply(x, function(x) {
        gsub("\u00a0", " ", rvest::html_text(rvest::read_html(
            paste0("<body>", x, "</body>")
        )))
    })))
}
