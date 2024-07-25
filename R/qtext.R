`qtext` <- function(x) {
    admisc::trimstr(html_text(read_html(
        paste0("<body>", x, "</body>")
    )))
}
