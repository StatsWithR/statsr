statswithr_lab = function(...) {

  # get the locations of resource files located within the package
  css = system.file("lab.css", package = "statsr")

  # call the base html_document function
  rmarkdown::html_document(css = css, 
                           highlight = "pygments",
                           theme = "cerulean",
                           fig_width = 7,
                           fig_height = 4,
                           ...)
}