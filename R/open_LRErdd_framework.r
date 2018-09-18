#' Example of LRErdd in shiny version
#'
#' @export

open_LRErdd_framework <- function() {
    appDir <- system.file("shiny-examples", "LRErdd", package = "LRErdd")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")
}
