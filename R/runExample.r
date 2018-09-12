#' Example of LRErdd in shiny version
#'
#' @export

runExample <- function() {
    appDir <- system.file("shiny-examples", "myapp", package = "LRErdd")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, display.mode = "normal")
}
