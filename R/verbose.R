#' Set global sfsclient verbosity
#'
#' A convenience function that sets the verbosity level for all `sfsclient` functions, without having to explicitly set it on each function call.
#' @param verbose logical: give debug info?
#'
#' @return If called with no parameters, the current verbosity setting. Otherwise sets the value and returns it
#'
#' @examples
#' sfs_verbose(TRUE)
#' sfs_verbose()
#'
#' @export
sfs_verbose <- function(verbose) {
    opts <- getOption("sfsclient")
    if (is.null(opts)) opts <- list()
    if (is.null(opts$verbose)) opts$verbose <- FALSE ## default
    if (!missing(verbose)) {
        opts$verbose <- isTRUE(verbose)
        options(sfsclient = opts)
    }
    opts$verbose
}

