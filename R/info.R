#' Retrieve API information from a Synology File Station
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param url string: URL to connect to, e.g. "https://my.sfs.com/webapi/"
#' @param verbose logical: give debug info?
#'
#' @return An sfsclient object
#'
#' @export
sfs_info <- function(url, verbose = sfs_verbose()) {
    verbose <- isTRUE(verbose)
    info_url <- paste0(sub("/+$", "/", paste0(url, "/")), "query.cgi?api=SYNO.API.Info&version=1&method=query&query=all")
    rsp <- sfsget(NULL, info_url, verbose = verbose)
    ##cat(str(content(rsp)))
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE)$data
    } else {
        NULL
    }
}
