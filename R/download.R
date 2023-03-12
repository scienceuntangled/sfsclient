#' Download files or folders
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param sfs sfsclient: as returned by [sfs_connect()]
#' @param path character: one or more file or folder paths (each starting with a shared folder. Probably needs to start with a "/")
#' @param mode string: "download" (default) or "open"
#' @param progress logical: show download progress?
#' @param verbose logical: give debug info?
#'
#' @return A data.frame or `NULL`
#'
#' @export
sfs_download <- function(sfs, path, mode = "download", progress = TRUE, verbose = sfs_verbose()) {
    mode <- match.arg(mode, c("download", "open"))
    url <- paste0("entry.cgi?api=SYNO.FileStation.Download&version=2&method=download&path=", sqb(path), "&mode=\"", mode, "\"")
    outfile <- if (length(path) == 1) {
                   tryCatch({
                       tempd <- tempfile()
                       dir.create(tempd)
                       file.path(tempd, basename(path))
                   }, error = function(e) tempfile())
               } else {
                   tempfile()
               }
    rsp <- sfsget(sfs, url, outfile = outfile, progress = progress, verbose = verbose)
    if (rsp$status_code == 200) {
        ## rename the output file if we can
        if (identical(rsp$type, "application/zip")) {
            zfile <- paste0(tools::file_path_sans_ext(outfile), ".zip")
            file.rename(outfile, zfile)
            outfile <- zfile
        }
        outfile
    } else {
        NULL
    }
}
