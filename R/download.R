#' Download files or folders
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param sfs sfsclient: as returned by [sfs_connect()]
#' @param path character: one or more file or folder paths (each starting with a shared folder. Probably needs to start with a "/")
#' @param out_dir string: destination directory. If not provided, a temporary directory will be used
#' @param mode string: "download" (default) or "open"
#' @param progress logical: show download progress?
#' @param verbose logical: give debug info?
#'
#' @return A data.frame or `NULL`
#'
#' @export
sfs_download <- function(sfs, path, out_dir, mode = "download", progress = TRUE, verbose = sfs_verbose()) {
    if (missing(out_dir)) {
        out_dir <- tempfile()
        dir.create(out_dir)
    }
    if (!dir.exists(out_dir)) stop("out_dir does not exist: ", out_dir)
    mode <- match.arg(mode, c("download", "open"))
    url <- paste0("entry.cgi?api=SYNO.FileStation.Download&version=2&method=download&path=", sqb(path), "&mode=\"", mode, "\"")
    outfile <- if (length(path) == 1) file.path(out_dir, basename(path)) else tempfile(tmpdir = out_dir)
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
