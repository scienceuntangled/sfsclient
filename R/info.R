#' Retrieve API information from a Synology File Station
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param url string or sfsclient: URL to connect to, e.g. "https://my.sfs.com/webapi/", or an sfsclient object
#' @param verbose logical: give debug info?
#'
#' @return A list
#'
#' @export
sfs_info <- function(url, verbose = sfs_verbose()) {
    if (inherits(url, "sfsclient")) url <- url$base_url
    info_url <- paste0(sub("/+$", "/", paste0(url, "/")), "query.cgi?api=SYNO.API.Info&version=1&method=query&query=all")
    rsp <- sfsget(NULL, info_url, verbose = verbose)
    ##cat(str(content(rsp)))
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE)$data
    } else {
        NULL
    }
}

#' Retrieve File Station information
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param sfs sfsclient: as returned by [sfs_connect()]
#' @param verbose logical: give debug info?
#'
#' @return A list, with information potentially including
#' * is_manager - if the logged-in user is an administrator
#' * support_virtual_protocol - types of virtual file system which the logged user is able to mount on (CIFS, NFS, ISO)
#' * support_sharing - if the logged-in user can share files/folders
#' * hostname - host name
#'
#' @export
sfs_fs_info <- function(sfs, verbose = sfs_verbose()) {
    url <- "entry.cgi?api=SYNO.FileStation.Info&version=2&method=get"
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE)$data
    } else {
        NULL
    }
}
