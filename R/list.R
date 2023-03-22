#' List shared folders
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param sfs sfsclient: as returned by [sfs_connect()]
#' @param additional character: one or more of "real_path", "owner", "time", "perm", "mount_point_type", "sync_share", "volume_status"
#' @param verbose logical: give debug info?
#'
#' @return A data.frame or `NULL`
#'
#' @export
sfs_list_share <- function(sfs, additional = c(), verbose = sfs_verbose()) {
    url <- "entry.cgi?api=SYNO.FileStation.List&version=2&method=list_share"
    if (length(additional) > 0) {
        url <- paste0(url, "&additional=", sqb(additional))
    }
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE)$data$shares
    } else {
        NULL
    }
}


#' List files in a shared folder
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param sfs sfsclient: as returned by [sfs_connect()]
#' @param path string: folder path. Probably needs to start with a "/"
#' @param additional character: one or more of "real_path", "size", "owner", "time", "perm", "type", "mount_point_type"
#' @param verbose logical: give debug info?
#'
#' @return A data.frame or `NULL`
#'
#' @export
sfs_list <- function(sfs, path, additional = c("time"), verbose = sfs_verbose()) {
    url <- paste0("entry.cgi?api=SYNO.FileStation.List&version=2&method=list&folder_path=\"", path, "\"")
    if (length(additional) > 0) {
        url <- paste0(url, "&additional=", sqb(additional))
    }
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        out <- fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE, flatten = TRUE)$data$files
        names(out) <- sub("^additional\\.", "", names(out))
        names(out) <- gsub("\\.+", "_", sub("^time\\.", "", names(out)))
        for (tmc in intersect(c("atime", "crtime", "ctime", "mtime"), names(out))) out[[tmc]] <- as.POSIXct(out[[tmc]], origin = "1970-01-01")
        out
  } else {
        NULL
    }
}
