#' Search files according to given criteria
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param sfs sfsclient: as returned by [sfs_connect()]
#' @param path string: one or more folder paths. Probably needs to start with a "/"
#' @param recursive logical: search recursively?
#' @param pattern string: search for files whose names and extensions match a case-insensitive glob pattern. If `pattern` doesn't contain any glob syntax ("?" or "*"), "*" will be added at begin and end of the string automatically for partially matching the pattern. You can use " " to separate multiple glob patterns
#' @param extension string: Search for files whose extensions match a file type pattern in a case-insensitive glob pattern. If you give this criterion, folders aren't matched. You can use commas "," to separate multiple glob patterns.
#' @param taskid string: search task ID as returned by `sfs_search_start`
#' @param additional character: one or more of "real_path", "size", "owner", "time", "perm", "type", "mount_point_type"
#' @param clean logical: if the search has finished, stop the search process and remove the temporary database? Note that further `sfs_search_list` requests cannot then be issued against this `taskid`
#' @param verbose logical: give debug info?
#'
#' @return A data.frame or `NULL`
#'
#' @export
sfs_search_start <- function(sfs, path, recursive = TRUE, pattern = NULL, extension = NULL, verbose = sfs_verbose()) {
    url <- paste0("entry.cgi?api=SYNO.FileStation.Search&version=2&method=start&folder_path=", sqb(path), "&recursive=", tolower(as.character(recursive)))
    if (length(pattern) > 0) {
        url <- paste0(url, "&pattern=\"", paste(pattern, collapse = " "), "\"")
    }
    if (length(extension) > 0) {
        url <- paste0(url, "&extension=\"", paste(extension, collapse = ","), "\"")
    }
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE, flatten = TRUE)$data$taskid
    } else {
        NULL
    }
}

#' @rdname sfs_search_start
#' @export
sfs_search_list <- function(sfs, taskid, additional = c("time", "size"), clean = TRUE, verbose = sfs_verbose()) {
    url <- paste0("entry.cgi?api=SYNO.FileStation.Search&version=2&method=list&taskid=", taskid)
    if (length(additional) > 0) {
        url <- paste0(url, "&additional=", sqb(additional))
    }
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        out <- fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE, flatten = TRUE)$data
        if ("files" %in% names(out) && is.data.frame(out$files)) {
            names(out$files) <- sub("^additional\\.", "", names(out$files))
            names(out$files) <- gsub("\\.+", "_", sub("^time\\.", "", names(out$files)))
            for (tmc in intersect(c("atime", "crtime", "ctime", "mtime"), names(out$files))) out$files[[tmc]] <- as.POSIXct(out$files[[tmc]], origin = "1970-01-01")
        }
        if (isTRUE(clean) && isTRUE(out$finished)) {
            sfs_search_stop(sfs, taskid = taskid)
            sfs_search_clean(sfs, taskid = taskid)
        }
        out
    } else {
        NULL
    }
}

#' @rdname sfs_search_start
#' @export
sfs_search_stop <- function(sfs, taskid, verbose = sfs_verbose()) {
    url <- paste0("entry.cgi?api=SYNO.FileStation.Search&version=2&method=stop&taskid=", taskid)
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE, flatten = TRUE)$data
    } else {
        NULL
    }
}

#' @rdname sfs_search_start
#' @export
sfs_search_clean <- function(sfs, taskid, verbose = sfs_verbose()) {
    url <- paste0("entry.cgi?api=SYNO.FileStation.Search&version=2&method=clean&taskid=", taskid)
    rsp <- sfsget(sfs, url, verbose = verbose)
    if (status_code(rsp) == 200) {
        fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE, flatten = TRUE)$data
    } else {
        NULL
    }
}

