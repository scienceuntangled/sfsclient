## returns the httr response (if outfile is NULL) or the curl response (otherwise)
sfsget <- function(sfs, path_query, outfile = NULL, progress = FALSE, verbose = FALSE) {
    if (is.null(sfs)) {
        if (!grepl("(query|auth)\\.cgi", path_query)) stop("sfs object must be supplied")
    } else if (grepl("(query|auth)\\.cgi", path_query)) {
        ## auth or info
        sfs <- NULL
    } else {
        assert_that(inherits(sfs, "sfsclient"))
        if (is.null(sfs$sid)) stop("no session, use `sfs_connect()` first")
    }
    if (!is.null(sfs) && !grepl("^http", path_query)) {
        path_query <- paste0(sfs$base_url, sub("^/+", "", path_query)) ## ensure no leading / on path_query, because base_url has it
    }
    u <- parse_url(path_query)
    if (!is.null(sfs$sid)) u$query$`_sid` <- sfs$sid
    u <- build_url(u)
    if (is.null(outfile)) {
        rgs <- list(u, add_headers(.headers = c(Cookie = "type=tunnel; stay_login=0;")))
        if (isTRUE(verbose)) rgs <- c(rgs, list(httr::verbose()))
        if (isTRUE(progress)) rgs <- c(rgs, list(httr::progress()))
        rsp <- do.call(GET, rgs)
        if (status_code(rsp) != 200) warning("status code not 200")
        chk <- tryCatch(fromJSON(content(rsp, as = "text"), simplifyDataFrame = TRUE, flatten = TRUE), error = function(e) list())
        if ("success" %in% names(chk) && !isTRUE(chk$success)) {
            errcode <- if ("error" %in% names(chk) && "code" %in% names(chk$error)) chk$error$code else "unknown"
            warning("request was unsuccessful, error code:", errcode)
        }
    } else {
        ## download
        h <- curl::new_handle(Cookie = "type=tunnel; stay_login=0;")
        ##h <- curl::handle_setheaders(h, Cookie = "type=tunnel; stay_login=0;")
        if (isTRUE(progress)) h <- curl::handle_setopt(h, noprogress = FALSE, progressfunction = progress_bar("down", stdout())) ##mode = "wb", 
        rsp <- curl::curl_fetch_disk(u, path = outfile, handle = h)
        if (rsp$status_code != 200) warning("status code not 200")
    }
    rsp
}


sqb <- function(x) {
    ## quote entries of x and collapse to a comma-separated list with square brackets
    paste0("[\"", paste0(x, collapse = "\",\""), "\"]")
}


## httr:::progress_bar
## not exported by that package, so source code copied here
## Copyright (c) 2023 httr authors, MIT license, see https://github.com/r-lib/httr/blob/main/LICENSE.md
progress_bar <- function(type, con) {
    bar <- NULL
    bytes <- function (x, digits = 3, ...) {
        power <- min(floor(log(abs(x), 1000)), 4)
        if (power < 1) {
            unit <- "B"
        }
        else {
            unit <- c("kB", "MB", "GB", "TB")[[power]]
            x <- x/(1000^power)
        }
        formatted <- format(signif(x, digits = digits), big.mark = ",", scientific = FALSE)
        paste0(formatted, " ", unit)
    }
    show_progress <- function(down, up) {
        if (type == "down") {
            total <- down[[1]]
            now <- down[[2]]
        } else {
            total <- up[[1]]
            now <- up[[2]]
        }

        if (total == 0 && now == 0) {
            ## Reset progress bar when seeing first byte
            bar <<- NULL
        } else if (total == 0) {
            cat("\rDownloading: ", bytes(now, digits = 2), "     ", sep = "", file = con)
            utils::flush.console()
            ## Can't automatically add newline on completion because there's no way to tell when then the file has finished downloading
        } else {
            if (is.null(bar)) {
                bar <<- utils::txtProgressBar(max = total, style = 3, file = con)
            }
            utils::setTxtProgressBar(bar, now)
            if (now == total) close(bar)
        }
        TRUE
    }
    show_progress
}
