#' Connect to a Synology File Station
#'
#' @references <https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
#' @param url string: URL to connect to, e.g. "https://my.sfs.com/webapi/"
#' @param user string: username. If missing, will be taken from the environment variable `SFS_USER`
#' @param pass string: password. If missing, will be taken from the environment variable `SFS_PASS`
#' @param verbose logical: give debug info?
#'
#' @return An sfsclient object
#'
#' @export
sfs_connect <- function(url, user, pass, verbose = sfs_verbose()) {
    verbose <- isTRUE(verbose)
    if (missing(user) || is.null(user)) user <- Sys.getenv("SFS_USER")
    if (missing(pass) || is.null(pass)) pass <- Sys.getenv("SFS_PASS")
    if (is.null(user) || !nzchar(user) || is.null(pass) || !nzchar(pass)) stop("missing username or password")
    url <- sub("/+$", "/", paste0(url, "/")) ## ensure trailing /
    auth_url <- paste0(url, "auth.cgi?api=SYNO.API.Auth&version=2&format=sid&method=login&account=", user, "&passwd=", pass, "&session=FileStation")
    auth <- sfsget(NULL, auth_url, verbose = verbose)
    ##cat(str(content(auth)))
    sid <- tryCatch(suppressWarnings(fromJSON(content(auth))$data$sid), error = function(e) {
        if (verbose) warning(conditionMessage(e))
        NULL
    })
    if (is.null(sid) || !nzchar(sid)) stop("session could not be established")
    structure(list(base_url = url, sid = sid), class = "sfsclient")
}

