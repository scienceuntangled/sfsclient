% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list.R
\name{sfs_list_share}
\alias{sfs_list_share}
\title{List shared folders}
\usage{
sfs_list_share(sfs, additional = c(), verbose = sfs_verbose())
}
\arguments{
\item{sfs}{sfsclient: as returned by [sfs_connect()]}

\item{additional}{character: one or more of "real_path", "owner", "time", "perm", "mount_point_type", "sync_share", "volume_status"}

\item{verbose}{logical: give debug info?}
}
\value{
A data.frame or `NULL`
}
\description{
List shared folders
}
\references{
<https://global.download.synology.com/download/Document/Software/DeveloperGuide/Package/FileStation/All/enu/Synology_File_Station_API_Guide.pdf>
}
