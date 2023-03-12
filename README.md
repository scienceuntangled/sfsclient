
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfsclient

<!-- badges: start -->

[![R-CMD-check](https://github.com/scienceuntangled/sfsclient/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/scienceuntangled/sfsclient/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

``` r
remotes::install_github("scienceuntangled/sfsclient")
```

## Example usage

``` r
library(sfsclient)

## provide user and pass directly, or via environment variables SFS_USER and SFS_PASS
sfs <- sfs_connect("https://my.sfs-domain.com/webapi", user = "myusername", pass = "mypassword")

## list shared folders
sfs_list_share(sfs)

## list folder contents
sfs_list(sfs, "/some/shared/folder")

## download file or folder
sfs_download(sfs, "/some/path/file.png")
```
