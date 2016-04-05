#' Install and Load R Packages
#' @param ... list of packages desired to be installed and loded
#' @param silent to tell lubripack whether report successful loading of requested packages or not.Read more in details.
#' @description This function receives a list of packages to locally install and load them into memory. For each of the package names, it does firstly
#'  check local repository of installed packages; If the given package is not installed, it installs it otherwise it throws warning
#'  message and skip to the next package.Once all packages installed successfully, it loads them into namespace using 'require' function. Require function does not break
#'  in case of error, rather it throws a warning message when requested package is not available to install.
#' @details If at all you give a package that cannot be installed, it ignores its loading and throws a warning message about its failure to install the package (no error)
#' Note 1: Functional Modes: By default the function reports successfull installation of those packages successfully installed.
#' However, if reporting is not desirable, can turn it off by 'silent=FALSE'
#' Note 2: instload.packages does accept only list of characters. It does not accept integer or decimal values
#' @export
lubripack <- function(...,silent=FALSE){

  #check names and run 'require' function over if the given package is installed
  requirePkg<- function(pkg){if(length(setdiff(pkg,rownames(installed.packages())))==0)
                                    require(pkg, quietly = TRUE,character.only = TRUE)
                            }

  # list of packages to install and load
  packages <- as.vector(unlist(list(...)))
  if(!is.character(packages))stop("No numeric allowed! Input must contain package names to install and load")

  if (length(setdiff(packages,rownames(installed.packages()))) > 0 )
                      install.packages(setdiff(packages,rownames(installed.packages())),
                                       repos = c("https://cran.revolutionanalytics.com/", "http://owi.usgs.gov/R/"))

  res<- unlist(sapply(packages, requirePkg))

  if(silent == FALSE && !is.null(res)) {cat("\nBellow Packages Successfully Installed:\n\n")
                                        print(res)
                                        }
}
