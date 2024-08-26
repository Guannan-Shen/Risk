# global variable of all dependencies, pass to function
PKG <- c("tidyverse", "expss", "tinytex", "formatR", "kableExtra")

# dependency management function

pkgLoad <- function(packages = "required") {
  if (length(packages) == 1L && packages == "required") {
    packages <- PKG
  }
  
  packagecheck <- match(packages, utils::installed.packages()[, 1])
  
  packagestoinstall <- packages[is.na(packagecheck)]
  
  if (length(packagestoinstall) > 0L) {
    utils::install.packages(packagestoinstall, dependencies = TRUE)
  } else {
    message("All required packages have been installed.")
  }
  
  for (package in packages) {
    suppressPackageStartupMessages(library(package, character.only = TRUE, quietly = TRUE))
  }
  message("Packages loaded.")
  
}
# at late stage proj/ new working environment, 
# run this file to make sure all packages installed
# tinytex::install_tinytex()
pkgLoad()
