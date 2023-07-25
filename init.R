# init.R

#code to install packages
req_pkgs <- c('shiny', 'tidyverse', 'bslib', 'magrittr', 'gridExtra', 'thematic', 'ragg', 'shinyscreenshot')
install_if_missing = function(pg) {
  if (pg %in% rownames(installed.packages()) == FALSE) {
    install.packages(pg)
  }
}
invisible(sapply(req_pkgs, install_if_missing))