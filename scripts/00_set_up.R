################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Install remotes
install.packages("remotes")
# Install startR
remotes::install_github("emlab-ucsb/startR")
# Build directories
startR::create_local_dirs(other_dirs = c("data", "data/raw", "data/processed", "data/output"))
some new code here


## PROCESSING ##################################################################
