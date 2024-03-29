# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.
# ---------------

# Cache utilities
# Note that much of this code is inspired (cough *taken*) from the lovely bcmaps() function. All credit to the authors and contributors of this package.
# January 15, 2021, Ashlee Jollymore, BC River Forecast Centre

data_dir <- function() {
  if (R.Version()$major >= 4) {
    getOption("bcsnowstats.data_dir", default = tools::R_user_dir("bcsnowstats", "cache"))
  } else {
    getOption("bcsnowstats.data_dir", default = rappdirs::user_cache_dir("bcsnowstats"))
  }
}


show_cached_files <- function() {
  file.path(list.files(data_dir(), full.names = TRUE))
}


check_write_to_data_dir <- function(dir, ask) {

  if (ask) {
    ans <- gtools::ask(paste("bcsnowstats would like to store this layer in the directory:",
                             dir, "Is that okay?", sep = "\n"))
    if (!(ans %in% c("Yes", "YES", "yes", "y"))) stop("Exiting...", call. = FALSE)
  }

  if (!dir.exists(dir)) {
    message("Creating directory to hold bcsnowstats data at ", dir)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  } else {
    message("Saving to bcsnowstats data directory at ", dir)
  }
}


