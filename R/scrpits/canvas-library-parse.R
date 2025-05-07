# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(fs, quietly = TRUE, warn.conflicts = FALSE) # Working with file paths
library(zip, quietly = TRUE, warn.conflicts = FALSE) # Zip file creation
library(purrr, quietly = TRUE, warn.conflicts = FALSE) # Functional programming tools
library(stringr, quietly = TRUE, warn.conflicts = FALSE)

# Core parsing logic for each webpage
source("R/util-canvas-parse.R")
# Format saving functions
source("R/util-data-formats.R")

# Command line argument for final zip directory
args <- commandArgs(trailingOnly = TRUE)
final_zip_dir <- if (length(args) >= 1) args[1] else "zips"

# Get all avalaible html quizes from canvas directory
canvas_quiz_fles <- fs::dir_ls("data/canvas", glob = "*.html")
# Great a table of classes and file names
file_info <- tibble(path = canvas_quiz_fles) %>% dplyr::mutate(
    filename = path_file(path),
    base = path_ext_remove(filename),
    class = str_extract(base, "(?<=\\()[^ )]+")
)
# Catalog of avalaible formats and there functions for each canvas html
formats <- list(
    rds = list(ext = ".rds", save = function(data, path) saveRDS(data, path)),
    quizizz = list(ext = ".xlsx", save = save_formated_quizizz),
    quizlet = list(ext = ".txt", save = save_formated_quizlet),
    kahoot = list(ext = ".xlsx", save = save_formatted_kahoot),
    googleforms = list(ext = ".xlsx", save = save_formatted_googleforms),
    csv = list(ext = ".csv", save = save_formatted_csv)
)
# Group files by class and loop through each
file_info %>%
    dplyr::group_by(class) %>%
    # Walk through each class and parse files
    dplyr::group_walk(~{
        # Create a class directory in output directory
        class_dir <- file.path(final_zip_dir, .y$class)
        fs::dir_create(class_dir, recurse = TRUE)
        # Build a tmp directory for format
        tmp_dir <- file.path(tempdir(), paste0("tmp_", .y$class))
        fs::dir_create(tmp_dir)
        # On exit delete the tmp directory
        on.exit(fs::dir_delete(tmp_dir), add = TRUE)
        # Get formated questions for each link in class
        for (i in seq_len(nrow(.x))) {
            quiz_tbl <- get_formated_questions(.x$path[i], NULL)
            # Map each format to tmp directory
            for (fmt in names(formats)) {
                info  <- formats[[fmt]]
                outfp <- file.path(tmp_dir, paste0(.x$base[i], info$ext))
                info$save(quiz_tbl$questions, outfp)
            }
        }
        # Create zip file for each format in class directory
        for (fmt in names(formats)) {
            # Get a list of file per format and drop them in class folder
            files <- fs::dir_ls(tmp_dir, glob = paste0("*", formats[[fmt]]$ext))
            zipfile <- file.path(class_dir, paste0(.y$class, "_", fmt, ".zip"))
            # Zip files if they exist
            if (length(files) > 0) zip::zip(zipfile = zipfile, files = files, mode = "cherry-pick")
        }
  })