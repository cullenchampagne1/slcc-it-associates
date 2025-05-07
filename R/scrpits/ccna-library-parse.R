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

# Utilties for downloading data from cache
source("R/util-data-download.R")
# Core parsing logic for each webpage
source("R/util-ccna-parse.R")
# Format saving functions
source("R/util-data-formats.R")

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(fs, quietly = TRUE, warn.conflicts = FALSE) # Working with file paths
library(zip, quietly = TRUE, warn.conflicts = FALSE) # Zip file creation
library(purrr, quietly = TRUE, warn.conflicts = FALSE) # Functional programming tools

# Command line argument for final zip directory
args <- commandArgs(trailingOnly = TRUE)
final_zip_dir <- if (length(args) >= 1) args[1] else "zips"

# Get content from the CCNA exam listings page
group_exam_listings_content <- download_fromHTML("https://itexamanswers.net/ccna-v7", force_refresh = TRUE)
# Get all link elements from the listings page
content_links <- group_exam_listings_content %>% rvest::html_elements("a") %>% rvest::html_attr("href")
# Filter links that contain "modules" indicating they are exam content pages
group_exam_links <- content_links[
    (grepl("modules", content_links, ignore.case = TRUE) |
    grepl("final-exam", content_links, ignore.case = TRUE)) &
    !grepl("test-online", content_links, ignore.case = TRUE)
]
# Standardiz relative URLs to absolute URLs
group_exam_links <- ifelse(grepl("^http", group_exam_links), group_exam_links, paste0("https://itexamanswers.net", group_exam_links))
# Filter out any duplicate links
group_exam_links <- unique(group_exam_links)
# remove all spaces from each URL
group_exam_links <- gsub("\\s+", "", group_exam_links)

# Define the formats and their save functions
formats <- list(
    rds = list(ext = ".rds", save = function(data, path) saveRDS(data, path)),
    quizizz = list(ext = ".xlsx", save = save_formated_quizizz),
    quizlet = list(ext = ".txt", save = save_formated_quizlet),
    kahoot = list(ext = ".xlsx", save = save_formatted_kahoot),
    googleforms = list(ext = ".xlsx", save = save_formatted_googleforms),
    csv = list(ext = ".csv", save = save_formatted_csv)
)
# Create temporary directories for each format
temp_dirs <- purrr::map(names(formats), function(fmt) {
    # Create a temporary directory for the format
    dir <- tempfile(paste0(fmt, "_data_dir_"))
    fs::dir_create(dir)
    dir # Return the directory path
})
# Swap names to match the formats for later use
names(temp_dirs) <- names(formats)

# Loop through each exam link and parse the questions
for (exam_link in group_exam_links) {
    # Genarte a file name based on the URL
    name_base <- sub("\\.html$", "", basename(exam_link))
    # extract the semester number (the “1” in “ccna-1”)
    semester <- str_match(name_base, "^ccna-(\\d)")[,2]
    if (str_detect(name_base, regex("modules-\\d+-\\d+", ignore_case = TRUE))) {
        # it’s a modules page → pull out the two module numbers
        modules <- str_match(name_base, "modules-(\\d+)-(\\d+)")[,2:3]
        file_name <- sprintf("ccna-semester-%s-modules-%s-%s",
                            semester, modules[1], modules[2])
    } else {
        # everything else is a “final” of some sort (including practice finals)
        # strip off the leading ccna-<n>-vX.Y- so we can append the rest
        suffix <- sub("^ccna-\\d+-v[\\d\\.]+-", "",
                        name_base, ignore.case = TRUE)
        file_name <- sprintf("ccna-semester-%s-%s", semester, suffix)
    }
    # Parse the questions from the exam link
    question_data <- get_formated_questions(exam_link, NULL)
    # Apply the save functions to each format listed
    purrr::walk2(formats, temp_dirs, ~ {
        out_path <- file.path(.y, paste0(file_name, .x$ext))
        .x$save(question_data$questions, out_path)
    })
}

# Create the zip conatiners for all formats
invisible(purrr::imap(temp_dirs, ~ {
    # Generate the zip file path
    zip_path <- file.path(final_zip_dir, paste0("ccna-semester-1-3-", .y, ".zip"))
    # Get all files in fortmats diretory
    files <- dir(.x, full.names = TRUE)
    # Zip contents of the directory
    zip(zipfile = zip_path, files = files, mode = "cherry-pick")
}))
