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
# Quizzzz format saving function
source("R/formats/question-quizizz-format.R")

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(fs, quietly = TRUE, warn.conflicts = FALSE)
library(zip, quietly = TRUE, warn.conflicts = FALSE)

args <- commandArgs(trailingOnly = TRUE)
final_zip_dir <- if (length(args) >= 1) args[1] else "zips"

# Get content from the CCNA exam listings page
group_exam_listings_content <- download_fromHTML("https://itexamanswers.net/ccna-v7", force_refresh = TRUE)
# Get all link elements from the listings page
content_links <- group_exam_listings_content %>% rvest::html_elements("a") %>% rvest::html_attr("href")
# Filter links that contain "modules" indicating they are exam content pages
group_exam_links <- content_links[grepl("modules", content_links, ignore.case = TRUE) &
                                  !grepl("test-online", content_links, ignore.case = TRUE)]
# Standardiz relative URLs to absolute URLs
group_exam_links <- ifelse(grepl("^http", group_exam_links), group_exam_links, paste0("https://itexamanswers.net", group_exam_links))
# Filter out any duplicate links
group_exam_links <- unique(group_exam_links)
# Add hardcoded final exam links
final_exam_links <- c(
    "https://itexamanswers.net/ccna-1-v7-0-final-exam-answers-full-introduction-to-networks.html",
    "https://itexamanswers.net/ccna-2-v7-0-final-exam-answers-full-switching-routing-and-wireless-essentials.html",
    "https://itexamanswers.net/ccna-3-v7-0-final-exam-answers-full-enterprise-networking-security-and-automation.html"
)
group_exam_links <- append(group_exam_links, final_exam_links)

# Create a temporary directory for saving quizizz data
quizizz_data_dir <- tempfile("quizizz_data_dir")
dir_create(quizizz_data_dir)
# Create temporary directories for storing question data
question_data_dir <- tempfile("question_data_dir")
dir_create(question_data_dir)

# Loop through each exam link and parse the questions
for (exam_link in group_exam_links) {
    # Genarte a file name based on the URL
    name_base <- gsub(".html$", "", basename(exam_link))
    semester <- stringr::str_match(name_base, "ccna-(\\d)")[,2]
    modules <- stringr::str_match(name_base, "modules-(\\d+)-(\\d+)")[, 2:3]
    if (any(is.na(modules))) file_name <- paste0("ccna-semester-", semester, "-final")
    else file_name <- paste0("ccna-semester-", semester, "-modules-", modules[1], "-", modules[2])
    # Parse the questions from the exam link
    question_data <- get_formated_questions(exam_link, file.path(question_data_dir, paste0(file_name, "_raw.rds")))
    # Save the formatted questions to a quizzz file
    save_formated_quizzz(question_data$questions, file.path(quizizz_data_dir, paste0(file_name, "_raw.rds")))
}

# Create zip files for the question and quizizz data directories
question_data_zip <- file.path(final_zip_dir, "ccna-semester-1-3_raw.zip")
quizizz_data_zip <- file.path(final_zip_dir, "ccna-semester-1-3_quizizz.zip")
# Zip all files in the question and quizizz data directories
zip::zip(zipfile = question_data_zip, files = dir(question_data_dir, full.names = TRUE), mode = "cherry-pick")
zip::zip(zipfile = quizizz_data_zip, files = dir(quizizz_data_dir, full.names = TRUE), mode = "cherry-pick")
