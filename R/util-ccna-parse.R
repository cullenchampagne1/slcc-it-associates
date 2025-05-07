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

library(rvest, quietly = TRUE, warn.conflicts = FALSE) # Filter through and parse html objects
library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(stringr, quietly = TRUE, warn.conflicts = FALSE) # String manipulation
library(xml2, quietly = TRUE, warn.conflicts = FALSE) # XML parsing tools
library(purrr, quietly = TRUE, warn.conflicts = FALSE) # Functional programming tools
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE) # JSON parsing tools

#' Takes in a url to a webpage containing questions and parses them into a structured format
#' sutable for use with an formats created.
#'
#' @param url url to hosted webpage containing questions to be parsed
#'
#' @return A tibble containing the following information:
#'  parsed questions
#'  options
#'  correct answers
#'  planations
#'  metadata
#'
get_formated_questions <- function(url, file_name) {
    # Download the HTML content from the URL (or cache)
    html_content <- download_fromHTML(url)
    # Extract the page title from the HTML content
    exam_title <- html_content %>% rvest::html_element("h1") %>% rvest::html_text(trim = TRUE)
    # Remove unwanted elements from the HTML content
    wpd <- rvest::html_element(html_content, "div#wpdcom")
    if (!is.na(wpd)) xml2::xml_remove(wpd)
    nav <- rvest::html_element(html_content, "div.nav-links")
    if (!is.na(nav)) {
        rvest::html_elements(nav, xpath = "following-sibling::*") |> purrr::walk(xml2::xml_remove)
        xml2::xml_remove(nav)
    }
    # Get all relevant nodes from the HTML content
    nodes <- rvest::html_elements(html_content, xpath = ".//p | .//li | .//h4 | .//img | .//div")
    # Initialize variables to store questions and options
    question_blocks  <- list()
    current_question <- NULL
    option_count <- 0
    in_option <- FALSE
    # Loop through each node to extract questions and options
    for (nd in nodes) {
        # Get the tag and text of the current node
        tag  <- rvest::html_name(nd)
        # Get the text content of the node
        text <- rvest::html_text2(nd)
        # Detect if current node is a question
        if ((tag == "h4" || (tag == "p" && str_detect(text, "^\\s*\\d+\\."))) && nchar(text) > 0 && !str_detect(tolower(text), "match")) {
            # If we were already in a question, save it before starting a new one
            if (!is.null(current_question)) question_blocks <- append(question_blocks, list(current_question))
            # Start a new question block
            current_question <- list(question = text, images = character(),
                  options = character(), correct = integer(),
                  explanation = "")
            # Reset option count and in_option flag
            option_count <- 0
            in_option <- TRUE
            # Check if the current node has images
            imgs <- rvest::html_elements(nd, "img") |> rvest::html_attr("src")
            # If so add them to the current question
            if (length(imgs)) current_question$images <- imgs
            next # Continue to next node
        }
        # If no current question, continue to next node
        if (is.null(current_question)) next

        #' Helper function to determine if an option is valid and meets criteria
        #' set up to filter out invalid options
        keep_option <- function(opt) {
            bad <- c("ccna", "explain:", "answer:", "thanks", "reply", "valid", "explanation:")
            # Accept even very short tokens (e.g. STP, VTP, DTP) as long as they are
            # 2–4 uppercase letters or longer words. Reject only if it matches junk.
            too_short_and_not_acronym <- nchar(opt) < 2 ||
                (nchar(opt) < 3 && !str_detect(opt, "^[A-Z]{2,4}$"))
            if (too_short_and_not_acronym) return(FALSE)
            !any(map_lgl(bad, ~ str_starts(tolower(opt), .x)))
        }
        #' Helper function to check if a node has red style, meaning it is a
        #' correct answer option 
        has_red_style <- function(node) {
            style <- html_attr(node, "style")
            if (is.na(style)) style <- ""        # convert NA to empty string
            style <- tolower(style)
            # direct check on this element
            if (str_detect(style, "color\\s*:\\s*(red|#?ff0000)")) return(TRUE)
            # recurse into children that themselves have a style attribute
            any(
                html_elements(node, xpath = ".//*[@style]") |>
                map_lgl(has_red_style)
            )
        }
        # Check if the current node is an option
        if (in_option && tag %in% c("li", "p") && !str_detect(text, "^\\s*\\d+\\.")) {
            # Check if text is a valid option
            if (keep_option(text)) {
                # Add the option to the current question
                current_question$options <- c(current_question$options, text)
                # Check if the option has red style (indicating it's correct), add to correct answers
                if (has_red_style(nd)) current_question$correct <- c(current_question$correct, length(current_question$options) - 1L)
                # Increment the option count
                option_count <- option_count + 1L
            }
            next # Continue to next node
        }
        # Check if current node is an explination if so add it
        if (str_detect(tolower(text), "explanation")) current_question$explanation <- text
        # If current node is an image, add it to the current question
        if (tag == "img") current_question$images <- unique(c(current_question$images, rvest::html_attr(nd, "src")))
    }
    # If last question is still being built, add it to the list
    if (!is.null(current_question)) question_blocks <- append(question_blocks, list(current_question))
    # Combine the question blocks into a tibble
    questions_final <- dplyr::tibble(
        question = purrr::map_chr(question_blocks, "question"),
         images = purrr::map(question_blocks, "images"),
         options = purrr::map(question_blocks, "options"),
         correct_index = purrr::map(question_blocks, "correct"),
         explanation = purrr::map_chr(question_blocks, "explanation")
    ) %>%
    # Filter out questions with no options or too many options
    dplyr::filter(purrr::map_int(options, length) <= 8) %>%
    dplyr::filter(purrr::map_int(options, length) > 1)
    questions_final$questions <- questions_final
    # Add title to the final questions tibble
    questions_final$title <- exam_title
    # Save the final questions tibble to a JSON file
    cat(paste0("`", url, "`\n"))
    saveRDS(questions_final, file_name)
    questions_final # Return the final questions tibble
}