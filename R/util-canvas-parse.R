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
library(stringr, quietly = TRUE, warn.conflicts = FALSE) # String manipulation
library(xml2, quietly = TRUE, warn.conflicts = FALSE) # XML parsing tools
library(purrr, quietly = TRUE, warn.conflicts = FALSE) # Functional programming tools
library(jsonlite, quietly = TRUE, warn.conflicts = FALSE) # JSON parsing tools

#' Parse a Canvas quiz HTML and return questions in the CCNA-parser format.
#'
#' @param path local file path to the exported Canvas quiz page.
#' @param file_name Optional RDS filename to save the output.
#'
#' @return A tibble with columns: question, images, options, correct_index, explanation, title.
get_formated_questions <- function(path, file_name = NULL) {
    # Load the HTML content from the local file
    html_content <- xml2::read_html(path)
    # Extact question blocks from the HTML content
    question_blocks <- html_content %>% rvest::html_elements(".quiz_sortable.question_holder")
    # Extract the title from the HTML content
    title <- html_content %>% rvest::html_element("title") %>% rvest::html_text2()
    # Map question blocks to extract question data
    qustion_list <- purrr::map(question_blocks, function(q) {
        # Extract and format question text
        question_node <- q %>% rvest::html_element(".question_text.user_content")
        question_text <- question_node %>% rvest::html_text2()
        # Extract all option nodes and correct indeces
        answer_wrappers <- q %>% html_elements(".answers_wrapper > div.answer")
        options <- purrr::map_chr(answer_wrappers, ~ .x %>% rvest::html_element(".select_answer.answer_type .answer_html") %>% rvest::html_text2())
        correct_index <- which(str_detect(html_attr(answer_wrappers, "class"), "correct_answer")) - 1L
        # Check for explination or feedback nodes
        expl_node <- q %>% rvest::html_element(".quiz_comment .answer_comment")
        explanation <- if (!is.na(expl_node)) expl_node %>% rvest::html_text2() else ""
        explanation <- str_remove(explanation, "^(Correct\\.|Incorrect\\.)\\s*")
        # Return a list with the extracted data
        list(
            question = question_text,
            images = "",
            options = options,
            correct_index = correct_index,
            explanation = explanation
        )
    })
    # Convert all list to formatted tibble
    formated_questions <- tibble(
        question = map_chr(qustion_list, "question"),
        images = map(qustion_list, "images"),
        options = map(qustion_list, "options"),
        correct_index = map(qustion_list, "correct_index"),
        explanation = map_chr(qustion_list, "explanation")
    )
    # Drop any questions that do not have multiple options
    formated_questions <- formated_questions %>% filter(lengths(options) > 1)
    cat(paste0("- ", title, "`", nrow(formated_questions), "q`\n"))
    # Add title and questions to the result
    formated_questions <- list(
        questions = formated_questions,
        title = title
    )
    # Save to file if avalaible
    if (!is.null(file_name)) saveRDS(formated_questions, file_name)
    formated_questions # Return the final questions tibble
}