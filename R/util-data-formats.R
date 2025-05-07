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
library(purrr, quietly = TRUE, warn.conflicts = FALSE) # Functional programming tools
library(writexl, quietly = TRUE, warn.conflicts = FALSE) # Excel file writing

#' Takes in a list of questions and formats them into a structure suitable for Quizlet import.
#'
#' @param questions A tibble or data frame with list-columns:
#'   - question  (character)
#'   - images    (list, ignored here)
#'   - options   (list of character vectors)
#'   - correct_index (numeric or list of indices)
#'   - explanation (character, ignored here)
#' @param file_name Path to the CSV file to write
#'
save_formated_quizlet <- function(questions, file_name) {
    quizlet_rows <- pmap_chr(questions, function(question, images, options, correct_index, explanation) {
        # Convert to 1 base indices if avalaible 
        correct_indices <- if (is.null(correct_index)) integer(0)
        else if (is.numeric(correct_index)) correct_index + 1
        else as.integer(unlist(correct_index)) + 1
        # Get correct answers by index(s)
        correct_answers <- options[correct_indices]
        answer_str <- paste(correct_answers, collapse = ", ")
        # Construct card line: question | answer(s)
        paste0(question, "|", answer_str)
    })
    # Write all lines to the file provided
    writeLines(as.character(quizlet_rows), con = file_name)
}

#' Save questions and their correct answers as a two-column CSV
#'
#' @param questions A tibble or data frame with list-columns:
#'   - question  (character)
#'   - images    (list, ignored here)
#'   - options   (list of character vectors)
#'   - correct_index (numeric or list of indices)
#'   - explanation (character, ignored here)
#' @param file_name Path to the CSV file to write
#'
save_formatted_csv <- function(questions, file_name) {
  # Build a two-column tibble
  questions_table <- purrr::pmap_dfr(questions, function(question, images, options, correct_index, explanation) {
    # Normalize correct_index to 1-based integer vector
    correct_inds <- if (is.null(correct_index)) integer(0)
    else if (is.numeric(correct_index)) correct_index + 1
    else as.integer(unlist(correct_index)) + 1
    # Extract correct answers and collapse with ", "
    correct_answers <- options[correct_inds]
    answer_str <- paste(correct_answers, collapse = ", ")
    # Return a one-row tibble
    tibble::tibble(question = question, answer = answer_str)
  })
  # Write out as CSV (no row names)
  write.csv(questions_table, file_name, row.names = FALSE)
}


#' Takes in a list of questions and formats them into a structure suitable for Quizizz import.
#'
#' @param questions A tibble or data frame with list-columns:
#'   - question  (character)
#'   - images    (list, ignored here)
#'   - options   (list of character vectors)
#'   - correct_index (numeric or list of indices)
#'   - explanation (character, ignored here)
#' @param file_name Path to the CSV file to write
#'
save_formated_quizizz <- function(questions, file_name) {
    quizizz_rows <- pmap_dfr(questions, function(question, images, options, correct_index, explanation) {
        # Parse correct indices and shift to 1-based
        correct_indices <- if (is.null(correct_index)) integer(0)
        else if (is.numeric(correct_index)) correct_index + 1
        else  as.integer(unlist(correct_index)) + 1
        # Determine question type
        question_type <- if (length(correct_indices) == 1) "Multiple Choice" else "Checkbox"
        # Build list of correct options
        correct_opts <- options[correct_indices]
        
        all_included <- unique(c(correct_indices, seq_along(options)))
        fill_indices <- setdiff(seq_along(options), correct_indices)
        filler_opts <- options[fill_indices]

        final_opts <- c(correct_opts, filler_opts)
        final_opts <- unique(final_opts)[1:min(length(unique(final_opts)), 5)]

        # Map back updated correct indices in this reduced set
        new_correct_indices <- which(final_opts %in% correct_opts)
        correct_answer_str <- paste(new_correct_indices, collapse = ",")

        padded_opts <- c(final_opts, rep("", 5))[1:5]
        # Use first image if present
        image_link <- if (length(images) > 0 && nzchar(images[[1]])) images[[1]] else ""

        tibble(
            `Question Text` = question,
            `Question Type` = question_type,
            `Option 1` = padded_opts[1],
            `Option 2` = padded_opts[2],
            `Option 3` = padded_opts[3],
            `Option 4` = padded_opts[4],
            `Option 5` = padded_opts[5],
            `Correct Answer` = correct_answer_str,
            `Time in seconds` = 30,
            `Image Link` = image_link,
            `Answer explanation` = explanation %||% "",
        )
    })
    # Save the formatted quizzz data to a CSV file
    writexl::write_xlsx(quizizz_rows, file_name)
    # Return the formatted quizizz rows
    quizizz_rows
}