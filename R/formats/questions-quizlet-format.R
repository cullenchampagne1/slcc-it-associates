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

#' Takes in a list of questions and formats them into a structure suitable for Quizlet import.
#'
#' @param questions object containing question data
#' @param file_name name of the file to save the data to
#'
#' @return A tibble containing the following information:
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