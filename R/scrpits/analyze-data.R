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

library(dplyr, quietly = TRUE, warn.conflicts = FALSE) # Mutation / Management of dataframes
library(tidyr, quietly = TRUE, warn.conflicts = FALSE) # 
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE) # 

# Load all answer / question data into program
data <- read.csv("data/raw/canvas-answers.csv", header = TRUE)

my_theme <- theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, margin = margin(b = 2)),
    plot.margin        = unit(c(1,1,1,1), "mm"),
    panel.background   = element_rect(fill = "#f8f8f8", color = NA),
    plot.background    = element_rect(fill = "#f8f8f8", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "#e5e5e5"),
    axis.text          = element_text(color = "black"),
    axis.ticks         = element_blank()
)

data_long <- data %>%
    select(Correct.Answer) %>%
    separate_rows(Correct.Answer, sep = ",") %>%
    mutate(ans_num = as.integer(trimws(Correct.Answer)), ans_letter = LETTERS[ans_num])
counts <- data_long %>% count(ans_letter, name = "Count")

p_a <- ggplot(counts, aes(x = Count, y = reorder(ans_letter, Count))) +
  geom_col(fill = "#333333", width = 0.3) +
  geom_text(aes(label = Count), hjust = -0.1, size = 2.8, color = "black") +
  labs(title = "How Often Each Option Was Chosen", x = "Times Chosen", y = "Option") +
  scale_x_continuous(expand = expansion(c(0,0.1))) +
  my_theme
ggsave("output/frequency.png", p_a, width=12, height=3.5, dpi=300)

qtype <- data %>% count(Question.Type, name="Count")
p_b <- ggplot(qtype, aes(x = Count, y = reorder(Question.Type, Count))) +
  geom_col(fill = "#333333", width = 0.3) +
  geom_text(aes(label = Count), hjust = -0.1, size = 2.8, color = "black") +
  labs(title = "Number of Questions by Type", x = "Count", y = "Question Type") +
  scale_x_continuous(expand = expansion(c(0,0.1))) +
  my_theme
ggsave("output/plot_by_type.png", p_b, width=12, height=3, dpi=300)

qlen <- data %>%
  mutate(n_char = str_length(Question.Text)) %>%
  count(n_char, name = "Count")

p1 <- ggplot(qlen, aes(x = n_char, y = Count)) +
  geom_col(fill = "#333333", width = 0.7) +
  labs(title = "How Many Questions by Length (chars)",
       x     = "Question Length (characters)",
       y     = NULL) +
  scale_x_continuous(expand = expansion(c(0,0.05))) +
  my_theme

ggsave("output/quiz_question_length.png", p1, width=12, height=2, dpi=300)