# Generate LaTeX table from evaluation results
library(xtable)
library(dplyr)

# Load the evaluation results
eval_aggregate <- read.csv("data/out/eval_aggregate.csv")

# Create LaTeX table
latex_table <- eval_aggregate %>% 
  select(name, avg_accuracy_winner, min_accuracy_winner, max_accuracy_winner) %>% 
  arrange(desc(avg_accuracy_winner)) %>%
  # Create LaTeX table with custom caption and formatting
  xtable(caption = "Average, Maximum and Minimum Accuracy of Candidate Vote Share Prediction Models for the Elections 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025.",
         label = "tab:eval_aggregate",
         digits = c(0, 0, 2, 2, 2))

# Save to .tex file
print(latex_table, 
      include.rownames = FALSE,
      caption.placement = "top",
      table.placement = "H",
      comment = FALSE,
      sanitize.text.function = identity,
      add.to.row = list(pos = list(-1), command = "\\hline \\hline \n"),
      floating = TRUE,
      size = "\\footnotesize",
      file = "tables/eval_aggregate_table.tex")

print("LaTeX table saved as tables/eval_aggregate_table.tex")
