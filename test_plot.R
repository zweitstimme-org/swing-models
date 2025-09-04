# Test plot generation
library(ggplot2)
library(dplyr)

# Load the evaluation results
eval_results <- read.csv("data/out/eval_results.csv")
eval_aggregate <- read.csv("data/out/eval_aggregate.csv")

# Create plot with individual election results and overall average
# First, create a factor with proper ordering based on average accuracy
model_order <- eval_aggregate$name[order(eval_aggregate$avg_accuracy_winner, decreasing = TRUE)]
eval_results$name_ordered <- factor(eval_results$name, levels = model_order)
eval_aggregate$name_ordered <- factor(eval_aggregate$name, levels = model_order)

p <- ggplot() +
  # Individual election results as dots with labels
  geom_point(data = eval_results, aes(x = name_ordered, y = accuracy_winner, group = name), 
             position = position_jitter(width = 0.2, seed = 123), 
             alpha = 0.6, size = 2, color = "steelblue") +
  # Add text labels for individual elections
  geom_text(data = eval_results, aes(x = name_ordered, y = accuracy_winner, label = election), 
            position = position_jitter(width = 0.2, seed = 123), 
            size = 2.5, hjust = -0.3, vjust = 0.5, color = "darkblue") +
  # Overall average as a different symbol (diamond)
  geom_point(data = eval_aggregate, aes(x = name_ordered, y = avg_accuracy_winner), 
             shape = 23, size = 4, fill = "red", color = "darkred") +
  # Add average line
  geom_hline(data = eval_aggregate, aes(yintercept = avg_accuracy_winner, group = name_ordered), 
             linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Out-of-sample Performance of Models",
       subtitle = "Individual elections (dots) and overall average (red diamonds)",
       x = "Model",
       y = "Accuracy (Winner Prediction)") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "none")

print(p)

# Save the plot to pdf
ggsave("data/out/accuracy_new.pdf", p, width = 10, height = 8)
print("Plot saved as data/out/accuracy_new.pdf")

