#Author: Tope A. Ibisanmi (tutorial class)

# Function to check if a package is installed
is_installed <- function(package) {
  is.element(package, installed.packages()[,1])
}

# Install necessary packages if not already installed
if (!is_installed("ggplot2")) install.packages("ggplot2")
if (!is_installed("dplyr")) install.packages("dplyr")
#to use waterfalls and ggridges you must have R version ..3.3, to use them, update your R and remove the # in front of the code.
#if (!is_installed("ggridges")) install.packages("ggridges")
#if (!is_installed("waterfalls")) install.packages("waterfalls")


# Load the libraries
library(ggplot2)
library(dplyr)
library(ggridges)
#library(waterfalls)

# Set seed for reproducibility
set.seed(42)

# Generate random data
data <- data.frame(
  Antibiotic = rep(c("Antibiotic A", "Antibiotic B", "Antibiotic C", "Antibiotic D"), each = 100),
  Resistance = sample(c("Resistant", "Non-Resistant"), 400, replace = TRUE),
  Age = sample(18:90, 400, replace = TRUE),
  Count = sample(1:50, 400, replace = TRUE)
)

# Function to add signature
add_signature <- function(p) {
  p +
    annotate("text", x = Inf, y = -Inf, label = "Author: Tope A. Ibisanmi (tutorial class)", hjust = 1.1, vjust = -1.5, size = 3, color = "grey50")
}

# Function to save plots
save_plot <- function(plot, filename) {
  ggsave(filename, plot, path = "Results", width = 8, height = 6)
}

# Create the Results folder if it doesn't exist
if (!dir.exists("Results")) {
  dir.create("Results")
}

# 1. Bar Plot
bar_plot <- ggplot(data, aes(x = Antibiotic, fill = Resistance)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of People Resistant to Different Antibiotics",
       x = "Antibiotic",
       y = "Count") +
  theme_minimal()

bar_plot <- add_signature(bar_plot)
print(bar_plot)
save_plot(bar_plot, "bar_plot.jpeg")

# 2. Box Plot
box_plot <- ggplot(data, aes(x = Antibiotic, y = Age, fill = Resistance)) +
  geom_boxplot() +
  labs(title = "Age Distribution of People Resistant to Different Antibiotics",
       x = "Antibiotic",
       y = "Age") +
  theme_minimal()

box_plot <- add_signature(box_plot)
print(box_plot)
save_plot(box_plot, "box_plot.jpeg")

# 3. Violin Plot
violin_plot <- ggplot(data, aes(x = Antibiotic, y = Age, fill = Resistance)) +
  geom_violin() +
  labs(title = "Violin Plot of Age Distribution by Antibiotic Resistance",
       x = "Antibiotic",
       y = "Age") +
  theme_minimal()

violin_plot <- add_signature(violin_plot)
print(violin_plot)
save_plot(violin_plot, "violin_plot.jpeg")

# 4. Scatter Plot
scatter_plot <- ggplot(data, aes(x = Age, y = Count, color = Resistance)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Resistance Count",
       x = "Age",
       y = "Resistance Count") +
  theme_minimal()

scatter_plot <- add_signature(scatter_plot)
print(scatter_plot)
save_plot(scatter_plot, "scatter_plot.jpeg")

# 5. Faceted Plot
faceted_plot <- ggplot(data, aes(x = Age, y = Count)) +
  geom_point(aes(color = Resistance)) +
  facet_wrap(~ Antibiotic) +
  labs(title = "Faceted Plot of Age vs. Resistance Count by Antibiotic",
       x = "Age",
       y = "Resistance Count") +
  theme_minimal()

faceted_plot <- add_signature(faceted_plot)
print(faceted_plot)
save_plot(faceted_plot, "faceted_plot.jpeg")

# 6. Density Plot
density_plot <- ggplot(data, aes(x = Age, fill = Resistance)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age Distribution by Resistance",
       x = "Age",
       y = "Density") +
  theme_minimal()

density_plot <- add_signature(density_plot)
print(density_plot)
save_plot(density_plot, "density_plot.jpeg")

# 7. Heatmap
heatmap_data <- data %>%
  group_by(Antibiotic, Age) %>%
  summarize(Count = sum(Count))

heatmap_plot <- ggplot(heatmap_data, aes(x = Antibiotic, y = Age, fill = Count)) +
  geom_tile() +
  labs(title = "Heatmap of Antibiotic Resistance by Age",
       x = "Antibiotic",
       y = "Age") +
  theme_minimal()

heatmap_plot <- add_signature(heatmap_plot)
print(heatmap_plot)
save_plot(heatmap_plot, "heatmap_plot.jpeg")

# 8. Histogram
histogram_plot <- ggplot(data, aes(x = Age, fill = Resistance)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  labs(title = "Histogram of Age Distribution by Resistance",
       x = "Age",
       y = "Frequency") +
  theme_minimal()

histogram_plot <- add_signature(histogram_plot)
print(histogram_plot)
save_plot(histogram_plot, "histogram_plot.jpeg")

# 9. Line Plot
line_data <- data %>%
  group_by(Age, Resistance) %>%
  summarize(Total_Count = sum(Count))

line_plot <- ggplot(line_data, aes(x = Age, y = Total_Count, color = Resistance)) +
  geom_line() +
  labs(title = "Line Plot of Resistance Count by Age",
       x = "Age",
       y = "Total Count") +
  theme_minimal()

line_plot <- add_signature(line_plot)
print(line_plot)
save_plot(line_plot, "line_plot.jpeg")

# 10. Area Plot
area_plot <- ggplot(line_data, aes(x = Age, y = Total_Count, fill = Resistance)) +
  geom_area(alpha = 0.6) +
  labs(title = "Area Plot of Resistance Count by Age",
       x = "Age",
       y = "Total Count") +
  theme_minimal()

area_plot <- add_signature(area_plot)
print(area_plot)
save_plot(area_plot, "area_plot.jpeg")

# 11. Pie Chart
pie_data <- data %>%
  group_by(Resistance) %>%
  summarize(Total_Count = n())

pie_chart <- ggplot(pie_data, aes(x = "", y = Total_Count, fill = Resistance)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Resistance Proportions") +
  theme_void()

pie_chart <- add_signature(pie_chart)
print(pie_chart)
save_plot(pie_chart, "pie_chart.jpeg")

# 12. Lollipop Chart
lollipop_chart <- ggplot(pie_data, aes(x = Resistance, y = Total_Count)) +
  geom_point(size = 4) +
  geom_segment(aes(x = Resistance, xend = Resistance, y = 0, yend = Total_Count)) +
  labs(title = "Lollipop Chart of Resistance Counts",
       x = "Resistance",
       y = "Total Count") +
  theme_minimal()

lollipop_chart <- add_signature(lollipop_chart)
print(lollipop_chart)
save_plot(lollipop_chart, "lollipop_chart.jpeg")

# 13. Ridge Plot
ridge_plot <- ggplot(data, aes(x = Age, y = Antibiotic, fill = Resistance)) +
  geom_density_ridges(alpha = 0.8) +
  labs(title = "Ridge Plot of Age Distribution by Antibiotic and Resistance",
       x = "Age",
       y = "Antibiotic") +
  theme_minimal()

ridge_plot <- add_signature(ridge_plot)
print(ridge_plot)
save_plot(ridge_plot, "ridge_plot.jpeg")

# 14. Bubble Plot
bubble_plot <- ggplot(data, aes(x = Age, y = Count, size = Count, color = Resistance)) +
  geom_point(alpha = 0.6) +
  labs(title = "Bubble Plot of Age vs. Count by Resistance",
       x = "Age",
       y = "Count") +
  theme_minimal()

bubble_plot <- add_signature(bubble_plot)
print(bubble_plot)
save_plot(bubble_plot, "bubble_plot.jpeg")

# 15. Dot Plot
dot_plot <- ggplot(data, aes(x = Antibiotic, y = Count, color = Resistance)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center') +
  labs(title = "Dot Plot of Resistance Counts by Antibiotic",
       x = "Antibiotic",
       y = "Count") +
  theme_minimal()

dot_plot <- add_signature(dot_plot)
print(dot_plot)
save_plot(dot_plot, "dot_plot.jpeg")

# 16. Waterfall Chart
#waterfall_data <- data %>%
 # group_by(Antibiotic) %>%
  #summarize(Total_Count = sum(Count))

#waterfall_chart <- waterfall(waterfall_data, values = 'Total_Count', labels = 'Antibiotic') +
 # labs(title = "Waterfall Chart of Resistance Counts by Antibiotic")

#waterfall_chart <- add_signature(waterfall_chart)
#print(waterfall_chart)
#save_plot(waterfall_chart, "waterfall_chart.jpeg")
