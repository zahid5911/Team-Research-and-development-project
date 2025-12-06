# ==========================
# Load Libraries
# ==========================
library(tidyverse)

# ==========================
# Load Dataset
# ==========================
# Replace 'data.csv' with the actual filename
df <- read.csv("Protein_Supply_Quantity_Data.csv")

# ==========================
# Select Needed Variables
# ==========================
data_clean <- df %>%
  select(Country, Animal.Products = Animal.Products, Obesity) %>%
  filter(!is.na(Animal.Products), !is.na(Obesity))

# ==========================
# Scatter Plot (Main Plot)
# ==========================
p1 <- ggplot(data_clean, aes(x = Animal.Products, y = Obesity)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship Between Animal Product Consumption and Obesity",
       x = "Animal Products Consumption (kg per capita)",
       y = "Obesity Prevalence (%)") +
  theme_minimal()

print(p1)

# ==========================
# Histogram of Variables
# ==========================
p2 <- ggplot(data_clean, aes(x = Animal.Products)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Animal Products Consumption",
       x = "Animal Products Consumption",
       y = "Frequency") +
  theme_minimal()

print(p2)

p3 <- ggplot(data_clean, aes(x = Obesity)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Obesity Rates",
       x = "Obesity (%)",
       y = "Frequency") +
  theme_minimal()

print(p3)

ggsave("hist_animal_products.pdf", plot = p2, width = 7, height = 5)
ggsave("hist_obesity.pdf", plot = p3, width = 7, height = 5)

ggsave("hist_animal_products.png", plot = p2, width = 7, height = 5)
ggsave("hist_obesity.png", plot = p3, width = 7, height = 5)

# Save scatter plot
ggsave("scatter_plot.pdf", plot = p1, width = 7, height = 5)
ggsave("scatter_plot.png", plot = p1, width = 7, height = 5, dpi = 300)

# ==========================
# Statistical Test
# Pearson Correlation Test
# ==========================
cor_test <- cor.test(data_clean$Animal.Products,
                     data_clean$Obesity,
                     method = "pearson")

print(cor_test)

# ==========================
# Linear Regression Model
# (Optional but recommended)
# ==========================
model <- lm(Obesity ~ Animal.Products, data = data_clean)
summary(model)
