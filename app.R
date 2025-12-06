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

# ===========================
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

