#Example code to determine diet contributions from different size classes to a larger group
#Using Alex 2016 Adarai information

# Step 1: Create length classes from your existing data
lutjanidae_with_diet <- lutjanidae_adarai %>%
  mutate(
    # Create length classes
    length_class = case_when(
      Length < 20 ~ "small (<20cm)",
      Length >= 20 & Length < 30 ~ "medium (20-30cm)",
      Length >= 30 ~ "large (>30cm)"
    )
  )

# Step 2: Get population structure (proportion in each length class)
population_structure <- lutjanidae_with_diet %>%
  group_by(length_class) %>%
  summarize(
    count = n(),
    proportion = n() / nrow(lutjanidae_with_diet)
  )

# Step 3: Assume diet composition by length class (from literature or stomach content analysis)
diet_by_length <- data.frame(
  length_class = c("small (<20cm)", "medium (20-30cm)", "large (>30cm)"),
  invertebrates = c(70, 45, 20),
  small_fish = c(25, 40, 50),
  large_fish = c(5, 15, 30)
)

# Step 4: Join population structure with diet data
diet_with_proportion <- diet_by_length %>%
  left_join(population_structure, by = "length_class")

# Step 5: Calculate weighted diet
diet_long <- diet_with_proportion %>%
  pivot_longer(
    cols = c(invertebrates, small_fish, large_fish),
    names_to = "prey_item",
    values_to = "diet_percent"
  )

# Calculate weighted average
weighted_diet <- diet_long %>%
  group_by(prey_item) %>%
  summarize(
    weighted_percent = sum(diet_percent * proportion),
    .groups = "drop"
  )

print("Population-level diet composition:")
print(weighted_diet)

