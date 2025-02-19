library(dplyr)
library(readr)
library(ggplot2)

#load the datasets
hcv_data <- read_csv("HCV-Egy-Data.csv")

#change the column names
colnames(hcv_data) <- gsub(" ", "_", colnames(hcv_data))
print(colnames(hcv_data))

str(hcv_data)

#factorizes the categorical variables 
categorical_cols <- c("Gender", "Fever", "Nausea/Vomting", "Headache", "Diarrhea", 
                      "Fatigue_&_generalized_bone_ache", "Jaundice", "Epigastric_pain", 
                      "Baseline_histological_Grading", "Baselinehistological_staging")

#factorize the categorical variables 
hcv_data[categorical_cols] <- lapply(hcv_data[categorical_cols], as.factor)

str(hcv_data)
summary(hcv_data)

#gender distribution 

#convert Gender 1 -> "Male" and 2 -> "Female"
hcv_data <- hcv_data %>%
  mutate(Gender = recode(Gender, `1` = "Male", `2` = "Female"))

# Define age bins
hcv_data <- hcv_data %>%
  mutate(Age_Group = cut(Age, 
                         breaks = c(0, 32, 37, 42, 47, 52, 57, 62), 
                         labels = c("0-32", "32-37", "37-42", "42-47", "47-52", "52-57", "57-62"),
                         include.lowest = TRUE))

#calculate distribution within each age group
age_distribution <- hcv_data %>%
  group_by(Age_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(age_distribution)

# Plot histogram with the defined bins
ggplot(hcv_data, aes(x = Age_Group)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution by Defined Groups",
       x = "Age Group",
       y = "Count") +
  theme_minimal()

#BASED ON GENDER 
#calculate gender distribution within each age group
gender_age_distribution <- hcv_data %>%
  group_by(Age_Group, Gender) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

#print results
print(gender_age_distribution)

#plot gender distribution across age groups
ggplot(gender_age_distribution, aes(x = Age_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  labs(title = "Gender Distribution Across Age Groups", 
       x = "Age Group", 
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))



#BMI distribution
# Define BMI bins
hcv_data <- hcv_data %>%
  mutate(BMI_Group = cut(BMI, 
                         breaks = c(0, 18.5, 25, 30, 35, 40), 
                         labels = c("0-18.5", "18.5-25", "25-30", "30-35", "35-40"),
                         include.lowest = TRUE))

# Count the number of patients in each BMI group
bmi_distribution <- hcv_data %>%
  group_by(BMI_Group) %>%
  summarise(Count = n())

print(bmi_distribution)

# Plot BMI distribution
ggplot(bmi_distribution, aes(x = BMI_Group, y = Count, fill = BMI_Group)) +
  geom_bar(stat = "identity", color = "black") +  
  labs(title = "BMI Distribution of HCV Patients",
       x = "BMI Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

#GENDER

# Calculate gender distribution within BMI groups
bmi_gender_distribution <- hcv_data %>%
  group_by(BMI_Group, Gender) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(bmi_gender_distribution)

# Plot BMI groups divided by gender
ggplot(bmi_gender_distribution, aes(x = BMI_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  labs(title = "Gender Distribution Across BMI Groups", 
       x = "BMI Group", 
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))

#SYMPTOMS
# Define symptom columns
symptom_cols <- c("Fever", "Nausea/Vomting", "Headache", "Diarrhea", 
                  "Fatigue_&_generalized_bone_ache", "Jaundice", "Epigastric_pain")

# Recode 1 → "Absent", 2 → "Present"
hcv_data <- hcv_data %>%
  mutate(across(all_of(symptom_cols), ~ recode(., `1` = "Absent", `2` = "Present")))

# Check if changes applied
summary(hcv_data[symptom_cols])

# Function to plot each symptom's distribution
plot_symptom <- function(symptom) {
  symptom_data <- hcv_data %>%
    group_by(.data[[symptom]]) %>%
    summarise(Count = n())
  
  ggplot(symptom_data, aes(x = .data[[symptom]], y = Count, fill = .data[[symptom]])) +
    geom_bar(stat = "identity", color = "black") +  
    labs(title = paste("Distribution of", symptom),
         x = symptom, 
         y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("Absent" = "darkgreen", "Present" = "red"))  
}

# Generate plots for all symptoms
symptom_plots <- lapply(symptom_cols, plot_symptom)

# Print all plots
print(symptom_plots)

#GENDER BREAK DOWN 
# Function to plot each symptom's distribution broken down by gender
plot_symptom_by_gender <- function(symptom) {
  symptom_data <- hcv_data %>%
    group_by(Gender, .data[[symptom]]) %>%
    summarise(Count = n(), .groups = "drop")
  
  ggplot(symptom_data, aes(x = .data[[symptom]], y = Count, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +  
    labs(title = paste("Distribution of", symptom, "by Gender"),
         x = symptom, 
         y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  
}

# Generate plots for all symptoms by gender
symptom_gender_plots <- lapply(symptom_cols, plot_symptom_by_gender)

# Print all plots
print(symptom_gender_plots)

#WBC
#ALL
# Ensure WBC is numeric
hcv_data <- hcv_data %>%
  mutate(WBC = as.numeric(WBC))

# Define WBC bins
hcv_data <- hcv_data %>%
  mutate(WBC_Group = cut(WBC, 
                         breaks = c(0, 4000, 11000, 12101),  
                         labels = c("0-4000", "4000-11000", "11000-12101"),
                         include.lowest = TRUE))

# Count the number of patients in each WBC group
wbc_distribution <- hcv_data %>%
  group_by(WBC_Group) %>%
  summarise(Count = n(), .groups = "drop")

wbc_distribution

# Plot general WBC distribution
ggplot(wbc_distribution, aes(x = WBC_Group, y = Count, fill = WBC_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "WBC Distribution of HCV Patients",
       x = "WBC Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")  

#BY GENDER
# Count the number of patients in each WBC group by gender
wbc_gender_distribution <- hcv_data %>%
  group_by(WBC_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

wbc_gender_distribution

# Plot WBC distribution broken down by gender
ggplot(wbc_gender_distribution, aes(x = WBC_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "WBC Distribution by Gender",
       x = "WBC Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) 

#RBC
#ALL
# Ensure RBC is numeric
hcv_data <- hcv_data %>%
  mutate(RBC = as.numeric(RBC))

# Define RBC bins
hcv_data <- hcv_data %>%
  mutate(RBC_Group = cut(RBC, 
                         breaks = c(0, 3000000, 5000000, 5018451),  
                         labels = c("0-3M", "3M-5M", "5M-5.018M"),
                         include.lowest = TRUE))

# Count the number of patients in each RBC group
rbc_distribution <- hcv_data %>%
  group_by(RBC_Group) %>%
  summarise(Count = n(), .groups = "drop")

rbc_distribution

# Plot general RBC distribution
ggplot(rbc_distribution, aes(x = RBC_Group, y = Count, fill = RBC_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "RBC Distribution of HCV Patients",
       x = "RBC Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")  

#BY GENDER
# Count the number of patients in each RBC group by gender
rbc_gender_distribution <- hcv_data %>%
  group_by(RBC_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

# Plot RBC distribution broken down by gender
ggplot(rbc_gender_distribution, aes(x = RBC_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RBC Distribution by Gender",
       x = "RBC Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) 

#Platelets
#ALL
# Ensure Platelet (Plat) is numeric
hcv_data <- hcv_data %>%
  mutate(Plat = as.numeric(Plat))

# Define Platelet bins
hcv_data <- hcv_data %>%
  mutate(Plat_Group = cut(Plat, 
                          breaks = c(93013, 100000, 255000, 226465),  
                          labels = c("93K-100K", "100K-255K", "255K-226K"),
                          include.lowest = TRUE))

# Count the number of patients in each Platelet group
plat_distribution <- hcv_data %>%
  group_by(Plat_Group) %>%
  summarise(Count = n(), .groups = "drop")

plat_distribution

# Plot general Platelet distribution
ggplot(plat_distribution, aes(x = Plat_Group, y = Count, fill = Plat_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Platelet (Plat) Distribution of HCV Patients",
       x = "Platelet Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")  

#BY GENDER
# Count the number of patients in each Platelet group by gender
plat_gender_distribution <- hcv_data %>%
  group_by(Plat_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

plat_gender_distribution

# Plot Platelet distribution broken down by gender
ggplot(plat_gender_distribution, aes(x = Plat_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Platelet (Plat) Distribution by Gender",
       x = "Platelet Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) 

#HEMOGLOBIN
#ALL
# Define gender-specific bins for Hemoglobin (HGB)
hcv_data <- hcv_data %>%
  mutate(HGB_Group = case_when(
    Gender == "Male" & HGB >= 2 & HGB < 14 ~ "2-14",
    Gender == "Male" & HGB >= 14 & HGB <= 17.5 ~ "14-17.5",
    Gender == "Male" & HGB > 17.5 & HGB <= 20 ~ "17.5-20",
    Gender == "Female" & HGB >= 2 & HGB < 12.3 ~ "2-12.3",
    Gender == "Female" & HGB >= 12.3 & HGB <= 15.3 ~ "12.3-15.3",
    Gender == "Female" & HGB > 15.3 & HGB <= 20 ~ "15.3-20",
    TRUE ~ NA_character_  # Assigns NA if values are out of range
  ))

# Count the number of patients in each gender-specific Hemoglobin group
hgb_distribution <- hcv_data %>%
  group_by(HGB_Group) %>%
  summarise(Count = n(), .groups = "drop")

hgb_distribution

# Plot general Hemoglobin (HGB) distribution with gender-specific bins
ggplot(hgb_distribution, aes(x = HGB_Group, y = Count, fill = HGB_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Hemoglobin (HGB) Distribution of HCV Patients (Gender-Specific Bins)",
       x = "Hemoglobin Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

#BY GENDER
# Count the number of patients in each Hemoglobin group by gender
hgb_gender_distribution <- hcv_data %>%
  group_by(HGB_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

hgb_gender_distribution

# Plot Hemoglobin (HGB) distribution broken down by gender with gender-specific bins
ggplot(hgb_gender_distribution, aes(x = HGB_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Hemoglobin (HGB) Distribution by Gender (Gender-Specific Bins)",
       x = "Hemoglobin Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) 

#AST_1
#ALL
# Ensure AST_1 is numeric
hcv_data <- hcv_data %>%
  mutate(AST_1 = as.numeric(AST_1))

# Define AST_1 bins
hcv_data <- hcv_data %>%
  mutate(AST1_Group = cut(AST_1, 
                          breaks = c(0, 20, 40, 128),  
                          labels = c("0-20", "20-40", "40-128"),
                          include.lowest = TRUE))

# Count the number of patients in each AST_1 group
ast1_distribution <- hcv_data %>%
  group_by(AST1_Group) %>%
  summarise(Count = n(), .groups = "drop")

ast1_distribution

# Plot general AST1 distribution
ggplot(ast1_distribution, aes(x = AST1_Group, y = Count, fill = AST1_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "AST1 (1 Week) Distribution of HCV Patients",
       x = "AST1 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

#BY GENDER
# Count the number of patients in each AST1 group by gender
ast1_gender_distribution <- hcv_data %>%
  group_by(AST1_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

# Plot AST1 distribution broken down by gender
ggplot(ast1_gender_distribution, aes(x = AST1_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "AST1 (1 Week) Distribution by Gender",
       x = "AST1 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  

#ALT
# Ensure ALT variables are numeric
alt_vars <- c("ALT_1", "ALT4", "ALT_12", "ALT_24", "ALT_36", "ALT_48", "ALT_after_24_w")
hcv_data[alt_vars] <- lapply(hcv_data[alt_vars], as.numeric)

# Convert Gender 1 → "Male", 2 → "Female"
hcv_data <- hcv_data %>%
  mutate(Gender = recode(Gender, `1` = "Male", `2` = "Female"))

# Function to categorize ALT values into bins
categorize_alt <- function(df, alt_var) {
  df %>%
    mutate(!!paste0(alt_var, "_Group") := cut(.data[[alt_var]], 
                                              breaks = c(0, 20, 40, 128),  
                                              labels = c("0-20", "20-40", "40-128"),
                                              include.lowest = TRUE))
}

# Apply categorization to each ALT variable
for (alt in alt_vars) {
  hcv_data <- categorize_alt(hcv_data, alt)
}

# Function to plot general ALT distribution
plot_alt_general <- function(alt_var) {
  alt_distribution <- hcv_data %>%
    group_by(.data[[paste0(alt_var, "_Group")]]) %>%
    summarise(Count = n(), .groups = "drop")
  
  ggplot(alt_distribution, aes(x = .data[[paste0(alt_var, "_Group")]], y = Count, fill = .data[[paste0(alt_var, "_Group")]])) +
    geom_bar(stat = "identity", color = "black") +
    labs(title = paste(alt_var, "Distribution of HCV Patients"),
         x = "ALT Group",
         y = "Count") +
    theme_minimal() +
    scale_fill_brewer(palette = "Blues") 
}

# Function to plot ALT distribution by gender
plot_alt_gender <- function(alt_var) {
  alt_gender_distribution <- hcv_data %>%
    group_by(.data[[paste0(alt_var, "_Group")]], Gender) %>%
    summarise(Count = n(), .groups = "drop")
  
  ggplot(alt_gender_distribution, aes(x = .data[[paste0(alt_var, "_Group")]], y = Count, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = paste(alt_var, "Distribution by Gender"),
         x = "ALT Group",
         y = "Count") +
    theme_minimal() +
    scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  # Custom colors for gender
}

# Generate and print plots for all ALT variables
alt_general_plots <- lapply(alt_vars, plot_alt_general)
alt_gender_plots <- lapply(alt_vars, plot_alt_gender)

print(alt_general_plots)
print(alt_gender_plots)

#RNA BASE
#ALL
# Define RNA_Base bins
hcv_data <- hcv_data %>%
  mutate(RNA_Base_Group = cut(RNA_Base, 
                              breaks = c(0, 5, 1201086),  
                              labels = c("0-5", ">5"),
                              include.lowest = TRUE))

# Count the number of patients in each RNA_Base group
rna_base_distribution <- hcv_data %>%
  group_by(RNA_Base_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA_Base Distribution")
print(rna_base_distribution)

# Plot general RNA_Base distribution
ggplot(rna_base_distribution, aes(x = RNA_Base_Group, y = Count, fill = RNA_Base_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "RNA_Base Distribution of HCV Patients",
       x = "RNA_Base Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

#BY GENDER
# Count the number of patients in each RNA_Base group by gender
rna_base_gender_distribution <- hcv_data %>%
  group_by(RNA_Base_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA_Base Distribution by Gender")
print(rna_base_gender_distribution)

# Plot RNA_Base distribution broken down by gender
ggplot(rna_base_gender_distribution, aes(x = RNA_Base_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RNA_Base Distribution by Gender",
       x = "RNA_Base Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "pink"))

#RNA 4
#ALL
# Ensure RNA 4 is numeric
hcv_data <- hcv_data %>%
  mutate(RNA_4 = as.numeric(RNA_4))

# Convert Gender 1 → "Male", 2 → "Female"
hcv_data <- hcv_data %>%
  mutate(Gender = recode(Gender, `1` = "Male", `2` = "Female"))

# Categorize RNA 4 into bins
hcv_data <- hcv_data %>%
  mutate(RNA_4_Group = cut(RNA_4, 
                           breaks = c(0, 5, 1201715),  
                           labels = c("0-5", "5-1201715"),
                           include.lowest = TRUE))

# Print general RNA 4 distribution table
rna_4_distribution <- hcv_data %>%
  group_by(RNA_4_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA 4")
print(rna_4_distribution)

# Plot general RNA 4 distribution
rna_4_general_plot <- ggplot(rna_4_distribution, aes(x = RNA_4_Group, y = Count, fill = RNA_4_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "RNA 4 Distribution of HCV Patients",
       x = "RNA 4 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

print(rna_4_general_plot)


#BY GENDER
# Print RNA 4 distribution table by gender
rna_4_gender_distribution <- hcv_data %>%
  group_by(RNA_4_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA 4 by Gender")
print(rna_4_gender_distribution)

# Plot RNA 4 distribution by gender
rna_4_gender_plot <- ggplot(rna_4_gender_distribution, aes(x = RNA_4_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RNA 4 Distribution by Gender",
       x = "RNA 4 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  # Custom colors for gender

print(rna_4_gender_plot)

#RNA 12
#ALL
# Ensure RNA 12 is numeric
hcv_data <- hcv_data %>%
  mutate(RNA_12 = as.numeric(RNA_12))

# Convert Gender 1 → "Male", 2 → "Female"
hcv_data <- hcv_data %>%
  mutate(Gender = recode(Gender, `1` = "Male", `2` = "Female"))

# Categorize RNA 12 into bins
hcv_data <- hcv_data %>%
  mutate(RNA_12_Group = cut(RNA_12, 
                            breaks = c(0, 5, 1201715),  
                            labels = c("0-5", "5-1201715"),
                            include.lowest = TRUE))

# Print general RNA 12 distribution table
rna_12_distribution <- hcv_data %>%
  group_by(RNA_12_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA 12")
print(rna_12_distribution)

# Plot general RNA 12 distribution
rna_12_general_plot <- ggplot(rna_12_distribution, aes(x = RNA_12_Group, y = Count, fill = RNA_12_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "RNA 12 Distribution of HCV Patients",
       x = "RNA 12 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")  

print(rna_12_general_plot)

#BY GENDER
# Print RNA 12 distribution table by gender
rna_12_gender_distribution <- hcv_data %>%
  group_by(RNA_12_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA 12 by Gender")
print(rna_12_gender_distribution)

# Plot RNA 12 distribution by gender
rna_12_gender_plot <- ggplot(rna_12_gender_distribution, aes(x = RNA_12_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RNA 12 Distribution by Gender",
       x = "RNA 12 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  # Custom colors for gender

print(rna_12_gender_plot)

#RNA EF
#ALL
# Ensure RNA EOT is numeric
hcv_data <- hcv_data %>%
  mutate(RNA_EOT = as.numeric(RNA_EOT))

# Categorize RNA EOT into bins
hcv_data <- hcv_data %>%
  mutate(RNA_EOT_Group = cut(RNA_EOT, 
                             breaks = c(0, 5, 808450),  
                             labels = c("0-5", "5-808450"),
                             include.lowest = TRUE))

# Print general RNA EOT distribution table
rna_eot_distribution <- hcv_data %>%
  group_by(RNA_EOT_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA EOT")
print(rna_eot_distribution)

# Plot general RNA EOT distribution
rna_eot_general_plot <- ggplot(rna_eot_distribution, aes(x = RNA_EOT_Group, y = Count, fill = RNA_EOT_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "RNA EOT Distribution of HCV Patients",
       x = "RNA EOT Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

print(rna_eot_general_plot)

#BY GENDER 

# Print RNA EOT distribution table by gender
rna_eot_gender_distribution <- hcv_data %>%
  group_by(RNA_EOT_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA EOT by Gender")
print(rna_eot_gender_distribution)

# Plot RNA EOT distribution by gender
rna_eot_gender_plot <- ggplot(rna_eot_gender_distribution, aes(x = RNA_EOT_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RNA EOT Distribution by Gender",
       x = "RNA EOT Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))

print(rna_eot_gender_plot)

#RNA EF
#ALL
# Categorize RNA EF into bins
hcv_data <- hcv_data %>%
  mutate(RNA_EF_Group = cut(RNA_EF, 
                            breaks = c(0, 5, 808450),  
                            labels = c("0-5", "5-808450"),
                            include.lowest = TRUE))

# Print general RNA EF distribution table
rna_ef_distribution <- hcv_data %>%
  group_by(RNA_EF_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA EF")
print(rna_ef_distribution)

# Plot general RNA EF distribution
rna_ef_general_plot <- ggplot(rna_ef_distribution, aes(x = RNA_EF_Group, y = Count, fill = RNA_EF_Group)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "RNA EF (Elongation Factor) Distribution of HCV Patients",
       x = "RNA EF Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

print(rna_ef_general_plot)

#BY GENDER
# Print RNA EF distribution table by gender
rna_ef_gender_distribution <- hcv_data %>%
  group_by(RNA_EF_Group, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for RNA EF by Gender")
print(rna_ef_gender_distribution)

# Plot RNA EF distribution by gender
rna_ef_gender_plot <- ggplot(rna_ef_gender_distribution, aes(x = RNA_EF_Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RNA EF (Elongation Factor) Distribution by Gender",
       x = "RNA EF Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink")) 

print(rna_ef_gender_plot)

#BASELINE GRADING 
#ALL
# Print general Baseline Histological Grading distribution table
grading_distribution <- hcv_data %>%
  group_by(Baseline_histological_Grading) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Baseline Histological Grading")
print(grading_distribution)

# Plot general Baseline Histological Grading distribution
grading_general_plot <- ggplot(grading_distribution, aes(x = Baseline_histological_Grading, y = Count, fill = Baseline_histological_Grading)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Baseline Histological Grading Distribution",
       x = "Baseline Histological Grading",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

print(grading_general_plot)

#BY GENDER
# Print Baseline Histological Grading distribution table by gender
grading_gender_distribution <- hcv_data %>%
  group_by(Baseline_histological_Grading, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Baseline Histological Grading by Gender")
print(grading_gender_distribution)

# Plot Baseline Histological Grading distribution by gender
grading_gender_plot <- ggplot(grading_gender_distribution, aes(x = Baseline_histological_Grading, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Baseline Histological Grading Distribution by Gender",
       x = "Baseline Histological Grading",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  

print(grading_gender_plot)

#BASELINE STAGING
#ALL
# Print general Baseline Histological Staging distribution table
staging_distribution <- hcv_data %>%
  group_by(Baselinehistological_staging) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Baseline Histological Staging")
print(staging_distribution)

# Plot general Baseline Histological Staging distribution
staging_general_plot <- ggplot(staging_distribution, aes(x = Baselinehistological_staging, y = Count, fill = Baselinehistological_staging)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Baseline Histological Staging Distribution",
       x = "Baseline Histological Staging",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues") 

print(staging_general_plot)

#BY GENDER
# Print Baseline Histological Staging distribution table by gender
staging_gender_distribution <- hcv_data %>%
  group_by(Baselinehistological_staging, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Baseline Histological Staging by Gender")
print(staging_gender_distribution)

# Plot Baseline Histological Staging distribution by gender
staging_gender_plot <- ggplot(staging_gender_distribution, aes(x = Baselinehistological_staging, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Baseline Histological Staging Distribution by Gender",
       x = "Baseline Histological Staging",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "steelblue", "Female" = "pink"))  

print(staging_gender_plot)








#BY AGE GROUP

# Convert Gender 1 → "Male", 2 → "Female"
hcv_data <- hcv_data %>%
  mutate(Gender = recode(Gender, `1` = "Male", `2` = "Female"))

# Define age bins
hcv_data <- hcv_data %>%
  mutate(Age_Group = cut(Age, 
                         breaks = c(0, 32, 37, 42, 47, 52, 57, 62), 
                         labels = c("0-32", "32-37", "37-42", "42-47", "47-52", "52-57", "57-62"),
                         include.lowest = TRUE))

# Calculate age distribution
age_distribution <- hcv_data %>%
  group_by(Age_Group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(age_distribution)

# Plot age distribution
ggplot(hcv_data, aes(x = Age_Group)) +
  geom_bar(fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution by Defined Groups",
       x = "Age Group",
       y = "Count") +
  theme_minimal()

# BMI Distribution by Age
hcv_data <- hcv_data %>%
  mutate(BMI_Group = cut(BMI, 
                         breaks = c(0, 18.5, 25, 30, 35, 40), 
                         labels = c("0-18.5", "18.5-25", "25-30", "30-35", "35-40"),
                         include.lowest = TRUE))

bmi_age_distribution <- hcv_data %>%
  group_by(BMI_Group, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print(bmi_age_distribution)

ggplot(bmi_age_distribution, aes(x = BMI_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Age Distribution by BMI Groups",
       x = "BMI Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

# Define symptom columns
symptom_cols <- c("Fever", "Nausea/Vomting", "Headache", "Diarrhea", 
                  "Fatigue_&_generalized_bone_ache", "Jaundice", "Epigastric_pain")

# Recode 1 → "Absent", 2 → "Present"
hcv_data <- hcv_data %>%
  mutate(across(all_of(symptom_cols), ~ recode(., `1` = "Absent", `2` = "Present")))

# Loop through each symptom and plot the distribution with symptoms on x-axis
for (symptom in symptom_cols) {
  symptom_age_distribution <- hcv_data %>%
    group_by(.data[[symptom]], Age_Group) %>%
    summarise(Count = n(), .groups = "drop")
  
  print(paste("Table for", symptom, "by Age"))
  print(symptom_age_distribution)
  
  # Ensure the plot is displayed by using print()
  print(
    ggplot(symptom_age_distribution, aes(x = .data[[symptom]], y = Count, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(title = paste(symptom, "Distribution by Age"),
           x = symptom,
           y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  )
}

# WBC Distribution by Age
hcv_data <- hcv_data %>%
  mutate(WBC_Group = cut(WBC, 
                         breaks = c(0, 4000, 11000, 12101),  
                         labels = c("0-4000", "4000-11000", "11000-12101"),
                         include.lowest = TRUE))

wbc_age_distribution <- hcv_data %>%
  group_by(WBC_Group, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print(wbc_age_distribution)

ggplot(wbc_age_distribution, aes(x = WBC_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "WBC Distribution by Age",
       x = "WBC Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

# RBC Distribution by Age
hcv_data <- hcv_data %>%
  mutate(RBC_Group = cut(RBC, 
                         breaks = c(0, 3000000, 5000000, 5018451),  
                         labels = c("0-3M", "3M-5M", "5M-5.018M"),
                         include.lowest = TRUE))

rbc_age_distribution <- hcv_data %>%
  group_by(RBC_Group, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print(rbc_age_distribution)

ggplot(rbc_age_distribution, aes(x = RBC_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "RBC Distribution by Age",
       x = "RBC Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

#HEMOGLOBIN
# Define HGB bins based on gender
hcv_data <- hcv_data %>%
  mutate(HGB_Group = case_when(
    Gender == "Male" & HGB >= 2 & HGB < 14 ~ "2-14",
    Gender == "Male" & HGB >= 14 & HGB <= 17.5 ~ "14-17.5",
    Gender == "Male" & HGB > 17.5 & HGB <= 20 ~ "17.5-20",
    Gender == "Female" & HGB >= 2 & HGB < 12.3 ~ "2-12.3",
    Gender == "Female" & HGB >= 12.3 & HGB <= 15.3 ~ "12.3-15.3",
    Gender == "Female" & HGB > 15.3 & HGB <= 20 ~ "15.3-20",
    TRUE ~ NA_character_
  ))

# Count distribution by age
hgb_age_distribution <- hcv_data %>%
  group_by(HGB_Group, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Hemoglobin (HGB) by Age")
print(hgb_age_distribution)

# Plot HGB distribution by age
ggplot(hgb_age_distribution, aes(x = HGB_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Hemoglobin (HGB) Distribution by Age",
       x = "Hemoglobin Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

#PLATELET
# Define Platelet bins
hcv_data <- hcv_data %>%
  mutate(Plat_Group = cut(Plat, 
                          breaks = c(93013, 100000, 255000, 226465),  
                          labels = c("93K-100K", "100K-255K", "255K-226K"), 
                          include.lowest = TRUE))

# Count distribution by age
plat_age_distribution <- hcv_data %>%
  group_by(Plat_Group, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Platelet (Plat) by Age")
print(plat_age_distribution)

# Plot Platelet distribution by age
ggplot(plat_age_distribution, aes(x = Plat_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Platelet (Plat) Distribution by Age",
       x = "Platelet Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

#AST
# Define AST_1 bins
hcv_data <- hcv_data %>%
  mutate(AST1_Group = cut(AST_1, 
                          breaks = c(0, 20, 40, 128),  
                          labels = c("0-20", "20-40", "40-128"),
                          include.lowest = TRUE))

# Count AST distribution by age
ast_age_distribution <- hcv_data %>%
  group_by(AST1_Group, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for AST1 by Age")
print(ast_age_distribution)

# Plot AST1 distribution by age
ggplot(ast_age_distribution, aes(x = AST1_Group, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "AST1 (1 Week) Distribution by Age",
       x = "AST1 Group",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

#ALT
# Define ALT variables
alt_vars <- c("ALT_1", "ALT4", "ALT_12", "ALT_24", "ALT_36", "ALT_48", "ALT_after_24_w")

# Ensure ALT variables are numeric
hcv_data[alt_vars] <- lapply(hcv_data[alt_vars], as.numeric)

# Categorize ALT values into bins
for (alt in alt_vars) {
  hcv_data <- hcv_data %>%
    mutate(!!paste0(alt, "_Group") := cut(.data[[alt]], 
                                          breaks = c(0, 20, 40, 128),  
                                          labels = c("0-20", "20-40", "40-128"),
                                          include.lowest = TRUE))
}

# Loop through all ALT variables and generate tables + plots
for (alt in alt_vars) {
  alt_age_distribution <- hcv_data %>%
    group_by(.data[[paste0(alt, "_Group")]], Age_Group) %>%
    summarise(Count = n(), .groups = "drop")
  
  print(paste("Table for", alt, "by Age"))
  print(alt_age_distribution)
  
  print(
    ggplot(alt_age_distribution, aes(x = .data[[paste0(alt, "_Group")]], y = Count, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(title = paste(alt, "Distribution by Age"),
           x = "ALT Group",
           y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues")
  )
}

#RNA
# Define RNA variables
rna_vars <- c("RNA_Base", "RNA_4", "RNA_12", "RNA_EOT", "RNA_EF")

# Ensure RNA variables are numeric
hcv_data[rna_vars] <- lapply(hcv_data[rna_vars], as.numeric)

# Categorize RNA values into bins
rna_bins <- list(
  "RNA_Base" = c(0, 5, 1201086),
  "RNA_4" = c(0, 5, 1201715),
  "RNA_12" = c(0, 5, 1201715),
  "RNA_EOT" = c(0, 5, 808450),
  "RNA_EF" = c(0, 5, 808450)
)

rna_labels <- c("0-5", ">5")

# Apply binning to all RNA variables
for (rna in rna_vars) {
  hcv_data <- hcv_data %>%
    mutate(!!paste0(rna, "_Group") := cut(.data[[rna]], 
                                          breaks = rna_bins[[rna]],  
                                          labels = rna_labels,
                                          include.lowest = TRUE))
}

# Loop through all RNA variables and generate tables + plots
for (rna in rna_vars) {
  rna_age_distribution <- hcv_data %>%
    group_by(.data[[paste0(rna, "_Group")]], Age_Group) %>%
    summarise(Count = n(), .groups = "drop")
  
  print(paste("Table for", rna, "by Age"))
  print(rna_age_distribution)
  
  print(
    ggplot(rna_age_distribution, aes(x = .data[[paste0(rna, "_Group")]], y = Count, fill = Age_Group)) +
      geom_bar(stat = "identity", position = "dodge", color = "black") +
      labs(title = paste(rna, "Distribution by Age"),
           x = "RNA Group",
           y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues")
  )
}

# Baseline Histological Grading Distribution by Age
grading_age_distribution <- hcv_data %>%
  group_by(Baseline_histological_Grading, Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Baseline Histological Grading by Age")
print(grading_age_distribution)

ggplot(grading_age_distribution, aes(x = Baseline_histological_Grading, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Baseline Histological Grading Distribution by Age",
       x = "Baseline_histological_Grading",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Baseline Histological Staging Distribution by Age
staging_age_distribution <- hcv_data %>%
  group_by(Baselinehistological_staging,Age_Group) %>%
  summarise(Count = n(), .groups = "drop")

print("Table for Baseline Histological Staging by Age")
print(staging_age_distribution)

ggplot(staging_age_distribution, aes(x = Baselinehistological_staging, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Baseline Histological Staging Distribution by Age",
       x = "Baselinehistological_staging",
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
