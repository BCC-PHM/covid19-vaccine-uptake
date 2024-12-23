source("R/config.R")
library(readxl)
library(dplyr)
library(ggplot2)
library(see)

# Only load vaccine data if it's not already loaded
if(!exists("vaccine_data")) {
  vaccine_data <- read_excel(
    paste0(
      vaccine_data_path, 
      "/covid_vaccinations-oct23_to_sept24.xlsx"
    )
  ) %>%
    mutate(
      Ethnicity = gsub("\\w: ","", ethnicity_description),
      Ethnicity = case_when(
        Ethnicity == "British" ~ "White British",
        TRUE ~ Ethnicity
      ),
      BroadEthnicity = case_when(
        Ethnicity %in% c("Pakistani", "Indian", 
                         "Bangladeshi", "Any other asian background") ~ "Asian",
        Ethnicity %in% c("African", "Caribbean", 
                         "Any other black background") ~ "Black",
        Ethnicity %in% c("White and black caribbean", 
                         "White and black african", 
                         "White and asian",
                         "Any other mixed background") ~ "Mixed",
        Ethnicity %in% c("White British", "Irish", 
                         "Any other white background") ~ "White",
        Ethnicity %in% c("Chinese","Any other ethnic group") ~ "Other",
        Ethnicity %in% c("Not stated") ~ "Unknown"
      )
    ) %>%
    arrange(BroadEthnicity, Ethnicity)
  
  # Order by Broad ethnicity and alphabetical 
  vaccine_data$Ethnicity <- factor(
    vaccine_data$Ethnicity,
    labels = rev(unique(vaccine_data$Ethnicity))
    )
}

# Only load GP registration data if it's not already loaded
if(!exists("GP_reg_data")) {
  GP_reg_data <- read.csv(
    paste0(
      GP_data_path, 
      "/GP_reg_data_full_Dec_24.csv"
    )
  )
}

#################################################################
#                 Analyse age distribution                      # 
#################################################################

# Plot age distribution of those receiving the Covid-19 vaccine
female_data <- vaccine_data %>% filter(gender == "Female")
male_data <- vaccine_data %>% filter(gender == "Male")

age_annotation <- vaccine_data %>%
  group_by(Ethnicity, gender) %>%
  filter(gender %in% c("Female", "Male")) %>%
  summarise(
    median_age = median(age, rm.na = T)
  ) %>%
  tidyr::pivot_wider(
    id_cols = Ethnicity,
    names_from = gender,
    values_from = median_age
  ) %>%
  mutate(
    age_text = paste0("F:", Female, " | M:", Male)
  )

p <- ggplot(female_data, 
            aes(x=Ethnicity, y=age)) +
  see::geom_violinhalf(
    aes(fill = "Female"),
    ) +
  see::geom_violinhalf(
    data = male_data, 
    flip = T,
    aes(fill = "Male"),
    ) +
  coord_flip(ylim = c(0, 100), 
             clip = 'off') + 
  theme_bw() +
  labs(
    x = "",
    y = "Age",
    fill = "",
    title = "Age distribution of Covid-19 Vaccine Recipients\n(October 23 – September 24)"
    ) +
  scale_fill_manual(values = c("#18981a", "#880990")) +
  theme(
    legend.position = "top",
    plot.margin = unit(c(1,6,1,1), "lines"),
    plot.title = element_text(hjust = 0.5)
        )+
  geom_text(
    data = age_annotation,
    aes(label = age_text),
    y = 110,
    hjust = 0,
    size = 4) 
p
ggsave("output/age_dist.png", p,
       width = 7, height = 10, dpi = 300)

# Plot uptake rates by age group (< 65, 65-80, 80+), ethnicity, and LA
uptake_rates <- vaccine_data %>%
  mutate(
    AgeBand = paste0(
      floor(age / 5) * 5,
      "-",
      floor(age / 5) * 5 + 4
    )
  ) %>%
  left_join(
    GP_reg_data %>%
      group_by(AgeBand, Ethnic_Description_National)
  )
  