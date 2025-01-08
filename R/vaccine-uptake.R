source("R/config.R")
library(dplyr)
library(ggplot2)
library(see)
library(BSol.mapR)

male_color <- "#9657E0"
female_color <- "#1DAA47"

LSOA_IMD_lookup <- read.csv("data/WM-LSOA-lookup.csv")

# Only load vaccine data if it's not already loaded
if(!exists("vaccine_data")) {
  vaccine_data <- read.csv(
    paste0(
      vaccine_data_path, 
      "/covid_vaccinations-oct23_to_sept24.csv"
    )
  ) %>%
    # Filter for only BSol LSOAs
    filter(
      LSOA %in% LSOA11@data$LSOA11
    ) %>%
    mutate(
      # Fix excel madness
      AgeBand = case_when(
        AgeBand == "05-Sep" ~ "5-19",
        AgeBand == "Oct-14" ~ "10-14",
        TRUE ~ AgeBand
      ),
      Ethnicity = gsub("\\w: ","", ethnicity_description),
      Ethnicity = case_when(
        Ethnicity == "British" ~ "White British",
        TRUE ~ Ethnicity
      ),
      # Fix capitalisation
      Ethnicity = gsub("asian", "Asian", Ethnicity),
      Ethnicity = gsub("black", "Black", Ethnicity),
      Ethnicity = gsub("african", "African", Ethnicity),
      Ethnicity = gsub("caribbean", "Caribbean", Ethnicity),
      BroadEthnicity = case_when(
        Ethnicity %in% c("Pakistani", "Indian", 
                         "Bangladeshi", "Other Asian",
                         "Chinese") ~ "Asian",
        Ethnicity %in% c("African", "Caribbean", 
                         "Other Black") ~ "Black",
        Ethnicity %in% c("White and Black Caribbean", 
                         "White and Black African", 
                         "White and Asian",
                         "Mixed other") ~ "Mixed",
        Ethnicity %in% c("White British", "Irish", 
                         "Other White",
                         "Gypsy or Irish Traveller",
                         "Roma") ~ "White",
        Ethnicity %in% c("Any other ethnic group", "Arab") ~ "Other",
        Ethnicity %in% c("Not stated") ~ "Unknown"
      )
    ) %>%
    rename(Sex = gender) %>%
    left_join(LSOA_IMD_lookup %>%
                select(-Locality),
              by = join_by(LSOA)) %>%
    arrange(BroadEthnicity, Ethnicity)
  eth_order <- rev(unique(vaccine_data$Ethnicity))
  # Order by Broad ethnicity and alphabetical 
  vaccine_data$Ethnicity <- factor(
    vaccine_data$Ethnicity,
    levels = eth_order
    )
}

# Only load GP registration data if it's not already loaded
if(!exists("GP_reg_data")) {
  GP_reg_data <- read.csv(
    paste0(
      GP_data_path, 
      "/GP_reg_data_full_Dec_24.csv"
    )
  ) %>%
    # Filter for only BSol LSOAs
    filter(
      LSOA_2011 %in% LSOA11@data$LSOA11
    ) %>%
    mutate(
      # Fix excel madness
      AgeBand = case_when(
        AgeBand == "05-Sep" ~ "5-19",
        AgeBand == "Oct-14" ~ "10-14",
        TRUE ~ AgeBand
      ),
      Ethnicity = case_when(
        Ethnic_Description_National == "British" ~ "White British",
        TRUE ~ Ethnic_Description_National
      ),
      BroadEthnicity = case_when(
        Ethnicity %in% c("Pakistani", "Indian", 
                         "Bangladeshi", "Any other Asian background") ~ "Asian",
        Ethnicity %in% c("African", "Caribbean", 
                         "Any other Black background") ~ "Black",
        Ethnicity %in% c("White and Black Caribbean", 
                         "White and Black African", 
                         "White and Asian",
                         "Any other mixed background") ~ "Mixed",
        Ethnicity %in% c("White British", "Irish", 
                         "Any other white background") ~ "White",
        Ethnicity %in% c("Chinese","Any other ethnic group") ~ "Other",
        Ethnicity %in% c("Not stated") ~ "Unknown"
      )
    ) %>%
    rename(LSOA = LSOA_2011,
           Sex = Gender) %>%
    left_join(LSOA_IMD_lookup,
              by = join_by(LSOA))
}


#################################################################
#           Uptake distributions (Age, ethnicity, IMD)          # 
#################################################################

##### Age #####

# Plot age distribution of those receiving the Covid-19 vaccine
female_data <- vaccine_data %>% filter(Sex == "Female")
male_data <- vaccine_data %>% filter(Sex == "Male")

age_annotation <- vaccine_data %>%
  group_by(Ethnicity, Sex) %>%
  filter(Sex %in% c("Female", "Male")) %>%
  summarise(
    median_age = median(age, rm.na = T)
  ) %>%
  tidyr::pivot_wider(
    id_cols = Ethnicity,
    names_from = Sex,
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
  scale_fill_manual(values = c(female_color, male_color)) +
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

##### Ethnicity #####

p2 <- vaccine_data %>%
  filter(Sex %in% c("Female", "Male")) %>%
  count(Ethnicity, Sex) %>%
  mutate(
     plot_num = prettyNum(n, big.mark = ",", scientific = F)
  ) %>%
  ggplot( 
    aes(y = Ethnicity, x = n, fill = Sex)
    ) +
  geom_col(
    position = "dodge",
  ) +
  geom_text(
    aes(
      x = n,
      label = plot_num
      ),
    position = position_dodge(width = 1),
    hjust = -0.15,
    size = 3.5) +
  theme_bw() +
  labs(
    x = "Number of Covid-19 Vaccines",
    y = "",
    fill = "",
    title = "Ethnicty Distribution of Covid-19 Vaccine Recipients\n(October 23 – September 24)"
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top",
  ) +
  scale_x_continuous(
    breaks = seq(0, 1.5e5, 0.5e5),
    labels = prettyNum(seq(0, 1.5e5, 0.5e5), big.mark = ",",
                       scientific = F),
    lim = c(0, 1.5e5)
  )
p2
ggsave("output/eth_dist.png", p2,
       width = 6, height = 7, dpi = 300)

#################################################################
#             Uptake rates (Age, ethnicity, sex)                # 
#################################################################

# Plot uptake rates by age group (< 65, 65-80, 80+), ethnicity, and sex
eth_uptake_rates <- GP_reg_data %>%
  group_by(AgeBand, BroadEthnicity, Ethnicity, Sex) %>%
  summarise(
    N = sum(Observations, na.rm = T)
  ) %>%
  full_join(
    vaccine_data %>% 
      count(Ethnicity, Sex, AgeBand),
    by = join_by("AgeBand", "Ethnicity", "Sex")
    ) %>%
  filter(AgeBand != "NA-NA",
         Ethnicity != "Not stated",
         Ethnicity != "Not Known",
         Sex %in% c("Male", "Female")) %>%
  mutate(
    # Impute NA in N and n with 0
    N = case_when(
      is.na(N) ~ 0,
      TRUE ~ N
    ),
    n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    ),
    Age_Group = case_when(
      AgeBand %in% c("65-69","70-74","75-79") ~ "65-79",
      AgeBand %in% c("80-84", "85-89","90-94","95-99","100-104") ~ "80+",
      TRUE ~ "Less than 65"
    ),
  ) %>%
  group_by(
    Ethnicity, BroadEthnicity, Sex, Age_Group
  ) %>%
  summarise(
    n = sum(n),
    N_GP = sum(N),
    p_GP = n/N_GP,
    perc_GP = 100 * p_GP,
    Z = qnorm(0.975),
    LowerCI95 = 100 * n * (1 - 1/(9*n) - Z/3 * sqrt(1/(n+1)))**3/N_GP,
    UpperCI95 = 100 * (n + 1) * (1 - 1/(9*(n + 1)) + Z/3 * sqrt(1/(n + 1)))**3/N_GP
  )

eth_uptake_rates$Ethnicity <- factor(
  eth_uptake_rates$Ethnicity,
  levels = eth_order
)

uptake_rates_age <- eth_uptake_rates %>%
  group_by(Age_Group) %>%
  summarise(
    n = sum(n),
    N_GP = sum(N_GP),
    p_GP = n/N_GP,
    perc_GP = 100 * p_GP,
    Z = qnorm(0.975),
    LowerCI95 = 100 * n * (1 - 1/(9*n) - Z/3 * sqrt(1/(n+1)))**3/N_GP,
    UpperCI95 = 100 * (n + 1) * (1 - 1/(9*(n + 1)) + Z/3 * sqrt(1/(n + 1)))**3/N_GP
  )


p3 <- ggplot(eth_uptake_rates, aes(y = Ethnicity, x = perc_GP, fill = Sex)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(xmin = LowerCI95, 
        xmax = UpperCI95),
    position = position_dodge(width = 1),
    linewidth = 0.5,
    width = 0.3
  ) +
  theme_bw() +
  facet_wrap(
    ~~factor(Age_Group, levels=c("Less than 65", "65-79", "80+")), 
    ncol = 3, 
    scales = "free_x"
    ) +
  geom_vline(
    data = uptake_rates_age,
    aes(xintercept = perc_GP,
        linetype = "BSol Average")
  ) +
  labs(
    y = "",
    x = "Covid-19 Vaccine Uptake %",
    linetype = "",
    title = "Covid-19 Vaccine Uptake % by Ethnicity, Age, and Sex\n(October 23 – September 24)"
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  scale_linetype_manual(values = c("dashed")) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 

p3

ggsave("output/vaccine-uptake.png", p3,
       width = 9, height = 7)

# Uptake by IMD
IMD_uptake <- GP_reg_data %>%
  group_by(AgeBand, IMD_quintile, Sex) %>%
  summarise(
    N = sum(Observations, na.rm = T)
  ) %>%
  full_join(
    vaccine_data %>% 
      count(IMD_quintile, Sex, AgeBand),
    by = join_by("AgeBand", "IMD_quintile", "Sex")
  ) %>%
  filter(AgeBand != "NA-NA",
         !is.na(IMD_quintile),
         Sex %in% c("Male", "Female")) %>%
  mutate(
    # Impute NA in N and n with 0
    N = case_when(
      is.na(N) ~ 0,
      TRUE ~ N
    ),
    n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    ),
    Age_Group = case_when(
      AgeBand %in% c("65-69","70-74","75-79") ~ "65-79",
      AgeBand %in% c("80-84", "85-89","90-94","95-99","100-104") ~ "80+",
      TRUE ~ "Less than 65"
    ),
    IMD_quintile = as.character(IMD_quintile),
  ) %>%
  group_by(
    IMD_quintile, Sex, Age_Group
  ) %>%
  summarise(
    n = sum(n),
    N_GP = sum(N),
    p_GP = n/N_GP,
    perc_GP = 100 * p_GP,
    Z = qnorm(0.975),
    LowerCI95 = 100 * n * (1 - 1/(9*n) - Z/3 * sqrt(1/(n+1)))**3/N_GP,
    UpperCI95 = 100 * (n + 1) * (1 - 1/(9*(n + 1)) + Z/3 * sqrt(1/(n + 1)))**3/N_GP
  )

p4 <- ggplot(IMD_uptake, aes(y = IMD_quintile, x = perc_GP, fill = Sex)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(xmin = LowerCI95, 
        xmax = UpperCI95),
    position = position_dodge(width = 1),
    linewidth = 0.5,
    width = 0.3
  ) +
  theme_bw() +
  facet_wrap(
    ~factor(Age_Group, levels=c("Less than 65", "65-79", "80+")), 
    ncol = 3, 
    scales = "free_x"
  ) +
  geom_vline(
    data = uptake_rates_age,
    aes(xintercept = perc_GP,
        linetype = "BSol Average")
  ) +
  labs(
    y = "",
    x = "Covid-19 Vaccine Uptake %",
    linetype = "",
    title = "Covid-19 Vaccine Uptake % by IMD Quintile, Age, and Sex\n(October 23 – September 24)"
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  scale_linetype_manual(values = c("dashed")) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 

p4

ggsave("output/vaccine-uptake-IMD.png", p4,
       width = 9, height = 7)


## Uptake % by ethnicity and IMD

IMD_eth_uptake <- IMD_uptake <- GP_reg_data %>%
  group_by(AgeBand, Ethnicity, BroadEthnicity, IMD_quintile, Sex) %>%
  summarise(
    N = sum(Observations, na.rm = T)
  ) %>%
  full_join(
    vaccine_data %>% 
      count(IMD_quintile, Sex, Ethnicity, AgeBand),
    by = join_by("AgeBand", "Ethnicity","IMD_quintile", "Sex")
  ) %>%
  filter(AgeBand != "NA-NA",
         !is.na(IMD_quintile),
         Sex %in% c("Male", "Female"),
         Ethnicity != "Not stated",
         Ethnicity != "Not Known"
         ) %>%
  mutate(
    # Impute NA in N and n with 0
    N = case_when(
      is.na(N) ~ 0,
      TRUE ~ N
    ),
    n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    ),
    Age_Group = case_when(
      AgeBand %in% c("65-69","70-74","75-79") ~ "65-79",
      AgeBand %in% c("80-84", "85-89","90-94","95-99","100-104") ~ "80+",
      TRUE ~ "Less than 65"
    ),
    IMD_quintile = as.character(IMD_quintile),
  ) 


IMD_broadeth_rate <- IMD_eth_uptake %>%
  group_by(
    BroadEthnicity, IMD_quintile, Sex, Age_Group
  ) %>%
  summarise(
    n = sum(n),
    N_GP = sum(N),
    p_GP = n/N_GP,
    perc_GP = 100 * p_GP,
    Z = qnorm(0.975),
    LowerCI95 = 100 * n * (1 - 1/(9*n) - Z/3 * sqrt(1/(n+1)))**3/N_GP,
    UpperCI95 = 100 * (n + 1) * (1 - 1/(9*(n + 1)) + Z/3 * sqrt(1/(n + 1)))**3/N_GP
  )

# Save for plotting in python using EquiPy
write.csv(
  IMD_broadeth_rate,
  paste0(
    vaccine_data_path,
    "/covid_vac_rates_broad-oct23_to_sept24.csv"
    )
  )

IMD_eth_rate <- IMD_eth_uptake %>%
  group_by(
    Ethnicity, IMD_quintile, Sex, Age_Group
  ) %>%
  summarise(
    n = sum(n),
    N_GP = sum(N),
    p_GP = n/N_GP,
    perc_GP = 100 * p_GP,
    Z = qnorm(0.975),
    LowerCI95 = 100 * n * (1 - 1/(9*n) - Z/3 * sqrt(1/(n+1)))**3/N_GP,
    UpperCI95 = 100 * (n + 1) * (1 - 1/(9*(n + 1)) + Z/3 * sqrt(1/(n + 1)))**3/N_GP
  )

# Save for plotting in python using EquiPy
write.csv(
  IMD_eth_rate,
  paste0(
    vaccine_data_path,
    "/covid_vac_rates-oct23_to_sept24.csv"
  )
)


#################################################################
#               Calculate areas of opportunity                  # 
#################################################################

# Assuming that we want each community to have at least one Covid-19
# vaccine within the last 12 months, how many people in each group
# are "un-vaccinated in the last 12 months".

# TODO:
#  - Calculate estimated number not vaccinated for each ethnic group  
#  - Plot estimated number not vaccinated across Birmingham
#  - Plot estimated number not vaccinated for each ethnicity

unvaxed <- GP_reg_data %>%
  group_by(AgeBand, BroadEthnicity, Ethnicity, IMD_quintile, Sex) %>%
  summarise(
    N = sum(Observations, na.rm = T)
  ) %>%
  full_join(
    vaccine_data %>% 
      count(Ethnicity, Sex,IMD_quintile, AgeBand),
    by = join_by("AgeBand", "Ethnicity", "IMD_quintile", "Sex")
  ) %>%
  filter(AgeBand != "NA-NA",
         Ethnicity != "Not stated",
         Ethnicity != "Not Known",
         Sex %in% c("Male", "Female"),
         !is.na(IMD_quintile)) %>%
  mutate(
    # Impute NA in N and n with 0
    N = case_when(
      is.na(N) ~ 0,
      TRUE ~ N
    ),
    n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    ),
    Age_Group = case_when(
      AgeBand %in% c("65-69","70-74","75-79") ~ "65-79",
      AgeBand %in% c("80-84", "85-89","90-94","95-99","100-104") ~ "80+",
      TRUE ~ "Less than 65"
    )
  ) %>% 
  group_by(
    Age_Group, Sex, Ethnicity, IMD_quintile
  ) %>%
  summarise(
    N = sum(N),
    n = sum(n)
  ) %>%
  mutate(
    # If value is < 0 (i.e. people double vaxed) then fix to zero
    number_unvaxed = case_when(
      N - n >= 0 ~ N - n,
      TRUE ~ 0
    )
  )

unvaxed$Ethnicity <- factor(
  unvaxed$Ethnicity,
  levels = eth_order
)

plt_unvaxed_eth <- unvaxed %>%
  group_by(Sex, Ethnicity, Age_Group) %>%
  summarise(number_unvaxed = sum(number_unvaxed)) %>%
  ggplot(aes(y = Ethnicity, x = number_unvaxed, fill = Sex)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_wrap(
    ~factor(Age_Group, levels=c("Less than 65", "65-79", "80+")), 
    ncol = 3, 
    scales = "free_x"
    ) +
  labs(
    x = "Estimated Number not Vaccinated for Covid-19",
    y = "",
    title = "Estimated Number of Residents not Vaccinated for Covid-19 by Ethnicity, Age, and Sex\n(October 2023 – September 2024)"
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 

plt_unvaxed_eth
ggsave("output/unvaccinated/unvaccinated_eth.png", plt_unvaxed_eth,
       width = 9, height = 7)

plt_unvaxed_IMD <- unvaxed %>%
  mutate(IMD_quintile = as.character(IMD_quintile)) %>%
  group_by(Sex, IMD_quintile, Age_Group) %>%
  summarise(number_unvaxed = sum(number_unvaxed)) %>%
  ggplot(aes(y = IMD_quintile, x = number_unvaxed, fill = Sex)) +
  geom_col(position = "dodge") +
  theme_bw() +
  facet_wrap(
    ~factor(Age_Group, levels=c("Less than 65", "65-79", "80+")), 
    ncol = 3, 
    scales = "free_x"
  ) +
  labs(
    x = "Estimated Number not Vaccinated for Covid-19",
    y = "",
    title = "Estimated Number of Residents not Vaccinated for Covid-19 by IMD Quintile, Age, and Sex\n(October 2023 – September 2024)"
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  ) 

plt_unvaxed_IMD
ggsave("output/unvaccinated/unvaccinated_imd.png", plt_unvaxed_IMD,
       width = 9, height = 7)

# Estimate number for IMD, ethnicity, age, and sex
unvaxed_count_eth_IMD <- unvaxed %>%
  mutate(IMD_quintile = as.character(IMD_quintile)) %>%
  group_by(Sex, Ethnicity, IMD_quintile, Age_Group) %>%
  summarise(number_unvaxed = sum(number_unvaxed))
# Save for plotting in python using EquiPy
write.csv(
  unvaxed_count_eth_IMD,
  paste0(
    vaccine_data_path,
    "/covid_unvaccinated_counts-oct23_to_sept24.csv"
  )
)

# Estimate number for IMD, ethnicity, and age 
unvaxed_count_eth_IMD_both <- unvaxed %>%
  mutate(IMD_quintile = as.character(IMD_quintile)) %>%
  group_by(Ethnicity, IMD_quintile, Age_Group) %>%
  summarise(number_unvaxed = sum(number_unvaxed))
# Save for plotting in python using EquiPy
write.csv(
  unvaxed_count_eth_IMD_both,
  paste0(
    vaccine_data_path,
    "/covid_unvaccinated_counts_comb-oct23_to_sept24.csv"
  )
)

# Map unvaccinated estimates in for each constituency for:
# - Indian
# - Pakistani
# - Bangladeshi 
# - Caribbean
# - African
# - Any other White background
# - White British
# For everyone aged less than 65

unvaxed_LSOA <- GP_reg_data %>%
  filter(
    !(AgeBand %in% c("65-69","70-74","75-79","80-84","85-89","90-94","95-99",
                   "100-104","105-109","110-114")),
    Ethnicity != "Not Known",
    LSOA %in% LSOA11@data$LSOA11
  ) %>% 
  group_by(LSOA, Ethnicity, AgeBand, Constituency) %>%
  summarise(N = sum(Observations)) %>%
  left_join(
    vaccine_data %>%
      filter(
        Ethnicity != "Not stated"
      ) %>%
      count(LSOA, Ethnicity, AgeBand),
    by = join_by("LSOA", "Ethnicity","AgeBand")
  ) %>%
  mutate(
    n = case_when(
      is.na(n) ~ 0,
      TRUE ~ n
    )
  ) %>% 
  group_by(
    Constituency, LSOA, Ethnicity
  ) %>%
  summarise(
    number_unvaxed = sum(N) - sum(n),
    total_number = sum(N)
  ) %>%
  mutate(
    number_unvaxed = case_when(
      number_unvaxed < 0 ~ 0,
      TRUE ~ number_unvaxed
    )
  ) %>%
  rename(LSOA11 = LSOA)

for (eth_i in c(eth_order, "All")) {
  if (eth_i != "All") {
    unvaxed_LSOA_i <- unvaxed_LSOA %>%
      filter(
        Ethnicity == eth_i
      ) 
    
    map_title <- paste(
      "Estimated number of",
      eth_i,
      "residents aged 65+ who didn't receive a Covid-19",
      "vaccine between Oct 2023 and Sept 2024"
    )
    
  } else {
    unvaxed_LSOA_i <- unvaxed_LSOA %>%
      group_by(
        LSOA11, Constituency
      ) %>%
      summarise(
        number_unvaxed = sum(number_unvaxed),
        total_number = sum(total_number)
      )
    
    map_title <- paste(
      "Estimated number of residents",
      "of all ethnicities",
      "aged 65+ who didn't receive a Covid-19 vaccine",
      "between Oct 2023 and Sept 2024"
    )
  }

  print(eth_i)
  print(paste(
    sum(unvaxed_LSOA_i$number_unvaxed),
    "of",
    sum(unvaxed_LSOA_i$total_number)
    )
    )
  print(unvaxed_LSOA_i %>%
          group_by(Constituency) %>%
          summarise(
            number_unvaxed = sum(number_unvaxed),
            total_number = sum(total_number)
          ) %>%
          ungroup() %>%
          mutate(
            perc = round(100*number_unvaxed / sum(number_unvaxed), 1)
          ) %>%
          arrange(desc(perc)))

  # Make sure that counts less than 5 are suppressed
  unvaxed_LSOA_i <- unvaxed_LSOA_i %>%
    mutate(
      number_unvaxed = case_when(
        number_unvaxed < 5 ~ 0,
        TRUE ~ number_unvaxed
      )
    )

  
  map <- plot_map(
    unvaxed_LSOA_i,
    "number_unvaxed",
    map_type = "LSOA11",
    fill_missing  = 0,
    style = "cont",
    map_title = map_title
  )
  save_name <- paste0(
    "output/unvaccinated/maps/less-than-65/",
    gsub(" ", "-", eth_i),
    ".png"
  )
  save_map(map, save_name)
}

unvaxed_eth_tables <- list()
    
for (eth_i in c(eth_order, "All")) {
  if (eth_i != "All") {
    unvaxed_LSOA_i <- unvaxed_LSOA %>%
      filter(
        Ethnicity == eth_i
      ) 
    
    map_title <- paste(
      "Estimated number of",
      eth_i,
      "residents aged less than 65 who didn't receive a Covid-19",
      "vaccine between Oct 2023 and Sept 2024"
    )
    
  } else {
    unvaxed_LSOA_i <- unvaxed_LSOA %>%
      group_by(
        LSOA11, Constituency
      ) %>%
      summarise(
        number_unvaxed = sum(number_unvaxed),
        total_number = sum(total_number)
      )
    
    map_title <- paste(
      "Estimated number of residents",
      "of all ethnicities",
      "aged less than 65 who didn't receive a Covid-19 vaccine",
      "between Oct 2023 and Sept 2024"
    )
  }

  print(eth_i)
  print(paste(
    sum(unvaxed_LSOA_i$number_unvaxed),
    "of",
    sum(unvaxed_LSOA_i$total_number)
    )
    )
  unvaxed_eth_table_i <- unvaxed_LSOA_i %>%
    group_by(Constituency) %>%
    summarise(
      number_unvaxed = sum(number_unvaxed),
      total_number = sum(total_number)
    ) %>%
    ungroup() %>%
    mutate(
      perc = round(100*number_unvaxed / sum(number_unvaxed), 1)
    ) %>%
    arrange(desc(perc))
  
  print(unvaxed_eth_table_i)
  
  unvaxed_eth_tables[[eth_i]] <- unvaxed_eth_table_i
  
  # Make sure that counts less than 5 are suppressed
  unvaxed_LSOA_i <- unvaxed_LSOA_i %>%
    mutate(
      number_unvaxed = case_when(
        number_unvaxed < 5 ~ 0,
        TRUE ~ number_unvaxed
      )
    )

  
  map <- plot_map(
    unvaxed_LSOA_i,
    "number_unvaxed",
    map_type = "LSOA11",
    fill_missing  = 0,
    style = "cont",
    map_title = map_title
  )
  save_name <- paste0(
    "output/unvaccinated/maps/less-than-65/",
    gsub(" ", "-", eth_i),
    ".png"
  )
  save_map(map, save_name)
}

writexl::write_xlsx(
  unvaxed_eth_tables, 
  "output/unvaccinated/unvaccinated-by-const-less-than-65.xlsx"
  )