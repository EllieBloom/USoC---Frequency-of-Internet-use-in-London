# Summarising Understanding Society - frequency of internet use


# Understanding Society data can be accessed via the UK Data Service (safeguarded, required end user license):
# https://datacatalogue.ukdataservice.ac.uk/studies/study/6614#details
# Understanding Society: Waves 1-15, 2009-2024 and Harmonised BHPS: Waves 1-18, 1991-2009
# Persistent identifier (DOI): 10.5255/UKDA-SN-6614-21

# 1. Setup -------------------------------------------------------------------

# Wave to use: change as required
wave <- "o"

# Loading packages
library(dplyr)

# Update this to personal working directory if required
#setwd("")

# 2. Reading data ---------------------------------------------------------

# Update to data folder:
inpath <- "Data/UKDA-6614-stata/stata/stata14_se/ukhls/"

# Defining files and variables for wave of interest
cols <- c("hidp", # Household id - needed to link with other dataset for IMD
          "age_dv", # Single year age
          "sex_dv", # Sex
          "ethn_dv", # Major ethnicity
          "hiqual_dv", # Highest qualification
          "gor_dv", # Region
          "netpusenew", # Frequency of internet use variable
          "inding2_xw",# Note this weight is only used in wave 14 (n) and wave 15 (o) - before this - indinui_xw - could change if need to go further back
          "strata",
          "psu")  #!Adding this in, but not used 

# Open the data - wave specified (o)
data <- haven::read_dta(
  file = paste0(inpath,wave,"_indresp.dta"),
  col_select = paste0(wave,"_",cols))

# Remove the wave prefix
names(data) <- substring(names(data), 3)


# Define missing values
missval <- c(-9,-8,-7,-2,-1)

for (i in 1:5) {
  data <- data %>% mutate_all(., list(~na_if(., missval[i])))
}


# Filter for London only
data <- data %>% 
        filter(gor_dv == 7) %>% #Could adapt to use for other regions easily
        mutate(wave = wave)

# Indall dataset for IMD
# Columns to select
cols_imd <- c("hidp",
              "imd2019qe_dv") #IMD 2019 (2025 not there) quintile - 1 most deprived


# Opening data of specified wave
indall_data <- haven::read_dta(file = paste0(inpath,wave,"_indall.dta"),
                        col_select = paste0(wave,"_",cols_imd))%>%
                        distinct()

# Removes wave prefix
names(indall_data) <- substring(names(indall_data), 3)

# Combine IMD with data using key hidp (household id)
data <- left_join(data,indall_data, by="hidp")


# 3. Deriving new variables -----------------------------------------------


# a) Groupings to match other workstreams -----------------------------

data <- data %>%
        mutate(age = case_when(
          age_dv<=25 ~ "16-25", # Combined 16-17 and 18-25 as sample size v small for former
          age_dv >25 & age_dv<=64 ~ "26-64",
          age_dv >64 & age_dv<=74 ~ "65-74",
          age_dv >74 ~ "75+" # Combined 75-89 and 90+ as latter sample size v small
                 )) %>%
        mutate(sex = case_when(
          sex_dv == 1 ~ "Male",
          sex_dv == 2 ~ "Female",
          sex_dv <=0 ~ NA
                 )) %>%
        mutate(ethn = case_when(
          ethn_dv %in% c(1, # british/english/scottish/welsh/northern irish
                         2, # irish
                         3, # gypsy or irish traveller 
                         4  # any other white background
                         ) ~ "White",
          ethn_dv %in% c(5, # white and black caribbean
                         6, # white and black african
                         7, # white and asian
                         8 # any other mixed background
                         ) ~ "Mixed or multiple ethnic groups",
          ethn_dv %in% c(9, # indian
                        10, # pakistani
                        11, # bangladeshi
                        12, # chinese
                        13 #any other asian background
                        ) ~ "Asian or Asian British",
          ethn_dv %in% c(14, # caribbean
                         15, # african
                         16 # any other black background
                        ) ~ "Black, Black British, Caribbean or African",
          ethn_dv %in% c(17, # arab
                         97 # any other ethnic group
                          ) ~ "Any other ethnic group"
        ))%>%
        mutate(hiqual = case_when(          
           hiqual_dv %in% c(1,  # Degree
                            2,  # Other higher degree
                            3,  # A-level etc
                            4,  # GCSE etc
                            5   # Other qualification
                          ) ~ "Formal qualification",
          hiqual_dv == 9 ~ "No formal qualification"
        )) %>%
        mutate(netpusenew_grouped = case_when(
          netpusenew %in% c(1, # Almost all of the time
                            2, # Several times a day
                            3, # Once or twice a day
                            4 # Several times a week
                            ) ~ 1, #Frequent (daily/weekly)
          netpusenew %in% c(5, # Several times a month
                            6, # Once a month
                            7, # Less than once per month
                            8, # Never use
                            9 # No access at home, at work or elsewhere
                            ) ~ 0
        ))%>%
        mutate(imd2019_qe = as.character(imd2019qe_dv)) # Same data type as others

# Note IMD quintile already 1-5 (1=most deprived)


# 4. Specifying survey structure ---------------------------------------------

data_srvyr <- data %>% 
                  srvyr::as_survey_design(
                    strata = strata, 
                    weights = inding2_xw
                    ) 

# Not including psus as there are stratum with single psus so standard errors cannot be computed


# 5. Confidence intervals and sample sizes--------------------------------------

## a) By characteristics ---------------------------------------------------

output_summary <- tibble()

cat_vars <- c("age", "sex", "ethn", "hiqual", "imd2019_qe")

for (i in 1:length(cat_vars)){
output_type <- data_srvyr %>%
                        group_by(eval(parse(text=cat_vars[i]))) %>%
                        summarise(netpusenew_grouped = srvyr::survey_mean(netpusenew_grouped,vartype="ci", 
                                                                          na.rm=TRUE,
                                                                          proportion=TRUE)
                                  )%>% 
                        mutate(category_type=cat_vars[i])%>%
                        rename('category_subtype' = 1)%>%
                        relocate(category_type, .before=category_subtype)
output_summary <- rbind(output_summary,output_type)}

# Remove NAs  
output_summary <- output_summary %>% filter(!is.na(category_subtype))




## b) Overall London  --------------------------------------------------


# Overall London rate
london <- data_srvyr %>% 
                summarise(netpusenew_grouped = srvyr::survey_mean(netpusenew_grouped,vartype="ci", na.rm=TRUE))%>%
                mutate(category_type="London (all)")%>%
                mutate(category_subtype = "London (all)")%>%
                relocate(category_type, .before=category_subtype)


# Adding London to characteristics data
output_summary <- output_summary %>% bind_rows(london)
output_summary

## c) Adding in sample sizes --------------------------------------------------

data_separate <- data %>% 
  select(wave,
         age,
         sex,
         ethn,
         hiqual,
         imd2019_qe,
         netpusenew_grouped,
         inding2_xw) 


data_category_agg <- function(data_input,
                              # category_type_name, # E.g. 'Age'
                              category_variable){ # Variable name e.g. 'age'
data_output <- 
    data_input  %>% 
    mutate(category_type = category_variable) %>%
    mutate(category_subtype = eval(parse(text=category_variable)))%>% # This allows the string e.g. 'age' to be conerted to a variable 
    filter(!is.na(netpusenew_grouped))%>%
    group_by(category_type,category_subtype) %>%
    # Calculating the total number and weight within each category (e.g. 16-25)
    # Using mutate (instead of summarise) avoids other variables being dropped
    mutate(weight_tot_cat = sum(inding2_xw),
           n_tot_cat = n()
    ) %>%
    # Getting total number and weight for each category x internet freq combination
    summarise(weight = sum(inding2_xw),
              n = n())%>%
    ungroup()
  return(data_output)
} 

# Applying the function one by one to each characteristics

data_age <- data_category_agg(data_input = data_separate,
                              # category_type_name = 'Age', 
                              category_variable = "age")


data_sex <- data_category_agg(data_input = data_separate,
                              # category_type_name = 'Sex', 
                              category_variable = "sex")

data_ethn <- data_category_agg(data_input = data_separate,
                               # category_type_name = 'Ethnicity', 
                               category_variable = "ethn")

data_hiqual <- data_category_agg(data_input = data_separate,
                                 # category_type_name = 'Highest qualification', 
                                 category_variable = "hiqual")


data_imd <- data_category_agg(data_input = data_separate,
                              # category_type_name = 'IMD', 
                              category_variable = "imd2019_qe")


data_london <- data_separate  %>% 
  mutate(category_type = "London (all)") %>%
  mutate(category_subtype = "London (all)")%>% 
  group_by(category_type,category_subtype)%>%
  mutate(weight_tot_cat = sum(inding2_xw),
         n_tot_cat = n()
  ) %>%
  # Getting total number and weight for each category x internet freq combination
  summarise(weight = sum(inding2_xw),
            n = n())%>%
  ungroup()

data_london


# Combining into one sample size datset:
data_final_n <- bind_rows(data_age,data_sex,data_ethn,data_hiqual,data_imd,data_london)


# d) Combining ci data with sample sizes --------------------------------------
output_final <- left_join(output_summary,data_final_n)


# 6. Exporting ---------------------------------------------------------------

# Update to output location desired:
output_file <- paste0("Outputs/usoc_summary_ci_wave_",wave,
                      ".xlsx")

openxlsx::write.xlsx(output_final,output_file)
