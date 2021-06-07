# Health




```{.r .numberLines}
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
```

I load the health section data for each year and combine it into a single tibble/dataframe for easy analysis. 

The health section was skipped in 2015's survey so I remove that filename before loading the data. 

*There are parsing errors for some columns. I find it better to just choose the required columns for the analysis with `cols_only` and manually specify types for them then manually fix encoding issues.*


```{.r .numberLines}
root_dir <- "C:/Users/R/Desktop/pslm/data_clean"
year_dirs <- head(list.dirs(root_dir, recursive = F), -1)
all_files <- map(year_dirs, function(year_dir) paste(year_dir, "3 - Health.csv", sep="/")) %>% 
  keep(file.exists)

health <- map_dfr(all_files, read_csv)
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_character(),
##   year = col_double(),
##   hhcode = col_double(),
##   startum = col_double(),
##   psu = col_double(),
##   hhno = col_double(),
##   idc = col_double(),
##   consult_times = col_double(),
##   no_consult_reason_a = col_logical(),
##   no_consult_reason_b = col_logical()
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_character(),
##   year = col_double(),
##   hhcode = col_double(),
##   psu = col_double(),
##   idc = col_double(),
##   diarrhoea_days = col_double(),
##   diarrhoea_motions = col_double(),
##   diarrhoea_ors_water_glasses = col_double(),
##   child_age_days = col_double(),
##   child_age_months = col_double(),
##   recent_immunization_day = col_double(),
##   recent_immunization_month = col_double(),
##   recent_immunization_year = col_double(),
##   first_bcg_days_after_birth = col_double(),
##   imminuzation_cost = col_double(),
##   malaria_days = col_double(),
##   malaria_treatment_else_type = col_logical()
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   year = col_double(),
##   hhcode = col_double(),
##   province = col_character(),
##   district = col_character(),
##   region = col_character(),
##   psu = col_double(),
##   hhno = col_double(),
##   idc = col_double(),
##   section = col_character(),
##   sick_last_2weeks = col_character(),
##   consult = col_character(),
##   consult_type = col_character(),
##   consult_times = col_double(),
##   consult_problems_a = col_character(),
##   consult_problems_b = col_character(),
##   no_consult_reason_a = col_character(),
##   no_consult_reason_b = col_character(),
##   unknown_question = col_character()
## )
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_character(),
##   year = col_double(),
##   hhcode = col_double(),
##   idc = col_double(),
##   diarrhoea_days = col_double(),
##   diarrhoea_motions = col_double(),
##   diarrhoea_ors_water_glasses = col_double(),
##   child_age_days = col_double(),
##   child_age_months = col_double(),
##   recent_immunization_day = col_double(),
##   recent_immunization_month = col_double(),
##   recent_immunization_year = col_double(),
##   first_bcg_days_after_birth = col_double(),
##   imminuzation_cost = col_double(),
##   polio_vac = col_double(),
##   polio_vac1 = col_double(),
##   round = col_double(),
##   psu = col_double(),
##   malaria_days = col_double(),
##   tb_consulted = col_logical(),
##   tb_consulted_type = col_logical()
##   # ... with 1 more columns
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   year = col_double(),
##   hhcode = col_double(),
##   province = col_character(),
##   district = col_character(),
##   region = col_character(),
##   psu = col_double(),
##   section = col_character(),
##   idc = col_double(),
##   sick_last_2weeks = col_character(),
##   consult = col_character(),
##   consult_type = col_character(),
##   consult_times = col_double(),
##   consult_problems_a = col_character(),
##   consult_problems_b = col_character(),
##   no_consult_reason_a = col_character(),
##   no_consult_reason_b = col_logical(),
##   lhw_visit_last_month = col_character(),
##   family_visit_hu_last_month = col_character()
## )
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   year = col_double(),
##   hhcode = col_double(),
##   province = col_character(),
##   region = col_character(),
##   psu = col_double(),
##   section = col_character(),
##   idc = col_double(),
##   sick_last_2weeks = col_character(),
##   consult = col_character(),
##   consult_type = col_character(),
##   consult_times = col_double(),
##   consult_problems_a = col_character(),
##   consult_problems_b = col_character(),
##   no_consult_reason_a = col_character(),
##   no_consult_reason_b = col_logical(),
##   lhw_visit_last_month = col_character(),
##   family_visit_hu_last_month = col_character()
## )
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_logical(),
##   year = col_double(),
##   hhcode = col_double(),
##   psu = col_double(),
##   province = col_character(),
##   region = col_character(),
##   section = col_character(),
##   idc = col_double(),
##   diarrhoea_last_month = col_character(),
##   diarrhoea_days = col_double(),
##   diarrhoea_motions = col_double(),
##   diarrhoea_still = col_character(),
##   diarrhoea_consulted = col_character(),
##   diarrhoea_consulted_type = col_character(),
##   diarrhoea_why_private = col_character(),
##   diarrhoea_why_not_govt = col_character(),
##   diarrhoea_consulted_distance = col_character(),
##   diarrhoea_consulted_else = col_character(),
##   diarrhoea_consulted_else_type = col_character(),
##   diarrhoea_ors = col_character(),
##   diarrhoea_ors_where = col_character()
##   # ... with 6 more columns
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   year = col_double(),
##   hhcode = col_double(),
##   province = col_character(),
##   region = col_character(),
##   district = col_character(),
##   psu = col_double(),
##   section = col_character(),
##   idc = col_double(),
##   sick_last_2weeks = col_character(),
##   consult = col_character(),
##   consult_type = col_character(),
##   consult_times = col_double(),
##   consult_problems_a = col_character(),
##   consult_problems_b = col_character(),
##   no_consult_reason_a = col_character(),
##   no_consult_reason_b = col_logical(),
##   lhw_visit_last_month = col_character(),
##   family_visit_hu_last_month = col_character()
## )
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_character(),
##   year = col_double(),
##   hhcode = col_double(),
##   psu = col_double(),
##   filler = col_double(),
##   idc = col_double(),
##   diarrhoea_days = col_double(),
##   diarrhoea_motions = col_double(),
##   diarrhoea_ors_water_glasses = col_double(),
##   hhcode_new = col_double(),
##   stratum = col_double(),
##   psu_new = col_double(),
##   child_age_days = col_double(),
##   child_age_months = col_double(),
##   recent_immunization_day = col_double(),
##   recent_immunization_month = col_double(),
##   recent_immunization_year = col_double(),
##   first_bcg_days_after_birth = col_double(),
##   imminuzation_cost = col_double(),
##   respondent_idc = col_double(),
##   malaria_days = col_double()
##   # ... with 1 more columns
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   year = col_double(),
##   hhcode = col_double(),
##   psu = col_double(),
##   province = col_character(),
##   region = col_character(),
##   district = col_character(),
##   hhno = col_double(),
##   section = col_character(),
##   idc = col_double(),
##   sick_last_2weeks = col_character(),
##   consult = col_character(),
##   consult_type = col_character(),
##   consult_times = col_double(),
##   consult_problems_a = col_character(),
##   consult_problems_b = col_character(),
##   no_consult_reason_a = col_logical(),
##   no_consult_reason_b = col_logical(),
##   lhw_visit_last_month = col_character(),
##   family_visit_hu_last_month = col_character()
## )
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_character(),
##   year = col_double(),
##   hhcode = col_double(),
##   psu = col_double(),
##   idc = col_double(),
##   diarrhoea_days = col_double(),
##   diarrhoea_motions = col_double(),
##   diarrhoea_consulted_else_type = col_logical(),
##   diarrhoea_ors_water_glasses = col_double(),
##   child_age_days = col_double(),
##   child_age_months = col_double(),
##   recent_immunization_day = col_double(),
##   recent_immunization_month = col_double(),
##   recent_immunization_year = col_double(),
##   first_bcg_days_after_birth = col_double(),
##   imminuzation_cost = col_double(),
##   respondent_idc = col_double(),
##   malaria_days = col_double(),
##   hepatitis_precautions_what = col_logical()
## )
## i Use `spec()` for the full column specifications.
```

Each individual in the survey is uniquely identified by a triplet of (year, hhcode, idc) where `year` is the year of survey, `hhcode` is household code/ID and `idc` is person ID *within the household*. 


```{.r .numberLines}
head(health)
```

```
## # A tibble: 6 x 110
##    year     hhcode province district  region startum     psu  hhno section   idc
##   <dbl>      <dbl> <chr>    <chr>     <chr>    <dbl>   <dbl> <dbl> <chr>   <dbl>
## 1  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 D           1
## 2  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 D           2
## 3  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 D           3
## 4  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 D           4
## 5  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 D           5
## 6  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 D           6
## # ... with 100 more variables: sick_last_2weeks <chr>, consult <chr>,
## #   consult_type <chr>, consult_times <dbl>, consult_problems_a <chr>,
## #   consult_problems_b <chr>, no_consult_reason_a <chr>,
## #   no_consult_reason_b <chr>, lhw_visit_last_month <chr>,
## #   family_visit_hu_last_month <chr>, diarrhoea_last_month <chr>,
## #   diarrhoea_days <dbl>, diarrhoea_motions <dbl>, diarrhoea_still <chr>,
## #   diarrhoea_consulted <chr>, diarrhoea_consulted_type <chr>,
## #   diarrhoea_why_private <chr>, diarrhoea_why_not_govt <chr>,
## #   diarrhoea_consulted_distance <chr>, diarrhoea_consulted_else <chr>,
## #   diarrhoea_consulted_else_type <chr>, diarrhoea_ors <chr>,
## #   diarrhoea_ors_where <chr>, diarrhoea_ors_water_glasses <dbl>,
## #   diarrhoea_ors_water_type <chr>, diarrhoea_breastfeed <chr>,
## #   diarrhoea_food_liquid <chr>, diarrhoea_food_solid <chr>,
## #   diarrhoea_food_water <chr>, child_age_days <dbl>, child_age_months <dbl>,
## #   immunized_ever <chr>, immunization_card <chr>, immunization_bcg <chr>,
## #   immunization_dpt_1 <chr>, immunization_dpt_2 <chr>,
## #   immunization_dpt_3 <chr>, immunization_polio_0 <chr>,
## #   immunization_polio_1 <chr>, immunization_polio_2 <chr>,
## #   immunization_polio_3 <chr>, immunization_measles <chr>,
## #   immunization_hepatitis_1 <chr>, immunization_hepatitis_2 <chr>,
## #   immunization_hepatitis_3 <chr>, recent_immunization_where <chr>,
## #   recent_immunization_day <dbl>, recent_immunization_month <dbl>,
## #   recent_immunization_year <dbl>, first_bcg_days_after_birth <dbl>,
## #   disease_inspite_a <chr>, disease_inspite_b <chr>, disease_inspite_c <chr>,
## #   imminuzation_distance <chr>, imminuzation_cost <dbl>,
## #   why_no_immunization <chr>, malaria_last_month <chr>, malaria_days <dbl>,
## #   malaria_still_sick <chr>, malaria_treatment <chr>,
## #   malaria_treatment_type <chr>, malaria_treatment_why_private <chr>,
## #   malaria_treatment_why_not_govt <chr>, malaria_treatment_distance <chr>,
## #   malaria_treatment_else <chr>, malaria_treatment_else_type <chr>,
## #   tb_ever <chr>, tb_consulted <chr>, tb_consulted_type <chr>,
## #   tb_consulted_distance <chr>, unknown_question <chr>, polio_vac <dbl>,
## #   polio_vac1 <dbl>, round <dbl>, tb_last_month <chr>, filler <dbl>,
## #   hhcode_new <dbl>, stratum <dbl>, psu_new <dbl>, immunization_penta_1 <chr>,
## #   immunization_penta_2 <chr>, immunization_penta_3 <chr>,
## #   immunization_pneumo_1 <chr>, immunization_pneumo_2 <chr>,
## #   immunization_pneumo_3 <chr>, immunization_ipv <chr>,
## #   immunization_measles_1 <chr>, immunization_measles_2 <chr>,
## #   respondent_idc <dbl>, diarrhoea_last_15days <chr>, malaria_last_year <chr>,
## #   malaria_treatment_when <chr>, malaria_symptoms <chr>,
## #   hepatitis_last_year <chr>, hepatitis_last_year_type <chr>,
## #   hepatitis_precautions <chr>, hepatitis_precautions_what <lgl>,
## #   tb_last_year <chr>, tb_know_spread <chr>, tb_know_cure <chr>
```

Since the survey questionnaire changes across years and some questions are not asked there are some columns with significant variations in the available data. This is before considering that even if a question is asked the respondent might not provide/know the answer. 


```{.r .numberLines}
health %>% 
  group_by(year) %>% 
  summarise_all(~ mean(!is.na(.)))
```

```
## # A tibble: 11 x 110
##     year hhcode province district region startum   psu  hhno section   idc
##    <dbl>  <dbl>    <dbl>    <dbl>  <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>
##  1  2004      1        1        1      1       1 1         1   1      1   
##  2  2005      1        1        0      1       0 1         0   0      1   
##  3  2006      1        1        1      1       0 1         1   1      1   
##  4  2007      1        1        0      1       0 0.998     0   0.999  1   
##  5  2008      1        1        1      1       0 1         0   1      1   
##  6  2010      1        1        0      1       0 1         0   1      1   
##  7  2011      1        1        0      1       0 1         0   1      1   
##  8  2012      1        1        1      1       0 1         0   1      1   
##  9  2013      1        1        0      1       0 1         0   0      1   
## 10  2014      1        1        1      1       0 1         1   1      1   
## 11  2018      1        1        0      1       0 1         0   0      1.00
## # ... with 100 more variables: sick_last_2weeks <dbl>, consult <dbl>,
## #   consult_type <dbl>, consult_times <dbl>, consult_problems_a <dbl>,
## #   consult_problems_b <dbl>, no_consult_reason_a <dbl>,
## #   no_consult_reason_b <dbl>, lhw_visit_last_month <dbl>,
## #   family_visit_hu_last_month <dbl>, diarrhoea_last_month <dbl>,
## #   diarrhoea_days <dbl>, diarrhoea_motions <dbl>, diarrhoea_still <dbl>,
## #   diarrhoea_consulted <dbl>, diarrhoea_consulted_type <dbl>,
## #   diarrhoea_why_private <dbl>, diarrhoea_why_not_govt <dbl>,
## #   diarrhoea_consulted_distance <dbl>, diarrhoea_consulted_else <dbl>,
## #   diarrhoea_consulted_else_type <dbl>, diarrhoea_ors <dbl>,
## #   diarrhoea_ors_where <dbl>, diarrhoea_ors_water_glasses <dbl>,
## #   diarrhoea_ors_water_type <dbl>, diarrhoea_breastfeed <dbl>,
## #   diarrhoea_food_liquid <dbl>, diarrhoea_food_solid <dbl>,
## #   diarrhoea_food_water <dbl>, child_age_days <dbl>, child_age_months <dbl>,
## #   immunized_ever <dbl>, immunization_card <dbl>, immunization_bcg <dbl>,
## #   immunization_dpt_1 <dbl>, immunization_dpt_2 <dbl>,
## #   immunization_dpt_3 <dbl>, immunization_polio_0 <dbl>,
## #   immunization_polio_1 <dbl>, immunization_polio_2 <dbl>,
## #   immunization_polio_3 <dbl>, immunization_measles <dbl>,
## #   immunization_hepatitis_1 <dbl>, immunization_hepatitis_2 <dbl>,
## #   immunization_hepatitis_3 <dbl>, recent_immunization_where <dbl>,
## #   recent_immunization_day <dbl>, recent_immunization_month <dbl>,
## #   recent_immunization_year <dbl>, first_bcg_days_after_birth <dbl>,
## #   disease_inspite_a <dbl>, disease_inspite_b <dbl>, disease_inspite_c <dbl>,
## #   imminuzation_distance <dbl>, imminuzation_cost <dbl>,
## #   why_no_immunization <dbl>, malaria_last_month <dbl>, malaria_days <dbl>,
## #   malaria_still_sick <dbl>, malaria_treatment <dbl>,
## #   malaria_treatment_type <dbl>, malaria_treatment_why_private <dbl>,
## #   malaria_treatment_why_not_govt <dbl>, malaria_treatment_distance <dbl>,
## #   malaria_treatment_else <dbl>, malaria_treatment_else_type <dbl>,
## #   tb_ever <dbl>, tb_consulted <dbl>, tb_consulted_type <dbl>,
## #   tb_consulted_distance <dbl>, unknown_question <dbl>, polio_vac <dbl>,
## #   polio_vac1 <dbl>, round <dbl>, tb_last_month <dbl>, filler <dbl>,
## #   hhcode_new <dbl>, stratum <dbl>, psu_new <dbl>, immunization_penta_1 <dbl>,
## #   immunization_penta_2 <dbl>, immunization_penta_3 <dbl>,
## #   immunization_pneumo_1 <dbl>, immunization_pneumo_2 <dbl>,
## #   immunization_pneumo_3 <dbl>, immunization_ipv <dbl>,
## #   immunization_measles_1 <dbl>, immunization_measles_2 <dbl>,
## #   respondent_idc <dbl>, diarrhoea_last_15days <dbl>, malaria_last_year <dbl>,
## #   malaria_treatment_when <dbl>, malaria_symptoms <dbl>,
## #   hepatitis_last_year <dbl>, hepatitis_last_year_type <dbl>,
## #   hepatitis_precautions <dbl>, hepatitis_precautions_what <dbl>,
## #   tb_last_year <dbl>, tb_know_spread <dbl>, tb_know_cure <dbl>
```


