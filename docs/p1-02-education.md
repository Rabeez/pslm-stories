# Education




```{.r .numberLines}
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
```

I load the education section data for each year and combine it into a single tibble/dataframe for easy analysis. 

*There are parsing errors for some columns. I find it better to just choose the required columns for the analysis with `cols_only` and manually specify types for them then manually fix encoding issues.*


```{.r .numberLines}
root_dir <- "C:/Users/R/Desktop/pslm/data_clean"
year_dirs <- head(list.dirs(root_dir, recursive = F), -1)
all_files <- map(year_dirs, function(year_dir) paste(year_dir, "2 - Education.csv", sep="/"))

education <- map_dfr(all_files, read_csv)
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
##   idc = col_double()
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
##   years_for_primary = col_double(),
##   start_school_age = col_double(),
##   years_for_primary2 = col_double(),
##   cost_fees_exams = col_double(),
##   cost_uniform_books_etc = col_double(),
##   cost_other = col_double()
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
##   hhno = col_double(),
##   idc = col_double(),
##   reason_for_no_school_r2 = col_logical()
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   year = col_double(),
##   hhcode = col_double(),
##   section = col_character(),
##   idc = col_double(),
##   educational_background = col_character(),
##   highest_level = col_character(),
##   school_in_last_year = col_character(),
##   grade_last_year = col_character(),
##   completed_grade_last_year = col_character(),
##   current_level = col_character(),
##   school_in_last_year2 = col_character(),
##   grade_last_year2 = col_character(),
##   province = col_character(),
##   region = col_character(),
##   years_education = col_double()
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
##   school_ever = col_character(),
##   highest_level = col_character(),
##   school_currently = col_character(),
##   current_level = col_character(),
##   years_education = col_double()
## )
## 
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
##   school_ever = col_character(),
##   highest_level = col_character(),
##   school_currently = col_character(),
##   current_level = col_character(),
##   years_education = col_double()
## )
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
##   idc = col_double(),
##   educational_background = col_character(),
##   highest_level = col_character(),
##   school_in_last_year = col_character(),
##   grade_last_year = col_character(),
##   completed_grade_last_year = col_character(),
##   current_level = col_character(),
##   school_in_last_year2 = col_character(),
##   grade_last_year2 = col_character(),
##   years_education = col_double()
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
##   district = col_character(),
##   section = col_character(),
##   idc = col_double(),
##   can_read_write = col_character(),
##   can_math = col_character(),
##   school_ever = col_character(),
##   highest_level = col_character(),
##   school_currently = col_character(),
##   current_level = col_character(),
##   current_school_type = col_character(),
##   school_problems_r1 = col_character(),
##   school_problems_r2 = col_logical(),
##   reason_for_no_school_r1 = col_character(),
##   reason_for_no_school_r2 = col_character()
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
##   years_for_primary = col_double(),
##   start_school_age = col_double(),
##   years_for_primary2 = col_double(),
##   cost_fees = col_double(),
##   cost_uniform = col_double(),
##   cost_books = col_double(),
##   cost_exams = col_double(),
##   cost_private_tuition = col_double(),
##   cost_transport = col_double(),
##   cost_hostel = col_double(),
##   cost_other = col_double(),
##   cost_total = col_double(),
##   hhcode_new = col_double(),
##   stratum = col_double(),
##   psu_new = col_double()
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
##   idc = col_double()
## )
## i Use `spec()` for the full column specifications.
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   .default = col_double(),
##   province = col_character(),
##   region = col_character(),
##   section = col_character(),
##   can_read = col_character(),
##   can_write = col_character(),
##   can_math = col_character(),
##   school_ever = col_character(),
##   highest_level = col_character(),
##   school_currently = col_character(),
##   current_level = col_character(),
##   current_school_type = col_character()
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
##   years_for_primary = col_double(),
##   start_school_age = col_double(),
##   years_for_primary2 = col_double(),
##   cost_fees = col_double(),
##   cost_uniform = col_double(),
##   cost_books = col_double(),
##   cost_exams = col_double(),
##   cost_private_tuition = col_double(),
##   cost_transport = col_double(),
##   cost_hostel = col_double(),
##   cost_other = col_double(),
##   cost_total = col_double()
## )
## i Use `spec()` for the full column specifications.
```

Each individual in the survey is uniquely identified by a triplet of (year, hhcode, idc) where `year` is the year of survey, `hhcode` is household code/ID and `idc` is person ID *within the household*. 


```{.r .numberLines}
head(education)
```

```
## # A tibble: 6 x 56
##    year     hhcode province district  region startum     psu  hhno section   idc
##   <dbl>      <dbl> <chr>    <chr>     <chr>    <dbl>   <dbl> <dbl> <chr>   <dbl>
## 1  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 C           1
## 2  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 C           2
## 3  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 C           3
## 4  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 C           4
## 5  2004 1001100101 punjab   islamabad urban        1  1.00e7     1 C           5
## 6  2004 1001100102 punjab   islamabad urban        1  1.00e7     2 C           1
## # ... with 46 more variables: can_read_write <chr>, can_math <chr>,
## #   school_ever <chr>, highest_level <chr>, school_currently <chr>,
## #   current_level <chr>, current_school_type <chr>, school_problems_r1 <chr>,
## #   school_problems_r2 <chr>, reason_for_no_school_r1 <chr>,
## #   reason_for_no_school_r2 <chr>, can_read <chr>, can_write <chr>,
## #   educational_background <chr>, reason_for_no_school <chr>,
## #   last_school_type <chr>, reason_for_last_school <chr>,
## #   years_for_primary <dbl>, school_in_last_year <chr>, grade_last_year <chr>,
## #   completed_grade_last_year <chr>, reason_for_leaving <chr>,
## #   reason_for_current_school <chr>, start_school_age <dbl>,
## #   school_in_last_year2 <chr>, grade_last_year2 <chr>,
## #   years_for_primary2 <dbl>, school_distance <chr>, cost_fees_exams <dbl>,
## #   cost_uniform_books_etc <dbl>, cost_other <dbl>, years_education <dbl>,
## #   cost_fees <dbl>, cost_uniform <dbl>, cost_books <dbl>, cost_exams <dbl>,
## #   cost_private_tuition <dbl>, cost_transport <dbl>, cost_hostel <dbl>,
## #   cost_total <dbl>, filter__ <chr>, hhcode_new <dbl>, stratum <dbl>,
## #   psu_new <dbl>, hh <chr>, vocational_training <chr>
```

Since the survey questionnaire changes across years and some questions are not asked there are some columns with significant variations in the available data. This is before considering that even if a question is asked the respondent might not provide/know the answer. 

This section is very egregious in terms of changing the survey questions (look at the literacy questions `can_read`/`can_write` etc). 


```{.r .numberLines}
education %>% 
  group_by(year) %>% 
  summarise_all(~ mean(!is.na(.)))
```

```
## # A tibble: 12 x 56
##     year hhcode province district region startum   psu  hhno section   idc
##    <dbl>  <dbl>    <dbl>    <dbl>  <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>
##  1  2004      1        1        1      1       1     1     1       1     1
##  2  2005      1        1        0      1       0     1     0       0     1
##  3  2006      1        1        1      1       0     1     1       1     1
##  4  2007      1        1        0      1       0     0     0       1     1
##  5  2008      1        1        0      1       0     1     0       1     1
##  6  2010      1        1        0      1       0     1     0       1     1
##  7  2011      1        1        0      1       0     1     0       0     1
##  8  2012      1        1        1      1       0     0     0       1     1
##  9  2013      1        1        0      1       0     1     0       0     1
## 10  2014      1        1        1      1       0     1     0       1     1
## 11  2015      1        1        0      1       0     1     0       1     1
## 12  2018      1        1        0      1       0     1     0       0     1
## # ... with 46 more variables: can_read_write <dbl>, can_math <dbl>,
## #   school_ever <dbl>, highest_level <dbl>, school_currently <dbl>,
## #   current_level <dbl>, current_school_type <dbl>, school_problems_r1 <dbl>,
## #   school_problems_r2 <dbl>, reason_for_no_school_r1 <dbl>,
## #   reason_for_no_school_r2 <dbl>, can_read <dbl>, can_write <dbl>,
## #   educational_background <dbl>, reason_for_no_school <dbl>,
## #   last_school_type <dbl>, reason_for_last_school <dbl>,
## #   years_for_primary <dbl>, school_in_last_year <dbl>, grade_last_year <dbl>,
## #   completed_grade_last_year <dbl>, reason_for_leaving <dbl>,
## #   reason_for_current_school <dbl>, start_school_age <dbl>,
## #   school_in_last_year2 <dbl>, grade_last_year2 <dbl>,
## #   years_for_primary2 <dbl>, school_distance <dbl>, cost_fees_exams <dbl>,
## #   cost_uniform_books_etc <dbl>, cost_other <dbl>, years_education <dbl>,
## #   cost_fees <dbl>, cost_uniform <dbl>, cost_books <dbl>, cost_exams <dbl>,
## #   cost_private_tuition <dbl>, cost_transport <dbl>, cost_hostel <dbl>,
## #   cost_total <dbl>, filter__ <dbl>, hhcode_new <dbl>, stratum <dbl>,
## #   psu_new <dbl>, hh <dbl>, vocational_training <dbl>
```


