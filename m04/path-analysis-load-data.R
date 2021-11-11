
standardize <- rethinking::standardize

source("gss_example/GSS.r", chdir = TRUE)

df <- GSS %>% 
  mutate(
    educ = case_when(
      DEGREE==0 ~ 10,
      DEGREE==1 ~ 12,
      DEGREE==2 ~ 14,
      DEGREE==3 ~ 16,
      DEGREE==4 ~ 18,
      TRUE ~ NA_real_
    ),
    father_school = case_when(
      PADEG==0 ~ 10,
      PADEG==1 ~ 12,
      PADEG==2 ~ 14,
      PADEG==3 ~ 16,
      PADEG==4 ~ 18,
      TRUE ~ NA_real_
    ),
    mother_school = case_when(
      MADEG==0 ~ 10,
      MADEG==1 ~ 12,
      MADEG==2 ~ 14,
      MADEG==3 ~ 16,
      MADEG==4 ~ 18,
      TRUE ~ NA_real_
    ),
    race = case_when(
      RACE==1 ~ "white",
      RACE==2 ~ "black",
      RACE==3 ~ "other"
    ),
    sex = if_else(SEX==1, 1, 0)
  ) %>% 
  rename(income = REALINC, year=YEAR, age=AGE) %>% 
  mutate_at(vars(contains("school")), ~if_else(is.na(.x), 0, .x)) %>% 
  mutate(par_educ = map2_dbl(father_school, mother_school, max)) %>% 
  mutate(par_educ = ifelse(par_educ==0, NA, par_educ)) %>% 
  select(year, income, sex, race, educ, par_educ, age) %>% 
  filter(year > 2012) %>% 
  as_tibble()