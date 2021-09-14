nypd <- read_delim("../data/frisk_with_noise.tsv", delim=" ")
df <- nypd %>% 
  mutate(
    ethnicity = case_when(
      eth == 1 ~ "black",
      eth == 2 ~ "hispanic",
      eth == 3 ~ "white"
    ),
    crime_type = case_when(
      crime==1 ~ "violent",
      crime==2 ~ "weapons",
      crime==3 ~ "property",
      crime==4 ~ "drug",
    ),
    precinct = factor(precinct)
  ) %>% 
  rename(arrests = past.arrests) %>% 
  select(-eth, -crime)

write_csv(df, "../data/frisk_with_noise.csv")