## make visual search example data

library(tidyverse)

df_feat <- read_delim("https://search.bwh.harvard.edu/new/data_sets/RVvGV.data.10Ss.ForWeb.txt", delim="\t")
df_conj <- read_delim("https://search.bwh.harvard.edu/new/data_sets/Full%20Data%20Sets%20to%20Post/RVvRHGV.txt", delim="\t")

df_conj %>% 
  group_by(Subject) %>% 
  filter(Error==0, percent_rank(`RT(ms)`) < .99) %>%
  ggplot(aes(x=factor(Subject), y = `RT(ms)`)) +
  geom_violin()

df_feat %>% 
  filter(BlockReport=="experiment", CondReport == "RVvGV") %>% 
  filter(error==0) %>% 
  group_by(sinit,setsize, message) %>% 
  group_by(sinit) %>% 
  filter(percent_rank(RT) < .99) %>% 
  ggplot(aes(x=factor(sinit), y = `RT`)) +
  geom_violin()

df <- df_feat %>% 
  filter(BlockReport=="experiment", CondReport == "RVvGV") %>% 
  filter(error==0) %>%
  group_by(sinit) %>%
  filter(percent_rank(RT) < .99) %>%
  group_by(sinit, setsize, message) %>% 
  summarize(rt = mean(RT), se = sd(RT)/sqrt(n()-1)) %>% 
  mutate(condition = "feature") %>% 
  rename(subject=sinit) %>% 
  bind_rows(
    df_conj %>%
      filter(Condition == "RVvRHGV") %>%
      group_by(Subject) %>% 
      filter(Error==0, percent_rank(`RT(ms)`) < .99) %>%
      group_by(Subject, setsize, message) %>%
      summarize(rt = mean(`RT(ms)`), se = sd(`RT(ms)`)/sqrt(n()-1)) %>%
      rename(subject=Subject) %>% 
      mutate(condition="conjunction", subject=as.character(subject))
  ) %>% filter(subject!="rm", subject!="4")

write_csv(df, "../data/visual-search.csv")
