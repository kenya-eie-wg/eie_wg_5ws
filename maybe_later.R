eie_5ws %>% 
  mutate(sub_counties = ifelse(str_detect(sub_county, "\\,|All|all"), 
                               "mixed", "correct")) %>% 
  filter(sub_counties == "mixed") %>% 
  count(implementing_partners)

eie_5ws %>% 
  filter(implementing_partners == "Save the Children") %>% 
  mutate(sub_counties = ifelse(str_detect(sub_county, "\\,|All|all"), 
                               "mixed", "correct")) %>% 
  count(sub_counties, wt = total_reached)

eie_5ws %>% count(implementing_partners, sort = TRUE)

eie_5ws %>% filter(county == "Wajir") %>% count(implementing_partners, sub_county, sort = TRUE)

eie_5ws %>% 
  filter(implementing_partners == "Save the Children" & county == "Wajir" & 
           str_detect(indicator, "1")) %>% 
  group_by(month, lead_organisation, sub_county) %>% 
  summarise(reached = total_reached) %>% 
  flextable() %>% 
  theme_zebra() %>% 
  set_caption("Extract from 5Ws -- Save the Children, Indicator 1")

eie_5ws %>% 
  count(sub_county, 
        sort = TRUE)

library(flextable)
eie %>% 
  mutate(sub_counties = ifelse(str_detect(sub_county, "\\,|All|all") | is.na(sub_county), 
                               "mixed", "correct")) %>% 
  group_by(sub_counties) %>% 
  summarise(rows = n(), 
            beneficiaries = sum(total_reached, na.rm = TRUE)) %>% 
  mutate(percent = round(beneficiaries / sum(beneficiaries, na.rm = TRUE) * 100, digits = 2)) %>% 
  flextable() %>% 
  theme_zebra() %>% 
  set_table_properties(layout = "autofit", width = .8)

eie %>% 
  mutate(sub_counties = ifelse(str_detect(sub_county, "\\,|All|all"), 
                               "mixed", "correct")) %>% 
  filter(sub_counties == "mixed") %>% 
  group_by(lead_organisation, sub_county, month) %>% 
  filter(lead_organisation == "Save the Children") %>% 
  summarise(beneficiaries = sum(total_reached)) %>% 
  flextable() %>% 
  theme_zebra() %>% 
  set_table_properties(layout = "autofit", width = .99)

sitrep %>% 
  select(-contains("x")) %>%
  pivot_longer(cols = indicator1_all_total:indicator7_narrative, 
               names_to = "indicator", 
               values_to = "value") %>% 
  mutate(sex_modifier = case_when(str_detect(indicator, "girls|women|Women|Girls") ~ "female", 
                                  str_detect(indicator, "boys|Boys|men|Men") ~ "male", 
                                  str_detect(indicator, "sex_modifier_total") ~ "total"), 
         beneficiary_group = case_when(str_detect(indicator, "host_community") ~ "host_community", 
                                       str_detect(indicator, "idps") ~ "idps", 
                                       str_detect(indicator, "refugees") ~ "refugees",
                                       str_detect(indicator, "all") ~ "all_beneficiary_type_total"), 
         age_modifier = case_when(str_detect(indicator, "boys|girls") ~ "children", 
                                  str_detect(indicator, "men|women") ~ "adults", 
                                  str_detect(indicator, "all") ~ "age_modifier_total")) %>% 
  naniar::replace_with_na(replace = list(value = 0)) %>% 
  filter(!is.na(value)) %>% 
  mutate(indicator_number = as.factor(parse_number(indicator)), 
         indicator_short = case_when(str_detect(indicator, "1") ~ 
                                       "1. OOSC accessing formal education", 
                                     str_detect(indicator, "2") ~ 
                                       "2. Children benefiting from child-friendly environment", 
                                     str_detect(indicator, "3") ~ 
                                       "3. Children who receive assistance to continue learning", 
                                     str_detect(indicator, "4") ~ 
                                       "4. Teacheres trained", 
                                     str_detect(indicator, "5") ~ 
                                       "5. BoM trained", 
                                     str_detect(indicator, "6") ~ 
                                       "6. Govt. officials trained", 
                                     str_detect(indicator, "7") ~ 
                                       "7. Children life skills mentorship")) %>%
  distinct(month, county, indicator, value)
transpose_df()
pivot_longer(cols = month, names_to = "month", values_to = "month")
mutate(indicator = case_when(str_detect()))
glimpse()

read_excel("./data/unicef_internal/Monthly Education SitRep_Jun23_mohamed.xlsx") %>% 
  setNames(messy) %>% 
  slice(10:284) %>% 
  mutate(county = ifelse(str_detect(month, "\\."), 
                         month, 
                         NA_character_), 
         county = gsub("[[:punct:]]|[[:digit:]]", "", county), 
         county = case_when(
           str_detect(county, "Garissa") ~ "Garissa", 
           str_detect(county, "Turkana") ~ "Turkana",
           str_detect(county, "Homa") ~ "Homa Bay", 
           TRUE ~ county)) %>% 
  fill(county) %>% 
  filter(month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) %>% 
  select(-contains("x"), -county, 
         -matches("narrative")) %>%
  group_by(month) %>%
  mutate_at(vars(indicator1_all_total:indicator7_cwd_girls), ~as.numeric(.)) %>% 
  summarise_at(vars(indicator1_all_total:indicator7_cwd_girls), ~ sum(.x, na.rm = TRUE)) %>% 
  mutate(month = fct_relevel(month, c("Jan", "Feb", "Mar","Apr", "May", "Jun"))) %>% 
  arrange(month) %>% 
  write_csv("./data/flat_table_sub_indicator_summary_mohamed.csv")

sitrep %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!str_detect(sub_indicator, "all|total|narrative")) %>% 
  group_by(sub_indicator, month) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  write_csv("./data/sub_indicator_summary_month_mohamed.csv")


sitrep %>% 
  mutate(value = as.numeric(value)) %>%   
  write_csv("./data/sitrep_indicators.csv")

read_excel("./data/unicef_internal/Monthly Education SitRep_Jun23_mohamed.xlsx") %>% 
  setNames(messy) %>% 
  slice(10:284) %>% 
  mutate(county = ifelse(str_detect(month, "\\."), 
                         month, 
                         NA_character_), 
         county = gsub("[[:punct:]]|[[:digit:]]", "", county), 
         county = case_when(
           str_detect(county, "Garissa") ~ "Garissa", 
           str_detect(county, "Turkana") ~ "Turkana",
           str_detect(county, "Homa") ~ "Homa Bay", 
           TRUE ~ county)) %>% 
  fill(county) %>% 
  filter(month %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun")) %>% 
  select(-contains("x")) %>% 
  relocate(county, .before = month) %>% 
  write_csv("./data/flat_table_mohamed.csv")

combined %>% 
  filter(month %in% c("June")) %>% 
  group_by(unicef_indicator) %>% 
  summarise(girls = sum(value[str_detect(sub_indicator, "girls")], na.rm = TRUE), 
            boys = sum(value[str_detect(sub_indicator, "boys")], na.rm = TRUE), 
            men = sum(value[str_detect(sub_indicator, "men")], na.rm = TRUE), 
            women = sum(value[str_detect(sub_indicator, "women")], na.rm = TRUE), 
            counties = n_distinct(county)) %>% 
  mutate_at(vars(girls, boys, men, women), ~ replace_na(., 0)) %>% 
  mutate(total = girls + boys + men + women)

combined %>% count(unicef_indicator_number)

combined %>% 
  mutate(value = pmax(mohamed_value, beth_value, osman_value))

combined %>% 
  filter(sex_modifier == "female") %>% 
  {sum(.$value, na.rm = TRUE)}

eie %>% 
  filter(indicator_short %in% c("1. Access ECD spaces/schools") & 
           lead_organisation == "UNICEF") %>% 
  {sum(.$total_reached)}

combined %>% 
  filter(month %in% c("June")) %>% 
  filter(str_detect(unicef_indicator, "1") & sex_modifier == "total") %>% 
  {sum(.$value)}

eie %>% 
  filter(str_detect(indicator, "1") & lead_organisation == "UNICEF") %>% 
  {sum(.$total_reached)}

eie %>% count(indicator)


partner_means <- eie %>% 
  filter(activity_status == "Completed") %>% 
  mutate(implementing_partner = str_sub(implementing_partner, end = 50L)) %>% 
  group_by(implementing_partner) %>% 
  summarise(reached = sum(total_reached, na.rm = TRUE), 
            counties = n_distinct(adm1_pcode), 
            activities = n_distinct(indicator_short)) %>% 
  filter(reached > 0) %>%
  mutate(group = case_when(reached < 1000 ~ "small", 
                           reached >= 1000 & reached < 10000 ~ "medium", 
                           reached >= 10000 ~ "large")) %>%
  group_by(group) %>% 
  summarise(reached = mean(reached, na.rm = TRUE))

eie %>% 
  filter(activity_status == "Completed") %>% 
  mutate(implementing_partner = str_sub(implementing_partner, end = 50L)) %>% 
  group_by(implementing_partner) %>% 
  summarise(reached = sum(total_reached, na.rm = TRUE), 
            counties = n_distinct(adm1_pcode), 
            activities = n_distinct(indicator_short)) %>% 
  filter(reached > 0) %>%
  mutate(group = case_when(reached < 1000 ~ "small", 
                           reached >= 1000 & reached < 10000 ~ "medium", 
                           reached >= 10000 ~ "large")) %>% 
  ggplot(aes(x = reached, y = group, fill = group)) +
  geom_boxplot(alpha = .5, 
               outlier.alpha = 0) + 
  geom_jitter() + 
  geom_vline(data = partner_means, 
             aes(xintercept = reached), 
             colour = "#31688e", lty = 2) + 
  scale_fill_manual(values = c("#90d743", "#35b779", "#31688e")) +
  scale_x_log10()

eie %>% 
  group_by(indicator_short, county) %>% 
  summarise(beneficiaries = sum(total_reached, na.rm = TRUE)) %>% 
  full_join(targets %>% 
              filter(target_unit %in% c("people")) %>% 
              select(county, indicator_short, county_target), 
            by = c("county", "indicator_short")) %>% 
  replace_na(list(county_target = 0, 
                  beneficiaries = 0)) %>% 
  filter(beneficiaries > 0) %>% 
  group_by(indicator_short) %>% 
  summarise(beneficiaries = sum(beneficiaries), 
            target = sum(county_target)) %>% 
  mutate(pc = ifelse(target > 0, beneficiaries / target, 0)) 
