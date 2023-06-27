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