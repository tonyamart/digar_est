library(tidyverse)
library(vistime)
library(lubridate)
library(data.table)
library(wesanderson)

# metadata: all_issues() metadata output
dat <- read.csv("data/all_issues_meta.csv")

glimpse(dat)

#### data for timeline viz ####

# clean newspaper titles : select newspapers with same keyid but different full titles

titles <- dat %>% select(keyid, title) %>% 
  distinct()
length(unique(titles$keyid))
duplicated(titles$title)

x <- titles %>% 
  count(keyid, sort = T) %>% 
  filter(n > 1) %>% 
  left_join(titles, by = "keyid") %>% 
  select(-n) %>% 
  group_by(keyid) %>% 
  group_split()


# assign the first key-title pair out of many to each newspaper

y <- list()

for (i in 1:length(x)) {
  y[i][[1]] <- x[i][[1]][1, 1:2]
}

y <- as.tibble(data.table::rbindlist(y)) %>% 
  rename(title_cln = title)

dat_upd <- dat %>% 
  left_join(y, by = "keyid") %>% 
  mutate(title_cln = ifelse(keyid %in% y$keyid, title_cln, title))

rm(x, y, titles)

#### create data for timeline

glimpse(dat_upd)

d_short <- dat_upd %>% 
  select(keyid, title_cln, date, language, country)

glimpse(d_short)

d_timeline <- d_short %>%  
  mutate(title_cln = str_replace_all(title_cln, '\"+', '"')) %>% 
  mutate(dates = ymd(date)) %>% 
  group_by(keyid) %>% 
  summarise(title_cln = title_cln,
            start = min(dates), 
            end = max(dates), 
            language = language,
            country = country) %>% 
  #filter(start != end) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(duration = as.numeric(difftime(end, start, units = "days")/365)) %>% 
  mutate(start = as.character(start),
         end = as.character(end),
         tooltip = paste0(
           title_cln, "<br>From <b>", start, "</b> to <b>", end, 
           "</b><br>issued in <b>", country, "</b> in <b>", 
           language, "</b> language</br>"))

longest <- d_timeline %>% 
  slice_max(order_by = duration, n = 5)

glimpse(d_timeline)
glimpse(longest)

longest_np <- as.vector(longest$keyid)


#### language selection & colors ####

unique(d_timeline$language) # 9

languages_colors <- tibble(language = unique(dat$language), 
                           l_color = c("#899DA4", "#78B7C5", "#C7B19C", "#446455", "#0B775E", "#3B9AB2",
                                       "#F8AFA8", "#74A089", "#FDD262"))

# attach colours to a separate column
d_timeline <- d_timeline %>% 
  left_join(languages_colors, by = "language")

# 1 grey -- #899DA4 estonian
# 2 light blue -- #78B7C5 english
# 3 light brown -- #C7B19C russian
# 4 green1 -- #446455 german
# 5 blue -- #3B9AB2 - finnish
# 6 green2 -- #0B775E latin
# 7 pink -- #F8AFA8 french
# 8 mint -- #74A089 dutch
# 9 yellow -- #FDD262 swedish

#### attach clean titles & timeline data to issues data #### 

glimpse(d_timeline)

dat_upd <- dat %>% 
  left_join(d_timeline %>% select(-language, -country), by = "keyid")

glimpse(dat_upd)

# summarise issues information for app

issues <- dat_upd %>% 
  # clean section column
  separate_rows(section, sep = ";") %>% 
  filter(section != "") %>% 
  mutate(section = str_remove_all(section, "^\\s+|\\s+$")) %>% 
  mutate(section = str_replace_all(section, "newspaperr", "newspaper")) %>% 
  mutate(title_cln = ifelse(str_detect(title_cln, "Ühistegelised Uudised"), "Ühistegelised Uudised", title_cln)) %>% 
  # group issues info by year
  group_by(year, keyid) %>%
  summarise(DocumentType = DocumentType,
            title = title_cln,
            language = language,
            country = country,
            section = section,
            n = n(),
            
            # timeline data
            start = start,
            end = end,
            duration = duration,
            tooltip = tooltip,
            l_color = l_color,
            
            # data for seach_title_table
            access = access,
            place = place,
            publisher = publisher,
            section = section,
            pages_exist,
            sections_exist,
            permalink = permalink,
            ester_id = ester_id
            ) %>%
  ungroup() %>%
  distinct()

glimpse(issues)

write.csv(issues, "~/Documents/ds/digar/apps/issues_summary.csv")
