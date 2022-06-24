library(tidyverse)
library(tidytext)

library(wesanderson)
library(pals)
theme_set(theme_minimal())

setwd("Documents/est_np/")

# all meta
meta <- read.csv("data/meta_rus_lang.csv")

str(meta)

meta_1880 <- meta %>% 
  filter(year.x < 1891 & year.x > 1879)

meta_1917 <- meta %>% 
  filter(year.x == 1917)

#### 1880s bilingual set ####
beta <- read.csv("data/models/beta_1880_20.csv")
gamma <- read.csv("data/models/gamma_1880_20.csv")

glimpse(beta)
glimpse(gamma)

#### beta ####
top_beta <- beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) 

top_beta %>%
  ggplot(aes(x = beta, y = reorder_within(term, beta, topic), 
             fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  scale_y_reordered() + 
  facet_wrap(~ topic, scales = "free")

# 7th, 12th, 14th, 17th, 19th etc.
top_beta_1880 %>% 
  filter(topic == 19)

#### gamma ####

gamma <- gamma %>% 
  mutate(title = str_remove_all(document, "\\d+|\\W")) %>% 
  mutate(year = str_replace_all(document, 
                                "\\w+(\\d{4})(\\d{2})(\\d{2})\\..*", "\\1")) %>% 
  mutate(month = str_replace_all(document, 
                                 "\\w+(\\d{4})(\\d{2})(\\d{2})\\..*", "\\2"))

gamma_summary <- gamma %>% 
  group_by(title, year, topic) %>% 
  summarise(gamma_avg = mean(gamma))

gamma_years <- gamma %>% 
  group_by(year, topic) %>% 
  summarise(gamma_avg = mean(gamma))

glimpse(gamma_summary)
glimpse(gamma_years)


#### gamma by years: german and russian sets ####
gamma_years %>% 
  ggplot(aes(x = year, y = gamma_avg, fill = factor(topic))) + 
  geom_col() + 
  scale_fill_manual(values = cols25(n = 20))
# two different sets of topics before / after 1885 ( germ / rus )

top_gamma_years <- gamma_years %>% 
  group_by(year) %>% 
  slice_max(gamma_avg, n = 5)

# Prevail of Russian texts starts roughly in 1886
top_gamma_years %>% 
  ungroup() %>% 
  filter(year < "1886") %>% 
  count(topic, sort = T)

# top topics before 1886: 16? 17, 14, 7, 6  
  
# 16th top words: руб, сумма, лес, торг, судно
# 17 : german: Riga, gouvernements, stadt, quartier, appertinentien, gehorig, gewesene, immobil
# 3 : суд, срок, волостной
# 14 : ger : forderungen, anspruche gebauden appertinentien einwendungen kerisgericht
# 7 : rigaschen hierdurch gericht anspruche riga hypotheken landcogteigerichts obengenannten aufgefordert
# 6 : городской (only prob), руб управа

# top topics starting from 1886: 16, 3, 10, 11, 8
# 10 = губернский лифляндский лицо случай рижский производить розыск полицейский магистрат
# 11 продажа публичный суд судно предлагать высокий ипотечный
# 8 русский общество церковь общий язык дорога комитет православный край школа

#### gamma for sources : est vs liv ####

glimpse(gamma_summary)

gamma_summary %>% 
  ggplot(aes(x = year, y = gamma_avg, fill = factor(topic))) + geom_col() + 
  facet_wrap(~title, scales = "free", ncol = 1) + 
  scale_fill_manual(values = cols25(n = 20))

gamma_summary %>% 
  ungroup() %>% 
  group_by(title, year) %>% 
  slice_max(gamma_avg, n = 5) %>% 
  #filter(year < "1886") %>% 
  filter(year > "1885") %>% 
  group_by(title) %>% 
  count(topic, sort = T) %>% 
  ungroup() %>% 
  distinct(topic, .keep_all = T)

top_beta %>% 
  filter(topic == 16)

glimpse(gamma)
table(gamma$title)

glimpse(beta)

t <- beta %>% 
  mutate(lang = ifelse(str_detect(term, "[А-Яа-я]"), "rus", "ger"))
table(t$lang)


##### 1917 #####

beta <- read.csv("data/models/beta_1917_20.csv")
gamma <- read.csv("data/models/gamma_1917_20.csv")

glimpse(beta)
glimpse(gamma)

#### beta ####
top_beta <- beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) 

top_beta %>%
  ggplot(aes(x = beta, y = reorder_within(term, beta, topic), 
             fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  scale_y_reordered() + 
  facet_wrap(~ topic, scales = "free") + 
  scale_fill_manual(values = cols25(20))

top_beta %>% 
  filter(topic == 5)

#### gamma ####

gamma <- gamma %>% 
  mutate(title = str_remove_all(document, "\\d+|\\W")) %>% 
  mutate(year = str_replace_all(document, 
                                "\\w+(\\d{4})(\\d{2})(\\d{2})\\..*", "\\1")) %>% 
  mutate(month = str_replace_all(document, 
                                 "\\w+(\\d{4})(\\d{2})(\\d{2})\\..*", "\\2"))

gamma_month_title <- gamma %>% 
  group_by(title, month, topic) %>% 
  summarise(gamma_avg = mean(gamma))

gamma_month <- gamma %>% 
  group_by(month, topic) %>% 
  summarise(gamma_avg = mean(gamma))

glimpse(gamma_month)
glimpse(gamma_month_title)

gamma_month %>% 
  ggplot(aes(x = month, y = gamma_avg, fill = factor(topic))) + 
  geom_col() + 
  scale_fill_manual(values = cols25(n = 20))

length(unique(gamma_month_title$title))

gamma_month_title %>% 
  filter(title != "nasheslovo") %>%  # includes only 2 months
  ggplot(aes(x = month, y = gamma_avg, fill = factor(topic))) + 
  geom_col() + 
  scale_fill_manual(values = cols25(n = 20)) + 
  facet_wrap(~ title, scales = "free", ncol = 2)

top_beta %>% 
  filter(topic == 1)

unique(meta_1917$title)