library(tidyverse)
library(tidytext)
library("wesanderson")

theme_set(theme_minimal())

setwd("Documents/est_np/")
dat <- read.csv("data/meta_rus_lang.csv")
glimpse(data_full)

#### preparation (if took full dataset) ####
dat <- data_full %>% 
  select(docid, PageID, lang, PageImageHeight, PageImageWidth,
         PageOCRAccuracy, PageTitle, MeanOCRAccuracyVol, PageViewURL, PageTextWordCount,
         date.x, year.x, decade.x, DocumentTitle, firstyear, lastyear, title, 
         language, country, place)

glimpse(dat)



#### Plot page sizes ####

dat %>% 
  select(PageImageHeight, PageImageWidth, decade.x) %>% 
  distinct() %>% 
  mutate(xmin = 0, ymin =0) %>% 
  ggplot(aes(x = PageImageWidth, y = PageImageHeight)) + 
  geom_point(alpha = 0.5) +
  geom_segment(aes(x = PageImageWidth, y = PageImageHeight, 
                   xend = 0, yend = PageImageHeight), col = "lightblue", alpha = 0.2) + 
  geom_segment(aes(x = PageImageWidth, y = PageImageHeight, 
                   xend = PageImageWidth, yend = 0), col = "lightblue", alpha = 0.2) + 
  facet_wrap(~decade.x)

#### Metadata cleaning ####

unique(dat$place)

unique(dat$lang)
table(dat$lang)
unique(dat$language)

# only 40 na-s
dat %>% 
  filter(is.na(lang)) %>% 
  count()

# selecting only most frequent languages & ones not detected by a mistake (e.g. nepali)
langs <- c("german", "russian", "ukrainian", "estonian", "latvian")

dat <- dat %>% 
  # add clean places of publishing
  mutate(place_cln = str_replace_all(place, "^(\\w+)[[:space:]].*", "\\1")) %>% 
  # replace "В. П. Янчевецкая" as Revel since her newspaper "Ревельские известия" was issued there
  mutate(place_cln = str_replace_all(place_cln, "В. П. Янчевецкая", "Ревель")) %>% 
  
  # summarise languages
  mutate(lang_cln = str_replace_all(lang, "^(\\w+)-.*", "\\1")) %>% 
  filter(!is.na(lang_cln) & lang_cln %in% langs) %>% 
  
  # OCR accuracy as numeric column
  mutate(ocr_acc = str_remove_all(PageOCRAccuracy, "%")) %>% 
  mutate(ocr_acc = as.numeric(ocr_acc))

#write.csv(dat, "~/Documents/ds/digar_newspapers_2022/data/meta_rus_lang.csv")

#### Plot: languages in 'Russian' newspapers ####

years_count <- dat %>% 
  #filter(place_cln %in% c("Ревель", "Рига")) %>% 
  group_by(year.x, lang_cln
           #, place_cln
  ) %>% 
  count()

dat %>% 
  #filter(place_cln %in% c("Ревель", "Рига")) %>% 
  group_by(year.x, lang_cln
           #, place_cln
           ) %>% 
  count() %>% 
  ggplot(aes(x = year.x, y = n, fill = lang_cln)) + geom_col() + 
  scale_fill_manual(values = wes_palette("Darjeeling2")) + 
  #facet_wrap(~place_cln) + 
  labs(x = "Year", 
       y = "Number of pages",
       fill = "Language", 
       title = "Languages detected inside the pages of 'Russian' newspapers",
       subtitle = "'textcat' package was used for detection: it calculates the most probable language for a page, \ne.g. if 30% of the page is in Russian and 70% is in German, it will be tagged as German") 

#### Plot: places of publishing ####
glimpse(dat)

dat %>% 
  group_by(year.x, place_cln, lang_cln) %>% 
  count() %>% 
  ggplot(aes(x = year.x, y = n, fill = place_cln)) + geom_col() + 
  scale_fill_manual(values = sample(wes_palette("Cavalcanti1"),5)) + 
  labs(x = "Year", 
       y = "Number of pages", 
       fill = "Place of publishing", 
       title = "Places of publishing of 'Russian' newspapers",
       subtitle = "German and Russian language pages counted together, \nno preferences between place and langauge discovered")

### check corr between place and language of publishing
# dat %>% 
#   filter(lang_cln %in% c("german", "russian")) %>% 
#   group_by(year.x, place_cln, lang_cln) %>% 
#   count() %>% 
#   ggplot(aes(x = year.x, y = n, fill = place_cln)) + geom_col() + 
#   scale_fill_manual(values = sample(wes_palette("Cavalcanti1"),5)) + 
#   facet_wrap(~lang_cln)

# same but only for pages detected as 'Russian'
dat %>% 
  filter(lang_cln == "russian") %>% 
  group_by(year.x, place_cln) %>% 
  count() %>% 
  ggplot(aes(x = year.x, y = n, fill = place_cln)) + geom_col() + 
  scale_fill_manual(values = sample(wes_palette("Cavalcanti1"),5)) + 
  labs(x = "Year", 
       y = "Number of pages", 
       fill = "Place of publishing", 
       title = "Places of publishing for pages in Russian")

#### OCR accuracy ####
glimpse(dat)

# 32k pages with no OCR accuracy values (96k has the values)
dat %>% 
  filter(!is.na(ocr_acc)) %>% 
  count()

dat %>% 
  filter(!is.na(ocr_acc)) %>% 
  group_by(year.x, ocr_acc) %>% 
  #summarise(mean_acc = )
  ggplot(aes(x = year.x, y = ocr_acc)) + geom_point(col = "lightblue", alpha = 0.5) + 
  geom_smooth()

dat %>% 
  filter(!is.na(ocr_acc)) %>% 
  group_by(decade.x, ocr_acc) %>% 
  ggplot(aes(x = as.factor(decade.x), y = ocr_acc)) + geom_boxplot(col = "#798E87")
# OCR accuracy is lower with smaller percentage of pages in German??

wes_palette("Moonrise2")[1]

dat %>% 
  filter(!is.na(ocr_acc) & lang_cln %in% c("german", "russian")) %>% 
  group_by(decade.x, ocr_acc, lang_cln) %>% 
  ggplot(aes(x = as.factor(decade.x), y = ocr_acc, fill = lang_cln)) + 
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.55)) + 
  scale_fill_manual(values = wes_palette("Chevalier1")) + 
  labs(x = "", 
       y = "OCR accuracy (%)",
       fill = "Language", 
       title = "OCR accuracy dependence on language",
       subtitle = "N pages = 96 030 \nNB! Very few pages in German after 1890")
  
glimpse(dat)
dat %>% 
  filter(!is.na(ocr_acc)) %>%
  ggplot(aes(x = PageImageWidth, y = ocr_acc)) + geom_point(alpha = 0.5, color = "lightblue")

# to do: Ukrainian is Russian, probably detected in a wrong way bcs of very bad OCR quality: make a plot to see whether it is true
