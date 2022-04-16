# SETUP ####
# Libraries used
library(rvest)      # web scraping
library(stringr)    # string processing
library(tidyr)      # unnest list
library(dplyr)      # data manipulation - summarise by groups, etc
library(reshape2)   # helps shape data from wide to long
library(ggplot2)    # plots


# GET LINKS TO ALL UNIVERSITY PAGES ####
# Read source for main landing page
html_document <- rvest::read_html("https://sidata-ptn.ltmpt.ac.id/ptn_sb.php")
# Read specific xpath that leads to each university page (I used SelectorGadget)
unilinks <- html_document %>%
  rvest::html_nodes(xpath = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'btn-xs', ' ' ))]") %>%
  rvest::html_attr(name = "href")
# Check results
unilinks


# SCRAPE DATA FROM EACH PAGE ####
# Make function to get capacity (tampung) and interest (minat) data
tampungminat <- function(url){
  # main landing page + university code
  url <- paste0("https://sidata-ptn.ltmpt.ac.id/ptn_sb.php", url)
  webc <- rvest::read_html(url)
  # note: whether STEM/non-STEM majors are shown is a dynamic choice on the page
  # choose and store STEM majors table
  webipa <- webc %>%
    rvest::html_nodes(xpath = "//*[@id='jenis1']/table") %>% 
    rvest::html_table(dec = ",") 
  # choose and store non-STEM majors table
  webips <- webc %>%
    rvest::html_nodes(xpath = "//*[@id='jenis2']/table") %>% 
    rvest::html_table(dec = ",")
  # use conditionals for further processing because a university may not have STEM majors
  if(length(webipa)!=0){
    webipa <- webipa[[1]]
    webipa$`PEMINAT 2021` <- as.numeric(stringr::str_replace(webipa$`PEMINAT 2021`, '\\.', ''))
  }
  if(length(webips)!=0){
    webips <- webips[[1]]
    webips$`PEMINAT 2021` <- as.numeric(stringr::str_replace(webips$`PEMINAT 2021`, '\\.', ''))
  }
  # return both tables
  unitm <- list(webipa, webips)
  return(unitm)
}
# Test to see if function works for one university before looping
tampungminat(unilinks[1])
# Loop over all universities
alltm <- lapply(unilinks, tampungminat)
# Rename tables to help with processing
for(i in 1:length(alltm)) names(alltm[[i]]) <- c("IPA", "IPS")


# PROCESS DATA INTO APPROPRIATE FORMAT ####
# Unnest list into data.frame, by IPA (STEM) and IPS (non-STEM)
unnested <- tidyr::tibble(alltm) %>% tidyr::unnest_longer(alltm) %>% tidyr::unnest_wider(alltm)
# Calculate sum of 2022 seats for STEM and non-STEM programs per university
tampung <- unnested %>%
  dplyr::group_by(alltm_id) %>%
  dplyr::summarise(tampung = `DAYA TAMPUNG 2022`) %>%
  tidyr::unnest_wider(tampung)
tampung <- as.data.frame(tampung[,-1]) # remove column with characters
tampung$Row_Sums <- rowSums(tampung, na.rm=TRUE)
# Calculate sum of last year's test-takers choosing STEM and non-STEM programs per university
minat <- unnested %>%
  dplyr::group_by(alltm_id) %>%
  dplyr::summarise(minat = `PEMINAT 2021`) %>%
  tidyr::unnest_wider(minat)
minat <- as.data.frame(minat[,-1]) # remove column with characters
minat$Row_Sums <- rowSums(minat, na.rm=TRUE)

# University codes from links
KODE   <- substr(unilinks, 6, 8)
# Add information about islands per university (in order to remove the need for external references, I coded it into a vector)
# This is rather important information for Indonesians, because of the difference between islands... especially for those outside of Java
PULAU <- c(rep("Sumatera",22), rep("Jawa",35), rep("Kalimantan",6), rep("Bali, NTB, & NTT", 6), rep("Sulawesi",10), rep("Papua & Maluku", 6))
# Make data.frames
ipa <- cbind.data.frame(KODE, PULAU, `PEMINAT IPA 2021` = minat$Row_Sums[1:85], `DAYA TAMPUNG IPA 2022` = tampung$Row_Sums[1:85])
ips <- cbind.data.frame(KODE, PULAU, `PEMINAT IPS 2021` = minat$Row_Sums[86:170], `DAYA TAMPUNG IPS 2022` = tampung$Row_Sums[86:170])
# Check to see that we have the data we want
head(ipa) ; head(ips)

# Transform 
tipa <- reshape2::melt(ipa, id.vars = c("KODE","PULAU"))
tips <- reshape2::melt(ips, id.vars = c("KODE","PULAU"))


# MAKE CHARTS ####
# Which chart is good?

# Chart 1
ggplot(tips, aes(fill=PULAU, y=value, x=KODE)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c(1:6)) +
  theme_light() +
  labs(fill = "Jumlah") + ylab("") + xlab("Kode Universitas")
# What is the "problem" with this chart? 
# You can see the groups by island but you can't see the capacity vs interest at all!
# They are added together

# Chart 2
ggplot(tips, aes(fill=variable, y=value, x=KODE)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_fill_manual(values = c("red", "darkblue")) +
  labs(fill = "Jumlah") + ylab("") + xlab("Kode Universitas")
# What is the "problem" with this chart? 
# You can see the capacity vs interest, but not the islands

# Chart 4
ggplot(tips, aes(fill=PULAU, y=value, x=KODE)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~variable) +
  scale_fill_manual(values = c(1:6)) +
  theme_light() +
  labs(fill = "Jumlah") + ylab("") + xlab("Kode Universitas")
# What is the "problem" with this chart? 
# You have all the information, but there's waste of space which does not lead to easier comparison per island/uni

# Chart 5
ggplot(tips, aes(fill=variable, y=value, x=KODE)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~PULAU) +
  scale_fill_manual(values = c("red", "darkblue")) +
  theme_light() +
  labs(fill = "Jumlah") + ylab("") + xlab("Universitas") + theme(axis.text.x = element_text(angle = 90))
# What is the "problem" with this chart? 
# Easier comparison per island/uni, but lots of empty space

# FINAL CHARTS
ggplot(tips, aes(fill=variable, y=value, x=KODE)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~PULAU, scales = "free_x") +
  scale_fill_manual(values = c("red", "darkblue")) +
  labs(fill = "(Jumlah)") + ylab("") + xlab("(Kode Universitas)") + theme(axis.text.x = element_text(angle = 90)) +
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ),
    strip.text.y = element_text(
      size = 12, color = "black"
    ),
    legend.position="top",
    legend.title = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
ggplot(tipa, aes(fill=variable, y=value, x=KODE)) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~PULAU, scales = "free_x") +
  scale_fill_manual(values = c("red", "darkblue")) +
  labs(fill = "(Jumlah)") + ylab("") + xlab("(Kode Universitas)") + theme(axis.text.x = element_text(angle = 90)) +
  theme(
    strip.text.x = element_text(
      size = 12, color = "black"
    ),
    strip.text.y = element_text(
      size = 12, color = "black"
    ),
    legend.position="top",
    legend.title = element_text(size = 12),
    axis.title = element_text(size = 12)
  )



