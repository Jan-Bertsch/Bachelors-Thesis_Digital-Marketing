## =============================================================================
## Bachelor's Thesis                                            ----------------
##                                                              ----------------
## Digital Marketing                                            ----------------
## Strategy and Performance Implications for Firms              ----------------
##                                                              ----------------
## Author: JAN BERTSCH                                          ----------------
## =============================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)
memory.limit(30500)


## Load Packages                                                                 # On this side of the Code,I give the corresponding  
# For a better overview, of where the packages where used, I included some       # text paragraphs text, to provide easy comparison
# of the library() comments in the respective code parts

library(tidyverse)
library(tidytext)
library(readxl)
library(readr)
library(data.table)
library(quanteda)
library(SentimentAnalysis)
library(pscl)
library(stargazer)
library(fixest)
library(jtools)
library(moments)
library(apaTables)
library(sjPlot)


## =============================================================================
## -----------------------------------------------------------------------------
## ---------------  Part 1. :  Read in relevant datasets  ----------------------
## -----------------------------------------------------------------------------
## =============================================================================

# Read in Earnings Call Data
calls_data <- fread(
  "D:/Thesis/EarningCalls/sampleEarningsCalls_matched_edited.csv")


# Read In Calls Data matched with Compustat data
calls_matched <- fread(
  "D:/Thesis/EarningCalls/allEarningsCallsInfo_matched.csv", 
                       dec = ",")


# Read in Compustat Data to retrieve financial variables
compustat_qtr <- fread("D:/Compustat2/compustat_quarterly_20002020.csv")


# Match compustat quarterly data with variables in Dataset
compustat_qtr <- calls_matched %>% 
  select(id, gvkey, datacqtr) %>% 
  left_join(compustat_qtr, by = c("gvkey", "datacqtr"))


## Since the Calls Dataset has been shortened, the calls_matched dataframe 
## containing the compustat data, must be adjusted by matching it only with 
## the calls that are present in the calls dataset

calls_matched_new <- calls_data %>% 
  select(id) %>% 
  distinct() %>% 
  left_join(calls_matched, by = "id")
write.csv(calls_matched_new, 
          "D:/Thesis/EarningCalls/allEarningsCallsInfo_matched_new2.csv", 
          row.names = FALSE)


## Read in the Data
calls_matched <- read.csv(
  "D:/Thesis/EarningCalls/allEarningsCallsInfo_matched_new2.csv", 
  dec = ",")


## =============================================================================
## -----------------------------------------------------------------------------
## ------------------------  Part 2. :  Text Analysis  ------------------------- ## Text Paragrah:
## ----------------------------------------------------------------------------- # 5.2 Textual Analysis
## =============================================================================


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.1 - Import Dictionaries ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Function to import dictionaries
imp_dict <- function(path, v.files){
  
  # Load packages
  require(stringr)
  require(readtext)
  
  # Import and process dictionaries
  dict <- list()
  for(i in 1:length(v.files)){
    temp <- readtext(paste0(path, "\\", v.files[i]), encoding = "utf8") %>%
      select(text) %>%
      str_split(pattern = "\n")
    dict[[i]] <- temp[[1]]}
  names(dict) <- v.files
  return(dict)
}


# Set path that contains dictionaries
p.dict1 <- "C:/Thesis/Dictionaries"


# List files in imported directories
l.dict1 <- list.files(p.dict1, pattern = "*.txt")


# Import dictionaries
di.GeneralTerms <- imp_dict(p.dict1, l.dict1[1])
di.Internet <- imp_dict(p.dict1, l.dict1[2])
di.KPI <- imp_dict(p.dict1, l.dict1[3])
di.SocialMedia <- imp_dict(p.dict1, l.dict1[4])
di.Technology <- imp_dict(p.dict1, l.dict1[5])
di.Webpage <- imp_dict(p.dict1, l.dict1[6])

                                                                                
# Combine dictionaries
di.combined <- list(GeneralTerms = di.GeneralTerms[[1]], 
                        Internet = di.Internet[[1]],
                        KPI = di.KPI[[1]],
                        SocialMedia = di.SocialMedia[[1]],
                        Technology = di.Technology[[1]],
                        Webpage = di.Webpage[[1]])

di.combined <- di.combined %>% 
  dictionary(tolower = TRUE)


## Note: 
# In part 4.I revised the dictionaries used in the primary analysis and 
# merge the Social Media, Webpage and Internet dictionaries as their terms are
# more or less similar.The final dictionaries can also be found in the GitHub
# repository. However, I still included the code of the initial approach above, 
# as Ialready ran the analysis. Running it twice would have consumed too much 
# time due to the RAM and CPU limitations, so it was easier to just add the 
# variableslater on.


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                     ## Text Pragraph:
## 2.2 - Create Function to Analyze Text ~~~                                     # 5.2.1 Operationalization of the Variable: DM.rtf
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(quanteda)

dictionary_analysis_multdict <- function(df){
  testo <- corpus(df, text_field = "speech")
  dict.count <- tokens(testo) %>%
    tokens_lookup(di.combined) %>%
    dfm()
  
  #convert to dataframe
  dict.count.df <- convert(dict.count, to ="data.frame")
  
  #build sums over the rows to get the total number of words
  dict.count.df$total <- rowSums(dict.count.df[, 2:7])
  
  #number of tokens without and with stopwords
  df$tokens_sw <- ntoken(tokens_select(tokens(df$speech, 
                                              remove_punct = TRUE, 
                                              remove_numbers = TRUE,
                                              remove_symbols = TRUE),
                                      pattern = stopwords("en"), 
                                      selection = "remove"))   

  #merge with corpus
  calls1_dict <- cbind(df, dict.count.df)
  
  #calculate relative frequencies
  calls1_dict$DM.rtf <- calls1_dict$total / calls1_dict$tokens_sw
  return(calls1_dict)
}



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.3 - Perform Textual Analysis ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Concatenate Single Speeches and group by ID, so that Analysis can be 
## performed for each Call (ID)

calls_grouped <- calls_data %>%
  group_by(id) %>%
  summarise(speech = paste0(speech, 
                            collapse = " "))


# Perform analysis (it was necessary to run analysis in chunks due  to 
# performance limitations of my computer)
text_analysis <- dictionary_analysis_multdict(calls_grouped[1:10000,])
text_analysis2 <- dictionary_analysis_multdict(calls_grouped[10001:15000,])
text_analysis3 <- dictionary_analysis_multdict(calls_grouped[15001:20000,])
text_analysis4 <- dictionary_analysis_multdict(calls_grouped[20001:25000,])
text_analysis5 <- dictionary_analysis_multdict(calls_grouped[25001:30000,])
text_analysis6 <- dictionary_analysis_multdict(calls_grouped[30001:35000,])
text_analysis7 <- dictionary_analysis_multdict(calls_grouped[35001:40000,])
text_analysis8 <- dictionary_analysis_multdict(calls_grouped[40001:45000,])
text_analysis9 <- dictionary_analysis_multdict(calls_grouped[45001:50000,])
text_analysis10 <- dictionary_analysis_multdict(calls_grouped[50001:55000,])
text_analysis11 <- dictionary_analysis_multdict(calls_grouped[55001:60000,])
text_analysis12 <- dictionary_analysis_multdict(calls_grouped[60001:65000,])
text_analysis13 <- dictionary_analysis_multdict(calls_grouped[65001:70708,])


# Bind dataframes by rows
binded <- rbind(text_analysis, text_analysis2, text_analysis3, 
                text_analysis4, text_analysis5, text_analysis6, 
                text_analysis7, text_analysis8, text_analysis9, 
                text_analysis10, text_analysis11, text_analysis12,
               text_analysis13)


# Extract only word count variables and ID (Variables "speech" and "doc_id" 
# are being ignored)

binded <- binded %>% 
  select(id, tokens_sw, 5:12) %>%
  
  #Add firm names and relocate
  merge(calls_matched[ , c("id", "firm")], by = "id", all.x = TRUE) %>% 
  relocate(firm, .after = 1)

# Rename for "aestethic" reasons
binded <- binded %>% 
  rename(year = fyearq)
  

# Calculate shares of each Word category
binded <- binded %>% 
  mutate(GeneralTerms.rtf = generalterms/tokens_sw, 
         Internet.rtf = internet/tokens_sw, 
         KPI.rtf = kpi/tokens_sw, 
         SocialMedia.rtf = socialmedia/tokens_sw, 
         Technology.rtf = technology/tokens_sw, 
         WebPage.rtf = webpage/tokens_sw)


# Convert shares to percentages
binded[,10:16] <- binded[,10:16]*100


# Save analysis as CSV File
write.csv(binded, "C:/Thesis/multdict_analysis.csv")


## =============================================================================
## -----------------------------------------------------------------------------
## ----------------  Part 3. :  Create specific Variables  ---------------------
## -----------------------------------------------------------------------------
## =============================================================================


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1 - Match Dataset with GICS Sector Names ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Select Relevant Variables for join
calls_sectorID <- calls_matched %>% 
  select(id, gsector) %>% 
  distinct()


# Read in GIC Code Descriptions retrieved from spglobal.com                                                 
GIC_gsector <- read_excel(
  "D:/OneDrive/Uni/WS22/Thesis/Daten/Compustat/GIC Sectors.xls", 
  sheet = 2)


# Match Dataframe with Code variables (gsector)
calls_matched <- calls_matched %>% 
  left_join(GIC_gsector, by = "gsector")


# Check for NAs
calls_matched %>% 
  filter(is.na(Sector)) %>% 
  select(firm)


# Manually input missing Sector (Health Care)
calls_matched$Sector[is.na(calls_matched$Sector)] <- 
  "Health Care"

# Match with binded dataframe
Sectors <- calls_matched %>% 
  select(id, Sector)

binded <- binded %>% 
  left_join(Sectors, by = "id") %>%
  
  # Convert to Factor
  mutate(Sector = factor(Sector))



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1.1 - Sector Overview ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~

binded %>% group_by(Sector) %>% 
  summarize(count = n()) %>% 
  mutate(share = round((count/sum(count))*100, digits = 2)) %>% 
  ggplot(aes(reorder(Sector, share), share)) +
  
  ggtitle("Sectors by GIC Code") +
  xlab("") + ylab("% of total sample") +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_apa() +
  geom_text(aes(label = paste(format(share, digits = 1), "%")), hjust = -0.1,
            size = 3, colour = "black") +
  scale_y_continuous(limits = c(0, 37)) + 
  theme(text = element_text(family = "Arial", face = "bold", size = 10),
        axis.title.y = element_blank())

ggsave("Sectors.png", height = 7, width = 17, units = "cm", 
       dpi = 500 , path = "C:/Thesis/Plots")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                          ## Text Paragraph:
## 3.2 - Create "CMO" Variable to indicate Presence ~~~                          # 5.2.2 Operationalization of the Variable: CMO Presence
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Idea:                                                                                                       
# Create dictionary, that contains all relevant position of the CMO Board and 
# analyze participants of the call 
#
# A bivariate Variable (Present/Not Present) will then be created                                             


# Create Function similar to text analysis function
dictionary_analysis_cmo <- function(df){
  testo <- corpus(df, text_field = "corporate.participants")
  dict.count <- tokens(testo, split_hyphens = TRUE) %>%
    tokens_lookup(cmo_dict) %>%
    dfm() 
  
  #convert to dataframe
  dict.count.df <- convert(dict.count, to ="data.frame")
  dict.count.df$CMO = dict.count.df$cmo>0
  dict.count.df$CMO = as.numeric(dict.count.df$CMO)
  
  #merge with corpus
  calls1_dict <- cbind(df, dict.count.df[3])
  return(calls1_dict)
}


# Read in .txt file 
cmo_dict <- read.delim("C:/Thesis/CMO_Dictionary.txt", 
                       header = FALSE)


# Convert to dictionary
cmo_dict_list <- list(c(cmo_dict))
cmo_dict <- dictionary(list(cmo = cmo_dict_list))


# Perform analysis to create Variable
CMO_presence <- calls_matched %>% 
  dictionary_analysis_cmo() %>% 
  select(id, CMO)


# Add Variable to "binded" Dataset
binded <- binded %>% 
  left_join(CMO_presence, by = "id") %>% 
  mutate(CMO = CMO=="Present") %>% 
  mutate(CMO = as.numeric(CMO))



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.3 - Create "Digital Marketing Focus "DM_Foc" Variable ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

binded <- binded %>% 
  mutate(DM_Foc = DM.rtf > mean(DM.rtf)) %>%
  mutate(DM_Foc = as.numeric(DM_Foc)) #%>% 
  mutate(DM_Foc = factor(DM_Foc, levels = c(FALSE, TRUE), 
                         labels = c("above Average", "below Average")))
  


## =============================================================================
## -----------------------------------------------------------------------------
## --------------  Part 4. :  Sentiment Analysis  ------------------------------ ## Text Paragraph:
## ----------------------------------------------------------------------------- # 5.2.3 Operationalization of the Variable: Sentiment 
## =============================================================================  
  

# At this point, I decided to run a sentiment analysis using the LM-Dictionary
# to be able to include another text variable in the regression analyses, to 
# get more insights on how the qualitative text content interacts with certain 
# financial metrics.
  
  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.1 - Retrieve LM Dictionary from SentimentAnalysis package ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(SentimentAnalysis)

LM_dict = (DictionaryLM)


# Extract Dictionaries to include in GitHub Repository
neg <- dict[[1]]
pos <- dict[[2]]
uncer <- dict[[3]]
cat(neg, sep = "\n", file = "Negative.txt")
cat(pos, sep = "\n", file = "Positive.txt")
cat(uncer, sep = "\n", file = "Uncertainty.txt")
  
  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.2 - Perform Sentiment Analysis ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dictionary_analysis_LM <- function(df){
  testo <- corpus(df, text_field = "speech")
  dict.count <- tokens(testo) %>%
    tokens_lookup(LM_dict) %>%
    dfm()
    
  #convert to dataframe
  dict.count.df <- convert(dict.count, to ="data.frame")
  
  #number of tokens without and with stopwords
  df$tokens_sw <- ntoken(tokens_select(tokens(df$speech,  
                                              remove_punct = TRUE,
                                              remove_numbers = TRUE,
                                              remove_symbols = TRUE),
                                       pattern = stopwords("en"), 
                                       selection = "remove"))   
  
  #merge with corpus
  calls1_dict <- cbind(df, dict.count.df)
  
  #calculate relative frequencies
  calls1_dict = calls1_dict %>% 
    mutate(negative_rtf = (negative/tokens_sw)*100, 
           positive_rtf = (positive/tokens_sw)*100,
           uncertainty_rtf = (uncertainty/tokens_sw)*100) %>% 
    select(id, tokens_sw, negative, positive, uncertainty, 
           negative_rtf, positive_rtf, uncertainty_rtf)
  return(calls1_dict)
}


# Again, due to RAM and CPU limitations, the analysis had to be split in
# different chunks

sentiment_analysis <- dictionary_analysis_LM(calls_grouped[1:10000,])
sentiment_analysis2 <- dictionary_analysis_LM(calls_grouped[10001:15000,])
sentiment_analysis3 <- dictionary_analysis_LM(calls_grouped[15001:20000,])
sentiment_analysis4 <- dictionary_analysis_LM(calls_grouped[20001:25000,])
sentiment_analysis5 <- dictionary_analysis_LM(calls_grouped[25001:30000,])
sentiment_analysis6 <- dictionary_analysis_LM(calls_grouped[30001:35000,])
sentiment_analysis7 <- dictionary_analysis_LM(calls_grouped[35001:40000,])
sentiment_analysis8 <- dictionary_analysis_LM(calls_grouped[40001:45000,])
sentiment_analysis9 <- dictionary_analysis_LM(calls_grouped[45001:50000,])
sentiment_analysis10 <- dictionary_analysis_LM(calls_grouped[50001:55000,])
sentiment_analysis11 <- dictionary_analysis_LM(calls_grouped[55001:60000,])
sentiment_analysis12 <- dictionary_analysis_LM(calls_grouped[60001:65000,])
sentiment_analysis13 <- dictionary_analysis_LM(calls_grouped[65001:70708,])
  

# bind dataframes by row
sentiment_analysis_binded <- rbind(sentiment_analysis, sentiment_analysis2, 
                                   sentiment_analysis3, sentiment_analysis4, 
                                   sentiment_analysis5, sentiment_analysis6, 
                                   sentiment_analysis7, sentiment_analysis8, 
                                   sentiment_analysis9, sentiment_analysis10, 
                                   sentiment_analysis11, sentiment_analysis12, 
                                   sentiment_analysis13)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.3 - Compute sentiment score relative to call length ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sentiment_analysis_binded <- sentiment_analysis_binded %>% 
  mutate(sentiment = (positive - negative)/tokens_sw)


# Save file
write.csv(sentiment_analysis_binded, "sentiment_analysis_binded.csv")


# Join with previously created dataframe containing all text related variables
binded_wSentiment = binded %>% 
  left_join(sentiment_analysis_binded)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.4 - Adjust Dictionary Subcategories ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# At this point, I revised the dictionaries used in the primary analysis and 
# decided to merge the Social Media, Webpage and Internet dictionaries as their
# terms are more or less similar.

# Instead of running a new analysis, I just summarized all 3 term categories
# to 1 category with the name "web".

binded_wSentiment_new <- binded_wSentiment %>% 
  mutate(web = internet + socialmedia + webpage, 
         Web.rtf = web/tokens_sw) %>% 
  relocate(web, .before = kpi) %>% 
  relocate(Web.rtf, .before = KPI.rtf) %>% 
  # Delete unnecessary variables
  select(-c(internet, socialmedia, webpage, Internet.rtf, SocialMedia.rtf, 
            WebPage.rtf))


# Save as .csv file
write.csv(binded_wSentiment_new, "binded_wSentiment_new.csv")



## =============================================================================
## -----------------------------------------------------------------------------
## -----------  Part 5. :  Visualization of Textual Analysis  ------------------
## -----------------------------------------------------------------------------
## =============================================================================


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                              ## Text Paragraph
## 5.1 - Token Share over Time  ~~~                                              # 6.1 Insights from Textual Analysis
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load Package for APA ggplot theme
library(jtools)

binded_wSentiment_new %>% 
  group_by(year) %>% 
  mutate(avg_token_share = mean(DM.rtf)) %>% 
  
  ggplot(aes(year, avg_token_share)) +
  ggtitle("Token Share over Time") +
  labs(x = "Year", y = "Share in %") +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  
  # Use theme according to APA standard
  theme_apa() + 
  theme(text = element_text(family = "Arial", face = "bold", size = 12))

# Save plot to include in paper
ggsave("tokenshare_over_years_plot2.png", 
       height = 7, width = 16, units = "cm", dpi = 500, 
       path = "C:/Thesis/Plots")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.2 - Sentiment over Time ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

binded_wSentiment_new %>% 
  group_by(year) %>% 
  mutate(avg_sentiment = mean(sentiment)) %>% 
  
  ggplot(aes(year, avg_sentiment)) +
  ggtitle("Sentiment Score over Time") +
  labs(x = "Year", y = "Sentiment Score") +
  geom_line(size = 1, color = "steelblue") +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3)) +
  
  # Highlight decrease during fiancial crisis and start of CoVid-19 Pandemic
  geom_rect(xmin = 2007.5, xmax = 2008.5, ymin = 0, ymax = 0.02, 
            fill = "grey", alpha = 0.02) +
  geom_rect(xmin = 2019.5, xmax = 2020.3, ymin = 0, ymax = 0.02, 
            fill = "grey", alpha = 0.02)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.3 - Dictionary Share over Time ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

binded_wSentiment_new%>% 
  group_by(year) %>% 
  summarize(KPI = mean(KPI.rtf), 
            Technology = mean(Technology.rtf),
            Web = mean(Web.rtf),
            GeneralTerms = mean(GeneralTerms.rtf)) %>% 
  
  ggplot(aes(year, colour = Dictionary)) +
  ggtitle("Dictionary Share over Time") +
  xlab("Year") + ylab("Share in %") +
  theme(legend.title=element_blank())+ 
  geom_line(aes(y = KPI , color="KPI"), size = 1) + 
  geom_line(aes(y = Technology, color="Technology"), size = 1) +
  geom_line(aes(y = GeneralTerms, color = "General Terms"), size = 1) +
  geom_line(aes(y = Web, color = "Web"), size = 1) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.4 - Radar Plot: Dictionary Share over Time ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

word_categories <- binded_wSentiment_new %>% 
  group_by(year) %>% 
  summarise(KPI = mean(KPI.rtf), 
            Technology = mean(Technology.rtf),
            Web = mean(Web.rtf),
            GeneralTerms = mean(GeneralTerms.rtf)) %>% 
  pivot_longer(!year, names_to = "Dictionary", values_to = "Share")

library(plotly)

radar_plot_categories <- word_categories %>%
  plot_ly(type = 'scatterpolar',
          mode = 'markers',
          r = ~Share,
          theta = ~Dictionary,
          fill = 'toself',
          frame = ~year)

radar_plot_categories <- radar_plot_categories %>%
  layout(polar = list(
    radialaxis = list(
      visible = T,
      range = c(0,0.38))),
    showlegend = F)

radar_plot_categories


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.5 - Token share across Economic Sectors ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# All Sectors
binded_wSentiment_new %>% 
  group_by(year, Sector) %>% 
  summarise(avg_token_share = mean(DM.rtf))%>%
  pivot_wider(names_from = "Sector", values_from = "avg_token_share") %>% 
  
  ggplot(aes(year, colour =NA)) +
  ggtitle("Evolution of Term Share by Industry") +
  xlab("Year") + ylab("Share in %") +
  theme(legend.title=element_blank()) +
  geom_line(aes(y = `Consumer Discretionary`, color = "Consumer Discretionary"), 
            size = 1) + 
  geom_line(aes(y = `Consumer Staples` , color = "Consumer Staples"), 
            size = 1) +
  geom_line(aes(y = Energy, color = "Energy"), 
            size = 1) + 
  geom_line(aes(y = Financials, color="Financials"), 
            size = 1) +
  geom_line(aes(y = `Health Care`, color = "Health Care"), 
            size = 1) +
  geom_line(aes(y = Industrials, color = "Industrials"), 
            size = 1) +
  geom_line(aes(y = `Information Technology`, color = "Information Technology"), 
            size = 1) +
  geom_line(aes(y = Materials, color = "Materials"), 
            size = 1) +
  geom_line(aes(y = `Telecommunication Services`, color = "Telecommunication 
                Services"), 
            size = 1) +
  geom_line(aes(y = Utilities, color = "Utilities"), 
            size = 1) +
  geom_line(aes(y = `Real Estate`, color = "Real Estate"), 
            size = 1) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3))


## As the previous plot is a little messy, I created seperate plots, with similar
## economic sector to better compare the results


# Financials, IT, Telecommunication, Real Estate
binded_wSentiment_new %>% 
  group_by(year, Sector) %>% 
  summarise(avg_token_share = mean(DM.rtf))%>%
  pivot_wider(names_from = "Sector", values_from = "avg_token_share") %>%
  
  ggplot(aes(year, colour =NA)) +
  ggtitle("Evolution of Term Share by Industry") +
  xlab("Year") + ylab("Share in %") +
  theme(legend.title=element_blank()) + 
  geom_line(aes(y = Financials, color = "Financials"), 
            size = 1) +
  geom_line(aes(y = `Information Technology`, color = "Information Technology"), 
            size = 1) +
  geom_line(aes(y = `Telecommunication Services`, color = "Telecommunication 
                Services"), 
            size = 1) +
  geom_line(aes(y = `Real Estate`, color = "Real Estate"), 
            size = 1) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3))



# Consumer Discretionary, Consumer Staples, Health care
binded_wSentiment_new %>% 
  group_by(year, Sector) %>% 
  summarise(avg_token_share = mean(DM.rtf)) %>%
  pivot_wider(names_from = "Sector", values_from = "avg_token_share") %>% 
  
  ggplot(aes(year, colour =NA)) +
  ggtitle("Evolution of Term Share by Industry") +
  xlab("Year") + ylab("Share in %") +
  theme(legend.title=element_blank()) +
  geom_line(aes(y = `Consumer Discretionary`, color = "Consumer Discretionary"), 
            size = 1) + 
  geom_line(aes(y = `Consumer Staples` , color = "Consumer Staples"), 
            size = 1) +
  geom_line(aes(y = `Health Care`, color = "Health Care"), 
            size = 1) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3))



# Energy, Industrials, Materials, Utilities
binded_wSentiment_new %>% 
  group_by(year, Sector) %>% 
  summarise(avg_token_share = mean(DM.rtf))%>%
  pivot_wider(names_from = "Sector", values_from = "avg_token_share") %>% 
  
  ggplot(aes(year, colour =NA)) +
  ggtitle("Evolution of Term Share by Industry") +
  xlab("Year") + ylab("Share in %") +
  theme(legend.title=element_blank()) +
  geom_line(aes(y = Energy, color = "Energy"), 
            size = 1) +
  geom_line(aes(y = Industrials, color = "Industrials"), 
            size = 1) +
  geom_line(aes(y = Materials, color = "Materials"), 
            size = 1) +
  geom_line(aes(y = Utilities, color = "Utilities"), 
            size = 1) +
  scale_x_continuous(breaks = seq(2006, 2021, by = 3))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.6 - Dictionary Term Share across Industries ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Dictionary Term Share across Industries
binded_wSentiment_new %>% 
  group_by(Sector) %>% 
  summarize(KPI = mean(KPI.rtf),
            Web = mean(Web.rtf),
            GeneralTerms = mean(GeneralTerms.rtf),
            Technology = mean(Technology.rtf)) %>% 
  pivot_longer(!Sector, names_to = "Dictionary", values_to = "Term Share") %>% 
  mutate(Dictionary = as.factor(Dictionary)) %>% 
  
  ggplot(aes(Sector, `Term Share`, fill = Dictionary)) +
  ggtitle("Dictionary Term Share across Industries") +
  ylab("Term Share in %") + xlab("") +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_brewer() +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 0.8, by = 0.1))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.7 - Comparison of overall average across economic sectors ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

binded_wSentiment_new %>% 
  group_by(Sector) %>% 
  summarise(termshare_overall = mean(DM.rtf)) %>% 
  
  ggplot(aes(reorder(Sector, termshare_overall), termshare_overall)) +
  xlab("GIC Sectors") + ylab("Overall DM.rtf") +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_apa() +
  geom_text(aes(label = paste(format(termshare_overall, digits = 1), "%")), 
            hjust = -0.1,
            size = 3, 
            colour = "black") + 
  theme(text = element_text(family = "Arial", face = "bold", size = 10),
        axis.title.y = element_blank())


# Save to include in paper
ggsave("Sectors.png", height = 7, width = 17, units = "cm", 
       dpi = 500 , path = "C:/Thesis/Plots")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.8 - Radar Plot: Comparison of overall average across economic sectors ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

share_by_sector <- binded_wSentiment_new %>% 
  filter(year < 2021) %>% 
  group_by(year, Sector) %>% 
  summarise(avg_token_share = mean(DM.rtf))


radar_plot_sectors <- share_by_sector %>%
  plot_ly(
    type = 'scatterpolar',
    mode = 'markers',
    r = ~avg_token_share,
    theta = ~Sector,
    fill = 'toself',
    frame = ~year
    
  ) 
radar_plot_sectors <- radar_plot_sectors %>%
  layout(polar = list(
    radialaxis = list(
      visible = T,
      range = c(0,1.95)
    )
  ),
  showlegend = F)

radar_plot_sectors

  

## =============================================================================
## -----------------------------------------------------------------------------
## --------------  Part 6. :  Descriptive Analysis  ---------------------------- ## Text Paragraph
## ----------------------------------------------------------------------------- # 6.2 Descriptive Statistics
## =============================================================================

## How many firms?
calls_matched %>% 
  select(firm) %>% 
  distinct() %>% 
  nrow()  

# 3169 firms are present in the dataset


# Inspect top companies by revenue to provide overview
regression_data_sentiment %>% 
  select(firm, Sector, revtq) %>%  
  group_by(Sector) %>% 
  top_n(1, revtq) %>% 
  view()


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6.2 - Most frequent words in IT Sector~~~                                      ## Text Paragraph
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                      # 6.1 Insights from Textual Analysis

# Extract id of top 50 with highest Technology.rtf
top_tech <- binded_wSentiment_new %>% 
  arrange(desc(Technology.rtf)) %>% 
  head(n = 50) %>% 
  pull(id) 

# Create dataset with respective observations
technology_call <- calls_data %>% 
  filter(id %in% top_tech)


toks <- tokens(technology_call$speech) 

# Look up dictionary tokens in Dataset
dfm_list <- list()
for (key in names(di.combined)) {
  this_dfm <- tokens_select(toks, di.combined[key], pad = TRUE) %>%
    tokens_compound(di.combined[key]) %>%
    tokens_replace("", "OTHER") %>%
    dfm(tolower = FALSE)
  dfm_list <- c(dfm_list, this_dfm)
}

names(dfm_list) <- names(di.combined)
dfm_list

# Choose Dictionary 5, which is Technology Dictionary
top <- topfeatures(dfm_list[[5]], n = 100)
top[[1]]

# Create dataframe
top_words <- as.data.frame(top)
top_words$term <- rownames(top_words)
rownames(top_words) <- 1:nrow(top_words)

# Adjust column order
top_words <- top_words %>% 
  relocate(, term, .before = 1) %>% 
  # view results
  view()
  

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
## 6.1 - Test for Skewness of DM.rtf variable ~~~                                 ## Text Paragraph
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                 # 6.2 Descriptive Statistics

library(moments)
skewness(binded$DM.rtf)

binded %>% 
  ggplot(aes(DM.rtf)) +
  xlab("DM.rtf") +
  geom_histogram(binwidth = 0.3, 
                 fill = "grey", 
                 color = "white") +
  geom_density(aes(y=0.35*..count..), 
               colour = "black", 
               adjust = 4, size = 0.5) +
  theme_apa() +
  annotate("text", x = 3.1, y = 12500, label = 
             paste("Skewness = ", round(skewness(binded$DM.rtf), 
                                        digits = 2), "\n Kurtosis = ",
                   round(kurtosis(binded$DM.rtf), digits = 2))) + 
  theme(text = element_text(family = "Arial", face = "bold", size = 10),
        axis.title.y = element_blank()) +
  scale_x_continuous(limits = c(-0.2, 4.5))





## =============================================================================
## -----------------------------------------------------------------------------
## --------------  Part 7. :  Perform Regression Analysis  ---------------------
## -----------------------------------------------------------------------------
## =============================================================================

  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          ## Text Paragraph
## 7.1 - Calculate HHI, Tobin's q, Leverage and Marketing Intensity ~~~          # 5.3 Dependent Variables
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~          # 5.5 Control Variables
  
compustat_qtr <- compustat_qtr %>%
  group_by(gsector) %>% 
  mutate(hhi_gsector = sum(revty, na.rm = T)) %>% 
  mutate(hhi_gsector = ((revty/hhi_gsector)*100)^2) %>% 
  mutate(hhi_gsector = sum(hhi_gsector, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(tobin_q = (atq+(cshoq*prccq)-ceqq)/atq, 
         lev = (dlttq + dlcq) /atq, 
         MKT = (xsgaq-xrdq)/atq,)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                   
## 7.2 - Create Dataset with relevant regression variables ~~~                   
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regression_data_sentiment <- binded_wSentiment_new %>% 
  left_join(compustat_qtr, by = "id") %>% 
  select(id, firm, total, DM.rtf, DM_Foc, CMO, sentiment, Sector, 
         hhi_gsector, lev, tobin_q, MKT, year, atq, mkvaltq, revtq)  


# Transform relevant variables to logarithms
regression_data_sentiment <- regression_data_sentiment %>% 
  mutate(Rev = log(revtq), 
         Tobins_Q = log(tobin_q), 
         MKVal = log(mkvaltq),
         IndConc = log(hhi_gsector), 
         Size = log(atq)) %>% 
  # Convert Year Variable to factor to control for time effects
  mutate(year_fct = as.factor(year))


# Treat -Inf values created in log process as NA's
regression_data_sentiment[is.na(regression_data_sentiment) | 
                            regression_data_sentiment == "-Inf"] <- 0


# Save as .csv file
write.csv(regression_data_sentiment, 
          "C:/Thesis/regression_data_sentiment.csv")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.2.1 - Create Table with descriptive statistics and correlations ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract variables used in regressions
CorTable <- regression_data_sentiment %>% 
  select(DM.rtf, CMO, Rev, Tobins_Q, lev, MKT,
         MKVal, Size, IndConc)


# Create table, with output as Word Document, to include in Paper
library(apaTables)

apa.cor.table(CorTable, 
              filename = "C:/Thesis/Plots/CorTable.doc", 
              table.number = 1, 
              show.conf.interval = FALSE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                        ## Text Paragraph
## 7.3 - Model 1: Logistic Regression ~~~                                        # 5.6 Regression Models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                        # 6.3.1 Logistic Regression

glm1 <- glm(DM_Foc ~ CMO + sentiment + MKT + IndConc + Size + year_fct + 
             Sector, 
            family = "binomial", 
            data = regression_data_sentiment)

library(stargazer)
stargazer(glm1, type = "text")


# Check Pseudo-R²
library(pscl)

pR2(glm1)


# Compute Odd-Ratio
exp(glm1$coefficients) %>% 
  as.data.frame() %>% 
  view()


# Save Regression results table as html file
stargazer(glm1, type = "html", out = "C:/Thesis/Models/LogReg_model.html")



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.3.1 - Logistic Regression prediction test ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Set train sample to 67% of the original sample
sample_size <- floor(0.67 * nrow(regression_data_sentiment))


# Set a seed to be able to reproduce the partition
set.seed(123)
train_indicator <- sample(seq_len(nrow(regression_data_sentiment)), 
                          size = sample_size)

train_sample <- regression_data_sentiment[train_indicator, ]
test_sample <- regression_data_sentiment[-train_indicator, ]


# define prediction model (similar to glm1)
glm_train <- glm(DM_Foc ~ CMO + sentiment + MKT + IndConc + Size + 
                   year_fct + Sector, 
                 family = "binomial", 
                 data = train_sample)


# Use the model to predict DM_Foc for test sample
test_sample$Predicted_DMFoc_prob <- predict(glm_train, test_sample, 
                                            type = 'response')


# Set threshold to 50%
test_sample$Predicted_DMFoc <- test_sample$Predicted_DMFoc_prob >= 0.5
test_sample$Predicted_DMFoc <- as.numeric(test_sample$Predicted_DMFoc)


# Create classification table
Classification_table <- table(test_sample$DM_Foc, test_sample$Predicted_DMFoc)


# compute hit-rate, to assess predictability of the model
HitRate <- sum(diag(Classification_table))/sum(Classification_table)
HitRate



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                ## Text Paragraphs:
## 7.4 - Models 2 and 3: Multiple Regressions ~~~                                # 5.6 Regression Models
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                # 6.3.2 Multiple Regression Models

# Coonvert year variable to factor variable to control for time-series effects
regression_data_sentiment <- regression_data_sentiment %>% 
  mutate(year_fct = as.factor(year))


## Define models
# Using the "fixest" package allows to estimate the models with 
# heteroskedasticity-robust standard errors

library(fixest)

model1_lm <- feols(Rev ~ DM.rtf + sentiment + DM.rtf*sentiment +
                         IndConc + lev + MKT + Size + year_fct, 
                       data = regression_data_sentiment)

model2_lm <- feols(Tobins_Q ~ DM.rtf + sentiment + 
                     DM.rtf*sentiment + IndConc + lev + MKT + MKVal + 
                     Size + year_fct, 
                       data = regression_data_sentiment)


# The command se = "hetero" estimates heteroskedasticity robust standard errors
etable(model1_lm, model2_lm, se = "hetero", digits = 4, 
       signif.code = "letters", drop = "year_fct",
       fitstat = c("n", "r2", "ar2", "f.stat", "f.p"))


# Unfortunately, the etable command only allows printing out tables in LaTex
# format which is why I used the tab_model command from the "sjPlot" package
# to be able to save the table as html and open it in word. Then, the results
# from the etable command were inserted manually

library(sjPlot)

tab_model(model1_lm, model2_lm, show.ci = F, show.se = T, show.fstat = TRUE,
          show.re.var = T, p.style = "stars", file = 
            "C:/Thesis/Models/sentiment_models_lm_new.html")


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.4.1 - Sample for Interaction Term Interpretation ~~~
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sample <- binded_wSentiment_new %>% 
  select(id, firm, year, tokens_sw, DM.rtf, sentiment) %>% 
  filter(id == 7542619)



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                 ## Text Paragraph:
## 7.5 - Robustness checks of the multiple regression models ~~~                 # Robustness Tests
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define industry-fixed effects models
FEmodel1_sector <- feols(Rev ~ DM.rtf + sentiment + 
                           DM.rtf*sentiment + IndConc + lev + MKT + Size + 
                           year_fct + Sector, 
                         data = regression_data_sentiment)

FEmodel2_sector <- feols(Tobins_Q ~ DM.rtf + sentiment + 
                           DM.rtf*sentiment + IndConc + lev + MKT + 
                           MKVal + Size + year_fct + Sector, 
                         data = regression_data_sentiment)


# The command se = "cluster" estimates cluster-robust standard errors
etable(FEmodel1_sector, FEmodel2_sector, se = "cluster", cluster = "Sector",
       digits = 4, signif.code = "letters", drop = c("year_fct", "Sector"),
       fitstat = c("n", "r2", "ar2", "f.stat", "f.p"))




## =============================================================================
## -----------------------------------------------------------------------------
## ----------------------  Part 8. :  Package Citations  -----------------------
## -----------------------------------------------------------------------------
## =============================================================================

citation(package = "tidyverse")
citation(package = "tidytext")
citation(package = "readxl")
citation(package = "readr")
citation(package = "data.table")
citation(package = "quanteda")
citation(package = "SentimentAnalysis")
citation(package = "pscl")
citation(package = "stargazer")
citation(package = "fixest")
citation(package = "jtools")
citation(package = "moments")
citation(package = "apaTables")
citation(package = "sjPlot")