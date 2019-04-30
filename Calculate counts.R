## Typology - Phonetic Correlates of Word Stress
## Script by: Timo B Roettger
## Description: Takes the corpus as input and calculates counts for respective statistics. Reproduces graphs used in Gordon & Roettger and Roettger & Gordon.
## Date: 18.04.2017


###################
## preprocessing ##
###################

## load in relevant packages
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)


## clean R
rm(list = ls())

## load in data 
xdata <- read.csv("Gordon&Roettger_Stress Corpus.csv")
xdata <- tbl_df(xdata)


##############
## Plot map ##
##############

## load package
library(lingtypology)

## to save to png you need the following package
library(mapview)

## which requires PhantomJS
#webshot::install_phantomjs()

## plot
map <- map.feature(unique(xdata$glottolog), color = "red", stroke.color = "black", 
                   opacity = 0.5, tile = "Esri.WorldGrayCanvas")

## export as png
mapshot(map, file = "/Users/timoroettger/Research/Typology of Word stress/map.png", zoom = 2)


####################
## General counts ##
####################

## get size of corpus
nrow(xdata)

## get number of languages 
unique(xdata$language)


###########################
## methodological counts ##
###########################

## split "corpus" levels ("lab speech" vs. "spontaneous speech") ----
xdata$corpus <- as.character(xdata$corpus)

## split
xdata$corpus_dummy1 <- sapply(strsplit(xdata$corpus,split = ","), function(x){x[1]})
xdata$corpus_dummy2 <- sapply(strsplit(xdata$corpus,split = ","), function(x){x[2]})

## write into binary column
xdata$corpus_lab <- ifelse(xdata$corpus_dummy1 == "lab speech", 1, 
                                       ifelse(xdata$corpus_dummy2 == "lab speech", 1, 0))

xdata$corpus_spont <- ifelse(xdata$corpus_dummy1 == "spontaneous speech", 1, 
                               ifelse(xdata$corpus_dummy2 == "spontaneous speech", 1, 0))

## delete dummy columns
xdata <- xdata[ , -which(names(xdata) %in% c("corpus_dummy1","corpus_dummy2"))]

## split "context" for lab speech ("context phrase", "metalinguistic phrase", "isolation", "text")

## factor to character
xdata$context <- as.character(xdata$context)

## split
xdata$context_dummy1 <- sapply(strsplit(xdata$context,split = ","), function(x){x[1]})
xdata$context_dummy2 <- sapply(strsplit(xdata$context,split = ","), function(x){x[2]})
xdata$context_dummy3 <- sapply(strsplit(xdata$context,split = ","), function(x){x[3]})
xdata$context_dummy4 <- sapply(strsplit(xdata$context,split = ","), function(x){x[4]})

## write into binary columnns
xdata$context_phrase <- ifelse(xdata$context_dummy1 == "context phrase" |
                               xdata$context_dummy2 == "context phrase" | 
                               xdata$context_dummy3 == "context phrase" |
                               xdata$context_dummy4 == "context phrase" | 
                               xdata$context_dummy1 == "metalinguistic phrase" | 
                               xdata$context_dummy2 == "metalinguistic phrase" | 
                               xdata$context_dummy3 == "metalinguistic phrase" | 
                               xdata$context_dummy4 == "metalinguistic phrase" |
                               xdata$context_dummy1 == "text" | 
                               xdata$context_dummy2 == "text" | 
                               xdata$context_dummy3 == "text" | 
                               xdata$context_dummy4 == "text", 1, 0)
                              

xdata$context_isolation <- ifelse(xdata$context_dummy1 == "isolation" | 
                                  xdata$context_dummy2 == "isolation" | 
                                  xdata$context_dummy3 == "isolation" | 
                                  xdata$context_dummy4 == "isolation" , 1, 0)


## delete dummy columns
xdata <- xdata[ , -which(names(xdata) %in% c("context_dummy1", "context_dummy2", "context_dummy3", "context_dummy4"))]

## split "phrase_position" ("isolation", "non-final", "final", "controlled")

## factor to character
xdata$phrase_position <- as.character(xdata$phrase_position)

## split
xdata$phrase_dummy1 <- sapply(strsplit(xdata$phrase_position,split = ","), function(x){x[1]})
xdata$phrase_dummy2 <- sapply(strsplit(xdata$phrase_position,split = ","), function(x){x[2]})

# write into binary column
xdata$phrase_final <- ifelse(xdata$phrase_dummy1 == "final" | 
                             xdata$phrase_dummy2 == "final", 1, 0)

xdata$phrase_non_final <- ifelse(xdata$phrase_dummy1 == "non-final" | 
                                 xdata$phrase_dummy2 == "non-final", 1, 0)

xdata$phrase_unspec <- ifelse(xdata$phrase_dummy1 == "unspecified" | 
                              xdata$phrase_dummy2 == "unspecified", 1 ,0)

xdata$phrase_varied <- ifelse(xdata$phrase_dummy1 == "varied" | 
                              xdata$phrase_dummy2 == "varied", 1, 0)

xdata$phrase_controlled <- ifelse(xdata$phrase_dummy1 == "controlled" | 
                                  xdata$phrase_dummy2 == "controlled", 1, 0)

## delete dummy columns
xdata <- xdata[ , -which(names(xdata) %in% c("phrase_dummy1", "phrase_dummy2"))]

## split "accent_information_structure" ("accented", "unaccented", "implicit contrast", "accent controlled")----

## factor to character
xdata$accent_information_structure <- as.character(xdata$accent_information_structure)

## write into binary column
xdata$accent_confound <- ifelse(xdata$accent_information_structure == "implicit contrast" |
                                xdata$accent_information_structure == "accented", 1, 0)

xdata$accent_controlled <- ifelse(xdata$accent_information_structure == "accent controlled" |
                                  xdata$accent_information_structure == "unaccented", 1, 0)

xdata$accent_unspecified <- ifelse(xdata$accent_information_structure == "unspecified", 1, 0)


## "NAs" to "0" ----
vars <- c("corpus_lab", "corpus_spont", "context_phrase", "context_isolation", "phrase_final", 
           "phrase_non_final", "phrase_unspec", "phrase_varied", "phrase_controlled", "accent_confound", "accent_controlled", "accent_unspecified")

xdata[vars][is.na(xdata[vars])] <- 0 


###################################
## collect methodological counts ##
###################################

## lab vs. spontaneous 
lab_counts <- sum(xdata$corpus_lab, na.rm = T)
spont_counts <- sum(xdata$corpus_spont, na.rm = T)

## isolation vs. phrase counts within lab counts
isolation_counts <- sum(xdata[xdata$corpus_lab == 1,]$context_isolation, na.rm = T)
phrase_counts <- sum(xdata[xdata$corpus_lab == 1,]$context_phrase, na.rm = T)

## phrase position counts within phrase counts
unspecified_position_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1,]$phrase_unspec, na.rm = T)
varied_position_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1,]$phrase_varied, na.rm = T)
controlled_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1,]$phrase_controlled, na.rm = T)
final_position_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1,]$phrase_final, na.rm = T)
non_final_position_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1,]$phrase_non_final, na.rm = T)

## accent counts as a function of position counts
nf_unspecified_accent_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_non_final == 1,]$accent_unspecified, na.rm = T)
nf_accent_controlled_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_non_final == 1,]$accent_controlled, na.rm = T)
nf_accent_confounded_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_non_final == 1,]$accent_confound, na.rm = T)

f_unspecified_accent_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_final == 1,]$accent_unspecified, na.rm = T)
f_accent_controlled_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_final == 1,]$accent_controlled, na.rm = T)
f_accent_confounded_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_final == 1,]$accent_confound, na.rm = T)

c_unspecified_accent_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_controlled == 1,]$accent_unspecified, na.rm = T)
c_accent_controlled_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_controlled == 1,]$accent_controlled, na.rm = T)
c_accent_confounded_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_controlled == 1,]$accent_confound, na.rm = T)

u_unspecified_accent_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_unspec == 1,]$accent_unspecified, na.rm = T)
u_accent_controlled_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_unspec == 1,]$accent_controlled, na.rm = T)
u_accent_confounded_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_unspec == 1,]$accent_confound, na.rm = T)

v_unspecified_accent_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_varied == 1,]$accent_unspecified, na.rm = T)
v_accent_controlled_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_varied == 1,]$accent_controlled, na.rm = T)
v_accent_confounded_counts <- sum(xdata[xdata$corpus_lab == 1 & xdata$context_phrase == 1 & xdata$phrase_varied == 1,]$accent_confound, na.rm = T)


## create data.frame with count values
corpus <- c("spontaneous speech", rep("lab speech", 16))
context <- c("phrase", "isolation", rep("phrase", 15))
position <- c(rep(NA, 2), rep("unspecified", 3), rep("non final", 3), rep("final", 3), rep("controlled", 3), rep("varied", 3))
accent <- c(rep(NA, 2), rep(c("accent unspecified", "accent controlled", "accent confounded"), 5))
counts <- c(spont_counts, isolation_counts, 
            u_unspecified_accent_counts, u_accent_controlled_counts, u_accent_confounded_counts,
            nf_unspecified_accent_counts, nf_accent_controlled_counts, nf_accent_confounded_counts,
            f_unspecified_accent_counts, f_accent_controlled_counts, f_accent_confounded_counts,
            c_unspecified_accent_counts, c_accent_controlled_counts, c_accent_confounded_counts,
            v_unspecified_accent_counts, v_accent_controlled_counts, v_accent_confounded_counts)
counts_df <- data.frame(corpus, context, position, accent, counts)

## subset for plots
counts_df_sum1 <-
  counts_df %>%
  group_by(corpus, context) %>%
  summarise(counts = sum(counts))

counts_df_sum2 <-
  counts_df %>%
  filter(context == "phrase" & position != "NA") %>%
  group_by(position, accent) %>%
  summarise(counts = sum(counts))


################################
## plot methodological counts ##
################################

## plot corpus and context
corpus_context_plot <-
ggplot(counts_df_sum1, aes(x = corpus, y = counts, fill = context, label = counts)) +
      geom_bar(stat = "identity") +
      geom_text(aes(y = counts), size = 5, colour = c("white",  "black", "white"), hjust = 0.5, position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = c("#cccccc", "#666666"),
                        name = "Elicitation context\n",
                        breaks = c("NA", "isolation", "phrase"),
                        labels = c("spontaneous speech", "isolation", "phrase")) +
  coord_flip() +   
  theme_classic() +
      ylab("\nnumber of studies") +
      xlab("\n corpus type") +
      theme(legend.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            axis.title.y = element_blank(),
            axis.title = element_text(size = 20, face = "bold"))

#ggsave(filename = "corpus_context_plot.png", plot = corpus_context_plot, width = 10, height = 4.5, dpi = 600)


# plot position and accent
position_accent_plot <-
ggplot(counts_df_sum2, aes(x = position, y = counts, fill = accent, label = counts)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#CC6666", "#9999CC", "#66CC99"),
                    name = "Accentuation Status\n",
                    breaks = c("NA", "accent confounded", "accent controlled", "accent unspecified"),
                    labels = c("NA", "potentially confounded", "controlled", "unspecified")) +
  scale_x_discrete(labels=c("non final" = "non-final", "final" = "final", "controlled" = "controlled (both)",
                            "unspecified" = "unspecified",  "varied" = "varied")) +
  coord_flip() + 
  theme_classic() +
  ylab("\nnumber of studies") +
  xlab("\nphrase position of target word") +
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 20, face = "bold"))

#ggsave(filename = "position_accent_plot.png", plot = position_accent_plot, width = 12, height = 4,  dpi = 600)


#######################################
## plot acoustic measurements counts ##
#######################################

## collect measurements counts 
xdata$duration_count <- as.factor(ifelse(grepl("greater", xdata$duration) | grepl("smaller", xdata$duration), "effect", 
                         ifelse(grepl("ns", xdata$duration), "no effect", "not tested")))
xdata$f0_count <- as.factor(ifelse(grepl("greater", xdata$f0) | grepl("higher", xdata$f0) | grepl("lower", xdata$f0), "effect",     
                                  ifelse(grepl("ns", xdata$f0), "no effect", "not tested")))
xdata$intensity_count <- as.factor(ifelse(grepl("greater", xdata$intensity), "effect", 
                               ifelse(grepl("ns", xdata$intensity), "no effect", "not tested")))
xdata$formants_count <- as.factor(ifelse(xdata$formants == "ns" | xdata$formants == "ns F1,ns F2", "no effect", 
                               ifelse(grepl("F1", xdata$formants), "effect", "not tested")))
xdata$spectral_tilt_count <- as.factor(ifelse(grepl("greater", xdata$spectral_tilt) | grepl("lower", xdata$spectral_tilt) | grepl("phon-dB", xdata$spectral_tilt), "effect",
                              ifelse(grepl("ns", xdata$spectral_tilt), "no effect","not tested")))

## create data.frame with measurement count values
measurement_df <- xdata %>%
  select(duration_count, f0_count, intensity_count, formants_count, spectral_tilt_count) %>%
  gather(measurement, category, c(duration_count, f0_count, intensity_count, formants_count, spectral_tilt_count)) %>% 
  group_by(measurement, category) %>%
  summarise(count = n())
  
# as factor
measurement_df$measurement = as.factor(measurement_df$measurement) 
measurement_df$category = as.factor(measurement_df$category) 

measurement_df$category <- factor(measurement_df$category, levels = c("not tested", "no effect", "effect"))
measurement_df$measurement <- factor(measurement_df$measurement, levels = c("duration_count", "f0_count", "intensity_count",
                                                                         "formants_count", "spectral_tilt_count"))

## plot measurements counts ####
measurement_plot <-
ggplot(measurement_df, aes(x = measurement, y = count, fill = category, label = count)) +
  geom_bar(stat = "identity", colour = "black") +
  geom_text(aes(y = count), size = 5, colour = c("#CCCCCC","#000000","#000000","#CCCCCC","#000000","#000000",
                                                 "#CCCCCC","#000000","#000000","#CCCCCC","#000000","#000000",
                                                 "#CCCCCC","#000000","#000000"),
            hjust = 0.5, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FFFFFF", "#CCCCCC", "#000000"),
                    name = "",
                    breaks = c("not tested", "no effect", "effect"),
                    labels = c( "not measured", "no effect", "effect")) +
  scale_x_discrete(labels = c("duration_count" = "duration", "f0_count" = "f0",
                            "intensity_count" = "intensity", "formants_count" = "formants",
                            "spectral_tilt_count" = "spectral tilt")) +
  theme_classic() +
  ylab("number of studies\n") +
  xlab("\n acoustic measurement") +
  theme(#legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"))

#ggsave(filename = "measurement_plot.png", plot = measurement_plot, width = 11, height = 4.8)


########################
## plot sample counts ##
########################

## sample distribution
## relevant columns to numeric
xdata$no_words <- as.numeric(xdata$no_words)
xdata$repetitions <- as.numeric(xdata$repetitions)
xdata$no_speakers <- as.numeric(xdata$no_speakers)

## function for multiple plots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## subject plot
subjects_hist <- 
  ggplot(xdata, aes(x = no_speakers)) +
  geom_histogram(binwidth = 1, colour = "darkgrey", fill = "darkgrey") +
  theme_classic() +
  xlab("\nnumber of speakers") +
  ylab("\n") +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 25, face = "bold"))

## word plot
words_hist <- 
  ggplot(xdata, aes(x = no_words)) +
  geom_histogram(colour = "darkgrey", fill = "darkgrey", binwidth = 10) +
  theme_classic() +
  xlab("\nnumber of words") +
  ylab("number of studies\n") +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 25, face = "bold"))

## rep plot
reps_hist <- 
  ggplot(xdata, aes(x = repetitions)) +
  geom_histogram(colour = "darkgrey", fill = "darkgrey", binwidth = 1) +
  theme_classic() +
  xlab("\nnumber of repetitions") +
  ylab("\n") +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 25, face = "bold"))

## plot all together
multiplot(subjects_hist, words_hist, reps_hist, cols = 1)


