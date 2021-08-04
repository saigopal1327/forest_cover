##################################################
# installing/loading libraries required for report
##################################################


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org") # now a part 
                                                                                                # of tidyverse
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(data.table)
library(caret)
library(knitr)
library(grid)
library(ggridges)
library(GGally)
library(RColorBrewer)
library(e1071)        # confusionMatrix in caret required this package

################################
# data download and reading file
################################

dl <- tempfile()

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz", dl)

readLines(gzfile(description = dl), n = as.integer(5)) # determining separators; n = 'specify the number of 
                                                       # lines to be printed

dat<- fread(text = readLines(gzfile(description = dl)), sep = ",") # fread function
                                                                   # detect header automatically

rm(dl)

################################################
# dataset dimensions, headers and missing values
################################################

dim(dat) # dimension (rows x columns)

dat[c(1:5), c(1:5)] # indexing first five rows and columns

any(is.na(dat)) # checking for missing values. any function returns TRUE if any TRUE is returned with
                # the function is.na. is.na returns TRUE if there are any missing value

###################################################
# variable description & data wrangling for headers 
###################################################

dl <- tempfile()

download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info", dl)

description_dat<- readLines(gzfile(description = dl)) # saving the test document

rm(dl)

#######################
# variables description
#######################
variables_desc<- description_dat[174:186] # names of variables were printed in these 
                                          # lines with four parameters (names, variable type, unit 
                                          # and description)


str_trunc(variables_desc, 80, side = "right") # prints only the specified width (80 here) from
                                              # side specified (right)
##################
# headers 1 to 10
##################

header_1_10<- str_split(variables_desc, pattern = "\\s+", simplify = T) # pattern \\s+ means 
                                                                        # one or more white space
                                                                        # simplifies = T, returns output 
                                                                        # as a table

header_1_10<- header_1_10[,1][1:10] # specifying the first 10 rows of the first column

remove(variables_desc)

##################
# headers 10 to 14
##################
header_11_14<- c("rawah", "neota", "comanche_peak", "cache_la_poudre")

header_11_14

##############################
# soil types description table
##############################

header_15_54<- description_dat[200:239] # names of variable 15 to 54 (soil type) were printed in these 
                                        # lines with three parameters (study code, climatic and geologic code, 
                                        # and description)

header_15_54<- str_split(header_15_54, pattern = "\\t", simplify = T) # three parameter were separated by  
                                                                      # the string "\t", additional \ is to  
                                                                      # escape the backslash

soil_table<- data.frame(Study_code = header_15_54[,2], ELU_code = header_15_54[,3],
                          Description = header_15_54[,5])

kable(soil_table[c(1, 2, 7, 8),], 
      caption = "Soil Types Codes & Description") # printing the specified rows of soil_table
                                                  # caption = title for the table

remove(soil_table)

##################
# headers 15 to 55
##################

header_15_54<- str_trim(header_15_54[,3], side = "left") # str_trim removes white space in specified 'side'

header_15_54<- paste0("soil_type_", header_15_54) # paste0 attaches the two strings provided as arguments

header_15_54

header_55<- "Cover_Type" # the dependent variable

###################
# attaching headers
###################

headers<- c(header_1_10, header_11_14, header_15_54, header_55) # creating vector with header names

colnames(dat)<- headers # assigning names to columns

remove(header_1_10, header_11_14, header_15_54, header_55, headers) # removing the headers vectors

###########################
# soil classification table
###########################

soil_type_tab<- description_dat[242:249] # indexing the line containing the info, contains both
                                         # climatic and geologic codes

soil_type_tab<- str_trim(soil_type_tab, side = "both") # removing white space on both sides

climatic_zones<- str_extract_all(soil_type_tab, 
                                 pattern = "^[1-8].\\s+[a-z]+\\s*[a-z]*\\s*[a-z]*\\s*[a-z]*", 
                                 simplify = T) # pattern is for a string starting with one digit,
                                               # followed by a period and containing
                                               # one to three words separated by white space

climatic_zones<- str_squish(climatic_zones[,1]) # removes space right, left and center

geologic_zones<- str_remove(soil_type_tab, 
                            pattern = "^[1-8].\\s+[a-z]+\\s*[a-z]*\\s*[a-z]*\\s*[a-z]*") # removing climatic
                                                                              # zones codes to extract 
                                                                              # the remaining geologic codes     

geologic_zones<- str_squish(geologic_zones) # removes space right, left and center


soil_type_tab<- cbind('Climatic Zones' = climatic_zones, 'Geologic Zones' = geologic_zones) # creating table

soil_type_tab %>% kable(caption = "ELU Soil Classifciation") # caption = title for the table

remove(soil_type_tab, description_dat, climatic_zones, geologic_zones) # removing objects used for creating above table


##############################################
# converting Slope in degree to percent slope
##############################################

dat<- dat %>% mutate(Slope = tan(Slope * pi / 180) * 100) # multiplying degree's by pi/180
                                                          # convert it to radian

##################################################################
# azimuth figure and creating new variables from variable "Aspect"
##################################################################

df<- data.frame(x = c(0, 0, 0, -1, 1), y =c(0, -1, 1, 0, 0)) # dataframe for creating axis for four quadrants

df_labels<- data.frame(a = c("North", "0°/360°", "East", "90°",
                             "South", "180°", "West", "270°", "North & West",
                             "North & East", "South & East", "South & West"),
                       x = c(-0.1, 0.1, 1.1, 1.1, -0.1, 0.1, 
                             -1.1, -1.1, -0.7, 0.7, 0.7, -0.7),
                       y = c(1.1, 1.1, 0.05, -0.05, -1.1, -1.1, 
                             0.05, -0.05, 0.7, 0.7, -0.7, -0.7)) # x & y values
                                                                 # to specify the location of the 
                                                                 # labels (varible = a) in the four quadrants

ggplot(data = df, aes(x = x, y = y)) +
  geom_point() +
  geom_segment(aes(x = 0, y = 1, xend = 0, yend = -1), arrow = arrow(ends = "both")) + # horizontal axis with
                                                                                       # arrows at both ends
  geom_segment(aes(x = -1, y = 0, xend = 1, yend = 0), arrow = arrow(ends = "both")) + # vertical axis (arrows)
  theme_minimal() + theme(axis.text = element_blank(),
                          axis.title = element_blank()) +
  geom_text(data = df_labels, aes(x, y, label = a)) +     # assigning labels to axis
  ggtitle("Figure 1: Azimuth Degree")                     # title of the figure

grid.circle(x = 0.5, y = 0.5, r = 0.25) # adding a circle to figure, 
                                        # can adjust position by changing x and y values

remove(df, df_labels)                  # removing dataframes used to make figure

##############################################
# creating two variable to capture aspect data
##############################################

dat<- dat %>% 
  mutate(north = ifelse(Aspect %in% c(0:90) | Aspect %in% c(270:360), 1, 0), # aspect values between 0 and 90
                                                                            # or 270 and 360 = 1 (Yes for north)
         east = ifelse(Aspect %in% c(0:180), 1, 0)) %>% # aspect value between 0 and 180 = 1 (Yes for east)
  select(- Aspect)                                      # removing the variable "Aspect"


####################################################
# converting 40 soil type into 11 general soil types
####################################################

dat<- dat %>% mutate(soil_type_27 = ifelse(soil_type_2702 == 1 | soil_type_2703 == 1 |     # if any sub-types of soil 27 = 1, 
                                                                                           # then 1  
                                             soil_type_2704 == 1 | soil_type_2705 == 1 |   # similar logic for all types
                                             soil_type_2706 == 1 | soil_type_2717 == 1, 1, 0),
                     soil_type_35 = ifelse(soil_type_3501 == 1 | soil_type_3502 == 1, 1, 0),
                     soil_type_42 = soil_type_4201,
                     soil_type_47 = ifelse(soil_type_4703 == 1 | soil_type_4704 == 1 |
                                             soil_type_4744 == 1 | soil_type_4758 == 1, 1, 0),
                     soil_type_51 = ifelse(soil_type_5101 == 1 | soil_type_5151 == 1, 1, 0),
                     soil_type_61 = ifelse(soil_type_6101 == 1 | soil_type_6102 == 1, 1, 0),
                     soil_type_67 = soil_type_6731,
                     soil_type_71 = ifelse(soil_type_7101 == 1 | soil_type_7102 == 1 |
                                             soil_type_7103 == 1, 1, 0),
                     soil_type_72 = ifelse(soil_type_7201 == 1 | soil_type_7202 == 1, 1, 0),
                     soil_type_77 = ifelse(soil_type_7700 == 1 | soil_type_7701 == 1 |
                                             soil_type_7702 == 1 | soil_type_7709 == 1 |
                                             soil_type_7710 == 1 | soil_type_7745 == 1 |
                                             soil_type_7746 == 1 | soil_type_7755 == 1 |
                                             soil_type_7756 == 1 | soil_type_7757 == 1 |
                                             soil_type_7790 == 1, 1, 0),
                     soil_type_87 = ifelse(soil_type_8703 == 1 | soil_type_8707 == 1 |
                                             soil_type_8708 == 1 | soil_type_8771 == 1 |
                                             soil_type_8772 == 1 | soil_type_8776 == 1 , 1, 0)) %>%
  select(- c(14:53)) # removing the original 40; it will cause perfect co-linearity with new 11 variables

###########################################
# labels to outcome variable and prevalence
###########################################

dat$Cover_Type<- factor(dat$Cover_Type,                                # converting integer into factor class
                        levels = c("1", "2", "3", "4", "5", "6", "7"), # these levels/codes were in the info doc
                        labels = c("Spruce-Fir",                       # assigning labels to each code above
                                   "Lodgepole Pine",
                                   "Ponderosa Pine",
                                   "Cottonwood/Willow",
                                   "Aspen",
                                   "Douglas-fir",
                                   "Krummholz"))


dat %>% group_by(Cover_Type) %>%
  summarise(Number = n(), Proportion = n()/nrow(dat)) %>% # summarizing number of observation in each class
                                                          # and dividing by total observation to get  
                                                          # proportion/prevalence
  arrange(desc(Proportion)) %>%                           # arranging the type by descending prevalence                     
  kable(digits = 3, caption = "Prevalence of Cover Types") # digits = number of significant digits
                                                           # to be displayed; caption = title for the table


########################################
# splitting data into train and test set
########################################

set.seed(27, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(27)`

test_index<- createDataPartition(y = dat$Cover_Type, p = 0.02, 
                                 times = 1, list = F)   

train_set<- dat[- test_index, ] # creating train set

test_set<- dat[test_index, ]   # creating test set

nrow(train_set)               # number of rows in train set

nrow(test_set)                # number of rows in test set

remove(test_index, dat)           # removing test index and full dataset

###########################################
# exploratory data analysis
###########################################

#################################
# cover types and wilderness area
#################################
wilderness_dat<- train_set %>% 
  select(rawah, neota, comanche_peak, cache_la_poudre, Cover_Type) %>% # selecting only required variables
  mutate(Wildnerness = case_when(                              # creating one variable with all four areas
    rawah == 1 ~ "Rawah",
    neota == 1 ~ "Neota",
    comanche_peak == 1 ~ "Comanche Peak",
    cache_la_poudre == 1 ~ "Cache la Poudre"
  )) 

kable(prop.table(table(wilderness_dat$Wildnerness, wilderness_dat$Cover_Type), margin = 2),
      digits = 3, caption = "Cover Types by Wilderness Areas")  # margin = 2 returns within column %; 
                                                                # digits = number of significant digits
                                                                # caption = title

remove(wilderness_dat) # removing the wilderness dataset

############################
# cover types and elevation
############################
train_set %>% mutate(Cover_Type = reorder(Cover_Type, 
                                          Elevation, FUN = mean)) %>% # reordering factor levels based on mean 
                                                                      # elevation- visualization principle
  ggplot(aes(x = Cover_Type, y = Elevation)) +                       # mapping x and y
  geom_boxplot() +                                                   # specifying the plot type/geometry
  ggtitle("Figure 2: Distribution of Elevation by Cover Type") +     # title for figure
  xlab("Forest Cover Type") +                                       # x axis label
  stat_summary(fun = mean, geom="point", size = 2, 
               col = "brown") +                           # adding mean to figure
  theme_minimal() +                                       # different themes for background and axes appearance
  theme(title = element_text(size = 8), axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6))                # changing size of the text in figure 

####################################
# cover types and quadrants (aspect)
####################################

aspect_dat<- train_set %>% select(Cover_Type, north, east) %>% 
  mutate(Quadrant = case_when(                         # creating one variable with four quadrants
  north == 1 & east == 1 ~ "North & East",             # if north and east =yes then north & east
  north == 1 & east == 0 ~ "North & West",             # similar logic for other three
  north == 0 & east == 1 ~ "South & East",
  north == 0 & east == 0 ~ "South & West"
)) 


kable(prop.table(table(aspect_dat$Quadrant, aspect_dat$Cover_Type), margin = 2),
      digits = 3, caption = "Cover Types by Aspect Quadrants")  # margin = 2 returns within column %
                                                                # digits = number of significant digits
                                                                # caption = title
remove(aspect_dat)

#######################
# cover type and slope
#######################

train_set %>% mutate(Cover_Type = reorder(Cover_Type, 
                                          Slope, FUN = mean)) %>% # reordering the factor levels based  
                                                               # on mean slope -visualization principle
  ggplot(aes(x = Slope, y = Cover_Type)) +                    # mapping and y axis variable
  geom_density_ridges() +                                     # type of plot/ geometry - ridge plot
  xlab("Percent Slope") +
  ylab("Cover Types") +                                       # x and y axis labels
  ggtitle("Fiure 3: Distribution of Slope by Cover Types") +  # figure title
  theme_minimal() +                                  # themes for background, axes ], legends.. appearance
  theme(title = element_text(size = 8), axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))         # changing text size from default

######################################################
# cover type and distances to roadway and fire points
######################################################
Cover_Types<- c("Spruce-Fir", "Lodgepole Pine", "Ponderosa Pine",
                "Cottonwood/Willow", "Aspen", "Douglas-fir","Krummholz") # creating a vector with names of 
                                                                         # cover types

tab<- train_set %>% group_by(Cover_Type) %>%
  summarise(Mean = mean(Horizontal_Distance_To_Roadways), # computing the mean and sd of distance roadway
                                                          # by cover types ( 7 X 3 dataframe)
            SD = sd(Horizontal_Distance_To_Roadways))

tab<- t(tab[,2:3])                                       # indexing the 2nd and 3rd column  
                                                         # this return a  matrix with 2 x 7 rows mean and sd

colnames(tab)<- Cover_Types                              # assigning column-names/headers to the matrix above

tab1<- train_set %>% group_by(Cover_Type) %>%                  # same procedure as above for distance to fire point
  summarise(Mean = mean(Horizontal_Distance_To_Fire_Points), 
            SD = sd(Horizontal_Distance_To_Fire_Points))

tab1<- t(tab1[,2:3])

colnames(tab1)<- Cover_Types

kable(tab, caption = "Distance to Roadways", digits = 3)  # creating table with title and 
                                                          # digits = number of significant digits to display

kable(tab1, caption = "Distance to Fire Points", digits = 3)

remove(Cover_Types, tab, tab1)                         # removing the objects created for tables

######################################
# cover type and distance to hydrology
######################################

hydro_dat<- train_set %>% 
  select(Horizontal_Distance_To_Hydrology, 
         Vertical_Distance_To_Hydrology, Cover_Type) # creating a dataframe with distance 
                                                     # to hydrology and cover type



hydro_dat<- gather(data = hydro_dat, key = distance_type, # collapsing the two distance column into one
                                                          # variable named -distance (value = distance)
                                                          # key = distance_type holds the headers of the two
                                                          # original variables
                   value = distance, - Cover_Type)


hydro_dat$distance_type<- factor(hydro_dat$distance_type, 
                                 labels = c("Horizontal", "Vertical")) # reducing the length of the
                                 # distance_type class (eg- Horizontal_Distance_To_Hydrology to Horizontal)

hydro_dat %>% mutate(Cover_Type = reorder(Cover_Type, 
                                          distance, FUN = mean)) %>% # reordering factor levels
  ggplot(aes(x = Cover_Type, y = distance)) + # mapping x and y variables
  geom_boxplot(aes(col = distance_type)) +                # specifying plot type, 
                                                          # col = distance- automatically 
                                                          # groups the box by distance_type (horizo.. & vert..)
  xlab("Forest Cover Type") +
  ylab("Distance in meters") +                            # x and y axis label
  ggtitle("Figure 4: Distance to Hydrology by Cover Type") + # figure title
  scale_color_discrete(name = "Distance Type") +           # assigning the header for legend 
  theme_minimal() +
  theme(title = element_text(size = 8), axis.title = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) # changing test size to 8; tilting the x axis
                                                                     # text 45 degree. 

remove(hydro_dat)

##################################
# cover types and hillshade index
##################################

Cover_Types<- c("Spruce-Fir", "Lodgepole Pine", "Ponderosa Pine",
                "Cottonwood/Willow", "Aspen", "Douglas-fir","Krummholz") # creating a vector with names of 
                                                                         # cover types
tab<- train_set %>% group_by(Cover_Type) %>%
  summarise(Mean = mean(Hillshade_9am), 
            SD = sd(Hillshade_9am))      # computing the mean and sd of hillshade index at 9 am
                                         # by cover types ( 7 X 3 dataframe)

tab<- t(tab[,2:3])                       # indexing the 2nd and 3rd column  
                                         # this return a  matrix with 2 x 7 rows mean and sd

colnames(tab)<- Cover_Types              # assigning column names to matrix created above

tab1<- train_set %>% group_by(Cover_Type) %>%   # same procedures as above for shade index at noon
  summarise(Mean = mean(Hillshade_Noon), 
            SD = sd(Hillshade_Noon))

tab1<- t(tab1[,2:3])

colnames(tab1)<- Cover_Types

tab2<- train_set %>% group_by(Cover_Type) %>%  # same procedures as above for shade index at 3 pm
  summarise(Mean = mean(Hillshade_3pm), 
            SD = sd(Hillshade_3pm))

tab2<- t(tab2[,2:3])

colnames(tab2)<- Cover_Types

kable(tab, caption = "Hillshade Index 9 AM", digits = 3)  # tabulating with 3 significant digit and title
kable(tab1, caption = "Hillshade Index Noon", digits = 3)
kable(tab2, caption = "Hillshade Index 3 PM", digits = 3)
remove(tab, tab1, tab2)

############################################
# correlation between quantitative variables
############################################

cor_dat<- train_set %>% 
  select(Elevation, Slope, Horizontal_Distance_To_Hydrology, # selecting all quantitative variables
         Vertical_Distance_To_Hydrology, Horizontal_Distance_To_Fire_Points,
         Horizontal_Distance_To_Roadways, Hillshade_9am, Hillshade_Noon,
         Hillshade_3pm) %>%
  rename(elev = Elevation, slope = Slope, hyd_H = Horizontal_Distance_To_Hydrology,
         hyd_V = Vertical_Distance_To_Hydrology, fire = Horizontal_Distance_To_Fire_Points,
         road = Horizontal_Distance_To_Roadways, shad_9 = Hillshade_9am, shad_n = Hillshade_Noon,
         shad_3 = Hillshade_3pm) # renaming with short form to fit page

ggcorr(cor_dat, low = "#FF0000", mid = "#FFFFFF", high = "#00FF00", label = TRUE, # low hex code is red
       label_round = 2, legend.position = "none") +    # mid = white and high = green, cor significant digits =2
  ggtitle("Figure 6: Correlation Matrix")              # removing legend and adding title to figure                      

########################################
# identifying the variable to be removed
#######################################

findCorrelation(cor(cor_dat), cutoff = 0.75, names = T) # cutoff was most reported in literature
remove(cor_dat)                                         # names = T, return name of variable otherwise col number

####################################################################
# computing propotion of present in each soil type & nearoZerovar fun
###################################################################

train_set %>% select(17:27) %>% # selecting all soil types
  summarise_all(.funs = function(x) mean(x == 1)) # calculating proportion

nearZeroVar(train_set %>% select(17:27), names = T) # names = T retirn variable name, otherwie col number

#############################
# heatmap soil and cover types
#############################

soil_dat<- train_set %>% 
  select(14, 17, 20, 25:27) %>%  # selecting cover type and soil types column (excluding types flagged above)
  group_by(Cover_Type) %>%        
  summarise_all(.funs = sum)   # since coded as 0 and 1, sum will tabulate each soil type by cover type

cover_names<- soil_dat$Cover_Type # creating a vector for assigning row names to matrix

soil_matrix<- as.matrix(soil_dat[, c(2:6)]) # converting df into matrix, 1st character col removed

rownames(soil_matrix)<- cover_names      # assigning row name to matrix

soil_matrix<- sweep(soil_matrix, MARGIN = 1, 
                    STATS = rowSums(soil_matrix), FUN = "/") # dividing each row by row total to
                                                             # compute within cover type proportion

hmcol <- colorRampPalette(brewer.pal(9, "RdBu"))(20)  # heatmap color palette from red to blue

par(cex.main = 0.5) # adjusting the header size for figure below. This heatmap is a base plot

heatmap(soil_matrix, Rowv = NA, Colv = NA, col = hmcol,      # Rowv and Colv,removes dendrogram
        scale = "none", cexRow = 0.75, cexCol = 0.75,        # scaling was not done, default is row
        main = "Figure 7: Heatmap of Soil Types and Cover Types") # figure title

remove(soil_dat, soil_matrix, hmcol) # removing objects

########################################################
# removing variables flagged above in train and test set
#######################################################

train_set<- train_set %>%
  select(-c(8, 18:19, 21:24)) %>%  # 8 = hillshade_3, 18 = soil_35, 19 = soil_42, 21 to 24 are
  # soil_types 51,61, 67 and 71 respectively
  mutate(Cover_Type = factor(case_when(   # changing names to abbreviations to make 
    # confusion matrix fit in a line
    Cover_Type == "Spruce-Fir" ~ "SF",
    Cover_Type == "Lodgepole Pine" ~ "LP",
    Cover_Type == "Ponderosa Pine" ~ "PP",
    Cover_Type == "Cottonwood/Willow" ~ "CW",
    Cover_Type == "Aspen" ~ "AS",
    Cover_Type == "Douglas-fir" ~ "DF",
    Cover_Type == "Krummholz" ~ "KR"
  )))

test_set<- test_set %>%            # same as train set
  select(-c(8, 18:19, 21:24)) %>%
  mutate(Cover_Type = factor(case_when(
    Cover_Type == "Spruce-Fir" ~ "SF",
    Cover_Type == "Lodgepole Pine" ~ "LP",
    Cover_Type == "Ponderosa Pine" ~ "PP",
    Cover_Type == "Cottonwood/Willow" ~ "CW",
    Cover_Type == "Aspen" ~ "AS",
    Cover_Type == "Douglas-fir" ~ "DF",
    Cover_Type == "Krummholz" ~ "KR"
  )))

names(test_set) # printing the names of variable to be included in models

########################
# multinomial regression
########################

scale2 <- function(x) (x - mean(x)) / sd(x) # the scale function in base returns object of
# class = matrix; this custom function  
# preserves the vector class

multinom_train<- train_set %>% 
  select(- rawah) %>%                             # dropping 'rawah' to avoid collinearity      
  mutate_at(c(1:8), .funs = scale2)               # scaling the continuous variables

multinom_test<- test_set %>% select(- rawah, -Cover_Type) %>% # removing dependent variable
  mutate_at(c(1:8), .funs = scale2)                

multinom_fit<- multinom(Cover_Type ~ ., data = multinom_train, trace = FALSE) # model fit

y_hat_mutlinom<- predict(multinom_fit, newdata = multinom_test, type = "class")

accuracy_multinom<- confusionMatrix(y_hat_mutlinom, test_set$Cover_Type)

accuracy_multinom$table

accuracy_multinom$overall[c("Accuracy", "AccuracyLower", "AccuracyUpper")]

remove(multinom_fit, multinom_test, multinom_train, y_hat_mutlinom, accuracy_multinom)

########################################################
# KNN creating 3 folds and first fold cross validation
#######################################################

set.seed(27, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(27)`

folds<- createFolds(y = train_set$Cover_Type, k = 50, list = T)[1:3] # indexing first 3 vectors

cv_fold1<- train_set %>% slice(folds$Fold01) %>%  # selecting fold 1 observations
  select(-Cover_Type, -rawah) %>%   # cover type removed - dependent, rawah removed
  # as it is redundant for wilderness information
  mutate_at(c(1:8), .funs = scale2) # scaling continuous variables (preprocessing for KNN)

x1<- train_set %>% select(- Cover_Type, - rawah) %>%  # same as above but for training
  slice(- folds$Fold01) %>%                           # removing fold 1 observations
  mutate_at(c(1:8), .funs = scale2) %>% as.matrix()

y1<- train_set$Cover_Type[- folds$Fold01]  # creating a vector of class

knn_fit1_k3<- knn3(x1, y1, k = 3)         # knn fit using k = 3

y_hat1_k3<- predict(knn_fit1_k3, cv_fold1, type = "class") # prediction in CV fold 1 set

f1_k3<- confusionMatrix(y_hat1_k3, 
                        train_set$Cover_Type[folds$Fold01]) # computing performance metrics

remove(knn_fit1_k3, y_hat1_k3) # removing fitted model (k=3) and its predictions

knn_fit1_k7<- knn3(x1, y1, k = 7) # knn fit using k = 7
y_hat1_k7<- predict(knn_fit1_k7, cv_fold1, type = "class")
f1_k7<- confusionMatrix(y_hat1_k7, train_set$Cover_Type[folds$Fold01])
remove(knn_fit1_k7, y_hat1_k7)

knn_fit1_k11<- knn3(x1, y1, k = 11) # knn fit using k = 11
y_hat1_k11<- predict(knn_fit1_k11, cv_fold1, type = "class")
f1_k11<- confusionMatrix(y_hat1_k11, train_set$Cover_Type[folds$Fold01])
remove(knn_fit1_k11, y_hat1_k11)

knn_fit1_k15<- knn3(x1, y1, k = 15) # knn fit using k = 15
y_hat1_k15<- predict(knn_fit1_k15, cv_fold1, type = "class")
f1_k15<- confusionMatrix(y_hat1_k15, train_set$Cover_Type[folds$Fold01])
remove(knn_fit1_k15, y_hat1_k15, x1, y1, cv_fold1)

accuracy_fold1<- data.frame(Fold = "Fold 1", K = c(3,7,11, 15), 
                            Accuracy = c(f1_k3$overall["Accuracy"], f1_k7$overall["Accuracy"],
                                         f1_k11$overall["Accuracy"], f1_k15$overall["Accuracy"]))
remove(f1_k3,f1_k7,f1_k11, f1_k15)
accuracy_fold1


########################################
# KNN fold 2 and 3; accuracy vs k plot
#########################################

cv_fold2<- train_set %>% slice(folds$Fold02) %>%   # same as above for fold 2
  select(-Cover_Type, -rawah) %>%
  mutate_at(c(1:8), .funs = scale2)
x2<- train_set %>% select(- Cover_Type, - rawah) %>%
  slice(- folds$Fold02) %>%
  mutate_at(c(1:8), .funs = scale2) %>% as.matrix()
y2<- train_set$Cover_Type[- folds$Fold02]

knn_fit2_k3<- knn3(x2, y2, k = 3)
y_hat2_k3<- predict(knn_fit2_k3, cv_fold2, type = "class")
f2_k3<- confusionMatrix(y_hat2_k3, train_set$Cover_Type[folds$Fold02])
remove(knn_fit2_k3, y_hat2_k3)

knn_fit2_k7<- knn3(x2, y2, k = 7)
y_hat2_k7<- predict(knn_fit2_k7, cv_fold2, type = "class")
f2_k7<- confusionMatrix(y_hat2_k7, train_set$Cover_Type[folds$Fold02])
remove(knn_fit2_k7, y_hat2_k7)

knn_fit2_k11<- knn3(x2, y2, k = 11)
y_hat2_k11<- predict(knn_fit2_k11, cv_fold2, type = "class")
f2_k11<- confusionMatrix(y_hat2_k11, train_set$Cover_Type[folds$Fold02])
remove(knn_fit2_k11, y_hat2_k11)

knn_fit2_k15<- knn3(x2, y2, k = 15)
y_hat2_k15<- predict(knn_fit2_k15, cv_fold2, type = "class")
f2_k15<- confusionMatrix(y_hat2_k15, train_set$Cover_Type[folds$Fold02])
remove(knn_fit2_k15, y_hat2_k15, x2, y2, cv_fold2)

accuracy_fold2<- data.frame(Fold = "Fold 2", K = c(3,7,11, 15), 
                            Accuracy = c(f2_k3$overall["Accuracy"], f2_k7$overall["Accuracy"],
                                         f2_k11$overall["Accuracy"], f2_k15$overall["Accuracy"]))
remove(f2_k3,f2_k7,f2_k11, f2_k15)

cv_fold3<- train_set %>% slice(folds$Fold03) %>%     # same as above for fold 3
  select(-Cover_Type, -rawah) %>%
  mutate_at(c(1:8), .funs = scale2)
x3<- train_set %>% select(- Cover_Type, - rawah) %>%
  slice(- folds$Fold03) %>%
  mutate_at(c(1:8), .funs = scale2) %>% as.matrix()
y3<- train_set$Cover_Type[- folds$Fold03]

knn_fit3_k3<- knn3(x3, y3, k = 3)
y_hat3_k3<- predict(knn_fit3_k3, cv_fold3, type = "class")
f3_k3<- confusionMatrix(y_hat3_k3, train_set$Cover_Type[folds$Fold03])
remove(knn_fit3_k3, y_hat3_k3)

knn_fit3_k7<- knn3(x3, y3, k = 7)
y_hat3_k7<- predict(knn_fit3_k7, cv_fold3, type = "class")
f3_k7<- confusionMatrix(y_hat3_k7, train_set$Cover_Type[folds$Fold03])
remove(knn_fit3_k7, y_hat3_k7)

knn_fit3_k11<- knn3(x3, y3, k = 11)
y_hat3_k11<- predict(knn_fit3_k11, cv_fold3, type = "class")
f3_k11<- confusionMatrix(y_hat3_k11, train_set$Cover_Type[folds$Fold03])
remove(knn_fit3_k11, y_hat3_k11)

knn_fit3_k15<- knn3(x3, y3, k = 15)
y_hat3_k15<- predict(knn_fit3_k15, cv_fold3, type = "class")
f3_k15<- confusionMatrix(y_hat3_k15, train_set$Cover_Type[folds$Fold03])
remove(knn_fit3_k15, y_hat3_k15, x3, y3, cv_fold3)

accuracy_fold3<- data.frame(Fold = "Fold 3", K = c(3,7,11, 15), 
                            Accuracy = c(f3_k3$overall["Accuracy"], f3_k7$overall["Accuracy"],
                                         f3_k11$overall["Accuracy"], f3_k15$overall["Accuracy"]))

remove(f3_k3,f3_k7,f3_k11, f3_k15)

accuracy_folds<- rbind(accuracy_fold1, accuracy_fold2, accuracy_fold3)

remove(accuracy_fold1, accuracy_fold2, accuracy_fold3, folds)

accuracy_folds %>% ggplot(aes(x = K, y = Accuracy, col = Fold)) +
  geom_point() + geom_line() + 
  ggtitle("Figure 8: KNN Accuracy and K") +
  theme_minimal() +
  theme(title = element_text(size = 8), axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8), legend.text = element_text(size = 8)) 

remove(accuracy_folds)

##############
# KNN test set
##############

x<- train_set %>% select(- Cover_Type, - rawah) %>%
  mutate_at(c(1:8), .funs = scale2) %>% as.matrix() # train set predictor matrix

y<- train_set$Cover_Type

knn_fit<- knn3(x, y, k = 3)  # best k from cross validation

knn_test<- test_set %>% select(- Cover_Type, - rawah) %>%
  mutate_at(c(1:8), .funs = scale2)   # test set data frame ; dependent variable removed

y_hat_knn<- predict(knn_fit, knn_test, type = "class")

accuracy_knn<- confusionMatrix(y_hat_knn, test_set$Cover_Type)

accuracy_knn$table

accuracy_knn$overall[c("Accuracy", "AccuracyLower", "AccuracyUpper")]

remove(x, y, knn_fit, y_hat_knn, accuracy_knn)

############################################
# classification fold creation and fold 1 cv
############################################

set.seed(3, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(3)`

folds<- createFolds(y = train_set$Cover_Type, k = 50, list = T)[1:3] # indexing first 3 vectors

rpart_train<- train_set %>%
  mutate_at(c(14:20), .funs = function(x)factor(ifelse(x == 1, "Yes", "No"))) %>%
  mutate(Wildnerness = factor(case_when(    # one column for wilderness information
    rawah == 1 ~ "Rawah",
    neota == 1 ~ "Neota",
    comanche_peak == 1 ~ "Comanche Peak",
    cache_la_poudre == 1 ~ "Cache la Poudre"
  ))) %>% select(- c(9:12))                # removing the binary wilderness columns


cv_fold1_train<- rpart_train %>%
  slice(- folds$Fold01)          # removing fold 1 observations

cv_fold1<-  rpart_train %>%
  slice(folds$Fold01) %>%       # slicing only fold 1 observation
  select(- Cover_Type)          # removing the variable to be predicted (dependent variable)

rpart_fit1_c1<- rpart(Cover_Type ~ ., data = cv_fold1_train, method = "class",
                      control = rpart.control(cp = 0, minsplit = 8))
y_hat_rpart1_c1<- predict(rpart_fit1_c1, cv_fold1, type = "class")
cm1<- confusionMatrix(y_hat_rpart1_c1, train_set$Cover_Type[folds$Fold01])
remove(rpart_fit1_c1, y_hat_rpart1_c1)

rpart_fit1_c2<- rpart(Cover_Type ~ ., data = cv_fold1_train, method = "class",
                      control = rpart.control(cp = 0.0025, minsplit = 8))
y_hat_rpart1_c2<- predict(rpart_fit1_c2, cv_fold1, type = "class")
cm2<- confusionMatrix(y_hat_rpart1_c2, train_set$Cover_Type[folds$Fold01])
remove(rpart_fit1_c2, y_hat_rpart1_c2)

rpart_fit1_c3<- rpart(Cover_Type ~ ., data = cv_fold1_train, method = "class",
                      control = rpart.control(cp = 0.005, minsplit = 8))
y_hat_rpart1_c3<- predict(rpart_fit1_c3, cv_fold1, type = "class")
cm3<- confusionMatrix(y_hat_rpart1_c3, train_set$Cover_Type[folds$Fold01])
remove(rpart_fit1_c3, y_hat_rpart1_c3)

rpart_fit1_c4<- rpart(Cover_Type ~ ., data = cv_fold1_train, method = "class",
                      control = rpart.control(cp = 0.0075, minsplit = 8))
y_hat_rpart1_c4<- predict(rpart_fit1_c4, cv_fold1, type = "class")
cm4<- confusionMatrix(y_hat_rpart1_c4, train_set$Cover_Type[folds$Fold01])
remove(rpart_fit1_c4, y_hat_rpart1_c4)

accuracy_fold1<- data.frame(Fold = "Fold 1", 
                            CP = c(0, 0.0025, 0.005, 0.0075),
                            Accuracy = c(cm1$overall["Accuracy"],
                                         cm2$overall["Accuracy"],
                                         cm3$overall["Accuracy"],
                                         cm4$overall["Accuracy"]))

remove(cv_fold1, cv_fold1_train)
remove(cm1,cm2,cm3, cm4)
accuracy_fold1

#################################################
# classification fold2 and 3; accuracy vs cp plot
#################################################

cv_fold2_train<- rpart_train %>%
  slice(- folds$Fold02)          # removing fold 2 observations

cv_fold2<-  rpart_train %>%
  slice(folds$Fold02) %>%       # slicing only fold 2 observation
  select(- Cover_Type)          # removing the variable to be predicted (dependent variable)

rpart_fit2_c1<- rpart(Cover_Type ~ ., data = cv_fold2_train, method = "class",
                      control = rpart.control(cp = 0, minsplit = 8))
y_hat_rpart2_c1<- predict(rpart_fit2_c1, cv_fold2, type = "class")
cm1<- confusionMatrix(y_hat_rpart2_c1, train_set$Cover_Type[folds$Fold02])
remove(rpart_fit2_c1, y_hat_rpart2_c1)


rpart_fit2_c2<- rpart(Cover_Type ~ ., data = cv_fold2_train, method = "class",
                      control = rpart.control(cp = 0.0025, minsplit = 8))
y_hat_rpart2_c2<- predict(rpart_fit2_c2, cv_fold2, type = "class")
cm2<- confusionMatrix(y_hat_rpart2_c2, train_set$Cover_Type[folds$Fold02])
remove(rpart_fit2_c2, y_hat_rpart2_c2)

rpart_fit2_c3<- rpart(Cover_Type ~ ., data = cv_fold2_train, method = "class",
                      control = rpart.control(cp = 0.005, minsplit = 8))
y_hat_rpart2_c3<- predict(rpart_fit2_c3, cv_fold2, type = "class")
cm3<- confusionMatrix(y_hat_rpart2_c3, train_set$Cover_Type[folds$Fold02])
remove(rpart_fit2_c3, y_hat_rpart2_c3)

rpart_fit2_c4<- rpart(Cover_Type ~ ., data = cv_fold2_train, method = "class",
                      control = rpart.control(cp = 0.0075, minsplit = 8))
y_hat_rpart2_c4<- predict(rpart_fit2_c4, cv_fold2, type = "class")
cm4<- confusionMatrix(y_hat_rpart2_c4, train_set$Cover_Type[folds$Fold02])
remove(rpart_fit2_c4, y_hat_rpart2_c4)

accuracy_fold2<- data.frame(Fold = "Fold 2", 
                            CP = c(0, 0.0025, 0.005, 0.0075),
                            Accuracy = c(cm1$overall["Accuracy"],
                                         cm2$overall["Accuracy"],
                                         cm3$overall["Accuracy"],
                                         cm4$overall["Accuracy"]))

remove(cv_fold2, cv_fold2_train)
remove(cm1,cm2,cm3, cm4)

cv_fold3_train<- rpart_train %>%
  slice(- folds$Fold03)          # removing fold 3 observations

cv_fold3<-  rpart_train %>%
  slice(folds$Fold03) %>%       # slicing only fold 3 observation
  select(- Cover_Type)          # removing the variable to be predicted (dependent variable)

rpart_fit3_c1<- rpart(Cover_Type ~ ., data = cv_fold3_train, method = "class",
                      control = rpart.control(cp = 0, minsplit = 8))
y_hat_rpart3_c1<- predict(rpart_fit3_c1, cv_fold3, type = "class")
cm1<- confusionMatrix(y_hat_rpart3_c1, train_set$Cover_Type[folds$Fold03])
remove(rpart_fit3_c1, y_hat_rpart3_c1)

rpart_fit3_c2<- rpart(Cover_Type ~ ., data = cv_fold3_train, method = "class",
                      control = rpart.control(cp = 0.0025, minsplit = 8))
y_hat_rpart3_c2<- predict(rpart_fit3_c2, cv_fold3, type = "class")
cm2<- confusionMatrix(y_hat_rpart3_c2, train_set$Cover_Type[folds$Fold03])
remove(rpart_fit3_c2, y_hat_rpart3_c2)

rpart_fit3_c3<- rpart(Cover_Type ~ ., data = cv_fold3_train, method = "class",
                      control = rpart.control(cp = 0.005, minsplit = 8))
y_hat_rpart3_c3<- predict(rpart_fit3_c3, cv_fold3, type = "class")
cm3<- confusionMatrix(y_hat_rpart3_c3, train_set$Cover_Type[folds$Fold03])
remove(rpart_fit3_c3, y_hat_rpart3_c3)

rpart_fit3_c4<- rpart(Cover_Type ~ ., data = cv_fold3_train, method = "class",
                      control = rpart.control(cp = 0.0075, minsplit = 8))
y_hat_rpart3_c4<- predict(rpart_fit3_c4, cv_fold3, type = "class")
cm4<- confusionMatrix(y_hat_rpart3_c4, train_set$Cover_Type[folds$Fold03])
remove(rpart_fit3_c4, y_hat_rpart3_c4)

accuracy_fold3<- data.frame(Fold = "Fold 3", 
                            CP = c(0, 0.0025, 0.005, 0.0075),
                            Accuracy = c(cm1$overall["Accuracy"],
                                         cm2$overall["Accuracy"],
                                         cm3$overall["Accuracy"],
                                         cm4$overall["Accuracy"]))

remove(cv_fold3, cv_fold3_train)
remove(cm1,cm2,cm3,cm4)

accuracy_folds<- rbind(accuracy_fold1, accuracy_fold2, accuracy_fold3)

remove(accuracy_fold1, accuracy_fold2, accuracy_fold3)

accuracy_folds %>% ggplot(aes(x = CP, y = Accuracy, col = Fold)) +
  geom_point() + geom_line() + 
  ggtitle("Figure 9: Classification Accuracy and Tuning Paramters") +
  theme_minimal() +
  theme(title = element_text(size = 8), axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8), legend.text = element_text(size = 8)) 

remove(accuracy_folds)

#########################
# classification test set
#########################

rpart_test<- test_set %>%
  mutate_at(c(14:20), .funs = function(x)factor(ifelse(x == 1, "Yes", "No"))) %>%
  mutate(Wildnerness = factor(case_when(
    rawah == 1 ~ "Rawah",
    neota == 1 ~ "Neota",
    comanche_peak == 1 ~ "Comanche Peak",
    cache_la_poudre == 1 ~ "Cache la Poudre"
  ))) %>% select(- c(9:12)) %>% select(- Cover_Type) # removing the dependent variable

rpart_fit<- rpart(Cover_Type ~ ., data = rpart_train, method = "class",
                  control = rpart.control(cp = 0.0025, minsplit = 8)) # best cp

y_hat_rpart<- predict(rpart_fit, rpart_test, type = "class")

accuracy_rpart<- confusionMatrix(y_hat_rpart, test_set$Cover_Type)

accuracy_rpart$table

accuracy_rpart$overall[c("Accuracy", "AccuracyLower", "AccuracyUpper")]

remove(rpart_test, rpart_train, rpart_fit, y_hat_rpart, accuracy_rpart)

