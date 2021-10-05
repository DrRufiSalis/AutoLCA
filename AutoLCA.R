#### AutoLCA - LIFE CYCLE ASSESSMENT (ISO 14040) SCRIPT FOR 1 ASSESSMENT ####
#Author: Dr Martí Rufí-Salís (ORCID: 0000-0003-3696-1033)
#Sostenipra Research Group - Tectum Garden - Universitat Autònoma de Barcelona (UAB)

#The LCA part of the Script is divided in the Goal and Scope, Life Cycle Inventory and Life Cycle Impact Assessment following ISO 14040.
#This script only works if the LCA has an attributional perspective, where 2kg of Functional Unit has the double of impact compared to 1kg of Functional Unit.
#The interpretation phase is left out of the script since it is expected to be provided in external reports, papers, etc.
#A short description fo every subsection is provided to help the user understand how the specific code line works

#### Installing and Loading Packages  ####
#Option 1
Packages_2_install <- c("tidyverse", "scales", "mudata2", "scales", "lubridate", "readxl", "gganimate", "gifski", "png", "shiny", "writexl", "pacman")
install.packages(Packages_2_install) #Only run this if you don't have the packages installed since it takes some time to reinstall and update them

#Option 2
library(pacman) #This library includes the function "p_load" which lets you load multiple packages with a single code line
p_load(tidyverse, scales, mudata2, scales, lubridate, readxl, gganimate, gifski, png, shiny, writexl) #Loads the packages


#### Setting the working directory ####

#Run JUST ONE of the following lines depending on the device you are working with. Edit to change the specific directory
#SETWD WINDOWS UNI - line to use if you are working in a UNIVERSITY WINDOWS computer
setwd("C:/Users/NIU/Folder1/Folder2/Folder3/Folder4")
#SETWD WINDOWS LAPTOP - line to use if you are working in a Windows Laptop
setwd("C:/Users/Name/Folder1/Folder2/Folder3/Folder4")
#SETWD MAC - line to use if you are working with a Mac laptop
setwd("~/Folder1/Folder2/Folder3/Folder4")



#### GOAL AND SCOPE ####
#The goal of the study entails the application, audience and reasons to carry the analysis. 
#The scope encompasses a set of key parameters that will be used as reference for the following phases. 
#The functional unit (FU) is the reference quantity of product that will be used to normalize the input and outputs of the analysis
#The system boundaries define the processes included in the assessment and the ones that are to be left out. 
#Quality of data, assumptions used and potential limitations are also part of the goal and scope phase.

#1) Project name: 
#2) Interested party: 
#3) Author: 
#4) LCA Practitioner: 
#5) Goal: 
#6) Data source: 
#7) Data quality: Bad/Acceptable/Good/Very Good/Excellent
#8) Functional Unit: 
#9) System Boundaries: Cradle-to-Gate/Cradle-to-grave
#10) Background databases: 
#11) Software: 
#12) Impact Method: 
#13) Highlighted impact categories: 



#### LIFE CYCLE INVENTORY (LCI) ####
#Collection and calculation of data to meet the goal of the study. 
#Dynamic and resource-intensive: as limitations and hidden aspects of the system may appear, different sources and types of data may be required. 

#Import and view data - Inventory 
#Imports the data from an excel file in the working directory, specifying the sheet and column types
LCI_1 <- read_excel("AutoLCA_EXCEL.xlsx", sheet = "LCI_SHEET1", col_types = c("text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "numeric", "text", "numeric", "numeric", "numeric", "text", "text"))

#Define Reference Flows (in the same unit as in the functional unit)
#Here we add the reference flows (e.g. total production of the agricultral system under assessment)
RF1 = 1.111 #Add the Reference Flow for LCI_1 here

#Omitting NAs 
#CAUTION: with this function all lines in the excel file that have an empty space will be deleted
#It is useful to omit lacking information from the Inventory
LCI_1_NA <- na.omit(LCI_1)

#Filtering specific data
#It is useful to omit specific data that is not needed (e.g. data regarding the functional unit included as a row in excel file)
#FilterList1 <- c('Item1', 'Item2')
#LCI_1_NA <- dplyr::filter(LCI_1_NA, !Item %in% FilterList1)

#Recalculate LCI with Lifespan
#With the following operation we recalculate all the inventory considering the lifespan of the different elements:
#Recalculated Values = Assessment Length * Total Resources / Lifespan (the information on how to structure the excel file can be found in the excel template - LINK AT THE TOP OF THE SCRIPT)
LCI_1_NA$AssessmentLCI <- LCI_1_NA$Assessment_Length_years * LCI_1_NA$Total / LCI_1_NA$Lifespan_years



#### LIFE CYCLE IMPACT ASSESSMENT (LCIA) ####
#The LCIA is the final calculation step of the LCA, encompassing the classification, characterization, normalization and weighting phases.
#The first two are mandatory, while the other are optional and relevant for specific cases.
#By using the flows gathered in the LCI in reference to the FU, the environmental impacts of the system are calculated for a set of impact categories, normally classified using impact methods.
#Once these categories are chosen by the LCA practitioner, the elementary flows of the LCI are allocated to each impact category, with the possibility of one flow contributing to multiple impact categories (classification). 
#Then, each flow is multiplied by characterization factors accounting for how harmful is that emission in that impact category (characterization). 
#Finally, the product of the aggregated multiplications is summed to yield the final score of a specific impact category in a specific unit (e.g. kg P equivalents for freshwater eutrophication or kg SO2 equivalents for terrestrial acidification). 
#NOT INCLUDED IN THIS SCRIPT: The optional phases of LCIA are normalization (comparing the values of the impact categories to a reference information) and weighting (aggregation of impact categories into single values based on weighting factors).

#Import and view data - Impacte 1 kg
#Imports the data from an excel file in the working directory, specifying the sheet and column types. 
#The datasheet imported below includes the impact of 1 unit of all the element of the inventory for all the selected impact categories.
#It is useful to include all the impact categories from the method we are using since some may be important following LCA's iterativity
Imp1U <- read_excel("AutoLCA_EXCEL.xlsx", sheet = "LCIA_1U")

#GatherFunction - http://statseducation.com/Introduction-to-R/modules/tidy%20data/gather/
#To make sure that the gather function workds, follow the instructions on how this specific data should be structured in the excel template (LINK AT THE TOP OF THE SCRIPT)
Imp1U <- Imp1U %>% gather(Process, Imp1Unit, -ImpactMethod, -IC, -Abbr, -Unit)

#Merge Inventory and 1Unit LCIA
#The following function merges the LCI and LCIA spreadsheets by the parameter "Process" (i.e. name of Ecoinvent process), which is common in both spreadsheets
LCA_1 <- merge(LCI_1_NA, Imp1U, by = "Process")

#Create Total LCIA data for the entire reference flow (i.e. without considering the functional unit)
#Impacts for the reference flow = Recalculated values (considering the readjustment with the lifespan) * Impact for 1 unit of every specific element
LCA_1$Impacts <- LCA_1$AssessmentLCI * LCA_1$Imp1Unit

#Create Total LCIA data for the functional unit (i.e. dividing the impact for the entire reference flow by the reference flow)
#Impact per functional unit = Impact for the reference flow / reference flow
LCA_1$Impacts_FU <- LCA_1$Impacts / RF1


#### Total Impacts of the LCA per IC ####
#LCA_1 - All Impact Categories
LCA_1_TOTAL <- LCA_1 %>% group_by(IC, System) %>% summarise(mean = mean(Impacts_FU), sum = sum(Impacts_FU))







#### At this point of the script, we have the LCIA calculated for all the impact categories. ####
#Depending on the data resolution of the excel file we may be able to go more or less deeper regarding the data visualization and interpretation



#### Impact Summary Datasets - per Subsystem, Item and Element #### 
#Using the dplyr function "group_by", we group and the LCIA by Item and Subsystem, calculting means and sums through the funciton "summarise"

#LCA_1 - All Impact Categories
LCA_1_SubSys <- LCA_1 %>% group_by(IC, Subsystem) %>% summarise(mean = mean(Impacts_FU), sum = sum(Impacts_FU))
LCA_1_Item <- LCA_1 %>% group_by(IC, Item) %>% summarise(mean = mean(Impacts_FU), sum = sum(Impacts_FU))



#Save the impact datasheets for all impact categories per Item - Library "writexl"
sheets <- list("LCA_1" = LCA_1_Item, "LCA_1_v2" = LCA_1_Item) #Create an extra (v2) tab to edit the data as you wan t
#The follwoing function will save an excel file with the above sheets in the working directory
write_xlsx(sheets, "Global_LCIA.xlsx")



#### Plots - 1 IMPACT CATEGORY ####
#In this section we are filtering the datset to select only 1 Impact Category
#The section is currently selecitng the Global Warming impact category, but can be changed

#1C Filter
LCA_1_1C <- dplyr::filter(LCA_1, IC == "Global warming") #Change Global Warming to select any other Impact Category

#1C Genotyping Plots - per ITEM - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)

#LCA_1 - 1C
LCA_1I_1C <- ggplot(LCA_1_1C, aes(x = IC, y = Impacts_FU)) + 
geom_bar(aes(fill = Item), stat = "identity", position = "stack") +
ylab("Impact per FU") + xlab("Global Warming (kg CO2 eq)") + #Change the Lab title to adapt it to the selectec impact category
theme(axis.text.y = element_text(size = 15))

  
#3C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1I_1C #LCA_1


#1C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig1C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig1C", filename = "LCA_1I_1C.jpeg", plot = LCA_1I_1C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #LCA_1


#1C Genotyping Plots - per ELEMENT - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)
#x <- x[which(x$VOLUME <= 20),] FUNCTION TO SUBSET VALEUS LOWER THAN A THRESHOLD

#LCA_1 - 1C - Element
LCA_1E_1C <- ggplot(LCA_1_1C, aes(x = IC, y = Impacts_FU)) + 
  geom_bar(aes(fill = Element), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.y = element_text(size = 15))




#1C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1E_1C #LCA_1


#1C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig1C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig1C", filename = "LCA_1E_1C.jpeg", plot = LCA_1E_1C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #Lettuce - Batch 1



#### Plots - 2 IMPACT CATEGORIES ####
#In this section we are filtering the datset to select 2 impact categories
# We exmplify this section by using the following impact categories: Global Warming and Fossil Resources Scarcity.
#The selected impact categories can be changed depending on the project or the hotspots of the analyzed system.

#2C Filter and ordering
SelectedICs <- c("Global warming", "Fossil resource scarcity") #Creating a list with the name of the impact categories to filter
#LCA_1
LCA_1_2C <- dplyr::filter(LCA_1, IC %in% SelectedICs) #Filter
LCA_1_2C$IC = factor(LCA_1_2C$IC, levels=c("Global warming", "Fossil resource scarcity")) #Order to sort how they will appear in the plot


#2C Genotyping Plots - per ITEM - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)

#LCA_1 - 2C - Item
LCA_1I_2C <- ggplot(LCA_1_2C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Item), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")



#3C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1I_2C #LCA_1


#2C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig2C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig2C", filename = "LCA_1I_2C.jpeg", plot = LCA_1I_2C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #LCA_1


#2C Genotyping Plots - per ELEMENT - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)
#x <- x[which(x$VOLUME <= 20),] FUNCTION TO SUBSET VALEUS LOWER THAN A THRESHOLD

#LCA_1 - 2C - Element
LCA_1E_2C <- ggplot(LCA_1_2C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Element), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")




#2C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1E_2C #LCA_1


#2C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig2C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig2C", filename = "LCA_1E_2C.jpeg", plot = LCA_1E_2C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #Lettuce - Batch 1



#### Plots - 3 IMPACT CATEGORIES ####
#In this section we are filtering the datset to select 3 impact categories
# We exmplify this section by using the following impact categories: Global Warming, Fossil Resources Scarcity and Land Use.
#The selected impact categories can be changed depending on the project or the hotspots of the analyzed system.

#3C Filter and ordering
SelectedICs <- c("Global warming", "Fossil resource scarcity", "Land use") #Creating a list with the name of the impact categories to filter
#LCA_1
LCA_1_3C <- dplyr::filter(LCA_1, IC %in% SelectedICs) #Filter
LCA_1_3C$IC = factor(LCA_1_3C$IC, levels=c("Global warming", "Fossil resource scarcity", "Land use")) #Order to sort how they will appear in the plot


#3C Genotyping Plots - per ITEM - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)

#LCA_1 - 3C - Item
LCA_1I_3C <- ggplot(LCA_1_3C, aes(x = System, y = Impacts_FU)) + 
geom_bar(aes(fill = Item), stat = "identity", position = "stack") +
ylab("Impact per FU") + xlab("Impact Categories") +
theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
#facet_wrap(~Abbr, scales = "free_y")
facet_wrap(~IC, scales = "free_y")




#3C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1I_3C #LCA_1


#3C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig3C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig3C", filename = "LCA_1I_3C.jpeg", plot = LCA_1I_3C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #LCA_1


#3C Genotyping Plots - per ELEMENT - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)
#x <- x[which(x$VOLUME <= 20),] FUNCTION TO SUBSET VALEUS LOWER THAN A THRESHOLD

#LCA_1 - 3C - Element
LCA_1E_3C <- ggplot(LCA_1_3C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Element), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")



#3C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1E_3C #LCA_1


#3C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig3C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig3C", filename = "LCA_1E_3C.jpeg", plot = LCA_1E_3C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #Lettuce - Batch 1



#### Plots - 4 IMPACT CATEGORIES ####
#In this section we are filtering the datset to select 4 impact categories
# We exmplify this section by using the following impact categories: Global Warming, Freshwater Eutrophication, Marine Euetrophication and Land Use.
#The selected impact categories can be changed depending on the project or the hotspots of the analyzed system.

#4C Filter and ordering
SelectedICs <- c("Global warming", "Land use", "Freshwater eutrophication", "Marine Eutrophication") #Creating a list with the name of the impact categories to filter
#LCA_1
LCA_1_4C <- dplyr::filter(LCA_1, IC %in% SelectedICs) #Filter
LCA_1_4C$IC = factor(LCA_1_4C$IC, levels=c("Global warming", "Freshwater Eutrophication", "Marine Eutrophication", "Land use")) #Order to sort how they will appear in the plot


#4C Genotyping Plots - per ITEM - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)

#LCA_1 - 4C - Item
LCA_1I_4C <- ggplot(LCA_1_4C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Item), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")



#4C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1I_4C #LCA_1


#4C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig4C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig4C", filename = "LCA_1I_4C.jpeg", plot = LCA_1I_4C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #LCA_1


#4C Genotyping Plots - per ELEMENT - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)
#x <- x[which(x$VOLUME <= 20),] FUNCTION TO SUBSET VALEUS LOWER THAN A THRESHOLD

#LCA_1 - 4C - Element
LCA_1E_4C <- ggplot(LCA_1_4C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Element), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")



#4C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1E_4C #LCA_1

#4C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig4C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig4C", filename = "LCA_1E_4C.jpeg", plot = LCA_1E_4C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #Lettuce - Batch 1


#### Plots - 5 IMPACT CATEGORIES ####
#In this section we are filtering the datset to select 3 impact categories
# We exmplify this section by using the following impact categories: Global Warming, Freshwater Eutrophication, Marine Euetrophication, Fossil Resources Scarcity and Land Use.
#The selected impact categories can be changed depending on the project or the hotspots of the analyzed system.

#5C Filter and ordering
SelectedICs <- c("Global warming", "Fossil resource scarcity", "Land use", "Freshwater eutrophication", "Marine Eutrophication") #Creating a list with the name of the impact categories to filter
#LCA_1
LCA_1_5C <- dplyr::filter(LCA_1, IC %in% SelectedICs) #Filter
LCA_1_5C$IC = factor(LCA_1_5C$IC, levels=c("Global warming", "Freshwater Eutrophication", "Marine Eutrophication", "Fossil resource scarcity", "Land use")) #Order to sort how they will appear in the plot


#5C Genotyping Plots - per ITEM - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)

#LCA_1 - 5C - Item
LCA_1I_5C <- ggplot(LCA_1_5C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Item), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")


#5C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1I_5C #LCA_1


#5C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig5C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig5C", filename = "LCA_1I_5C.jpeg", plot = LCA_1I_5C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #LCA_1


#5C Genotyping Plots - per ELEMENT - Here we create the graphs and store them in specific names
#To change a specific graphical parameter just look for the way to do it in the interent (e.g. stackoverflow, statmethods, sthda, r-coder, etc)
#x <- x[which(x$VOLUME <= 20),] FUNCTION TO SUBSET VALEUS LOWER THAN A THRESHOLD

#LCA_1 - 5C - Element
LCA_1E_5C <- ggplot(LCA_1_5C, aes(x = System, y = Impacts_FU)) + 
  geom_bar(aes(fill = Element), stat = "identity", position = "stack") +
  ylab("Impact per FU") + xlab("Impact Categories") +
  theme(axis.text.x = element_blank(), axis.text.y = element_text(size = 15), strip.text.x = element_text(size = 15)) +
  #facet_wrap(~Abbr, scales = "free_y")
  facet_wrap(~IC, scales = "free_y")



#5C Phenotyping Plots - Here we show the graphs in the "Plots" viewing tool at the bottom-right of RStudio Environment
LCA_1E_5C #LCA_1


#5C Saving Plots - https://ggplot2.tidyverse.org/reference/ggsave.html
dir.create("Fig5C") #Creating a folder to contain the following graphs
#Function to save the graphs as images with the ideal conditions for our purpose. Graphs will be saved in the working directory
ggsave(path = "Fig5C", filename = "LCA_1E_5C.jpeg", plot = LCA_1E_5C, device = "jpeg", scale = 1, width = NA, height = NA, units = "mm", dpi = 300, bg = "white") #Lettuce - Batch 1