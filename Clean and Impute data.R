#### Clean and Impute data

library(dplyr)
library(xlsx)
library(readxl)

#######################################################################################
#  Merge data from 1982-2003
#######################################################################################
# 1982-2003
load("community_1982_2003.RData")
data1 <- combined_data
years <- unique(data1$SampleYear)
# Match species names with Latin names
data1 <- data1[-which(is.na(data1$Plot)),] # 27 empty rows with no information

## Update the species-name matching table
CheckList <- read.xlsx("CheckList.xlsx", sheetIndex = 1)
## Identify elements in vec1 that are not in vec2 (all matched)
vec1 <- unique(data1$Name)
vec2 <- unique(CheckList$AccSpeciesName)
aa <- vec1[!vec1 %in% vec2]

colnames(data1)[4] <- c("AccSpeciesName")
## 3. Correct species names ----------
checked_final_data <- data1 %>% left_join(.,CheckList,by = "AccSpeciesName")

## 4. Add Latin names ----------
sort(unique(checked_final_data$Checked_Name))
checked_final_data <- checked_final_data[, c(1:3, 5:10)]
SpeInfo <- read.xlsx("SpeInfo.xlsx", sheetIndex = 1)
my_final_data <- checked_final_data %>% left_join(.,SpeInfo, by = "Checked_Name")
my_final_data[!is.na(my_final_data$Checked_Name) & is.na(my_final_data$AccSpeciesName), ] # NA

data3 <- my_final_data[,c(1:8,10)]



data <- data3
data <- data[, c(2,3,1,4:9)]



#######################################################################################
# 2004-2012
## In the data file, replace values <0.1 with 0.05 (62 records)
data1 <- read_xls("community_2004_2012.xls")
data1$日期 <- gsub("-", "", data1$日期)
data1$SampleYear <- substr(data1$日期, 1,4)
years <- as.character(c(2004:2012))
data1 <- data1[data1$SampleYear %in% years, ]
data1$WeightWet <- NA # This variable is not available in the file
data2 <- data1[, c(8,1,2,4,5,6,9,7,3)]
colnames(data2) <- colnames(data)

# Match species names with Latin names
## Identify elements in vec1 that are not in vec2 (all matched)
vec1 <- unique(data2$AccSpeciesName)
vec2 <- unique(c(CheckList$Checked_Name,CheckList$AccSpeciesName))
aa <- vec1[!vec1 %in% vec2]

## 3. Correct species names ----------
checked_final_data <- data2 %>% left_join(.,CheckList,by = "AccSpeciesName")

## 4. Add Latin names ----------
sort(unique(checked_final_data$Checked_Name))
checked_final_data <- checked_final_data[, c(1:8, 10)]
my_final_data <- checked_final_data %>% left_join(.,SpeInfo, by = "Checked_Name")
my_final_data[!is.na(my_final_data$Checked_Name) & is.na(my_final_data$AccSpeciesName), ] # 0

data3 <- my_final_data[,c(1:8,10)]


data <- rbind(data, data3)

data[which(is.na(data$AccSpeciesName)),] #1

#######################################################################################
# 2013
data1 <- read_xls("community_2013.xls")
data1$日期 <- gsub("-", "", data1$日期)
data1$SampleYear <- substr(data1$日期, 1,4)
years <- as.character(c(2013))
data1 <- data1[data1$SampleYear %in% years, ]
data1$WeightWet <- NA
data2 <- data1[, c(8,1,2,4,5,6,9,7,3)]
colnames(data2) <- colnames(data)

## 3. Correct species names ----------
checked_final_data <- data2 %>% left_join(.,CheckList,by = "AccSpeciesName")

## 4. Add Latin names ----------
sort(unique(checked_final_data$Checked_Name))
checked_final_data <- checked_final_data[, c(1:8, 10)]
my_final_data <- checked_final_data %>% left_join(.,SpeInfo, by = "Checked_Name")
my_final_data[!is.na(my_final_data$Checked_Name) & is.na(my_final_data$AccSpeciesName), ] # 0 record
my_final_data[is.na(my_final_data$Checked_Name),] # 0 record

data3 <- my_final_data[,c(1:8,10)]

data <- rbind(data, data3)
data[which(is.na(data$AccSpeciesName)),] # 1 record

#######################################################################################
# 2014-2024
data1 <- read.csv("community_2014_2024.csv",fileEncoding = 'GBK')

data2 <- data1[, c(1:3, 5:7, 9:10, 4)]
colnames(data2) <- colnames(data)

data2[is.na(data2$AccSpeciesName),] # 1 record

# Match species names with Latin names
## Identify elements in vec1 that are not in vec2 (all matched)
vec1 <- unique(data2$AccSpeciesName)
vec2 <- unique(c(CheckList$Checked_Name,CheckList$AccSpeciesName))
aa <- vec1[!vec1 %in% vec2]

sort(unique(data2$AccSpeciesName))

## 3. Correct species names ----------
checked_final_data <- data2 %>% left_join(.,CheckList,by = "AccSpeciesName")

## 4. Add Latin names ----------
sort(unique(checked_final_data$Checked_Name))
checked_final_data <- checked_final_data[, c(1:8, 10)]
my_final_data <- checked_final_data %>% left_join(.,SpeInfo, by = "Checked_Name")
my_final_data[!is.na(my_final_data$Checked_Name) & is.na(my_final_data$AccSpeciesName), ] # 0 records
my_final_data[is.na(my_final_data$Checked_Name),] # 1 records

data3 <- my_final_data[,c(1:8,10)]

data <- rbind(data, data3)
data[which(is.na(data$AccSpeciesName)),] # 2 records

#######################################################################################
# Manually inspect and modify data
change_data <- list()
change_data[["WeightWet"]] <- matrix()
change_data[["WeightDry"]] <- matrix()

#######################################################################################
data$HeightVegTiller <- as.numeric(data$HeightVegTiller)
data$HeightReprodTiller <- as.numeric(data$HeightReprodTiller)
data$IndividualNum <- as.numeric(data$IndividualNum)
length(which(data$HeightVegTiller < 0.0001)) # 10,847 records
data$HeightVegTiller[which(data$HeightVegTiller < 0.0001)] <- NA
length(which(data$HeightReprodTiller < 0.0001)) # 42,129 records
data$HeightReprodTiller[which(data$HeightReprodTiller < 0.0001)] <- NA
length(which(data$IndividualNum < 0.0001)) # 824 records
data$IndividualNum[which(data$IndividualNum < 0.0001)] <- NA
length(which(data$WeightWet < 0.0001)) # 9,260 records
data$WeightWet[which(data$WeightWet < 0.0001)] <- NA
length(which(data$WeightDry < 0.0001)) # 395 records
data$WeightDry[which(data$WeightDry < 0.0001)] <- NA

data <- data %>%
  left_join(SpeInfo, by = "AccSpeciesName")

data$Change_after <- NA
#######################################################################################
length(which((data$WeightWet > 0.3 | is.na(data$WeightWet)) & abs(data$WeightDry - 0.01) < 1e-8))# 959 records
tmp <- data[which((data$WeightWet > 0.3 | is.na(data$WeightWet)) & abs(data$WeightDry - 0.01) < 1e-8),]
tmp$reason <- "WeightDry==0.01 with WeightWet>0.3 or NA"
change_data[["WeightDry"]] <- tmp
data$WeightDry[which((data$WeightWet > 0.3 | is.na(data$WeightWet)) & abs(data$WeightDry - 0.01) < 1e-8)] <- NA


length(which(((data$WeightWet > 2 & !is.na(data$WeightWet)) | (is.na(data$WeightWet) & (( !is.na(data$HeightVegTiller) & data$HeightVegTiller > 15 ) | ( !is.na(data$HeightReprodTiller) & data$HeightReprodTiller > 15 )))) & abs(data$WeightDry - 0.1) < 1e-8))# 61 records
tmp <- data[which(((data$WeightWet > 2 & !is.na(data$WeightWet)) | (is.na(data$WeightWet) & (( !is.na(data$HeightVegTiller) & data$HeightVegTiller > 15 ) | ( !is.na(data$HeightReprodTiller) & data$HeightReprodTiller > 15 )))) & abs(data$WeightDry - 0.1) < 1e-8),]
tmp$reason <- "WeightDry==0.1 with WeightWet>2 or (is.na(WeightWet) with large Reprod/Vegtiller height)"
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(((data$WeightWet > 2 & !is.na(data$WeightWet)) | (is.na(data$WeightWet) & (( !is.na(data$HeightVegTiller) & data$HeightVegTiller > 15 ) | ( !is.na(data$HeightReprodTiller) & data$HeightReprodTiller > 15 )))) & abs(data$WeightDry - 0.1) < 1e-8)] <- NA

length(which(abs(data$WeightWet * 100 - round(data$WeightWet * 100)) > 0.0001)) # 5 records
tmp <- data[which(abs(data$WeightWet * 100 - round(data$WeightWet * 100)) > 0.0001),]
tmp$reason <- "WeightWet was not rounded to 2 decimals and was considered not to be original data"
change_data[["WeightWet"]] <- tmp
data$WeightWet[which(abs(data$WeightWet * 100 - round(data$WeightWet * 100)) > 0.0001)] <- NA

length(which(abs(data$WeightDry * 100 - round(data$WeightDry * 100)) > 0.0001)) # 575 records
tmp <- data[which(abs(data$WeightDry * 100 - round(data$WeightDry * 100)) > 0.0001),]
tmp$reason <- "WeightDry was not rounded to 2 decimals and was considered not to be original data"
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(abs(data$WeightDry * 100 - round(data$WeightDry * 100)) > 0.0001)] <- NA


sum(is.na(data$AccSpeciesName)) # 2 of 96,838 observations
data$AccSpeciesName[which(is.na(data$AccSpeciesName))] <- paste0("sp", 15:16)


# Remove duplicate records from the data
# Remove 20 completely identical records
# Add a unique ID
data_with_id <- data %>%
  mutate(.row_id = row_number())

# Rows retained
data_unique_id <- data_with_id %>%
  distinct(
    SampleYear,
    SampleDate,
    Plot,
    AccSpeciesName,
    WeightDry,
    .keep_all = TRUE
  )

# Identify deleted rows
deleted_rows <- data_with_id %>%
  filter(!(.row_id %in% data_unique_id$.row_id)) %>%
  mutate(reason = "Duplicate data records")
deleted_rows <- deleted_rows[,-12]

# Store the deleted rows in change_data
change_data[["Delete"]] <- deleted_rows

data_unique <- data_unique_id %>%
  select(-.row_id)


aaa <- data_unique %>%
  group_by(SampleYear, SampleDate, Plot, AccSpeciesName, HeightVegTiller, IndividualNum, WeightWet) %>%
  filter(n() > 1) %>%
  ungroup()
tmp <- data_unique[which(data_unique$SampleDate == "19990810" & data_unique$Plot == 16 & data_unique$WeightDry==1.44),]
tmp$reason <- "Duplicate data records"
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19990810" & data_unique$Plot == 16 & data_unique$WeightDry==1.44),]
data_unique$WeightDry[which(data_unique$SampleDate == "19990810" & data_unique$Plot == 16 & data_unique$WeightDry==1.65)] <- 1.55 #mean

aaa <- data_unique %>% 
  group_by(SampleYear, SampleDate, Plot, AccSpeciesName) %>%
  filter(n() > 1) %>%
  ungroup()


change_data[["Checked_Name"]] <- data_unique[0, ]
change_data[["Checked_Name"]]$reason <- character(0)

change_data[["Plot"]] <- data_unique[0, ]
change_data[["Plot"]]$reason <- character(0)

## 1. Suspected row-entry error -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19820630" & data_unique$Checked_Name == "羊草" & data_unique$Plot== 13 & data_unique$WeightWet>300),]
tmp$reason <- "Suspected wrong row entry"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19820630" & data_unique$Checked_Name == "羊草" & data_unique$Plot== 13 & data_unique$WeightWet>300),]


## 2. Suspected row-entry error -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19820630" & data_unique$Checked_Name == "黄囊薹草" & data_unique$Plot== 14 & is.na(data_unique$WeightDry)),]
tmp$reason <- "Suspected wrong row entry"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19820630" & data_unique$Checked_Name == "黄囊薹草" & data_unique$Plot== 14 & is.na(data_unique$WeightDry)),]


## 3. Value too small -> set Checked_Name to NA
tmp <- data_unique[which(data_unique$SampleDate == "19820729" & data_unique$Checked_Name == "大针茅" & data_unique$WeightDry<0.5), ]
tmp$reason <- "Dry weight was too small and inconsistent with Stipa grandis, so Checked_Name was set to NA"
tmp$Change_after <- NA
change_data[["Checked_Name"]] <- rbind(change_data[["Checked_Name"]], tmp)
data_unique$Checked_Name[which(data_unique$SampleDate == "19820729" & data_unique$Checked_Name == "大针茅" & data_unique$WeightDry<0.5)] <- NA


## 4. Value too small; the abundance-dry weight relationship is consistent only with Salsola collina -> change Checked_Name to Salsola collina
tmp <- data_unique[which(data_unique$SampleDate == "19820814" & data_unique$Checked_Name == "大针茅" & data_unique$Plot== 9 & data_unique$WeightDry<5),]
tmp$reason <- "The relationship between abundance and dry weight was inconsistent with Stipa grandis and more consistent with Salsola collina, so Checked_Name was changed to Salsola collina"
tmp$Change_after <- "猪毛菜"
change_data[["Checked_Name"]] <- rbind(change_data[["Checked_Name"]], tmp)
data_unique$Checked_Name[which(data_unique$SampleDate == "19820814" & data_unique$Checked_Name == "大针茅" & data_unique$Plot== 9 & data_unique$WeightDry<5)] <- "猪毛菜"


## 5. Value too large and marked in red in the original data sheet; the unusually high dry weight per individual suggests Artemisia pubescens -> change Checked_Name to Artemisia pubescens
tmp <- data_unique[which(data_unique$SampleDate == "19820814" & data_unique$Checked_Name == "木地肤" & data_unique$WeightDry>50),]
tmp$reason <- "Dry weight per individual was unusually large, and the record was marked in red in the original sheet, suggesting misidentification; It is more consistent with Silene jeniseensis, so Checked_Name was changed to it"
tmp$Change_after <- "柔毛蒿"
change_data[["Checked_Name"]] <- rbind(change_data[["Checked_Name"]], tmp)
data_unique$Checked_Name[which(data_unique$SampleDate == "19820814" & data_unique$Checked_Name == "木地肤" & data_unique$WeightDry>50)] <- "柔毛蒿"


## 6. Empty row with no data -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19830831" & data_unique$Checked_Name == "细叶鸢尾" & is.na(data_unique$WeightDry)),]
tmp$reason <- "Duplicate row with no weight data"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19830831" & data_unique$Checked_Name == "细叶鸢尾" & is.na(data_unique$WeightDry)),]



## 7. Suspected row-entry error -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19840730" & data_unique$Checked_Name == "柔毛蒿" & data_unique$Plot== 8 & data_unique$WeightWet<5),]
tmp$reason <- "Suspected wrong row entry"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19840730" & data_unique$Checked_Name == "柔毛蒿" & data_unique$Plot== 8 & data_unique$WeightWet<5),]



## 8. Value too small -> set Checked_Name to NA
tmp <- data_unique[which(data_unique$SampleDate == "19860830" & data_unique$Checked_Name == "大针茅" & data_unique$WeightDry<0.5),]
tmp$reason <- "Dry weight was too small and inconsistent with Stipa grandis, so Checked_Name was set to NA"
tmp$Change_after <- NA
change_data[["Checked_Name"]] <- rbind(change_data[["Checked_Name"]], tmp)
data_unique$Checked_Name[which(data_unique$SampleDate == "19860830" & data_unique$Checked_Name == "大针茅" & data_unique$WeightDry<0.5)] <- NA



## 9. Impute based on moisture content of other records -> change WeightDry to 0.04
tmp <- data_unique[which(data_unique$SampleDate == "19870915" & data_unique$Checked_Name == "红柴胡" & data_unique$Plot== 6 & is.na(data_unique$WeightDry)), ]
tmp$reason <- "WeightDry was missing and was imputed based on the moisture content relationship of other samples"
tmp$Change_after <- 0.04
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data_unique$WeightDry[which(data_unique$SampleDate == "19870915" & data_unique$Checked_Name == "红柴胡" & data_unique$Plot== 6 & is.na(data_unique$WeightDry))] <- 0.04


## 10. Impute based on moisture content of other records -> change WeightDry to 0.13
tmp <- data_unique[which(data_unique$SampleDate == "19870915" & data_unique$Checked_Name == "红柴胡" & data_unique$Plot== 10 & is.na(data_unique$WeightDry)), ]
tmp$reason <- "WeightDry was missing and was imputed based on the moisture content relationship of other samples"
tmp$Change_after <- 0.13
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data_unique$WeightDry[which(data_unique$SampleDate == "19870915" & data_unique$Checked_Name == "红柴胡" & data_unique$Plot== 10 & is.na(data_unique$WeightDry))] <- 0.13



## 11. Empty row with no data -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19880830" & data_unique$Checked_Name == "翼茎风毛菊" & is.na(data_unique$WeightDry)),]
tmp$reason <- "Duplicate row with no weight data"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19880830" & data_unique$Checked_Name == "翼茎风毛菊" & is.na(data_unique$WeightDry)),]



## 12. Two plots were recorded together and cannot be reliably separated -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19900830" & data_unique$Plot == 1),]
tmp$reason <- "Two plots were suspected to recorded together and could not be reliably separated"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <-data_unique[-which(data_unique$SampleDate == "19900830" & data_unique$Plot == 1),]



## 13. Plot number was recorded incorrectly -> change Plot to 10
tmp <- data_unique[which(data_unique$SampleDate == "19920831" & data_unique$Checked_Name == "羊草" & data_unique$Plot== 9 & data_unique$WeightDry<26),]
tmp$reason <- "Plot number was suspected to be incorrectly recorded; based on related records, Plot was changed from 9 to 10"
tmp$Change_after <- 10
change_data[["Plot"]] <- rbind(change_data[["Plot"]], tmp)
data_unique$Plot[which(data_unique$SampleDate == "19920831" & data_unique$Checked_Name == "羊草" & data_unique$Plot== 9 & data_unique$WeightDry<26)] <- 10



## 14. Two plots were recorded together and cannot be reliably separated -> delete
tmp <- data_unique[which(data_unique$SampleDate == "19920913" & data_unique$Plot== 14),]
tmp$reason <- "Two plots were suspected to recorded together and could not be reliably separated"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data_unique <- data_unique[-which(data_unique$SampleDate == "19920913" & data_unique$Plot== 14),]



## 15. Plot number was recorded incorrectly -> change Plot to 20
tmp <- data_unique[which(data_unique$SampleDate == "20050815" & data_unique$Checked_Name == "羊草" & data_unique$Plot== 19 & data_unique$WeightDry>15),]
tmp$reason <- "Plot number was suspected to be incorrectly recorded; based on record characteristics, Plot was changed from 19 to 20"
tmp$Change_after <- 20
change_data[["Plot"]] <- rbind(change_data[["Plot"]], tmp)
data_unique$Plot[which(data_unique$SampleDate == "20050815" & data_unique$Checked_Name == "羊草" & data_unique$Plot== 19 & data_unique$WeightDry>15)] <- 20



data_unique <- data_unique %>%
  group_by(SampleYear, SampleDate, Plot, Checked_Name) %>%
  summarise(
    HeightVegTiller = ifelse(
      all(is.na(HeightVegTiller)),
      NA,
      max(HeightVegTiller, na.rm = TRUE)
    ),
    HeightReprodTiller = ifelse(
      all(is.na(HeightReprodTiller)),
      NA,
      max(HeightReprodTiller, na.rm = TRUE)
    ),
    IndividualNum = sum(IndividualNum, na.rm = TRUE),
    WeightWet = sum(WeightWet, na.rm = TRUE),
    WeightDry = sum(WeightDry, na.rm = TRUE),
    .groups = "drop"
  )


data <- data_unique # 96,603 observations
length(which(data$IndividualNum < 0.1)) # 1,154 records
data$IndividualNum[which(data$IndividualNum < 0.1)] <- NA
length(which(data$WeightWet < 0.0001)) # 31,259 records
data$WeightWet[which(data$WeightWet < 0.0001)] <- NA
length(which(data$WeightDry < 0.0001)) # 3,424 records
data$WeightDry[which(data$WeightDry < 0.0001)] <- NA

# Add Latin names
data <- data %>% left_join(.,SpeInfo, by = "Checked_Name")
sum(is.na(data$AccSpeciesName)) # 4 of 96,603 observations
data$AccSpeciesName[which(is.na(data$AccSpeciesName))] <- paste0("sp", 15:18)


data$WeightDryPerIndi <- data$WeightDry/data$IndividualNum
data$DensityPerHeightVeg <- data$WeightDry/(data$IndividualNum * data$HeightVegTiller)
data$DensityPerHeightRep <- data$WeightDry/(data$IndividualNum * data$HeightReprodTiller)
result <- data %>%
  group_by(SampleDate, Plot) %>%
  summarise(plot_spe_num = n_distinct(AccSpeciesName),
            plot_dry_weight = sum(WeightDry, na.rm = TRUE))
result$SampleYear <- substr(result$SampleDate, 1, 4)

#######################################################################################
# Manually inspect data-entry errors
# Check whether the observed data patterns contain outliers
library(ggplot2)

# Variables to plot
y_vars <- c(
  "HeightVegTiller",
  "HeightReprodTiller",
  "IndividualNum",
  "WeightWet",
  "WeightDry",
  "WeightDryPerIndi",
  "DensityPerHeightVeg",
  "DensityPerHeightRep"
)

# List for storing plots
plot_list <- list()

# Loop through variables and generate plots
for (y in y_vars) {
  
  p <- ggplot(
    data,
    aes(
      x = Checked_Name,
      y = .data[[y]]
    )
  ) +
    geom_point(size = 2, alpha = 0.1) +
    theme_bw() +
    labs(
      title = y,
      x = "Species (Checked_Name)",
      y = y
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_list[[y]] <- p
  ggsave(paste0("plot_to_find_data-entry_errors_1_",y,".png"), 
         plot = p,
         width = 30,  
         height = 5, 
         dpi = 300)
}

sort(unique(data$Checked_Name))

y_vars <- c(
  "plot_spe_num",
  "plot_dry_weight"
)
for(y in y_vars){
  p <- ggplot(
    result,
    aes(
      x = SampleDate,
      y = .data[[y]]
    )
  ) +
    geom_point(size = 2, alpha = 0.5) +
    theme_bw() +
    labs(
      title = y,
      x = "SampleDate",
      y = y
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(paste0("plot_to_find_data-entry_errors_2_",y,".png"), 
         plot = p,
         width = 40,  
         height = 5, 
         dpi = 300)
}

# Variables to plot
y_vars <- c(
  "HeightVegTiller",
  "HeightReprodTiller",
  "IndividualNum",
  "WeightWet",
  "WeightDry"
)
# List for storing plots
plot_list <- list()

# Loop through variables and generate plots
for (y in y_vars) {
  
  p <- ggplot(
    data[which(data$SampleDate=="19820729"),],
    aes(
      x = Checked_Name,
      y = .data[[y]]
    )
  ) +
    geom_point(size = 2, alpha = 0.1) +
    theme_bw() +
    labs(
      title = y,
      x = "Species (Checked_Name)",
      y = y
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  plot_list[[y]] <- p
  ggsave(paste0("plot_to_find_data-entry_errors_3_",y,".png"), 
         plot = p,
         width = 30,  
         height = 5, 
         dpi = 300)
}


#######################################################################################
# Adjust column names
data$Change_after <- NA
data <- data[,c(1:3, 5:10, 4, 11:14)]
data$reason <- NA

target_cols <- colnames(data)
for(i in 1:5){
  df <- change_data[[i]]
  missing_cols <- setdiff(target_cols, colnames(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[, target_cols]
  change_data[[i]] <- df
}
data <- data[,-15]

change_data[["HeightVegTiller"]] <- data[0, ]
change_data[["HeightVegTiller"]]$reason <- character(0)

change_data[["HeightReprodTiller"]] <- data[0, ]
change_data[["HeightReprodTiller"]]$reason <- character(0)

change_data[["IndividualNum"]] <- data[0, ]
change_data[["IndividualNum"]]$reason <- character(0)


# Identify anomalous records with identical dry-weight values within the same plot
aaa <- data %>%
  group_by(SampleDate, Plot, WeightWet, WeightDry) %>%   # Group by date, plot, and weight
  filter(n() > 1) %>% 
  ungroup()
aaa <- aaa[which(aaa$WeightDry>100),]
aaa <- data %>%
  group_by(SampleDate, Plot, HeightVegTiller, HeightReprodTiller, IndividualNum, WeightWet, WeightDry) %>%   # 按日期、样地、重量分组
  filter(n() > 1) %>% 
  ungroup()
aaa <- aaa[which(aaa$WeightDry>1),]



## 1. Based on visual inspection, change Schizonepeta multifida to Leymus chinensis -> Checked_Name
tmp <- data[which(data$Checked_Name == "裂叶荆芥" &
                    data$IndividualNum > 250 &
                    data$WeightDry > 50), ]
tmp$reason <- "Based on plots for the same sampling date, this record was judged more likely to be Leymus chinensis than Schizonepeta multifida, so Checked_Name was changed"
tmp$Change_after <- "羊草"
change_data[["Checked_Name"]] <- rbind(change_data[["Checked_Name"]], tmp)
data$Checked_Name[which(data$Checked_Name == "裂叶荆芥" & data$IndividualNum >250 & data$WeightDry > 50)] <- "羊草"



## 2. 19920731 Plot 18: duplicated records include an anomalous Gentianella record that is difficult to impute -> Delete
tmp <- data[which(data$SampleDate == "19920731" &
                    data$Plot == 18), ]
tmp$reason <- "This plot includes an anomalous record of Gentianella that could not be reliably imputed, so the plot was deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19920731" & data$Plot == 18),]



## 3. The following plots contain completely identical data for different species within the same plot -> Delete
tmp <- data[which(data$SampleDate == "19820830" &
                    data$Plot == 3), ]
tmp$reason <- "Records of 2 different species within the same plot had exactly identical values, indicating unreliable duplicated data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19820830" & data$Plot == 3),]



tmp <- data[which(data$SampleDate == "19830701" &
                    data$Plot == 19), ]
tmp$reason <- "Records of 2 different species within the same plot had exactly identical values, indicating unreliable duplicated data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19830701" & data$Plot == 19),]



tmp <- data[which(data$SampleDate == "19840915" &
                    data$Plot == 16), ]
tmp$reason <- "Records of 2 different species within the same plot had exactly identical values, indicating unreliable duplicated data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19840915" & data$Plot == 16),]



tmp <- data[which(data$SampleDate == "19850714" &
                    data$Plot == 5), ]
tmp$reason <- "Records of 2 different species within the same plot had exactly identical values, indicating unreliable duplicated data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19850714" & data$Plot ==5 ),]



tmp <- data[which(data$SampleDate == "19860916" &
                    data$Plot == 20), ]
tmp$reason <- "Records of 2 different species within the same plot had exactly identical values, indicating unreliable duplicated data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19860916" & data$Plot == 20),]



tmp <- data[which(data$SampleDate == "19880615" &
                    data$Plot == 11), ]
tmp$reason <- "Records of 2 different species within the same plot had exactly identical values, indicating unreliable duplicated data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19880615" & data$Plot == 11),]


## 4. One plot number was omitted -> Plot
tmp <- data[which(is.na(data$Plot)), ]
tmp$reason <- "Plot number was missing and was filled as 20 based on the note that one plot number had been omitted"
tmp$Change_after <- 20
change_data[["Plot"]] <- rbind(change_data[["Plot"]], tmp)
data$Plot[which(is.na(data$Plot))] <- 20


## 5. HeightVegTiller is implausibly high -> HeightVegTiller
tmp <- data[which(data$HeightVegTiller > 200), ]
tmp$reason <- "Vegetative tiller height was implausibly high and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$HeightVegTiller>200)] <- NA



tmp <- data[which(data$SampleDate == "19820615" &
                    data$Plot == 8 &
                    data$Checked_Name == "糙隐子草"), ]
tmp$reason <- "Vegetative tiller height was judged implausibly high for this record and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$SampleDate == "19820615" & data$Plot == 8 & data$Checked_Name == "糙隐子草")] <- NA



tmp <- data[which(data$Checked_Name == "鸡冠茶" &
                    data$HeightVegTiller > 50), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "鸡冠茶" & data$HeightVegTiller >50)] <- NA



tmp <- data[which(data$Checked_Name == "狗娃花" &
                    data$HeightVegTiller > 100), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "狗娃花" & data$HeightVegTiller >100)] <- NA



tmp <- data[which(data$Checked_Name == "黄囊薹草" &
                    data$HeightVegTiller > 70), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "黄囊薹草" & data$HeightVegTiller >70)] <- NA



tmp <- data[which(data$Checked_Name == "灰绿藜" &
                    data$HeightVegTiller > 70), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "灰绿藜" & data$HeightVegTiller >70)] <- NA


tmp <- data[which(data$Checked_Name == "瓦松" &
                    data$HeightVegTiller > 70), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "瓦松" & data$HeightVegTiller >70)] <- NA



tmp <- data[which(data$Checked_Name == "山韭" &
                    data$HeightVegTiller > 60), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "山韭" & data$HeightVegTiller >60)] <- NA



tmp <- data[which(data$Checked_Name == "羊草" &
                    data$HeightVegTiller > 100), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "羊草" & data$HeightVegTiller >100)] <- NA



## 6. HeightReprodTiller is implausibly high -> HeightReprodTiller
tmp <- data[which(data$HeightReprodTiller > 200), ]
tmp$reason <- "Reproductive tiller height was implausibly high and was set to NA"
tmp$Change_after <- NA
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$HeightReprodTiller[which(data$HeightReprodTiller>200)] <- NA



tmp <- data[which(data$SampleDate == "19820630" &
                    data$Plot == 7 &
                    data$Checked_Name == "轴藜"), ]
tmp$reason <- "Reproductive tiller height was judged implausibly high for this record and was set to NA"
tmp$Change_after <- NA
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$HeightReprodTiller[which(data$SampleDate == "19820630" & data$Plot == 7 & data$Checked_Name == "轴藜")] <- NA



tmp <- data[which(data$SampleDate == "19840730" &
                    data$Plot == 7 &
                    data$Checked_Name == "羊草"), ]
tmp$reason <- "Reproductive tiller height was judged implausibly high for this record and was set to NA"
tmp$Change_after <- NA
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$HeightReprodTiller[which(data$SampleDate == "19840730" & data$Plot == 7 & data$Checked_Name == "羊草")] <- NA



tmp <- data[which(data$SampleDate == "19920731" &
                    data$Plot == 3 &
                    data$Checked_Name == "野韭"), ]
tmp$reason <- "Reproductive tiller height was judged implausibly high for this record and was set to NA"
tmp$Change_after <- NA
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$HeightReprodTiller[which(data$SampleDate == "19920731" & data$Plot == 3 & data$Checked_Name == "野韭")] <- NA



tmp <- data[which(data$SampleDate == "19970803" &
                    data$Plot == 17 &
                    data$Checked_Name == "钝叶瓦松"), ]
tmp$reason <- "Reproductive tiller height was judged implausibly high for this record and was set to NA"
tmp$Change_after <- NA
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$HeightReprodTiller[which(data$SampleDate == "19970803" & data$Plot == 17 & data$Checked_Name == "钝叶瓦松")] <- NA



tmp <- data[which(data$Checked_Name == "硬质早熟禾" &
                    data$HeightReprodTiller > 100), ]
tmp$reason <- "Reproductive tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$HeightReprodTiller[which(data$Checked_Name == "硬质早熟禾" & data$HeightReprodTiller >100)] <- NA



## 7. IndividualNum is implausibly low -> IndividualNum
tmp <- data[which(data$Checked_Name == "冰草" &
                    data$WeightDryPerIndi > 25), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "冰草" & data$WeightDryPerIndi>25)] <- NA



tmp <- data[which(data$Checked_Name == "柔毛蒿" &
                    data$WeightDryPerIndi > 25), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "柔毛蒿" & data$WeightDryPerIndi>25)] <- NA



tmp <- data[which(data$Checked_Name == "大针茅" &
                    data$WeightDryPerIndi > 25), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "大针茅" & data$WeightDryPerIndi>25)] <- NA



tmp <- data[which(data$Checked_Name == "多叶棘豆" &
                    data$WeightDryPerIndi > 25), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "多叶棘豆" & data$WeightDryPerIndi>25)] <- NA



tmp <- data[which(data$Checked_Name == "黄囊薹草" &
                    data$WeightDryPerIndi > 10), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "黄囊薹草" & data$WeightDryPerIndi>10)] <- NA



tmp <- data[which(data$Checked_Name == "羊茅" &
                    data$WeightDryPerIndi > 20), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "羊茅" & data$WeightDryPerIndi>20)] <- NA



## 8. IndividualNum is implausibly high -> IndividualNum
tmp <- data[which((data$IndividualNum > 300 &
                     !(data$Checked_Name == "黄囊薹草" |
                         data$Checked_Name == "羊草" |
                         data$Checked_Name == "猪毛菜")) |
                    (data$IndividualNum > 500 &
                       data$Checked_Name == "黄囊薹草")), ]
tmp$reason <- "Individual number was implausibly large for the species and was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which((data$IndividualNum>300 & !(data$Checked_Name=="黄囊薹草" | data$Checked_Name=="羊草" | data$Checked_Name=="猪毛菜")) | (data$IndividualNum>500 & data$Checked_Name=="黄囊薹草"))] <- NA



tmp <- data[which(data$Checked_Name == "花苜蓿" &
                    data$IndividualNum > 100), ]
tmp$reason <- "Individual number was too large for this species and was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "花苜蓿" & data$IndividualNum >100)] <- NA



tmp <- data[which(data$Checked_Name == "硬质早熟禾" &
                    data$IndividualNum > 125), ]
tmp$reason <- "Individual number was too large for this species and was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "硬质早熟禾" & data$IndividualNum >125)] <- NA



tmp <- data[which(data$Checked_Name == "羽茅" &
                    data$IndividualNum > 125), ]
tmp$reason <- "Individual number was too large for this species and was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "羽茅" & data$IndividualNum >125)] <- NA


## 9. Plant height and individual number were suspected to have been reversed -> IndividualNum and HeightReprodTiller
idx <- which(data$Checked_Name == "硬质早熟禾" &
               data$IndividualNum > 55)
tmp <- data[idx, ]
tmp$reason <- "Individual number and reproductive tiller height were suspected to have been reversed; IndividualNum was changed to 1 and HeightReprodTiller was changed to 62"
tmp$Change_after <- "IndividualNum=1; HeightReprodTiller=62"
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
change_data[["HeightReprodTiller"]] <- rbind(change_data[["HeightReprodTiller"]], tmp)
data$IndividualNum[idx] <- 1
data$HeightReprodTiller[idx] <- 62


## 10. WeightWet is extremely large and implausible -> WeightWet
tmp <- data[which(data$WeightWet > 1000), ]
tmp$reason <- "Wet weight was implausibly large and was set to NA"
tmp$Change_after <- NA
change_data[["WeightWet"]] <- rbind(change_data[["WeightWet"]], tmp)
data$WeightWet[which(data$WeightWet>1000)] <- NA



tmp <- data[which(data$SampleDate == "19890830" &
                    data$WeightWet > 200 &
                    data$Checked_Name == "冰草"), ]
tmp$reason <- "Wet weight was too large for this species on this sampling date and was set to NA"
tmp$Change_after <- NA
change_data[["WeightWet"]] <- rbind(change_data[["WeightWet"]], tmp)
data$WeightWet[which(data$SampleDate == "19890830" & data$WeightWet > 200 & data$Checked_Name == "冰草")] <- NA

# Check other wet- and dry-weight records based on the relationship between wet and dry weight

# Orostachys has relatively high water content, so check it separately first
change_data_weight <- data[which((data$Checked_Name=="瓦松" | data$Checked_Name=="钝叶瓦松") & ((data$WeightWet/data$WeightDry < 2) | (data$WeightWet/data$WeightDry>20))),]
# Add a row ID to change_data_weight to facilitate row-wise calculations
change_data_weight <- change_data_weight %>%
  mutate(row_id = row_number())
# Define the calculation function
calc_range <- function(row) {
  
  sd <- row$SampleDate
  cn <- row$Checked_Name
  pl <- row$Plot
  
  # Filter the corresponding records in data and exclude the focal anomalous record itself
  temp <- data %>%
    filter(
      SampleDate == sd,
      Checked_Name == cn,
      !(SampleDate == sd & Plot == pl & Checked_Name == cn)
    )
  
  # Return NA if no usable data are available
  if (nrow(temp) == 0) {
    return(data.frame(
      Wet_Range = NA,
      Dry_Range = NA,
      Wet_InRange = NA,
      Dry_InRange = NA
    ))
  }
  
  # Calculate the mean and standard deviation
  wet_mean <- mean(temp$WeightWet, na.rm = TRUE)
  wet_sd   <- sd(temp$WeightWet, na.rm = TRUE)
  
  dry_mean <- mean(temp$WeightDry, na.rm = TRUE)
  dry_sd   <- sd(temp$WeightDry, na.rm = TRUE)
  
  indi_mean <- mean(temp$IndividualNum, na.rm = TRUE)
  
  water <- round(wet_mean/dry_mean, 2)
  wei_indi <- round(dry_mean/indi_mean, 2)
  
  # Calculate the intervals
  wet_low <- wet_mean - 2 * wet_sd
  wet_high <- wet_mean + 2 * wet_sd
  
  dry_low <- dry_mean - 2 * dry_sd
  dry_high <- dry_mean + 2 * dry_sd
  
  # Create interval strings
  wet_range <- paste0("[", round(wet_low, 3), ", ", round(wet_high, 3), "]")
  dry_range <- paste0("[", round(dry_low, 3), ", ", round(dry_high, 3), "]")
  
  # Determine whether each value falls within the interval
  wet_in <- row$WeightWet >= wet_low & row$WeightWet <= wet_high
  dry_in <- row$WeightDry >= dry_low & row$WeightDry <= dry_high
  
  data.frame(
    Wet_Range = wet_range,
    Dry_Range = dry_range,
    Wet_InRange = wet_in,
    Dry_InRange = dry_in,
    Water = water,
    Wei_indi = wei_indi
  )
}

# Apply the function to each row
result <- change_data_weight %>%
  rowwise() %>%
  do(cbind(., calc_range(.))) %>%
  ungroup()

# Remove the helper column
result <- result %>%
  select(-row_id)

# Replace the original object
change_data_weight <- result
change_data_weight <- change_data_weight[,-c(11:13)]
write.xlsx(change_data_weight, file = "change_data_weight_瓦松.xlsx")

# Manually inspect each record and store the results in "WeightWet1" and "WeightDry1"
change_data_weight <- read.xlsx("change_data_weight_瓦松_new.xlsx",sheetIndex = 1) 

# 15 records in total
change_data_weight1 <- change_data_weight %>%
  filter(
    !(
      coalesce(WeightWet1 == WeightWet, 
               is.na(WeightWet1) & is.na(WeightWet)) &
        coalesce(WeightDry1 == WeightDry, 
                 is.na(WeightDry1) & is.na(WeightDry))
    )
  )
change_data_weight1$WeightWet1 <- as.numeric(change_data_weight1$WeightWet1)
change_data_weight1$WeightDry1 <- as.numeric(change_data_weight1$WeightDry1)

# Other records with anomalous relationships between dry and wet weight
change_data_weight <- data[which((!(data$Checked_Name=="瓦松" | data$Checked_Name=="钝叶瓦松")) &((data$WeightWet/data$WeightDry<1) | (data$WeightWet/data$WeightDry>20 | (data$WeightWet/data$WeightDry>10 & data$WeightDry>0.5)))),]

# Extract all anomalous-record keys first so they can be excluded consistently
abno_keys <- change_data_weight %>%
  select(SampleDate, Checked_Name, Plot) %>%
  distinct()

# Define the calculation function
calc_range <- function(row, abno_keys) {
  
  sd <- row$SampleDate
  cn <- row$Checked_Name
  
  # Identify all anomalous Plot values within the group
  bad_plots <- abno_keys %>%
    filter(
      SampleDate == sd,
      Checked_Name == cn
    ) %>%
    pull(Plot)
  
  # Filter normal records from data, excluding all anomalous records
  temp <- data %>%
    filter(
      SampleDate == sd,
      Checked_Name == cn,
      !Plot %in% bad_plots
    )
  
  # Return NA if no valid data are available
  if (nrow(temp) == 0) {
    return(data.frame(
      Wet_Range = NA,
      Dry_Range = NA,
      Wet_InRange = NA,
      Dry_InRange = NA
    ))
  }
  
  # Calculate means and standard deviations
  wet_mean <- mean(temp$WeightWet, na.rm = TRUE)
  wet_sd   <- sd(temp$WeightWet, na.rm = TRUE)
  
  dry_mean <- mean(temp$WeightDry, na.rm = TRUE)
  dry_sd   <- sd(temp$WeightDry, na.rm = TRUE)
  
  indi_mean <- mean(temp$IndividualNum, na.rm = TRUE)
  
  water <- round(wet_mean/dry_mean, 2)
  wei_indi <- round(dry_mean/indi_mean, 2)
  
  # Calculate intervals
  wet_low <- wet_mean - 2 * wet_sd
  wet_high <- wet_mean + 2 * wet_sd
  
  dry_low <- dry_mean - 2 * dry_sd
  dry_high <- dry_mean + 2 * dry_sd
  
  # Create interval strings
  wet_range <- paste0("[", round(wet_low, 3), ", ", round(wet_high, 3), "]")
  dry_range <- paste0("[", round(dry_low, 3), ", ", round(dry_high, 3), "]")
  
  # Determine whether each value falls within the interval
  wet_in <- row$WeightWet >= wet_low & row$WeightWet <= wet_high
  dry_in <- row$WeightDry >= dry_low & row$WeightDry <= dry_high
  
  data.frame(
    Wet_Range = wet_range,
    Dry_Range = dry_range,
    Wet_InRange = wet_in,
    Dry_InRange = dry_in,
    Water = water,
    Wei_indi = wei_indi
  )
}

# Apply the function
change_data_weight <- change_data_weight %>%
  rowwise() %>%
  do(cbind(., calc_range(., abno_keys))) %>%
  ungroup()
change_data_weight <- change_data_weight[,-c(11:13)]
write.xlsx(change_data_weight, file = "change_data_weight_others.xlsx")

# Manually inspect each record and modify it individually
change_data_weight <- read.xlsx("change_data_weight_others_new.xlsx",sheetIndex = 1) 
change_data_weight[change_data_weight == "NA"] <- NA

# 395 records
change_data_weight <- change_data_weight %>%
  filter(
    !(
      coalesce(WeightWet1 == WeightWet, 
               is.na(WeightWet1) & is.na(WeightWet)) &
        coalesce(WeightDry1 == WeightDry, 
                 is.na(WeightDry1) & is.na(WeightDry))
    )
  )

change_data_weight$WeightWet1 <- as.numeric(change_data_weight$WeightWet1)
change_data_weight$WeightDry1 <- as.numeric(change_data_weight$WeightDry1)

change_data_weight <- change_data_weight[,-1]
change_data_weight1 <- change_data_weight1[,-1]
change_data_weight <- rbind(change_data_weight, change_data_weight1)
# 410 records
write.xlsx(change_data_weight, file = "change_data_weight_all.xlsx", row.names = F)
change_data_weight <- read.xlsx("change_data_weight_all.xlsx",sheetIndex = 1) 


change_data_weight <- change_data_weight[,-c(1,12:18)]
change_data_weight[["WeightDryPerIndi"]] <- NA
change_data_weight[["DensityPerHeightVeg"]] <- NA
change_data_weight[["DensityPerHeightRep"]] <- NA
change_data_weight <- change_data_weight[,c(1:10,13:15,11,12)]


df_wet_diff <- change_data_weight[
  is.na(change_data_weight$WeightWet) != is.na(change_data_weight$WeightWet1) |
    (!is.na(change_data_weight$WeightWet) &
       !is.na(change_data_weight$WeightWet1) &
       change_data_weight$WeightWet != change_data_weight$WeightWet1),
]
df_wet_diff <- df_wet_diff[,-15]
colnames(df_wet_diff)[14] <- "Change_after"
df_wet_diff$reason <- "WeightWet<WeightDry or WeightWet>>WeightDry, and each record was manually checked to find out whether it was really unreasonable data"
change_data[["WeightWet"]] <- rbind(change_data[["WeightWet"]], df_wet_diff)

df_dry_diff <- change_data_weight[
  is.na(change_data_weight$WeightDry) != is.na(change_data_weight$WeightDry1) |
    (!is.na(change_data_weight$WeightDry) &
       !is.na(change_data_weight$WeightDry1) &
       change_data_weight$WeightDry != change_data_weight$WeightDry1),
]
df_dry_diff <- df_dry_diff[,-14]
colnames(df_dry_diff)[14] <- "Change_after"
df_dry_diff$reason <- "WeightWet<WeightDry or WeightWet>>WeightDry, and each record was manually checked to find out whether it was really unreasonable data"
change_data[["WeightWet"]] <- rbind(change_data[["WeightWet"]], df_dry_diff)



data <- data %>%
  left_join(
    change_data_weight %>%
      mutate(flag_update = TRUE) %>%
      select(SampleDate, Plot, Checked_Name,
             WeightWet1, WeightDry1, flag_update),
    by = c("SampleDate", "Plot", "Checked_Name")
  ) %>%
  mutate(
    WeightWet = if_else(!is.na(flag_update),
                        WeightWet1,
                        WeightWet),
    
    WeightDry = if_else(!is.na(flag_update),
                        WeightDry1,
                        WeightDry)
  ) %>%
  select(-WeightWet1, -WeightDry1, -flag_update)

result <- data %>%
  group_by(SampleDate, Plot) %>%
  summarise(plot_spe_num = n_distinct(AccSpeciesName),
            plot_dry_weight = sum(WeightDry, na.rm = TRUE))
result$SampleYear <- substr(result$SampleDate, 1, 4)

data$WeightDryPerIndi <- data$WeightDry/data$IndividualNum
data$DensityPerHeightVeg <- data$WeightDry/(data$IndividualNum * data$HeightVegTiller)
data$DensityPerHeightRep <- data$WeightDry/(data$IndividualNum * data$HeightReprodTiller)


#######################################################################################
sum(is.na(data$WeightDry)) # 3,640 of 96,490 observations
sum(is.na(data$IndividualNum)) # 1,169 of 96,490 observations
sum(is.na(data$WeightWet)) # 31,400 of 96,490 observations
sum(is.na(data$HeightVegTiller)) # 17,488 of 96,490 observations
sum(is.na(data$HeightReprodTiller)) # 71,551 of 96,490 observations

#######################################################################################
## Data from the sampling period with the highest biomass
result <- data %>%
  # First group by year and sampling date
  group_by(SampleYear, SampleDate) %>%
  # Calculate the total dry weight and the number of plots for each sampling date
  summarise(
    TotalWeightDry = sum(WeightDry, na.rm = TRUE),
    NumPlots = n_distinct(Plot),  # Calculate the number of distinct plots for this sampling date
    MeanWeightPerPlot = TotalWeightDry / NumPlots  # Mean dry weight per plot
  ) %>%
  # Within each year, identify the sampling date with the highest mean dry weight
  group_by(SampleYear) %>%
  filter(MeanWeightPerPlot == max(MeanWeightPerPlot, na.rm = TRUE)) %>%
  # Sort by year
  arrange(SampleYear) %>%
  # Select the required columns
  select(SampleYear, SampleDate, MeanWeightPerPlot)

data1 <- data[data$SampleDate %in% result$SampleDate, ]
sum(is.na(data1$WeightDry)) # 266 of 12,381 observations
sum(is.na(data1$IndividualNum)) # 80 of 12,381 observations
sum(is.na(data1$WeightWet)) # 4,552 of 12,381 observations
sum(is.na(data1$HeightVegTiller)) # 3,337 of 12,381 observations
sum(is.na(data1$HeightReprodTiller)) # 7,062 of 12,381 observations


## Data from the three sampling periods with the highest biomass
result <- data %>%
  # First group by year and sampling date
  group_by(SampleYear, SampleDate) %>%
  # Calculate the total dry weight and the number of plots for each sampling date
  summarise(
    TotalWeightDry = sum(WeightDry, na.rm = TRUE),
    NumPlots = n_distinct(Plot),  # Calculate the number of distinct plots for this sampling date
    MeanWeightPerPlot = TotalWeightDry / NumPlots  # Mean dry weight per plot
  ) %>%
  # Select the three sampling dates with the highest mean dry weight in each year
  group_by(SampleYear) %>%
  slice_max(
    order_by = MeanWeightPerPlot,
    n = 3
  ) %>%
  # Sort by year
  arrange(SampleYear) %>%
  # Select the required columns
  select(SampleYear, SampleDate, MeanWeightPerPlot)

write.xlsx(as.data.frame(result), file = "three_sampling_periods_with_the_highest_biomass_1.xlsx", row.names = F)

# Carefully inspect these three sampling periods for anomalous records

library(ggplot2)
library(dplyr)
library(gridExtra)

y_vars <- c(
  "HeightVegTiller",
  "IndividualNum",
  "WeightWet",
  "WeightDry",
  "WeightDryPerIndi",
  "DensityPerHeightVeg"
)


dates <- unique(result$SampleDate)
plot_list <- list()
for (d in dates) {
  df_sub <- data %>% 
    filter(SampleDate == d)
  date_plots <- list()
  for (y in y_vars) {
    p <- ggplot(
      df_sub,
      aes(
        x = Checked_Name,
        y = .data[[y]],
        color = is.na(WeightDry)
      )
    ) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(
        values = c("FALSE" = "black", "TRUE" = "red"),
        guide = "none"
      ) +
      theme_bw() +
      labs(
        x = "Species (Checked_Name)",
        y = y
      ) +
      annotate("text", x = -Inf, y = Inf, 
               label = d, hjust = -0.1, vjust = 1.2) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    date_plots[[y]] <- p
  }
  plot_list[[as.character(d)]] <- date_plots
}
col_plot_list <- list()
for (y in y_vars) {
  tmp <- list()
  for (d in names(plot_list)) {
    tmp[[d]] <- plot_list[[d]][[y]]
  }
  col_plot_list[[y]] <- tmp
}

nrow_per_page <- 7
ncol_per_page <- 3
plots_per_page <- nrow_per_page * ncol_per_page
for (y in names(col_plot_list)) {
  plot_vec <- col_plot_list[[y]]
  total_pages <- ceiling(length(plot_vec) / plots_per_page)
  for (page in 1:total_pages) {
    start_index <- (page - 1) * plots_per_page + 1
    end_index <- min(page * plots_per_page,
                     length(plot_vec))
    current_plots <- plot_vec[start_index:end_index]
    combined_plot <- grid.arrange(
      grobs = current_plots,
      nrow = nrow_per_page,
      ncol = ncol_per_page,
      padding = unit(1, "lines")
    )
    ggsave(
      filename = paste0(y, "_Page_", page, ".png"),
      plot = combined_plot,
      width = 32,
      height = 60,
      dpi = 300,
      limitsize = FALSE
    )
  }
}

rm(list = c("col_plot_list", "tmp", "plot_list", "p", "plot_vec", "current_plots", "combined_plot", "date_plots"))
gc()

## 1. Vegetative tiller height is too low -> HeightVegTiller
tmp <- data[which(data$SampleDate == "19870915" &
                    data$Checked_Name == "羊草" &
                    data$HeightVegTiller < 5), ]
tmp$reason <- "Vegetative tiller height was too low for Leymus chinensis and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$SampleDate == "19870915" & data$Checked_Name == "羊草" & data$HeightVegTiller <5)] <- NA



## 2. Dry weight is abnormally large and difficult to impute -> delete the entire plot
tmp <- data[which(data$SampleDate == "19920831" &
                    data$Plot == 11), ]
tmp$reason <- "Dry weight was abnormally large in this plot and could not be reliably imputed, so the plot was deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19920831" & data$Plot == 11),]



## 3. Dry weight exactly equals the total dry weight of the other individuals in the same plot and no other information is available; suspected recording error -> delete this record and store it in Delete
tmp <- data[which(data$SampleDate == "19920831" &
                    data$Checked_Name == "狗娃花" &
                    data$Plot == 3), ]
tmp$reason <- "WeightDry was exactly equal to the total dry weight of the other individuals in the same plot, with no additional supporting information, so it was judged to be a recording error and the record was deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "19920831" & data$Checked_Name == "狗娃花" & data$Plot == 3),]



## 4. Dry weight is abnormally large -> change WeightDry to 21.19
tmp <- data[which(data$SampleDate == "20080915" &
                    data$Checked_Name == "羽茅" &
                    data$Plot == 18), ]
tmp$reason <- "Dry weight was abnormally large and was corrected to 21.19"
tmp$Change_after <- 21.19
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(data$SampleDate == "20080915" & data$Checked_Name == "羽茅" & data$Plot == 18)] <- 21.19



## 5. Individual number is too small -> IndividualNum
tmp <- data[which(data$SampleDate == "19820814" &
                    data$Checked_Name == "菊叶委陵菜" &
                    data$WeightDryPerIndi > 5), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$SampleDate == "19820814" & data$Checked_Name == "菊叶委陵菜" & data$WeightDryPerIndi>5)] <- NA


# Check whether any additional observations fall far outside the multi-year variation range
dates <- unique(result$SampleDate)
plot_list <- list()

df_sub <- data %>% 
  filter(SampleDate %in% dates)
date_plots <- list()
for (y in y_vars) {
  p <- ggplot(
    df_sub,
    aes(
      x = Checked_Name,
      y = .data[[y]],
      color = is.na(WeightDry)
    )
  ) +
    geom_point(size = 2, alpha = 0.02) +
    scale_color_manual(
      values = c("FALSE" = "black", "TRUE" = "red"),
      guide = "none"  
    ) +
    theme_bw() +
    labs(
      x = "Species (Checked_Name)",
      y = y
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  date_plots[[y]] <- p
}
for(y in y_vars){
  ggsave(
    filename = paste0("all_year_",y, ".png"),
    plot = date_plots[[y]],
    width = 40,
    height = 5,
    dpi = 300,
    limitsize = FALSE
  )
}

data$WeightDryPerIndi <- data$WeightDry/data$IndividualNum
data$DensityPerHeightVeg <- data$WeightDry/(data$IndividualNum * data$HeightVegTiller)
data$DensityPerHeightRep <- data$WeightDry/(data$IndividualNum * data$HeightReprodTiller)

## 1. Vegetative tiller height is too high -> HeightVegTiller
tmp <- data[which(data$Checked_Name == "星毛委陵菜" &
                    data$HeightVegTiller > 50), ]
tmp$reason <- "Vegetative tiller height was too high for this species and was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "星毛委陵菜" & data$HeightVegTiller >50)] <- NA



## 2. Individual number is too large -> IndividualNum
tmp <- data[which(data$Checked_Name == "糙隐子草" &
                    data$IndividualNum > 50), ]
tmp$reason <- "Individual number was too large for this species and was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "糙隐子草" & data$IndividualNum >50)] <- NA



tmp <- data[which(data$Checked_Name == "大针茅" &
                    data$IndividualNum > 100 &
                    data$SampleDate == "19990810"), ]
tmp$reason <- "Individual number was too large for this species on this sampling date and was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "大针茅" & data$IndividualNum >100 & data$SampleDate == "19990810")] <- NA



## 3. Dry weight is abnormally large -> WeightDry
tmp <- data[which(data$Checked_Name == "山韭" &
                    data$WeightDryPerIndi > 30), ]
tmp$reason <- "Dry weight was abnormally large and was corrected to 4.22"
tmp$Change_after <- 4.22
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(data$Checked_Name == "山韭" & data$WeightDryPerIndi > 30)] <- 4.22



tmp <- data[which(data$Checked_Name == "瓣蕊唐松草" &
                    data$DensityPerHeightVeg > 0.5 &
                    data$SampleDate == "19910714"), ]
tmp$reason <- "Dry weight was abnormally large and was set to NA"
tmp$Change_after <- NA
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(data$Checked_Name == "瓣蕊唐松草" & data$DensityPerHeightVeg > 0.5 & data$SampleDate == "19910714")] <- NA



tmp <- data[which(data$Checked_Name == "红纹马先蒿" &
                    data$DensityPerHeightVeg > 1), ]
tmp$reason <- "Dry weight was abnormally large and was corrected to 1.67"
tmp$Change_after <- 1.67
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(data$Checked_Name == "红纹马先蒿" & data$DensityPerHeightVeg > 1)] <- 1.67


tmp <- data[which(data$Checked_Name == "花苜蓿" &
                    data$DensityPerHeightVeg > 1), ]
tmp$reason <- "Dry weight was abnormally large; both WeightDry and WeightWet were set to NA"
tmp$Change_after <- NA
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
change_data[["WeightWet"]] <- rbind(change_data[["WeightWet"]], tmp)
data$WeightDry[which(data$Checked_Name == "花苜蓿" & data$DensityPerHeightVeg > 1)] <- NA
data$WeightWet[which(data$Checked_Name == "花苜蓿" & data$DensityPerHeightVeg > 1)] <- NA


tmp <- data[which(data$Checked_Name == "花苜蓿" &
                    data$WeightDry > 30), ]
tmp$reason <- "Dry weight was abnormally large; both WeightDry and WeightWet were set to NA"
tmp$Change_after <- NA
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
change_data[["WeightWet"]] <- rbind(change_data[["WeightWet"]], tmp)
data$WeightWet[which(data$Checked_Name == "花苜蓿" & data$WeightDry > 30)] <- NA
data$WeightDry[which(data$Checked_Name == "花苜蓿" & data$WeightDry > 30)] <- NA



tmp <- data[which(data$Checked_Name == "野韭" &
                    data$DensityPerHeightVeg > 0.3 &
                    (data$SampleDate == "19830730" |
                       data$SampleDate == "19910714" |
                       data$SampleDate == "19920831")), ]
tmp$reason <- "Dry weight was abnormally large and was set to NA"
tmp$Change_after <- NA
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(data$Checked_Name == "野韭" & data$DensityPerHeightVeg > 0.3 & (data$SampleDate == "19830730" | data$SampleDate == "19910714" | data$SampleDate == "19920831"))] <- NA


tmp <- data[which(data$Checked_Name == "猪毛菜" &
                    data$DensityPerHeightVeg > 0.5), ]
tmp$reason <- "Dry weight was abnormally large and was set to NA"
tmp$Change_after <- NA
change_data[["WeightDry"]] <- rbind(change_data[["WeightDry"]], tmp)
data$WeightDry[which(data$Checked_Name == "猪毛菜" & data$DensityPerHeightVeg > 0.5)] <- NA



## 4. Individual number is too small -> IndividualNum
tmp <- data[which(data$Checked_Name == "羽茅" &
                    data$WeightDryPerIndi > 15), ]
tmp$reason <- "Individual number was judged too small because dry weight per individual was implausibly large for this species, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "羽茅" & data$WeightDryPerIndi > 15)] <- NA



tmp <- data[which(data$Checked_Name == "羊草" &
                    data$DensityPerHeightVeg > 0.3 &
                    data$SampleDate == "20120916"), ]
tmp$reason <- "Individual number was judged too small because the density-to-height relationship was implausibly large for this species on this sampling date, so IndividualNum was set to NA"
tmp$Change_after <- NA
change_data[["IndividualNum"]] <- rbind(change_data[["IndividualNum"]], tmp)
data$IndividualNum[which(data$Checked_Name == "羊草" & data$DensityPerHeightVeg > 0.3 & data$SampleDate == "20120916")] <- NA



## 5. Vegetative tiller height is too low -> HeightVegTiller
tmp <- data[which(data$Checked_Name == "串铃草" &
                    data$DensityPerHeightVeg > 1), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "串铃草" & data$DensityPerHeightVeg > 1)] <- NA



tmp <- data[which(data$Checked_Name == "瓣蕊唐松草" &
                    data$DensityPerHeightVeg > 0.5 &
                    data$SampleDate == "19900730"), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species on this sampling date, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "瓣蕊唐松草" & data$DensityPerHeightVeg > 0.5 & data$SampleDate == "19900730")] <- NA



tmp <- data[which(data$Checked_Name == "钝叶瓦松" &
                    data$DensityPerHeightVeg > 0.8), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "钝叶瓦松" & data$DensityPerHeightVeg > 0.8)] <- NA


tmp <- data[which(data$Checked_Name == "鳞叶龙胆" &
                    data$DensityPerHeightVeg > 1), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "鳞叶龙胆" & data$DensityPerHeightVeg > 1)] <- NA



tmp <- data[which(data$Checked_Name == "瓦松" &
                    data$DensityPerHeightVeg > 1), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "瓦松" & data$DensityPerHeightVeg > 1)] <- NA



tmp <- data[which(data$Checked_Name == "野亚麻" &
                    data$DensityPerHeightVeg > 1), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "野亚麻" & data$DensityPerHeightVeg > 1)] <- NA



tmp <- data[which(data$Checked_Name == "硬质早熟禾" &
                    data$DensityPerHeightVeg > 0.3), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "硬质早熟禾" & data$DensityPerHeightVeg > 0.3)] <- NA


tmp <- data[which(data$Checked_Name == "猪毛蒿" &
                    data$DensityPerHeightVeg > 0.5), ]
tmp$reason <- "Vegetative tiller height was judged too low because the density-to-height relationship was implausibly large for this species, so HeightVegTiller was set to NA"
tmp$Change_after <- NA
change_data[["HeightVegTiller"]] <- rbind(change_data[["HeightVegTiller"]], tmp)
data$HeightVegTiller[which(data$Checked_Name == "猪毛蒿" & data$DensityPerHeightVeg > 0.5)] <- NA



## 6. Empty row with no data -> Delete
tmp <- data[which(data$SampleDate == "20210914" &
                    is.na(data$WeightDry) &
                    data$Plot == 20), ]
tmp$reason <- "Empty row with no weight data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "20210914" & is.na(data$WeightDry) & data$Plot == 20),]



tmp <- data[which(data$SampleDate == "20240831" &
                    is.na(data$WeightDry) &
                    data$Plot == 1), ]
tmp$reason <- "Empty row with no weight data; deleted"
tmp$Change_after <- NA
change_data[["Delete"]] <- rbind(change_data[["Delete"]], tmp)
data <- data[-which(data$SampleDate == "20240831" & is.na(data$WeightDry) & data$Plot == 1),]


# Add Latin names
data <- data[, -c(9,11:14)]
data <- data %>% left_join(.,SpeInfo, by = "Checked_Name")
sum(is.na(data$AccSpeciesName)) # 4 of 96,468 observations
data$AccSpeciesName[which(is.na(data$AccSpeciesName))] <- paste0("sp", 15:18)


data00 <- data # 96,468 observations
save(data00,file="community.Rdata")
load("community.Rdata")
data <- data00
#######################################################################################
sum(is.na(data$WeightDry)) # 3,648 of 96,468 observations
sum(is.na(data$IndividualNum)) # 1,173 of 96,468 observations
sum(is.na(data$WeightWet)) # 31,380 of 96,468 observations
sum(is.na(data$HeightVegTiller)) # 17,503 of 96,468 observations
sum(is.na(data$HeightReprodTiller)) # 71,534 of 96,468 observations

aaa <- data %>%
  group_by(SampleDate, Plot) %>%
  summarise(TotalWeightDry = sum(WeightDry, na.rm = TRUE))


# Impute missing dry-weight values
result <- data %>%
  # First group by year and sampling date
  group_by(SampleYear, SampleDate) %>%
  # Calculate total dry weight and the number of plots for each sampling date
  summarise(
    TotalWeightDry = sum(WeightDry, na.rm = TRUE),
    NumPlots = n_distinct(Plot),  # Calculate the number of distinct plots for this sampling date
    MeanWeightPerPlot = TotalWeightDry / NumPlots  # Mean dry weight per plot
  ) %>%
  # Within each year, identify the sampling date with the highest mean dry weight
  group_by(SampleYear) %>%
  filter(MeanWeightPerPlot == max(MeanWeightPerPlot, na.rm = TRUE)) %>%
  # Sort by year
  arrange(SampleYear) %>%
  # Select the required columns
  select(SampleYear, SampleDate, MeanWeightPerPlot)
write.xlsx(as.data.frame(result), file = "Max_weight_time0.xlsx", row.names = F)



### 1. Imputation based on wet weight
# Copy the data to avoid modifying the original data
data_imputed <- data
data2 <- data_imputed[data_imputed$SampleDate %in% result$SampleDate, ]
sum(is.na(data2$WeightDry)) # 256 of 12,361 records

# Get the indices of all missing values to be processed (960 records)
missing_indices <- which(is.na(data_imputed$WeightDry) & !is.na(data_imputed$WeightWet))

# Loop through each missing value
for (i in missing_indices) {
  current_sample_date <- data_imputed$SampleDate[i]
  current_species <- data_imputed$AccSpeciesName[i]
  current_wet_weight <- data_imputed$WeightWet[i]
  
  # Obtain records of the same species from other plots on the same day
  subset_data <- data_imputed[
    data_imputed$SampleDate == current_sample_date &
      data_imputed$AccSpeciesName == current_species &
      !is.na(data_imputed$WeightWet) &
      !is.na(data_imputed$WeightDry) &
      data_imputed$Plot != data_imputed$Plot[i], 
  ]
  
  data_imputed$WeightDry[i] <- current_wet_weight*sum(subset_data$WeightDry)/sum(subset_data$WeightWet)
  
}
length(which(is.na(data_imputed$WeightDry) & !is.na(data_imputed$WeightWet)))
# 409 records remain missing; 551 records were imputed in this step
data2 <- data_imputed[data_imputed$SampleDate %in% result$SampleDate, ]
sum(is.na(data2$WeightDry)) # 221 of 12,361 observations

### 2. Imputation based on individual number and vegetative tiller height
# Get the indices of all missing values to be processed (2,476 records)
missing_indices <- which(is.na(data_imputed$WeightDry) & !(is.na(data_imputed$IndividualNum) | is.na(data_imputed$HeightVegTiller)))
# Loop through each missing value
for (i in missing_indices) {
  current_sample_date <- data_imputed$SampleDate[i]
  current_species <- data_imputed$AccSpeciesName[i]
  current_individualnum <- data_imputed$IndividualNum[i]
  current_HeightVegTiller <- data_imputed$HeightVegTiller[i]
  
  # Obtain records of the same species from other plots on the same day
  subset_data <- data_imputed[
    data_imputed$SampleDate == current_sample_date &
      data_imputed$AccSpeciesName == current_species &
      !is.na(data_imputed$IndividualNum) &
      !is.na(data_imputed$HeightVegTiller) &
      !is.na(data_imputed$WeightDry) &
      data_imputed$Plot != data_imputed$Plot[i],
  ]
  
  data_imputed$WeightDry[i] <- current_individualnum*current_HeightVegTiller*mean(subset_data$WeightDry/(subset_data$IndividualNum*subset_data$HeightVegTiller))
  
}
length(which(is.na(data_imputed$WeightDry) & (!is.na(data_imputed$IndividualNum)) & (!is.na(data_imputed$HeightVegTiller))))
# 766 records remain missing; 1,710 records were imputed in this step
data2 <- data_imputed[data_imputed$SampleDate %in% result$SampleDate, ]
sum(is.na(data2$WeightDry)) # 87 of 12,361 observations

### 3. Imputation based on individual number
# Get the indices of all missing values to be processed (1,011 records)
missing_indices <- which(is.na(data_imputed$WeightDry) & !(is.na(data_imputed$IndividualNum)))
# Loop through each missing value
for (i in missing_indices) {
  current_sample_date <- data_imputed$SampleDate[i]
  current_species <- data_imputed$AccSpeciesName[i]
  current_individualnum <- data_imputed$IndividualNum[i]
  
  # Obtain records of the same species from other plots on the same day
  subset_data <- data_imputed[
    data_imputed$SampleDate == current_sample_date &
      data_imputed$AccSpeciesName == current_species &
      !is.na(data_imputed$IndividualNum) &
      !is.na(data_imputed$WeightDry) &
      data_imputed$Plot != data_imputed$Plot[i], 
  ]
  
  data_imputed$WeightDry[i] <- current_individualnum*mean(subset_data$WeightDry/(subset_data$IndividualNum))
  
}
length(which(is.na(data_imputed$WeightDry) & (!is.na(data_imputed$IndividualNum))))
# 770 records remain missing; 241 records were imputed in this step
data2 <- data_imputed[data_imputed$SampleDate %in% result$SampleDate, ]
sum(is.na(data2$WeightDry)) # 43 of 12,361 observations


### 4. Imputation based on vegetative tiller height
# Get the indices of all missing values to be processed (967 records)
missing_indices <- which(is.na(data_imputed$WeightDry) & !(is.na(data_imputed$HeightVegTiller)))
# Loop through each missing value
for (i in missing_indices) {
  current_sample_date <- data_imputed$SampleDate[i]
  current_species <- data_imputed$AccSpeciesName[i]
  current_HeightVegTiller <- data_imputed$HeightVegTiller[i]
  
  # Obtain records of the same species from other plots on the same day
  subset_data <- data_imputed[
    data_imputed$SampleDate == current_sample_date &
      data_imputed$AccSpeciesName == current_species &
      !is.na(data_imputed$HeightVegTiller) &
      !is.na(data_imputed$WeightDry) &
      data_imputed$Plot != data_imputed$Plot[i], 
  ]
  
  data_imputed$WeightDry[i] <- current_HeightVegTiller*mean(subset_data$WeightDry/(subset_data$HeightVegTiller))
  
}
length(which(is.na(data_imputed$WeightDry) &  (!is.na(data_imputed$HeightVegTiller))))
# 793 records remain missing; 174 records were imputed in this step
data2 <- data_imputed[data_imputed$SampleDate %in% result$SampleDate, ]
sum(is.na(data2$WeightDry)) # 29 of 12,361 records

data <- data_imputed


## Data from the sampling period with the highest biomass
result <- data %>%
  # First group by year and sampling date
  group_by(SampleYear, SampleDate) %>%
  # Calculate total dry weight and the number of plots for each sampling date
  summarise(
    TotalWeightDry = sum(WeightDry, na.rm = TRUE),
    NumPlots = n_distinct(Plot),  # Calculate the number of distinct plots for this sampling date
    MeanWeightPerPlot = TotalWeightDry / NumPlots  # Mean dry weight per plot
  ) %>%
  # Within each year, identify the sampling date with the highest mean dry weight
  group_by(SampleYear) %>%
  filter(MeanWeightPerPlot == max(MeanWeightPerPlot, na.rm = TRUE)) %>%
  # Sort by year
  arrange(SampleYear) %>%
  # Select the required columns
  select(SampleYear, SampleDate, MeanWeightPerPlot)


data1 <- data[data$SampleDate %in% result$SampleDate, ]
data1[which(data1$WeightWet < data1$WeightDry),] #0 records
aaa <- data1[which(is.na(data1$WeightDry)),]

sum(is.na(data1$WeightDry)) # 29 of 12,361 observations
sum(is.na(data1$IndividualNum)) # 81 of 12,361 observations
sum(is.na(data1$WeightWet)) # 4,533 of 12,361 observations
sum(is.na(data1$HeightVegTiller)) # 3,338 of 12,361 observations
sum(is.na(data1$HeightReprodTiller)) # 7,047 of 12,361 observations


# Function for handling missing dry-weight values
process_dry_weight <- function(data) {
  # Copy the data frame to avoid modifying the original data
  result <- data
  
  # Process only cases where WeightDry is NA
  na_indices <- which(is.na(result$WeightDry))
  
  for(i in na_indices) {
    height_veg <- result$HeightVegTiller[i]
    height_reprod <- result$HeightReprodTiller[i]
    if(!is.na(result$WeightWet[i])){
      result$WeightDry[i] <- result$WeightWet[i]/2
    }else if(is.na(height_veg) & is.na(height_reprod)) {
      result$WeightDry[i] <- 0.1
    }else if(max(height_veg, height_reprod, na.rm = TRUE) <= 15) {
      result$WeightDry[i] <- 0.5
    }else if(max(height_veg, height_reprod, na.rm = TRUE) <= 40){
      result$WeightDry[i] <- 1.5
    }else{
      result$WeightDry[i] <- 2
    }
  }
  
  return(result)
}
sum(is.na(data$WeightDry)) # 972 of 96,468 observations
data <- process_dry_weight(data)




## Data from the sampling period with the highest biomass
result <- data %>%
  # First group by year and sampling date
  group_by(SampleYear, SampleDate) %>%
  # Calculate total dry weight and the number of plots for each sampling date
  summarise(
    TotalWeightDry = sum(WeightDry, na.rm = TRUE),
    NumPlots = n_distinct(Plot),  # Calculate the number of distinct plots for this sampling date
    MeanWeightPerPlot = TotalWeightDry / NumPlots  # Mean dry weight per plot
  ) %>%
  # Within each year, identify the sampling date with the highest mean dry weight
  group_by(SampleYear) %>%
  filter(MeanWeightPerPlot == max(MeanWeightPerPlot, na.rm = TRUE)) %>%
  # Sort by year
  arrange(SampleYear) %>%
  # Select the required columns
  select(SampleYear, SampleDate, MeanWeightPerPlot)

save(result,file = "Max_weight_time.RData")
# 12,361 records
data1 <- data[data$SampleDate %in% result$SampleDate, ]
data1[which(data1$WeightWet < data1$WeightDry),] # 0 records

data2 <- data00[data00$SampleDate %in% result$SampleDate, ]
sum(is.na(data2$WeightDry)) # 265 of 12,361 observations

save(data,data1, file = "data_for_process.Rdata")

aaa <- change_data[["WeightDry"]][change_data[["WeightDry"]]$SampleDate %in% result$SampleDate, ]
View(change_data[["WeightDry"]])
View(change_data[["WeightWet"]])
View(change_data[["Delete"]])
View(change_data[["Checked_Name"]])
View(change_data[["Plot"]])
View(change_data[["HeightVegTiller"]])
View(change_data[["HeightReprodTiller"]])
View(change_data[["IndividualNum"]])

# Save the data-processing records as an xlsx file
library(openxlsx)
wb <- createWorkbook()
# Loop through the list and write each data frame to a separate sheet
for (sheet_name in names(change_data)) {
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, x = change_data[[sheet_name]])
}
saveWorkbook(wb, file = "0324change_data.xlsx", overwrite = TRUE)

colnames(result)
match_summary <- data.frame(
  DataFrameName = names(change_data),
  MaxWeightTimeCount = sapply(change_data, function(df) {
    sum(df$SampleDate %in% result$SampleDate, na.rm = TRUE)
  }),
  row.names = NULL
)

xlsx::write.xlsx(x = match_summary, file =  "data_change_records.xlsx", sheetName ="MaxWeightTimeCount", append = TRUE)
