# importing libraries
library(tidyverse)
library(magrittr)
library(ggplot2)
library(readr)
library(magick)
library(cowplot)
theme_set(theme_cowplot())

# Cleares the environment
rm(list=ls())


## Importing csv file ---------------
Sec_Enrol_2019 <- read_csv("../Education/Secondary_Enrolment_by_Age_2019.csv")


## Cleaning data ------------------------
# Remove council and ward
Sec_Enrol_2019 %<>% select(-COUNCIL, -WARD)

# Format columns
names(Sec_Enrol_2019) %<>% str_replace_all("[[:space:]]", "\\.")

Sec_Enrol_2019 %<>% select(Region = REGION,
                           School = SCHOOL.NAME,
                           Ownership = SCHOOL.OWNERSHIP,
                           School_Number = SCHOOL.REG..NUMBER,
                           M11 = `<.12.years-M`,
                           F11 = `<.12.years-F`,
                           M12 = `12.years-M`,
                           F12 = `12.years-F`,
                           M13 = `13.years-M`,
                           F13 = `13.years-F`,
                           M14 = `14.years-M`,
                           F14 = `14.years-F`,
                           M15 = `15.years-M`,
                           F15 = `15.years-F`,
                           M16 = `16.years-M`,
                           F16 = `16.years-F`,
                           M17 = `17.years-M`,
                           F17 = `17.years-F`,
                           M18 = `18.years-M`,
                           F18 = `18.years-F`,
                           M19 = `19.years-M`,
                           F19 = `19.years-F`,
                           M20 = `>.19.years-M`,
                           F20 = `>.19.years-F`,
                           Total_Male_Students = `Total-M`,
                           Total_Female_Students = `Total-F`, 
                           Total_Male_Teachers = `Total.Teachers-Male`,
                           Total_Female_Teachers = `Total.Teachers-Female`, 
                           Qualified_Male_Teachers = `Qualified.Teachers-Male`,
                           Qualified_Female_Teachers =  `Qualified.Teachers-Female`,
                           Pupil_Teacher_Ratio = PTR,
                           Pupil_Qualified_Teacher_Ratio = PQTR,)



# Gathering columns
Sec_Enrol_2019 %<>% gather(key="Gender_Age",
                           value="Count",
                           M11:F12,
                           M13:F13,
                           M14:F14,
                           M15:F15,
                           M16:F16,
                           M17:F17,
                           M18:F18,
                           M19:F20) 

count(Sec_Enrol_2019, Gender_Age)

# Separate Gender_Age
Sec_Enrol_2019 %<>% separate(Gender_Age,
                             into=c("Gender", "Age"),
                             sep="(?<=[A-Z]{1})", 
                             extra="merge",
                             fill="right")


Age_Gender_Count <- Sec_Enrol_2019 %>%
  group_by(Age, Gender) %>%
  summarize(Count = sum(Count, na.rm=TRUE)) %>%
  ungroup()

my_plot <- ggplot(Age_Gender_Count, aes(x=Age, y=Count, fill = Gender)) +
  geom_col(position = "dodge") + 
  scale_y_continuous(breaks=seq(0,280000,70000)) +
  scale_fill_manual(values = alpha(c("red", "blue", 0.3))) +
  labs(title = ("           United Republic of Tanzania"),
                subtitle = ("          Secondary School Enrollment by Age\n                                   2019"),
                x = ("Age of Students"), y = ("Number of Enrollments"),
                caption = ("Source: \"https://www.nbs.go.tz\"\n done by: Vivian J. Goshashy"  )) + 
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold")
  )

ggdraw() +
  draw_image("Court of Arm.png", x =0.42, y =0.39, scale = .2) +
  draw_plot(my_plot)

 ggsave("Graphs/Age_Gender_Count.jpg", device="jpg")

# The count of students in each region by Age and Sex
Age_Gender_Region_Count <- Sec_Enrol_2019 %>%
  group_by(Age, Gender, Region) %>%
  summarize(Count = sum(Count, na.rm=TRUE)) %>%
  ungroup()

# Add a ratio of Girls/Boys, total number of students by age and region, total proportion of students by age
Age_Gender_Region_Count %<>% pivot_wider(names_from = Gender, values_from = Count) %>%
  mutate(Ratio_of_G_to_B = if_else(`M` !=0,
                                   `F`/`M`,
                                   0)) %>%
  filter(Age <= 17) %>%
  mutate(Total_Count = `M` + `F`) %>%
  group_by(Region) %>%
  mutate(Total_Region_Student_Pop = sum(Total_Count, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Total_Prop = Total_Count/Total_Region_Student_Pop)

# Add a weighted ratio of Girls/Boys for each region
Age_Gender_Region_Count %<>% group_by(Region) %>%
  summarize(Weighed_Ratio = sum(Ratio_of_G_to_B*Total_Prop))


# Maps -----------------------------------------------------------------------------------------
library(sp)
library(broom)

# Read in the map files
TZA_Lev0 <- readRDS("../../Shapefiles/R (Spatial Polygons)/gadm36_TZA_Lev0_sp.rds")

TZA_Lev1 <- readRDS("../../Shapefiles/R (Spatial Polygons)/gadm36_TZA_Lev1_sp.rds")

# Convert the SpatialPolygonDataFrame to a normal data frame
TZA_Lev0 %<>% tidy()
TZA_Lev1 %<>% tidy()


# Gather the map region names
Regions <- read_csv("Patches/Region.csv",
                    col_types="cc")


# Joining the names to map
TZA_Lev1 %<>% left_join(Regions)

# Format the names in the data tbl
Age_Gender_Region_Count %<>% mutate(Region = str_to_title(Region)) %>%
  mutate(Region = recode(Region,
                         "Dar Es Salaam"="Dar es Salaam"))

# Mbeya and Songwe need to be combined. Mbeya has a count of 4600 in the original data.
# Songwe has a count of 2340
Age_Gender_Region_Count %<>% mutate(Weight = case_when(
  Region == "Mbeya" ~ 4600,
  Region == "Songwe" ~ 2340,
  TRUE ~ as.numeric(1)
)) %>%
  mutate(Region = recode(Region,
                         "Songwe"="Mbeya")) %>%
  group_by(Region) %>%
  mutate(Weighted_Ratio = sum(Weighed_Ratio * Weight)/sum(Weight)) %>%
  ungroup() %>%
  arrange(Region) %>%
  select(-Weighed_Ratio, -Weight) %>%
  distinct()


# Find missing regions
#View(Regions %>% anti_join(Age_Gender_Region_Count,
#                      by=c("Region_Name"="Region")))

#View(Age_Gender_Region_Count %>% anti_join(Regions,
#                           by=c("Region"="Region_Name")))

# Joining the data to tbl
TZA_Lev1 %<>% left_join(Age_Gender_Region_Count,
                        by=c("Region_Name"="Region"))


# Plotting in ggplot2
ggplot(data = TZA_Lev1, fill=Weighted_Ratio) +
  geom_polygon(aes(x=long, y=lat, group=group),
               color="white")

ggplot(data=TZA_Lev1) +
  aes(x=long, y=lat, group=group, fill=id) +
  geom_polygon(data=TZA_Lev1, fill="blue") +
  geom_polygon(data=TZA_Lev1 %>% filter(id==01), color="blue") +
  scale_fill_discrete()

ggplot(data=TZA_Lev1) +
  aes(x=long, y=lat, group=group, fill=id) +
  #  geom_polygon(data=TZA_Lev1, fill="blue") +
  geom_polygon(data=TZA_Lev1) +
  scale_fill_discrete()

my_map <- ggplot(data=TZA_Lev1) +
  aes(x=long, y=lat, group=group, fill=Weighted_Ratio) +
  geom_polygon(data=TZA_Lev1) +
  scale_fill_continuous() +
  labs(fill="Ratio of female\nto male students\nunder 17") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = ("             United Republic of Tanzania") ,
       subtitle = ("    Secondary School Enrollment Female to Male Ratio  \n                                          2019"),
caption = ("Source: \"https://www.nbs.go.tz\"\n done by: Vivian J. Goshashy"  )) + 
  theme(
    plot.title = element_text(color="steel blue", size=14, face="bold.italic")
  ) 

ggdraw() +
  draw_image("Court of Arm.png", x =0.42, y =0.39, scale = .2) +
  draw_plot(my_map)


ggsave("Graphs/TZA_Lev1.jpg", device="jpg")


