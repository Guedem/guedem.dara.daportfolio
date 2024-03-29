---
#COURSEWORK INFORMATION ON MAIN PAGE
# An instance of the Featurette widget.
# Documentation: https://wowchemy.com/docs/page-builder/
widget: #coursework

#rl: '#coursework'

# This file represents a page section.
headless: true

# Order that this section appears on the page.
weight: 30

title: Coursework
subtitle:

featured: TRUE
#image: content/coursework/featured/

# image:
#   caption: featured.jpg
#   focal_point: Smart

#To add a background picture
# design:
#   background:
#     #name of image in `assets/media/`
#     image: featured.jpg

header:
  caption: ""
  image: "featured.jpg"
  
  #   icon: udemy
  #   icon_pack: fab
  #   name: Other   
  # 
  # - description: 10%
  #   icon: camera-retro
  #   icon_pack: fas
  #   name: Photography
# Uncomment to use emoji icons.
#- icon: ":smile:"
#  icon_pack: "emoji"
#  name: "Emojiness"
#  description: "100%"

# Uncomment to use custom SVG icons.
# Place your custom SVG icon in `assets/media/icons/`.
# Reference the SVG icon name (without `.svg` extension) in the `icon` field.
# For example, reference `assets/media/icons/xyz.svg` as `icon: 'xyz'`
#- icon: "your-custom-icon-name"
#  icon_pack: "custom"
#  name: "Surfing"
#  description: "90%"
---

## **Dara Guedem's Coursework Data**

To give a summary of some of the graduate courses that I have taken over the years, I created a data set of 29 courses and five variables where: 

  - year = academic year
  - season = Spring or Fall semester, 
  - FieldOfStudy = field of study, 
  - course = course department number and title, 
  - credits = the number of credits per course (1-10), and 
  - software = software used in a course. 
  
The graph below shows some of the graduate level courses that I completed from the University of Montana; MA in Economics (2012 - 2015) and MSBA (2017 - 2018). I've also resourced myself through different books and online tutorials (from YouTube) post graduation. Lately, I have completed some online courses via Udemy.

Note: ECNS 599 is a 10 credits course split into 2 semesters (Fall 2014 and Spring 2015). 

## **Frequency of Software Usage**
Table of frequency of software used in the different courses that I completed over the years. 

```{r, echo=FALSE}
library(dplyr)
freq_by_course <- coursework %>%
                    group_by(software) %>%
                    summarise(n = n(), .groups = 'drop') %>%
                    mutate(Frequency = paste0(round(100 * n/sum(n), 0), '%'))
freq_by_course
```

```
## # A tibble: 11 × 3
##    software         n Frequency
##    <fct>        <int> <chr>    
##  1 "ArcGIS"         2 7%       
##  2 "docs"           3 10%      
##  3 "docs & PPT"     2 7%       
##  4 "Excel"          1 3%       
##  5 "PostgreSQL"     1 3%       
##  6 "Power BI"       1 3%       
##  7 "Python "        3 10%      
##  8 "R"              8 28%      
##  9 "SPSS"           1 3%       
## 10 "Stata"          6 21%      
## 11 "Weka"           1 3%
```

Note: I've used STATA the longest because I started using it as an undergraduate student at the UM. 

