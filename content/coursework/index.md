---
title: "Coursework"
author: "Dara, Guedem"
date: 2022-11-26T21:13:14-05:00
categories: ["R"]
tags: ["R Markdown", "treemap", "summary"]
widget: coursework
weight: 15
---



## `Dara Guedem's Coursework Data`

To give a summary of some of the graduate courses that I have taken over the years, I created a data set of 29 courses and five variables where: 

  - year = academic year
  - season = Spring or Fall semester, 
  - FieldOfStudy = field of study, 
  - course = course department number and title, 
  - credits = the number of credits per course (1-10), and 
  - software = software used in a course. 
  
The graph below shows some of the graduate level courses that I completed from the University of Montana; MA in Economics (2012 - 2015) and MSBA (2017 - 2018). I've also resourced myself through different books and online tutorials (from YouTube) post graduation. Lately, I have completed some online courses via Udemy.




```r
#load my coursework data
coursework <- read.csv("coursework.csv")
coursework$course <- factor(coursework$course)              #course dpt & title
coursework$FieldOfStudy <- factor(coursework$FieldOfStudy)  #field of study
coursework$software <- factor(coursework$software)          #software
coursework
```

```
##    year                             season       FieldOfStudy
## 1  2012               Autumn Semester 2012           Elective
## 2  2012               Autumn Semester 2012           Elective
## 3  2012               Autumn Semester 2012           Elective
## 4  2013               Spring Semester 2013          Economics
## 5  2013               Spring Semester 2013          Economics
## 6  2013               Autumn Semester 2013          Economics
## 7  2013               Autumn Semester 2013           Elective
## 8  2013               Autumn Semester 2013           Elective
## 9  2013               Autumn Semester 2013           Elective
## 10 2014               Spring Semester 2014          Economics
## 11 2014               Autumn Semester 2014           Elective
## 12 2014               Spring Semester 2014           Elective
## 13 2014               Spring Semester 2015           Elective
## 14 2015 Autumn 2014 & Spring Semester 2015          Economics
## 15 2015               Spring Semester 2015          Economics
## 16 2015               Spring Semester 2015           Elective
## 17 2017               Autumn Semester 2017 Business Analytics
## 18 2017               Autumn Semester 2017           Elective
## 19 2017               Autumn Semester 2017 Business Analytics
## 20 2017               Autumn Semester 2017 Business Analytics
## 21 2017               Autumn Semester 2017           Elective
## 22 2018               Autumn Semester 2018 Business Analytics
## 23 2018               Autumn Semester 2018 Business Analytics
## 24 2018               Autumn Semester 2018           Elective
## 25 2018               Autumn Semester 2018           Elective
## 26 2022                             Spring              Udemy
## 27 2022                             Spring              Udemy
## 28 2022                             Spring              Udemy
## 29 2022                               Fall              Udemy
##                                                   course credits   software
## 1                GPHY 468: Community & Regional Analysis       3     ArcGIS
## 2              GPHY 469: Planning & Analysis Laboratory        1     ArcGIS
## 3                        STAT 451: Statistical Methods I       3          R
## 4                   ECNS 569: Empirical Research Methods       1      Stata
## 5                              ECNS 513: Macro Economics       3      Stata
## 6                            ECNS 596: Independent Study       1      Stata
## 7                        STAT 542: Applied Linear Models       3          R
## 8                     STAT 457: Computer Data Analysis I       1          R
## 9                ECNS 433: Economics of the Environment        3       docs
## 10     ECNS 450: Advanced Topics in Economic Development       3       docs
## 11                            STAT 549: Applied Sampling       3       docs
## 12                      STAT 452: Statistical Methods II       3          R
## 13                   STAT 458: Computer Data Analysis II       1          R
## 14                                      ECNS 599: Thesis      10      Stata
## 15                       ECNS 560: Advanced Econometrics       4      Stata
## 16                          PSCI 524: Management Skills        3 docs & PPT
## 17                      BMKT 670: Applied Data Analytics       3    Python 
## 18                       MATH 467 : Pract. Big Data Ana.       3          R
## 19            BMIS 694: Text Mining of Unstructured Data       1    Python 
## 20                       BMIS 601: Business Intelligence       3          R
## 21                     JRNL 414: Investigative Reporting       3      Excel
## 22                           MBA 694: Project Management       1 docs & PPT
## 23                       BMIS 650: Quantitative Analysis       2       SPSS
## 24                            CSCI 547: Machine Learning       3    Python 
## 25                             CSCI 595: Mining Big Data       3       Weka
## 26                          The Essential Guide to Stata       3      Stata
## 27 Microsoft Power BI Desktop for Business Intelligence        3   Power BI
## 28                            The Complete SQL Bootcamp        3 PostgreSQL
## 29                                         R-Programming       3          R
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(treemap)


treemap(coursework,
        index = c("FieldOfStudy", "course", "software"),    #Field of study, course, and software.
        vSize = "credits",                                  #course credits
        vColor =  "FieldOfStudy",
        title = "Dara Guedem's Relevant Coursework by Field of Study and Software Usage",
        align.labels = list(
          c("left", "bottom"), 
          c("center", "center")
        )
      )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/load dataset-1.png" width="672" />

Note: ECNS 599 is a 10 credits course split into 2 semesters (Fall 2014 and Spring 2015). 

## `Frequency of Software Usage`
Table of frequency of software used in the different courses that I completed over the years. 


```r
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

<img src="{{< blogdown/postref >}}index_files/figure-html/load dataset-1.png" width="672" />

