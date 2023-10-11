#Week 4: dplyr package

#Task: Write the function to get a dataset from Base R: HairEyeColor
#and give the dataset a new name of your choice
HairEyeColor
HairEyeClr<-data.frame(HairEyeColor)

#See the top rows of the data
#TASK: Write the function to see the top rows of the data
head(HairEyeClr)


#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr

install.packages("dplyr")
library(dplyr)

#Let's just see the hair and sex columns
#Task: Write the function to 'select' just the hair and sex columns 
#(hint: use the 'select' function)

HairEyeClr %>% select(Hair,Sex)

#Let's name the dataset with just the two columns, Hair and Sex
#TASK: Write the function to save the two columns as a new dataset
#and give it a name
Hair<-(HairEyeClr$Hair)
Sex<-(HairEyeClr$Sex)
HairandSex<-data.frame(Hair,Sex)

#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column)

HairandSex%>%select(Hair)

#Let's rename a column name
#TASK: Write the function that renames 'sex' to 'gender'

rename(HairandSex,gender=Sex)

#Let's make a new dataset with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column

Hairandgender<-data.frame(Hair, gender)

#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'

Allmale<-filter(Hairandgender,gender=="Male")
Allmale

#Let's 'group' our data by gender
#TASK: Write the function to group the data by gender (hint: group_by)

group_by(Hairandgender,gender)


#Let's see how many students were examined in the dataset (total the frequency)
#TASK: replace 'datasetname' with the name of your dataset and get the total
#After you run it, write the total here:592____

total=mutate(HairEyeClr, total=cumsum(Freq))
total

#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'

Allfemale<-Filter(Hairandgender,gender=="female")
Allfemale
#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

union(Allfemale,Allmale)

#Optional Task: add any of the other functions 
#you learned about from the dplyr package

