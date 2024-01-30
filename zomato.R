###############################################
               ### GROUP 7 ###

###############################################

########## A descriptive analysis of ##########
########## Restaurants in Bangalore  ##########

###############################################

library(moments)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(readr)
library(tm)
library(wordcloud)
library(data.table)
library(tidyverse)
library(GoodmanKruskal)

## Load the data set ##
zomato = read.csv("E:/3rd Year/2nd Semester/ST 3082 - Statistical Learning I/Group 07/Project 01/zomato.csv")
View(zomato)

# Use unique() function to get unique observations
unique_values = unique(zomato$cuisines)

# Calculate the number of unique observations
num_unique = length(unique_values)

# Print the result
print(num_unique)

## Expand data set ##
glimpse(zomato)
colnames(zomato)

## Remove unwanted variables ##
zomato = zomato[,-c(1,2,8,11,14,15)]
view(zomato)

## Change type of variables ##
zomato$rate = as.numeric(str_replace(zomato$rate,'5/',''))
zomato$approx_cost.for.two.people. = as.double(zomato$approx_cost.for.two.people.)
zomato$votes = as.double(zomato$votes)

## Identify missing values ##

missing_value = sapply(zomato,function(x)sum(is.na(x)))
missing_value

## histogram for cost variable before removing missing values ##

hist(zomato$approx_cost.for.two.people.)
summary(zomato$approx_cost.for.two.people.)

## impute missing values ##
zomato$rate = ifelse(is.na(zomato$rate),mean(zomato$rate,na.rm= TRUE),zomato$rate)
zomato$approx_cost.for.two.people. = ifelse(is.na(zomato$approx_cost.for.two.people.),
                                            median(zomato$approx_cost.for.two.people.,na.rm= TRUE),
                                            zomato$approx_cost.for.two.people.)



## Check for missing values in each column for following variables
missing_location = is.na(zomato$location)
missing_rest_type = is.na(zomato$rest_type)
missing_cuisines = is.na(zomato$cuisines)

# Replace missing values with mode only if there are any missing values
if (any(missing_location)) {
  zomato[missing_location,]$location = mode(zomato$location, na.rm = TRUE)
}

if (any(missing_rest_type)) {
  zomato[missing_rest_type,]$rest_type = mode(zomato$rest_type, na.rm = TRUE)
}

if (any(missing_cuisines)) {
  zomato[missing_cuisines,]$cuisines = mode(zomato$cuisines, na.rm = TRUE)
}

## histogram for cost variable after removing missing values ##

hist(zomato$approx_cost.for.two.people.)
summary(zomato$approx_cost.for.two.people.)

## record the variables ##

#1. Rest Type

resttype_list = strsplit(zomato$rest_type,", ")
resttype_vector = unlist(resttype_list)

resttype_counts = table(resttype_vector)
resttype_counts

resttype_counts = sort(resttype_counts,decreasing = TRUE)

top_five_resttype = names(head(resttype_counts,5))
top_five_resttype

# Create a list of the five main rest type
main_resttype = c("Quick Bites" , "Casual Dining","Cafe" ,         
                   "Delivery" , "Dessert Parlor")

recode_resttype = function(rest_type){
  rest_type = strsplit(rest_type,", ")[[1]]
  main_resttype_found = rest_type %in% main_resttype
  if (sum(main_resttype_found)>1){
    return("Multiple")
  }
  if (sum(main_resttype_found)==1){
    return(rest_type[main_resttype_found])
  }
  return("Other")
}
zomato$rest_type = sapply(zomato$rest_type,recode_resttype)

#2.Cuisine 

cuisines_list = strsplit(zomato$cuisines,", ")
cuisines_vector = unlist(cuisines_list)

cuisines_vector[cuisines_vector %in% c("Fast Food","Pizza","Cafe","Bakery")] = "American"
cuisines_vector[cuisines_vector %in% c("Biryani","North Indian","South Indian")] = "Indian"

cuisines_counts = table(cuisines_vector)
cuisines_counts

cuisines_counts = sort(cuisines_counts,decreasing=TRUE)

top_ten_cuisines = names(head(cuisines_counts,10))
top_ten_cuisines

main_cuisines = c("Indian" ,"Chinese" ,"Continental","Desserts")
american_cuisines = c("Fast Food","Pizza","Cafe","Bakery")
indian_cuisines = c("North Indian","South Indian","Biryani")

zomato$main_cuisines = "Other"
cuisines_list = strsplit(zomato$cuisines,", ")

for (i in 1:nrow(zomato)){
  #Extract the cuisines for the current observation
  cuisines = cuisines_list[[i]]
  
  #If any of the cuisines match the "American" cuisines,store "American"
  if (any(cuisines %in% american_cuisines)){
    zomato$main_cuisines[i]="Ametican"
  }else if(any(cuisines %in% indian_cuisines)){
    zomato$main_cuisines[i] = "Indian"
  
  }else{
    # Count the number of main cuisines in the list
    main_cuisine_count = sum(cuisines %in% main_cuisines)
    
    #If there is only one main cuisines in the list
    if (main_cuisine_count==1){
      zomato$main_cuisines[i] = cuisines[cuisines %in% main_cuisines]
    }
    #If there are multiple main cuisines, store "multiple"
    else if (main_cuisine_count>1){
      zomato$main_cuisines[i] = "Multiple"
    }
    
  }
}

zomato$cuisines = zomato$main_cuisines

## Correlation between categorical variables ##

colnames(zomato)
var_set = c("rest_type","listed_in.type.","cuisines" ,"location" ,
            "listed_in.city.","online_order","book_table")
df1 = subset(zomato,select = var_set)
gkmatrix = GKtauDataframe(df1)
plot(gkmatrix)

## correlation between continuous variables ##

t = subset(zomato,select = c("rate","approx_cost.for.two.people.","votes"))
cor(t,method = "spearman")


## Split data into training and testing ##
set.seed(123)
split_ratio = 0.8
train_index = sample(1:nrow(zomato),size = round(split_ratio*nrow(zomato)))
zomato_train = zomato[train_index,]
zomato_test = zomato[-train_index,]
summary(zomato_train)

# Use unique() function to get unique observations
unique_values = unique(zomato_train$name)

# Calculate the number of unique observations
num_unique = length(unique_values)

# Print the result
print(num_unique)


### Graphical Summary ###

#1. PIE CHART OF BOOK TABLE

#create a frequency table for the book_table variable
book_table_freq = table(zomato_train$book_table)

#calculate the percentage of each book_table category
book_table_perc = round(book_table_freq/sum(book_table_freq)*100,2)

#Plot the pie chart
ggplot(data.frame(book_table_freq),aes(x= "",y=Freq,fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y",start = 0)+
  scale_fill_manual(values = c("#0000FF","#FF0000"))+
  ggtitle(" Pie Chart of book table")+
  theme_void()+
  geom_text(aes(label = paste(book_table_perc,"%")),
            position = position_stack(vjust = 0.5))

#2. PIE CHART OF ONLINE_ORDER

#create a frequency table for the online_order variable
online_oder_freq = table(zomato_train$online_order)

#calculate the percentage of each online order category
online_oder_perc = round(online_oder_freq/sum(online_oder_freq)*100,2)

#Plot the pie chart
ggplot(data.frame(online_oder_freq),aes(x= "",y=Freq,fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y",start = 0)+
  scale_fill_manual(values = c("#FF0000","#0000FF"))+
  ggtitle("Online order Pie Chart")+
  theme_void()+
  geom_text(aes(label = paste(online_oder_perc,"%")),
            position = position_stack(vjust = 0.5))


#3.1 BAR CHART OF LISTED_IN_TYPE (EATING SYSTEM)

Eatting_freq = as.data.frame(table(zomato_train$listed_in.type.))

ggplot(Eatting_freq,aes(x = Var1,y = Freq))+
  geom_bar(stat ="identity",fill="#FF0000")+
  xlab("Eatting system")+
  ylab("Frequency")+
  theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust=1))

# Existing code for creating the bar plot
Eatting_freq <- as.data.frame(table(zomato_train$listed_in.type.))

# Calculate percentages
Eatting_freq$Percentage <- (Eatting_freq$Freq / sum(Eatting_freq$Freq)) * 100

# Plotting the bar plot
ggplot(Eatting_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#FF0000") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 3) +  # Add percentage labels
  xlab("Eating system") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#3.2 PIE CHART OF LISTED_IN_TYPE (EATING SYSTEM)

#create a frequency table for the listed_in.type. variable
listed_in.type._freq = table(zomato_train$listed_in.type.)

#calculate the percentage of each listed_in.type. category
listed_in.type._perc = round(listed_in.type._freq/sum(listed_in.type._freq)*100,2)

#Plot the pie chart
ggplot(data.frame(listed_in.type._freq),aes(x= "",y=Freq,fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y",start = 0)+
  scale_fill_manual(values = c("#008000","#FFFF00","#4040C0",
                               "#999999", "#E69F00","#40FFFF","#FF00C0"))+
  ggtitle(" Pie Chart of Eatting System")+
  theme_void()+
  geom_text(aes(label = paste(listed_in.type._perc,"%")),
            position = position_stack(vjust = 0.5))

#4. PIE CHART OF RESTAURANT TYPE

#create a frequency table for the rest_type variable
rest_type_freq = table(zomato_train$rest_type)

#calculate the percentage of each rest_type. category
rest_type_perc = round(rest_type_freq/sum(rest_type_freq)*100,2)

#Plot the pie chart
ggplot(data.frame(rest_type_freq),aes(x= "",y=Freq,fill = Var1))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y",start = 0)+
  scale_fill_manual(values = c("#008000","#FFFF00","#4040C0",
                               "#999999", "#E69F00","#40FFFF","#FF00C0"))+
  ggtitle(" Pie Chart of restaurant type")+
  theme_void()+
  geom_text(aes(label = paste(rest_type_perc,"%")),
            position = position_stack(vjust = 0.5))

#5. RATE VS COST SCATTER PLOT
ggplot(zomato_train,aes(y=approx_cost.for.two.people.,x = rate))+
  geom_point()+
  geom_smooth(method ="lm")+
  labs(title = "rate vs cost of two people",y ="cost",x ="rate")+
  theme_classic()

#6. VOTES VS COST SCATTER PLOT
ggplot(zomato_train,aes(y=approx_cost.for.two.people.,x = votes))+
  geom_point()+
  labs(title = "votese vs cost of two people",y ="cost",x ="votes")+
  theme_classic()

#7.AVERAGE COST VS CUISINES BAR PLOT
ave_cost = aggregate(approx_cost.for.two.people.~cuisines,data = zomato_train,mean)

ggplot(ave_cost,aes(x = cuisines,y = approx_cost.for.two.people.))+
  geom_bar(stat ="identity",fill="#00008B")+
  xlab("Cuisines")+
  ylab("Average cost")+
  ggtitle("Average Cost by Cuisines")+
  theme_classic()




# Existing code for creating the bar plot
ave_cost <- aggregate(approx_cost.for.two.people. ~ cuisines, data = zomato_train, mean)

# Calculate percentages
ave_cost$Percentage <- (ave_cost$approx_cost.for.two.people. / sum(ave_cost$approx_cost.for.two.people.)) * 100

# Plotting the bar plot with percentages
ggplot(ave_cost, aes(x = cuisines, y = Percentage)) +
  geom_bar(stat = "identity", fill = "#00008B") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 3) +  # Add percentage labels
  xlab("Cuisines") +
  ylab("Percentage of Total Cost") +
  ggtitle("Percentage of Total Cost by Cuisines") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#8.CUISINES BAR PLOT

#calculate the frequency of each category
cuisines_freq = as.data.frame(table(zomato_train$cuisines))

ggplot(cuisines_freq,aes(x = Var1,y = Freq))+
  geom_bar(stat ="identity",fill="#FF0000")+
  xlab("Cuisines")+
  ylab("Frequency")+
 theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust=1))



# Existing code for creating the bar plot
cuisines_freq <- as.data.frame(table(zomato_train$cuisines))

# Calculate percentages
cuisines_freq$Percentage <- (cuisines_freq$Freq / sum(cuisines_freq$Freq)) * 100

# Plotting the bar plot
ggplot(cuisines_freq, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#FF0000") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 3) +  # Add percentage labels
  xlab("Cuisines") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#9.AVE_COST VS RESTAURANT TYPE BAR PLOT

#calculate the average cost for each category of type
ave_cost_by_sys = aggregate(approx_cost.for.two.people.~rest_type,data = zomato_train,mean)

ggplot(ave_cost_by_sys,aes(x=rest_type,y=approx_cost.for.two.people.))+
  geom_bar(stat ="identity",fill="#00008B")+
  xlab("Restaurant type")+
  ylab("Average cost")+
  ggtitle("Average Cost by Restaurant type")+
  theme_classic()




# Existing code for creating the bar plot
ave_cost_by_sys <- aggregate(approx_cost.for.two.people. ~ rest_type, data = zomato_train, mean)

# Calculate percentages
ave_cost_by_sys$Percentage <- (ave_cost_by_sys$approx_cost.for.two.people. / sum(ave_cost_by_sys$approx_cost.for.two.people.)) * 100

# Plotting the bar plot with percentages
ggplot(ave_cost_by_sys, aes(x = rest_type, y = Percentage)) +
  geom_bar(stat = "identity", fill = "#00008B") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 3) +  # Add percentage labels
  xlab("Restaurant type") +
  ylab("Percentage of Total Cost") +
  ggtitle("Percentage of Total Cost by Restaurant type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#10. AVERAGE COST OF CITY BAR PLOT

#calculate average cost for each city
city_average_cost = aggregate(zomato_train$approx_cost.for.two.people.,by=list(zomato_train$listed_in.city.),mean)

ggplot(city_average_cost,aes(x = Group.1,y = x))+
  geom_bar(stat ="identity",fill="#00008B")+
  xlab("City")+
  ylab("Average Cost")+
  theme(axis.text.x = element_text(angle=90,vjust = 0.5,hjust=1))

#11. BOXPLOT OF COST VS BOOK TABLE

ggplot(zomato_train,aes(x = book_table,y = approx_cost.for.two.people.)) +
  geom_boxplot() +
  ggtitle("cost for two people by book table") +
  xlab("book table") +
  ylab("cost")

#12. BOXPLOT OF COST VS ONLINE ORDER

ggplot(zomato_train,aes(x = online_order,y = approx_cost.for.two.people.)) +
  geom_boxplot() +
  ggtitle("cost for two people by online order") +
  xlab("online order") +
  ylab("cost")

#13.AVERAGE COST VS CITY BAR PLOT

ave_cost = aggregate(approx_cost.for.two.people. ~ listed_in.city., data = zomato_train, mean)

# Reorder the levels based on average cost in descending order
ave_cost$listed_in.city.= reorder(ave_cost$listed_in.city., -ave_cost$approx_cost.for.two.people.)

# Plotting
ggplot(ave_cost, aes(x = listed_in.city., y = approx_cost.for.two.people.)) +
  geom_bar(stat = "identity", fill = "#00008B") +
  xlab("City") +
  ylab("Average cost") +
  ggtitle("Average Cost by City (Descending Order)") +
  theme_classic()

#14.AVERAGE COST VS Eatting system
ave_cost = aggregate(approx_cost.for.two.people.~listed_in.type.,data = zomato_train,mean)

ggplot(ave_cost,aes(x = listed_in.type.,y = approx_cost.for.two.people.))+
  geom_bar(stat ="identity",fill="#00008B")+
  xlab("Eatting syestem(listed_in.type.)")+
  ylab("Average cost")+
  ggtitle("Average Cost by Eating system")+
  theme_classic()


# Existing code for creating the bar plot
ave_cost <- aggregate(approx_cost.for.two.people. ~ listed_in.type., data = zomato_train, mean)

# Calculate percentages
ave_cost$Percentage <- (ave_cost$approx_cost.for.two.people. / sum(ave_cost$approx_cost.for.two.people.)) * 100

# Plotting the bar plot with percentages
ggplot(ave_cost, aes(x = listed_in.type., y = Percentage)) +
  geom_bar(stat = "identity", fill = "#00008B") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 3) +  # Add percentage labels
  xlab("Eating system (listed_in.type.)") +
  ylab("Percentage of Total Cost") +
  ggtitle("Percentage of Total Cost by Eating system") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Use the Kruskal-Wallis test
kruskal.test(online_order ~ approx_cost.for.two.people., data = zomato_train)
kruskal.test(book_table ~ approx_cost.for.two.people., data = zomato_train)
kruskal.test(rest_type ~ approx_cost.for.two.people., data = zomato_train)
kruskal.test(cuisines ~ approx_cost.for.two.people., data = zomato_train)
kruskal.test(listed_in.type. ~ approx_cost.for.two.people., data = zomato_train)
kruskal.test(listed_in.city. ~ approx_cost.for.two.people., data = zomato_train)
