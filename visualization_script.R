rm(list = ls())
getwd()
setwd("/Users/rociozhong/Library/Mobile Documents/com~apple~CloudDocs/STAT_578/ml-100k")
############### data input #####################
# load full data
fulldta = read.table("u.data", dec = ",")
colnames(fulldta) = c("uid", "movieid", "rating", "time")


# load item information data
item = read.csv(file = "u.item", sep = "|", header = F)
colnames(item) = c("movieid", "title", "rdate", "vrdate", "url", "unknown", "action", "adventure", 
                   "animation", "child", "comedy", "crime", "doc", "drama", "fantasy", "film-noir", 
                   "horror", "musical", "mystery", "romance", "sci", "thriller", "war", "western")
dim(item)  # 1682, 24

# load user information
user = read.csv("u.user", sep = "|", header = F)
colnames(user) = c("uid", "age", "gender", "occupation", "zip" )
head(user)
dim(user) # 943, 5

############################## data clean ###################

# for item(movie) data
which(item[, 4] != "NA") # can delete this video release date col
which(item[, 6] == 1)  # 267 1373

unknownid = item$movieid[which(item$title == 'unknown')] # 267
item = item[- unknownid, ]
item = item[, -4]

which(fulldta$movieid == unknownid) # 2173  3782  7246 12476 14757 15293 49296 93524 99724
fulldta = fulldta[ - which(fulldta$movieid == unknownid), ]
##################
cols = c("title", "rdate", "url", "unknown")
item = item[, -which(names(item) %in% cols)]

item_user = merge(fulldta, item, by = "movieid")
new_itemuser = merge(item_user, user, by = "uid")

which(colnames(new_itemuser) == "action")
which(colnames(new_itemuser) == "western")

new_itemuser[new_itemuser$action == 1, ]
moviegenre = colnames(new_itemuser)[5:22]
# data = list()
# for (i in length(moviegenre)){
#   data[[i]] = as.matrix(new_itemuser %>% filter(moviegenre[i] == 1) %>% group_by(gender) %>% summarize(average = mean(rating)))
# }

temp = new_itemuser %>% filter(action == 1) %>% group_by(gender) %>% count(rating)
#summarize(average = mean(rating), total.count=n())
ggplot(temp, aes(x = rating, y = n, fill = gender)) + geom_bar(stat="identity", position="dodge")
dt1 = new_itemuser %>% filter(action == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) 

dt2 = new_itemuser %>% filter(adventure == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt3 = new_itemuser %>% filter(animation == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt4 = new_itemuser %>% filter(child == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt5 = new_itemuser %>% filter(comedy == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt6 = new_itemuser %>% filter(crime == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt7 = new_itemuser %>% filter(doc == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male ave rating higher
dt8 = new_itemuser %>% filter(drama == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt9 = new_itemuser %>% filter(fantasy == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt10 = new_itemuser %>% filter(`film-noir` == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt11 = new_itemuser %>% filter(horror == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt12 = new_itemuser %>% filter(musical == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt13 = new_itemuser %>% filter(mystery == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))# male higher
dt14 = new_itemuser %>% filter(romance == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating))
dt15 = new_itemuser %>% filter(sci == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt16 = new_itemuser %>% filter(thriller == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt17 = new_itemuser %>% filter(war == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher
dt18 = new_itemuser %>% filter(western == 1) %>% group_by(gender) %>% summarize(average = mean(rating), var = var(rating)) # male higher


genre_gender = rbind(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9, dt10, dt11, dt12, dt13, dt14, dt15, dt16, dt17, dt18)
genre_gender$genre = rep(moviegenre, each = 2)

p_agemean = ggplot(genre_gender, aes(x = gender, y = average)) + geom_line(aes(group = genre, colour = genre), size = 1)
p_agevar = ggplot(genre_gender, aes(x = gender, y = var)) + geom_line(aes(group = genre, colour = genre), size = 1) +
  xlab("Gender") + ylab("Variance of ratings") 
library(gridExtra)
grid.arrange(p_agemean, p_agevar, nrow = 1)

###### rating variance among ages #####
dis_age = unique(new_itemuser$age)
age_movie = new_itemuser %>% group_by(movieid, age) %>% summarize(ave = mean(rating))

ggplot(age_movie[age_movie$movieid == 1,], aes(x = age, y = ave)) + geom_point() + geom_smooth(se=FALSE)

ggplot(age_movie[1:5000, ], aes(x = age, y = ave)) + geom_smooth(aes(group = movieid, colour = movieid), size = 0.2, se = FALSE) +
  xlab("Age") + ylab("Mean of ratings") 

# number_movie = unique(age_movie$movieid)
# random_movieid = sample(1: max(number_movie), 150, replace = T)
# 
# random_movieid
# tempdta = age_movie[age_movie$movieid %in% random_movieid, ]
# ggplot(tempdta, aes(x = age, y = ave)) + 
#   geom_smooth(aes(group = movieid, colour = movieid), size = 0.2, se = FALSE) +
#   xlab("Age") + ylab("Mean of ratings") 

# most important

gender_movie = new_itemuser %>% group_by(movieid, gender) %>% summarize(ave = mean(rating), std = sd(rating))
gender_movie = gender_movie[complete.cases(gender_movie), ]
#gender_movie$var[is.na(gender_movie$var)] = 0



p_genderstd = ggplot(gender_movie[1:500, ], aes(x = gender, y = std)) + geom_line(aes(group = movieid, colour = movieid), size = 0.2) + 
  xlab("Gender") + ylab("Standard deviation of ratings")

p_gendermean = ggplot(gender_movie[1:500, ], aes(x = gender, y = ave)) + geom_line(aes(group = movieid, colour = movieid), size = 0.2) + 
  xlab("Gender") + ylab("Mean of ratings")

library(gridExtra)
grid.arrange(p_genderstd, p_gendermean, nrow = 1)

########################



###########################
ggplot(new_itemuser[new_itemuser$movieid == 1, ], aes(x = age, y = rating)) + geom_line()

###################### missing data pattern #########################
newdta = fulldta[order(fulldta$uid), ]
plotdta = as.matrix(table(newdta$uid, newdta$rating))
user_rat = as.data.frame.matrix(table(newdta$uid, newdta$rating))
user_rat = cbind(sort(unique(newdta$uid)), user_rat)
colnames(user_rat) = c("uid", "1", "2", "3", "4", "5")

# calculate each user's total number of ratings
test = apply(plotdta, 1, sum) # numbers of ratings for users

# creat another data set including user id and total number of ratings
user_ratings = data.frame(sort(unique(fulldta$uid)), test)
colnames(user_ratings) = c("uid", "total_ratings")

# plot user id versus the total number of ratings they have
library(ggplot2)
ggplot(user_ratings, aes(x = uid, y = total_ratings)) + geom_point()



# combine user dataset and user_tratings dataset
new_user = merge(user, user_ratings, by = "uid")

fit1 = lm(total_ratings ~ factor(gender)  + age, data = new_user)
summary(fit1)
library(stargazer)
stargazer(fit1)


# since gender and age are siginificant, let's visualize!
ggplot(new_user, aes(x = uid, y = total_ratings, color = gender)) + 
  xlab("user's id") + ylab("total number of ratings") + geom_point()

# variance of total ratings among female and male
sd(new_user[new_user$gender == "F", "total_ratings"])
sd(new_user[new_user$gender == "M", "total_ratings"])

ages = unique(new_user$age)
var_vec = rep(0, length(ages))
# variance of total rating among different age

for (i in 1:length(ages)){
     var_vec[i] = sd(new_user[new_user$age == ages[i], "total_ratings"])
}

# replace NA with 0
var_vec[is.na(var_vec)] = 0

plot(x = ages, y = var_vec)

sum(new_user$gender == "M")/dim(new_user)[1] # male percentage in data is about 71%

ggplot(new_user, aes(x = total_ratings, fill = gender)) + 
  geom_histogram(breaks = seq(1, 800, by = 30), alpha = 0.5, position = "identity") + 
  scale_y_continuous(limits = c(0, 175)) +
  ylab("Male count") + xlab("total number of ratings") 

ggplot(new_user[(new_user$gender == "M"), ], aes(x = total_ratings)) + 
  geom_histogram(breaks = seq(1, 800, by = 30), position = "identity", alpha = 0.5) + scale_y_continuous(limits = c(0, 175)) +
  ylab("Male count") + xlab("total number of ratings") 
ggplot(new_user[(new_user$gender == "F"), ], aes(x = total_ratings)) + 
  geom_histogram(breaks = seq(1, 800, by = 30), position = "dodge") + scale_y_continuous(limits = c(0, 175)) +
  ylab("Female count") + xlab("total number of ratings") 

# library(gridExtra)
# grid.arrange(p_m, p_f, nrow = 1)

# age distribution in the dataset
p_age_hist = ggplot(new_user, aes(x = age)) + geom_histogram(breaks = seq(1, 90, by = 2))
p_age_point = ggplot(new_user, aes(x = age, y = total_ratings)) + geom_point() + xlab("user's age") + ylab("total number of ratings")

library(gridExtra)
grid.arrange(p_age_hist, p_age_point, nrow = 1)

# long occupation words
library(stringr)

ggplot(new_user, aes(x = occupation, y = total_ratings, fill = gender)) + 
  geom_bar(stat="identity", position="dodge") + theme(axis.text.x = element_text(angle = 90))

max(new_user$total_ratings)
min(new_user$total_ratings)
# library(zipcode)
# data(zipcode)
# zipdata = data.frame(zipcode$zip, zipcode$state)
# up_user = merge(zipdata, new_user, by = "zip")

############### break down ratings by gender


user_break_up = merge(fulldta, user, by = "uid")

ggplot(user_break_up, aes(x = rating, fill = gender)) + geom_histogram(binwidth = 0.5) +
  xlab("rating scale") + ylab("number of users")

num = rep(0, 5)
for (i in 1:5){
  num[i] = dim(user_break_up[(user_break_up$gender == "M" & user_break_up$rating == i), ])[1] / 
  dim(user_break_up[(user_break_up$rating == i), ])[1]}

