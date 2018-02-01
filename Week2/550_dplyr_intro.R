


names  <- c("P1","P2","P3","P4","P5")
temp   <- c(98.2,101.3,97.2,100.2,98.5)
pulse  <- c(66,72,83,85,90)
gender <- c("M","F","M","M","F")

my_df <- data.frame(names,temp,pulse,gender) # Much more flexible


plot(my_df$pulse ~ my_df$temp,main="Pulse Rate",xlab="Patient",ylab="BPM")
mean(my_df[,2:3])



data(mtcars)

str(mtcars)


nrow(mtcars)	# How many rows does it have ?

ncol(mtcars)	# How many columns are there ?

mtcars[,-11]


mtcars  	# Notice that carb is included

%

mtcars[,-3:-5]  # Print all columns except for columns 3 through 5

mtcars[,c(-3,-5)] # Print all columns except for colums 3 AND 5


mtcars[mtcars$mpg >= 30.0,]


mtcars[mtcars$mpg >= 30.0,2:6]



mtcars[mtcars$mpg >= 30.0 & mtcars$cyl < 6,]


mtcars[mtcars$am==0,]

nrow(mtcars[mtcars$am == 0,])


nrow(mtcars[mtcars$am == 1,])


mtcars[mtcars$mpg > mean(mtcars$mpg),]


quantile(mtcars$mpg)


mtcars[mtcars$mpg > quantile(mtcars$mpg)[4],]



str(mtcars)

unique(mtcars$am)   # Tells us what the unique values are

sapply(mtcars, function(x) length(unique(x)))

summary(mtcars$am)


data(mtcars)   # Reload a "pure" copy of mtcars

mpgrate <- cut(mtcars$mpg, 
               breaks = quantile(mtcars$mpg),
               labels=c("horrible","Bad","Good","Great"),include.lowest=T)

mtcars <- cbind(mtcars,mpgrate)

-OR-
  
  mtcars$mpgrate <- mpgrate   # The column just magically appears !

head(mtcars)

bwplot(~mpg|mpgrate,data=mtcars,layout=c(1,4))



transform(mtcars,wt = (wt*1000), qsec = round(qsec), 
          am = factor(am,labels=c("A","M")))




url <- "https://raw.githubusercontent.com/pittardsp/bios545r_spring_2018/master/SUPPORT/hsb2.csv"


data1 <- read.table(url,header=T,sep=",")

head(data1)




install.packages("dplyr")
install.packages("readr")    # Get's the equivalent to data.table's fread package

# Loads the package

library(dplyr)         

# Launches a browser to explore

browseVignettes(package = "dplyr")  


head(mtcars,12)

df <- data.frame(id = 1:5,
gender = c("MALE","MALE","FEMALE","MALE","FEMALE"),
age = c(70,76,60,64,68))

filter(df,gender == "FEMALE")

filter(df, id %in% c(1,3,5))

mutate(df,meanage = mean(age))

mutate(df,old_young=ifelse(df$age>=mean(df$age),"Y","N"))


tmp <- mutate(df, color = ifelse(age > mean(age),"red","blue"))
plot(tmp$age,col=tmp$color,type="p",pch=19,main="Ages",ylab="Age")
grid()
abline(h=mean(tmp$age),lty=2)


arrange(df, desc(age))

arrange(df, gender,desc(age))

select(df,gender,id,age)  # Reorder the columns

select(df,-age)   # Select all but the age column


select(df,id:age)  # Can use : to select a range

library(ggplot2)
data(diamonds)
names(diamonds)

head(select(diamonds,starts_with("c")))

head(select(diamonds,ends_with("t")))

testdf <- expand.grid(m_1=seq(60,70,10),age=c(25,32),m_2=seq(50,60,10))

head(testdf, 4)


head( select(testdf,matches("_")) ,2)


head( select(testdf,contains("_"), 2)


head( select(testdf,num_range("m_",1:2)), 2)



group_by(df,gender)   # Hmm. Did this really do anything ? 


df


( gdf <- group_by(df,gender)    # Hmm. Did this really do anything ? 


summarize(group_by(df,gender),total=n())

summarize(group_by(df,gender),av_age=mean(age))


summarize(group_by(df,gender),av_age=mean(age),total=n())



df

tapply(df$age,df$gender,mean)    # tapply function
FEMALE   MALE 
64     70 

aggregate(age~gender,data=df,mean) # aggregate works also


lapply(split(df,df$gender),function(x) mean(x$age)) # complicated




head(select(mtcars, mpg, am))

mtcars %>% select(mpg, am) %>% head


df %>% group_by(gender) %>% summarize(avg=mean(age)) 


df %>% group_by(gender) %>% summarize(avg=mean(age),total=n()) 

df %>% filter(gender == "MALE") %>% summarize(med_age=median(age))

df %>% filter(gender == "MALE") %>% summarize(med_age=median(age))


mtcars %>% filter(wt > 3.3)  %>% 
mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>% 
group_by(ab_be) %>% summarize(mean_mpg=mean(mpg))


mtcars %>% filter(wt > 3.3)


mtcars %>% filter(wt > 3.3) %>% 

mtcars %>% filter(wt > 3.3)  %>% 
  mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>% 
  group_by(ab_be) %>% summarize(mean_mpg=mean(mpg))


mtcars %>% filter(wt > 3.3)  %>% 
  mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>% 
  group_by(ab_be) %>% summarize(mean_mpg=mean(mpg)) %>%
  ggplot(aes(x=ab_be,y=mean_mpg)) + geom_bar(stat="identity") +
  ggtitle("Mean MPG") + labs(x = "ab_be", y = "Mean MPG")

\begin{verbatim}
library(readr)

dt <- read_delim("combined_wiki.zip",delim=" ")

nrow(dt)

head(dt,5)


head(dt,5)



dt %>% mutate(MB=bytes/1000000) %>% 
group_by(proj)%>% 
summarize(avg=round(mean(MB),2)) %>% 
arrange(desc(avg))


system.time( dt %>% mutate(MB=bytes/1000000) %>%
group_by(proj)%>%
summarize(avg=round(mean(MB),2)) %>%
arrange(desc(avg)) )


myaggre <- function(df) {
df$bytes <- round(df$bytes/1000000,2)
hold <- aggregate(bytes~proj,df,mean)
hold <- hold[order(-hold$bytes),]
return(hold)
}

system.time( myaggre(df))

mtcars %>% sample_n(2) # Sample 2 records from a data frame


mtcars %>% group_by(cyl) %>% do(sample_n(.,2)) 

by_cyl <- group_by(mtcars, cyl) 
models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))

summarise(models, rsq = summary(mod)$r.squared)

mtcars %>% group_by(cyl) %>% do(mod = lm(mpg ~ disp, data = .)) 
%>% summarize(rsq = summary(mod)$r.squared)

idatime <- data.frame(id=rep(1:3,each=2),time=rep(0:1,each=3))


idawt <- data.frame(id=c(1,2,4),wt=c(110,130,115))


inner_join(idatime,idawt)



url <- "https://raw.githubusercontent.com/pittardsp/bios545r_spring_2018/master/SUPPORT/msleep_ggplot2.csv"

library(readr)

download.file(url,"msleep_ggplot2.csv")
msleep <- read_csv("msleep_ggplot2.csv")

names(msleep)


