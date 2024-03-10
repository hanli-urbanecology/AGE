install.packages('ggpubr')
install.packages('broom')
install.packages('dplyr')
install.packages('corrr')

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

library('tidyr')
library('ggplot2')
library('vegan')
library('multcomp')
library('suncalc')
library('plyr')
library('AICcmodavg')
library('wesanderson')
library('ggpubr')
library('dplyr')
library('broom')
library('vegan')
library('cowplot')
library('corrr')

################### test of weather ##########################
weather <- read.csv("weather.csv")

weather$date <- as.Date(weather$date)
weather$weekday <- weekdays(weather$date)
weather$weekday <- factor(weather$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

weather$year <- as.factor(format(weather$date, "%Y"))
weather$month <- as.factor(format(weather$date, "%m"))
weather$day <- as.factor(format(weather$date, "%d"))

fit<- glm(temp~year, data=weather)
summary(fit)

################### clean up UNCG data #######################
rawdf <- read.csv("UNCG.csv")
df <- rawdf[, c(1:3, 5, 6, 8, 10, 12, 15, 16, 18)]

##df$date <-paste(df$year, df$month, df$day, sep="-")
               
df$date <- as.Date(with(df, paste(df$year, df$month, df$day, sep="-")), "%Y-%m-%d")

bat_total_long <- subset(df[, c(1:3, 12)], df$PULSES>2)
bat_total <- aggregate(bat_total_long, 
                       by =  list(bat_total_long$date, bat_total_long$site, bat_total_long$habitat, bat_total_long$treatment), 
                       FUN = length)
names(bat_total)[1]<- "date"
names(bat_total)[2]<- "site"
names(bat_total)[3]<- "habitat"
names(bat_total)[4]<- "treatment"
names(bat_total)[5]<- "total"

bat_total <- bat_total[, c(1:5)]


bat_species_long <- subset(df[, c(1:3, 9, 12)], df$MATCH.RATIO >= 0.5)
bat_species <- aggregate(bat_species_long, by =  list(bat_species_long$date, 
                                                      bat_species_long$site, 
                                                      bat_species_long$habitat, 
                                                      bat_species_long$treatment,
                                                      bat_species_long$species), FUN = length)
names(bat_species)[1] <- "date"
names(bat_species)[2] <- "site"
names(bat_species)[3] <- "habitat"
names(bat_species)[4] <- "treatment"
names(bat_species)[5] <- "species"
names(bat_species)[6] <- "count"
bat_species <- bat_species[, c(1:6)]

bat_species_final <- spread(bat_species, species, count)
bat_species_final[is.na(bat_species_final)] <- 0

###### building date frame #####
site_range <- unique(df$site)
date_range <- rep(seq.Date(from = min(df$date), to = max(df$date), by = "days" ))
final <- expand.grid(date_range, site_range)
names(final)[1] <- "date"
names(final)[2] <- "site"

#### merging all data####
final1 <- merge(final, bat_total, by=c("date", "site"), all = TRUE)
final1 <- merge(final1, bat_species_final[, c(1,2, 5:11)], by=c("date", "site"), all = TRUE )

weather <- read.csv("weather.csv")
weather$date <- as.Date(weather$date)
weather <- subset(weather, weather$date > as.Date("2016-11-18"))

final1 <- merge(final1, weather, by="date", all.x = TRUE)

#####determing day of the week########
final1$date <- as.Date(final1$date)
final1$weekday <- weekdays(final1$date)
final1$weekday <- factor(final1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

final1$year <- as.factor(format(final1$date, "%Y"))
final1$month <- as.factor(format(final1$date, "%m"))
final1$day <- as.factor(format(final1$date, "%d"))

#### season ####
final1$season <- final1$month
final1$season <- revalue(final1$season, c("12"="Winter","01"="Winter", "02"="Winter", 
                         "03"="Spring", "04"="Spring", "05"="Spring",
                         "06"="Summer", "07"="Summer", "08"="Summer",
                         "09"="Fall", "10"="Fall", "11"="Fall"))

### julian day###
final1$julian <- final1$date
final1$julian <- format(final1$julian, "%j")
final1$julian1 <- as.numeric(final1$julian)

###fill in NA for numeric####
final1[is.na(final1)] <- 0


write.csv(final1, "UNCG_allbats.csv")

##########################################################################################################
################### clean up GSC data #######################
rawdf <- read.csv("GSC.csv")
df <- rawdf[, c(1, 11, 17, 18, 20)]

df$DATE <- as.Date(df$DATE)

min(df$DATE)
max(df$DATE)

bat_total_long <- subset(df[, c(1:3)], df$PULSES>2)
bat_total <- aggregate(bat_total_long, 
                       by =  list(bat_total_long$site, bat_total_long$DATE), 
                       FUN = length)
names(bat_total)[2]<- "date"
names(bat_total)[1]<- "site"
names(bat_total)[3]<- "total"

bat_total <- bat_total[, c(1:3)]

bat_species_long <- subset(df[, c(1:3)], df$MATCH.RATIO >= 0.5)
bat_species <- aggregate(bat_species_long, by =  list(bat_species_long$DATE, 
                                                      bat_species_long$site, 
                                                      bat_species_long$AUTO.ID.), FUN = length)
names(bat_species)[1] <- "date"
names(bat_species)[2] <- "site"
names(bat_species)[3]  <- "species"
names(bat_species)[4] <- "count"
bat_species <- bat_species[, c(1:4)]

bat_species_final <- spread(bat_species, species, count)
bat_species_final[is.na(bat_species_final)] <- 0

###### building date frame #####
site_range <- unique(df$site)
date_range <- rep(seq.Date(from = min(df$DATE), to = max(df$DATE), by = "days" ))
final <- expand.grid(date_range, site_range)
names(final)[1] <- "date"
names(final)[2] <- "site"

#### merging all data####
final1 <- merge(final, bat_total, by=c("date", "site"), all = TRUE)
final1 <- merge(final1, bat_species_final, by=c("date", "site"), all = TRUE )

weather <- read.csv("weather.csv")
weather$date <- as.Date(weather$date)
weather <- subset(weather, weather$date > as.Date("2016-11-18"))

final1 <- merge(final1, weather, by="date", all.x = TRUE)

#####determing day of the week########
final1$date <- as.Date(final1$date)
final1$weekday <- weekdays(final1$date)
final1$weekday <- factor(final1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

final1$year <- as.factor(format(final1$date, "%Y"))
final1$month <- as.factor(format(final1$date, "%m"))
final1$day <- as.factor(format(final1$date, "%d"))

#### season ####
final1$season <- final1$month
final1$season <- revalue(final1$season, c("12"="Winter","01"="Winter", "02"="Winter", 
                                          "03"="Spring", "04"="Spring", "05"="Spring",
                                          "06"="Summer", "07"="Summer", "08"="Summer",
                                          "09"="Fall", "10"="Fall", "11"="Fall"))

### julian day###
final1$julian <- final1$date
final1$julian <- format(final1$julian, "%j")
final1$julian1 <- as.numeric(final1$julian)

###fill in NA for numeric####
final1[is.na(final1)] <- 0
final1$complete[final1$total>0] <- "yes"
final1$complete[final1$total==0] <- "no"

write.csv(final1, "GSC_allbats.csv")




########################### preliminary for AGE#####################
bat3<- read.csv("UNCG_allbats.csv")
bat3$complete <- revalue(bat3$habitat, c("Wood"="yes", "Rec"="yes"))
levels <- levels(bat3$complete)
levels[length(levels) + 1] <- "no"
bat3$complete <- factor(bat3$complete, levels = levels)
bat3$complete[is.na(bat3$complete)] <-"no"

bat3 <- bat3[, c(1,2, 5:29)]

bat3$site1 <- revalue(bat3$site, c("Reccontrol"="UNCG 1", "Woodcon"="UNCG 2", 
                                   "Recwet"="UNCG 3", "Woodwet"="UNCG 4"))
bat3$id <- paste(bat3$site1, bat3$season, sep = "-")

bat2<- read.csv("GSC_allbats.csv")
bat2 <- bat2[, c(1:6, 8:28)]

bat1 <- rbind(bat2, bat3)
bat1 <- subset(bat1, bat1$complete=="yes")

bat1<- bat10
bat1$weekday <- factor(bat1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bat1$season <- factor(bat1$season, levels =
                        c("Spring", "Summer", "Fall", "Winter"))

bat1$month1 <- as.factor(bat1$month)
bat1$month1 <- revalue(bat1$month1, c("12"="Dec","1"="Jan", "2"="Feb", 
                                          "3"="Mar", "4"="Apr", "5"="May",
                                          "6"="Jun", "7"="Jul", "8"="Aug",
                                          "9"="Sep", "10"="Oct", "11"="Nov"))
bat1$year1 <- as.factor(bat1$year)


###################################### wetland - control pair tests#########################
bat10<- read.csv("all_2016_2021.csv")
bat10$ddate <- as.Date(bat10$date)

#summary(bat10[bat10$site=="Woodwet"& bat10$ddate>"2017-12-31"&bat10$ddate<"2021-01-01"|
#                bat10$site=="Woodcon"& bat10$ddate>"2017-12-31"&bat10$ddate<"2021-01-01",]$total1_sd)
##############shapiro#############
shapiro.test(bat10[bat10$site=="Woodwet"& bat10$ddate>"2017-12-31"&bat10$ddate<"2021-01-01",]$total1_sd)
shapiro.test(bat10[bat10$site=="Woodcon"& bat10$ddate>"2017-12-31"&bat10$ddate<"2021-01-01",]$total1_sd)
shapiro.test(bat10[bat10$site=="Recwet"& bat10$ddate>"2017-12-31"&bat10$ddate<"2021-01-01",]$total1_sd)
shapiro.test(bat10[bat10$site=="Reccontrol"& bat10$ddate>"2017-12-31"&bat10$ddate<"2021-01-01",]$total1_sd)
##############prepare data####

df <- bat10[bat10$site=="Recwet"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01"|
              bat10$site=="Reccontrol"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]

df$year2 <- as.factor(df$year2)
df$season <- factor(df$season, levels = c("Spring", "Summer", "Fall", "Winter"))

df <- bat10[bat10$site=="Woodwet"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01"|
              bat10$site=="Woodcon"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]

df$year2 <- as.factor(df$year2)
df$season <- factor(df$season, levels = c("Spring", "Summer", "Fall", "Winter"))


#####extra ######
results <- data.frame(expand.grid(
  year=unique(df$year2),
  season=unique(df$season),
  species=names(df[c(42:49)]))
)

results <- data.frame(expand.grid(
  year=unique(df$year2),
  season=unique(df$season))
)
#####loop for wilcoxon ##########

sink("test2.txt")

for (i in unique(df$year2)){
  for (j in unique(df$season)){
    for (k in names(df[c(42:49)])){
      new_df<- subset(df, df$season==j & df$year2==i)
      print (i)
      print (j)
      print (k)
      print ("Woodwet")
      print(median(new_df[new_df$site=="Woodwet",k]))
      print ("Woodcon")
      print(median(new_df[new_df$site=="Woodcon",k]))
      wc <- wilcox.test(new_df[new_df$site=="Woodwet",k], 
                        new_df[new_df$site=="Woodcon",k],
                        paired = FALSE)
      print(wc$statistic)
      print(wc$p.value)
    }
  }
}

sink()

long <- read.csv("test2.csv")
wide <- spread(long, item, value)
write.csv(wide, "pair results2.csv")

####### fig 1, table 1#######

pair_rec_long <- read.csv("pair_rec.csv")
pair_rec_graph <- pair_rec_long[, c(1, 5, 8, 9)]

pair_rec_graph$time <- factor(pair_rec_graph$time, levels = c(
  "2017-Spring", "2017-Summer", "2017-Fall", "2017-Winter",
  "2018-Spring", "2018-Summer", "2018-Fall", "2018-Winter",
  "2019-Spring", "2019-Summer", "2019-Fall", "2019-Winter",
  "2020-Spring", "2020-Summer", "2020-Fall", "2020-Winter" ))

pair_rec_graph$compare <- factor(pair_rec_graph$compare, levels = c(
  "control", "wetland", "same"))

fig1 <- ggplot(data = pair_rec_graph, aes(x=species, y=time)) +
  geom_tile(aes(fill=compare),, color="white") +
  scale_fill_manual(name = "Habitat with \nhigher bat activity", 
                    values = wes_palette("Moonrise1", n = 3),
                    labels = c("Control", "Wetland", "No difference")) +
  xlab("Species") +
  ylab("Timeline") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    #legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =14)
  )

ggsave("fig1.jpg", plot=fig1, height = 7.5, width = 10, dpi =1200)

#####table###
table1 <- spread(pair_rec_graph[,c(1,2,4)], species, p.value)
write.csv(table1, "table1.csv")

pair_rec_long$time <- factor(pair_rec_long$time, levels = c(
  "2017-Spring", "2017-Summer", "2017-Fall", "2017-Winter",
  "2018-Spring", "2018-Summer", "2018-Fall", "2018-Winter",
  "2019-Spring", "2019-Summer", "2019-Fall", "2019-Winter",
  "2020-Spring", "2020-Summer", "2020-Fall", "2020-Winter" ))
table11 <- spread(pair_rec_long[,c(1,2,9)], species, w.value)
write.csv(table11, "table11.csv")


####### fig 2, table 2 #############

pair_wood_long <- read.csv("pair_wood.csv")
pair_wood_graph <- pair_wood_long[, c(1, 6, 8, 9)]

pair_wood_graph$time <- factor(pair_wood_graph$time, levels = c(
  "2017-Spring", "2017-Summer", "2017-Fall", "2017-Winter",
  "2018-Spring", "2018-Summer", "2018-Fall", "2018-Winter",
  "2019-Spring", "2019-Summer", "2019-Fall", "2019-Winter",
  "2020-Spring", "2020-Summer", "2020-Fall", "2020-Winter" ))

pair_wood_graph$compare <- factor(pair_wood_graph$compare, levels = c(
  "control", "wetland", "same"))

fig2 <- ggplot(data = pair_wood_graph, aes(x=species, y=time)) +
  geom_tile(aes(fill=compare),, color="white") +
  scale_fill_manual(name = "Habitat with \nhigher bat activity", 
                    values = wes_palette("Moonrise1", n = 3),
                    labels = c("Control", "Wetland", "No difference")) +
  xlab("Species") +
  ylab("Timeline") + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    #legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =14)
  )

ggsave("fig2.jpg", plot=fig2, height = 7.5, width = 10, dpi =1200)

#####table###
table2 <- spread(pair_wood_graph[,c(1,2,3)], species, p.value)
write.csv(table2, "table2.csv")

pair_wood_long$time <- factor(pair_wood_long$time, levels = c(
  "2017-Spring", "2017-Summer", "2017-Fall", "2017-Winter",
  "2018-Spring", "2018-Summer", "2018-Fall", "2018-Winter",
  "2019-Spring", "2019-Summer", "2019-Fall", "2019-Winter",
  "2020-Spring", "2020-Summer", "2020-Fall", "2020-Winter" ))
table21 <- spread(pair_wood_long[,c(1,6,7)], species, w.value)
write.csv(table21, "table21.csv")

##extra##
wilcox.test(bat10[bat10$site=="Woodwet"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]$total1_sd,
            bat10[bat10$site=="Woodcon"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]$total1_sd,
            paired = FALSE)

wilcox.test(bat10[bat10$site=="Recwet"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]$total1_sd,
            bat10[bat10$site=="Reccontrol"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]$total1_sd,
            paired = FALSE)


fit <- glm(data=df, total1_sd~site*year1, family="quasipoisson")
fit <- glm(data=df, total1_sd~site*month1, family="quasipoisson")
fit <- glm(data=df, total1_sd~site*season*year1, family="quasipoisson")

summary(fit)

####### raw data plot for supplementary####
grec <- ggplot(data=df, aes(x=site, y=EPTFUS_sd)) +
  geom_boxplot() +
  facet_grid(season~year2, scales = "free")+
  stat_compare_means(label = "p.format",  method = "wilcox.test", label.x.npc = "center", 
                     label.y.npc = 0.9,)

grec
ggsave(plot=grec, filename="rec_epfu.jpeg", units = "cm", height = 24, width = 40, dpi=300)


df2 <- bat10[bat10$site=="Woodwet"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01"|
               bat10$site=="Woodcon"& bat10$ddate>"2017-03-31"&bat10$ddate<"2021-01-01",]
df2$year1 <- as.factor(df2$year1)
df2$season <- factor(df2$season, levels = c("Spring", "Summer", "Fall", "Winter"))

gwet <- ggplot(data=df2, aes(x=site, y=TADBRA_sd)) +
  geom_boxplot() +
  facet_grid(season~year1, scales = "free")+
  stat_compare_means(label = "p.format",  method = "wilcox.test", label.x.npc = "center", 
                     label.y.npc = 0.9,)

gwet
ggsave(plot=gwet, filename="wood_tabr.jpeg", units = "cm", height = 24, width = 40, dpi=300)


gdf <- read.csv("gdf.csv")
gdf$cool <- as.factor(gdf$cool)
ggplot(data=gdf, 
       aes(x=species, y=time))+ 
  geom_tile(aes(fill=cool))+
  scale_fill_manual(breaks = c("3", "2", "1"), 
                    values=c("red", "grey", "yellow"))


+ 
  # scale_x_discrete(labels = c("Big brown", "Red", "Hoary", "Silver-haired", 
  #    "Evening", "Tricolored", "Mexcian \nfree-tailed")) + 
  scale_y_discrete(limits = rev(levels(all2$month)),
                   labels = rev(c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"))) +
  scale_fill_continuous(name = "Percent of all\nidentified bat \npasses", 
                        low ="white", high="#005633", 
                        labels = scales::percent) + 
  facet_wrap(~ site, ncol = 2, scales = "free") +
  xlab("Species") +
  ylab("Month") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    #legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =14)
  )



############################################################################################################################

########################### monthly tests ##################################

#### aggregate data by month #####

bat10<- read.csv("all_2016_2021.csv")
bat4 <- bat10[, c(1,2, 24, 28, 29,41:48)]


bat_mounth <- aggregate(bat4, by=list(bat4$site, bat4$season, bat4$month1, bat4$year1), FUN = mean)

bat_mounth <- bat_mounth[, c(1:4, 10:17)]

bat_month <- bat_mounth %>%
  rename(
    site = Group.1,
    season = Group.2,
    month = Group.3,
    year = Group.4
  )

####monthly plot#####
ggplot(data=bat_month, aes(x=month, y=total1_sd, fill=year, color=year, group=year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol=4, scales = "free")

bat_month$date <- paste(bat_month$year, bat_month$month, sep = "-")
bat_month$date <- factor(bat_month$date, levels = c(
  "2016-Nov", "2016-Dec",
  "2017-Jan", "2017-Feb", "2017-Mar", "2017-Apr", "2017-May", "2017-Jun", "2017-Jul", "2017-Aug", "2017-Sep", "2017-Oct", "2017-Nov", "2017-Dec",
  "2018-Jan", "2018-Feb", "2018-Mar", "2018-Apr", "2018-May", "2018-Jun", "2018-Jul", "2018-Aug", "2018-Sep", "2018-Oct", "2018-Nov", "2018-Dec",
  "2019-Jan", "2019-Feb", "2019-Mar", "2019-Apr", "2019-May", "2019-Jun", "2019-Jul", "2019-Aug", "2019-Sep", "2019-Oct", "2019-Nov", "2019-Dec",
  "2020-Jan", "2020-Feb", "2020-Mar", "2020-Apr", "2020-May", "2020-Jun", "2020-Jul", "2020-Aug", "2020-Sep", "2020-Oct", "2020-Nov", "2020-Dec",
  "2021-Jan"
))

ggplot(data=bat_month, aes(x=date, y=total1_sd, group=1, color=season)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, ncol=1, scales = "free") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    strip.text = element_text(family="serif", size =8, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=8),
    legend.text = element_text(family="serif",size=8),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=8), 
    axis.text = element_text(family="serif", size =8),
    axis.text.x = element_text(angle = 45)
  ) 

write.csv(bat_month,"bat_month_all.csv")

########################################MRPP################################
#################in its own file########################

#######################################NMDS##################################
################### test for 2018-2020###
df <- subset(bat10, year=="2018" & site=="Recwet"&id_raw>0|year=="2019" & site=="Recwet"&id_raw>0|year=="2020" & site=="Recwet"&id_raw>0)
bat_matirx <- as.matrix(df[, c(42:48)])
nmds_18_20 <- metaMDS(bat_matirx, k=4, distance  = "euclidean", trymax = 100)
nmds_18_20_3 <- metaMDS(bat_matirx, k=3, distance  = "euclidean", trymax = 100)
nmds_18_20_2 <- metaMDS(bat_matirx, k=2, distance  = "bray", trymax = 5000)

####################################monthly###########################################
###############################Recwet###########################################
bat_month <- read.csv("bat_month.csv")
bat_month$season <- factor(bat_month$season, levels =
                        c("Spring", "Summer", "Fall", "Winter"))
bat_month$year <- as.factor(bat_month$year)

df <- subset(bat_month, 
      year=="2017" & site=="Recwet"|
      year=="2018" & site=="Recwet"|
      year=="2019" & site=="Recwet"|
      year=="2020" & site=="Recwet")
bat_matirx <- as.matrix(df[, c(6:12)])
nmds_month <- metaMDS(bat_matirx, k=2, distance  = "euclidean", trymax = 100)

orditorp(nmds_month, display = "sites")

data.scores <- as.data.frame(scores(nmds_month))  
data.scores$X <- as.numeric(rownames(data.scores))

species.scores <- as.data.frame(scores(nmds_month, "species"))  
species.scores$species <- rownames(species.scores)

df_graph <- cbind(data.scores[, c(1,2)], df)
df_graph$year <- as.factor(df_graph$year)
nmds_graph_recwet <- ggplot(data=df_graph, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(shape=year, color=season), size =5, alpha =0.7) +
  ggrepel::geom_text_repel(data= species.scores, 
            aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
            size=4, alpha=1)+
  geom_segment(data = species.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_recwet

###############################Reccontrol###########################################
df_reccontrol <- subset(bat_month,
                        year=="2017" & site == "Reccontrol"|
             year=="2018" & site=="Reccontrol"|
               year=="2019" & site=="Reccontrol"|
               year=="2020" & site=="Reccontrol")
bat_matirx_reccontrol <- as.matrix(df_reccontrol[, c(6:12)])
nmds_month_reccontrol <- metaMDS(bat_matirx_reccontrol, k=2, distance  = "euclidean", trymax = 100)


data.scores_reccontrol <- as.data.frame(scores(nmds_month_reccontrol))  
data.scores_reccontrol$X <- as.numeric(rownames(data.scores_reccontrol))

species.scores_reccontrol <- as.data.frame(scores(nmds_month_reccontrol, "species"))  
species.scores_reccontrol$species <- rownames(species.scores_reccontrol)

df_graph_reccontrol <- cbind(data.scores_reccontrol[, c(1,2)], df_reccontrol)

nmds_graph_reccontrol <- ggplot(data=df_graph_reccontrol, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(color=season, shape=year), size =5, alpha = 0.7) +
  ggrepel::geom_text_repel(data= species.scores_reccontrol, 
            aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
            size=4, alpha=1)+
  geom_segment(data = species.scores_reccontrol, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_reccontrol

###############################woodwet###########################################
df_woodwet <- subset(bat_month, 
                     year=="2017" & site=="Woodwet"|
                        year=="2018" & site=="Woodwet"|
                          year=="2019" & site=="Woodwet"|
                          year=="2020" & site=="Woodwet")
bat_matirx_woodwet <- as.matrix(df_woodwet[, c(6:12)])
nmds_month_woodwet <- metaMDS(bat_matirx_woodwet, k=2, distance  = "euclidean", trymax = 100)


data.scores_woodwet <- as.data.frame(scores(nmds_month_woodwet))  
data.scores_woodwet$X <- as.numeric(rownames(data.scores_woodwet))

species.scores_woodwet <- as.data.frame(scores(nmds_month_woodwet, "species"))  
species.scores_woodwet$species <- rownames(species.scores_woodwet)

df_graph_woodwet <- cbind(data.scores_woodwet[, c(1,2)], df_woodwet)

nmds_graph_woodwet<- ggplot(data=df_graph_woodwet, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(color=season, shape=year), size =5, alpha = 0.7) +
  ggrepel::geom_text_repel(data= species.scores_woodwet, 
            aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
            size=4, alpha=1)+
  geom_segment(data = species.scores_woodwet, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_woodwet

###############################woodcon###########################################
df_woodcon <- subset(bat_month,
                     year=="2017" & site=="Woodcon"|
                     year=="2018" & site=="Woodcon"|
                       year=="2019" & site=="Woodcon"|
                       year=="2020" & site=="Woodcon")
bat_matirx_woodcon <- as.matrix(df_woodcon[, c(6:12)])
nmds_month_woodcon <- metaMDS(bat_matirx_woodcon, k=2, distance  = "euclidean", trymax = 100)


data.scores_woodcon <- as.data.frame(scores(nmds_month_woodcon))  
data.scores_woodcon$X <- as.numeric(rownames(data.scores_woodcon))

species.scores_woodcon <- as.data.frame(scores(nmds_month_woodcon, "species"))  
species.scores_woodcon$species <- rownames(species.scores_woodcon)

df_graph_woodcon <- cbind(data.scores_woodcon[, c(1,2)], df_woodcon)

nmds_graph_woodcon<- ggplot(data=df_graph_woodcon, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(color=season, shape=year), size =5, alpha = 0.7) +
  ggrepel::geom_text_repel(data= species.scores_woodcon, 
            aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
            size=4, alpha=1)+
  geom_segment(data = species.scores_woodcon, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_woodcon

###############################roof###########################################
bat_month <- read.csv("bat_month_all.csv")
bat_month$season <- factor(bat_month$season, levels =
                             c("Spring", "Summer", "Fall", "Winter"))
bat_month$year <- as.factor(bat_month$year)

df <- subset(bat_month, 
             year=="2017" & site=="roof"|
               year=="2018" & site=="roof"|
               year=="2019" & site=="roof"|
               year=="2020" & site=="roof")
bat_matirx <- as.matrix(df[, c(7:13)])
nmds_month <- metaMDS(bat_matirx, k=2, distance  = "euclidean", trymax = 100)

#orditorp(nmds_month, display = "sites")

data.scores <- as.data.frame(scores(nmds_month))  
data.scores$X <- as.numeric(rownames(data.scores))

species.scores <- as.data.frame(scores(nmds_month, "species"))  
species.scores$species <- rownames(species.scores)

df_graph <- cbind(data.scores[, c(1,2)], df)
df_graph$year <- as.factor(df_graph$year)
nmds_graph_roof <- ggplot(data=df_graph, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(shape=year, color=season), size =5, alpha =0.7) +
  ggrepel::geom_text_repel(data= species.scores, 
                           aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
                           size=4, alpha=1)+
  geom_segment(data = species.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_roof

###############################wild###########################################
bat_month <- read.csv("bat_month_all.csv")
bat_month$season <- factor(bat_month$season, levels =
                             c("Spring", "Summer", "Fall", "Winter"))
bat_month$year <- as.factor(bat_month$year)

df <- subset(bat_month, 
             year=="2017" & site=="wild"|
               year=="2018" & site=="wild"|
               year=="2019" & site=="wild"|
               year=="2020" & site=="wild")
bat_matirx <- as.matrix(df[, c(7:13)])
nmds_month <- metaMDS(bat_matirx, k=2, distance  = "euclidean", trymax = 100)

#orditorp(nmds_month, display = "sites")

data.scores <- as.data.frame(scores(nmds_month))  
data.scores$X <- as.numeric(rownames(data.scores))

species.scores <- as.data.frame(scores(nmds_month, "species"))  
species.scores$species <- rownames(species.scores)

df_graph <- cbind(data.scores[, c(1,2)], df)
df_graph$year <- as.factor(df_graph$year)
nmds_graph_wild <- ggplot(data=df_graph, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(shape=year, color=season), size =5, alpha =0.7) +
  ggrepel::geom_text_repel(data= species.scores, 
                           aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
                           size=4, alpha=1)+
  geom_segment(data = species.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_wild

###############################woods###########################################
bat_month <- read.csv("bat_month_all.csv")
bat_month$season <- factor(bat_month$season, levels =
                             c("Spring", "Summer", "Fall", "Winter"))
bat_month$year <- as.factor(bat_month$year)

df <- subset(bat_month, 
             year=="2017" & site=="woods"|
               year=="2018" & site=="woods"|
               year=="2019" & site=="woods"|
               year=="2020" & site=="woods")
bat_matirx <- as.matrix(df[, c(7:13)])
nmds_month <- metaMDS(bat_matirx, k=2, distance  = "euclidean", trymax = 100)

#orditorp(nmds_month, display = "sites")

data.scores <- as.data.frame(scores(nmds_month))  
data.scores$X <- as.numeric(rownames(data.scores))

species.scores <- as.data.frame(scores(nmds_month, "species"))  
species.scores$species <- rownames(species.scores)

df_graph <- cbind(data.scores[, c(1,2)], df)
df_graph$year <- as.factor(df_graph$year)
nmds_graph_woods <- ggplot(data=df_graph, aes(x=NMDS1, y=NMDS2)) +
  geom_point(aes(shape=year, color=season), size =5, alpha =0.7) +
  ggrepel::geom_text_repel(data= species.scores, 
                           aes(x=NMDS1,y=NMDS2, label=species, family="serif", fontface = "bold"), 
                           size=4, alpha=1)+
  geom_segment(data = species.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3)+
  scale_color_manual(values = wes_palette("Moonrise2", n = 4)) 

nmds_graph_woods


####################### combine panels ########################
ggarrange(nmds_graph_reccontrol, nmds_graph_recwet, nmds_graph_woodcon, nmds_graph_woodwet,
          labels = c("Rec control", "Rec wetland",
                     "wood control", "wood wetland"),
          ncol=2, nrow=2)

#######################################################################################


############################################part 3####################################
#####################################mantel test#############
bat_month <- read.csv("bat_month_mantel1.csv")
bat_month$season <- factor(bat_month$season, levels =
                             c("Spring", "Summer", "Fall", "Winter"))
bat_month$year <- as.factor(bat_month$year)


df <- subset(bat_month, site=="roof")
           #  year=="2017" & site=="roof"|
           #    year=="2018" & site=="roof"|
           #    year=="2019" & site=="roof"|
            #   year=="2020" & site=="roof")
bat_matirx <- as.matrix(df[, c(7:13)])

##############test for matirx vs doubled maxtrix######################
bat_matirx2 <- bat_matirx*2
dist1 <- vegdist(bat_matirx, method = "euclidean")
dist2 <- vegdist(bat_matirx2, method = "euclidean")
mantel(dist1, dist2, permutations = 9999, na.rm = TRUE)
###########################################################################
########################back to mantel test#########################
dist_roof <- vegdist(bat_matirx, method = "euclidean")

df <- subset(bat_month, site=="woods")
           #  year=="2017" & site=="woods"|
            #   year=="2018" & site=="woods"|
             #  year=="2019" & site=="woods"|
              # year=="2020" & site=="woods")
bat_matirx <- as.matrix(df[, c(7:13)])

dist_woods <- vegdist(bat_matirx, method = "euclidean")

df <- subset(bat_month, site=="wild")
            # year=="2017" & site=="wild"|
             #  year=="2018" & site=="wild"|
              # year=="2019" & site=="wild"|
               # year=="2020" & site=="wild")
bat_matirx <- as.matrix(df[, c(7:13)])

dist_wild <- vegdist(bat_matirx, method = "euclidean")

df <- subset(bat_month, site=="Recwet")
           #  year=="2017" & site=="Recwet"|
            #   year=="2018" & site=="Recwet"|
             #  year=="2019" & site=="Recwet"|
              # year=="2020" & site=="Recwet")
bat_matirx <- as.matrix(df[, c(7:13)])
dist_recwet <- vegdist(bat_matirx, method = "euclidean")

df <- subset(bat_month, site=="Reccontrol")
         #    year=="2017" & site=="Reccontrol"|
          #     year=="2018" & site=="Reccontrol"|
           #    year=="2019" & site=="Reccontrol"|
            #   year=="2020" & site=="Reccontrol")
bat_matirx <- as.matrix(df[, c(7:13)])
dist_reccontrol <- vegdist(bat_matirx, method = "euclidean")

df <- subset(bat_month, site=="Woodcon")
            #   year=="2017" & site=="Woodcon"|
             #  year=="2018" & site=="Woodcon"|
              # year=="2019" & site=="Woodcon"|
               # year=="2020" & site=="Woodcon")
bat_matirx <- as.matrix(df[, c(7:13)])
dist_woodcon <- vegdist(bat_matirx, method = "euclidean")

df <- subset(bat_month, site=="Woodwet")
         #    year=="2017" & site=="Woodwet"|
          #     year=="2018" & site=="Woodwet"|
           #    year=="2019" & site=="Woodwet"|
            #   year=="2020" & site=="Woodwet")
bat_matirx <- as.matrix(df[, c(7:13)])
dist_woodwet <- vegdist(bat_matirx, method = "euclidean")

recwet_recontrol <- mantel(dist_recwet, dist_reccontrol, permutations = 9999, na.rm = TRUE)
recwet_recontrol

recwet_woodwet <- mantel(dist_recwet, dist_woodwet, permutations = 9999, na.rm = TRUE)
recwet_woodwet

recwet_woodcon <- mantel(dist_recwet, dist_woodcon, permutations = 9999, na.rm = TRUE)
recwet_woodcon

recwet_roof <- mantel(dist_recwet, dist_roof, permutations = 9999, na.rm = TRUE)
recwet_roof

recwet_wild <- mantel(dist_recwet, dist_wild, permutations = 9999, na.rm = TRUE)
recwet_wild

recwet_woods <- mantel(dist_recwet, dist_woods, permutations = 9999, na.rm = TRUE)
recwet_woods

reccontrol_woodwet <- mantel(dist_reccontrol, dist_woodwet,permutations = 9999, na.rm = TRUE)
reccontrol_woodwet

recontrol_woodcon <- mantel(dist_woodcon, dist_reccontrol, permutations = 9999, na.rm = TRUE)
recontrol_woodcon

recontrol_roof <- mantel(dist_roof, dist_reccontrol, permutations = 9999, na.rm = TRUE)
recontrol_roof

recontrol_wild <- mantel(dist_wild, dist_reccontrol, permutations = 9999, na.rm = TRUE)
recontrol_wild

recontrol_woods <- mantel(dist_woods, dist_reccontrol, permutations = 9999, na.rm = TRUE)
recontrol_woods

woodwet_woodcon <- mantel(dist_woodcon, dist_woodwet, permutations = 9999, na.rm = TRUE)
woodwet_woodcon

woodwet_roof <- mantel(dist_roof, dist_woodwet, permutations = 9999, na.rm = TRUE)
woodwet_roof

woodwet_wild <- mantel(dist_wild, dist_woodwet, permutations = 9999, na.rm = TRUE)
woodwet_wild

woodwet_woods <- mantel(dist_woods, dist_woodwet, permutations = 9999, na.rm = TRUE)
woodwet_woods

woodcon_roof <- mantel(dist_roof, dist_woodcon, permutations = 9999, na.rm = TRUE)
woodcon_roof

woodcon_wild <- mantel(dist_wild, dist_woodcon, permutations = 9999, na.rm = TRUE)
woodcon_wild

woodcon_woods <- mantel(dist_woods, dist_woodcon, permutations = 9999, na.rm = TRUE)
woodcon_woods

roof_wild <- mantel(dist_roof, dist_wild, permutations = 9999, na.rm = TRUE)
roof_wild

roof_woods <- mantel(dist_roof, dist_woods, permutations = 9999, na.rm = TRUE)
roof_woods

wild_woods <- mantel(dist_wild, dist_woods, permutations = 9999, na.rm = TRUE)
wild_woods

########### automate mantel test, not done
#####https://anon0433.github.io/strong-individual-signatures/mantel_tests_and_spatial_autocorrelation#figure_4:_mantel-based_correlogram
ml <- list(dist_recwet, dist_reccontrol, dist_woodwet, dist_woodcon, dist_roof, dist_wild, dist_woods)

tr <- mantel(ml[1], ml[2], permutations = 999, na.rm = TRUE)

for(i in 1:length(ml)){
  if(i<length(ml)){
    
  }
}

###### network graph foe mantle/not used#####
try <- read.csv("try2.csv")
row.names(try) <- c("rec.wetland", "rec.control", "woody.wetland", "woody.control", "roof",  "woods")
try <- try[, 2:7]

xtry <- as_cordf(try, diagonal = 0)

network_plot(xtry, min_cor = 0.95, 
             , curved = FALSE, colors = "red")  +
  theme_bw()

try <- read.csv("try3.csv")
row.names(try) <- c("rec.wetland", "rec.control", "woody.wetland", "woody.control", "roof",  "wild")
try <- try[, 2:7]

xtry <- as_cordf(try, diagonal = 0)

network_plot(xtry, min_cor = 0.95, curved = FALSE)


try <- read.csv("try1.csv")
row.names(try) <- c("rec.wetland", "rec.control", "woody.wetland", "woody.control", "roof",  "wild", "woods")
try <- try[, 2:8]

xtry <- as_cordf(try, diagonal = 0)

network_plot(xtry, min_cor = 0.95, curved = FALSE, repel = TRUE)






######################################extra#########################################
################################# annual graph#########################
install.packages('devtools')
library(devtools)
install.packages('testthat')
library(testthat)
library(facetscales)

bat10 <- subset(subset(bat1, year=="2018"|year=="2019"|year=="2020"), season!="Winter")
#scales_y <- list(
#  `Spring` = scale_y_continuous(limits = c(0, quantile(bat10[bat10$season=="Spring",]$total1, 0.9))),
#  `Summer` = scale_y_continuous(limits = c(0, quantile(bat10[bat10$season=="Summer",]$total1, 0.9))),
#  `Fall` = scale_y_continuous(limits = c(0, quantile(bat10[bat10$season=="Fall",]$total1, 0.9))))
#facet_grid_sc(rows =vars(season),  scales = list(y = scales_y))

bat10$season1 <- droplevels(bat10$season)
bat10$total1 <- bat10$total/bat10$night_span
bat10$different <- "color1"
bat10[bat10$season=="Spring"&bat10$year=="2019",]$different <- "color2"
bat10[bat10$season=="Summer"&bat10$year=="2020",]$different <- "color2"
bat10[bat10$season=="Spring"&bat10$year=="2020",]$different <- "color3"

annual_plot <- ggplot(data= bat10, 
                      aes(x=year1, y=(total/night_span), fill=different)) +
  geom_boxplot() +
  facet_wrap(~season1, nrow=1, scales ="free")  +
  xlab("Year") +
  ylab("Total bat activity (number of bat passes/recording hour)") +
  scale_fill_manual(values = wes_palette("GrandBudapest2", n = 3)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =16)
  ) 

ggsave("annual_comparison3.png",plot=annual_plot, 
       dpi=500, dev='png', height=14, width=20, units="cm")




############################# analysis for posters ############################
###############################################################################
bat1<- read.csv("UNCG_allbats.csv")
bat1$weekday <- factor(bat1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bat1$season <- factor(bat1$season, levels =
                       c("Spring", "Summer", "Fall", "Winter"))


UNCG_open2020 <- subset(bat1, bat1$year=="2020" & bat1$site == "Reccontrol" 
                        & bat1$treatment == "Control" & bat1$season != "Winter" )


Plot_UNCG_Open2020 <-ggplot(data= UNCG_open2020, aes(x=weekday, y=(total/night_span))) +
  geom_boxplot() +
  facet_grid(season ~ ., scales = "free", 
             labeller = labeller(season = c(Spring= "Spring", Summer = "Summer", Fall = "Fall"))) +
  xlab("Day of the week") +
  ylab("Total bat activity (number of bat passes/recording hour)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =16)
  ) 

ggsave("UNCG_Open2020.png",plot=Plot_UNCG_Open2020, 
       dpi=500, dev='png', height=16, width=20, units="cm")


###############################
UNCG_woods2020 <- subset(bat1, bat1$year=="2020" & bat1$site == "Woodcon" 
                        & bat1$treatment == "Control" & bat1$season != "Winter" )


Plot_UNCG_woods2020 <-ggplot(data= UNCG_woods2020, aes(x=weekday, y=(total/night_span))) +
  geom_boxplot() +
  facet_grid(season ~ ., scales = "free", 
             labeller = labeller(season = c(Spring= "Spring", Summer = "Summer", Fall = "Fall"))) +
  xlab("Day of the week") +
  ylab("Total bat activity (number of bat passes/recording hour)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =16)
  ) 

ggsave("UNCG_Woods2020.png",plot=Plot_UNCG_woods2020, 
       dpi=500, dev='png', height=16, width=20, units="cm")


########################### GSC##############################
bat1<- read.csv("GSC_allbats.csv")
bat1 <- subset(bat1, complete=="yes")

bat1$weekday <- factor(bat1$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
bat1$season <- factor(bat1$season, levels =
                        c("Spring", "Summer", "Fall", "Winter"))


GSC_open2020 <- subset(bat1, bat1$year=="2020" & bat1$site == "roof" 
                         & bat1$season != "Winter" )

Plot_GSC_Open2020 <-ggplot(data= GSC_open2020, aes(x=weekday, y=(total/night_span))) +
  geom_boxplot() +
  facet_grid(season ~ ., scales = "free", 
             labeller = labeller(season = c(Spring= "Spring", Summer = "Summer", Fall = "Fall"))) +
  xlab("Day of the week") +
  ylab("Total bat activity (number of bat passes/recording hour)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =16)
  ) 

ggsave("GSC_Open2020.png",plot=Plot_GSC_Open2020, 
       dpi=500, dev='png', height=16, width=20, units="cm")


###############################
GSC_woods2020 <- subset(bat1, bat1$year=="2020" & bat1$site == "woods" 
                        & bat1$season != "Winter" )

Plot_GSC_woods2020 <-ggplot(data= GSC_woods2020, aes(x=weekday, y=(total/night_span))) +
  geom_boxplot() +
  facet_grid(season ~ ., scales = "free", 
             labeller = labeller(season = c(Spring= "Spring", Summer = "Summer", Fall = "Fall"))) +
  xlab("Day of the week") +
  ylab("Total bat activity (number of bat passes/recording hour)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.background = element_blank(),
    strip.text = element_text(family="serif", size =16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family="serif",size=14),
    legend.text = element_text(family="serif",size=14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family="serif", size=16), 
    axis.text = element_text(family="serif", size =16)
  ) 

ggsave("GSC_Woods2020.png",plot=Plot_GSC_woods2020, 
       dpi=500, dev='png', height=16, width=20, units="cm")

###glm###
###summer###
fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Summer"&site=="Reccontrol"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Summer"&site=="Woodcon"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Summer"&site=="roof"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Summer"&site=="woods"), family = "quasipoisson")
summary(fit)

###spring

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Spring"&site=="Reccontrol"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Spring"&site=="Woodcon"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Spring"&site=="roof"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Spring"&site=="woods"), family = "quasipoisson")
summary(fit)

###Fall

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Fall"&site=="Reccontrol"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Fall"&site=="Woodcon"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Fall"&site=="roof"), family = "quasipoisson")
summary(fit)

fit <- glm((total/night_span)~weekday + temp, 
           data=subset(bat1, year=="2020"&season=="Fall"&site=="woods"), family = "quasipoisson")
summary(fit)

####compare years
bat1$total1 <- bat1$total/bat1$night_span

bat_summer <- subset(bat1, season=="Summer")
bat_summer$year1 <- as.factor(bat_summer$year)
fit <- glm(total1~year*site + temp,  data=bat_summer, family = "quasipoisson")
summary(fit)

summary(glht(model = fit, linfct= mcp(year1 ="Tukey")))

bat_spring <- subset(bat1, season=="Spring")
bat_spring$year1 <- as.factor(bat_spring$year)
fit <- glm(total1~year1 + temp,  data=bat_spring, family = "quasipoisson")
summary(fit)
summary(glht(model = fit, linfct= mcp(year1 ="Tukey")))

bat_fall <- subset(bat1, season=="Fall"&year1!="2016")
bat_fall$year1 <- as.factor(bat_fall$year)
fit <- glm(total1~year1 + temp,  data=bat_fall, family = "quasipoisson")
summary(fit)
summary(glht(model = fit, linfct= mcp(year1 ="Tukey")))