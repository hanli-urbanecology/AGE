bat10<-read.csv("all_2016_2021.csv")

######Site roof###########
df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="roof"&year1=="2019"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp<- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="roof"&year1=="2019"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="roof"&year1=="2019"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="roof"&year1=="2019"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2019"&season=="Spring")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2019"&season=="Summer")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2019"&season=="Fall")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="roof"&year1=="2019"&season=="Winter")
df2<-subset(bat10, site=="roof"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

####################################################################################
######Site wild###########
df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="wild"&year1=="2019"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp<- mrpp(dat= df3[, c(4:10)], 
               grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="wild"&year1=="2019"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="wild"&year1=="2019"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="wild"&year1=="2019"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2019"&season=="Spring")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2019"&season=="Summer")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2019"&season=="Fall")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="wild"&year1=="2019"&season=="Winter")
df2<-subset(bat10, site=="wild"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

####################################################################################
######Site Reccontrol###########
df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp<- mrpp(dat= df3[, c(4:10)], 
               grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Spring")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Summer")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Fall")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Reccontrol"&year1=="2019"&season=="Winter")
df2<-subset(bat10, site=="Reccontrol"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

####################################################################################
######Site Recwet###########
df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp<- mrpp(dat= df3[, c(4:10)], 
               grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Spring")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Summer")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Fall")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2018"&season=="Winter")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Spring")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Spring")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Summer")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Summer")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Fall")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Fall")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp

df1<-subset(bat10, site=="Recwet"&year1=="2019"&season=="Winter")
df2<-subset(bat10, site=="Recwet"&year1=="2020"&season=="Winter")
df3 <-rbind(df1, df2)

df_mrpp <- mrpp(dat= df3[, c(4:10)], 
                grouping = df3$year1)
df_mrpp