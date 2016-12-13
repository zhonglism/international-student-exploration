library(dplyr)
library(xlsx)
library(highcharter)
origin<-read.xlsx('isfinal.xlsx',
                  sheetName = 'original')
schoollocation<-read.xlsx('isfinal.xlsx',
                          sheetName = 'schoollocation')
field1<-read.xlsx('isfinal.xlsx',
                  sheetName = 'field1')
field1$drilldown<-as.character(field1$drilldown)
field1[14,3]<-NA

drill<-read.xlsx('isfinal.xlsx',
                 sheetName = 'drill')
byori<-read.xlsx('isfinal.xlsx',
                 sheetName = 'byori')
level<-read.xlsx('isfinal.xlsx',
                 sheetName = 'level')
mo<-read.xlsx('isfinal.xlsx',
              sheetName = 'mo')



f_d<-data_frame(name=field1$fs,
                y=field1$amo,
                drilldown=field1$drilldown)
ds <- list_parse2(f_d)
names(f_d) <- NULL

to_sub<-field1$drilldown[1:13]
for (t in to_sub) {
  assign(paste0("an_", t),data_frame(
     name = as.character(drill[drill$type==t,]$fs),
     value = drill[drill$type==t,]$amount))
 } 
to_sub_ori<-levels(byori$type)

for (t in to_sub_ori) {
   assign(paste0("subori_",t),data_frame(
   name = as.character(byori[byori$type==t,]$place),
   value = byori[byori$type==t,]$amount))
}
