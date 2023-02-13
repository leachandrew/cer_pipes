library(tidyverse)
library(lubridate)
library(openxlsx)
library(RColorBrewer)



#PIPE CAPACITIES

#pipe_capacity_data <- read.xlsx(xlsxFile = "C:/Users/aleach/Google Drive/Reports/NEB/EF2016FigureData.xlsx", sheet = "10.7", startRow = 3,skipEmptyRows = TRUE,detectDates = TRUE)
pipe_capacity_data <- read.xlsx(xlsxFile = "pipes_capacity.xlsx", sheet = "2020_mods", startRow = 3,skipEmptyRows = TRUE,detectDates = TRUE)
names(pipe_capacity_data)[1]<-"Year"
names(pipe_capacity_data)[grep("Western.Refinery.Runs.Net.Burnaby",names(pipe_capacity_data))]<-"Western.Refinery.Runs"
#names(pipe_capacity_data)[grep("2016.NEB.Western.Canadian.Pipeline.Demand",names(pipe_capacity_data))]<-"2016.NEB.Western.Canadian.Pipeline.Demand"
pipe_capacity_data<-pipe_capacity_data%>% select(-`2014.CAPP.Supply.Raw`,-`2018.CAPP.Supply.Raw`)%>%
  mutate(CAPP.2019.Offtake=CAPP.2019.Supply.Raw/6.2929-Western.Refinery.Runs+Enbridge.Bakken.Volumes)%>%
  select(-CAPP.2019.Supply.Raw)%>%
  select(-CER.2019.Supply.Available)



df1<-pipe_capacity_data%>% pivot_longer(cols=-c("Year","CER.2021.Evolving","CER.2021.Current","CER.2021.Total.Capacity","CER.2020.Reference","CER.2020.Evolving","NEB.2016.Offtake","CAPP.2019.Offtake","CAPP.2018.Offtake","CAPP.2014.Offtake","TM.Ref.Prod","Western.Refinery.Runs","Enbridge.Bakken.Volumes","CER.2019.Mod"),
                                        values_to = "Capacity",names_to = "Pipeline")
expansions<-c("Keystone.XL","Trans.Mountain.Expansion","Northern.Gateway","Energy.East")
df1<-filter(df1,!(Pipeline %in% expansions))%>%
  mutate(Pipeline=factor(Pipeline,levels=
      c("Implied.Rail","Enbridge.Expansions","Keystone","Express","Trans.Mountain","Rangeland/Milk.River","Enbridge.Mainline")))

#c("Rail, suppos?","Agrandissements d'Enbridge","Keystone","Express","Trans.Mountain","Rangeland/Milk River","R?seau principal d'Enbridge" P?trole disponible pour l'exportation (NEB 2016)	P?trole disponible pour l'exportation (CAPP 2017))


levels(df1$Pipeline)<-gsub("\\.", " ", levels(df1$Pipeline)) 
levels(df1$Pipeline)[levels(df1$Pipeline)=="Implied Rail"]<-"Capacity Shortfall"

my_palette<-c(brewer.pal(7, "Set1"))

my_palette<-rev(c(brewer.pal(6, "Dark2")))



makeColors <- function(){
  maxColors <- 11
  usedColors <- c()
  possibleColors <- colorRampPalette(rev(brewer.pal(11 , "PuOr" )) )(maxColors)
  
  function(values){
    newKeys <- setdiff(values, names(usedColors))
    newColors <- possibleColors[1:length(newKeys)]
    usedColors.new <-  c(usedColors, newColors)
    names(usedColors.new) <- c(names(usedColors), newKeys)
    usedColors <<- usedColors.new
    
    possibleColors <<- possibleColors[length(newKeys)+1:maxColors]
    usedColors
  }
} 

mkColor <- makeColors()


png<-1

if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("pipe_capacity.png",height = 800,width = 1200)
ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack") +
  #geom_area(data=filter(df1,df1$Pipeline!="test"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",alpha=0.8) +
  geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test1" ),size=3) +
  #geom_line(aes(Year,CAPP.2018.Offtake*6.2929/1000, colour = "test2" ),size=3) +
  #geom_line(aes(Year,NEB.2016.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test4" ),size=3) +
  geom_line(aes(Year,CER.2019.Mod*6.2929/1000, colour = "test0" ),size=3) +
  scale_colour_manual("",labels=c("CER 2019 Export Demand","CAPP 2019 Export Demand","CAPP 2014 Export Demand"),values=c("Black","Grey40","Firebrick"))+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_fill_manual("",values = mkColor(df1$Pipeline),guide = "legend")+
  guides(colour = guide_legend(order = 1,nrow = (3)), 
         fill = guide_legend(order = 2,nrow = (4)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  labs(y="Export Capacity or Demand (million bbl/d)",x="Date",
       caption="Source: NEB Data, graph by Andrew Leach.",
       title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep=""))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


df1<-pipe_capacity_data%>% pivot_longer(cols=-c("Year","CER.2021.Evolving","CER.2021.Current","CER.2021.Total.Capacity","CER.2020.Reference","CER.2020.Evolving","NEB.2016.Offtake","CAPP.2019.Offtake","CAPP.2018.Offtake","CAPP.2014.Offtake","TM.Ref.Prod","Western.Refinery.Runs","Enbridge.Bakken.Volumes","CER.2019.Mod"),
                                        values_to = "Capacity",names_to = "Pipeline")


expansions<-c("Trans.Mountain.Expansion","Keystone.XL","Northern.Gateway","Energy.East")
pipe_dreams<-c("Northern.Gateway","Energy.East")
df1<-df1%>%filter(!Pipeline %in% pipe_dreams)%>%
mutate(Pipeline=factor(Pipeline,levels=c(rev(expansions),"Enbridge.Expansions","Keystone","Express","Trans.Mountain","Rangeland/Milk.River","Enbridge.Mainline"))
       )

levels(df1$Pipeline)<-gsub("\\.", " ", levels(df1$Pipeline)) 
levels(df1$Pipeline)[levels(df1$Pipeline)=="Implied Rail"]<-"Capacity Shortfall"

levels(df1$Pipeline)[levels(df1$Pipeline)=="Keystone XL"]<-"Keystone XL (as proposed)"

#my_palette<-c(brewer.pal(7, "Set1"))

#my_palette<-c(brewer.pal(10, "Set3"))

#my_palette<-rev(c(brewer.pal(10, "PuOr")))

my_palette<-brewer.pal(11, "PuOr")[c(6,7,8,5,4,3,2,1)]


pipes<-ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",color="black",size=0.5) +
  #geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, linetype = "test0"),size=1.5,color="black") +
  #geom_line(aes(Year, CER.2019.Mod *6.2929/1000, colour = "test1",linetype = "test1" ),size=1.5)+
  geom_line(aes(Year,CER.2021.Current/1000, linetype = "test2"),size=1.5,color="black") +
  geom_line(aes(Year,CER.2021.Evolving/1000, linetype = "test3"),size=1.5,color="black") +
  scale_linetype_manual("Export demand based on:\n",
                        labels=rev(c("CER (2021) Evolving Policies Case","CER (2021) Current Policies Case","CAPP (2014) Forecast")),
                        values=rev(c("11","33","solid")))+
  guides(colour = guide_legend(order = 1,nrow = (4)), linetype = guide_legend(order = 1,nrow = (4)),
         fill = guide_legend(order = 2,nrow = (4)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  theme(legend.key.width = unit(2,"cm"))+
  #ajl_line()+
  labs(y="Export Capacity or Demand (million bbl/d)",x="",
       #caption="Source: CER and CAPP Data, graph by Andrew Leach.",
       #title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep="")
       NULL
  )
pipes+scale_fill_manual("",values = my_palette,guide = "legend")
ggsave("pipe_capacity_real.png",height = 10,width = 16,dpi = 600)

my_palette<-c("white",grey.colors(n=7,end=0.85,start = 0.3))
pipes+scale_fill_manual("",values = my_palette,guide = "legend")
ggsave("pipe_capacity_real_bw.png",height = 10,width = 16,dpi = 600)
ggsave("pipe_capacity_real_bw.pdf",height = 10,width = 16,dpi = 600)


#AE2016FigureData.xlsx
#French
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("pipe_capacity_real_french.png",height = 1000,width = 1600)
ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack") +
  #geom_area(data=filter(df1,df1$Pipeline!="test"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",alpha=0.8) +
  geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test1" ),size=3) +
  geom_line(aes(Year,CAPP.2018.Offtake*6.2929/1000, colour = "test2" ),size=3) +
  #geom_line(aes(Year,NEB.2016.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test4" ),size=3) +
  scale_colour_manual("",labels=c("P?trole disponible pour l'exportation (ACPP, 2019)","P?trole disponible pour l'exportation (ACPP,2018)","P?trole disponible pour l'exportation (ACPP,2014)"),values=c("Black","Grey40","Firebrick"))+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_fill_manual("",values = mkColor(df1$Pipeline),guide = "legend",labels=pipes_french_real)+
  guides(colour = guide_legend(order = 1,nrow = (3)), 
         fill = guide_legend(order = 2,nrow = (4)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  labs(y="Capacit? ou quantit? pr?vue (mm bar/j)",x="Ann?e",
       title=paste("Pr?visions de la capacit? pipelini?re canadienne d'exportation et d'exportations de p?trole",sep=""),
       caption="Source: Donn?s de l'ONE, figure par Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






ref_runs <- read.xlsx(xlsxFile = "https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/crdrn-hstrcl.xlsx", sheet = "WeeklyRegional", startRow = 8,skipEmptyRows = TRUE,detectDates = TRUE)
names(ref_runs)[1]<-"Date"
names(ref_runs)[2]<-"Year_lag_date"
names(ref_runs)[3]<-"Region"
names(ref_runs)[5]<-"Runs"
names(ref_runs)[6]<-"Capacity Utilization"
names(ref_runs)[7]<-"4 week average"
names(ref_runs)[8]<-"Year Lag 4 week average"
names(ref_runs)[9]<-"YTD Average"
names(ref_runs)[10]<-"YTD Average Year Lag"
ref_runs<-ref_runs[,-4]
ref_runs$Date<-as.Date(ref_runs$Date)

ref_runs$m12_avg<-as.numeric(rollapply(ref_runs$Runs,52,mean,fill=NA,align = c("right")))

ref_runs<- ref_runs %>% group_by(Region) %>%
  mutate(m12_avg=rollapplyr(Runs, 52, mean, partial=TRUE,align = c("right"))) %>% ungroup()

ref_runs<- ref_runs %>% group_by(Date) %>%
  summarize(Canada_runs=sum(Runs)) %>%
  mutate(can_m12_avg=rollapplyr(Canada_runs, 52, mean, partial=TRUE,align = c("right"))) %>% ungroup()%>% 
  left_join(ref_runs,by="Date")



lims=c(min(ref_runs$Date),max(ref_runs$Date)+months(3))
breaks<-seq.Date(min(ref_runs$Date), max(ref_runs$Date)+months(3), by="1 year")

set_png("ref_runs_canada.png")
ggplot(ref_runs) +
  geom_area(aes(Date,Runs*6.2929,fill=Region),position="stack") +
  geom_line(aes(Date,can_m12_avg*6.2929,colour="m12_avg"),size=3) +
  scale_x_date(name=NULL,breaks=breaks, limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
    scale_fill_manual("",values = colors_ua10(),guide = "legend")+
  scale_colour_manual("",labels=c("52 Week Canadian Average"),values=colors_ua10()[4])+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  #scale_y_continuous(expand = c(0, 0)) +
  #scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Weekly Refinery Runs (Thousands of barrels per day)",x="Date",
       title=paste("Canadian Weekly Refinery Runs",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
dev.off()

set_png("ref_runs_west_canada.png")
ggplot(filter(ref_runs,Region=="Western Canada")) +
  geom_area(aes(Date,Runs*6.2929,fill=Region),position="stack") +
  geom_line(aes(Date,m12_avg*6.2929,colour=Region),position="stack",size=3) +
  scale_fill_manual("",values = colors_ua10(),guide = "legend",labels="Weekly Runs")+
  scale_colour_manual("",labels=c("52 Week Western Canadian Average"),values=colors_ua10()[3])+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Weekly Refinery Runs (Thousands of barrels per day)",x="Date",
       title=paste("Western Canadian Weekly Refinery Runs",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
dev.off()

