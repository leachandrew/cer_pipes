library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(RColorBrewer)
library(janitor)
library(cowplot)




#PIPE CAPACITIES
library(openxlsx)

# Energy Futures Supply

if(!file.exists("cer_figs.xlsx"))
  download.file("https://www.cer-rec.gc.ca/en/data-analysis/canada-energy-future/2023/figures.xlsx",destfile = "cer_figs.xlsx",mode="wb")

cer_supply<-read_xlsx("cer_figs.xlsx",sheet="R.33",skip = 5)%>%clean_names()%>%select(-unit,-"total_pipeline_capacity_and_structural_rail")


#PIPE CAPACITIES

#pipe_capacity_data <- read.xlsx(xlsxFile = "C:/Users/aleach/Google Drive/Reports/NEB/EF2016FigureData.xlsx", sheet = "10.7", startRow = 3,skipEmptyRows = TRUE,detectDates = TRUE)
pipe_capacity_data <- read_excel("pipes_capacity.xlsx", sheet = "2020_mods",skip=2)%>%
  rename("Year"=1)%>%clean_names()


pipe_capacity_data <-pipe_capacity_data%>%
  mutate(
    capp_2019_offtake=capp_2019_supply_raw/6.2929- western_refinery_runs_net_burnaby_nwr +enbridge_bakken_volumes)

pipe_capacity_data <- pipe_capacity_data %>% left_join(cer_supply%>%clean_names()%>%
                                                         mutate(across(-year, ~ .x * 0.158987)),
                                                        by = "year")

pipe_capacities<-pipe_capacity_data%>%
  pivot_longer(cols=c("enbridge_mainline","express","trans_mountain","keystone","rangeland_milk_river","keystone_xl","northern_gateway","energy_east"), names_to = "pipeline",values_to =  "capacity")%>%
  mutate(labels=str_to_title(gsub("_"," ",pipeline)),
         labels=gsub("Rangeland Milk River","Rangeland and Milk River",labels),
         labels=gsub("Xl","XL",labels))%>%
          mutate(pipeline= factor(pipeline, levels = pipeline, labels = labels),
                 pipeline=fct_relevel(pipeline,"Express",after=1),
                 pipeline=fct_relevel(pipeline,"Rangeland and Milk River",after=1),
                 pipeline=fct_rev(pipeline)
                 )
          
#levels(pipe_capacities$pipeline)
         
         forecasts<-pipe_capacity_data%>%
  rename_with(~ str_replace(.x, "^total_supply_available_for_export", "cer_2023")) %>%
  pivot_longer(cols=c(capp_2014_offtake,cer_2023_current_measures,s_p_2025), names_to = "forecast",values_to =  "supply")%>%
                 select(year,forecast,supply)%>%
  mutate(supply=case_when(
    forecast=="cer_2019_supply_available"~supply* 0.158987,
    #forecast=="cer_2020_reference"~supply* 0.158987,
    forecast=="cer_2021_current"~supply* 0.158987,
    grepl("cer_2023",forecast)~supply* 1000,
    grepl("s_p_2025",forecast)~supply* 0.158987,
    TRUE~supply),
    labels=gsub("Capp","CAPP",str_to_title(gsub("_"," ",forecast))),
    labels=gsub("Cer","CER",labels),
    labels=gsub("Reference","Reference Case",labels),
    labels=gsub("CER 2019 Current","CER 2019 Current Measures",labels),
    labels=gsub("CER 2021 Current","CER 2021 Current Measures",labels),
    labels=gsub("Supply Available","Current Measures",labels),
    labels=gsub("Offtake","Forecast",labels),
    labels=gsub("S P 2025","S&P 2025 (Approximate)",labels)
    )%>%
  mutate(forecast= factor(forecast, levels = forecast, labels = labels))
  

expansions<-c("Keystone XL","Northern Gateway","Energy East")


my_palette <- colorRampPalette(rev(brewer.pal(11 , "PuOr" )) )(10)[6:10]

ggplot(pipe_capacities) +
  geom_area(data=pipe_capacities%>% filter(!labels %in% expansions), aes(year,capacity*6.2981/1000,fill=pipeline),position="stack",colour="black",size=0.1) +
  geom_line(data=forecasts,aes(year,supply*6.2929/1000, linetype = forecast,colour=forecast),size=1.2)+
  geom_point(data=forecasts%>%filter(year%%5==0),aes(year,supply*6.2929/1000, shape = forecast,colour=forecast),size=2.5)+
  scale_colour_manual("", values = c("black","black","black","black")) +
  scale_shape_manual("", values = c(NA,NA,NA,NA)) +
  scale_linetype_manual("", values = c("22","41","solid","2111")) +
  scale_fill_manual("",values=my_palette,guide = "legend")+
  guides(colour = guide_legend(order = 1,nrow = (3), keywidth = 2.8),
         shape = guide_legend(order = 1,nrow = (3), keywidth = 2.8),
         linetype = guide_legend(order = 1,nrow = (3), keywidth = 2.8), 
         fill = guide_legend(order = 2,nrow = (3)))+
  scale_y_continuous(expand = c(0, 0),breaks = scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0),breaks = scales::pretty_breaks(10)) +
  theme_ps()+theme(plot.margin = margin(t=5,r=15,l=5))+
  labs(y="Export Capacity or Demand (million bbl/d)",x="",
       caption="Source: NEB Data, graph by Andrew Leach.",
       title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep=""))
ggsave("pipes_current.png",width=14,height=7,dpi = 300,bg="white")


ggplot(pipe_capacities) +
  geom_area(data=pipe_capacities%>% filter(!labels %in% expansions), aes(year,capacity*6.2981/1000,fill=pipeline),position="stack",colour="black",size=0.1) +
  geom_line(data=forecasts,aes(year,supply*6.2929/1000, linetype = forecast,colour=forecast),size=1.2)+
  geom_point(data=forecasts%>%filter(year%%5==0),aes(year,supply*6.2929/1000, shape = forecast,colour=forecast),size=2.5)+
  scale_colour_manual("", values = c("black","black","black","black")) +
  scale_shape_manual("", values = c(NA,NA,NA,NA)) +
  scale_linetype_manual("", values = c("22","41","solid","2111")) +
  scale_fill_manual("",values=my_palette,guide = "legend")+
  guides(colour = guide_legend(order = 1,nrow = (2), keywidth = 2.8),
         shape = guide_legend(order = 1,nrow = (2), keywidth = 2.8),
         linetype = guide_legend(order = 1,nrow = (2), keywidth = 2.8), 
         fill = guide_legend(order = 2,nrow = (3)))+
  scale_y_continuous(expand = c(0, 0),breaks = scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0),breaks = scales::pretty_breaks(10)) +
  theme_ps()+theme(plot.margin = margin(t=5,r=15,l=5))+
  labs(y="Export Capacity or Demand (million bbl/d)",x="",
       caption="Source: NEB Data, graph by Andrew Leach.",
       title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep=""))
ggsave("pipes_current.png",width=14,height=7,dpi = 300,bg="white")




my_palette <- colorRampPalette(rev(brewer.pal(11 , "PuOr" )) )(10)[c(2:4,6:10)]
pipe_plot<-ggplot(pipe_capacities) +
  geom_area(data=pipe_capacities, aes(year,capacity*6.2981/1000,fill=pipeline),position="stack",linewidth=0.1,colour="black") +
  geom_line(data=forecasts,aes(year,supply*6.2929/1000, linetype = forecast,colour=forecast),size=1.2)+
  geom_point(data=forecasts%>%filter(year%%5==0),aes(year,supply*6.2929/1000, shape = forecast,colour=forecast),size=2.5)+
  scale_colour_manual("", values = c("black","black","black","black","black")) +
  scale_shape_manual("", values = c(NA,NA,NA,NA,NA)) +
  scale_linetype_manual("", values = c("22","41","11")) +
  scale_fill_manual("",values=my_palette,guide = "legend")+
  guides(colour = guide_legend(order = 1,nrow = (3), keywidth = 2.2),
         shape = guide_legend(order = 1,nrow = (3), keywidth = 2.2),
         linetype = guide_legend(order = 1,nrow = (3), keywidth = 2.2), 
         fill = guide_legend(order = 2,nrow = (3)))+
  scale_y_continuous(expand = c(0, 0),breaks = scales::pretty_breaks()) +
  scale_x_continuous(expand = c(0, 0),breaks = scales::pretty_breaks(10)) +
  theme_ps()+theme(plot.margin = margin(t=5,r=15,l=5))+
  
  
  labs(y="Export Capacity or Demand (million bbl/d)",x="",
       caption="Source: NEB Data, graph by Andrew Leach.",
       title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep=""))


pipe_plot+
  labs(title=NULL,
       caption=NULL
  )       +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    
    plot.title = element_text(size = 10, color = "black"),
    axis.title  = element_text(size = 10, color = "black"),
    
    # x-axis labels shifted slightly down from ticks
    text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black",
                               margin = margin(t = 5, b = 5)),
    
    # y-axis labels unchanged
    axis.text.y = element_text(size = 12, color = "black"),
    
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(t = 5, r = 20, b = 5, l = 5)
  )



pipe_plot+
  labs(title=NULL,
       caption=NULL
  )       +
  theme_irpp(base_size = 16)+
  expand_limits(y=8.1)

ggsave("pipes_proposed.png",width=13,height=7,dpi = 600,bg="white")

ggsave("pipes_proposed.svg",width=6.5,height=5,dpi = 600,bg="white")



df1<-pipe_capacity_data%>%
  pivot_longer(cols=c("Enbridge.Mainline","Express","Trans.Mountain","Keystone","Rangeland/Milk.River","Keystone.XL","Trans.Mountain.Expansion","Northern.Gateway","Energy.East"), 
               names_to = "Pipeline",values_to =  "Capacity")


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
  geom_line(aes(Year,CER.2021.Current/1000, linetype = "test1"),size=1.5,color="black") +
  geom_line(aes(Year,CER.2021.Evolving/1000, linetype = "test2"),size=1.5,color="black") +
  geom_line(aes(Year,total_supply_available_for_export_current_measures, linetype = "test3"),size=1.5,color="black") +
  geom_line(aes(Year,total_supply_available_for_export_canada_net_zero, linetype = "test4"),size=1.5,color="black") +
  geom_line(aes(Year,total_supply_available_for_export_global_net_zero, linetype = "test5"),size=1.5,color="black") +
  scale_linetype_manual("Export demand based on:\n",
                        labels=rev(c("CER (2023) Global Net-Zero","CER (2023) Canada Net-Zero","CER (2023) Current Measures","CER (2021) Evolving Policies Case","CER (2021) Current Policies Case","CAPP (2014) Forecast")),
                        values=rev(c("31","3111","22","11","dotted","solid")))+
  guides(colour = guide_legend(order = 1,nrow = (3)), linetype = guide_legend(order = 1,nrow = (3)),
         fill = guide_legend(order = 2,nrow = (4)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  theme(legend.key.width = unit(1.9,"cm"))+
  #ajl_line()+
  labs(y="Export Capacity or Demand (million bbl/d)",x="",
       #caption="Source: CER and CAPP Data, graph by Andrew Leach.",
       #title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep="")
       NULL
  )
pipes+scale_fill_manual("",values = my_palette,guide = "legend")
ggsave("pipe_capacity_real.png",height = 10,width = 16,dpi = 600)

my_palette<-c("white",grey.colors(n=7,end=0.2,start = 0.9))
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

