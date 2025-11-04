library(tidyverse)
library(lubridate)
library(janitor)
library(viridis)
library(readxl)
library(RColorBrewer)

get_pipe_data<-function(){
names<-c("Alliance","Cochin","Enbridge-Mainline","Norman-Wells",
        "Keystone","MNP","Trans-Mountain","TQM","tcpl-mainline","Westcoast","ngtl")
#pipe<-"Keystone"
#https://www.cer-rec.gc.ca/open/energy/throughput-capacity/tcpl-mainline-throughput-and-capacity.csv
#https://www.neb-one.gc.ca/open/energy/throughput-capacity/nbridge-mainline-throughput-and-capacity-dataset.csv
#https://open.canada.ca/data/en/dataset/dc343c43-a592-4a27-8ee7-c77df56afb34/resource/4db7bc7c-d9cc-468b-8130-e799b13c69e8
#https://www.cer-rec.gc.ca/open/energy/throughput-capacity/keystone-throughput-and-capacity.csvnames<-c("Enbridge-mainline","Keystone","Trans-Mountain")

  data_store <- list()

for(pipe in names){
  #file_name<-paste("https://www.neb-one.gc.ca/open/energy/throughput-capacity/",pipe,"-throughput-and-capacity-dataset.csv",sep="")
  file_name<-paste("https://www.cer-rec.gc.ca/open/energy/throughput-capacity/",pipe,"-throughput-and-capacity.csv",sep="") 
  
  download.file(file_name,"neb-data.csv",mode="wb")
  
   
  
  var_name<-paste(pipe,"_data",sep="")
  pipe_data <- read_csv(file = "neb-data.csv")%>%clean_names()%>%mutate(pipe_name=pipe)
  data_store[[pipe]]<-pipe_data
}

bind_rows(data_store)
}

#pipe_data<-get_pipe_data()

save(pipe_data,file = "pipe_data.Rdata")
load(file = "pipe_data.Rdata")

#ALL EXPORTS

oil_export_lines<-c("Enbridge-Mainline","Keystone","MNP","Trans-Mountain")
export_points <-c("ex-Gretna","Sumas","Burnaby","Westridge","International boundary at or near Haskett, Manitoba")

oil_export<-pipe_data %>% filter(pipe_name %in% oil_export_lines,key_point %in% export_points,product!="total")
#test<-
  oil_export%>% filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),pipeline=factor(str_to_title(pipeline)))%>%
    mutate(
         product=fct_recode(product,"Light"="Domestic Light / Ngl","Light"="Domestic Light" ),
         product=fct_recode(product,"Heavy"="Domestic Heavy"),
         product=fct_recode(product,"Refined Products"="Refined Petroleum Products"),
         product=fct_recode(product,"Light"="Foreign Light"),
         product=fct_relevel(product,"Light"),
         product=fct_relevel(product,"Refined Products"),
         pipeline=fct_recode(pipeline,"Enbridge Mainline"="Enbridge Canadian Mainline System\r\n"),
         pipeline=fct_recode(pipeline,"Keystone"="Keystone Pipeline"),
         pipeline=fct_recode(pipeline,"Trans Mountain"="Trans Mountain Pipeline"),
         label=paste(pipeline," (",product,")",sep=""))%>%
      
  group_by(date,pipeline,product,label)%>%
  summarize(throughput=sum(throughput_1000_m3_d))%>%
  #group_by(date)%>%
  #mutate(total_capacity=sum(capacity))%>%
  ggplot(aes(date,throughput,group = interaction(product,pipeline),fill=pipeline,alpha=product)) +
  geom_area(position = "stack",color="black",linewidth=.25) +
  geom_area(position = "stack",color="black",linewidth=.25) +
  scale_alpha_manual("Grades denoted by fill shading, e.g. for Trans Mountain:",values=c(0.25,0.6,1))+
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     sec.axis = sec_axis( trans=~.*6.289810770, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
    expand_limits(y=800)+
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=0.8,end=1,option = "E",direction=1)[1]),order = 10) ,
         fill = guide_legend(order = 1) )+
  theme_ps() +
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
        legend.box = "vertical",
        legend.title = element_text(size = 12),
        
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       title=paste("Canadian Major Export Pipeline Shipments by Product",sep=""),
       caption="Source: CER Data for Enbridge Mainline (ex-Gretna), Keystone (MB border), and TransMountain, graph by Andrew Leach.")
ggsave("cer_oil.png",dpi=300,bg="white",width = 15,height = 8)





oil_export%>% filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         label=paste(pipeline," (",product,")",sep=""))%>%
  group_by(date,product)%>%
  summarize(throughput=sum(throughput_1000_m3_d))%>%
  ggplot(aes(date,throughput,group = product,fill=product)) +
  geom_area(position = "stack",color="black",linewidth=.25) +
  scale_alpha_manual("Grades denoted by fill shading, e.g. for Enbridge Mainline:",values=c(.7,.5,1,.3))+
  scale_fill_manual("",values=viridis(n=4,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     sec.axis = sec_axis( trans=~.*1/.16, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=.8,end=0,option = "E",direction=-1)[1]),order = 10) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        legend.box = "vertical",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Product",sep=""),
       caption="Source: CER Data for Enbridge Mainline (ex-Gretna), Keystone (MB border), and TransMountain (all delivery points), graph by Andrew Leach.")
ggsave("cer_oil_grade.png",dpi=300,bg="white",width = 15,height = 8)

#test<-
oil_export%>% filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         label=paste(pipeline," (",product,")",sep=""))%>%
  group_by(date,pipeline)%>%
  summarize(throughput=sum(throughput_1000_m3_d))%>%
  ggplot(aes(date,throughput,group = pipeline,fill=pipeline)) +
  geom_area(position = "stack",color="black",linewidth=.25) +
  scale_alpha_manual("Grades denoted by fill shading, e.g. for Enbridge Mainline:",values=c(.7,1,.3))+
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     sec.axis = sec_axis( trans=~.*1/.16, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=.8,end=0,option = "E",direction=-1)[1]),order = 10) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        legend.box = "vertical",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Pipeline",sep=""),
       caption="Source: CER Data for Enbridge Mainline (ex-Gretna), Keystone (MB border), and TransMountain (all delivery points), graph by Andrew Leach.")
ggsave("cer_oil_pipe.png",dpi=300,bg="white",width = 15,height = 8)




pipe_data %>% filter(pipe_name == "Enbridge-Mainline")%>% filter(key_point =="Into-Sarnia")%>% 
  filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         label=paste(pipeline," (",product,")",sep=""))%>%
  
  group_by(date,pipeline,product,label)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=last(available_capacity_1000_m3_d))%>%
  ggplot(aes(date,throughput,group = product,fill=product)) +
  geom_area(position = "stack",,color="black",linewidth=.25) +
  geom_line(aes(date,capacity,colour="Capacity"),linewidth=.85,lty="21") +
  scale_alpha_manual("Grades denoted by fill shading, e.g. for Enbridge Mainline:",values=c(.7,1,.3))+
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_colour_manual("",values="black")+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     
                     sec.axis = sec_axis( trans=~.*6.2898, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=.8,end=0,option = "E",direction=-1)[1]),order = 10) ,
         colour= guide_legend(order = 1),fill = guide_legend(order = 2) )+
  theme_classic() +
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
        legend.box = "horizontal",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Product",sep=""),
       caption="Source: CER Data for Enbridge Mainline (into-Sarnia), graph by Andrew Leach.")
ggsave("enb_sarnia.png",dpi=300,bg="white",width = 15,height = 8)




enb_capacity<-
  pipe_data %>% filter(pipe_name == "Enbridge-Mainline")%>% filter(key_point =="ex-Gretna")%>% 
  filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         label=paste(pipeline," (",product,")",sep=""))%>%
  
  group_by(date,pipeline,product,label)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=last(available_capacity_1000_m3_d))


sarnia_capacity<-
  pipe_data %>% filter(pipe_name == "Enbridge-Mainline")%>% filter(key_point =="Into-Sarnia")%>% 
  filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         label=paste(pipeline," (",product,")",sep=""))%>%
  
  group_by(date,pipeline,product,label)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=last(available_capacity_1000_m3_d))%>%
  ungroup()%>%
  mutate(pipeline=fct_recode(pipeline,"Enbridge Mainline (Sarnia, ON)"="Enbridge Canadian Mainline system\r\n"))

keystone_capacity<-
  pipe_data %>% filter(pipe_name == "Keystone")%>% 
  filter(key_point =="International boundary at or near Haskett, Manitoba",product!="system",,product!="Total")%>% 
  filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         label=paste(pipeline," (",product,")",sep=""))%>%
  
  group_by(date,pipeline,product,label)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=pmax(available_capacity_1000_m3_d))
  
  

tm_capacity<-
  pipe_data %>% filter(pipe_name == "Trans-Mountain",!is.na(product),product!="Total")%>% 
  filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         )%>%
  group_by(date,pipeline,product)%>%
  summarize(throughput=sum(throughput_1000_m3_d))%>%
  left_join(
    pipe_data %>% filter(pipe_name == "Trans-Mountain",key_point=="system")%>%
      group_by(date,pipeline)%>%
      summarize(capacity=available_capacity_1000_m3_d,)
  )

capacity=bind_rows(enb_capacity,sarnia_capacity,keystone_capacity,tm_capacity)%>%
  mutate(pipeline=factor(str_to_title(pipeline)))%>%
  mutate(
    pipeline=fct_recode(pipeline,"Enbridge Mainline (Gretna, MB)"="Enbridge Canadian Mainline System\r\n"),
    pipeline=fct_recode(pipeline,"Enbridge Mainline (Sarnia, ON)"="Enbridge Mainline (Sarnia, On)"),
    pipeline=fct_recode(pipeline,"Keystone"="Keystone Pipeline"),
    pipeline=fct_recode(pipeline,"Trans Mountain"="Trans Mountain Pipeline"),
    product=fct_recode(product,"Refined Products"="Refined Petroleum Products"),
    pipeline=fct_relevel(pipeline,"Keystone",after = 2),
    )

capacity%>%
  mutate(year=year(date))%>%
  filter(year==2025)%>%
  group_by(pipeline,year,product)%>%
  summarize(throughput=max(throughput),
            capacity=max(capacity))%>%
  group_by(pipeline,year)%>%
  summarize(throughput=sum(throughput,na.rm = T),
            capacity=max(capacity))%>%
  ungroup()%>%
  mutate(util=throughput/capacity,
         total=sum(throughput)/sum(capacity))




facet_plot<-
  capacity%>%
  ggplot() +
  geom_area(aes(date,throughput,group = product,fill=product),
            position = "stack",color="black",linewidth=.05) +
  facet_wrap(~pipeline)+
  geom_line(data=capacity %>% group_by(date,pipeline) %>% summarize(capacity=first(capacity)),
              aes(date,capacity,colour="Capacity"),linewidth=.5,lty="solid") +
  scale_alpha_manual("Grades denoted by fill shading, e.g. for Enbridge Mainline:",values=c(.7,1,.3))+
  scale_fill_manual("",values=viridis(n=4,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_colour_manual("",values="black")+
  scale_x_date(name=NULL,date_breaks = "2 year", date_labels =  "%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     
                     sec.axis = sec_axis( trans=~.*6.2898, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=.8,end=0,option = "E",direction=-1)[1]),order = 10) ,
         colour= guide_legend(order = 1),fill = guide_legend(order = 2) )+
  theme_ps_grid() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10),angle=90),
        axis.title = element_text(size = 12),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        legend.box = "horizontal",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       #title=paste("Canadian Pipeline Shipments by Product",sep=""),
       #caption="Source: CER Data for Enbridge Mainline (ex-Gretna), Keystone (MB border), and TransMountain (all delivery points ex Kamloops terminal), graph by Andrew Leach."
       )
facet_plot

facet_plot+
  labs(title=NULL,
       caption=NULL
  )       +
  theme_irpp()+
  theme(axis.text.x = element_text(margin = margin(t = 2),angle=90))

ggsave("pipes_facet.png",dpi=300,bg="white",width = 6.2,height = 5)

ggsave("pipes_facet.svg",dpi=300,bg="white",width = 6.2,height = 5)




pipe_data %>% filter(pipe_name == "Trans-Mountain", product!="all",key_point!="system")%>% 
  filter(year>=2007)%>%
  mutate(product=factor(str_to_title(product)),
         product=fct_recode(product,"Domestic Light"="Domestic Light / Ngl" ),
         product=fct_relevel(product,"Domestic Light"),
         )%>%
  group_by(date,key_point,product)%>%
  summarize(throughput=sum(throughput_1000_m3_d))%>%
  ggplot(aes(date,throughput,group = product,fill=product)) +
  geom_area(position = "stack",color="black",linewidth=.25) +
  facet_wrap(~key_point,ncol = 1, scales = "free_y")+
  #scale_alpha_manual("Grades denoted by fill shading, e.g. for Burnaby:",values=c(.7,1,.3))+
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     
                     sec.axis = sec_axis( trans=~.*1/.16, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=.8,end=0,option = "E",direction=-1)[1]),order = 10) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        legend.box = "vertical",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Product",sep=""),
       caption="Source: CER Data for TransMountain (all deliveries), graph by Andrew Leach.")
ggsave("tm_prods.png",dpi=300,bg="white",width = 15,height = 8)



pipe_data %>% filter(pipe_name == "Trans-Mountain", product!="all",key_point!="system")%>% 
  filter(year>=2007)%>%
  mutate(key_point=as_factor(key_point),
         key_point=fct_recode(key_point,"Sumas (US)"="Sumas" ),
         key_point=fct_recode(key_point,"Westridge (port)"="Westridge" ),
         key_point=fct_relevel(key_point,"Westridge (port)"),
  )%>%
  group_by(date,key_point)%>%
  summarize(throughput=sum(throughput_1000_m3_d))%>%
  ggplot(aes(date,throughput,group = key_point,fill=key_point)) +
  geom_area(position = "stack",color="black",linewidth=.25) +
  #facet_wrap(~key_point,ncol = 1, scales = "free_y")+
  #scale_alpha_manual("Grades denoted by fill shading, e.g. for Burnaby:",values=c(.7,1,.3))+
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),
                     
                     sec.axis = sec_axis( trans=~.*1/.16, name="Shipments (Monthly, Thousands of Barrels per Day)")) +
  guides(alpha = guide_legend(override.aes = list(fill=viridis(n=3,alpha=1,begin=.8,end=0,option = "E",direction=-1)[1]),order = 10) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        legend.box = "vertical",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Thousands of Cubic Metres per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Product",sep=""),
       caption="Source: CER Data for TransMountain (all deliveries), graph by Andrew Leach.")
ggsave("tm_points.png",dpi=300,bg="white",width = 15,height = 8)






#pipe and capacity data
options(scipen = 99)
#test<-
pipe_data %>% filter(pipe_name == "tcpl-mainline",key_point=="Prairies")%>% 
  #filter(year>=2007)%>%
  #group_by(date,key_point,product)%>%
  #summarize(throughput=sum(throughput_1000_m3_d),capacity=mean(capacity_1000_m3_d))%>%
  ggplot(aes(date,throughput_1000_m3_d/1000,fill="Throughput")) +
  geom_area(position = "stack") +
  geom_line(aes(date,capacity_1000_m3_d/1000,color="Capacity"),linewidth=.75) +
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_color_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1)[2])+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(colour = guide_legend(order = 2) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        #legend.box = "vertical",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Million Cubic Metres Per Day)",x="Date",
       title=paste("Canadian Mainline Gas Shipments and Capacity",sep=""),
       caption="Source: CER Data for TCPL Mainline (Prairies), graph by Andrew Leach.")
ggsave("tcpl_main.png",dpi=300,bg="white",width = 15,height = 8)



pipe_data %>% filter(pipe_name == "ngtl",key_point=="Upstream of James River")%>% 
  #filter(year>=2007)%>%
  group_by(date,key_point)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=mean(capacity_1000_m3_d))%>%
  ggplot(aes(date,throughput,fill="Throughput")) +
  geom_area(position = "stack") +
  geom_line(aes(date,capacity,color="Capacity"),linewidth=.75) +
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_color_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1)[2])+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(colour = guide_legend(order = 2) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        #legend.box = "vertical",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Gigajoules Per Day)",x="Date",
       title=paste("Canadian Mainline Gas Shipments and Capacity",sep=""),
       caption="Source: CER Data for NGTL System (Upstream of James River), graph by Andrew Leach.")
ggsave("ngtl_james.png",dpi=300,bg="white",width = 15,height = 8)


pipe_data %>% filter(pipe_name == "ngtl",key_point=="West Gate")%>% 
  #filter(year>=2007)%>%
  group_by(date,key_point)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=mean(capacity_1000_m3_d))%>%
  ggplot(aes(date,throughput,fill="Throughput")) +
  geom_area(position = "stack") +
  geom_line(aes(date,capacity,color="Capacity"),linewidth=.75) +
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_color_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1)[2])+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(colour = guide_legend(order = 2) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        #legend.box = "vertical",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Gigajoules Per Day)",x="Date",
       title=paste("Canadian Mainline Gas Shipments and Capacity",sep=""),
       caption="Source: CER Data for NGTL System (West Gate), graph by Andrew Leach.")
ggsave("ngtl_west.png",dpi=300,bg="white",width = 15,height = 8)


pipe_data %>% filter(pipe_name == "ngtl",key_point=="East Gate")%>% 
  #filter(year>=2007)%>%
  group_by(date,key_point)%>%
  summarize(throughput=sum(throughput_1000_m3_d),capacity=mean(capacity_1000_m3_d))%>%
  ggplot(aes(date,throughput,fill="Throughput")) +
  geom_area(position = "stack") +
  geom_line(aes(date,capacity,color="Capacity"),linewidth=.75) +
  scale_fill_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1))+
  scale_color_manual("",values=viridis(n=3,alpha=1,begin=.7,end=0,option = "E",direction=-1)[2])+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(colour = guide_legend(order = 2) ,
         fill = guide_legend(order = 1) )+
  theme_classic() +
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
        #legend.box = "vertical",
        #legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Shipments (Monthly, Gigajoules Per Day)",x="Date",
       title=paste("Canadian Mainline Gas Shipments and Capacity",sep=""),
       caption="Source: CER Data for NGTL System (East Gate), graph by Andrew Leach.")
ggsave("ngtl_east.png",dpi=300,bg="white",width = 15,height = 8)


# Apportionment

file_name<-"https://www.cer-rec.gc.ca/open/energy/throughput-capacity/apportionment.csv"



#inv_file<-"http://ecccdocs.techno-science.ca/documents/ECCC_STB_SRAD_GHG_ECON_NS_1990-2016_prelim_EN.xlsx"
download.file(file_name,"neb-oil-app-data.csv")
oil_apportion <- read.csv(file = "neb-oil-app-data.csv")%>% clean_names()%>%
  rename(apportionment=apportionment_percentage,
         orig_noms=original_nominations_1000_m3_d)%>%
  mutate(date=ymd(date),
         apportionment=apportionment*100,
  )

#test<-
  oil_apportion%>%#filter(pipeline %in% c("Trans Mountain Pipeline"))%>%
  mutate(pipeline=str_to_lower(pipeline),pipeline=gsub("\n","",pipeline),pipeline=gsub(" system","",pipeline))%>%
  filter(pipeline %in% c("enbridge canadian mainline","trans mountain pipeline","keystone pipeline"))%>%
  #filter(!is.na(apportionment))%>%
  filter(!(pipeline =="enbridge canadian mainline" & key_point=="system"))%>%
  group_by(date,pipeline)%>%
  summarize(apportionment=max(apportionment))%>%
  ggplot() +
  geom_line(aes(date,apportionment,group = str_to_title(pipeline),colour=str_to_title(pipeline)),size=0.85) +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="E",begin=0,end=0.7)+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=100)+
  theme_ps_grid() +
  
  labs(y="Approtionment share (%)",x="Date",
       title=paste("Canadian Common Carrier Pipeline Apportionment",sep=""),
       caption="Source: CER Data, graph by Andrew Leach.")
ggsave("apportioned_pipes.png",dpi=300,bg="white",width = 15,height = 8)


#test<-
oil_apportion%>%#filter(pipeline %in% c("Trans Mountain Pipeline"))%>%
  filter((str_to_lower(pipeline) == c("enbridge canadian mainline")& grepl("Kerrobert - ",key_point))|
           (str_to_lower(pipeline) == "keystone pipeline")|
           (str_to_lower(pipeline) == "trans mountain pipeline")     )%>%
  mutate(apportionment=coalesce(apportionment,0))%>%
  group_by(date,pipeline)%>%
  summarize(avg_app=sum(apportionment/100*orig_noms)/sum(orig_noms,na.rm=T)*100
         )%>%
  mutate(
    key_point=ifelse(pipeline=="Enbridge Canadian Mainline","Enbridge Canadian Mainline, Kerrobert (weighted avg.)",pipeline))%>%
I()%>%
  
    ggplot() +
  geom_col(aes(date,avg_app),fill="dodgerblue",linewidth=0.2) +
  #geom_point(size=1) +
  scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_x_date(name=NULL,date_breaks = "2 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  #expand_limits(y=100)+
  theme_classic() +
  facet_wrap(~key_point)+
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
  labs(y="Approtionment share (%)",x="Date",
       title=paste("Canadian Common Carrier Pipeline Apportionment",sep=""),
       caption="Source: CER Data, graph by Andrew Leach.")
ggsave("apportioned_points.png",dpi=300,bg="white",width = 15,height = 8)







ggplot(utilization,aes(Date,utilization,group = Pipeline.Name,colour = Pipeline.Name)) +
  geom_line() +
  
  scale_color_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Product",sep=""),
       caption="Source: NEB Data for Enbridge Mainline, Keystone, TransMountain, graph by Andrew Leach.")




df1<-test %>% group_by(Date,Product,Pipeline.Name) %>% 
  summarise(bbls=sum(Throughput)*6.2929)

lims=c(min(df1$Date),max(df1$Date)+months(3))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("pipe_products.png",height = 800)
ggplot(df1,aes(Date,bbls,group = Product,colour=Product,fill=Product)) +
  geom_area(position = "stack") +
  facet_wrap(~Pipeline.Name,ncol = 1, scales = "free")+
  
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b%y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("Canadian Pipeline Shipments by Product",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#Month	Year	Corporate.Entity	Pipeline.Name	Key.Point	Latitude	Longitude	Direction.of.Flow	Trade.Type	Product	Throughput..1000.m3.d.	Nameplate.Capacity..1000.m3.day.	Capacity..1000.m3.d.	Reason.for.variance.from.nameplate.capacity	Date
#Year	Month	Corporate.Entity	Pipeline.Name	  Key.Point	Latitude	Longitude	Direction.of.Flow	Trade.Type	Product	Throughput.1000m3.day	Capacity..1000.m3.d.	Reason.for.variance.from.nameplate.capacity	Date	Nameplate.Capacity..1000.m3.day.
#Month	Year	Corporate.Entity	Pipeline.Name	Key.Point	Latitude	Longitude	Direction.of.Flow	Trade.Type	Product	Throughput.1000.m3.d	Capacity..1000.m3.d.	Reason.for.variance.from.nameplate.capacity	Date	Nameplate.Capacity..1000.m3.day.



#Trans_Mtn_Data$Date<-as.Date(paste(Trans_Mtn_Data$Year,Trans_Mtn_Data$Month,1,sep="-"))
df1<-`Trans-Mountain_data` %>% group_by(Date,Product) %>% 
  summarize(bbls=sum(Throughput)*6.2929)
lims=c(min(df1$Date),max(df1$Date)+months(3))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("tm_products.png")
ggplot(df1,aes(Date,bbls,group = Product,colour=Product,fill=Product)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("TransMountain Shipments by Product",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#Trans_Mtn_Data$Date<-as.Date(paste(Trans_Mtn_Data$Year,Trans_Mtn_Data$Month,1,sep="-"))
df1<-`Trans-Mountain_data` %>% group_by(Date,Product,Key.Point) %>% 
  summarize(bbls=sum(Throughput)*6.2929)
lims=c(min(df1$Date),max(df1$Date)+months(3))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("tm_deliveries.png")
ggplot(df1,aes(Date,bbls,group = Product,colour=Product,fill=Product)) +
  geom_area(position = "stack") +
  facet_wrap(~Key.Point,ncol = 1)+
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("TransMountain Shipments by Product",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


df1<-`Enbridge-mainline_data` %>% 
  filter(Key.Point=="into-Sarnia") %>%
  group_by(Date,Product) %>% summarize(bbls=sum(Throughput)*6.2929) %>% mutate(year=year(Date)) %>%
  group_by(year,Product) %>% mutate(annual_avg=mean(bbls))
lims=c(min(df1$Date),max(df1$Date)+months(3))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("enb_sarnia.png")
ggplot(df1,aes(Date,bbls,group = Product,colour=Product,fill=Product)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("Enbridge Mainline Shipments into Sarnia by Product",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



df1<-`Enbridge-mainline_data` %>% 
  filter(Key.Point=="ex-Gretna") %>%
  group_by(Date,Product) %>% summarize(bbls=sum(Throughput)*6.2929)
lims=c(min(df1$Date),max(df1$Date)+months(3))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("enb_exports.png")
ggplot(df1,aes(Date,bbls,group = Product,colour=Product,fill=Product)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("Enbridge Mainline Shipments ex-Gretna by Product",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



df1<-`Keystone_data` %>% 
  group_by(Date,Product) %>% summarize(bbls=sum(Throughput)*6.2929)
lims=c(min(df1$Date),max(df1$Date)+months(3))
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("keystone_products.png")
ggplot(df1,aes(Date,bbls,group = Product,colour=Product,fill=Product)) +
  geom_area(position = "stack") +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_fill_viridis("",discrete=TRUE,option="D")+   
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "1 year", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
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
  labs(y="Shipments (Monthly, Thousands of Barrels per Day)",x="Date",
       title=paste("Keystone Pipeline Shipments by Product",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#crude_by_rail_exports
#https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls
#require(xlsx)
#oil_by_rail <- xlsx::read.xlsx("https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls",sheetName = "CrudeOilExportsByRail")
download.file("https://www.cer-rec.gc.ca/en/data-analysis/energy-commodities/crude-oil-petroleum-products/statistics/canadian-crude-oil-exports-rail-monthly-data.xlsx", destfile="cndncrdlxprtsrl-eng.xlsx", mode = "wb")

oil_by_rail<-read_excel("cndncrdlxprtsrl-eng.xlsx", sheet = NULL, range = NULL, col_names = TRUE,
           col_types = NULL, na = "", trim_ws = TRUE, skip = 7)
drops <- c("X__1","X__2","X__3")
oil_by_rail<-oil_by_rail[ , !(names(oil_by_rail) %in% drops)]



#oil_by_rail <- xlsx::read.xlsx("cndncrdlxprtsrl-eng.xls",sheetName = "CrudeOilExportsByRail")
oil_by_rail$mth_num<-match(oil_by_rail$Month,month.name)
oil_by_rail$day<-days_in_month(oil_by_rail$mth_num)
oil_by_rail<- oil_by_rail %>% fill(Year)

oil_by_rail$Date<-as.Date(paste(oil_by_rail$Year,"-",oil_by_rail$mth_num,"-",oil_by_rail$day,sep = ""))


df1<-oil_by_rail %>% filter()

png<-1
lims=c(min(df1$Date),max(df1$Date)+months(12))
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("rail_exports.png")
ggplot(df1) +
  #geom_line(aes(Date,`Volume (bbl)`/days_in_month(mth_num)),size=1.75) +
  geom_col(aes(Date,`Volume (bbl)`/days_in_month(mth_num)/1000),fill=colors_ua10()[1],colour=colors_ua10()[1]) +
  #geom_point(size=1) +
  scale_color_viridis("",discrete=TRUE,option="D")+
  scale_x_date(name=NULL,date_breaks = "6 months", limits=lims,date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0)) +
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
  labs(y="Export Volumes (1000 bbl/d)",x="Date",
       title=paste("Canadian Oil Exports by Rail",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



#CN AND CP RAIL

#workbook has sheets from 2005 through present


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}



cn_AAR_file<-"https://www.cn.ca/-/media/Files/Investors/Investor-Performance-Measures/CN-AAR-WEBSUMMARY.xlsx?la=en&hash=B45CF24FE40477D08F902D8E3E2B28CB4ECB70B4"
download.file(cn_AAR_file,"cn_aar.xlsx",mode="wb")
#cn_sheets<-getSheetNames("cn_aar.xlsx")[-1]
cn_AAR_wb<-loadWorkbook("cn_aar.xlsx")
cn_sheets<-names(cn_AAR_wb)[-1]
mysheets <- read_excel_allsheets("cn_aar.xlsx")

test_df<-data.frame(mysheets[2],stringsAsFactors = F)


https://s21.q4cdn.com/736796105/files/doc_downloads/key-metrics/weekly/2019/03/02/CP-53-Week-Railway-Performance-Report.xlsx
https://s21.q4cdn.com/736796105/files/doc_downloads/key-metrics/weekly/2019/03/02/CP-Weekly-RTMs-and-Carloads.xlsx

library(openxlsx)

# Energy Futures Supply

if(!file.exists("cer.figs.xlsx"))
   download.file("https://www.cer-rec.gc.ca/en/data-analysis/canada-energy-future/2023/figures.xlsx",destfile = "cer_figs.xlsx",mode="wb")

cer_supply<-read_xlsx("cer_figs.xlsx",sheet="R.33",skip = 5)%>%clean_names()%>%select(-unit,-"total_pipeline_capacity_and_structural_rail")


#PIPE CAPACITIES

#pipe_capacity_data <- read.xlsx(xlsxFile = "C:/Users/aleach/Google Drive/Reports/NEB/EF2016FigureData.xlsx", sheet = "10.7", startRow = 3,skipEmptyRows = TRUE,detectDates = TRUE)
pipe_capacity_data <- read.xlsx(xlsxFile = "pipes_capacity.xlsx", sheet = "2020_mods", startRow = 3,skipEmptyRows = TRUE,detectDates = TRUE)
names(pipe_capacity_data)[1]<-"Year"
names(pipe_capacity_data)[grep("Western.Refinery.Runs.Net.Burnaby",names(pipe_capacity_data))]<-"Western.Refinery.Runs"
#names(pipe_capacity_data)[grep("2016.NEB.Western.Canadian.Pipeline.Demand",names(pipe_capacity_data))]<-"2016.NEB.Western.Canadian.Pipeline.Demand"
pipe_capacity_data$`2014.CAPP.Supply.Raw`<-NULL
pipe_capacity_data$`2018.CAPP.Supply.Raw`<-NULL

pipe_capacity_data$`CAPP.2019.Offtake`<-pipe_capacity_data$CAPP.2019.Supply.Raw/6.2929- pipe_capacity_data$Western.Refinery.Runs+pipe_capacity_data$Enbridge.Bakken.Volumes
pipe_capacity_data$`2019.CAPP.Supply.Raw`<-NULL

pipe_capacity_data$CER.2019.Supply.Available<-NULL

pipe_capacity_data <- pipe_capacity_data %>% left_join(cer_supply,by=c("Year"="year"))

df1<-pipe_capacity_data%>%pivot_longer(cols=c("Enbridge.Mainline","Express","Trans.Mountain","Keystone","Rangeland/Milk.River","Keystone.XL","Trans.Mountain.Expansion","Northern.Gateway","Energy.East"), names_to = "Pipeline",values_to =  "Capacity")
expansions<-c("Keystone.XL","Trans.Mountain.Expansion","Northern.Gateway","Energy.East")
df1<-filter(df1,!(Pipeline %in% expansions))
df1$Pipeline<-factor(df1$Pipeline,levels=
      c("Implied.Rail","Enbridge.Expansions","Keystone","Express","Trans.Mountain","Rangeland/Milk.River","Enbridge.Mainline"))

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


slide_theme<-function(){
  return( theme(panel.border = element_blank(),
                panel.grid = element_blank(),
                panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
                axis.line.x = element_line(color = "gray"),
                axis.line.y = element_line(color = "gray"),
                axis.text = element_text(size = 16),
                axis.text.x = element_text(margin = margin(t = 15)),
                axis.text.y = element_text(margin = margin(r = 15)),
                axis.title = element_text(size = 16),
                #axis.label.x = element_text(size=20,vjust=+5),
                plot.subtitle = element_text(size = 16,hjust=0.5),
                plot.title = element_text(size = 16,hjust=0.5),
                plot.caption = element_text(face="italic",size = 12,hjust=0),
                legend.key.width=unit(2,"line"),
                legend.position = "bottom",
                #legend.direction = "horizontal",
                #legend.box = "horizontal",
                legend.title = element_text(size = 14),
                legend.text = element_text(size = 12),
                plot.margin=unit(c(0.25,.75,0.25,0.25),"cm")
  )
  )
}




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


df1<-melt(pipe_capacity_data,id=c("Year","CAPP.2019.Supply.Raw","NEB.2016.Offtake","CAPP.2019.Offtake","CAPP.2018.Offtake","CAPP.2014.Offtake"), value.name = "Capacity",variable.name = "Pipeline")

#expansions<-c("Keystone.XL","Trans.Mountain.Expansion","Northern.Gateway","Energy.East")
expansions<-c("Trans.Mountain.Expansion","Keystone.XL","Northern.Gateway","Energy.East")
df1$Pipeline<-factor(df1$Pipeline,levels=c("Implied.Rail",rev(expansions),"Enbridge.Expansions","Keystone","Express","Trans.Mountain","Rangeland/Milk.River","Enbridge.Mainline"))
levels(df1$Pipeline)<-gsub("\\.", " ", levels(df1$Pipeline)) 
levels(df1$Pipeline)[levels(df1$Pipeline)=="Implied Rail"]<-"Capacity Shortfall"
#names(df1)[3]<-"2018.CAPP.Western.Canadian.Pipeline.Demand"
#names(df1)[4]<-"2014.CAPP.Western.Canadian.Pipeline.Demand"



my_palette<-c(brewer.pal(10, "Set3"))

my_palette<-rev(c(brewer.pal(11, "PuOr")))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("pipe_capacity_new.png",height = 1000,width = 1600)
ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack") +
  #geom_area(data=filter(df1,df1$Pipeline!="test"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",alpha=0.8) +
  geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test1" ),size=3) +
  geom_line(aes(Year,CAPP.2018.Offtake*6.2929/1000, colour = "test2" ),size=3) +
  #geom_line(aes(Year,NEB.2016.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test4" ),size=3) +
  scale_colour_manual("",labels=c("CAPP 2019 Export Demand","CAPP 2018 Export Demand","CAPP 2014 Export Demand"),values=c("Black","Grey40","Firebrick"))+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_fill_manual("",values = mkColor(df1$Pipeline),guide = "legend")+
  guides(colour = guide_legend(order = 1,nrow = (3)), 
         fill = guide_legend(order = 2,nrow = (4)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  labs(y="Export Capacity or Demand (million bbl/d)",x="Date",
       title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#French
pipes_french<-c("?nergie Est","Northern Gateway","Agrandissement du r?seau de Trans Mountain","Keystone XL","Agrandissements d'Enbridge","Keystone","Express","Trans Mountain","Rangeland/Milk River","R?seau principal d'Enbridge")
pipes_french_real<-c("Agrandissement du r?seau de Trans Mountain","Keystone XL","Agrandissements du r?seau d'Enbridge","Keystone","Express","Trans Mountain","Rangeland/Milk River","R?seau principal d'Enbridge")



if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("pipe_capacity_new_french.png",height = 1000,width = 1600)
ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack") +
  #geom_area(data=filter(df1,df1$Pipeline!="test"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",alpha=0.8) +
  geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test1" ),size=3) +
  geom_line(aes(Year,CAPP.2018.Offtake*6.2929/1000, colour = "test2" ),size=3) +
  #geom_line(aes(Year,NEB.2016.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test4" ),size=3) +
  scale_colour_manual("",labels=c("P?trole disponible pour l'exportation (ACPP,2018)","P?trole disponible pour l'exportation (ACPP,2018)","P?trole disponible pour l'exportation (ACPP,2014)"),values=c("Black","Grey40","Firebrick"))+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  scale_fill_manual("",values = mkColor(df1$Pipeline),guide = "legend",labels=pipes_french)+
  guides(colour = guide_legend(order = 1,nrow = (3)), 
         fill = guide_legend(order = 2,nrow = (5)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  labs(y="Capacit? ou quantit? pr?vue (mm bar/j)",x="Ann?e",
       title=paste("Pr?visions de la capacit? pipelini?re canadienne d'exportation et d'exportations de p?trole",sep=""),
       caption="Source: Donn?s de l'ONE, figure par Andrew Leach.")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




df1<-melt(pipe_capacity_data,id=c("Year","CER.2020.Reference","CER.2020.Evolving","NEB.2016.Offtake","CAPP.2019.Offtake","CAPP.2018.Offtake","CAPP.2014.Offtake","CER.2019.Mod"), value.name = "Capacity",variable.name = "Pipeline")
expansions<-c("Trans.Mountain.Expansion","Keystone.XL","Northern.Gateway","Energy.East")
pipe_dreams<-c("Northern.Gateway","Energy.East")
df1<-filter(df1,!(Pipeline %in% pipe_dreams))
df1$Pipeline<-factor(df1$Pipeline,levels=c("Implied.Rail",rev(expansions),"Enbridge.Expansions","Keystone","Express","Trans.Mountain","Rangeland/Milk.River","Enbridge.Mainline"))

levels(df1$Pipeline)<-gsub("\\.", " ", levels(df1$Pipeline)) 
levels(df1$Pipeline)[levels(df1$Pipeline)=="Implied Rail"]<-"Capacity Shortfall"


#my_palette<-c(brewer.pal(7, "Set1"))

#my_palette<-c(brewer.pal(10, "Set3"))

#my_palette<-rev(c(brewer.pal(10, "PuOr")))

my_palette<-brewer.pal(11, "PuOr")[c(6,7,8,5,4,3,2,1)]


ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",color="black",size=0.5) +
  #geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test0",linetype = "test0"),size=1.5) +
  geom_line(aes(Year, CER.2019.Mod *6.2929/1000, colour = "test1",linetype = "test1" ),size=1.5)+
  geom_line(aes(Year,CER.2020.Reference*6.2929/1000, colour = "test2",linetype = "test2"),size=1.5) +
  geom_line(aes(Year,CER.2020.Evolving*6.2929/1000, colour = "test3",linetype = "test3"),size=1.5) +
  
  #geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  #geom_point(size=1) +
  #scale_color_viridis("",discrete=TRUE,option="D",labels="Expected Export Demand")+
  #scale_color_viridis("",discrete=TRUE,option="D",labels="Expected Export Demand")+
  scale_colour_manual("Export demand based on:\n",labels=rev(c("CER (2020) Evolving Case","CER (2020) Reference Case","CER (2019) Modified Case","CAPP (2014) Forecast")),values=rev(c("Black","Black","Black","Black")))+
  scale_linetype_manual("Export demand based on:\n",labels=rev(c("CER (2020) Evolving Case","CER (2020) Reference Case","CER (2019) Modified Case","CAPP (2014) Forecast")),values=rev(c("11","31","33","solid")))+
  
  #scale_fill_manual("",values = mkColor(df1$Pipeline),guide = "legend")+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
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
ggsave("pipe_capacity_real.png",height = 10,width = 16,dpi = 600)

my_palette<-c("white",grey.colors(n=7,end=0.85,start = 0.3))


lab_set=c("Export demand based on CER (2019) Forecast","Export demand based on CAPP (2019) Forecast","Export demand based on CAPP (2014) Forecast")

lab_set=c("CER (2020) Reference Case","CER (2020) Evolving Case","CER (2020) Reference Case","CAPP (2014) Forecast")
val_set=c("Grey20","Black","Grey60","Red")
ggplot(df1) +
  geom_area(data=filter(df1,df1$Pipeline!="Capacity Shortfall"),aes(Year,Capacity*6.2929/1000,fill=Pipeline),position="stack",color="black",size=0.5) +
  #geom_line(aes(Year,CAPP.2019.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test0",linetype = "test0"),size=1.5) +
  geom_line(aes(Year, CER.2019.Mod *6.2929/1000, colour = "test1",linetype = "test1" ),size=1.5)+
  geom_line(aes(Year,CER.2020.Reference*6.2929/1000, colour = "test2",linetype = "test2"),size=1.5) +
  geom_line(aes(Year,CER.2020.Evolving*6.2929/1000, colour = "test3",linetype = "test3"),size=1.5) +
  
  #geom_line(aes(Year,CAPP.2014.Offtake*6.2929/1000, colour = "test3" ),size=3) +
  #geom_point(size=1) +
  #scale_color_viridis("",discrete=TRUE,option="D",labels="Expected Export Demand")+
  #scale_color_viridis("",discrete=TRUE,option="D",labels="Expected Export Demand")+
  scale_colour_manual("Export demand based on:\n",labels=rev(c("CER (2020) Evolving Case","CER (2020) Reference Case","CER (2019) Modified Case","CAPP (2014) Forecast")),values=rev(c("Black","Black","Black","Black")))+
  scale_linetype_manual("Export demand based on:\n",labels=rev(c("CER (2020) Evolving Case","CER (2020) Reference Case","CER (2019) Modified Case","CAPP (2014) Forecast")),values=rev(c("11","31","33","solid")))+
  
  #scale_fill_manual("",values = mkColor(df1$Pipeline),guide = "legend")+
  scale_fill_manual("",values = my_palette,guide = "legend")+
  #scale_fill_viridis("",discrete=TRUE,option="D")+
  guides(colour = guide_legend(order = 1,nrow = (4)), linetype = guide_legend(order = 1,nrow = (4)),
         fill = guide_legend(order = 2,nrow = (4)))+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()+slide_theme()+
  theme(legend.key.width = unit(2,"cm"),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18),
        )+
  #ajl_line()+
  labs(y="Export Capacity or Demand (million bbl/d)",x="",
       #caption="Source: CER and CAPP Data, graph by Andrew Leach.",
       #title=paste("Canadian Oil Export Pipeline Capacity and Export Demand",sep="")
       NULL
  )
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





download.file("https://www.cer-rec.gc.ca/en/data-analysis/energy-commodities/crude-oil-petroleum-products/statistics/weekly-crude-run-summary-data/historical-weekly-crude-run-donnees-sur-les-charges-hebdomadaires-historique.xlsx",destfile = "ref_runs.xlsx",mode="wb")
ref_runs <- read_excel("ref_runs.xlsx", sheet = "WeeklyRegional",skip=6)%>%clean_names()%>%
  select(date=1,region=region_english_region_anglais,runs=runs_for_the_week_charges_pour_la_semaine,
         cap_util=percent_of_capacity_percent_de_la_capacite
         )%>%
  mutate(date=ymd(date),region=make_clean_names(region,allow_dupes = TRUE),
         runs=runs*6.28981,#convert to bbls
         region=as_factor(region)
         )%>%
    #distinct()%>%
    #ungroup()%>%
    #pivot_wider(names_from = region,values_from=c(runs,cap_util))%>%
    #mutate(runs_canada=runs_ontario+runs_quebec_eastern_canada+runs_western_canada,
    #       cap_canada=runs_ontario/(cap_util_ontario/100)+runs_quebec_eastern_canada/(cap_util_quebec_eastern_canada/100)+
    #         runs_western_canada/(cap_util_western_canada/100),
    #       )
  group_by(region)%>%
  mutate(m12_avg=as.numeric(rollapply(runs,52,mean,partial=TRUE,align = c("right"))),
         
         )%>%
  group_by(date) %>%
  mutate(Canada_runs=sum(runs,na.rm=T)) %>%
  ungroup()%>%
  mutate(can_m12_avg=rollapplyr(Canada_runs, 52, mean, partial=TRUE,align = c("right"))) %>% 
  ungroup()%>%
  mutate(region_label=str_to_title(gsub("_"," ",as.character(region))))
  

ggplot(ref_runs%>%filter(region=="western_canada")) +
  geom_area(aes(date,m12_avg,fill=region_label),position="stack") +
  #geom_line(aes(date,can_m12_avg*6.2929,colour="m12_avg"),size=1.2,colour="black") +
  scale_x_date(breaks=pretty_breaks(),date_labels =  "%b\n%Y",expand=c(0,0)) +
    scale_fill_manual("",values = colors_ua10(),guide = "legend")+
  scale_colour_manual("",labels=c("52 Week Canadian Average"),values=colors_ua10()[4])+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  scale_y_continuous(expand = c(0, 0)) +
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
       caption="Source: CER Data, graph by Andrew Leach.")

ggplot(ref_runs%>%filter(region=="Western Canada")) +
  geom_area(aes(date,runs,fill=region),position="stack") +
  #geom_line(aes(date,can_m12_avg*6.2929,colour="m12_avg"),size=1.2,colour="black") +
  scale_x_date(breaks=pretty_breaks(),date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_fill_manual("",values = colors_ua10(),guide = "legend")+
  scale_colour_manual("",labels=c("52 Week Canadian Average"),values=colors_ua10()[4])+
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 2))+
  scale_y_continuous(expand = c(0, 0)) +
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
       caption="Source: CER Data, graph by Andrew Leach.")

