library(tidyverse)
library(lubridate)
library(janitor)
library(viridis)
library(scales)

pipe<-"Keystone"
  file_name<-paste("https://www.cer-rec.gc.ca/open/tolls/",pipe,"-tolls.csv",sep="") 
  options(timeout=max(300,getOption("timeout")))
  download.file(file_name,paste(tolower(pipe),"-tolls.csv",sep=""),mode="wb")
  var_name<-paste(pipe,"_data",sep="")
  pipe_tolls <- read_csv(file = paste(tolower(pipe),"-tolls.csv",sep=""))%>%clean_names()%>%mutate(pipe_name=pipe)

#Keystone tolls#
  
  tolls<-c( "Uncommitted","Committed, 10 yr Term, Total Contract",
            "Committed, 20 yr Term, Total Contract",     
            #"Committed, 20 yr A Term, Total Contract",
            #"Committed, 20 yr B Term, Total Contract"
            NULL)
  #dest<-

  pipe_tolls %>% #select(path,service) %>% 
    filter(service %in% tolls,path!="Hardisty, Alberta to International Boundary at or near Haskett, Manitoba",
           path!="Hardisty, Alberta to Houston, Texas") %>% 
    mutate(date=ymd(date))%>%
  ggplot(aes(date,toll,group = interaction(service,product),color=product,lty=service)) +
  geom_line(linewidth=1.25)+
  facet_wrap(~path)+
  expand_limits(y=c(10,20))+
                  
  scale_color_manual("Product:",values=c("black","dodgerblue"))+
  scale_linetype_manual("Service Term:",values=c("solid","11","31"))+
  scale_x_date(name=NULL,date_breaks = "2 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),breaks=pretty_breaks(),
                       sec.axis = sec_axis( trans=~.*1/6.2929, name="Tolls (to Canada/US border, dollars per barrel)")) +
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
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Tolls (to Canada/US border, dollars per cubic meter)",x="Date",
       title=paste("Keystone pipeline (Canadian) tolls by product, contract, and destination",sep=""),
       caption="Source: CER Data, graph by Andrew Leach.")
ggsave("keystone_tolls.png",dpi=300,bg="white",width = 15,height = 8)



#Mainline tolls#


pipe<-"Enbridge-Mainline"
file_name<-paste("https://www.cer-rec.gc.ca/open/tolls/",pipe,"-tolls.csv",sep="") 
options(timeout=max(300,getOption("timeout")))
download.file(file_name,paste(tolower(pipe),"-tolls.csv",sep=""),mode="wb")
var_name<-paste(pipe,"_data",sep="")
mainline_tolls <- read_csv(file = paste(tolower(pipe),"-tolls.csv",sep=""))%>%clean_names()%>%mutate(pipe_name=pipe)

tolls<-c( "Uncommitted","Committed, 10 yr Term, Total Contract",
          "Committed, 20 yr Term, Total Contract",     
          #"Committed, 20 yr A Term, Total Contract",
          #"Committed, 20 yr B Term, Total Contract"
          NULL)
#dest<-

mainline_tolls %>% #select(path,service) %>% distinct()
  filter(grepl("Edmonton Terminal, Alberta", path)) %>% 
  filter(grepl("Sarnia", path)) %>% 
  mutate(date=ymd(date),product=as_factor(str_to_title(product)),
         product=fct_recode(product,"Condensate"="Cnd"),
         product=fct_relevel(product,"Light",after = 3),
         product=fct_relevel(product,"Condensate",after = 3)
         )%>%
  ggplot(aes(date,toll,group = interaction(service,product),color=product)) +
  geom_line(linewidth=1.25)+
  facet_wrap(~path)+
  expand_limits(y=c(10,20))+
  scale_color_viridis("Product:",discrete = T, option="B",end=0.8)+
  #scale_linetype_manual("Service Term:",values=c("solid","11","31"))+
  scale_x_date(name=NULL,date_breaks = "2 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  scale_y_continuous(expand = c(0, 0),breaks=pretty_breaks(),
                     sec.axis = sec_axis( trans=~.*1/6.2929, name="Tolls (dollars per barrel)")) +
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
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Tolls (dollars per cubic meter)",x="Date",
       title=paste("Enbridge Mainline tolls by product, contract, and destination",sep=""),
       caption="Source: CER Data, graph by Andrew Leach.")
ggsave("mainline_tolls.png",dpi=300,bg="white",width = 15,height = 8)


