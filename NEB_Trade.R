#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive")
print(getwd())

source("andrew_base.R")




names<-c("PADD1","PADD2","PADD3",
        "PADD4","PADD5")


wb <- createWorkbook()
data_store <- list()
data_count<-1
#for testing
region<-"NonUS"
for(region in names){
  file_name<-paste("CO-Type-",region,".xls",sep="")
  #inv_file<-"http://ecccdocs.techno-science.ca/documents/ECCC_STB_SRAD_GHG_ECON_NS_1990-2016_prelim_EN.xlsx"
  export_data <- read_excel(file_name,skip = 8,
                            col_types=c("text","skip","skip","numeric","skip","skip","numeric","numeric","skip","numeric","skip","skip","skip"))
  export_data<-export_data%>%filter(!is.na(Total))
  names(export_data)<-c("date","light","medium","heavy","total")
  export_data$date<-ymd(export_data$date)
  export_data$region<-region
  data_store[[data_count]]<-export_data
  data_count<-data_count+1
}

export_data<-do.call(rbind,data_store)

# get US total
us_totals<-export_data %>% melt(id=c("date","region"))%>% filter(variable!="total") %>% group_by(date,variable)%>%
  summarize(US_total=sum(value,na.rm = T))

us_totals<-dcast(us_totals,date ~ variable,value.var = "US_total")
us_totals <- us_totals %>% mutate(
  total=light+medium+heavy
)

us_totals$region<-"US"
export_data<-rbind(export_data,us_totals)

export_data<-melt(export_data,id=c("date","region"),value.name ="exports",variable.name ="type")
#cast to get wide_format across all variables

export_data <-export_data %>% mutate(region=as_factor(region),
                                     region=fct_recode(region, 
                                                       "PADD 1 (East Coast)"="PADD1",
                                                       "PADD 2 (Midwest)"="PADD2",
                                                       "PADD 3 (Gulf Coast)"="PADD3",
                                                       "PADD 4 (Rocky Mountains)"="PADD4",
                                                       "PADD 5 (West Coast)"="PADD5",
                                                       "US Total"="US"))

levels(export_data$type)<-c("Light","Medium", "Heavy", "Total")

#6.289811

set_png("exports_type_PADD.png")
ggplot(filter(export_data,type!="Total"),aes(date,exports/1000,group = type,colour=type,fill=type)) +
  geom_area(position = "stack") +
  facet_wrap(~region,ncol = 2, scales = "fixed")+
  #geom_point(size=1) +
  scale_color_manual("",values=colors_ua10())+
  scale_fill_manual("",values=colors_ua10())+
  scale_y_continuous(sec.axis = sec_axis(~.*6.2929, name = "Exports (Monthly, Thousands of Barrels per Day)"))+
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "5 year",date_labels =  "%b\n%Y",expand=c(0,0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title.y.right = element_text(margin = margin(l = 10,r=10)),
        axis.title.y = element_text(margin = margin(l = 10,r=10)),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Exports (Monthly, Thousands of Cubic Meters per Day)",x="Date",
       title=paste("Canadian Crude Exports by Type and Destination",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
dev.off()


set_png("exports_PADD_type.png")
ggplot(filter(export_data,region!="US Total"),aes(date,exports/1000,group = region,colour=region,fill=region)) +
  geom_area(position = "stack") +
  facet_wrap(~type,ncol = 2, scales = "fixed")+
  #geom_point(size=1) +
  scale_color_manual("",values=colors_ua10())+
  scale_fill_manual("",values=colors_ua10())+
  scale_y_continuous(sec.axis = sec_axis(~.*6.2929, name = "Exports (Monthly, Thousands of Barrels per Day)"))+
  #scale_colour_brewer(NULL,labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports"),type = "seq", palette = "Paired", direction = 1)+
  #scale_x_date(date_breaks = "1 year", date_labels =  "%Y",limits=c(max(as.Date("2000-01-01"),min(df1$date)),Sys.Date()),expand=c(0,0)) +
  #scale_colour_manual(labels=c("Gasoline Exports","Gasoline Imports","Net Gasoline Exports",values=c("#41ae76","#238b45","#006d2c","#00441b","Black","Black","Black","Black"))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  #scale_y_continuous(limits=c(min(df1$value),max(df1$value)),expand=c(0,0))+
  scale_x_date(name=NULL,date_breaks = "5 year",date_labels =  "%b\n%Y",expand=c(0,0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(margin = margin(t = 10)),
        axis.title.y.right = element_text(margin = margin(l = 10,r=10)),
        axis.title.y = element_text(margin = margin(l = 10,r=10)),
        #axis.label.x = element_text(size=20,vjust=+5),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(1,"line"),
        legend.position = "bottom",
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        legend.text = element_text(size = 11),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(y="Exports (Monthly, Thousands of Cubic Meters per Day)",x="Date",
       title=paste("Canadian Crude Exports by Type and Destination",sep=""),
       caption="Source: NEB Data, graph by Andrew Leach.")
dev.off()




#Production
cdn_crude_supply<-get_cansim(2510006301)%>%
  mutate(Prov=as.factor(GEO),
         Code=fct_recode(Prov,"AB"="Alberta",
                         "BC"="British Columbia",
                         "NL"="Newfoundland and Labrador",
                         "MB"="Manitoba",
                         "SK"="Saskatchewan",
                         "NS"="Nova Scotia",
                         "ON"="Ontario",
                         "NT"="Northwest Territories",
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon",
                         "PE"="Prince Edward Island",
                         "NT & NU"="Northwest Territories and Nunavut",
                         "NT & NU"="Northwest Territories including Nunavut",
                         "ATL"="Atlantic Provinces"),
         Prov_name=GEO)%>% rename("Supply"="Supply and disposition")%>%
        select(REF_DATE,Prov_name,Code,VALUE,Supply)


cdn_crude_supply_old<-get_cansim(2510001401
)%>%
  mutate(Prov=as.factor(GEO),
         Code=fct_recode(Prov,"AB"="Alberta",
                         "BC"="British Columbia",
                         "NL"="Newfoundland and Labrador",
                         "MB"="Manitoba",
                         "SK"="Saskatchewan",
                         "NS"="Nova Scotia",
                         "ON"="Ontario",
                         "NT"="Northwest Territories",
                         "QC"="Quebec",
                         "NU"="Nunavut",
                         "NB"="New Brunswick",
                         "YT"="Yukon",
                         "PE"="Prince Edward Island",
                         "NT & NU"="Northwest Territories and Nunavut",
                         "NT & NU"="Northwest Territories including Nunavut",
                         "ATL"="Atlantic Provinces"),
         Prov_name=GEO)%>% rename("Supply"="Supply and disposition")%>% filter(Supply=="Total crude production") %>%
  select(REF_DATE,Prov_name,Code,VALUE,Supply,UOM)



 

year<-year(Sys.Date())
 download.file(paste("https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/",year,"/stmtdprdtncdncrdlqvlnt2019.xls",sep=""), destfile="neb_prod.xls", mode = "wb")
 
cdn_crude_prod<-read_excel("neb_prod.xls", sheet = "HIST - barrels per day", range = NULL, col_names = TRUE,
                         col_types = NULL, na = "", trim_ws = TRUE, skip =0) %>% rename("Date"="Month") %>%
                melt(id="Date")%>% mutate(code=gsub(" .*", "", variable),
                                          code=fct_other(factor(code),keep = c("AB","SK","NL"))
                                          ) %>% group_by(Date,code) %>%
  summarize(prod=sum(value,na.rm = T)/1000,nas=sum(1*is.na(value))) 
  



p<-ggplot(filter(cdn_crude_prod,Date<=ymd("2018-08-01")),aes(Date,prod,group=code,color=code))+
  geom_line(size=1.5)+
  weekly_graphs()+
  scale_x_datetime(expand = c(0,0),date_labels = "%b\n%Y",date_breaks = "18 months")+
  scale_y_continuous(expand = c(0,0),breaks=seq(0,4000,1000),limits=c(0,max(cdn_crude_prod$prod+300)))+
  #scale_color_manual("",values=colors_tableau10()[1:4])+
  scale_color_grey("",start=0,end=.4)+
  theme(legend.position = "none")+
  labs(x="",y="Average Monthly Production (bbl/d)",
       title="Crude Oil Production by Province (2000-2018)",
       caption="Source: NEB (https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/stmtdprdctn-eng.html), graph by Andrew Leach")

p


direct_labels <- cdn_crude_prod %>% 
  group_by(code) %>%
  summarize(
    x = last(Date)-months(1), 
    y = max(prod)
  )
direct_labels$y[1]<-3000
direct_labels$y[2]<-350

#library(patchwork)
library(cowplot)

direct_labels_axis <- axis_canvas(p, axis = "y") +
  geom_text(
    data = direct_labels, 
    aes(y = y, label = code), 
    x = 0.06, 
    hjust = 0, 
    size = 5, 
    col = grey.colors(4,start=0,end=0.4)
  )

p_direct_labels <- insert_yaxis_grob(p, direct_labels_axis)

set_png(file="crude_prod.png")
ggdraw(p_direct_labels)
dev.off()




 
#refining capapcity NA

download.file("ftp://ftp.maps.canada.ca/pub/nacei_cnaie/energy_infrastructure/Refineries_NorthAmerica_201708.xlsx", destfile="neb_refining.xlsx", mode = "wb")

NA_ref<-read_excel("neb_refining.xlsx", sheet = "Refineries", range = NULL, col_names = TRUE,
                           col_types = NULL, na = "", trim_ws = TRUE, skip =0) 


library(ggthemes,mapdata)

usa <- map_data("worldLores","usa")
canada <- map_data("worldLores","canada")
mexico <- map_data("worldLores", "Mexico")

cities<-rbind(canada.cities,us.cities)
cities$name[grep("OTTAWA ON",cities$name)]<-"Ottawa ON"


set_png("refinery_map.png")
NAmap <- ggplot() + geom_polygon(data = usa, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  coord_fixed(xlim = c(-150, -50),  ylim = c(20, 70), ratio = 1.2)+
  theme_map()+theme(plot.title = element_text(size = 18,face = "bold"),
                    plot.subtitle = element_text(size = 16, face = "italic"))+
  labs(title=paste("North American Refineries",sep=""),
       subtitle=paste("Atmospheric Distillation Capacity in Thousands of Barrels Per Day",sep=""),
       caption="Source: NEB and EIAData.")
NAmap+geom_point(data=filter(NA_ref,`Atmospheric Distillation (Mb/d)`>10),
                 aes(x=Longitude,y=Latitude,size= `Atmospheric Distillation (Mb/d)`),colour="black", shape=21, stroke = 1.25,fill="grey30",alpha=0.5)+
  scale_size_continuous("Atmospheric Distillation Capacity\n(1000 bbl/d)",range = c(2,8))
dev.off()


 
 



#crude_by_rail_exports
#https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls
#require(xlsx)
#oil_by_rail <- xlsx::read.xlsx("https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls",sheetName = "CrudeOilExportsByRail")
download.file("https://www.neb-one.gc.ca/nrg/sttstc/crdlndptrlmprdct/stt/cndncrdlxprtsrl-eng.xls", destfile="cndncrdlxprtsrl-eng.xls", mode = "wb")

oil_by_rail<-read_excel("cndncrdlxprtsrl-eng.xls", sheet = NULL, range = NULL, col_names = TRUE,
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

