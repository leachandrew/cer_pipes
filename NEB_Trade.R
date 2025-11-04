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



 

download.file("https://www.cer-rec.gc.ca/en/data-analysis/energy-commodities/crude-oil-petroleum-products/statistics/estimated-production-canadian-crude-oil-equivalent.xlsx",
              destfile="neb_prod.xlsx", mode = "wb")
 
cdn_crude_prod<-read_excel("neb_prod.xlsx", sheet = "HIST - barrels per day", range = NULL, col_names = TRUE,
                         col_types = NULL, na = "", trim_ws = TRUE, skip =0) %>% rename("Date"="Month") %>%
                clean_names()%>%select(-x20)%>%
                pivot_longer(-date,names_to = "crude_stream" )%>% 
                filter(!crude_stream%in%c("raw_mined_bitumen","raw_in_situ_bitumen"))%>%
                filter(!crude_stream%in%c("canada_total"))%>%
                mutate(date=as_date(date),
                        code=str_to_upper(sub("_.*","", crude_stream)),
                                          code=fct_other(factor(code),keep = c("AB","SK","NL"))
                                          ) %>% group_by(date,code) %>%
  summarize(prod=sum(value,na.rm = T)/10^6,nas=sum(1*is.na(value)),.groups = "drop") %>%
  filter(date<Sys.Date()-months(5))%>%
  ungroup()
  


labels <- 
  cdn_crude_prod %>%
  #filter(year(date)<2025)%>%
  filter(date == max(date)) %>%
  mutate(n=row_number())%>%arrange(-n)%>%
  mutate(prod=cumsum(prod))%>%
  mutate(lead=lag(prod),lead = replace_na(lead, 0))%>%
  mutate(prod=prod-(prod-lead)/2)

library(eia)
Sys.sleep(1)
us_prod <- eia_data(
  dir = "petroleum/sum/snd",
  data = "value",
  #facets = list(product="EPC0"),
  facets = list(series=list("MCRFPP11","MCRFPP21","MCRFPP31","MCRFPP41","MCRFPP51")),
  freq = "monthly",
  start = "2000",
  sort = list(cols = "period", order = "asc"),
)%>%clean_names()%>% mutate(series_description=gsub(" Field Production of Crude Oil \\(Thousand Barrels\\)","",series_description),
                            area=series_description,source="Domestic Production",
                            value=as.numeric(value))%>%
  select(period,area,source,value)


us_prod_collapse<-
  prod %>% 
  mutate(area=as_factor(area),
         area=fct_recode(area,"US Midwest and Rockies (US PADDs 2 and 4)"="Midwest (PADD 2)"),
         area=fct_recode(area,"US Midwest and Rockies (US PADDs 2 and 4)"="Rocky Mountain (PADD 4)"),
         area=fct_recode(area,"US Gulf Coast (US PADD 3)"="Gulf Coast (PADD 3)"),
  )%>%
  group_by(period,area,source)%>%
  summarize(value=sum(value,na.rm = T))%>%
  ungroup()%>%
  mutate(area=as.character(area))

mid_con_prod<-
  cdn_crude_prod %>%
    filter(code %in% c("AB","SK"))%>%
  rename(area=code)%>%
  mutate(
  area=fct_recode(area,"Alberta"="AB"),
  area=fct_recode(area,"Saskatchewan"="SK"),
  prod=prod*1000)%>%
  select(-nas)%>%
  bind_rows(
    us_prod_collapse%>%
      mutate(date=ymd(paste(period,01,sep = "-")),
             value=as.numeric(value/days_in_month(date))
             )%>%
      select(date,area,prod=value)
  )%>% filter(area %in% c("Alberta","Saskatchewan","US Midwest and Rockies (US PADDs 2 and 4)","US Gulf Coast (US PADD 3)"))%>%
 filter(date<=ymd("2025-05-01"))%>%
  mutate(area=factor(area, levels = c("Alberta","Saskatchewan","US Midwest and Rockies (US PADDs 2 and 4)","US Gulf Coast (US PADD 3)")))

  

labels_2 <- 
  mid_con_prod %>%
  #filter(year(date)<2025)%>%
  filter(date == max(date)) %>%
  mutate(n=row_number())%>%arrange(-n)%>%
  mutate(prod=cumsum(prod))%>%
  mutate(lead=lag(prod),lead = replace_na(lead, 0))%>%
  mutate(prod=prod-(prod-lead)/2)



p<-
  ggplot(cdn_crude_prod)+
  geom_text(
    data = labels,
    aes(x = date+months(2), y = prod, label = code,colour=code),
    hjust = 0, size = 3, fontface = "bold", show.legend = FALSE
  ) +
    geom_area(aes(date,prod,group=code,fill=code),linewidth=0.25,position = "stack")+
  scale_fill_manual(values = MetBrewer::met.brewer("Paquin", 10))+  # or "Signac" "Paquin"
  scale_colour_manual(values = MetBrewer::met.brewer("Paquin", 10))+  # or "Signac" "Paquin"
  scale_x_date(
    expand = c(0, 0),
    breaks = pretty_breaks(12),
    labels = date_format("%Y")
  ) +
  coord_cartesian(clip = "off") +  # allows drawing outside panel
  scale_y_continuous(expand = c(0, 0), breaks = pretty_breaks(10)) +
  theme_ps() +
  theme(
    #axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    legend.position = "none", # hide legend since we use direct labels
    plot.margin = margin(t = 5, r = 30, b = 5, l = 5)  # right margin expanded (r = 50)
  ) +
  labs(x="",y="Average Monthly Production (millions of barrels per day)",
       #title="Crude Oil and Lease Condensate Production by Province (2000-2025)",
       #caption="Data via Canada Energy Regulator, graph by Andrew Leach"
       )

p +
  theme_irpp() +
  theme()
ggsave("cdn_crude_prod.png",bg="white",width = 12,height=5,dpi=200)


canada_red<-"#FF0000"
alberta_blue<-"#0D3692"
oiler_copper<-"#FF4C00"
oiler_blue<-"#041E42"
ua_green<-"#007c41"
ua_gold<-"#ffdb05"


p2<-
  ggplot(mid_con_prod)+
  geom_text(
    data = labels_2,
    aes(x = date+months(2), y = prod/1000, label = area,colour = area),
    hjust = 0, size = 3, show.legend = FALSE
  ) +
  geom_area(aes(date,prod/1000,group=area,fill=area),linewidth=0.25,position = "stack")+
  scale_fill_manual("",values = c(alberta_blue,"#076A21","#B31942","#0A3161"))+  # or "Signac" "Paquin"
  scale_colour_manual("",values = c(alberta_blue,"#076A21","#B31942","#0A3161"))+  # or "Signac" "Paquin"
  
  #scale_colour_manual(values = MetBrewer::met.brewer("Paquin", 10))+  # or "Signac" "Paquin"
  scale_x_date(
    expand = c(0, 0),
    breaks = pretty_breaks(12),
    labels = date_format("%Y")
  ) +
  coord_cartesian(clip = "off") +  # allows drawing outside panel
  scale_y_continuous(expand = c(0, 0), breaks = pretty_breaks(10)) +
  theme_ps() +
  theme(
    #axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    legend.position = "none", # hide legend since we use direct labels
    plot.margin = margin(t = 5, r = 30, b = 5, l = 5)  # right margin expanded (r = 50)
  ) +
  labs(x="",y="Average Monthly Production (millions of barrels per day)",
       #title="Crude Oil and Lease Condensate Production by Province (2000-2025)",
       #caption="Data via Canada Energy Regulator, graph by Andrew Leach"
  )

p2 +
  theme_irpp()+
  theme(axis.text.x = element_text(margin = margin(t = 2),angle=90),
        plot.margin = margin(t = 5, r = 180, b = 5, l = 5),
        legend.position = "none"
  )

ggsave("mid_continent_crude_prod.png",bg="white",width = 6.5,height=5,dpi=600)
ggsave("mid_continent_crude_prod.svg",bg="white",width = 6.5,height=5,dpi=600)


p2<-
  ggplot(mid_con_prod)+
  geom_area(aes(date,prod/1000,group=area,fill=area),linewidth=0.25,position = "stack",colour="black",linewidth=0.25)+
  scale_fill_manual("",values = c(alberta_blue,"#076A21","#B31942","#0A3161"))+  # or "Signac" "Paquin"
  scale_colour_manual("",values = c(alberta_blue,"#076A21","#B31942","#0A3161"))+  # or "Signac" "Paquin"
  
  #scale_colour_manual(values = MetBrewer::met.brewer("Paquin", 10))+  # or "Signac" "Paquin"
  scale_x_date(
    expand = c(0, 0),
    breaks = pretty_breaks(12),
    labels = date_format("%Y")
  ) +
  coord_cartesian(clip = "off") +  # allows drawing outside panel
  scale_y_continuous(expand = c(0, 0), breaks = pretty_breaks(10)) +
  theme_ps() +
  theme(
    #axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
    legend.position = "none", # hide legend since we use direct labels
    plot.margin = margin(t = 5, r = 30, b = 5, l = 5)  # right margin expanded (r = 50)
  ) +
  labs(x="",y="Average Monthly Production (millions of barrels per day)",
       #title="Crude Oil and Lease Condensate Production by Province (2000-2025)",
       #caption="Data via Canada Energy Regulator, graph by Andrew Leach"
  )

p2 +
  theme_irpp()+
  theme(axis.text.x = element_text(margin = margin(t = 2),angle=90),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        legend.position = "bottom"
  )

ggsave("mid_continent_crude_prod2.png",bg="white",width = 7.5,height=7,dpi=600)
ggsave("mid_continent_crude_prod2.svg",bg="white",width = 7.5,height=7,dpi=600)






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


US_ref<-read_csv("Petroleum_Refineries_US_EIA_9172052887505603089.csv")%>%clean_names()%>%
  select(company,corp,site,state,padd,atmos_crude_dist,source,latitude,longitude)

can_ref<-read_excel("neb_refining.xlsx", sheet = "Refineries", range = NULL, col_names = TRUE,
                           col_types = NULL, na = "", trim_ws = TRUE, skip =0) %>%
  clean_names()%>%
  select(company=owner_name_company,corp=owner_name_company,site=city_site_name,state=state_province_territory,atmos_crude_dist=atmospheric_distillation_mb_d,source=source_agency,latitude,longitude)

NA_ref<-US_ref %>% bind_rows(can_ref)
library(ggthemes,mapdata)

usa <- map_data("world") %>% filter(region == "USA")
canada <- map_data("world") %>% filter(region == "Canada")
mexico <- map_data("world") %>% filter(region == "Mexico")

cities<-rbind(canada.cities,us.cities)
cities$name[grep("OTTAWA ON",cities$name)]<-"Ottawa ON"


NAmap <- 
  ggplot() + geom_polygon(data = usa, 
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
       caption="Source: NRCan and EIA data")
NAmap+geom_point(data=filter(NA_ref,atmos_crude_dist>10),
                 aes(x=longitude,y=latitude,size= atmos_crude_dist),colour="black", shape=21, stroke = 1.25,fill="grey30",alpha=0.5)+
  scale_size_continuous("Atmospheric Distillation Capacity\n(1000 bbl/d)",range = c(2,8))
ggsave("ref_map.png",height=8,width = 10,dpi=300,bg="white")


 
 



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

