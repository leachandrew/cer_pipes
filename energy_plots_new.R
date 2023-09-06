source("../andrew_base.R")

## Function to drop unnecessary columns and factorize provs
cleanCANSIM<-function(x) {
  x<-x %>% select(-c(STATUS,VECTOR,COORDINATE,SYMBOL,TERMINATED,DECIMALS,UOM_ID,DGUID))
  x
}

################################
#### Refined Products PLOT #####
################################

raw_data<-get_cansim(25100044) #get refinery supply and disposition data


#ref_data<-getCANSIM3(25100044) #get refinery supply and disposition data
ref_data <- raw_data %>% clean_names()%>% mutate(geo=as.factor(geo),
                                          REF=as.factor(`refined_petroleum_products`))
                                      
ref_data<-subset(ref_data, REF=="Propane and propane mixes"|
                             REF=="Butane and butane mixes"|
                             REF=="Motor gasoline"|
                             REF=="Aviation turbo fuel"|
                             REF=="Stove oil, kerosene"|                                   
                             REF=="Diesel fuel oil"|                                   
                             REF=="Light fuel oil"|                                       
                             REF=="Heavy fuel oil"|                                        
                             REF=="Asphalt"|                                        
                             REF=="Petroleum coke (including coke from catalytic cracker)"|
                             REF=="Lubricating oils and greases"|
                             REF=="Still gas"|                          
                             REF=="Refinery losses"|
                             REF=="Other petroleum products"|
                             REF=="Total refined petroleum products")
ref_data$Date<-ymd(paste(ref_data$ref_date,"-01",sep="-"))
#ref_data$Ref_Date <- NULL
ref_data<- ref_data %>% group_by(geo,refined_petroleum_products,supply_and_disposition) %>%
  mutate(m12_avg=rollapplyr(value, 12, mean, partial=TRUE,align = c("right"))) %>% ungroup()

df3<-ref_data%>%filter(geo=="Canada",refined_petroleum_products=="Total refined petroleum products", supply_and_disposition=="Imports")


refined_products_plot<-function(ref_prod,variable_name,prov_sent){
  #testing
  ref_prod<-c("Motor gasoline")
  variable_name<-"Domestic sales" 
  prov_sent<-c("Alberta","Canada")
  
    df1<-filter(ref_data,REF %in% c(ref_prod),supply_and_disposition==variable_name,geo%in% prov_sent) %>%
      mutate(pairs=interaction(REF,geo,sep = ", "),year=year(Date),month=month(Date))%>%
      group_by(REF,geo,year,supply_and_disposition) %>% mutate(annual_avg=mean(value,na.rm = TRUE)) %>% ungroup() 
#create a variable with the 2014 average value
    mean_2014<- df1 %>% group_by(REF,geo,year,pairs,supply_and_disposition) %>% filter(year==2014) %>% summarise(mean_2014=mean(value,na.rm = TRUE)) %>%
      ungroup()%>%select(-year)
    mean_2015<- df1 %>% group_by(REF,geo,year,pairs,supply_and_disposition) %>% filter(year==2015) %>% summarise(mean_2015=mean(value,na.rm = TRUE)) %>%
      ungroup()%>%select(-year)
    df1<- df1 %>% left_join(mean_2014,by = c("geo", "REF", "pairs")) %>% left_join(mean_2015,by = c("geo", "REF", "pairs")) %>%
      left_join(annual_gdp,by = c("geo", "year")) %>%
    left_join(pop_data,by = c("geo", "year")) %>%
      mutate(ctax=case_when(year(Date) <= 2016 ~ 0,
                            year(Date) == 2017 ~ 4.49,
                            year(Date) == 2018 ~ 6.73))%>%
      left_join(wti_prices,by = c("Date")) %>% filter(year>=1986)
  if(levels==1){
  #png<-1
  #if(png==1)#set these to only turn on if you're making PNG graphs
  #  png(file="refinery_production.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(data=subset(df1,Date>"2000-01-01"),aes(Date,value/10^3*6.289811/days_in_month(month(Date))))+
    geom_line(aes(group=pairs,color=pairs),size=1.5)+
    #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
    scale_y_continuous()+
    scale_colour_manual("",values=colors_tableau10())+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 15, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+
    labs(x="Date",y=paste("Monthly ",tolower(variable_name)," (1000 bbl/d)",sep=""),
         title="Supply and Disposition of Canadian Refined Products",
         subtitle=paste(variable_name," of ",tolower(paste(ref_prod,collapse = " and "))," in ",paste(prov_sent,collapse = " and "),sep = ""),
         caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
  #if(png==1)#set these to only turn on if you're making PNG graphs
  #  dev.off()
  }
  if(levels!=1){
    ggplot(data=subset(df1,Date>"2015-01-01"),aes(Date,value/mean_2015*100))+
      geom_line(aes(group=pairs,color=pairs),size=1.5)+
      #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
      scale_y_continuous()+
      scale_colour_manual("",values=colors_tableau10())+
      theme_minimal()+theme(
        legend.position = "bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 14, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+
      labs(x="Date",y=paste("Monthly, 2015 average = 100",sep=""),
           title="Supply and Disposition of Canadian Refined Products",
           subtitle=paste(variable_name," of ",tolower(paste(ref_prod,collapse = " and "))," in ",paste(prov_sent,collapse = " and "),sep = ""),
           caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")  
    
  }
  
 #per person per unit real GDP ($2012)
    
    ggplot(data=subset(df1,Date>"2000-01-01"))+
      #geom_line(aes(Date,value,group=pairs,color=pairs),size=1.5)+
      geom_line(aes(Date,value/population/gdp*6.289811*168/days_in_month(month(Date)),
                group=pairs,color=pairs),size=1.5)+
      #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
      scale_y_continuous()+
      scale_colour_manual("",values=colors_tableau10())+
      theme_minimal()+theme(
        legend.position = "bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 14, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 15, face = "italic"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 14,face = "bold"),
        axis.text = element_text(size = 14,face = "bold", colour="black")
      )+
      labs(x="Date",y=paste("Monthly ",tolower(variable_name)," (1000 bbl/d)",sep=""),
           title="Supply and Disposition of Canadian Refined Products",
           subtitle=paste(variable_name," of ",tolower(paste(ref_prod,collapse = " and "))," in ",paste(prov_sent,collapse = " and "),sep = ""),
           caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
    
    
    df1$ctax<-df1$ctax*(df1$geo=="Alberta")
    fuel.lm <- lm(formula = value/population ~ WTI+ctax+geo + (gdp/population*geo) +(gdp/population) + as.factor(month(Date))+ as.factor(year(Date)),
                  data = df1)
    library(stargazer)
    stargazer(fuel.lm, title="Results", align=TRUE,type="text")
    
    
    }

crude_disp<-get_cansim(25100014) #get old crude oil supply and disposition data

#ref_data<-getCANSIM3(25100044) #get refinery supply and disposition data
crude_disp <- crude_disp %>% clean_names()%>% mutate(geo=as.factor(geo),
                                                     Date=ymd(paste(ref_date,"-01",sep="-"))
                                                     )%>%
  group_by(geo,supply_and_disposition) %>%
  mutate(m12_avg=rollapplyr(value, 12, mean, partial=TRUE,align = c("right"))) %>% ungroup()

crude_disp_new <-get_cansim(25100063)%>% clean_names()%>% 
  mutate(geo=as.factor(geo),
         Date=ymd(paste(ref_date,"-01",sep="-"))
  )%>%
  group_by(geo,supply_and_disposition,units_of_measure) %>%
  mutate(m12_avg=rollapplyr(value, 12, mean, partial=TRUE,align = c("right"))) %>% ungroup()
  
  #get new crude oil supply and disposition data

crude_disp<-crude_disp %>% filter(Date<ymd("2016-01-01"))%>%
  bind_rows(crude_disp_new)
#,"Export to the United States"
test<-crude_disp_new %>% filter(geo=='Canada',supply_and_disposition%in% c("Exports","Export to the United States"),
                          uom=="Barrels")%>%
ggplot(aes(group=supply_and_disposition,color=supply_and_disposition))+geom_line(aes(Date,value))


df2<-filter(crude_disp_new,supply_and_disposition=="Input to canadian refineries",geo=="Canada",units_of_measure=="Barrels")
ggplot(df2)+geom_line(aes(Date,value))+geom_line(aes(Date,m12_avg))

df2<-filter(crude_disp_new,supply_and_disposition=="Imports",geo=="Canada",units_of_measure=="Barrels")
ggplot(df2)+geom_line(aes(Date,value))+geom_line(aes(Date,m12_avg))

df_test<-filter(crude_disp_new,(supply_and_disposition=="Condensate"|supply_and_disposition=="Pentanes plus"),
                geo%in%c("Canada"),units_of_measure=="Barrels")%>% group_by(Date) %>%
  summarize(value=sum(value)/mean(days_in_month(Date))/1000,m12_avg=sum(m12_avg)/mean(days_in_month(Date))/1000)
ggplot(df2)+geom_line(aes(Date,value))+geom_line(aes(Date,m12_avg))




df1<-filter(crude_disp,supply_and_disposition=="Total disposition to refineries",geo=="Canada")
ggplot(df1)+geom_line(aes(Date,value))+geom_line(aes(Date,m12_avg))




na_data<-ref_data %>% #filter(geo=="Canada")%>%
  filter(( 
    #supply_and_disposition=="Backflow to refinery of energy by-products"|     
    #supply_and_disposition=="Closing inventory"|                             
    #supply_and_disposition=="Deliveries to other reporting companies"|
    supply_and_disposition=="Domestic sales"|                                
      supply_and_disposition=="Exports" |                                       
      supply_and_disposition=="Imports" |                                       
      #supply_and_disposition=="Inter-product transfers"|                       
      supply_and_disposition=="Inter-provincial transfers in"|                 
      supply_and_disposition=="Inter-provincial transfers out"|                 
      #supply_and_disposition=="Losses and adjustments"|                        
      #supply_and_disposition=="Net production"| 
      #supply_and_disposition== "Mid-grade motor gasoline"|                     
      #supply_and_disposition== "Net sales of low sulphur"|                       
      #supply_and_disposition=="Opening inventory"|                     
      #supply_and_disposition== "Own consumption"|                              
      #supply_and_disposition=="Portions transferred to petro-chemicals"|
      #supply_and_disposition=="Premium motor gasoline"|        
      #supply_and_disposition=="Receipts from non-reporting companies"|
      #supply_and_disposition== "Receipts from other reporting companies"|
      supply_and_disposition=="Refinery production"                            
    #supply_and_disposition== "Regular leaded motor gasoline"|                  
    #supply_and_disposition=="Regular no-lead motor gasoline"|                
    #supply_and_disposition== "Retail sales of motor gasoline"|                
    #supply_and_disposition=="Total exports (National Energy Board-form 145)"|
    #supply_and_disposition== "Transfers to refinery feedstocks"
  ) ) %>% mutate(series_value=is.na(value)*1,
    refined_petroleum_products=fct_other(refined_petroleum_products, keep = c("Motor gasoline","Diesel fuel oil",
                                                                              "Aviation turbo fuel",
                                                                              "Propane and propane mixes",
                                                                              "Butane and butane mixes",
                                                                              "Petroleum coke (including coke from catalytic cracker)","Asphalt",
                                                                              "Heavy fuel oil","Light fuel oil"
    )),
    refined_petroleum_products=fct_recode(refined_petroleum_products, 
                              "Petroleum Coke" = "Petroleum coke (including coke from catalytic cracker)", 
                              "Propane" = "Propane and propane mixes")
    )


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="refinery_data_missing.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(filter(na_data,geo=="Canada"))+
  geom_tile(aes(Date,series_value,group=refined_petroleum_products,color=refined_petroleum_products,fill=refined_petroleum_products),size=1.5,position="stack")+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  scale_x_date(date_breaks = "2 years",date_labels = "%b\n%Y")+
  theme_minimal()+scale_y_continuous(breaks = c(0,2,4,6,8))+
  facet_wrap(~supply_and_disposition,scales="free_y")+
  expand_limits(x=ymd("2008-01-01"))+
  guides(fill = guide_legend(nrow = 3))+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8, face = "bold"))+
  labs(x="",y="Number of Missing Data Series",
       title="Missing Canadian Refinery Production Data",
       #subtitle="Canadian Refinery Production",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="AB_refinery_data_missing.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(filter(na_data,geo=="Alberta",Date>ymd("2001-01-01")))+
  geom_col(aes(Date,series_value,group=refined_petroleum_products,color=refined_petroleum_products,fill=refined_petroleum_products),size=1.5,position="stack")+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  scale_x_date(date_breaks = "2 years",date_labels = "%b\n%Y" )+
  theme_minimal()+scale_y_continuous(breaks = c(0,2,4,6,8))+
  facet_wrap(~supply_and_disposition,scales="free_y")+
  expand_limits(x=ymd("2008-01-01"))+
  guides(fill = guide_legend(nrow = 3))+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8, face = "bold"))+
  labs(x="",y="Number of Missing Data Series",
       title="Missing Alberta Refinery Production Data",
       #subtitle="Canadian Refinery Production",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="NB_refinery_data_missing.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(filter(na_data,geo=="New Brunswick",Date>ymd("2001-01-01")))+
  geom_col(aes(Date,series_value,group=refined_petroleum_products,color=refined_petroleum_products,fill=refined_petroleum_products),size=1.5,position="stack")+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  scale_x_date(date_breaks = "2 years",date_labels = "%b\n%Y" )+
  theme_minimal()+scale_y_continuous(breaks = c(0,2,4,6,8))+
  facet_wrap(~supply_and_disposition,scales="free_y")+
  expand_limits(x=ymd("2008-01-01"))+
  guides(fill = guide_legend(nrow = 3))+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8, face = "bold"))+
  labs(x="",y="Number of Missing Data Series",
       title="Missing New Brunswick Refinery Production Data",
       #subtitle="Canadian Refinery Production",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="refinery_production_missing.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(na_data)+
  geom_col(aes(Date,series_value,group=refined_petroleum_products,color=refined_petroleum_products,fill=refined_petroleum_products),size=1.5,position="stack")+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  theme_minimal()+scale_y_continuous(breaks = c(0,2,4,6,8))+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8, face = "bold"))+
  labs(x="",y="Number of Missing Data Series",
       title="Missing Canadian Refinery Production Data",
       #subtitle="Canadian Refinery Production",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


na_data<-ref_data %>% filter(geo=="Canada",is.na(value))%>%
  mutate(series_value=1)
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="refinery_production_missing.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(na_data)+
  geom_col(aes(Date,series_value,group=refined_petroleum_products,color=refined_petroleum_products,fill=refined_petroleum_products),size=1.5,position="stack")+
  #scale_colour_manual("",values=colors_tableau10())+
  #scale_fill_manual("",values=colors_tableau10())+
  theme_minimal()+scale_y_continuous(breaks = c(0,2,4,6,8))+
  facet_wrap(~supply_and_disposition,scales="free_y")+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8, face = "bold"))+
  labs(x="",y="Number of Missing Data Series",
       title="Missing Canadian Refinery Production Data",
       #subtitle="Canadian Refinery Production",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




plotdata<-ref_data %>% filter(geo=="Canada",supply_and_disposition=="Refinery production") %>% mutate(
  refined_petroleum_products=fct_other(refined_petroleum_products, keep = c("Motor gasoline","Diesel fuel oil",
                                                     "Aviation turbo fuel",
                                                     "Propane and propane mixes",
                                                     "Butane and butane mixes",
                                                     "Petroleum coke (including coke from catalytic cracker)"
                                                     ))
  
)%>% group_by(Date, refined_petroleum_products,supply_and_disposition,geo) %>% summarize(value=sum(value))
  
  
png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="refinery_production.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=filter(plotdata,Date>"2000-01-01",supply_and_disposition=="Refinery Production"),aes(Date,value/10^3*6.2898/days_in_month(Date)))+
  geom_line(aes(group=refined_petroleum_products,color=refined_petroleum_products),size=1.5)+
  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  scale_x_date(date_labels = "%b\n%Y",date_breaks="2 years")+
  scale_y_continuous(limits = c(0, max(sub_samp$value/10^3*6.2898/days_in_month(sub_samp$Date))))+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="",y="Monthly Supply (1000 barrels per day)",
       title="Supply and Disposition of Canadian Refined Products",
       subtitle="Canadian Refinery Production",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


#now imports and exports

sub_samp<-subset(ref_samp, 
                 #supply_and_disposition=="Backflow to refinery of energy by-products"|     
                 supply_and_disposition=="Closing inventory"|                             
                 #supply_and_disposition=="Deliveries to other reporting companies"|
                 supply_and_disposition=="Domestic sales"|                                
                 supply_and_disposition=="Exports" |                                       
                 supply_and_disposition=="Imports" |                                       
                 #supply_and_disposition=="Inter-product transfers"|                       
                 supply_and_disposition=="Inter-provincial transfers in"|                 
                 supply_and_disposition=="Inter-provincial transfers out"|                 
                 #supply_and_disposition=="Losses and adjustments"|                        
                 #supply_and_disposition=="Net production"| 
                 #supply_and_disposition== "Mid-grade motor gasoline"|                     
                 #supply_and_disposition== "Net sales of low sulphur"|                       
                 supply_and_disposition=="Opening inventory"|                     
                 #supply_and_disposition== "Own consumption"|                              
                 #supply_and_disposition=="Portions transferred to petro-chemicals"|
                 #supply_and_disposition=="Premium motor gasoline"|        
                 #supply_and_disposition=="Receipts from non-reporting companies"|
                 #supply_and_disposition== "Receipts from other reporting companies"|
                 supply_and_disposition=="Refinery production"                            
                 #supply_and_disposition== "Regular leaded motor gasoline"|                  
                 #supply_and_disposition=="Regular no-lead motor gasoline"|                
                 #supply_and_disposition== "Retail sales of motor gasoline"|                
                 #supply_and_disposition=="Total exports (National Energy Board-form 145)"|
                 #supply_and_disposition== "Transfers to refinery feedstocks"
)             

fuels<-(unique(sub_samp$refined_petroleum_products))
cats<-unique(sub_samp$supply_and_disposition)
sub_samp<-dcast(sub_samp,Date~refined_petroleum_products+supply_and_disposition,value.var = c("value"))
for(fuel in seq(1,length(fuels)))
{
  new_col_name<-paste(fuels[fuel],"_Net Transfers In",sep="")
  exp_col_name<-paste(fuels[fuel],"_Inter-provincial transfers out",sep="")
  imp_col_name<-paste(fuels[fuel],"_Inter-provincial transfers in",sep="")
  sub_samp[,new_col_name]<-sub_samp[,imp_col_name]-sub_samp[,exp_col_name]
}

for(fuel in seq(1,length(fuels)))
{
  new_col_name<-paste(fuels[fuel],"_Net Imports",sep="")
  test_col_name<-paste(fuels[fuel],"_Sales Diff",sep="")
  sum_col_name<-paste(fuels[fuel],"_Net Imports and Transfers",sep="")
  exp_col_name<-paste(fuels[fuel],"_Exports",sep="")
  imp_col_name<-paste(fuels[fuel],"_Imports",sep="")
  prod_col_name<-paste(fuels[fuel],"_Refinery production",sep="")
  sales_col_name<-paste(fuels[fuel],"_Domestic sales",sep="")
  transfer_col_name<-paste(fuels[fuel],"_Net Transfers In",sep="")
  sub_samp[,sum_col_name]<-sub_samp[,imp_col_name]-sub_samp[,exp_col_name]+sub_samp[,transfer_col_name]
  sub_samp[,new_col_name]<-sub_samp[,imp_col_name]-sub_samp[,exp_col_name]
  sub_samp[,test_col_name]<-sub_samp[,sales_col_name]-sub_samp[,prod_col_name]
}

col_sel<-c(grep("Date",colnames(sub_samp)),grep("Net Imports",colnames(sub_samp)),grep("Sales Diff",colnames(sub_samp)),grep("Net Transfers In",colnames(sub_samp)),grep("Refinery production",colnames(sub_samp)),grep("Domestic sales",colnames(sub_samp)))
sub_samp<-sub_samp[,col_sel]

sub_samp<-melt(sub_samp,id="Date",value.name = "Value")


sub_samp<-separate(sub_samp,variable, into = c("REF","variable"),sep="_")
sub_samp$REF<-as.factor(sub_samp$REF)
sub_samp$variable<-as.factor(sub_samp$variable)



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
    png(file="ref_prod_facet.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,Value/10^6,fill=variable,group=variable,color=variable))+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & variable=="Domestic sales"),size=1.5)+
  #geom_line(data=subset(sub_samp,Date>"2000-01-01" & REF=="Jet Fuel" & variable=="Domestic Sales"),aes(group=REF,color=REF),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & variable=="Refinery production"),size=1.5)+
  #geom_bar(data=subset(sub_samp,Date>"2000-01-01" & REF=="Motor Gasoline" & variable=="Net Imports"),stat ="identity")+
  #geom_bar(data=subset(sub_samp,Date>"2000-01-01" & REF=="Motor gasoline" & variable=="Sales Diff"),stat ="identity",alpha=1)+
  #geom_bar(data=subset(sub_samp,Date>"2000-01-01" & REF=="Motor gasoline" & variable=="Net Imports"),stat ="identity",alpha=0.5)+
  facet_wrap(~REF,nrow=3,ncol=1,scales="free_y")+
  expand_limits(y = 0)+
  #scale_y_continuous(limits = c(0, 5))+
  scale_x_date(date_labels = "%b\n%Y",date_breaks="2 years")+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Supply (cubic metres)\n",
       title="Canadian Supply and Disposition of Canadian Refined Products",
       #subtitle="Canadian Gasoline Production and Sales",
       caption="Source: CANSIM table 251-00044, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="trade.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,Value/10^6,fill=variable,group=variable,color=variable))+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & REF=="Motor Gasoline" & variable=="Net Imports"),aes(group=REF,color=REF),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & REF=="Jet Fuel" & variable=="Net Imports"),aes(group=REF,color=REF),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & REF=="Diesel Fuel" & variable=="Net Imports"),aes(group=REF,color=REF),size=1.5)+
  #scale_y_continuous(limits = c(-1.5, 5))+
  scale_y_continuous()+
  scale_colour_viridis("",discrete = TRUE,option="C")+
  scale_fill_viridis("",discrete=TRUE,option="C")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Supply (cubic metres)",
       title="Supply and Disposition of Canadian Refined Products",
       subtitle="Canadian Refined Product Net Imports",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="trade.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2013-01-01"),aes(Date,Value/10^6,fill=variable,group=variable,color=variable))+
  geom_line(data=subset(sub_samp,Date>"2013-01-01" & REF=="Motor Gasoline" & variable=="Net Imports"),aes(group=REF,color=REF),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2013-01-01" & REF=="Jet Fuel" & variable=="Net Imports"),aes(group=REF,color=REF),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2013-01-01" & REF=="Diesel Fuel" & variable=="Net Imports"),aes(group=REF,color=REF),size=1.5)+
  #scale_y_continuous(limits = c(-1.5, 5))+
  scale_y_continuous()+
  scale_colour_viridis("",discrete = TRUE,option="C")+
  scale_fill_viridis("",discrete=TRUE,option="C")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Supply (cubic metres)",
       title="Supply and Disposition of Canadian Refined Products",
       subtitle="Canadian Refined Product Net Imports",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="gasoline.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,Value/10^6,group=REF,color=variable,fill=variable))+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & Date<"2017-01-01" & REF=="Motor Gasoline" & variable=="Refinery production"),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & Date<"2017-01-01" & REF=="Motor Gasoline" & variable=="Domestic sales"),size=1.5)+
  geom_bar(data=subset(sub_samp,Date>"2000-01-01"  & Date<"2017-01-01" & REF=="Motor Gasoline" & variable=="Net Imports"),stat ="identity")+
  #geom_bar(data=subset(sub_samp,Date>"2000-01-01" & REF=="Diesel" & variable=="Net Imports"),stat ="identity",aes(fill=REF))+
  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  scale_y_continuous(limits = c(-1.5, 4))+
  scale_colour_viridis("",discrete = TRUE,option="C")+
  scale_fill_viridis(discrete=TRUE,option="C")+
  theme_minimal()+theme(
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Supply (cubic metres)",
       title="Supply and Disposition of Canadian Refined Products",
       subtitle="Canadian Gasoline Production and Net Imports",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="diesel.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,Value/10^6,group=REF,color=REF,fill=REF))+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & REF=="Motor Gasoline" & variable=="Refinery production"),size=1.5)+
  geom_line(data=subset(sub_samp,Date>"2000-01-01" & REF=="Diesel Fuel" & variable=="Refinery production"),size=1.5)+
  geom_bar(data=subset(sub_samp,Date>"2000-01-01" & REF=="Diesel Fuel" & variable=="Net Imports"),stat ="identity",alpha=1,show.legend =  FALSE)+
  geom_bar(data=subset(sub_samp,Date>"2000-01-01" & REF=="Motor Gasoline" & variable=="Net Imports"),stat ="identity",alpha=0.5,show.legend = FALSE)+
  
  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  scale_y_continuous(limits = c(-0.5, 4))+
  scale_colour_viridis("",discrete = TRUE,option="C")+
  scale_fill_viridis("",discrete=TRUE,option="C")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Supply (cubic metres)",
       title="Supply and Disposition of Canadian Refined Products",
       subtitle="Canadian Gasoline and Diesel Fuel Production and Net Imports",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




#Table 128-0006 Energy fuel consumption of manufacturing industries in gigajoules, by North American Industry Classification System (NAICS), annual (gigajoules)(1,17,18)														
energy_mfg_data<-getCANSIM2(1280006) #get manufacturing energy use

energy_mfg_data$FUELTYPE <- revalue(energy_mfg_data$FUELTYPE,
                                    c("Total, energy consumed as fuel (higher heating value)"="Total Energy",
                                      "Petroleum coke and coke from catalytic cracking catalyst"="Petroleum Coke"))
energy_mfg_data$FUELTYPE<-as.factor(energy_mfg_data$FUELTYPE)

selection<-unique(energy_mfg_data$FUELTYPE)[-1]
#energy_mfg_data<-energy_mfg_data[energy_mfg_data$FUELTYPE %in% selection,]

energy_mfg_data<-energy_mfg_data[energy_mfg_data$NAICS %in% "Manufacturing",]
energy_mfg_data$Year<-energy_mfg_data$Ref_Date
energy_mfg_data$Ref_Date <- NULL

#energy_mfg_data$FUELTYPE <- relevel(energy_mfg_data$FUELTYPE, "Total Energy")

energy_mfg_data$FUELTYPE <- factor(energy_mfg_data$FUELTYPE,c(levels(energy_mfg_data$FUELTYPE)[-14],levels(energy_mfg_data$FUELTYPE)[14]))

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="energy_mfg.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=energy_mfg_data,aes(Year,Value/10^9,group=FUELTYPE,color=FUELTYPE,fill=FUELTYPE))+
  geom_line(data=subset(energy_mfg_data,FUELTYPE =="Total Energy"),position="identity",size=2)+
  geom_area(data=subset(energy_mfg_data,FUELTYPE %in% selection),position="stack")+
  scale_y_continuous()+
  scale_colour_viridis("",discrete = TRUE,option="D",direction=-1)+
  scale_fill_viridis("",discrete=TRUE,option="D",direction=-1)+
  guides(colour=guide_legend(nrow=4,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Energy fuel consumption of manufacturing industries\n(Millions of gigajoules)",
       title="Canadian Energy Use From Manufacturing",
       caption="Source: CANSIM Table 128-0006, Graph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()  



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="energy_mfg_sub.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=energy_mfg_data,aes(Year,Value/10^9,group=FUELTYPE,color=FUELTYPE,fill=FUELTYPE))+
  geom_line(data=subset(energy_mfg_data,FUELTYPE =="Wood"),position="identity",size=2)+
  geom_line(data=subset(energy_mfg_data,FUELTYPE =="Natural gas"),position="identity",size=2)+
  geom_line(data=subset(energy_mfg_data,FUELTYPE =="Coal"),position="identity",size=2)+
  geom_line(data=subset(energy_mfg_data,FUELTYPE =="Electricity"),position="identity",size=2)+
  scale_y_continuous()+
  scale_colour_viridis("",discrete = TRUE,option="D",direction=-1)+
  scale_fill_viridis("",discrete=TRUE,option="D",direction=-1)+
  guides(colour=guide_legend(nrow=1,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Energy fuel consumption of manufacturing industries\n(Millions of gigajoules)",
       title="Canadian Energy Use From Manufacturing",
       caption="Source: CANSIM Table 128-0006, Graph by Andrew Leach")

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()  


#GDP by Sector Canada 379-0031



gdp_data<-getCANSIM2(3790031) #get refinery supply and disposition data
gdp_data<-gdp_data[gdp_data$SEAS=="Seasonally adjusted at annual rates",]
gdp_data<-gdp_data[gdp_data$PRICES=="Chained (2007) dollars",]

gdp_data$Date<-as.POSIXct(fast_strptime(gdp_data$Ref_Date,format="%Y/%m"))
gdp_data$Ref_Date <- NULL

gdp_data$pet_set<-0

gdp_data $pet_set<-0
gdp_data$pet_set[(gdp_data$NAICS=="All industries (x 1,000,000)"|
                   gdp_data$NAICS=="Pipeline transportation (x 1,000,000)"|
                   gdp_data$NAICS=="Petroleum refineries (x 1,000,000)"|
                   gdp_data$NAICS=="Oil and gas extraction (x 1,000,000)"|                                                                               
                   gdp_data$NAICS=="Conventional oil and gas extraction (x 1,000,000)"|                                    
                   gdp_data$NAICS=="Non-conventional oil extraction (x 1,000,000)")]<-1
pet_gdp_data<-subset(gdp_data,pet_set==1)
  #"All industries (x 1,000,000)"

sub_samp<-dcast(pet_gdp_data,Date~NAICS,value.var = c("Value"))
sub_samp$Other_industries<-sub_samp$`All industries (x 1,000,000)`-sub_samp$`Petroleum refineries (x 1,000,000)`-sub_samp$`Pipeline transportation (x 1,000,000)`-sub_samp$`Oil and gas extraction (x 1,000,000)`
sub_samp$`All industries (x 1,000,000)`<- NULL

pet_gdp_data<-melt(sub_samp,id=c("Date"),value.name = "Value",variable.name = "NAICS")


pet_gdp_data$NAICS <- revalue(pet_gdp_data$NAICS,
                        c("Conventional oil and gas extraction (x 1,000,000)"="Conventional oil and gas extraction",
                          "Non-conventional oil extraction (x 1,000,000)"="Non-conventional oil extraction",
                          "Pipeline transportation (x 1,000,000)"="Pipelines",
                          "Petroleum refineries (x 1,000,000)"="Refineries",
                          "Other_industries"="Other industries"))


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="gdp.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=pet_gdp_data,aes(Date,Value/10^3,group=NAICS,color=NAICS,fill=NAICS))+
  geom_area(data=subset(pet_gdp_data,Date>="2007-01-01" & NAICS!="Oil and gas extraction (x 1,000,000)"),position="stack")+
  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  scale_y_continuous(limits = c(0, 2000))+
  scale_colour_viridis("",discrete = TRUE,option="D",direction=-1)+
  scale_fill_viridis("",discrete=TRUE,option="D",direction=-1)+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Annual GDP (billions of chained 2007 $CAD)",
       title="Canadian GDP from Oil and Gas Extraction Refining and Pipelines Compared to Other Industries",
       caption="Source: CANSIM table 379-0031, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





################################################
####### POPULATION PLOT USING FACET METHOD #####
################################################
## Note to Andrew: pretty clumsy code as I was just learning here.
## Likely a lot of unnecessary lines.
## The gist is spreading (long to wide) and then melting (wide to long) the data to get it in usable form. Prob an easier, more direct way.

# parameters:
#latestQ<-"Dec 2016"
#prevQ<-"Sep 2016"
latestD<-"2016-12-01"

### Get population data
popdata<-getCANSIM2("0510005")
popdata<-convertdate(popdata)
popdata<-renameprov(popdata)
popdata<-cleanCANSIM(popdata)
popdata$year <- NULL
popdata$month <- NULL
popdata<-subset(popdata,GEO!="CAN")
popdata$date.ym<-as.yearmon(popdata$date)+2/12 #adjust forward 2 months to match latest migration data

### Get inter-provincial migration data
data<-getCANSIM2("0510045")
data<-convertdate(data)
data<-renameprovfromto(data)
data<-cleanCANSIM(data)
data$year<-NULL
data$month<-NULL
data$GEO.from<-factor(data$GEO.from,levels=unique(data$GEO.from))
data$GEO.to<-factor(data$GEO.to,levels=unique(data$GEO.to))
plotdata<-subset(data.fromto,date.ym==latestQ)
plotdata<-droplevels(plotdata)
plotdata$Value.to<-NULL
plotdata$Value.from<-NULL
names(plotdata)[4]<-"Interprovincial"

### Get international migration data
data<-getCANSIM2("0510037")
data<-convertdate(data)
data<-renameprov(data)
data<-cleanCANSIM(data)
data$year<-NULL
data$month<-NULL
data<-subset(data,GEO!="CAN" & date==latestD)
data<-spread(data,COMPONENTS,Value)
data$International<- -data[,3]+data[,4]+data[,5]-data[,6]+data[,7]
data[3:7]<-NULL
data$GEO<-factor(data$GEO,levels=unique(data$GEO))
plotdata<-merge(plotdata,data,by=c("date","GEO"))

### Get natural data
data<-getCANSIM2("0530001")
data<-convertdate(data)
data<-renameprov(data)
data<-cleanCANSIM(data)
data$year<-NULL
data$month<-NULL
data<-subset(data,GEO!="CAN" & date==latestD)
data<-spread(data,ESTIM,Value)
data$Natural<-data$Births-data$Deaths
data[3:4]<-NULL
data$GEO<-factor(data$GEO,levels=unique(data$GEO))
plotdata<-merge(plotdata,data,by=c("date","GEO"))
plotdata$date.ym<-NULL

## create per capita vqriqble
plotdata$pop<-NULL
plotdata<-melt(plotdata,id.vars=c("date","GEO"))
plotdata$date.ym<-as.yearmon(plotdata$date)
plotdata<-merge(plotdata,popdata,by=c("date.ym","GEO"))
names(plotdata)[3]<-"date"
names(plotdata)[4]<-"component"
names(plotdata)[6]<-"pop"
plotdata$date.y<-NULL
plotdata$valuepercap<-plotdata$value/plotdata$pop*100000
plotdata$pop<-NULL
plotdata<-melt(plotdata,id.vars=c("date","date.ym","GEO","component"))
plotdata$variable <- revalue(x = plotdata$variable ,c("value"="Unadjusted","valuepercap"="per 100k population"))
plotdata$GEO<-factor(plotdata$GEO,levels=c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU"))


ggplot(plotdata,aes(GEO,value))+
  geom_bar(aes(group=component,fill=component),stat="identity",position="stack")+
  geom_hline(yintercept = 0)+
  facet_wrap(~variable,nrow=2,ncol=1,scales="free_y")+
  labs(title="Components of Population Change, Q1 2017",
       caption="Sources: CANSIM 051-0045, 051-0037, 053-0001\nChart by Blake Shaffer (@bcshaffer)",
       x="",
       y="# of migrants")+
  theme_blake()+
  scale_fill_blake()+
  scale_colour_blake()+
  theme(legend.position = "right")
ggsave("pop_components.png",width=7.5,height=6,dpi=200)







ag_data <- read.xlsx(xlsxFile = "ag_exports.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE,check.names=FALSE)
lentil_data<-ag_data[ag_data$`HS_CODE/CODE_SH`==71340,]
df1 <- lentil_data %>% group_by(`MONTH/MOIS`) #%>% summarise(sum_val = sum(`VALUE/VALEUR`),sum_quant=sum(`QUANTITY/QUANTITÉ`))
can_melt <- melt(can_ghg_data2,id=c("Year"))


#BC exports of refined products

ref_data<-getCANSIM2(1340004) #get refinery supply and disposition data
ref_data$Coordinate <- NULL
ref_data$Vector <- NULL
ref_data$GEO<-as.factor(ref_data$GEO)
ref_data$REF<-as.factor(ref_data$REF)
ref_data$SUP<-as.factor(ref_data$SUP)
ref_data<-subset(ref_data, REF=="Propane and propane mixes"|
                   REF=="Butane and butane mixes"|
                   REF=="Motor gasoline"|
                   REF=="Aviation turbo fuel"|
                   REF=="Stove oil, kerosene"|                                   
                   REF=="Diesel fuel oil"|                                   
                   REF=="Light fuel oil"|                                       
                   REF=="Heavy fuel oil"|                                        
                   REF=="Asphalt"|                                        
                   REF=="Petroleum coke (including coke from catalytic cracker)"|
                   REF=="Lubricating oils and greases"|
                   REF=="Still gas"|                          
                   REF=="Refinery losses"|
                   REF=="Other petroleum products"|
                   REF=="Total refined petroleum products")
ref_data$Date<-as.POSIXct(fast_strptime(ref_data$Ref_Date,format="%Y/%m"))
ref_data$Ref_Date <- NULL

plotdata<-ref_data %>% filter(GEO=="British Columbia")
ref_samp<-subset(plotdata,  
                 #plotdata$REF=="Propane and propane mixes"|
                 #plotdata$REF=="Butane and butane mixes"|
                 #plotdata$REF=="Motor gasoline"|
                 #plotdata$REF=="Aviation turbo fuel"|
                   #plotdata$REF=="Stove oil, kerosene"|                                   
                 #plotdata$REF=="Diesel fuel oil"|                                   
                 plotdata$REF=="Light fuel oil"|                                       
                 plotdata$REF=="Heavy fuel oil"                                        
                 #plotdata$REF=="Asphalt"|                                        
                 #plotdata$REF=="Petroleum coke (including coke from catalytic cracker)"|
                 #plotdata$REF=="Lubricating oils and greases"
                 #plotdata$REF=="Still gas"|                          
                 #plotdata$REF=="Refinery losses"|
                 #plotdata$REF=="Other petroleum products"|
                 #plotdata$REF=="Total refined petroleum products"
)

sub_samp<-subset(ref_samp, 
                 #SUP=="Backflow to refinery of energy by-products"|     
                 #SUP=="Closing inventory"|                             
                 #SUP=="Deliveries to other reporting companies"|
                 SUP=="Domestic sales"                               
                 #SUP=="Exports"                                       
                 #SUP=="Imports" |                                       
                 #SUP=="Inter-product transfers"|                       
                 #SUP=="Inter-provincial transfers in"|                 
                 #SUP=="Inter-provincial transfers out"|                 
                 #SUP=="Losses and adjustments"|                        
                 #SUP=="Net production"| 
                 #SUP== "Mid-grade motor gasoline"|                     
                 #SUP== "Net sales of low sulphur"|                       
                 #SUP=="Opening inventory"|                     
                 #SUP== "Own consumption"|                              
                 #SUP=="Portions transferred to petro-chemicals"|
                 #SUP=="Premium motor gasoline"|        
                 #SUP=="Receipts from non-reporting companies"|
                 #SUP== "Receipts from other reporting companies"|
                 #SUP=="Refinery production"                            
                 #SUP== "Regular leaded motor gasoline"|                  
                 #SUP=="Regular no-lead motor gasoline"|                
                 #SUP== "Retail sales of motor gasoline"|                
                 #SUP=="Total exports (National Energy Board-form 145)"|
                 #SUP== "Transfers to refinery feedstocks"
)             

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="fuel_oil.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,na.approx(Value)/10^3*6.2929/days_in_month(month(Date)),group=REF,color=REF,fill=REF))+
  geom_line(data=subset(sub_samp,Date>"2000-01-01"),size=1.5)+

  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  #scale_y_continuous(limits = c(-0.5, 4))+
  scale_colour_viridis("",discrete = TRUE,option="D")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Sales (Thousands of Barrels Per Day)",
       title="Supply and Disposition of BC Refined Products",
       subtitle="BC Heavy Refined Product Sales",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


plotdata<-ref_data %>% filter(GEO=="British Columbia")
ref_samp<-subset(plotdata,  
                 #plotdata$REF=="Propane and propane mixes"|
                 #plotdata$REF=="Butane and butane mixes"|
                 plotdata$REF=="Motor gasoline"|
                 plotdata$REF=="Aviation turbo fuel"|
                 #plotdata$REF=="Stove oil, kerosene"|                                   
                 plotdata$REF=="Diesel fuel oil"                                   
                 #plotdata$REF=="Light fuel oil"|                                       
                # plotdata$REF=="Heavy fuel oil"                                        
                 #plotdata$REF=="Asphalt"|                                        
                 #plotdata$REF=="Petroleum coke (including coke from catalytic cracker)"|
                 #plotdata$REF=="Lubricating oils and greases"
                 #plotdata$REF=="Still gas"|                          
                 #plotdata$REF=="Refinery losses"|
                 #plotdata$REF=="Other petroleum products"|
                 #plotdata$REF=="Total refined petroleum products"
)

sub_samp<-subset(ref_samp, 
                 #SUP=="Backflow to refinery of energy by-products"|     
                 #SUP=="Closing inventory"|                             
                 #SUP=="Deliveries to other reporting companies"|
                 SUP=="Domestic sales"                                
                 #SUP=="Exports"                                       
                 #SUP=="Imports" |                                       
                 #SUP=="Inter-product transfers"|                       
                 #SUP=="Inter-provincial transfers in"|                 
                 #SUP=="Inter-provincial transfers out"                 
                 #SUP=="Losses and adjustments"|                        
                 #SUP=="Net production"| 
                 #SUP== "Mid-grade motor gasoline"|                     
                 #SUP== "Net sales of low sulphur"|                       
                 #SUP=="Opening inventory"|                     
                 #SUP== "Own consumption"|                              
                 #SUP=="Portions transferred to petro-chemicals"|
                 #SUP=="Premium motor gasoline"|        
                 #SUP=="Receipts from non-reporting companies"|
                 #SUP== "Receipts from other reporting companies"|
                 #SUP=="Refinery production"                            
                 #SUP== "Regular leaded motor gasoline"|                  
                 #SUP=="Regular no-lead motor gasoline"|                
                 #SUP== "Retail sales of motor gasoline"|                
                 #SUP=="Total exports (National Energy Board-form 145)"|
                 #SUP== "Transfers to refinery feedstocks"
)             

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="gas_diesel_jet.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,na.approx(Value)/10^3*6.2929/days_in_month(month(Date)),group=REF,color=REF,fill=REF))+
  geom_area(data=subset(sub_samp,Date>"2000-01-01"),position="stack")+
  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  #scale_y_continuous(limits = c(-0.5, 4))+
  scale_colour_viridis("",discrete = TRUE,option="D")+
  scale_fill_viridis("",discrete = TRUE,option="D")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Domestic Sales (Thousands of Barrels Per Day)",
       title="Supply and Disposition of Canadian Refined Products",
       subtitle="BC Light Refined Product Sales",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


plotdata<-ref_data %>% filter(GEO=="Alberta")
ref_samp<-subset(plotdata,  
                 #plotdata$REF=="Propane and propane mixes"|
                 #plotdata$REF=="Butane and butane mixes"|
                 #plotdata$REF=="Motor gasoline"|
                 #plotdata$REF=="Aviation turbo fuel"|
                 #plotdata$REF=="Stove oil, kerosene"|                                   
                 #plotdata$REF=="Diesel fuel oil"|                                   
                 plotdata$REF=="Light fuel oil"|                                       
                   plotdata$REF=="Heavy fuel oil"                                        
                 #plotdata$REF=="Asphalt"|                                        
                 #plotdata$REF=="Petroleum coke (including coke from catalytic cracker)"|
                 #plotdata$REF=="Lubricating oils and greases"
                 #plotdata$REF=="Still gas"|                          
                 #plotdata$REF=="Refinery losses"|
                 #plotdata$REF=="Other petroleum products"|
                 #plotdata$REF=="Total refined petroleum products"
)

sub_samp<-subset(ref_samp, 
                 #SUP=="Backflow to refinery of energy by-products"|     
                 #SUP=="Closing inventory"|                             
                 #SUP=="Deliveries to other reporting companies"|
                 #SUP=="Domestic sales"                               
                 #SUP=="Exports"                                       
                 #SUP=="Imports" |                                       
                 #SUP=="Inter-product transfers"|                       
                 #SUP=="Inter-provincial transfers in"|                 
                 #SUP=="Inter-provincial transfers out"|                 
                 #SUP=="Losses and adjustments"|                        
                 #SUP=="Net production"| 
                 #SUP== "Mid-grade motor gasoline"|                     
                 #SUP== "Net sales of low sulphur"|                       
                 #SUP=="Opening inventory"|                     
                 #SUP== "Own consumption"|                              
                 #SUP=="Portions transferred to petro-chemicals"|
                 #SUP=="Premium motor gasoline"|        
                 #SUP=="Receipts from non-reporting companies"|
                 #SUP== "Receipts from other reporting companies"|
                 SUP=="Refinery production"                            
                 #SUP== "Regular leaded motor gasoline"|                  
                 #SUP=="Regular no-lead motor gasoline"|                
                 #SUP== "Retail sales of motor gasoline"|                
                 #SUP=="Total exports (National Energy Board-form 145)"|
                 #SUP== "Transfers to refinery feedstocks"
)             

png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="AB_prod_fuel_oil.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(data=subset(sub_samp,Date>"2000-01-01"),aes(Date,na.approx(Value)/10^3*6.2929/days_in_month(month(Date)),group=REF,color=REF,fill=REF))+
  geom_line(data=subset(sub_samp,Date>"2000-01-01"),size=1.5)+
  
  #geom_label_repel(data = subset(plotdata, Ref_Date==2015), aes(label = GEO,fill=GEO),color="white",nudge_x=.25,box.padding = unit(0.1, "lines"),segment.color=NA)+
  #scale_y_continuous(limits = c(-0.5, 4))+
  scale_colour_viridis("",discrete = TRUE,option="D")+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Sales (Thousands of Barrels Per Day)",
       title="Supply and Disposition of Refined Products",
       subtitle="AB Heavy Refined Product Production",
       caption="Source: CANSIM table 134-0004, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


library(cansim)
commod_list<-c("Gasoline and aviation turbine fuel", 
               "Fuel oils and crude petroleum",
               "Gaseous hydrocarbons, including liquid petroleum gas (LPG's)",
               "Coal coke and petroleum coke",
               "Other refined petroleum and coal products",
               "Total traffic carried")

rail_data<-get_cansim("4040002")%>%clean_names()%>% #get rail
  filter(railway_carloading_components %in% commod_list)%>%
  mutate(geo=as_factor(geo),
       date=ym(ref_date)
)%>%
  rename(comp=railway_carloading_components)

ggplot()+
  geom_area(data=rail_data %>% filter(comp=="Total traffic carried",geo=="Canada"),aes(date,val_norm/10^6,group=comp,fill="All Other Carloadings"))+
  geom_area(data=rail_data %>% filter(uom=="Tonnes",comp!="Total traffic carried",geo=="Canada"),aes(date,val_norm/10^6,group=comp,fill=comp),color="black",position="stack")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  scale_fill_viridis("",discrete = TRUE,alpha=1,begin=.8,end=0,option = "E",direction=-1)+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=max(rail_data$date)+months(3))+
  scale_y_continuous(expand = c(0, 0))+
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
        plot.title = element_text(hjust=0.5,size = 14),
        plot.margin=unit(c(0.25,.75,0.25,0.25),"cm"))+
  labs(x=NULL,y="Monthly Carloadings (millions of tonnes)",
       title="Crude and Refined Petroleum Rail Loadings Compared to Other Traffic",
       #subtitle="Western Canadian Region",
       caption=paste("Source: Statistics Canada CANSIM table 404-0002 accessed ",format(as.Date(Sys.Date()), format="%B%e, %Y"),". Graph by Andrew Leach.",sep=""))
       #caption=paste("Source: Statistics Canada CANSIM table 404-0002 accessed ",format(as.Date(Sys.Date()), format="%B%e, %Y"),". Graph by Andrew Leach.\nMean density assumed to be 7.5 barrels per tonne.",sep=""))
ggsave("rail_all.png",dpi=300,bg="white",width = 15,height = 8)


ggplot()+
  #geom_area(data=rail_data %>% filter(comp=="Total traffic carried",geo=="Canada"),aes(date,val_norm/10^6,group=comp,fill="All Other Carloadings"))+
  geom_area(data=rail_data %>% filter(uom=="Tonnes",comp!="Total traffic carried",geo=="Canada"),aes(date,val_norm/10^6,group=comp,fill=comp),color="black",position="stack")+
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))+
  scale_fill_viridis("",discrete = TRUE,alpha=1,begin=.8,end=0,option = "E",direction=-1)+
  scale_x_date(name=NULL,date_breaks = "1 year", date_labels =  "%b\n%Y",expand=c(0,0)) +
  expand_limits(x=max(rail_data$date)+months(3))+
  scale_y_continuous(expand = c(0, 0))+
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
        plot.title = element_text(hjust=0.5,size = 14),
        plot.margin=unit(c(0.25,.75,0.25,0.25),"cm"))+
  labs(x=NULL,y="Monthly Carloadings (millions of tonnes)",
       title="Crude and Refined Petroleum Rail Loadings",
       #subtitle="Western Canadian Region",
       caption=paste("Source: Statistics Canada CANSIM table 404-0002 accessed ",format(as.Date(Sys.Date()), format="%B%e, %Y"),". Graph by Andrew Leach.",sep=""))
#caption=paste("Source: Statistics Canada CANSIM table 404-0002 accessed ",format(as.Date(Sys.Date()), format="%B%e, %Y"),". Graph by Andrew Leach.\nMean density assumed to be 7.5 barrels per tonne.",sep=""))
ggsave("rail_oil.png",dpi=300,bg="white",width = 15,height = 8)


png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("west_crude_moves_all.png")
ggplot(data=subset(rail_west,Date>="2010-01-01"),aes(Date,na.approx(Value)/1000/days_in_month(month(Date)),group=RAI,color=RAI,fill=RAI))+
  #geom_area(data=subset(rail_west,Date>"2010-01-01"),size=1.5,position = "stack")+
  geom_area(data=subset(rail_west,Date>="2010-01-01"& RAI=="Total traffic carried"),size=1.5,position = "identity")+
  geom_area(data=subset(rail_west,Date>="2010-01-01"& RAI!="Total traffic carried"),size=1.5,position="stack")+
  
  guides(col = guide_legend(nrow = 2, byrow = TRUE))+
  scale_colour_viridis("",discrete = TRUE,option="D")+
  scale_fill_viridis("",discrete = TRUE,option="D")+
  scale_x_datetime(breaks=breaks,labels = date_format("%b\n%Y", tz="America/Denver"),expand=c(0,0))+
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "gray",linetype="dotted"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.subtitle = element_text(size = 12,hjust=0.5),
        plot.caption = element_text(face="italic",size = 12,hjust=0),
        legend.key.width=unit(2,"line"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust=0.5,size = 14))+
  labs(x=NULL,y="Monthly Rail Loadings\n(Thousands of Tonnes Per Day)",
       title="Crude and Refined Petroleum Rail Loadings vs Total Loads Carried",
       subtitle="Western Canadian Region",
       caption=paste("Source: Statistics Canada CANSIM table 404-0002 accessed ",format(as.Date(Sys.Date()), format="%B%e, %Y"),". Graph by Andrew Leach",sep=""))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

#https://www150.statcan.gc.ca/n1/tbl/csv/14100287-eng.zip
lfs_data<-getCANSIM3(14100287) #get labour force survey data


lfs_ind_data<-getCANSIM3(14100291) #get labour force survey data industry level

lfs_prov_ind_data<-getCANSIM3(14100355) #get labour force survey data industry level

#https://www150.statcan.gc.ca/n1/tbl/csv/14100355-eng.zip

save(lfs_data,file="lfs_data.RData")

ggplot(lfs_data)+geom_line(aes(REF_DATE,VALUE))


lfs_data$year <- substr(lfs_data$Ref_Date, 1, 4)
lfs_data$month <- substr(lfs_data$Ref_Date, 6, 7)
lfs_data$date<-paste(lfs_data$year,lfs_data$month,"01",sep="-")
lfs_data$date <- as.Date(lfs_data$date, "%Y-%m-%d")



lfs_data$year <- substr(lfs_data$Ref_Date, 1, 4)
lfs_data$month <- substr(lfs_data$Ref_Date, 6, 7)
lfs_data$date<-paste(lfs_data$year,lfs_data$month,"01",sep="-")
lfs_data$date <- as.Date(lfs_data$date, "%Y-%m-%d")

breaks<-seq.Date(as.Date("2015-11-01"), as.Date(max(lfs_data$date)), by="4 months")
lims<-c(as.Date("2015-11-01"), as.Date(max(lfs_data$date)))




png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  set_png("ab_work_pop.png")
ggplot(data=subset(lfs_data,GEOGRAPHY=="Alberta" & AGEGROUP=="15 years and over" & SEX== "Both sexes" & CHARACTERISTICS=="Population (x 1,000)"),aes(as.Date(date),Value))+
  geom_line(size=2)+
  #guides(col = guide_legend(nrow = 2, byrow = TRUE))+
  #scale_colour_viridis("",discrete = FALSE,option="D")+
  #scale_fill_viridis("",discrete = TRUE,option="D")+
  scale_y_continuous(limits = c(3000,3500))+
  scale_x_date(limits=lims,breaks=breaks)+
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    legend.key.height=unit(2,"line"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="Date",y="Monthly Population Estimate\n(Thousands of Working-Aged People)",
       title="Alberta Working-Aged Population (Monthly)",
       #subtitle="Western Canadian Region",
       caption="Source: Statistics Canada CANSIM table 282-0087, Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

library(data.table)

getCANSIM<-function(x) {
  url<-paste("http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/0",x,"-eng.zip",sep="")
  url
  temp<-tempfile()
  download.file(url,temp)
  unzip(temp,paste("0",x,"-eng.csv",sep=""))
  data<-read.csv(paste("0",x,"-eng.csv",sep=""),stringsAsFactors=FALSE)
  data$Value<-as.numeric(as.character(data$Value)) # Convert factors to numeric
  data<-data.table(data)
  return(data)
}




migdata<-getCANSIM("0510017")
popdata<-getCANSIM("0510005") %>%
  rename(pop=Value) %>%
  mutate(Ref_Date=as.yearmon(Ref_Date,"%Y/%m"))
interdata<-getCANSIM("0510037")
birthdeath<-getCANSIM("0530001")

## Population Growth Rates
plotdata<-popdata %>%
  group_by(GEO) %>%
  mutate(growth=pop/lag(pop,8)-1) %>%
  left_join(provnames,by="GEO") %>%
  filter(Ref_Date==max(Ref_Date) & !is.na(short) & !(short %in% c("CAN","YT","NT","NU")))
ggplot(plotdata,aes(short,growth)) +
  geom_bar(stat="identity",fill="dodgerblue") +
  geom_text(aes(label=paste0(round(100*growth,1),"%")),vjust=ifelse(plotdata$growth>0,-0.5,1.5),fontface="bold")+
  geom_hline(aes(yintercept=0), colour="black", size=1) +
  mythemebar+
  scale_y_continuous(breaks = pretty_breaks(n=8),limit=c(-0.005,.04),label=percent) +
  labs(x="",y="Per Cent",title="Provincial Population Growth Between Oct 2015 and Oct 2017",
       subtitle="Source: CANSIM 051-0005
       ",caption="Graph by @trevortombe")
ggsave("plot.png",width=7,height=4,dpi=200)

## Net inflows and outflows, certain province
plotdata<-migdata %>%
  select(Ref_Date,GEO,INT,Value) %>%
  spread(INT,Value) %>%
  mutate(netflow=`In-migrants`-`Out-migrants`,
         Ref_Date=as.yearmon(Ref_Date,"%Y/%m")) %>%
  filter(Ref_Date>="Jan 2007" & GEO=="Alberta")
ggplot(plotdata,aes(x=Ref_Date,y=netflow)) +
  geom_bar(stat="identity",fill="dodgerblue") +
  geom_bar(data=plotdata %>% filter(netflow<0),
           stat="identity",fill="firebrick3") +
  geom_hline(aes(yintercept=0), colour="black", size=1) +
  geom_segment(data=plotdata %>% filter(Ref_Date==max(Ref_Date)),
               aes(x=2017,xend=Ref_Date,y=5000,yend=netflow+250),colour="dodgerblue",
               arrow=arrow(length=unit(2, "mm")),size=1)+
  geom_label(data=plotdata %>% filter(Ref_Date==max(Ref_Date)),
             aes(x=max(Ref_Date)-0.5,y=5000,label="Q3 2017:\n+743"),colour="dodgerblue",fontface="bold")+
  mytheme+
  scale_y_continuous(breaks = pretty_breaks(n=8),label=comma) +
  scale_x_yearmon(breaks=seq(2007,2017,2),format="%Y")+
  labs(x="",y="Number of Persons",title="Alberta's Net Interprovincial Migration, Quarterly",
       subtitle="Source: CANSIM 051-0017
       ",caption="Graph by @trevortombe")
ggsave("plot.png",width=7,height=4,dpi=200)

## Net inflows and outflows, all provinces in latest quarter
pop<-popdata %>%
  filter(Ref_Date==max(Ref_Date))
plotdata<-migdata %>%
  select(Ref_Date,GEO,INT,Value) %>%
  spread(INT,Value) %>%
  mutate(netflow=`In-migrants`-`Out-migrants`,
         Ref_Date=as.yearmon(Ref_Date,"%Y/%m")) %>%
  left_join(provnames,by="GEO") %>%
  filter(Ref_Date==max(Ref_Date) & short!="CAN" & !(short %in% c("YT","NT","NU"))) %>%
  left_join(pop,by=c("GEO"))
ggplot(plotdata,aes(x=short,y=netflow/pop)) +
  geom_bar(stat="identity",fill="royalblue") +
  geom_bar(data=plotdata %>% filter(netflow<0),
           stat="identity",color="black",fill="firebrick",size=0.5) +
  geom_hline(aes(yintercept=0), colour="black", size=1) +
  mythemebar+
  scale_y_continuous(limit=c(-0.0025,0.0025),breaks = pretty_breaks(n=8),label=percent) +
  labs(x="",y="Number of Persons",title="Net Interprovincial Migration, by Province (Q3 2017)",
       subtitle="Source: CANSIM 051-0017. Displays net interprovincial inflow/outflow as % of population.
       ",caption="Graph by @trevortombe")
ggsave("plot.png",width=6,height=4,dpi=200)

## Components of growth
interprovmig<-migdata %>%
  select(Ref_Date,GEO,INT,Value) %>%
  spread(INT,Value) %>%
  mutate(netflow=`In-migrants`-`Out-migrants`,
         Ref_Date=as.yearmon(Ref_Date,"%Y/%m")) %>%
  filter(Ref_Date>="Jan 2007")
intermig<-interdata %>%
  mutate(Ref_Date=as.yearmon(Ref_Date,"%Y/%m"),
         Value=ifelse(COMPONENTS=="Emigrants",-Value,Value),
         Value=ifelse(COMPONENTS=="Net temporary emigrants",-Value,Value)) %>%
  group_by(Ref_Date,GEO) %>%
  summarize(intermig=sum(Value)) %>%
  filter(Ref_Date>="Jan 2007")
natural<-birthdeath %>%
  filter(ESTIM!="Marriages") %>%
  mutate(Ref_Date=as.yearmon(Ref_Date,"%Y/%m"),
         Value=ifelse(ESTIM=="Deaths",-Value,Value)) %>%
  group_by(Ref_Date,GEO) %>%
  summarize(natural=sum(Value)) %>%
  filter(Ref_Date>="Jan 2007")
plotdata<-intermig %>%
  left_join(interprovmig,by=c("Ref_Date","GEO")) %>%
  left_join(natural,by=c("Ref_Date","GEO")) %>%
  select(Ref_Date,GEO,intermig,netflow,natural) %>%
  gather(type,flow,intermig,netflow,natural) %>%
  filter(GEO=="Alberta") %>%
  group_by(Ref_Date) %>%
  mutate(total=sum(flow))
ggplot(plotdata,aes(x=Ref_Date,y=flow,group=type,fill=type)) +
  geom_bar(stat="identity",position="stack") +
  geom_line(aes(y=total,color="Total Change"),size=1.5)+
  geom_hline(aes(yintercept=0), colour="black", size=1) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    legend.key.height=unit(2,"line"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  scale_color_manual(name="",values="black")+
  scale_fill_brewer(name="",palette="Set1",label=c("Net International\nMigration","Natural Increase","Net Interprovincial\n Migration"))+
  scale_y_continuous(breaks = pretty_breaks(n=8),label=comma) +
  scale_x_yearmon(expand=c(0,0),breaks=seq(2007,2017,2),format="%Y")+
  labs(x="",y="Number of Persons",title="Components of Alberta Population Change",
       subtitle="Sources: CANSIM 051-0017, 051-0037 and 053-0001. ",caption="Graph by Andrew Leach, code courtesy Trevor Tombe")
ggsave("plot.png",width=8,height=5,dpi=200)




################################################
####### Power Data #####
################################################
## Note to Andrew: pretty clumsy code as I was just learning here.
## Likely a lot of unnecessary lines.
## The gist is spreading (long to wide) and then melting (wide to long) the data to get it in usable form. Prob an easier, more direct way.



power_data<-getCANSIM3(25100015) #get electricity generation data by proivince

power_data<-power_data %>% cleanCANSIM() 
power_data<-renameprov(power_data)
names(power_data)<-gsub("Class.of.electricity.","",names(power_data)) 
names(power_data)<-gsub("Type.of.electricity.","",names(power_data)) 
power_data<-power_data %>% clean_names(case = "all_caps") %>%
  mutate(REF_DATE=ymd(paste(REF_DATE,"-01",sep = "")))


ggplot(filter(power_data,GENERATION!="Total all types of electricity generation" & GEO=="CAN"),
              aes(x=REF_DATE,y=VALUE,group=GENERATION,fill=GENERATION)) +
  geom_bar(stat="identity",position="stack") +
  geom_line(aes(y=total,color="Total Change"),size=1.5)+
  geom_hline(aes(yintercept=0), colour="black", size=1) +
  theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    legend.key.height=unit(2,"line"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  scale_color_manual(name="",values="black")+
  scale_fill_brewer(name="",palette="Set1",label=c("Net International\nMigration","Natural Increase","Net Interprovincial\n Migration"))+
  scale_y_continuous(breaks = pretty_breaks(n=8),label=comma) +
  scale_x_yearmon(expand=c(0,0),breaks=seq(2007,2017,2),format="%Y")+
  labs(x="",y="Number of Persons",title="Components of Alberta Population Change",
       subtitle="Sources: CANSIM 051-0017, 051-0037 and 053-0001. ",caption="Graph by Andrew Leach, code courtesy Trevor Tombe")
ggsave("plot.png",width=8,height=5,dpi=200)



#coshing missing data
#ggplot(refinery,aes(Period,Data2)) + geom_point(shape="square filled",size=2.1,stroke=0, aes(fill=MissingOrNA)) + theme(axis.text.x=element_text(angle=90,size=6),strip.text.y=element_text(angle=0),axis.text.y=element_text(size=5)) + facet_grid(`Product class`~.) + ylab("") + scale_fill_manual(values=c("white","red"))
