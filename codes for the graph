library(grid)
library(scales)
library(tools)



#Figure 1 tax comparison-----

scale_fill_tax <- c( 'Pre-tax Price'='#aeb3b6','Registration Tax'=ICCTblue, 'Value Added Tax'=ICCTgreen,'Annual Tax'=ICCTpurple )

tax_comparison <- import("~/Desktop/Turkey- tax System/R /R_input/tax_comparison.xlsx", sheet='r_data')
tax_comparison$`Car model`<- paste(tax_comparison$`Car model`,tax_comparison$Fuel)
tax_comparison <- tax_comparison%>% select("Car model","Country",'Pre-tax Price' =`Net Price`,"Registration Tax",'Value Added Tax'=VAT, "Annual Tax")%>%
  gather(key = "tax type","tax amount",'Pre-tax Price': "Annual Tax" )

colnames(tax_comparison) <- c("car_model","Country",'tax_type', 'tax_amount' )
total_price <-tax_comparison%>% filter(car_model=='Toyota C-HR (Hybrid)') %>% group_by(Country,car_model) %>% summarise(total_price=sum(tax_amount)) %>% arrange(desc(total_price))
tax_comparison$Country <- factor(tax_comparison$Country, levels=unique(total_price$Country))
tax_comparison$tax_type <- factor(tax_comparison$tax_type, levels =rev(unique(tax_comparison$tax_type) ))

two_model <- filter(tax_comparison,car_model%in% c("Toyota C-HR (Hybrid)","Fiat Egea (Diesel)") )
two_model$car_model<- factor(two_model$car_model, levels=c("Fiat Egea (Diesel)","Toyota C-HR (Hybrid)")) 
two_model<- ggplot(data=two_model, aes(Country,tax_amount,fill=tax_type))
two_model + geom_bar(position = 'stack',stat='identity',width = 0.5) + theme_pvs_helvetica + 
  labs(y = 'Consumer ownership costs (in TL)') +
  scale_fill_manual(values = scale_fill_tax) +
  theme(legend.justification = c(0.2,0.2), legend.position = c(0.28,0.77), legend.text=element_text(size=8.5),legend.title=element_blank(),
        plot.caption = element_text(size=8),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 9.5,face = 'bold'), axis.title.x  = element_blank(), axis.title.y =element_text(size=10),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm'))+
  scale_y_continuous(labels=comma,  limits = c(0,210000 ),expand = c(0,0))+
  facet_wrap(~ car_model, scales = 'free') +
  theme(strip.background = element_rect(fill=ICCTblue),
        strip.text = element_text(size=10, colour="white",face='bold'))

#Figure 2 current mtv----

mtv <- PassengerCar%>% select(age,mtv,engine_group) %>% mutate(price=1)
mtv$age<- mtv$age+1
PassengerCar$age_group <- case_when(
  between(PassengerCar$age,1,3)   ~1,
  between(PassengerCar$age,4,6)   ~2,
  between(PassengerCar$age,7,11)  ~3,
  between(PassengerCar$age,12,15) ~4,
  PassengerCar$age>=16 ~5
)
mtv_2018 <- import("~/Desktop/Turkey- tax System/R /R_input/tax_comparison.xlsx", sheet='mtv_2018')
mtv<- rbind(mtv,mtv_2018)
mtv$id <- paste(mtv$engine_group,mtv$price,sep = '')
mtv$engine_displacement <- case_when(
  mtv$engine_group==1 ~ '<1.3l',
  mtv$engine_group==2 ~ '1.3-1.6l',
  mtv$engine_group==3 ~ '1.6-2.0l',
  mtv$engine_group==4 ~ '>2.0l'
)
scale_color_mtv <- c( "11"=ICCTgreen,"12"=ICCTgreen,"13"=ICCTgreen,'21'=ICCTblue,'22'=ICCTblue,'23'=ICCTblue,'31'=ICCTorange,'32'=ICCTorange,'41'=ICCTbrown,'42'=ICCTbrown)
mtv$mtv<- mtv$mtv*1.159
mtv<- rbind(mtv,mtv%>% filter(age==1)%>% mutate(age=0))
mtv$age<- ifelse(mtv$age>2,mtv$age-1,mtv$age)

mtv_graph <- ggplot(data=filter(mtv,age<27), aes(age,mtv,color=as.factor(id)))
mtv_graph + geom_line(size=1)   + theme_pvs_helvetica +
  labs(x="Vehicle age",y='Annual tax per vehicle (in TL)')+
  scale_color_manual(values = scale_color_mtv)+
  theme(legend.position = 'none', 
        axis.text = element_text(size = 9,face = 'bold'), 
        axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 7.5),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm')) + 
  scale_x_continuous(breaks = seq(0,25,by=5),limits = c(0,28),expand = c(0,0)) +
  scale_y_continuous(labels = scales::comma, breaks = seq( 2000,12000, by = 2000), limits = c(0,12000),expand = c(0.01,0))+
  geom_vline(xintercept = 2, linetype="dotted", color = gray1, size=0.7)+
  geom_dl(aes(label = setNames(engine_displacement,id)), method = list(dl.combine("last.bumpup"), cex = 0.7, fontface='bold'))+
  geom_segment(aes(x=5,xend=2,y=10500,yend=10500), arrow = arrow(length = unit(0.15, "cm"),type = 'closed'), color = gray1)+
  annotate("text", x=13.9, y=10550, label= "Since 2018, vehicle net price introduced as an additional determinant",size=3)



#Figure 3 vehicle in the use comparison----

scale_fill_dummy<- c('no'=gray1, 'yes'=ICCTblue)
scale_fill_dummy2<- c('no'=ICCTorange, 'yes'=ICCTpurple)
car_in_use<-import("~/Desktop/Turkey- tax System/R /R_input/car_in_use.xlsx",sheet='total_car') %>% select(country,'Cars on the Road'='2015')
parc_density <- import("~/Desktop/Turkey- tax System/R /R_input/car_in_use.xlsx",sheet='parc_density') %>% select(country,'Parc Density'='2015')
car_in_use<- left_join(car_in_use,parc_density,by="country")%>%arrange(desc(car_in_use[,2])) ;rm(parc_density)
car_in_use<- car_in_use[!(car_in_use$country %in% c('Russia','Ukraine')),]
car_in_use$country<- factor(car_in_use$country, levels = unique(car_in_use$country))
car_in_use$dummy <- ifelse(car_in_use$country=='Turkey','yes','no')

ggplot(data = car_in_use, aes(x = country, y =`Cars on the Road`/1000000,fill=dummy)) + #start plot by by plotting bars
  geom_bar(stat = "identity") +labs(y='Passenger cars on the road (in millions)')+
  #plot line on same graph
  # rate multiplied by 10000000 to get on same scale as bars
  geom_point(data = car_in_use, aes(x = country, y = `Parc Density`/20, group = 1,color=dummy),
            inherit.aes = FALSE,size=1) +scale_fill_manual(values =scale_fill_dummy )+scale_color_manual(values =scale_fill_dummy2)+
  
  #specify secondary axis
  #apply inverse of above transformation to correctly scale secondary axis (/10000000)
  scale_y_continuous(limits = c(0,51),expand = c(0,0), label=comma,sec.axis = sec_axis(~.*20, name = "Cars per 1000 inhabitants"))+
  theme_pvs_helvetica+
  theme( legend.position = 'none',
         axis.text.x = element_text(angle = 90, hjust = 1),
         axis.text = element_text(size = 9,face = 'bold',color = ifelse(car_in_use$dummy=='yes',ICCTblue,gray1)),
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 10,face = 'bold',color = gray1),
         axis.text.y.right = element_text(color = gray1),
         axis.title.y.right = element_text(color=gray1),
         plot.caption = element_text(size = 7,face='bold'))+ 
  annotate("text", x='Ireland', y=48, label= "                 cars per 1,000 inhabitants",size=3,color=ICCTorange,fontface='bold')



#Figure 4 cars on the road - fuel type----


scale_fill_fuel <- c( Diesel=ICCTbrown,Gasoline=ICCTblue,'Liquefied Petroleum Gas'=ICCTpurple)

fuel <- filter(PassengerCar,fuel%in%c('diesel','gasoline','lpg'))
fuel$fuel<- ifelse(fuel$fuel=='lpg','Liquefied Petroleum Gas',fuel$fuel)
fuel <- ggplot( data=fuel,aes(model,total/1000,fill=toTitleCase(fuel)))
  fuel + geom_bar(position = 'stack',stat='identity') + theme_pvs_helvetica + scale_fill_manual(values = scale_fill_fuel) + 
  labs(x="Model year",y='Passenger cars on the road (in thousands)') +
  theme(legend.title = element_blank(), legend.position = c(0.2,0.6),legend.text = element_text(size = 9),
        axis.text = element_text(size = 10,face='bold'), axis.title = element_text(size = 11,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm'))+ 
        scale_x_continuous(breaks = seq(1985, 2015, by =5)) + 
          scale_y_continuous(expand = c(0,0), limits = c(0,1030))


#Figure 5 cars on the road - age groups----

scale_fill_age<- c('1-3 Age'=ICCTgreen,'4-6 Age'='#99c9d4','7-11 Age'='#4ca1b4','12-15 Age'=ICCTblue,'16 or older'=ICCTbrown)

age_group_emissions<- PassengerCar%>% group_by(age_group) %>% summarise(total_car=sum(total), total_co2=sum(total*co2),
                                                                        total_nox=sum(NOx*total), total_pm=sum(PM*total), total_mtv=sum(mtv*total)) %>%
  mutate('Market share'=total_car/sum(total_car), 'CO2 share'=total_co2/sum(total_co2),'NOx share'=total_nox/sum(total_nox), 
         'PM share' = total_pm/sum(total_pm), 'MTV share' = total_mtv/sum(total_mtv))  %>% 
  select(age_group,'Market share','CO2 share','NOx share','PM share','MTV share')
age_group_emissions<- gather(age_group_emissions,category,value,'Market share':'MTV share')
age_group_emissions$age_group<-case_when(
  age_group_emissions$age_group==1 ~'1-3 Age',
  age_group_emissions$age_group==2 ~'4-6 Age',
  age_group_emissions$age_group==3 ~ '7-11 Age',
  age_group_emissions$age_group==4 ~ '12-15 Age',
  age_group_emissions$age_group==5 ~'16 or older'
)

age_group_emissions$age_group<- factor(age_group_emissions$age_group, levels = rev(c('1-3 Age','4-6 Age','7-11 Age','12-15 Age','16 or older')))
age_group_emissions$category<- factor(age_group_emissions$category,levels = c('Market share','MTV share','CO2 share','NOx share','PM share'))
age_group_emissions <- ggplot(data = age_group_emissions,aes(x=category,y=value,fill=age_group))
age_group_emissions + geom_bar(position = 'stack',stat='identity') + 
  theme_pvs_helvetica + 
  scale_x_discrete(labels = c(expression(bold('Market share')) ,expression(bold('MTV share')), expression(bold(paste(CO[2],' share'))), expression(bold(paste(NO[x],' share'))),expression(bold('PM share'))))+
  scale_fill_manual(values = scale_fill_age) +
  geom_text(aes(label=scales::percent(round(value,2),accuracy = 1)),position = position_stack(vjust = 0.5),color='white',fontface='bold')+
  theme( legend.text=element_text(size=10),legend.title=element_blank(),
         legend.key = element_rect(size = 5),legend.key.height = unit(2,"line"),
         axis.text = element_text(size = 10,face = 'bold'), 
         axis.title.x  = element_blank(), axis.title.y =element_blank(),
         plot.margin  = unit(c(0.2,0.2,0.5,0.2),'cm'))+
  scale_y_continuous(labels=scales::percent,expand = c(0,0), limits = c(0,1.05))



#Figure 6 cars on the road - engine dipslacement----

scale_color_engine <- c('<1300'=ICCTgreen,"1301-1400"='#7fbcc9',"1401-1500"='#4ca1b4',
                         "1501-1600"=ICCTblue, "1601-2000"=ICCTorange, "2001+"=ICCTbrown)

engine<- PassengerCar %>% select(engine_displacement,model,total,engine_group)
engine$engine_displacement[engine$engine_displacement=="1300-"]<- '<1300'
engine$engine_displacement<- factor(engine$engine_displacement, levels = rev(unique(engine$engine_displacement)))
engine <- ggplot( engine, aes(model,total/1000,fill=engine_displacement))
engine + geom_bar(position = 'stack',stat='identity') + theme_pvs_helvetica  +
  scale_fill_manual(name=expression(bold(paste('Engine displacement (in ',cm^3,')'))) , values = scale_color_engine)+
  labs(x="Model year",y='Passenger cars on the road (in thousands)') +
  theme(legend.title = element_text(size = 10), legend.position = c(0.2,0.6),legend.text = element_text(size = 9),
        axis.text = element_text(size = 10,face='bold'), axis.title = element_text(size = 11,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm'))+ 
  scale_x_continuous(breaks = seq(1985, 2015, by =5)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1030))




#Figure 7   new sales      - country market comparison -----

#   I used OICA data for this because I dont have IHS data for Turkey before  2014

new_sales_country <- import("~/Desktop/Turkey- Tax System/Baseline Paper/r_input/Sales-Passenger-cars-2017.xlsx")
new_sales_country <- new_sales_country[new_sales_country$`2017`>250000&new_sales_country$`2005`>250000,] %>% na.omit() %>% filter(!Country%in%c('RUSSIA','SWITZERLAND','AUSTRIA'))
new_sales_country <- (gather(new_sales_country,key = 'Year','registration', 2:14)) 
colnames(new_sales_country)<- c('Country','Year','Registration')
new_sales_country$Country[new_sales_country$Country!= 'UK']<- toTitleCase(tolower( new_sales_country$Country[new_sales_country$Country!= 'UK']))
new_sales_country$is.Turkey <- ifelse(new_sales_country$Country=='Turkey','yes','no')
new_sales_country$Year<- as.numeric(new_sales_country$Year)
new_sales<-  ggplot( new_sales_country,aes(Year,Registration/1000000,group=Country, color=is.Turkey))
new_sales+ geom_line(data = new_sales_country%>% filter(is.Turkey=='no'), size=1)  +
  geom_line(data = new_sales_country%>% filter(is.Turkey=='yes'), size=1.2)+
  theme_pvs_helvetica +
  labs(y='New passenger car registrations (in millions) ')+
  scale_color_manual(values = c('yes'=ICCTred, 'no'='#aeb3b6'))+
  theme(legend.position = 'none', axis.title.x = element_blank(),
        axis.text = element_text(size = 9,face = 'bold'), axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm')) + 
  scale_y_continuous(breaks = seq( 1,4, by = 1), limits = c(0,4.1),expand = c(0,0))+
  scale_x_continuous(breaks = seq( 2005,2017, by = 1), limits = c(2005,2018.5),expand = c(0,0))+
geom_dl(aes(x=Year+0.1,label = Country), method = list(dl.combine('last.bumpup'), cex = 0.75,fontface='bold'))


#Figure 8   new sales   - brand market shares ----- 

#Škoda, Citroën was problematic

scale_fill_brand<- c( BMW=ICCTred, Other=gray1, 'Mercedes'=darken(ICCTcream,1.1), Fiat=ICCTorange,Ford='#98b476',Honda=ICCTgray, Hyundai=ICCTpurple,Kia=p2,Opel=g5,
                      Peugeot=g4,Citroën=ICCTgreen,Renault=ICCTbrown,Dacia=br2,Nissan=br1,Toyota=ICCTyellow, Volkswagen=b5,'Škoda'=b4,Audi=b3,Seat=b2)

brand<-filter(odd_pc,year==2017& group_names %in% c('BMW Group',"daimler","FCA","ford","honda","hyundai","Groupe PSA" ,"renault-nissan","toyota","Volkswagen Group"))%>%
  group_by(group_names,make)%>% summarise(total=sum(sales))%>%mutate(make=ifelse(total<5000,'other',make))%>% arrange(desc(total))
brand$make<- toTitleCase(brand$make)
brand[brand=='Mercedes-Benz'] <- 'Mercedes'
brand[brand=='Bmw'] <- 'BMW'
brand[brand=='Citroen'] <- 'Citroën'
brand[brand=='Skoda'] <- 'Škoda'
brand$group_names<- toTitleCase(brand$group_names)
brand$make<- factor(brand$make,levels = c("Renault","Dacia","Nissan","Volkswagen","Škoda","Audi",'Seat','Opel','Peugeot','Citroën','Fiat','Hyundai','Kia','Toyota','Ford','Honda','Mercedes','BMW','Other'))
brand$group_names<- factor(brand$group_names, levels= rev(c("Renault-Nissan","Volkswagen Group","Groupe PSA","FCA","Hyundai","Toyota","Ford","Honda","Daimler","BMW Group")))
brand$share <- brand$total/sum(brand$total)
brand_bar <- ggplot(data=brand, aes(group_names,share,group=group_names,fill=make))
brand_bar +  geom_bar(position = 'stack',stat='identity')+ theme_pvs_helvetica +scale_fill_manual(values = scale_fill_brand)+
  labs(x="",y='Total Sales')+guides(col = guide_legend(nrow = 2, byrow = TRUE))+
  coord_flip()+ 
  theme( plot.title = element_text( face = 'bold',size = 12),
         legend.position = 'none',
         axis.title.x = element_blank(), 
         axis.text.x = element_text(size = 10,face='bold',margin=margin(5,0,10,0)),
         axis.text.y = element_text(size = 10,face='bold'), 
         axis.title.y = element_text(size = 10,face = 'bold'),
         plot.caption = element_text(size = 8,face='bold'), 
         plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), limits = c(0,0.31), breaks = seq(0,0.3,by=0.05)) + 
  geom_text(data = filter(brand, share>0.01), aes(label= make),position = position_stack(vjust = 0.45),color='white',size=3.2,fontface='bold')



#Figure 9   new sales      - oem market shares -----

  
scale_color_group <- c('renault-nissan'=ICCTbrown,'Volkswagen Group'=ICCTblue,'Groupe PSA'=g4,honda=gray1,FCA=ICCTorange,ford='#98b476',
                     'BMW Group'=ICCTred,daimler=darken(ICCTcream,1.3),hyundai=ICCTpurple,toyota=ICCTyellow)

group_share<- (odd_pc%>% group_by(year,group_names) %>% summarise(total=sum(sales,na.rm=T)) %>% mutate(share=total/sum(total)))
group_share <- filter(group_share,group_names %in% c('BMW Group',"daimler","FCA","ford","honda","hyundai","Groupe PSA" ,"renault-nissan","toyota","Volkswagen Group"))
group_share <- ggplot(data=group_share, aes(year,share,color=group_names))
group_share +  geom_line(size=1.1)   + theme_pvs_helvetica + labs(y='Market share')+scale_color_manual(values = scale_color_group)+
  theme( legend.position = 'none',
         axis.text = element_text(size = 9,face = 'bold'),
         axis.title.y = element_text(size = 10,face = 'bold'),axis.title.x = element_blank(),
         plot.caption = element_text(size = 8,face='bold'),
         plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm')) + 
   
  scale_x_continuous(breaks = seq(2007, 2017, by =1), expand = c(0,0), limits = c(2007,2019.3))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.3, by =0.05), limits = c(0,0.3),expand = c(0,0))+
  geom_dl(aes(label = paste(' ',toTitleCase(group_names)), colour=group_names), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))



#Figure 10  new sales      - country CO2 comparison -----
tr_co2<- odd_pc%>% group_by(year) %>% summarise(co2_average=weighted.mean(co2_emission,sales,na.rm=T)) %>% mutate(country='turkey')
eu_co2<-tr_co2
eu_co2$co2_average<- c(159.0,	154.3,	147.5,	142.6,	137.7,	133.4,	126.8,	123.5,	119.6,	118.1, 119 )  # ugly but fast soltion
eu_co2$country<-'EU'
year_co2<-rbind(tr_co2,eu_co2) ; rm(eu_co2)
year_co2 <- ggplot(data=year_co2, aes(year,co2_average,color=country))
year_co2 +  geom_line(size=1.2)   + theme_pvs_helvetica + labs(y=expression(bold('Average ' *CO[2]* ' emission values (g/km)')))+
  scale_color_manual(values = c(EU=ICCTblue,turkey=ICCTred))+
  theme(legend.position ='none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.text.y = element_text(size = 10,face = 'bold'), 
        axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm")) + 
  scale_x_continuous(limits=c(2007,2018),breaks = seq(2007, 2017, by =1),expand = c(0,0))+
  scale_y_continuous( limits = c(110,160))+ 
  geom_dl(aes(label =paste(' ',toTitleCase(country)), colour=country), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))

#Figure 11: new sales      - fuel types CO2 comparison----

scale_fill_fuel <- c( diesel=ICCTbrown,gasoline=ICCTblue,LPG=ICCTpurple,'hybrid-electric'=ICCTpurple)

fuel_co2<- (odd_pc%>% group_by(year,fuel_type) %>% summarise(co2_average=weighted.mean(co2_emission,sales,na.rm=T)))
fuel_co2$fuel_type<-ifelse(fuel_co2$fuel_type=='electric','battery-electric',ifelse(fuel_co2$fuel_type=='hybrid','hybrid-electric',fuel_co2$fuel_type))
fuel_co2 <- ggplot(data=filter(fuel_co2,fuel_type!='lpg'), aes(year,co2_average,color=fuel_type))
fuel_co2 +  geom_line(size=1.2)   + theme_pvs_helvetica + labs(y=expression(bold('Average ' *CO[2]* ' emission values (g/km)')))+
  scale_color_manual(values = scale_fill_fuel)+
  theme(legend.position = 'none',
        axis.title.x = element_blank(), 
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm"),
        plot.caption = element_text(size=8,face='bold'),
        axis.text.y = element_text(size = 9,face = 'bold'), 
        axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.title = element_text(size = 10,face = 'bold')) + 
  scale_x_continuous(limits = c(2007,2019.3), breaks = seq(2007, 2017, by =1),expand = c(0,0))+
  scale_y_continuous( limits = c(0,200),expand = c(0,0)) +
  geom_dl(aes(label = paste(' ',toTitleCase(fuel_type)), colour=fuel_type), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))

#Figure 12: new sales      - oem CO2 comparison----

group_co2<-filter(odd_pc,group_names %in% c('BMW Group',"daimler","FCA","ford","honda","hyundai","Groupe PSA" ,"renault-nissan","toyota","Volkswagen Group"))%>%
  group_by(year,group_names)%>% summarise(co2=weighted.mean(co2_emission,sales,na.rm=T))
tr_average<- tr_co2%>% select(year,group_names=country,co2_average)%>% mutate(group_names='TR Average')
group_co2_chart <- ggplot(data=group_co2, aes(year,co2,color=group_names))
group_co2_chart +  geom_line(size=1.2)   + theme_pvs_helvetica + 
  labs(y=expression(bold('Average ' *CO[2]* ' emission values (g/km)')))+
  scale_color_manual(values = scale_color_group)+
  theme(legend.position = 'none',
        axis.title.x = element_blank(), 
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm"),
        axis.text.y = element_text(size = 10,face = 'bold'), 
        axis.text.x = element_text(size = 10,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.title = element_text(size = 10,face = 'bold'))  + 
  scale_x_continuous(breaks = seq(2007, 2017, by =1),limits = c(2007,2019.2),expand = c(0,0))+
  scale_y_continuous( limits = c(105,179))+
  geom_dl(aes(label = paste(' ',toTitleCase(group_names)), colour=group_names), method = list(dl.combine('last.bumpup'), cex = 0.75,fontface='bold'))


#Figure 13: new sales      - oem CO2 country comparison----

eu_group<-data.frame(  import("~/Desktop/Turkey- Tax System/Baseline Paper/r_input/eu_group.xlsx"))
group_co2_turkey_eu <-data.frame( group_co2%>% filter(year==2017)%>% mutate(country='Turkey'))
group_co2_turkey_eu<- rbind(group_co2_turkey_eu,eu_group) ;rm(eu_group)
group_co2_turkey_eu$country<- factor(group_co2_turkey_eu$country, levels = c('Turkey','EU'))
group_co2_turkey_eu$group_names<- str_wrap(group_co2_turkey_eu$group_names, width = 8) 
group_co2_turkey_eu$group_names <- factor(toTitleCase(group_co2_turkey_eu$group_names), levels= rev(c("Renault-\nNissan","Volkswagen\nGroup","FCA","Groupe\nPSA","Daimler","Ford","Toyota","BMW\nGroup","Hyundai","Honda")))
group_co2_turkey_eu_graph<- ggplot(group_co2_turkey_eu, aes(x=group_names,y=co2,group=country, fill=country))
group_co2_turkey_eu_graph+  geom_bar(stat = 'identity', position = 'dodge',width = 0.8)+theme_pvs_helvetica+
  labs(y=expression(bold('Average ' *CO[2]* ' emission values (g/km)')))+
  scale_fill_manual(values = c(Turkey=ICCTred,EU=ICCTblue))+
  theme( legend.position = c(0.85,0.87),
         legend.title = element_blank(), 
         legend.text = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),axis.title.y  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 8.5,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold')) +
  scale_y_continuous(limits=c(95,150),oob = rescale_none)+
  geom_text(data = group_co2_turkey_eu, aes(label=(round(co2))),position =position_dodge(width = 0.8),vjust=-0.2, color=ICCTgray,size=3,fontface='bold')+
  geom_line(y=120,linetype='dashed',color=ICCTred) +  geom_line(y=119,linetype='dashed',color=ICCTblue)



#Figure 14: new sales      - market share fuel----

scale_fill_fuel <- c( diesel=ICCTbrown,gasoline=ICCTblue,LPG=ICCTpurple,'hybrid-electric'=ICCTpurple,'battery-electric'=ICCTgreen)


fuel_share <- data.frame(na.omit(odd_pc%>% group_by(year,fuel_type) %>% summarise(total=sum(sales,na.rm=T)) %>% mutate(share=total/sum(total))))
fuel_share$fuel_type <-  case_when(
  fuel_share$fuel_type=='hybrid'~ 'hybrid-electric',
  fuel_share$fuel_type=='electric'~ 'battery-electric',
  TRUE ~ fuel_share$fuel_type
)
fuel_share <- ggplot(data=fuel_share, aes(year,share,color=fuel_type))
fuel_share +  geom_line(size=1.2)   + theme_pvs_helvetica + labs(y='Market shares')+
  scale_color_manual(values = scale_fill_fuel,guide='none')+
  theme( legend.position = 'none',
         legend.text = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),
         axis.title.y  = element_text(size = 11,face = 'bold'),
         axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold')) +
  scale_x_continuous(breaks = seq(2007, 2017, by =1), expand=c(0,0.05), limits = c(2007,2019.2))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by =0.2), limits = c(0,1.01),expand = c(0,0))+
  geom_dl(aes(x=year+0.1,y=ifelse(share<0.1,share+0.02,share),label = toTitleCase(fuel_type), colour=fuel_type), method = list(dl.combine('last.bumpup'), cex = 0.9,fontface='bold'))


#Figure 15: new sales      - oem diesel share country comparison----

group_eu_diesel_share<-data.frame(import('~/Desktop/Turkey- Tax System/Baseline Paper/r_input/eu_group_diesel_share.xlsx'))
group_diesel_share <- odd_pc %>% filter(year==2017) %>% 
  filter(group_names %in% c('BMW Group',"daimler","FCA","ford","honda","hyundai","Groupe PSA" ,"renault-nissan","toyota","Volkswagen Group"))%>%
  group_by(group_names,fuel_type)%>% summarise(total=sum(sales)) %>% mutate(share=total/sum(total)) %>% filter(fuel_type=='diesel') %>%
  select(group_names,share) %>% arrange(desc(share)) %>% mutate(country='Turkey')%>%data.frame()
group_diesel_share<- rbind(group_diesel_share,group_eu_diesel_share)
group_diesel_share$group_names<-  toTitleCase( str_wrap(group_diesel_share$group_names, width = 8))
group_diesel_share$group_names<- factor(group_diesel_share$group_names, levels = unique(group_diesel_share$group_names))
group_diesel_share$country<-factor(group_diesel_share$country, levels = c('Turkey','EU'))
ggplot(data = group_diesel_share, aes(x =group_names, y =share,fill=country)) +
  geom_bar(stat = "identity",position = 'dodge',width = 0.8) +labs(y="Diesel shares")+
  geom_line(y=0.61,linetype='dashed',color=ICCTred, aes(group=1)) +  
  geom_line(y=0.44,linetype='dashed',color=ICCTblue, aes(group=1))+
  scale_fill_manual(values = c(Turkey=ICCTred,EU=ICCTblue))+
  scale_y_continuous(limits = c(0,1.01),expand = c(0,0), label=scales::percent)+
  theme_pvs_helvetica+
  theme( legend.position = c(0.85,0.87),
         legend.title = element_blank(),
         legend.text = element_text(size = 10),
         plot.title = element_text(size = 12),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),
         axis.title.y  = element_text(size = 11,face = 'bold'),
         axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold'))+
  geom_text(data = group_diesel_share, aes(label=scales::percent(accuracy = 1, share)),position =position_dodge(width = 0.8),vjust=-0.2,hjust=0.45, color=ICCTgray,size=3,fontface='bold')
  
  
  
#Figure 16: new sales      - hybrid and electric sales----

ev_hev<-  data.frame(na.omit(odd_pc%>% filter(fuel_type%in% c("hybrid","electric"))%>% group_by(year,fuel_type) %>% summarise(total=sum(sales,na.rm=T)))) 
ev_hev$fuel_type <-  case_when(
  ev_hev$fuel_type=='hybrid'~ 'hybrid-electric',
  ev_hev$fuel_type=='electric'~ 'battery-electric'
)
ev_hev <- ggplot(data=ev_hev, aes(year,total,color=fuel_type))
ev_hev +     geom_line(size=1.2)   + theme_pvs_helvetica + 
  labs(y='New registrations')+
  scale_color_manual(values = scale_fill_fuel)+
  theme( legend.position = 'none',
         legend.text = element_text(size = 10),
         plot.title = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),
         axis.title.y  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold'))+
  scale_x_continuous(breaks = seq(2007, 2017, by =1), expand = c(0,0), limits = c(2009,2019.2))+
  scale_y_continuous(labels = scales::comma, limits = c(0,5100),expand = c(0,0))+
  geom_dl(aes(x=year+0.1,y=total+25,label = toTitleCase(fuel_type), colour=fuel_type), method = list(dl.combine('last.bumpup'), cex = 0.9,fontface='bold'))

  


#Figure 17: new sales      - HEV model sales----

hev_color<- c( 'Total HEV'=ICCTgray, 'C-HR' = ICCTblue, yaris=ICCTbrown,auris=ICCTred,rav4=ICCTorange,'CR-Z'=ICCTpurple,ioniq=ICCTyellow,niro=ICCTgreen)

hev_model<-  data.frame(na.omit(odd_pc%>% filter(fuel_type%in% c("hybrid"))%>% group_by(year,make,model) %>% summarise(total=sum(sales,na.rm=T)))) 
hev_total<- hev_model%>% group_by(year) %>% summarise(total=sum(total))
hev_total$model<- 'Total HEV'
hev_total$make<-''
hev_total<- select(hev_total, year,make,model,total)
hev_model<- rbind(hev_model,hev_total)
hev_model[nrow(hev_model) + 1,] = list("2016",'toyota',"rav4","0") 
hev_model[nrow(hev_model) + 1,] = list("2016",'toyota',"auris","0") 
hev_model[nrow(hev_model) + 1,] = list("2016",'hyundai', "ioniq","0")
hev_model[nrow(hev_model) + 1,] = list("2016",'kia',"niro","0")
hev_model[nrow(hev_model) + 1,] = list("2016",'honda',"cr-z hybrid","0")
hev_model[nrow(hev_model) + 1,] = list("2017",'honda',"cr-z hybrid","0")
hev_model$total<- as.numeric(hev_model$total)
hev_model$year<- as.numeric(hev_model$year);rm(hev_total)
hev_model$is.total<- ifelse(hev_model$model=='Total HEV','yes','no')
hev_model[hev_model=='c-hr'] <-'C-HR' 
hev_model[hev_model=='cr-z hybrid'] <-'CR-Z' 
hev_model_graph <- ggplot(data=filter(hev_model,model %in% c('Total HEV','C-HR','yaris','ioniq','CR-Z','rav4','auris','niro')), aes(year,total,color=model,linetype=is.total))
hev_model_graph +  geom_line(size=1.1)   + theme_pvs_helvetica + labs(y='New registrations')+
  theme( legend.position = 'none',
         legend.text = element_text(size = 10),
         plot.title = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),
         axis.title.y  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold'))+
  scale_color_manual(values = hev_color)+ 
  scale_x_continuous(breaks = seq(2007, 2017, by =1),limits = c(2009,2018.5),expand = c(0,0))+
  scale_y_continuous(limits = c(0,5000), labels = scales::comma, expand = c(0,0))+
  geom_dl(aes(x=year+0.1,y=ifelse(total<1000,total+100,total), label = paste(toTitleCase(make), toTitleCase(model)), colour=model), method = list(dl.combine('last.bumpup'), cex = 0.7,fontface='bold'))
  

#Figure 18: new sales      - BEV model sales----

ev_color<- c( fluence=ICCTgreen,i3=ICCTblue,zoe=ICCTpurple,'Total-BEV'=ICCTgray)

ev_model<-data.frame(na.omit(odd_pc%>% filter(fuel_type%in% c("electric"))%>% group_by(year,make,model) %>% summarise(total=sum(sales,na.rm=T)))) 
ev_total<- ev_model%>% group_by(year) %>% summarise(total=sum(total))
ev_total$model<- 'Total-BEV'
ev_total$make <- ''
ev_total<- select(ev_total,year,make,model,total)
ev_model<- rbind(ev_model,ev_total);rm(ev_total)
ev_model<- arrange(ev_model,total)
ev_model$is.total <- ifelse(ev_model$model=='Total-BEV','yes','no')
ev_model[ev_model=='bmw']<- 'BMW'
ev_model <- ggplot(data=ev_model, aes(year,total,color=model,linetype=is.total))
ev_model +  geom_line(size=1.1)   + 
  theme_pvs_helvetica + 
  labs(y='New registrations')+
  theme( legend.position = 'none',
         legend.text = element_text(size = 10),
         plot.title = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),
         axis.title.y  = element_text(size = 11,face = 'bold'),
         axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold'))+
  scale_color_manual(values =ev_color )+
  scale_x_continuous(breaks = seq(2012, 2017, by =1),limits = c(2012,2018),expand = c(0,0))+
  scale_y_continuous(limits = c(0,200), labels = scales::comma,expand = c(0,0))+
  geom_dl(aes(x=year+0.05,y=ifelse(total<10,total+4,total), label = paste(toTitleCase(make), toTitleCase(model)), colour=model), method = list(dl.combine('last.bumpup'), cex = 0.7,fontface='bold'))


#Figure 19: new sales      - country engine displacement comparison----
turkey_vs_eu <- import('~/Desktop/Turkey- Tax System/Baseline Paper/r_input/Turkey_vs_EU.xlsx')
turkey_vs_eu <- ggplot(data=turkey_vs_eu, aes(year,engine_displacement,color=country))
turkey_vs_eu +  geom_line(size=1.2)   + 
  theme_pvs_helvetica + 
  labs(y=expression(bold('Engine displacement (in cm' ^3*')' )))+scale_color_manual(values = c(EU=ICCTblue,turkey=ICCTred))+
  theme(legend.position ='none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.text.y = element_text(size = 9,face = 'bold'), 
        axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm")) + 
  scale_x_continuous(limits=c(2007,2018),breaks = seq(2007, 2017, by =1),expand = c(0,0))+
  scale_y_continuous( limits = c(1400,1800))+ 
  geom_dl(aes(label =paste(' ',toTitleCase(country)), colour=country), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))


#Figure 20: new sales      - oem engine displacement by years----

scale_color_group <- c('renault-nissan'=ICCTbrown,'Volkswagen Group'=ICCTblue,'Groupe PSA'=g4,honda=gray1,FCA=ICCTorange,ford='#98b476',
                       'BMW Group'=ICCTred,daimler=darken(ICCTcream,1.3),hyundai=ICCTpurple,toyota=ICCTyellow)


oem_engine <- odd_pc  %>% group_by(year,group_names) %>% summarise(engine_displacement= weighted.mean(engine_size,sales,na.rm=T))
oem_engine <- filter(oem_engine,group_names %in% c('BMW Group',"daimler","FCA","ford","honda","hyundai","Groupe PSA" ,"renault-nissan","toyota","Volkswagen Group"))
oem_engine <- ggplot(data=oem_engine, aes(year,engine_displacement,color=group_names))
oem_engine +  geom_line(size=1.1)   + theme_pvs_helvetica + 
  labs(y=expression(bold('Engine displacement (in cm' ^3*')' )))+
  scale_color_manual(values = scale_color_group)+
  theme( legend.position = 'none',
         axis.text = element_text(size = 9,face = 'bold'),
         axis.title.y = element_text(size = 10,face = 'bold'),axis.title.x = element_blank(),
         plot.caption = element_text(size = 8,face='bold'),
         plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm')) + 

  scale_x_continuous(breaks = seq(2007, 2017, by =1), expand = c(0,0), limits = c(2007,2019.5))+
  scale_y_continuous( breaks = seq(1200, 2200, by =200), limits = c(1200,2200))+
  geom_dl(aes(x=year+0.1,label = toTitleCase(group_names), colour=group_names), method = list(dl.combine('last.bumpup'), cex = 0.75,fontface='bold'))


#Figure 21: new sales      - oem engine displacement by country -----
odd_pc%>% filter(year==2017) %>% group_by(group_names) %>% summarise(weighted.mean(engine_size,sales))

group_turkey_vs_eu <-  import(file = '~/Desktop/Turkey- Tax System/Baseline Paper/r_input/group_Turkey_vs_EU.xlsx')
group_turkey_vs_eu$group_names<- str_wrap(group_turkey_vs_eu$group_names,width = 8)
group_turkey_vs_eu$group_names <- factor(toTitleCase(group_turkey_vs_eu$group_names), levels= (c("Daimler","BMW\nGroup","Honda","Ford","Groupe\nPSA" ,"Toyota","Hyundai","Renault-\nNissan","Volkswagen\nGroup","FCA")))
group_turkey_vs_eu$country<- factor(group_turkey_vs_eu$country,levels = c('Turkey','EU'))
group_engine_displacement_graph<- ggplot(group_turkey_vs_eu, aes(x=group_names,y=engine_displacement,group=country, fill=country))
group_engine_displacement_graph+  geom_bar(stat = 'identity', position = 'dodge',width = 0.8)+theme_pvs_helvetica+
  scale_fill_manual(values = c(Turkey=ICCTred,EU=ICCTblue))+
  labs(y=expression(bold('Engine displacement (in cm' ^3*')' )))+
  theme( legend.position = c(0.85,0.87),
         legend.title = element_blank(),
         legend.text = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),axis.title.y  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 8,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold')) +
  scale_y_continuous(limits=c(1200,2000),oob = rescale_none)+
  geom_text(data = group_turkey_vs_eu, aes(label=(round(engine_displacement))),position =position_dodge(width = 0.8),vjust=-0.2, color=ICCTgray,size=3,fontface='bold')+
  geom_line(y=1483,linetype='dashed',color=ICCTred, aes(group=1)) +  
  geom_line(y=1582,linetype='dashed',color=ICCTblue, aes(group=1))
  


#note: eu averages found by this method: 
#eu$oem<-oem_masterdatabase$oem[match(eu$Manufacturer,oem_masterdatabase$make)]
#View(eu%>% group_by(oem)%>% summarise(engine_displacement=weighted.mean(Engine_ccm,total,na.rm=T),
#                                      engine_power=weighted.mean(Engine_kw,total,na.rm=T),
#                                     Kerbweight=weighted.mean(Kerbweight,total,na.rm=T))%>%
#       filter(oem %in% c("BMW","Daimler","Fiat","Ford","Honda","Hyundai","PSA","Renault","Toyota","Volkswagen")))


#Figure 22: new sales      - country engine power comparison-----
# converted from hp to kW 
# odd_pc%>% group_by(year)%>% summarise(weighted.mean(motor_power,sales))

turkey_vs_eu <- import('~/Desktop/Turkey- Tax System/Baseline Paper/r_input/Turkey_vs_EU.xlsx') 
turkey_vs_eu <- ggplot(data=turkey_vs_eu, aes(year,engine_power,color=country))
turkey_vs_eu +  geom_line(size=1.2)   + theme_pvs_helvetica + labs(y=expression(bold('Engine power (in kW)' )))+
  scale_color_manual(values = c(EU=ICCTblue,turkey=ICCTred))+
  theme(legend.position ='none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.text.y = element_text(size = 9,face = 'bold'), 
        axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm")) + 
  scale_x_continuous(limits=c(2007,2018),breaks = seq(2007, 2017, by =1),expand = c(0,0))+
  scale_y_continuous( limits = c(70,100))+ 
  geom_dl(aes(x=year+0.1,label =toTitleCase(country), colour=country), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))


#Figure 23: new sales      - oem engine power by years--

#scale_color_oem <- c(renault=ICCTbrown,volkswagen=ICCTblue,PSA=g4,honda=gray1,fiat=ICCTorange,ford=g2,
#                    BMW=ICCTred,daimler=darken(ICCTcream,1.3),hyundai=ICCTpurple,toyota=ICCTyellow)

#oem_power <- odd_pc  %>% group_by(year,oem) %>% summarise(motor_power= weighted.mean(motor_power,sales,na.rm=T)) %>% mutate(motor_power=motor_power*0.746)
#oem_power <- filter(oem_power,oem %in% c("bmw","daimler","fiat","ford","honda","hyundai","psa","renault","toyota","volkswagen"))
#oem_power[oem_power$oem%in% c('psa','bmw'),]$oem <- toupper(oem_power[oem_power$oem%in% c('psa','bmw'),]$oem)
#oem_power <- ggplot(data=oem_power, aes(year,motor_power,color=oem))
#oem_power +  geom_line(size=1.1)   + theme_pvs_helvetica + 
#  labs(y=expression(bold('Engine power (in KW)' )),caption='source: ODD')+
#  scale_color_manual(values = scale_color_oem)+
#  theme( legend.position = 'none',
#         axis.text = element_text(size = 9,face = 'bold'),
#         axis.title.y = element_text(size = 10,face = 'bold'),axis.title.x = element_blank(),
#         plot.caption = element_text(size = 8,face='bold'),
#         plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm')) + 
#  
#  scale_x_continuous(breaks = seq(2007, 2017, by =1), expand = c(0,0), limits = c(2007,2019))+
#  scale_y_continuous( breaks = seq(50, 140, by =20), limits = c(50,132))+
#  geom_dl(aes(label = paste(' ',toTitleCase(oem)), colour=oem), method = list(dl.combine('last.bumpup'), cex = 0.7,fontface='bold'))


#Figure 23: new sales      - oem engine power by country----
group_turkey_vs_eu <-  import(file = '~/Desktop/Turkey- Tax System/Baseline Paper/r_input/group_Turkey_vs_EU.xlsx')
group_turkey_vs_eu$group_names<- str_wrap(group_turkey_vs_eu$group_names,width = 8)
group_turkey_vs_eu$group_names <- factor(toTitleCase(group_turkey_vs_eu$group_names), levels= (c("BMW\nGroup","Daimler","Honda","Volkswagen\nGroup","Hyundai","Toyota","Ford","Groupe\nPSA","FCA","Renault-\nNissan")))
group_turkey_vs_eu$country<- factor(group_turkey_vs_eu$country,levels = c('Turkey','EU'))
group_engine_power_graph<- ggplot(group_turkey_vs_eu, aes(x=group_names,y=engine_power,group=country, fill=country))
group_engine_power_graph+  geom_bar(stat = 'identity', position = 'dodge',width = 0.8)+theme_pvs_helvetica+
  scale_fill_manual(values = c(Turkey=ICCTred,EU=ICCTblue))+
  labs(y=expression(bold('Engine power (in kW)')))+
  theme( legend.position = c(0.85,0.77),
         legend.title = element_blank(),
         legend.text = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),
         axis.title.y  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 8,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold')) +
  scale_y_continuous(limits=c(70,140),oob = rescale_none)+
  geom_text(data = group_turkey_vs_eu, aes(label=(round(engine_power))),position =position_dodge(width = 0.8),vjust=-0.2, color=ICCTgray,size=3,fontface='bold')+
  geom_line(y=86,linetype='dashed',color=ICCTred, aes(group=1)) +  
  geom_line(y=97,linetype='dashed',color=ICCTblue, aes(group=1))
  




#Figure 24: new sales      - oem gross vehicle mass by country----
group_turkey_vs_eu <-  import(file = '~/Desktop/Turkey- Tax System/Baseline Paper/r_input/group_Turkey_vs_EU.xlsx')
group_turkey_vs_eu$group_names<- str_wrap(group_turkey_vs_eu$group_names, width = 8) 
group_turkey_vs_eu$group_names <- factor(toTitleCase(group_turkey_vs_eu$group_names), levels= (c("BMW\nGroup","Daimler","FCA","Ford","Volkswagen\nGroup","Groupe\nPSA","Hyundai","Renault-\nNissan","Honda","Toyota")))
group_turkey_vs_eu$country<- factor(group_turkey_vs_eu$country,levels = c('Turkey','EU'))
group_gross_weight_graph<- ggplot(group_turkey_vs_eu, aes(x=group_names,y=mass_in_running_order,group=country, fill=country))
group_gross_weight_graph+  geom_bar(stat = 'identity', position = 'dodge',width = 0.8)+theme_pvs_helvetica+
  scale_fill_manual(values = c(Turkey=ICCTred,EU=ICCTblue))+
  labs(y=expression(bold('Mass in running order (in kg)' )))+
  theme( legend.position = c(0.85,0.87),
         legend.title = element_blank(),
         legend.text = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title.x = element_blank(),axis.title.y  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 8,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold')) +
  scale_y_continuous(limits=c(1200,1700),oob = rescale_none)+
  geom_text(data = group_turkey_vs_eu, aes(label=(round(mass_in_running_order))),position =position_dodge(width = 0.8),vjust=-0.2, color=ICCTgray,size=3,fontface='bold')+
  geom_line(y=1350,linetype='dashed',color=ICCTred, aes(group=1)) +  
  geom_line(y=1395,linetype='dashed',color=ICCTblue, aes(group=1))



# Fiat Panda vs Egea: 
# View(pb.pv.raw%>% filter(year==2017,OEM=='Fiat') %>% group_by(Model) %>% summarise(sum(total), weighted.mean(mass_running_order_average,total,na.rm=T)))

#pb.pv.raw %>% filter(year==2017, Country=='Turkey') %>% group_by(OEM)%>% summarise(mass=weighted.mean(mass_running_order_average,total,na.rm=T),weight=weighted.mean(Grossweight_Average, total,na.rm=T))
#pb.pv %>% filter(Year==2017) %>% group_by(OEM)%>% summarise(mass=weighted.mean(mro_avg,Total,na.rm=T),weight=weighted.mean(Grossweight_Average, Total,na.rm=T))
#ihs_2017$mass_running_order_average<-as.numeric(ihs_2017$mass_running_order_average)
#ihs_2017$Grossweight_Average<- as.numeric(ihs_2017$Grossweight_Average)
#ihs_2017  %>% group_by(OEM)%>% summarise(mass=weighted.mean(mass_running_order_average,total,na.rm=T))
