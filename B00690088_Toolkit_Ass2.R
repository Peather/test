#Title:  Directing efforts to improve composting practices in Canada.

#Introduction:Of the 12.9 million tonnes of waste produced by Canadians annually, 8.5 million
#are sent to waste disposal facilities while 4.4 million are recycled or composted by waste 
#diversion programs. Composting allows food waste to be decomposed aerobically generating 
#carbon dioxide. Food waste that decomposes anaerobically in landfills generates methane, a 
#much more potent greenhouse gas that traps 25 times as much heat as CO2.

#Considering the critical state of Earth’s climate and the dire need to bring net greenhouse 
#gas emissions to zero in the next thirty years as outlined by the IPCC report in October 
#2018, humans will need to take every available measure to reach that target.

#Not only does composting reduce the amount of waste sent to landfills and the greenhouse 
#gases emitted by waste management facilities, but the compost product can be highly valuable 
#as fertilizer. The use of synthetic fertilizer has increased dramatically with industrial 
#agriculture and the depletion of soil quality from climate change and monocultures in 
#particular. Using compost as fertilizer is advantageous in the quest to reduce greenhouse
#gas emissions by eliminating those released from the production of synthetic fertilizer, but 
#it has also been demonstrated to boost soil quality immensely. Synthetic fertilizers have
#been linked to severe degradation of the aquatic ecosystems into which they leach, causing 
#eutrophication and subsequent anoxic dead zones. The addition of trace minerals and organic 
#carbon to soils with compost increases their ability to sequester carbon dioxide and 
#increases its structural stability which decreases erosion and harmful leachate into 
#watersheds.

#Composting participation in Canada has risen substantially over the last two decades,
#particularly in municipalities that have developed composting programs such as curb side 
#pickup, waste management centres and wet/dry waste separation. Curbside collection services
#were most widely used in Census metropolitan areas, where they are most widely available. 
#However, a variety of options exist for composting in urban and rural areas, and the
#practicality of each one depends primarily on volume of organic waste, space availability, 
#and income.

#The most prominent issues people find with composting are related to limited space and 
#the attraction of pests. A 2011 national census found that composting practices were 
#directly related to dwelling type. Income was also shown to have a large influence on 
#composting behavior, as well as education and tenure of the property (Table 1).

#Composting rates in single dwellings were over two times higher than those in apartments.
#In order to increase composting participation, efforts should be targeted to creating 
#programs for demographics demonstrating the lowest participation. This analysis examines 
#whether provinces with poor composting participation have lower proportions of single 
#dwellings, which might indicate a need for composting programs tailored to facilitating 
#composting for apartment dwellers.

#Methods
library(dplyr)
library(rvest)
library(ggplot2)

#Grab Website
HoCompost <- read_html("https://www150.statcan.gc.ca/n1/pub/16-002-x/2013001/article/11848-eng.htm")

#Select provincial compost data and format to numeric values that can be used in calculations
Messy <- HoCompost %>% html_nodes('table')  %>% .[[2]] %>% html_table(fill=TRUE)

prov.cdata <- Messy[c(4:13),c(1,3,4)]
colnames(prov.cdata) <- c("Province","Composted_kitchen_waste","Composted_yard_waste")
prov.cdata[,2]<-as.numeric(prov.cdata[,2])
prov.cdata[,3]<-as.numeric(prov.cdata[,3])

#find the total per cent of composters in each province using the average proportions of yard and kitchen composters
prov.cdata$total<-rowSums(prov.cdata[,c(2:3)])/2
prov.cdata

#Select and display provincial dwelling data and remove commas in data values by looping gsub through the dataframe

Promes <- read_html("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/td-tl/Table.cfm?Lang=Eng&T=101&SR=1&S=1&O=A&RPP=25")
Promes <- Promes %>% html_nodes('table')  %>% .[[1]] %>% html_table(fill=TRUE)

Promes <- Promes[c(4:13),c(1,3:9)]
colnames(Promes)<- c("Province","Single_detached_house","Apartment_five_or_more_storeys",
                      "Apartment_under_five_storeys",
                      "Row_house","Semi_detached house","Flat_duplex","Other_single_attached_house" )


for (i in 2:8){Promes[,i] <- as.numeric(gsub(",","",Promes[,i]))}
Promes

#Find sum of apartment building vs. house style dwellings and calculate the proportion that live in houses
#House style dwellings include single detached, semi-detached row and other single attached houses as well
#as Flat duplexes while buildings inlcude all apartments

buildings <- rowSums(Promes[,c(3,4)])
houses <- rowSums(Promes[,c(2,5:8)])

Provdwell<-data.frame("Province"= Promes$Province,"Buildings"= buildings,"Houses" = houses)

Provdwell$Prop.houses <- Provdwell$Houses/rowSums(Provdwell[,c(2:3)])
Provdwell

final<-left_join(Provdwell, prov.cdata)
final$Province<-as.factor(final$Province)

ggplot(final, aes(Prop.houses,total, shape=Province))+
  scale_shape_manual(values=c(1, 2,3,4,5,6,7,8,9,10))+
  labs(x="% House Dwellers", y="% Composters")+
  geom_point()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"))

##Compare provincial per capita GDP with per cent composters

GDP<-read_html("https://en.wikipedia.org/wiki/List_of_Canadian_provinces_and_territories_by_gross_domestic_product#GDP_and_per_capita_GDP")
GDP <- GDP %>% html_nodes('table')  %>% .[[1]] %>% html_table(fill=TRUE)

for(i in 2:11){GDP[i,5]<-strsplit(GDP[,5],'♠')[[i]][2]}

GDP[,5]<-as.numeric(gsub(",","",GDP[,5])) 

GDP<-GDP[2:11,c(1,5)]
colnames(GDP)<-c("Province","Percap.GDP")

final<-left_join(GDP,final)

ggplot(final, aes(Percap.GDP,total, shape=Province))+
  scale_shape_manual(values=c(1, 2,3,4,5,6,7,8,9,10))+
  geom_point()+
  labs(x="Per Capita GDP($)", y="% Composters",caption="Figure 2. Per capita GDP vs Proportion of composters by province")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#Discussion:
#Plotting proportion of single dwellings to composting participation for each province showed a positive linear relationship 
#for Nova Scotia, Prince Edward Island, Ontario, British Columbia and Quebec. Since British Columbia and Quebec had relatively
#low proportions of single dwelling houses and composters, efforts to increase composting participation in those provinces 
#might benefit from focusing on developing programs for apartment dwellers.The other five provinces had relatively high 
#proportions of single dwellings yet had low composting participation. Those deviances from the positive linear trend do not 
#support Statistics Canada’s conclusion that a direct relationship exists between dwelling type and composting participation. 
#They indicate that factors other than dwelling type have more influence over composting behavior in those provinces. 

#Statistics Canada identified income as the second most influential factor on composting participation. When per capita GDP 
#was used as a proxy for household income for each province and plotted against composting activities, Quebec, New Brunswick, 
#Manitoba, British Columbia and Ontario seemed to exhibit a positive linear relationship between them. The apparent 
#relationship may indicate that income is more influential for composting participation in those provinces, particularly in 
#New Brunswick and Manitoba that did not appear to be influenced by proportion of single dwelling types. In those provinces 
#efforts to improve composting participation may also consider focusing on making it more accessible to low income families.

#Nova Scotia and Prince Edward Island exhibited the greatest composting activity, while Quebec and Newfoundland composted the
#least. The stringency of waste management regulations in those provinces have been proposed to explain the differences. The 
#stringency of management regulations was not measured by stats Canada, but that would likely be a very influential factor for
#composting participation across all provinces because laws work. 

#This study had a number of limitations that MAY have affected the integrity of the results. Firstly, the years in which 
#dwelling type data, composting data and GDP data were collected differed by seven years. Secondly, per capita GDP was not the
#most accurate proxy for household income, but due to limited time and skills of the analyst, the Stats Canada table for 
#household income was not feasible for use in this study. Thirdly, the analysis could have been more precise and informative
#with a municipal scope rather than provincial, since composting programs tend to operate on municipalities. Finally, positive
#linear relationships are weak evidence for causal relationships. More in-depth regression analyses would be needed to give 
#validity to this study.

#As a signatory of the Paris agreement, Canada has a leading role to play in reduction of greenhouse gases. It is therefore 
#necessary that all efforts be taken to optimize waste management and composting practices. Household composting allows
#civilians to do their part to reduce their carbon footprint, and composting is a daily reminder to protect the planet.
