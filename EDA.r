
library(maps)
library(dplyr)
library(mice)
library(VIM)
library(pander)
library(lattice)
library(mltools)
library(car)
library(plyr)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(GGally)
library(corrplot)
library(Hmisc)
library(openintro)
knitr::opts_chunk$set(fig.width=12, fig.height=8) 

census2 = read.csv('acs2015_census_tract_data.csv', header = TRUE)

names(census2)

census = read.csv('acs2015_census_tract_data.csv', header = TRUE)
head(census)
summary(census)

census$State = factor(census$State)
census$County = factor(census$County)
missingdata= census[!complete.cases(census),]

census$Complete = complete.cases(census)
missing_percent = census %>% 
        dplyr::group_by(State) %>% 
        dplyr::summarise(total_count = n(), na_count = sum(!Complete), percent = sum(!Complete)/n())


tolower(missing_percent$State)

library('mapproj')

us_states <- map_data("state")
missing_percent$State = tolower(missing_percent$State)

temp = names(us_states)
temp[5] = "State"
names(us_states) = temp

us_states_missing <- left_join(us_states, missing_percent, by = "State")

missing_percent

options(repr.plot.width=10, repr.plot.height=4)
p1 <- ggplot(data = us_states_missing,
             mapping = aes(x = long, y = lat, group = group, fill = percent)) + 
      geom_polygon(color = "gray40", size = 0.2) +
      # coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
      scale_fill_gradient2(low = "#81C7D4",  high = "#F4A7B9", midpoint=0) + 
      # scale_fill_gradient(low = "white", high = "#FF0000") +
      #ggtitle("Fig1: Data Missing Ratio for each state") + 
      labs(fill = "Missing Percentage") + 
      theme_map() 

missing_percent_sorted = missing_percent[with(missing_percent, order(percent,decreasing = TRUE)),]
missing_percent_sorted$State = factor(missing_percent_sorted$State, levels = missing_percent_sorted$State )
p2 <- ggplot(data = missing_percent_sorted[1:10,], mapping = aes(x=State)) + 
    geom_bar(aes(weight = percent), fill="#81C7D4") + 
    #ggtitle("Fig1: Top 10 Missing States") + 
    ylab("Ratio of Rows Contain Missing Values")  + 
    theme_hc() + 
    theme(axis.text.x = element_text(angle = 90))

grid.arrange(p1, p2, widths=c(1.8,1))

missing_percent_sorted = missing_percent[with(missing_percent, order(percent,decreasing = TRUE)),]
missing_percent_sorted$State = factor(missing_percent_sorted$State, levels = missing_percent_sorted$State )
ggplot(data = missing_percent_sorted[1:10,], mapping = aes(x=State)) + 
    geom_bar(aes(weight = percent)) + 
    ggtitle("Fig1: Top 10 Missing States") + 
    ylab("Ratio of Rows Contain Missing Values")

census_aggr = aggr(census,col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
                   cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

for(i in 1:ncol(census)){
  census[is.na(census[,i]), i] <- mean(census[,i], na.rm = TRUE)
}
sum(!complete.cases(census))

attach(census)
census$Hispanic_pop = TotalPop*Hispanic/100
census$White_pop = TotalPop*White/100
census$Black_pop = TotalPop*Black/100
census$Native_pop = TotalPop*Native/100
census$Asian_pop = TotalPop*Asian/100
census$Pacific_pop = TotalPop*Pacific/100
census$Tot_Income = TotalPop*IncomePerCap
census$Poverty_pop = TotalPop*Poverty/100
census$ChildPoverty_pop = TotalPop*ChildPoverty/100
census$Professional_pop = TotalPop*Professional/100
census$Service_pop = TotalPop*Service/100
census$Office_pop = TotalPop*Office/100
census$Construction_pop = TotalPop*Construction/100
census$Production_pop = TotalPop*Production/100
census$Drive_pop = TotalPop*Drive/100
census$Carpool_pop = TotalPop*Carpool/100
census$WFH_pop = TotalPop*WorkAtHome/100
census$TotalCommute = TotalPop*MeanCommute
census$PrivateWork_pop = TotalPop*PrivateWork/100
census$PublicWork_pop = TotalPop*PublicWork/100
census$SelfEmployed_pop = TotalPop*SelfEmployed/100
census$Unemployment_pop = TotalPop*Unemployment/100

summed_census <- census %>%
dplyr::group_by(State) %>%
dplyr::summarise(sum(TotalPop), sum(Men), sum(Women), sum(Citizen),
          sum(Employed), sum(Hispanic_pop), sum(White_pop), sum(Black_pop), sum(Native_pop), sum(Asian_pop),
          sum(Pacific_pop), sum(Tot_Income), sum(ChildPoverty_pop), sum(Poverty_pop), sum(Professional_pop), sum(Service_pop), sum(Office_pop),
          sum(Construction_pop), sum(Production_pop), sum(Drive_pop), sum(Carpool_pop), sum(WFH_pop), sum(TotalCommute),
          sum(PrivateWork_pop), sum(PublicWork_pop), sum(SelfEmployed_pop), sum(Unemployment_pop))
detach(census)

attach(summed_census)
aggregated_data             <- data.frame(State) %>% group_by(State)

aggregated_data$Men         <- summed_census$"sum(Men)" / summed_census$"sum(TotalPop)"
aggregated_data$Women       <- summed_census$"sum(Women)" / summed_census$"sum(TotalPop)"
aggregated_data$Hispanic    <- summed_census$"sum(Hispanic_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$White       <- summed_census$"sum(White_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Black       <- summed_census$"sum(Black_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Native      <- summed_census$"sum(Native_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Asian       <- summed_census$"sum(Asian_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Pacific     <- summed_census$"sum(Pacific_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Citizen     <- summed_census$"sum(Citizen)" / summed_census$"sum(TotalPop)"
aggregated_data$IncomePerCap<- summed_census$"sum(Tot_Income)" / summed_census$"sum(TotalPop)"
aggregated_data$Pov         <- summed_census$"sum(Poverty_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$ChildPov    <- summed_census$"sum(ChildPoverty_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Professional<- summed_census$"sum(Professional_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Service     <- summed_census$"sum(Service_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Office      <- summed_census$"sum(Office_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Construction<- summed_census$"sum(Construction_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Production  <- summed_census$"sum(Production_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Drive       <- summed_census$"sum(Drive_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Carpool     <- summed_census$"sum(Carpool_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Employed    <- summed_census$"sum(Employed)" / summed_census$"sum(TotalPop)"
aggregated_data$WFH         <- summed_census$"sum(WFH_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$AVG_Commute <- summed_census$"sum(TotalCommute)" / summed_census$"sum(TotalPop)"
aggregated_data$Private_work<- summed_census$"sum(PrivateWork_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Public_work <- summed_census$"sum(PublicWork_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$SelfEmployed<- summed_census$"sum(SelfEmployed_pop)" / summed_census$"sum(TotalPop)"
aggregated_data$Unemployment<- summed_census$"sum(Unemployment_pop)" / summed_census$"sum(TotalPop)"

aggregated_data <- aggregated_data %>% ungroup(State)

detach(summed_census)

attach(aggregated_data)

us_states <- map_data("state")
head(us_states)
aggregated_data$State = tolower(State)
temp = names(us_states)
temp[5] = "State"
names(us_states) = temp
us_states_data <- left_join(us_states, aggregated_data, by = "State")
head(us_states_data)

options(repr.plot.width=10, repr.plot.height=6)

plot_map <- function (col_name, title, lab, midpoint=0.5){
ggplot(data = us_states_data,
             mapping = aes(x = long, y = lat, group = group, fill = get(col_name))) + 
      geom_polygon(color = "gray40", size = 0.2) +
      # coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
      scale_fill_gradient2(low = "#81C7D4",  high = "#F4A7B9", midpoint=midpoint) + 
      # scale_fill_gradient1(low = "white", high = "#FF0000") +
      # ggtitle(title) + 
      labs(fill = lab) + 
      theme_map() }

plot_map('Men', "Fig2: Male Ratio in different states", "Men Ratios")
plot_map('Women', "Fig2: Female Ratio in different states", "Female Ratios")
plot_map('Citizen', "Fig2: Citizen Ratio in different states", "Citizen Ratios")
plot_map('Employed', "Fig2: Employed Ratio in different states", "Employed Ratios")
plot_map('Unemployment', "Fig2: Unemployment Ratio in different states", "Unemployment Ratios")
plot_map('ChildPov', "Fig2: ChildPov Ratio in different states", "ChildPov Ratios")

plot_map('IncomePerCap', "Fig2: IncomePerCap Ratio in different states", "IncomePerCap Ratios")
plot_map('Pov', "Fig2: Pov Ratio in different states", "Pov Ratios")

plot_map('Hispanic', "Fig3: Hispanic Ratio in different states", "Hispanic Ratios")
plot_map('White', "Fig3: White Ratio in different states", "White Ratios")
plot_map('Black', "Fig3: Black Ratio in different states", "Black Ratios", midpoint = 0.1)
plot_map('Native', "Fig3: Native Ratio in different states", "Native Ratios")
plot_map('Asian', "Fig3: Asian Ratio in different states", "Asian Ratios")
plot_map('Pacific', "Fig3: Pacific Ratio in different states", "Pacific Ratios")

plot_map('Professional', "Fig3: Professional Ratio in different states", "Professional Ratios")
plot_map('Service', "Fig3: Service Ratio in different states", "Service Ratios")
plot_map('Office', "Fig3: Office Ratio in different states", "Office Ratios")
plot_map('Construction', "Fig3: Construction Ratio in different states", "Construction Ratios")
plot_map('Production', "Fig3: Production Ratio in different states", "Production Ratios")
plot_map('Drive', "Fig3: Drive Ratio in different states", "Drive Ratios")

plot_map('Carpool', "Fig3: Carpool Ratio in different states", "Carpool Ratios")
plot_map('WFH', "Fig3: WFH Ratio in different states", "WFH Ratios")
plot_map('AVG_Commute', "Fig3: AVG_Commute Ratio in different states", "AVG_Commute Ratios")
plot_map('Private_work', "Fig3: Private_work Ratio in different states", "Private_work Ratios")
plot_map('Public_work', "Fig3: Public_work Ratio in different states", "Public_work Ratios")
plot_map('SelfEmployed', "Fig3: SelfEmployed Ratio in different states", "SelfEmployed Ratios")

detach(aggregated_data)

killings = read.csv('police_killings_cleaned.csv', header = TRUE)

killings$gender = factor(killings$gender)
killings$raceethnicity = factor(killings$raceethnicity)
killings$day = factor(killings$day)
killings$city = factor(killings$city)
killings$cause = factor(killings$cause)
killings$armed = factor(killings$armed)
killings$county_bucket = factor(killings$county_bucket)
killings$nat_bucket = factor(killings$nat_bucket)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
killings$pov = as.numeric.factor(killings$pov)
killings$p_income = as.numeric.factor(killings$p_income)
killings$share_black = as.numeric.factor(killings$share_black)
killings$share_white = as.numeric.factor(killings$share_white)
killings$share_hispanic = as.numeric.factor(killings$share_hispanic)

missingdata= killings[!complete.cases(killings),]

killings_aggr = aggr(killings,col=mdc(1:2), numbers=TRUE, sortVars=TRUE, cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))


attach(killings)

state <- killings %>% dplyr::select(state)
killing_subset <- data.frame(killings %>%  dplyr::select(raceethnicity),apply(state, 1, abbr2state))

killing_counts <- killing_subset %>% 
dplyr::group_by(apply.state..1..abbr2state.) %>%
tally()

killing_counts$apply.state..1..abbr2state. = tolower(killing_counts$apply.state..1..abbr2state.)

county <- killings %>% dplyr::select(county_id)
killing_subset <- data.frame(killings %>%  dplyr::select(raceethnicity),apply(state, 1, abbr2state))

killing_counts <- killing_subset %>% 
dplyr::group_by(apply.state..1..abbr2state.) %>%
tally()

killing_counts$apply.state..1..abbr2state. = tolower(killing_counts$apply.state..1..abbr2state.)
detach(killings)

killing_counts

joined_data = merge(aggregated_data,killing_counts, by.x = 'State', by.y='apply.state..1..abbr2state.',all.x = TRUE)

killings

mu1 <- ddply(.data=killings, "raceethnicity", summarise, grp.mean=mean(age))
# Change density plot line colors by groups

p1<-ggplot(data=killings, aes(x=age, color=raceethnicity)) +
  geom_density()+
  # Add mean lines
  geom_vline(data=mu1, aes(xintercept=grp.mean, color=raceethnicity),
             linetype="dashed")

p2 = ggplot(data=killings,aes(raceethnicity)) +
  geom_bar(aes(fill = gender)) +
  scale_fill_brewer(palette="Paired")

p3 = ggplot(data=killings,aes(raceethnicity)) +
  geom_bar(aes(fill = cause)) +
  scale_fill_brewer(palette="Paired")

p4 = ggplot(data=killings,aes(raceethnicity)) +
  geom_bar(aes(fill = armed)) +
  scale_fill_brewer(palette="Paired")
grid.arrange(p1, p2, p3, p4, nrow = 2)

names(killings)
transpose(killings)

killings = read.csv('police_killings_cleaned.csv', header = TRUE)

levels(killings$raceethnicity) <- c(levels(killings$raceethnicity), "Other")
killings[killings['raceethnicity']!= "Black" & killings['raceethnicity']!= "White", 'raceethnicity'] = "Other"

killings

killings

killings_sorted = killings %>% 
  group_by(state) %>% 
  dplyr::summarise(total_count = n()) %>%
  arrange(desc(total_count))
killings_sorted$state

killings$state

killings$state = 
    factor(killings$state, levels = killings_sorted$state)

options(repr.plot.width=12, repr.plot.height=6)

ggplot(data = killings) +
    geom_bar(aes(state, fill=raceethnicity), alpha=0.65)+ 
    theme_hc() + 
      xlab("State") +
      ylab("Count")  +
      labs(fill="Races") + 
    theme(legend.position = c(0.95, 0.86)) 

killings[killings$cause == "Death in custody", "cause"]

levels(killings$cause)

levels(killings$cause) <- c(levels(killings$cause), "Custody", "Vehicle", "Other")
killings[killings$cause == "Death in custody", "cause"] = "Custody"
killings[killings$cause == "Struck by vehicle", "cause"] = "Vehicle"
killings[killings$cause == "Taser", "cause"] = "Other"
killings[killings$cause == "Unknown", "cause"] = "Other"

levels(killings$armed)

# levels(killings$cause) <- c(levels(killings$armed), "Custody", "Vehicle", "Other")
killings[killings$armed == "Disputed", "armed"] = "Other"
killings[killings$armed == "Non-lethal firearm", "armed"] = "Other"
killings[killings$armed == "Unknown", "armed"] = "Other"
killings[killings$armed == "Vehicle", "armed"] = "Other"

options(repr.plot.width=12, repr.plot.height=5)

g1 <- ggplot(data=killings,aes(raceethnicity)) +
  geom_bar(aes(fill = gender), alpha=0.75) +
  scale_fill_brewer(palette="Paired") + #+ 
  theme_hc() +
      xlab("Race") +
      ylab("Count")  +
      labs(fill="Gender") + 
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
  #theme(legend.position = c(1, 0.5))

g2 <- ggplot(data=killings,aes(raceethnicity)) +
  geom_bar(aes(fill = cause), alpha=0.75) +
  scale_fill_brewer(palette="Paired") + #+ 
  theme_hc() +
      xlab("Race") +
      ylab("Count")  +
      labs(fill="Cause") + 
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

g3 <- ggplot(data=killings,aes(raceethnicity)) +
  geom_bar(aes(fill = armed), alpha=0.75) +
  scale_fill_brewer(palette="Paired") + 
  theme_hc() +
      xlab("Race") +
      ylab("Count")  +
      labs(fill="Armed") + 
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
grid.arrange(g1,g2,g3, widths=c(1,1,1))

names(aggregated_data)

ggplot(data = joined_data, ) +
    geom_bar(aes(x = State, y = Black), stat="identity")

tmp = state.abb
state_abbr_names = c(tmp[1:8], 'D.C', tmp[9:38], 'PR', tmp[39:50])

new_joined = joined_data
new_joined$State = state_abbr_names
new_joined$Other = new_joined$Native + new_joined$Asian + new_joined$Pacific

options(repr.plot.width=12, repr.plot.height=6)
ggplot(data = new_joined) +
    #geom_point(aes(x = Black, y = n,)) + 
    geom_point(aes(x = Black, y = n, colour = IncomePerCap), size=3) + 
    geom_label(aes(x = Black, y = n, label = State, colour=IncomePerCap), nudge_y = 3, alpha=0.25) +
    labs(fill = "Income Per Capita") + 
    ylab("Number of Police Killings") + 
    xlab("Black Population Ratio") + 
    theme_hc()+ 
    theme(legend.position = c(0.9, 0.8))
          #legend.direction = "horizontal")

ggplot(data = joined_data) +
    #geom_point(aes(x = Black, y = n,)) + 
    geom_text(aes(x = White, y = n, label = State))

names(joined_data)

ggplot(data = joined_data) +
    geom_point(aes(x = Black, y = n, color=Employed)) 

ggplot(data = joined_data) +
    geom_point(aes(x = Black, y = n, color=Pov)) 



# census$State = tolower(census$State)

census_sorted_1 = census %>%
    dplyr::group_by(State) %>%
    dplyr::summarise(black_mean = mean(Black)) %>%
    arrange(-black_mean)
census_sorted_1$State

mathed_names = state.abb[match(census_sorted_1$State,state.name)]
mathed_names[1] = 'D.C.'
mathed_names[length(mathed_names)-2] = 'PR'

mathed_names

census$State = tolower(census$State)

census_sorted = census %>%
    dplyr::group_by(State) %>%
    dplyr::summarise(black_mean = mean(Black)) %>%
    arrange(-black_mean)
census_sorted$State

census$State <- factor(census$State, levels=census_sorted$State)

census_merged = merge(census, joined_data[, c("State", 'n')], by.x = 'State', by.y='State',all.x = TRUE)
ggplot(data=census_merged)+
    geom_boxplot(aes(x=State, y=Black, fill=n), outlier.alpha=0.05, outlier.color='#81C7D4') +
    scale_x_discrete(labels=mathed_names) + 
    theme_hc()+ 
    scale_fill_gradient(low = "#81C7D4",  high = "#F4A7B9") + 
    theme(legend.position = c(0.9, 0.8))

tmp_pop = census %>%
    dplyr::group_by(State) %>%
    dplyr::summarise(total = sum(TotalPop))

tmp = joined_data[, c("State", 'n')]
tmp[is.na(tmp)] = 0 
tmp$killing_ratio = tmp$n/tmp_pop$total

census_merged = merge(census, tmp , by.x = 'State', by.y='State',all.x = TRUE)
ggplot(data=census_merged)+
    geom_boxplot(aes(x=State, y=Black, fill=killing_ratio), outlier.alpha=0.05) +
    scale_x_discrete(labels=mathed_names) + 
    ylab("Black People Percentage") + 
    theme_hc()+ 
    scale_fill_gradient(low = "white",  high = "#F4A7B9") + 
    theme(legend.position = c(0.9, 0.8)) +
    labs(fill="Killing Ratios")
    theme(axis.text.x = element_text(size=7.5)) 

ggplot(data=census)+
    geom_boxplot(aes(x=State, y=Black), )

census$State = tolower(census$State)



new_joined = joined_data
new_joined$State = state_abbr_names
new_joined$Other = new_joined$Native + new_joined$Asian + new_joined$Pacific

new_joined$Other = new_joined$Native + new_joined$Asian + new_joined$Pacific

new_joined$Other 

options(repr.plot.width=10, repr.plot.height=4)
p2<-ggplot(new_joined) + 
    geom_density(aes(x=Black, fill="Black"), alpha=0.75) +
    geom_density(aes(x=White, fill="White"), alpha=0.75) + 
    geom_density(aes(x=Other, fill="Other"), alpha=0.40) + 
    labs(fill="Races Density") + 
    xlab("Ratio") + 
    theme(legend.position = c(0.75, 0.75))

p1<-plot_map('Black', "Fig3: Black Ratio in different states", "Black Ratios", midpoint = 0.1)
grid.arrange(p1, p2, widths=c(1.8,1))

# us_states <- map_data("state")
# head(us_states)
# aggregated_data$State = tolower(State)
# temp = names(us_states)
# temp[5] = "State"
# names(us_states) = temp
us_states_data_2 <- left_join(us_states, tmp, by = "State")
head(us_states_data_2)



options(repr.plot.width=12, repr.plot.height=4)
p1<-plot_map('Black', "Fig3: Black Ratio in different states", "Black Ratios", midpoint = 0.07)
p2 <- ggplot(data = us_states_data_2,
             mapping = aes(x = long, y = lat, group = group, fill = killing_ratio)) + 
      geom_polygon(color = "gray40", size = 0.2) +
      # coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
      scale_fill_gradient2(low = "#81C7D4",  high = "#F4A7B9", midpoint=0.65e-5) + 
      # scale_fill_gradient1(low = "white", high = "#FF0000") +
      # ggtitle(title) + 
      labs(fill = 'Killing Ratios') + 
      theme_map()
grid.arrange(p1, p2, widths=c(1,1))

options(repr.plot.width=12, repr.plot.height=4)
p1<-plot_map('Black', "Fig3: Black Ratio in different states", "Black Ratios", midpoint = 0.07)
p2 <- ggplot(data = us_states_data_2,
             mapping = aes(x = long, y = lat, group = group, fill = n)) + 
      geom_polygon(color = "gray40", size = 0.2) +
      # coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
      scale_fill_gradient2(low = "#81C7D4",  high = "#F4A7B9", midpoint=10) + 
      # scale_fill_gradient1(low = "white", high = "#FF0000") +
      # ggtitle(title) + 
      labs(fill = "Killing Counts") + 
      theme_map()
grid.arrange(p1, p2, widths=c(1,1))

killings = read.csv('police_killings_cleaned.csv', header = TRUE)

killings$gender = factor(killings$gender)
killings$raceethnicity = factor(killings$raceethnicity)
killings$day = factor(killings$day)
killings$city = factor(killings$city)
killings$cause = factor(killings$cause)
killings$armed = factor(killings$armed)
killings$county_bucket = factor(killings$county_bucket)
killings$nat_bucket = factor(killings$nat_bucket)

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
killings$pov = as.numeric.factor(killings$pov)
killings$p_income = as.numeric.factor(killings$p_income)
killings$share_black = as.numeric.factor(killings$share_black)
killings$share_white = as.numeric.factor(killings$share_white)
killings$share_hispanic = as.numeric.factor(killings$share_hispanic)

length(killings[rowSums(is.na(killings))>0, ])

levels(killings$raceethnicity)

length(killings[(killings$raceethnicity != 'Black') & (killings$raceethnicity != 'White'), ])

dim(killings[killings$raceethnicity == "Other", ])

96/467

levels(killings$raceethnicity) <- c(levels(killings$raceethnicity), "Other")
killings[killings['raceethnicity']!= "Black" & killings['raceethnicity']!= "White", 'raceethnicity'] = "Other"



killings$p_income = as.numeric(killings$p_income)
killings$h_income = as.numeric(killings$h_income)
killings$share_black = as.numeric(killings$share_black)

options(repr.plot.width=12, repr.plot.height=5)

p1 <- ggplot(data = killings) +
      geom_density(aes(x=h_income, fill=raceethnicity), alpha=0.55, color = NA) + 
      xlab("Household Income") +
      ylab("Density")  +
      labs(fill="Races") + 
      theme_hc()+ 
      theme(legend.position = c(0.90, 0.85))
p2 <- ggplot(data = killings) +
      geom_density(aes(x=age, fill=raceethnicity), alpha=0.55, color = NA) + 
      theme_hc()+ 
      xlab("Age") +
      ylab("Density")  +
      labs(fill="Races") + 
      theme(legend.position = c(0.90, 0.85))
grid.arrange(p1, p2, widths=c(1,1))

killings



options(repr.plot.width=12, repr.plot.height=6)

p1 <- ggplot(data = killings) +
      geom_point(aes(x=share_black, y=share_white, color=raceethnicity), alpha=0.85) +
      xlab("Black People Share for the Killing Region") +
      ylab("White People Share for the Killing Region")  +
      labs(color="Races") + 
      theme_hc()+ 
      theme(legend.position = c(0.90, 0.85))
p2 <- ggplot(data = killings) +
      geom_density2d(aes(x=share_black, y=share_white, color=raceethnicity), alpha=0.85, linemitre = 3)+
      xlab("Black People Share for the Killing Region") +
      ylab("White People Share for the Killing Region") +
      labs(color="Races") + 
      theme_hc()+ 
      theme(legend.position = c(0.90, 0.85))  
grid.arrange(p1, p2, widths=c(1,1))




