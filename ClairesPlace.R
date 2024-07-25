library(tidyverse)
library(moderndive)
library(infer)
library(GGally)
library(reshape2)

#Outlier testing

summary(SP23_Final_Claires_Donations_1_)
summary(SP23_Final_Email_Campaign_Report_1_)

Donations <- SP23_Final_Claires_Donations_1_
Email <- SP23_Final_Email_Campaign_Report_1_

Donations_num <- select_if(Donations, is.numeric)
Donations_cor <- round(cor(na.omit(Donations_num)), 2)
Donations_cor_melt <- melt(Donations_cor)

Donations_cor_melt %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", high="blue",
                       mid="white",midpoint=0) +
  geom_text(aes(x=Var1, y=Var2, label=value), size=3) +
  theme(axis.text.x=element_text(size=8, angle=30, hjust=0.8)) +
  labs(title="Correlation Matrix of Variables")

Email_num <- select_if(Email, is.numeric)
Email_cor <- round(cor(na.omit(Email_num)), 2)
Email_cor_melt <- melt(Email_cor)

Email_cor_melt %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="red", high="blue",
                       mid="white",midpoint=0) +
  geom_text(aes(x=Var1, y=Var2, label=value), size=3) +
  theme(axis.text.x=element_text(size=8, angle=30, hjust=0.8)) +
  labs(title="Correlation Matrix of Variables")

# Sends vs Opens
Email %>% 
  ggplot(aes(x=Sends, y=Opens)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "red")

# As the number of emails sent increases, so does the number 
# of times the email is opened. 

# Sends vs Mobile Open Rate
Email %>% 
  ggplot(aes(x=Sends, y=`Mobile Open Rate`)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "red")

# As the number of emails sent increases, the number 
# of times the email is opened on the mobile decreases. 

# Sends vs Desktop Open Rate
Email %>% 
  ggplot(aes(x=Sends, y=`Desktop Open Rate`)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "red")

# As the number of emails sent increases, the number 
# of times the email is opened on the desktop increases. 

# Sends vs Clicks
Email %>% 
  ggplot(aes(x=Sends, y=Clicks)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "red")

# As the number of emails sent increases, the number 
# of times the email is clicked increases. 

# Sends vs Bounces
Email %>% 
  ggplot(aes(x=Sends, y=Bounces)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "red")

# As the number of emails sent increases, the number 
# of times the email is bounced increases almost proportionally. 

# Sends vs Unsubscribes
Email %>% 
  ggplot(aes(x=Sends, y=Unsubscribes)) +
  geom_point(alpha = 0.8, color = "black") +
  geom_smooth(method = "lm", se= FALSE, color = "red")

# As the number of emails sent increases, the number 
# of unsubscribed emails increases. 


#Frequency and amount

SP23_Final_Claires_Donations_1_ %>%
  ggplot(aes(x=Amount,
         	y=Frequency, color=Amount)) +
	geom_point()
## Calculate the mean of amount for dedication
mean_by_category <- finaldonation %>%
  group_by(Dedication) %>%
  summarize(mean_amount = mean(Amount))

# Create the bar plot
ggplot(mean_by_category, aes(x = Dedication, y = mean_amount)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Average amount")

#mean of amount for match
  mean_by_Match <- finaldonation %>%
    group_by(Match) %>%
    summarize(meanmatch_amount = mean(Amount))
  
  ggplot(mean_by_Match, aes(x = Match, y = meanmatch_amount)) +
  geom_bar(stat = "identity") +
    labs(x = "Category", y = "Average amount")

#does region affect the amount of donations

ggplot(Donations, aes(x=Region, y=Amount)) +
  ggtitle("Comparison of Donations by Region") +
  geom_col(fill="purple")

#does state affect amount of donations 
#filter by US donations specifically 
USDonations <- filter(Donations, Region=="US")

#bar graph ggplot for state donations
#california is the most popular donation state 
#maybe host more events in California speciifcally + recognise CA donors
ggplot(USDonations, aes(x=State, y=Amount)) + 
  ggtitle("Amount of Donations Per State") +
  geom_col(fill="purple")

#which newsletter is most popular 
ggplot(Email_Campaign, aes(x=`Newsletter Link`, y=Opens)) + 
  geom_col(fill="purple") + 
  ggtitle("Newsletter popularity based on open rate") +

ggplot(Email_Campaign, aes(x=`Newsletter Link`, y=Clicks)) + 
  ggtitle("Newsletter popularity based on click Rate") +
  geom_col(fill="purple")

#H0: Having a dedication changes the amount donated
#H1: Having a dedication increases the amount donated

obs_dedication <- Donations %>% 
  specify(Amount~Dedication) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_dedication <- Donations %>% 
  specify(Amount~Dedication) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_dedication, obs_dedication, direction="right")

P-value: 0.108

#Fail to reject alternate hypothesis
#Having a dedication changes the amount donated

#H0: Being a donation match changes the amount donated
#H1: Being a donation match increases the amount donated

obs_match <- Donations %>% 
  specify(Amount~Match) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_match <- Donations %>% 
  specify(Amount~Match) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_match, obs_match, direction="right")

P-value: 0.138

#Fail to reject alternate hypothesis
#Having a dedication changes the amount donated

#H0: Anonymous or not amount donated is same
#H1: Anonymous(true) amount <anonymous(false) amount

obs_amount <- cleandata %>%
  specify(Amount~Anonymous) %>%
  calculate(stat="diff in means",
        	order=c("TRUE", "FALSE"))

#H0: Anonymous or not amount donated is same
#H1: Anonymous(true) amount <anonymous(false) amount

null_amount <- cleandata %>%
  specify(Amount~Anonymous) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type="permute") %>%
  calculate(stat="diff in means",
        	order=c("TRUE", "FALSE"))

get_p_value(null_amount, obs_amount,
        	direction="left")

##p-value is .007 which shows that people who are not anonymous on average donate more money.

Donations <- Donations %>% 
  mutate(USCat=ifelse(Region=="US", "TRUE", "FALSE"))

obs_dedicationUS <- Donations %>% 
  specify(Amount~USCat) %>% 
  calculate(stat="diff in props")

null_dedicationUS <- Donations %>% 
  specify(Amount~USCat) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in props")

get_p_value(null_dedicationR, obs_dedicationR, direction="right")


#H0: amount is not affected by recurring donor
#H1: Recurring(TRUE) amount < False amount
obs_rec <- SP23_Final_Claires_Donations_1_ %>%
  specify(Amount~Recurring) %>%
  calculate(stat="diff in means",
        	order=c("TRUE", "FALSE"))

null_rec <- SP23_Final_Claires_Donations_1_ %>%
  specify(Amount~Recurring) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type="permute") %>%
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_rec, obs_rec,
        	direction="left")
##p-value is zero
##amount donated by non-recurring donors is more than the amount donated by recurring donors—-actually not important



#H0: sequence is not affected by whether amount is matched or not
#H1: Anonymous (True) tip < Anonymous (false) tip
obs_tip<- SP23_Final_Claires_Donations_1_ %>%
  specify(`Operations Tip Amount`~Anonymous) %>%
  calculate(stat="diff in means",
        	order=c("TRUE", "FALSE"))

null_tip <- SP23_Final_Claires_Donations_1_ %>%
  specify(`Operations Tip Amount`~Anonymous) %>%
  hypothesize(null="independence") %>%
  generate(reps=1000, type="permute") %>%
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_tip, obs_tip,
        	direction="left")

#p-value of 0


#H0: Having a dedication changes the amount donated
#H1: Having a dedication increases the amount donated

obs_dedication <- Donations %>% 
  specify(Amount~Dedication) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_dedication <- Donations %>% 
  specify(Amount~Dedication) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_dedication, obs_dedication, direction="right")

#pvalue of 0.114

##
#Dataset is filtered with status being complete

DonationsFinal <- Donations %>% 
  filter(Status=="Complete")

obs_dedication <- DonationsFinal %>% 
  specify(Amount~Dedication) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_dedication <- DonationsFinal %>% 
  specify(Amount~Dedication) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_dedication, obs_dedication, direction="right")

#pvalue of 0.099

##

DonationsUST <- DonationsFinal %>% 
  mutate(USCat=ifelse(Region=="US", "TRUE", "FALSE")) %>% 
  filter(USCat=="TRUE") 


obs_dedication <- DonationsUST %>% 
  specify(Amount~Dedication) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_dedication <- DonationsUST %>% 
  specify(Amount~Dedication) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_dedication, obs_dedication, direction="right")

#p-value of 0.088

##

DonationsUSF <- DonationsFinal %>% 
  mutate(USCat=ifelse(Region=="US", "TRUE", "FALSE")) %>% 
  filter(USCat=="FALSE") 


obs_dedication <- DonationsUSF%>% 
  specify(Amount~Dedication) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_dedication <- DonationsUSF %>% 
  specify(Amount~Dedication) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_dedication, obs_dedication, direction="right")

#pvalue of 0.001

#H0: Being a donation match changes the amount donated
#H1: Being a donation match increases the amount donated

obs_match <- Donations %>% 
  specify(Amount~Match) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_match <- Donations %>% 
  specify(Amount~Match) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_match, obs_match, direction="right")

##

obs_matchF <- DonationsFinal %>% 
  specify(Amount~Match) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_matchF <- DonationsFinal %>% 
  specify(Amount~Match) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_matchF, obs_matchF, direction="right")

##


obs_matchUST <- DonationsUST %>% 
  specify(Amount~Match) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_matchUST <- DonationsUST %>% 
  specify(Amount~Match) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_matchUST, obs_matchUST, direction="right")

##

obs_matchUSF <- DonationsUSF%>% 
  specify(Amount~Match) %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

null_matchUSF <- DonationsUSF %>% 
  specify(Amount~Match) %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in means", order=c("TRUE", "FALSE"))

get_p_value(null_matchUSF, obs_matchUSF, direction="right")

#Linear Regression
#Amount Dedication for Filtered
lm_model <- lm(Amount ~ Dedication, data = DonationsFinal)

# View summary of the model
summary(lm_model)

# Plot the relationship between Amount and Dedication
ggplot(data = DonationsFinal, aes(x = Dedication, y = Amount)) +
  geom_point() +
  geom_smooth(method = "lm")

#H0: Having a match donations doesn't affect whether the donor is recurring
#H1: Having a match donation makes the donor more likely to be recurring

obs_Rmatch <- Donations %>% 
  specify(Recurring~Match, success = "TRUE") %>% 
  calculate(stat="diff in props", order=c("TRUE", "FALSE"))

null_Rmatch <- Donations %>% 
  specify(Recurring~Match, success="TRUE") %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in props", order=c("TRUE", "FALSE"))

get_p_value(null_Rmatch, obs_Rmatch, direction="right")

#p-value is 0.006


#Amount Dedication Unfiltered
lm_modelUnfiltered <- lm(Amount ~ Dedication, data = Donations)

# View summary of the model
summary(lm_model)

# Plot the relationship between Amount and Dedication
ggplot(data = Donations, aes(x = Dedication, y = Amount)) +
  geom_point() +
  geom_smooth(method = "lm")

#US VS International -> Linear Regression
lm_model_UST <- lm(Amount ~ Match, data = DonationsUST)
lm_model_USF <- lm(Amount ~ Match, data = DonationsUSF)

# View summary of the models
summary(lm_model_UST)
summary(lm_model_USF)

# Plot the relationship between Amount and Match for each dataset
ggplot(data = DonationsUST, aes(x = Match, y = Amount)) +
  geom_point() +
  geom_smooth(method = "lm")
  
ggplot(data = DonationsUSF, aes(x = Match, y = Amount)) +
  geom_point() +
  geom_smooth(method = "lm")

Bar Graph for Campaigns number donations
donations_by_campaign <- finaldonation %>%
  group_by(`Email Campaign`) %>%
  summarise(NumDonations = n())

ggplot(data = donations_by_campaign, aes(x = `Email Campaign`, y = NumDonations)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Email Campaign") +
  ylab("Number of Donations") +
  ggtitle("Number of Donations by Email Campaign") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Bar Graph for Campaigns amount donations
# Sum the total amount of money raised for each campaign
amountdonations_by_campaign <- finaldonation %>%
  group_by(`Email Campaign`) %>%
  summarize(TotalAmount = sum(Amount))

# Create bar plot of total amount raised by campaign
ggplot(data = amountdonations_by_campaign, aes(x = `Email Campaign`, y = TotalAmount)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Email Campaign") +
  ylab("Total Amount Raised") +
  ggtitle("Total Amount Raised by Email Campaign") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#filters

DonationsFinal <- Donations %>% 
  filter(Status=="Complete")

DonationsUST <- DonationsFinal %>% 
  mutate(USCat=ifelse(Region=="US", "TRUE", "FALSE")) %>% 
  filter(USCat=="TRUE") 

DonationsUSF <- DonationsFinal %>% 
  mutate(USCat=ifelse(Region=="US", "TRUE", "FALSE")) %>% 
  filter(USCat=="FALSE") 

#H0: Having a match donations doesn't affect whether the donor is recurring
#H1: Having a match donation makes the donor more likely to be recurring

obs_Rmatch <- Donations %>% 
  specify(Recurring~Match, success = "TRUE") %>% 
  calculate(stat="diff in props", order=c("TRUE", "FALSE"))

null_Rmatch <- Donations %>% 
  specify(Recurring~Match, success="TRUE") %>% 
  hypothesize(null="independence") %>% 
  generate(reps=1000, type="permute") %>% 
  calculate(stat="diff in props", order=c("TRUE", "FALSE"))

get_p_value(null_Rmatch, obs_Rmatch, direction="right")

#p-value is 0.006

# Calculate open rate and click rate
finalemail <- finalemail %>%
  mutate(Open_Rate = (Opens / Sends) * 100,
         Click_Rate = (Clicks / Opens) * 100)

# Identify top campaigns by open rate
top_open_campaigns <- finalemail %>%
  arrange(desc(Open_Rate)) %>%
  select(`Campaign Name`, Open_Rate) %>%
  head(10)

# Identify top campaigns by click rate
top_click_campaigns <- finalemail %>%
  arrange(desc(Click_Rate)) %>%
  select(`Campaign Name`, Click_Rate) %>%
  head(10)

#To test different subject lines and determine which ones lead to higher open rates, you can use A/B testing. Divide your subscriber list into two groups, and send each group a different email with a different subject line. Compare the open rates for each group to determine which subject line was more effective.

#unsubscribe
finalemail$Unsubscribe_Rate <- finalemail$Unsubscribes / finalemail$Sends

# Identify campaigns with highest unsubscribe rates
top_unsub <- finalemail %>% 
  select(`Campaign Name`, Unsubscribe_Rate) %>% 
  arrange(desc(Unsubscribe_Rate)) %>% 
  head(10)

#Plotting Rates
ggplot(top_click_campaigns, aes(x = `Campaign Name`, y = Click_Rate)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 5 Campaigns with Highest Click Rate", x = "Campaign Name", y = "Click Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(top_open_campaigns, aes(x = `Campaign Name`, y = Open_Rate)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 5 Campaigns with Highest Open Rate", x = "Campaign Name", y = "Open Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(top_unsub, aes(x = `Campaign Name`, y = Unsubscribe_Rate)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 5 Campaigns with Highest Unsubscribe Rate", x = "Campaign Name", y = "Unsubscribe Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# to visualize outliers better 

Donations %>% 
  ggplot(aes(x=Date, y=Amount)) + 
  geom_point(alpha=0.8, color="purple") + 
  geom_smooth(method="lm", se=FALSE, color="black")


ggplot(Donations, aes(x=Amount, y=Date)) +
  geom_boxplot() 
  
