####Peer graded assignment ######
dd <- read.csv('ities_short.csv',
                stringsAsFactors = F,
                header = T)
colSums(is.na(dd))
library(tidyr)
tax <- replace_na(dd$Tax, mean(dd$Tax,na.rm = TRUE))
sum(is.na(tax))
total_due <- replace_na(dd$TotalDue,median(dd$TotalDue, na.rm = TRUE))
sum(is.na(total_due))
library(dplyr)
dd_miss2 <- dd %>% filter(!is.na(Tax)) %>% filter(!is.na(CardholderName))
colSums(is.na(dd_miss2))
dd <- dd %>% mutate(LineItem_LongName = ifelse(nchar(LineItem) > mean(nchar(LineItem)),1,0))
cross_tab <- xtabs(~Department +LineItem_LongName, data = dd)
department <- rownames(cross_tab)
lineitem_longname <- colnames(cross_tab)
dd1 <- expand.grid(department,lineitem_longname)
Count <- as.vector(cross_tab)
dd1$Count <- Count
library(ggplot2)
p1 <- ggplot(data = dd1, aes(x=Var1, y = factor(Var2), size = Count))
p2 <- p1 + geom_point(col = "red") + labs(x = "Department", y = "LineItem_LongName")
p2
mean(dd$Price)
dd1 <- dd %>% filter(Price >= mean(Price))  %>% tail(dd, n = 5) 
length(unique(dd[["CashierName"]]))
#install.packages("esquisse")
dd2 <- dd %>% select(CashierName,Quantity) %>% arrange(desc(Quantity)) %>% group_by(CashierName) 
dd3 <- dd2 %>% head(dd2, n=10)
dd3
dd4 <- dd %>% select(Department, Month)
dd4$AvgPrice <- mean(dd$Price)
dd4$AvgQuantity <- mean(dd$Quantity)
dd4$AvgCost <- mean(dd$Cost)
dd4

                                                       




#dd_piped <- dd %>% select(Department) %>%  filter(Department == "Beverage")