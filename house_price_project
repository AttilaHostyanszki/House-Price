# ============================================================
# STEP 1 - LOAD LIBRARIES
# ============================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(plotrix)
library(car)

# ============================================================
# STEP 2 - IMPORT DATA
# ============================================================

houses <- read_excel(file.choose(), sheet = 1)

View(houses)

# ============================================================
# STEP 3 - DATA INSPECTION
# ============================================================

str(houses)

colSums(is.na(houses))

sum(duplicated(houses))

# ============================================================
# STEP 4 - CENTRAL TENDENCY AND DISPERSION
# ============================================================

summary(houses)

sapply(houses[, c("House_Price","Square_Footage","Num_Bedrooms","Lot_Size")],sd)

# ============================================================
# STEP 5 - HISTOGRAM OF HOUSE PRICES (UNCHANGED)
# ============================================================

ggplot(houses, aes(x = House_Price)) + geom_histogram(fill = "blue",bins = 30,color = "black") + annotate("rect",xmin = 400000,xmax = 900000,ymin = 0,ymax = Inf,fill = "orange",alpha = 0.25) + scale_x_continuous(breaks = seq(0, 1400000, by = 200000),labels = function(x) paste0(x/1000, "k")) + labs(title = "Distribution of House Prices",x = "House Price",y = "Number of Houses")

# ============================================================
# STEP 6 - BOXPLOT OF HOUSE PRICES
# ============================================================

ggplot(houses, aes(y = House_Price)) + geom_boxplot(fill = "green") + scale_y_continuous(breaks = seq(0, 1400000, by = 200000),labels = function(x) paste0(x/1000,"k")) + labs(title = "Boxplot of House Prices",y = "House Price") + theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())

# ============================================================
# STEP 7 - SCATTER PLOT WITH REGRESSION LINE (UNCHANGED)
# ============================================================

ggplot(houses, aes(x = Square_Footage, y = House_Price)) + geom_point(color = "orange",alpha = 0.6) + geom_smooth(method = "lm",color = "blue",se = FALSE) + scale_y_continuous(breaks = seq(0, 1400000, by = 200000),labels = function(x) paste0(x/1000, "k")) + labs(title = "Relationship between House Size and Price",x = "Square Footage",y = "House Price")

# ============================================================
# STEP 8 - PRICE BY BEDROOMS
# ============================================================

ggplot(houses, aes(x = factor(Num_Bedrooms), y = House_Price)) + geom_boxplot(fill = "turquoise") + scale_y_continuous(breaks = seq(0,1400000,by=200000),labels=function(x) paste0(x/1000,"k")) + labs(title="House Prices by Number of Bedrooms",x="Number of Bedrooms",y="House Price")

# ============================================================
# STEP 9 - FACETED SCATTER (UNCHANGED)
# ============================================================

ggplot(houses, aes(x = Square_Footage, y = House_Price)) + geom_point(color = "blue", alpha = 0.6) + geom_smooth(method = "lm", color = "orange", se = FALSE) + scale_y_continuous(breaks = seq(0, 1400000, by = 200000),labels = function(x) paste0(x/1000, "k")) + labs(title = "Relationship Between House Size and Price by Number of Bedrooms",x = "Square Footage",y = "House Price") + facet_wrap(~ Num_Bedrooms, scales = "free_x")

# ============================================================
# STEP 10 - AVERAGE PRICE BY BEDROOMS
# ============================================================

ggplot(houses, aes(x=factor(Num_Bedrooms), y=House_Price)) + stat_summary(fun=mean, geom="bar", fill="brown") + stat_summary(fun=mean,geom="text",aes(label=paste0(round(after_stat(y)/1000),"k")),vjust=-0.5,size=4) + scale_y_continuous(breaks=seq(0,1400000,by=200000),labels=function(x) paste0(x/1000,"k")) + labs(title="Average House Price by Bedrooms",x="Bedrooms",y="Average House Price")

# ============================================================
# STEP 11 - PIE CHART
# ============================================================

bedroom_counts <- table(houses$Num_Bedrooms)

percentages <- round(bedroom_counts / sum(bedroom_counts) * 100, 1)

labels <- paste0(names(bedroom_counts), " Bedrooms\n", percentages, "%")

pie(bedroom_counts,labels = labels,col = rainbow(length(bedroom_counts)),main = "Distribution of Houses by Number of Bedrooms")

# ============================================================
# STEP 12 – NUMBER OF HOUSES BY BEDROOMS
# ============================================================

ggplot(houses, aes(x = factor(Num_Bedrooms))) + geom_bar(fill = "skyblue", color = "black") + stat_count(geom = "text",aes(label = after_stat(count)),vjust = -0.5,size = 4) + labs(title = "Number of Houses by Bedrooms",x = "Number of Bedrooms",y = "Number of Houses")

# ============================================================
# STEP 13 - CORRELATION ANALYSIS
# ============================================================

houses_numeric <- houses[sapply(houses,is.numeric)]

correlation_matrix <- cor(houses_numeric)

correlation_matrix

correlation_matrix[, "House_Price"]

cor(houses$Square_Footage, houses$House_Price)

cor(houses$Square_Footage,houses$House_Price,method="spearman")

# ============================================================
# STEP 14 - NORMALITY TEST
# ============================================================

shapiro.test(houses$House_Price)

shapiro.test(houses$Square_Footage)

# ============================================================
# STEP 15 - REGRESSION MODEL
# ============================================================

model1 <- lm(House_Price ~ Square_Footage + Num_Bedrooms + Num_Bathrooms + Year_Built + Lot_Size + Garage_Size + Neighborhood_Quality,data=houses)

summary(model1)

# ============================================================
# STEP 16 - MULTICOLLINEARITY TEST
# ============================================================

vif(model1)

# ============================================================
# STEP 17 - COOK'S DISTANCE
# ============================================================

cooksD <- cooks.distance(model1)

plot(cooksD,type="h",main="Cook's Distance",ylab="Cook's Distance")

abline(h=4/length(cooksD), col="red")

# ============================================================
# STEP 18 - LEVERAGE VALUES
# ============================================================

plot(hatvalues(model1),main="Leverage Values",xlab="Observation Number",ylab="Leverage Value")

# ============================================================
# STEP 19 - DIAGNOSTIC PLOTS
# ============================================================

plot(model1)

# ============================================================
# STEP 20 - HOUSE PRICE PREDICTION
# ============================================================

test_data <- read_excel(file.choose(), sheet="Test")

View(test_data)

predicted_prices <- predict(model1, newdata=test_data)

test_data$Predicted_House_Price <- round(predicted_prices,0)

View(test_data)

test_data
