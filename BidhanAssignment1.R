dataset <- read.csv("train.csv", header = TRUE, stringsAsFactors = TRUE)
head(dataset)
summary(dataset)
library(plyr)
dataset$Dependents <- revalue(dataset$Dependents, c("3+"="3"))

# Count missing values in each column
missing_count <- colSums(is.na(dataset))
print(missing_count)

library(naniar)
library(ggplot2)

gg_miss_var(dataset) +
  theme_minimal() +
  labs(title = "Missing Data Pattern",
       x = "Variables",
       y = "Number of Missing Values")

#distribution
hist(dataset$LoanAmount, 
     main="Histogram for Loan Amount", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,700),
     breaks=20)
boxplot(dataset$LoanAmount, col='maroon',xlab = 'LoanAmount', main = 'Box Plot for Loan Amount')



hist(dataset$ApplicantIncome, 
     main="Histogram for Applicant Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,80000),
     breaks=50)
boxplot(dataset$ApplicantIncome, col='maroon',xlab = 'ApplicantIncome', main = 'Box Plot for Applicant Income')


# Load the required library
library(ggplot2)

# Create the density plot with customizations
ggplot(data=dataset, aes(x=LoanAmount, fill=Education)) +
  geom_density(alpha=0.6) +
  facet_grid(Education ~ .) +
  scale_fill_manual(values=c("Bachelor"="#1f78b4", "Master"="#33a02c", "Not Graduate"="#e31a1c")) +
  labs(title="Density Plot of Loan Amount by Education Level",
       subtitle="Comparison of Loan Amount Distributions across Different Education Levels",
       x="Loan Amount",
       y="Density",
       fill="Education Level") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face="bold", hjust=0.5, size=20, color="#4e4d4d"),
    plot.subtitle = element_text(hjust=0.5, size=14, color="#4e4d4d"),
    axis.title = element_text(face="bold", color="#4e4d4d"),
    axis.text = element_text(color="#4e4d4d"),
    legend.position = "bottom",
    legend.title = element_text(face="bold", size=12),
    legend.text = element_text(size=10)
  )



# Load the required libraries
library(ggplot2)
library(gridExtra)

# Define a custom color palette
custom_colors <- c("#66c2a5", "#fc8d62")

# Create individual bar plots
plot1 <- ggplot(data=dataset, aes(x=Gender, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Gender", x="Gender", y="Count") +
  theme_minimal()

plot2 <- ggplot(data=dataset, aes(x=Education, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Education", x="Education", y="Count") +
  theme_minimal()

plot3 <- ggplot(data=dataset, aes(x=Married, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Married", x="Married", y="Count") +
  theme_minimal()

plot4 <- ggplot(data=dataset, aes(x=Self_Employed, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Self Employed", x="Self Employed", y="Count") +
  theme_minimal()

plot5 <- ggplot(data=dataset, aes(x=Property_Area, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Property Area", x="Property Area", y="Count") +
  theme_minimal()

plot6 <- ggplot(data=dataset, aes(x=Credit_History, fill=Loan_Status)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=custom_colors) +
  labs(title="Loan Status by Credit History", x="Credit History", y="Count") +
  theme_minimal()

# Arrange the plots in a grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3)







library(RColorBrewer)
library(corrplot)


# Create a correlation matrix
correlation_matrix <- cor(dataset[, sapply(dataset, is.numeric)], use="pairwise.complete.obs")

# Create the correlation heatmap
corrplot(correlation_matrix, method="color", type="lower", 
         tl.col="black", tl.srt=45, 
         col=brewer.pal(n=8, name="RdYlBu"))

#Data Cleaning
library(dplyr)
attach(dataset)
loan_clean <- dataset %>% 
  select(-c(Loan_ID)) %>% 
  mutate(Credit_History = as.factor(Credit_History))

colSums(is.na(loan_clean))

loan_clean <- loan_clean %>% 
  filter(complete.cases(.)) 
colSums(is.na(loan_clean))




tr <- loan_clean 
tr$LogLoanAmount <- log(tr$LoanAmount)
par(mfrow=c(1,2))
hist(tr$LogLoanAmount, 
     main="Histogram for Loan Amount", 
     xlab="Loan Amount", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=20, prob = TRUE)
lines(density(tr$LogLoanAmount), col='black', lwd=3)
boxplot(tr$LogLoanAmount, col='maroon',xlab = 'Income', main = 'Box Plot for Applicant Income')





tr$Income <- tr$ApplicantIncome + tr$CoapplicantIncome
tr$ApplicantIncome <- NULL
tr$CoapplicantIncome <- NULL

tr$LogIncome <- log(tr$Income)
par(mfrow=c(1,2))
hist(tr$LogIncome, 
     main="Histogram for Applicant Income", 
     xlab="Income", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=50, prob = TRUE)
lines(density(tr$LogIncome), col='black', lwd=3)
boxplot(tr$LogIncome, col='maroon',xlab = 'Income', main = 'Box Plot for Applicant Income')