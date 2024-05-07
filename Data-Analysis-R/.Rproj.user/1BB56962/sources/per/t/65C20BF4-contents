# Load required library
library(readr)

# Read the CSV file
df <- read_csv('./Housing/Housing.csv')

# Display the first few rows of the dataframe
head(df)

# Get the dimensions of the dataframe
dim(df)

# Get the column names of the dataframe
names(df)

# Display information about the dataframe
str(df)

# Display summary statistics of the dataframe
summary(df)

# Check for missing values
colSums(is.na(df))

# Create a copy of the dataframe
df_encoded <- df

# Identify categorical columns
cat_cols <- names(df_encoded)[sapply(df_encoded, is.character)]

# Map categorical values to numerical values
furnishingstatus_mapping <- c('unfurnished' = 0, 'semi-furnished' = 1, 'furnished' = 2)
df_encoded$furnishingstatus <- furnishingstatus_mapping[df$furnishingstatus]

# Convert categorical variables to numerical using label encoding
library(dplyr)
library(purrr)
library(forcats)

cat_cols <- names(df_encoded)[sapply(df_encoded, is.character)]
df_encoded <- df_encoded %>%
  mutate_at(vars(cat_cols), as.factor) %>%
  mutate_if(is.factor, as.numeric)

# Display the first few rows of the encoded dataframe
head(df_encoded)

# Check value counts of the 'furnishingstatus' variable in the original dataframe
table(df$furnishingstatus)

# Display summary statistics of the encoded dataframe
summary(df_encoded)

# Identify the endogenous variable Y and the potential explanatory variables X
# Perform hypothesis on the direction of statistical relationships

# Perform distribution analysis and graphical representations if necessary

# Check for correlation between variables
cor_matrix <- cor(df_encoded)

# Create a heatmap of the correlation matrix
library(ggplot2)
library(reshape2)

ggplot(data = melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()

# Check correlation of variables with 'price'
cor_matrix[, 'price']

# Analyze statistical relationship between X and Y

# Fit regression model
model <- lm(price ~ ., data = df_encoded)

# Display regression summary
summary(model)

# Assess for statistical biases (omitted variable bias, reverse causality, outliers)

# Refine the analysis by estimating the relationship between X and Y for different groups of individuals
