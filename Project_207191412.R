library(tidyverse)
superstore <- read_csv("train.csv")

superstore <- superstore |> 
  mutate(across(where(is.character), ~ iconv(., to = "UTF-8", sub = "")))

View(superstore)

superstore <- superstore |> 
  mutate(
    Category = as.factor(Category),
    `Sub-Category` = as.factor(`Sub-Category`),
    Region = as.factor(Region),
    `Ship Mode` = as.factor(`Ship Mode`)
  )

summary(superstore |> select(Category, Region, `Ship Mode`))

classify_sales <- function(sales_amount) {
  if (sales_amount > 500) {
    return("High Value")
  } else {
    return("Standard Value")
  }
}

classify_sales_vec <- Vectorize(classify_sales)

# 5A: Calculations and Transformations
superstore <- superstore |> 
  mutate(
    Sales_with_VAT = Sales * 1.17,
    Is_Large_Order = Sales > 1000,
    Sales_Status = as.factor(classify_sales_vec(Sales))
  )

superstore |> 
  select(Sales, Sales_with_VAT, Is_Large_Order, Sales_Status) |> 
  head()

#Expolatory Data Analysis

superstore |> 
  group_by(Category) |> 
  summarize(Total_Sales = sum(Sales, na.rm = TRUE)) |> 
  ggplot(mapping = aes(x = Category, y = Total_Sales, fill = Category)) +
  geom_col() +
  labs(title = "Total Sales by Category", 
       x = "Product Category", 
       y = "Total Sales ($)")

ggplot(data = superstore, mapping = aes(x = Region, y = Sales, fill = Region)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1000)) + 
  labs(title = "Sales Distribution by Region", 
       x = "Region", 
       y = "Sales Amount ($)")

superstore |> 
  filter(Sales < 1000) |> 
  ggplot(mapping = aes(x = Sales)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Sales (Under $1000)", 
       x = "Sales ($)", 
       y = "Count")


# מבחן סטטיסטי ומסקנות

stat_test <- lm(Sales ~ Category, data = superstore)

# הדפסת תוצאות המבחן הסטטיסטי
summary(stat_test)


## ML - Decision Tree


# טעינת הספריות
library(rpart)
library(rpart.plot)
tree_model <- rpart(
  formula = Sales_Status ~ Category + Region + `Ship Mode`, 
  data = superstore, 
  method = "class" # משמעות המילה class היא שאנחנו חוזים קטגוריה (טקסט) ולא מספר
)

rpart.plot(
  x = tree_model,
  type = 3,
  extra = 104, # מוסיף לנו לכל קובייה בעץ אחוזים וספירה מדויקת
  box.palette = "Blues", # צובע את העץ בגוונים יפים של כחול
  main = "Decision Tree: Predicting Sales Value Status"
)
