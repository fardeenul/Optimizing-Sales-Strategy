library(dplyr)
library(tidyr)
library(ggplot2)

##DATA VALIDATION --------------------------------------------------------------------------------------------
product_sales <- read_csv("product_sales.csv")

# glimpse(product_sales)

#changing the class of columns to appropriate class
product_sales <- product_sales %>% 
                mutate(week = as.numeric(week),
                       sales_method = as.factor(sales_method), 
                       nb_sold = as.numeric(nb_sold),
                       years_as_customer = as.numeric(years_as_customer),
                       nb_site_visits = as.numeric(nb_site_visits),
                       state = as.factor(product_sales$state)
                )

glimpse(product_sales)
# product_sales <- product_sales[, -ncol(product_sales)]

# Display the structure of the dataframe
str(product_sales)

# Check for missing values
summary(product_sales)

#Week column : no cleaning was required (1-6) represents the weekly perfectl
#sales method column ------------------------------------------------
    unique(product_sales$sales_method)
    
    # Convert sales_method to lowercase
    product_sales$sales_method <- tolower(product_sales$sales_method)
    
    
    # Handle the case "em + call" as "email + call"
    product_sales$sales_method[product_sales$sales_method == "em + call"] <- "email + call"
    
    # Check unique values after conversion
    unique(product_sales$sales_method)


#customer_id column --------------------------------------
    # Check for duplicates in the customer_id column
    duplicate_customer_ids <- product_sales$customer_id[duplicated(product_sales$customer_id)]
    
    # Display the duplicate customer IDs
    print(duplicate_customer_ids)
    sum(is.na(product_sales$customer_id))
  
    # Check the total number of rows
    total_rows <- nrow(product_sales)
    
    # Check the number of unique customer IDs
    unique_customer_count <- length(unique(product_sales$customer_id))
    
    # Compare the counts
    if (total_rows == unique_customer_count) {
      print("The number of unique customer IDs is equal to the total number of rows.")
    } else {
      print("The number of unique customer IDs is different from the total number of rows.")
    }
    

#nb_sold column (no cleaning required)------------------------------------------
    unique(product_sales$nb_sold)
    summary(product_sales$nb_sold)
    
   
    
# revenue columns -------------------------------
    summary(product_sales$revenue)
    
    # Find and display rows with NA values in the revenue column
    rows_with_na <- product_sales[is.na(product_sales$revenue), ]
    
    # Display the rows with NA values
    print(rows_with_na)
    
    # Set revenue to 0 for rows where years_as_customer is 0 and nb_site_visits is 0
    product_sales$revenue[(product_sales$years_as_customer == 0)] <- 0
    
    #Comparison
    # Impute missing values with the mean
    mean_imputed_revenue <- product_sales$revenue
    mean_imputed_revenue[is.na(mean_imputed_revenue)] <- mean(product_sales$revenue, na.rm = TRUE)
    
    # Impute missing values with the median
    median_imputed_revenue <- product_sales$revenue
    median_imputed_revenue[is.na(median_imputed_revenue)] <- median(product_sales$revenue, na.rm = TRUE)
    
    # Compare mean and median imputations
    summary(mean_imputed_revenue)
    summary(median_imputed_revenue)
    
    # Impute the mean for remaining NA values in the revenue column
    mean_revenue <- mean(product_sales$revenue, na.rm = TRUE)
    product_sales$revenue[is.na(product_sales$revenue)] <- mean_revenue
    
    
#visualize the revenue COLUMN
    # Create a boxplot for the revenue column
    boxplot(product_sales$revenue,
            main = "Boxplot of Revenue",
            ylab = "Revenue",
            col = "lightblue",
            border = "black",
            notch = TRUE)
    
    hist(product_sales$revenue,
         main = "Histogram of Revenue",
         xlab = "Revenue",
         col = "skyblue",
         border = "black")    
    
  product_sales$revenue <-  round(product_sales$revenue,2)
    
#years_as_customers column -----------------------------------------

      unique(product_sales$years_as_customer)
  product_sales <- product_sales %>% 
                        filter(years_as_customer < 45)
  
  library(ggplot2)
  
  # Filter the data
  filtered_data <- product_sales %>% 
    filter(years_as_customer < 45)
  
  # Create a box plot
  ggplot(filtered_data, aes(x = "", y = years_as_customer, fill = "skyblue")) +
    geom_boxplot() +
    labs(title = "Distribution of Years as Customer",
         x = "",
         y = "Years as Customer") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove legend since there's only one fill color
  
  
  
#nb_site_visits column------------------------------------------------
  unique(product_sales$nb_site_visits)

#state column
  summary(product_sales$state)
  unique(product_sales$state)
  
##EDA -------------------------------------------------------------------
 
#sales by sales method : which method was most effective, revenue ~ #How many customers were there for each approach?

sales_method_count <-  product_sales %>% 
          group_by(sales_method) %>% 
          summarise(count_customers = n())
  
sales_method_summary <- 
                        product_sales %>% 
                          group_by(sales_method) %>% 
                          summarise(total_nb_sold = sum(nb_sold),
                                    total_revenue_generated = sum(revenue),
                                    count_customers = n(),
                                    avg_newproducts_sold = mean(nb_sold), 
                                    avg_revenue_generated = mean(revenue),
                                    avg_site_visits = mean(nb_site_visits)
                          ) 

#  #  What does the spread of the revenue look like overall? And for each method?

revenue_spread_by_state <-  product_sales %>% 
                            select(sales_method, nb_sold , revenue, state) %>% 
                            group_by(state) %>% 
                            summarize(total_nb_sold = sum(nb_sold),
                                      total_revenue_generated = sum(revenue),
                                      avg_nb_sold = mean(nb_sold), 
                                      avg_revenue_generated = mean(revenue),
                            ) %>% 
                          arrange(desc(total_revenue_generated))


library(ggplot2)

# Bar Plot for Total Revenue Generated by State
ggplot(revenue_spread_by_state, aes(x = state, y = total_revenue_generated, fill = total_revenue_generated)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Revenue Generated by State",
       x = "State",
       y = "Total Revenue Generated",
       fill = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# For 'call' method
call_range <- range(product_sales$revenue[product_sales$sales_method == "call"], na.rm = TRUE)
call_iqr <- IQR(product_sales$revenue[product_sales$sales_method == "call"], na.rm = TRUE)
call_sd <- sd(product_sales$revenue[product_sales$sales_method == "call"], na.rm = TRUE)

# Boxplot for Each Sales Method
ggplot(product_sales, aes(x = sales_method, y = revenue)) +
  geom_boxplot(fill = "lightgreen", color = "green") +
  labs(title = "Boxplot of Revenue by Sales Method",
       x = "Sales Method",
       y = "Revenue") +
  theme_minimal()


#sales by week : which week had the most sales unit and sales revenue? ~ #  Was there any difference in revenue over time for each of the methods?

# Based on the data, which method would you recommend we continue to use? 
# Some of these methods take more time from the team so they may not be the best for us to use if the results are similar.

product_sales %>% 
  group_by(week) %>% 
  summarize(total_nb_sold = sum(nb_sold),
            total_revenue_generated = sum(revenue),
            avg_nb_sold = mean(nb_sold), 
            avg_revenue_generated = mean(revenue),
  ) %>% 
  arrange(desc(total_revenue_generated))


total_sales_by_week <- product_sales %>%
  group_by(week) %>%
  summarize(total_sales_units = sum(nb_sold),
            total_sales_revenue = sum(revenue))

week_with_most_sales <- total_sales_by_week %>%
  filter(total_sales_units == max(total_sales_units),
         total_sales_revenue == max(total_sales_revenue))

total_revenue_by_week_method <- product_sales %>%
  group_by(week, sales_method) %>%
  summarize(total_revenue = sum(revenue))


total_NB_by_week_method <- product_sales %>%
  group_by(week, sales_method) %>%
  summarize(total_nb_sold = sum(nb_sold))

difference_in_revenue <- total_revenue_by_week_method %>%
  spread(sales_method, total_revenue) %>%
  mutate(difference = email + call - email)



library(ggplot2)

# Line Plot for Difference in Revenue Over Time

# Create a line plot for Revenue Over Time by Sales Method
ggplot(
  total_NB_by_week_method , aes(x = week, y = total_nb_sold, color = sales_method)) +
  geom_line() +
  labs(title = "nb sold over time",
       x = "Week",
       y = "Total nb sales",
       color = "Sales Method") +
  theme_minimal()

# efficiency of sales method by 

 product_sales <- product_sales %>%
           mutate(
                  avg_time_spent = case_when(
                         sales_method == 'email' ~ 5,
                         sales_method == 'call' ~ 30,
                         sales_method == 'email + call' ~ 10,
                         TRUE ~ NA_real_  # Add a default case if needed
                  ),
                  efficiency = revenue / avg_time_spent
           )
 


 # Assuming time_spent is in minutes
 # Calculate total efficiency by sales method
 total_efficiency_by_method <- product_sales %>%
   group_by(sales_method) %>%
   summarize(total_efficiency = sum(efficiency))
 
 # Print the results
 print(total_efficiency_by_method)
 library(ggplot2)
 
 # Assuming total_efficiency_by_method is the result from the previous code
 ggplot(total_efficiency_by_method, aes(x = sales_method, y = total_efficiency)) +
   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
   labs(
     title = "Efficiency Comparison by Sales Method",
     x = "Sales Method",
     y = "Total Efficiency"
   ) +
   theme_minimal()
 

 
 