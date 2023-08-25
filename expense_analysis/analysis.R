#ctrl + shift + enter to run code without highlighting

library(data.table)
library(ggplot2)
library(dplyr)

dt_in <- fread("Checking - 3746_01-01-2023_08-25-2023.csv")

View(dt_in)
#==============
#Pre-processing
#==============
# Remove any unnecessary columns

cols_remove <- c("Memo")
dt_in[, (cols_remove) := NULL]

# lowercase name column
dt_in[, Name:=tolower(Name)]

# Remove any credit transactions
dt <- dt_in[Transaction == "DEBIT"]

# Add a month column pulled from date column
dt$Month <- format(as.Date(dt$Date, format="%Y-%m-%d"),"%m")

#Remove latest month
dt <- dt[!as.integer(format(dt$Date, "%m")) == 8, ]

#=====
#Analysis
#====

#1) Food Expenses

food_key <-c("target", "trader joe s", "eastside food", "cub foods", "wholefds", 
             "kowalski's", "lunds&byerlys")
food_key_pattern <- paste(food_key, collapse = "|")

dt[grepl(food_key_pattern, Name), Category := "Groceries"]

View(dt)

dt_food <- dt[Category == "Groceries"]
food_total <- dt_food[, .(FoodExpenses = abs(sum(Amount))), by=.(Month = paste0(Month))]
p <- ggplot(data = food_total, aes(x = Month, y = FoodExpenses)) + geom_bar(stat = "identity", fill = "#6495ED") + geom_hline(aes(yintercept = mean(FoodExpenses)), linetype="dashed") + geom_text(aes(label=FoodExpenses), position=position_dodge(width=0.9), vjust=-0.25)
p

#2)Fixed Expenses
dt[, freq := .N, by=.(Name,Amount)]
dt[freq >1, Category:= "Fixed"]
View(dt)