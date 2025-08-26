# Exercicio 3 do Projeto de PE

# Installs the ggplot2 and readxl packages, and loads installed packages
install.packages("ggplot2")
install.packages("readxl")
library(ggplot2)
library(readxl)

# Reads the data from the Excel file, reads all columns as Text to ensure no coercion warnings are raised during the initial read
file_data <- read_excel("/home/leonor/Desktop/PE/Projeto_PE/Ex_3/electricity.xlsx", col_types = "text")

countries <- c("IEA Total", "Hungary", "Iceland") # Sets requested countries

filtered_data <- subset(file_data, YEAR >= 2015 & COUNTRY %in% countries) # Filters the data to include the requested countries and years

filtered_data$share <- as.numeric(filtered_data$share) # Converts the "share" column to numeric data type

# Filters the data for renewable products and obtains the proportion of renewable energy
renewables_data <- subset(filtered_data, PRODUCT == "Renewables")
renewables_data$Renewable_Proportion <- renewables_data$share * 100  # Converts to percentage

# Combines YEAR and MONTH columns to create a Date variable
renewables_data$Date <- as.Date(paste(renewables_data$YEAR, renewables_data$MONTH, "01", sep = "-"))

dev.new() # Opens a new graphics device to aid better visualization of the graphic

# Creates the ggplot
ggplot(renewables_data, aes(x = Date, 
                      y = Renewable_Proportion, 
                      color = COUNTRY)) +
  geom_line() +   # Connects the points of each month
  geom_point(size = 1) +  # Adds points for every month with smaller size
  scale_x_date(date_breaks = "1 months", labels = function(x) {
    ifelse(format(x, "%m") == "01" | format(x, "%m") == "04" | format(x, "%m") == "07" | format(x, "%m") == "10", 
           format(x, "%b %Y"), "")
  }) +  # Includes labels every three months; monthly labels would make the x-axis crowded and hard to read
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Monthly Proportion of Renewable Electricity Production",
       x = "Date",
       y = "Renewable Electricity Proportion (%)",
       color = "Country") +
  theme(plot.title = element_text(hjust = 0.5), # To center the title
        axis.title.x = element_text(size = 9),  # Adjusts the size of the x-axis label
        axis.title.y = element_text(size = 9),  # Adjusts the size of the y-axis label
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates x-axis labels diagonally

