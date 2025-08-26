# Exercicio 2 do Projeto de PE

# Installs and loads the ggplot2 package
install.packages("ggplot2")
library(ggplot2)

# URL for the data
url <- 'https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/master.csv'

# Reads the data from the URL
file_data <- read.csv(url, check.names = FALSE)

# Sets requested year and age
req_year <- 1986
req_age <- "25-34 years"

# Removes "years" from req_age for the title
req_age_clean <- sub(" years", "", req_age)

# Filters the data
filtered_data <- subset(file_data, year == req_year & age == req_age)

# Creates the ggplot
p <- ggplot(filtered_data, aes(x = sex, y = `suicides/100k pop`, fill = sex)) +
  geom_boxplot() + # Creates two boxplots, one for each gender
  labs(title = paste("Number of suicides in", req_year, "for the age group of", req_age_clean),
       x = "Gender",
       y = "Suicides/100k pop",
       fill = "Gender") +  # Changes the legend label to "Gender"
  theme(plot.title = element_text(hjust = 0.5),  # To center the title
        axis.title.x = element_text(size = 10),  # Adjusts the size of the x-axis label
        axis.title.y = element_text(size = 10)) +  # Adjusts the size of the y-axis label
  scale_fill_manual(values = c("salmon", "skyblue"))  # Sets colors for the boxplots

# Save the plot to a file
ggsave("plot.png", plot = p, width = 12, height = 6)
