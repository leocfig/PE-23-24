# Exercicio 1 do Projeto de PE

install.packages("ggplot2") # Installs the ggplot2 package

library(ggplot2) # Loads the installed package

url <- 'https://web.tecnico.ulisboa.pt/~paulo.soares/pe/projeto/Paises_PIB_ICH.csv' # URL for the data

file_data <- read.csv(url, check.names = FALSE) # Reads the data from the URL

continents <- c("Europe", "Americas") # Defines the requested continents

# Defines the requested countries to label
countries <- c("Lithuania", "Iceland", "United States", "Saint Lucia")

# Filters the data for the specified continents
cont_filtered_data <-subset(file_data, Continent %in% continents)

# Filters the data for the specified countries
country_filtered_data <- subset(cont_filtered_data, Country %in% countries)

# Creates the ggplot
ggplot(cont_filtered_data, aes(x = GDP, y = HCI, color = Continent, label = Country)) + 
  geom_point(aes(col = Continent), size = 2.5) +  # Original points
  geom_point(data = country_filtered_data, 
             stroke = 0.5, 
             col = "black", 
             size = 1) +  # Larger points as border
  geom_text(data = subset(cont_filtered_data, Country == "Lithuania"), aes(label = Country), 
             nudge_x = 0.05, nudge_y = -0.02, size = 3, color = "black") +
  geom_text(data = subset(cont_filtered_data, Country == "Iceland"), aes(label = Country), 
            nudge_x = 0.08, nudge_y = -0.01, size = 3, color = "black") +
  geom_text(data = subset(cont_filtered_data, Country == "United States"), aes(label = Country), 
             nudge_x = 0.1, nudge_y = -0.02, size = 3, color = "black") +
  geom_text(data = subset(cont_filtered_data, Country == "Saint Lucia"), aes(label = Country), 
             nudge_x = -0.22, nudge_y = 0.012, size = 3, color = "black") +
  scale_x_log10() +
  labs(title = "Gross domestic product (GDP) vs Human Capital Index (HCI)", 
       x = "GDP per capita adjusted for the cost of living\n(IMF estimates in 2023, international dollars)", 
       y = "Human Capital Index\n(values between 0 and 1, data from 2020)") +
  theme(plot.title = element_text(hjust = 0.5), # To center the title
        axis.title.x = element_text(size = 9),  # Adjusts the size of the x-axis label
        axis.title.y = element_text(size = 9))  # Adjusts the size of the y-axis label