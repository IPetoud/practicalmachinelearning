draw <- function(){
        
        deck <- c("Duke", "Assassin", "Captain", "Ambassador", "Contessa")
        
        hand <- sample(deck, size=3, replace = T)
        
        print(hand)
        
}
drawcoin <- function(){
        deck <- c("head","tail")
        hand <- sample(deck, size=100, replace = TRUE)
        print(hand)
}
flip <- function(){
        coin <- c("heads", "tails")
        toss <- sample(coin, 100, replace = TRUE, prob = c(0.3, 0.7))
        return(toss)
}

employee.data <- read.csv("C:/Users/pc/data/employee_data.csv", skip = 23, stringsAsFactors = FALSE)
employee.data <- as.tibble(employee.data)
employee.data$gender <- as.factor(employee.data$gender)
employee.data$title <- as.factor(employee.data$title)


any(is.na(employee.data))


employee.a <- employee.data %>% 
        select(ends_with("name"), gender, everything()) %>%
        filter(salary >= 70000) %>% 
        arrange(gender, last_name)

good.earners <- employee.a["emp_no"]

employee.b <- employee.data %>% 
        group_by(title, gender) %>% 
        summarise(avg.salary = mean(salary)) %>% 
        mutate(monthly = avg.salary/12) %>% 
        arrange(gender, desc(monthly))

# Tidying the weather data

weather <- read.csv("C:/Users/pc/Dropbox/365/R PROGRAMMING/data/weather_untidy.csv", stringsAsFactors = FALSE)
weather <- as.tibble(weather)

weather.a <- weather %>% gather(day, value, d1:d31, na.rm = TRUE)

weather.b <- weather.a %>% mutate(day = parse_number(day)) %>%
        select(id, year, month, day, element, value) %>%
        arrange(id, year, month, day)

weather.c <- weather.b %>% spread(element, value)

# Tidying the tb data

tb <- read.csv("C:/Users/pc/Dropbox/365/R PROGRAMMING/data/tb_untidy.csv", stringsAsFactors = FALSE)
tb <- as.tibble(tb)

names(tb) <- str_replace(names(tb), "new_sp_", "")
names(tb) <- str_replace(names(tb), "m", "m.")
names(tb) <- str_replace(names(tb), "f", "f.")

tb$m.04 <- NULL
tb$m.514 <- NULL
tb$f.04 <- NULL
tb$f.514 <- NULL
tb$m.u <- NULL 
tb$f.u <- NULL

tb.a <- tb %>% gather(m.014:f.65, key = "column", value = "cases", na.rm = TRUE) %>% arrange(country)
tb.b <- tb.a %>% separate(column, into = c("sex", "age"))

tb.b$age <- str_replace_all(tb.b$age, "0", "0-")
tb.b$age <- str_replace_all(tb.b$age, "15", "15-")
tb.b$age <- str_replace_all(tb.b$age, "25", "25-")
tb.b$age <- str_replace_all(tb.b$age, "35", "35-")
tb.b$age <- str_replace_all(tb.b$age, "45", "45-")
tb.b$age <- str_replace_all(tb.b$age, "55", "55-")
tb.b$age <- str_replace_all(tb.b$age, "65", "65-100")

tb.b


library(tidyverse)
install.packages("ggthemes")
library(ggthemes)

emp <- read.csv("employee_data.csv", skip = 23, stringsAsFactors = FALSE)
emp <- as.tibble(emp)
emp$gender <- as.factor(emp$gender)
emp$title <- as.factor(emp$title)

emp.a <- filter(emp, salary > 45000)

hist <- ggplot(emp.a, aes(salary))
hist + geom_histogram(binwidth = 5000, color = "darkslategray",
                      fill = "darkseagreen2", alpha = 0.7) +
        labs(title = "Salary distribution in the employee data",
             x = "Salary", y = "Number of employees in the salary bracket") +
        theme_solarized_2(light = FALSE, base_size = 15, base_family = "serif")


library(tidyverse)
library(ggthemes)

bar <- ggplot(emp, aes(title, fill = gender))
bar + geom_bar() + theme_fivethirtyeight() + scale_fill_manual(values = c("chartreuse4", "darkorange")) +
        labs(title = "Job Positions by Gender",
             y = "Employee count",
             x = "Job position")

# theme_fivethirtyeight() does not allow us to name the x- and y-axis; you can change it to one that works
# trying to pass the legend.position= argument into any available theme won't work; if you want to customise 
# your theme beyong font type and size, you would need to create a theme for yourself with the theme() function;
# it takes an abundance of arguments allowing you to modify virtually every aspect of your visualisation

bar <- ggplot(emp, aes(gender, fill = title)) 
bar + geom_bar() + theme_fivethirtyeight() + scale_fill_manual(values = c("magenta", "darkorange", "midnightblue",
                                                                          "springgreen4", "brown1", "gold")) +
        labs(title = "Job Positions by Gender")

# The aes(x = gender, fill = title) mapping is a lot more difficult to read;
# the only thing it makes relatively easy for me to see is that the data is too symmetrially distributed, which
# suggests it has indeed been artifically generated (shocker!)

# look up scale_fill_manual, and scale_color_manual functions
# Can you set title and x and y axis names? Why? Try using a different theme. Can you do it now? 
# What happens if you try to set the theme() argument legend.position = "right". Why do you think that is? 
# Change the mappings so that gender is plotted and the bars are filled with position segmentation. 
# Do you find this graph useful and easy to read? 
# Perhaps the only thing it convinces us in is that the data has been simulated. 




library(tidyverse)
library(ggthemes)
install.packages("wesanderson")
library(wesanderson)

emp <- read.csv("employee_data.csv", skip = 23, stringsAsFactors = FALSE)
emp <- as.tibble(emp)
emp$gender <- as.factor(emp$gender)
emp$title <- as.factor(emp$title)

emp.a <- filter(emp, salary > 45000)

boxx <- ggplot(emp, aes(x = title, y = salary))
my.bp <- boxx + geom_boxplot(outlier.color = "orangered1", outlier.shape = 3) +
        geom_jitter(width = 0.3, aes(color = gender)) +
        ggtitle("Salary distribution", subtitle = "based on position and gender") +
        ylab("Salary") + xlab("Job position") + 
        theme_economist_white() + 
        theme(legend.position = "right", axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_flip() # this can be added if the axis.text.x doesn't make sense to you; it's also easier to read

my.bp + scale_color_manual(values=wes_palette(name = "Darjeeling", n = 2))

# or

my.bp + scale_color_brewer(palette="Set1")

# the palette is part of the RColorBrewer package which you should already have on your
# machines because it comes with the tidyverse


# Load the two data sets into R: "skew_1.csv", and "skew_2.csv". 
# Identify the skew of the data sets, both visually, and numerically. 
# Try to interpret what you are seeing.

### Visual examination of the skew

sk1 <- read_csv("skew_1.csv")
sk2 <- read_csv("skew_2.csv")

skew1 <- ggplot(data = sk1, aes(x = `Dataset 1`))
skew1 + geom_histogram(binwidth = 100,
                       color = "darkslategray", 
                       fill = "darkslategray4", 
                       alpha = 0.5) +
        theme_light()

# The skew of this dataset is positive


skew2 <- ggplot(data = sk2, aes(x = `Dataset 2`))
skew2 + geom_histogram(binwidth = 100,
                       color = "darkslategray", 
                       fill = "darkslategray4", 
                       alpha = 0.5) +
        theme_light()

# The skew of this dataset is negative

### Numerical examination of the skew 

library(psych)

describe(sk1)
describe(sk2)

# Note: sometimes, using a graph to identify the skew can be misleading.	
# The sk2 dataset has a relatively strong negative skew ( -0.33 ). 	
# However, from a histogram with a few bins, you cannot clearly determine the skew.	
# For best results, use a more precise measure of skewness, such as R's calculations, instead of a simple graph	




# Task 1: What are the types of data and the levels of measurement of the following variables: Cust ID, Mortgage, Year of sale. 
# Task 2: Create a histogram which represents the Price variable. Use the default binwidth values first and then set bins of length $100,000. Use the data on all apartments, no matter if sold or not.
# Task 3: Interpret the results.
# Task 4: Create a scatter plot showing the relationship between Price and Area. Use the data on all apartments, no matter if sold or not. Interpret the results.
# Task 5: Calculate the mean, median, mode, skewness, and standard deviation of Price for all apartments, no matter if sold or not.
# Task 6: Interpret the measures.
# Task 7: Calculate the correlation between Price and Area. Is the result in line with the scatterplot?

library(tidyverse)

product <- read_csv("practical_product.csv")
customer <- read_csv("practical_customer.csv")

# Task 1

# Cust ID - Categorical/Nominal. This variable has the same properties as ID.
# Mortgage - Categorical/Nominal.	This is a Binary variable. Like a Yes/No question or Gender.
# Year of sale - Numerical, discrete/Interval.	Year is a numerical variable. It is always discrete. The level of measurement is questionable, but we would treat it as interval, as the 0 year would be the time when the Big Bang happened. The current BC-AD calendar was arbitrary chosen (similarly to degrees Celsius and Fahrenheit).

# Task 2

hg <- ggplot(data=product, aes(x = Price))
hg + geom_histogram(binwidth = 100000, 
                    color = "darkslategray", 
                    fill = "darkslategray4", 
                    alpha = 0.5) + 
        ggtitle("House Prices Frequency Distribution") + 
        labs(y = "Number of Houses", 
             x = "Price") + 
        theme_minimal() +
        scale_x_continuous(labels = comma) # this gets rid of the scientific notation on the x axis

# Task 3

# The two histograms point to similar insights - most of the apartment prices are concentrated in the interval ($217,000 to 317,000)	

# Task 4

sp <- ggplot(product, aes(`Area (ft.)`, Price))
sp + geom_point() + 
        theme_light() + 
        labs(x = "Area in Square Feet", 
             y = "Price in USD", 
             title = "Relationship between Price and Area") +
        scale_y_continuous(labels = comma) # this gets rid of the scientific notation on the y axis

# The scatter plot shows a very strong linear relationship between Price and Area. This was to be expected as often Real Estate companies price their property per square foot.	
# Notice how for cheaper apartments (lower areas respectively), the points are closer so the variance is smaller. However, the bigger the apartment, the bigger the difference in the price.	

# Task 5 

library(psych)

# either of these gives you a decent summary and the mean, median, skew, and standard deviation of the data of interest

describe(product$Price)
summary(product$Price)

# create a function that returns the mode
mode <- function(x){ 
        ta <- table(x)
        tam <- max(ta)
        if (all(ta == tam))
                mod <- NA
        else
                if(is.numeric(x))
                        mod <- as.numeric(names(ta)[ta == tam])
        else
                mod <- names(ta)[ta == tam]
        return(mod)
}

mode(product$Price)

# Task 6

# We will only comment on the skew, as it is a bit tougher. 
# The skew is right (positive). This means that most aparments are relatively cheap with a tiny portion that is more expensive.														

# Task 7

cor.test(product$`Area (ft.)`, product$Price)

# Yes, the result is in line with the scatter plot. The two variables are strongly correlated.







