# Load the necessary libraries
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors


# Read the CSV file
data <- read.csv("Samira_Azizi_ITC255.csv")
View(data)
# Scatter plot for Age and Height
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.))

ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.))

# Bar plot
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Sport))

# Boxplot
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = Gender, y = Height..cm.))

# Scatter plot with color.
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm., color = Sport))

# Scatter plot for testing size for a qualitative variable.
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm., size = Gender))

# testing Left, Scatter plot 
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm., alpha = Level.of.Education))

# testing Right, Scatter plot 
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm., shape = Sport))

# Scatter plot using my dataset with points colored yellow and green.
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.), color = "yellow")

ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.), color = "green")

# we should be careful with placing of (+) in its correct place otherwise shows error.
ggplot(data = data)  
  + geom_point(mapping = aes(x = Age, y = Height..cm.))

# Scatter plot with faceting part. 
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.)) + 
  facet_wrap(~ Sport, nrow = 8)

ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.)) + 
  facet_grid(Level.of.Education ~ Gender)

ggplot(data = data) + 
  geom_point(mapping = aes(x = Sport, y = Level.of.Satisfaction))

ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.)) +
  facet_grid(Gender ~ .)


ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.)) +
  facet_grid(. ~ Level.of.Education)
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.)) + 
  facet_wrap(~ Sport, nrow = 3)


# Left Scatter plot 
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.))

# Right Smoothe lined
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = Age, y = Height..cm.))

# Smoothed line with linetype by Sport
ggplot(data = data) + 
  geom_smooth(mapping = aes(x = Age, y = Height..cm., linetype = Sport))

# First, Smoothed line 
ggplot(data = data) +
  geom_smooth(mapping = aes(x = Age, y = Height..cm.))

# Second, Smoothed line
ggplot(data = data) +
  geom_smooth(mapping = aes(x = Age, y = Height..cm., group = Sport))

# Third, Smoothed line with color
ggplot(data = data) +
  geom_smooth(
    mapping = aes(x = Age, y = Height..cm., color = Sport),
    show.legend = FALSE
  )

# Scatter plot with smoothed line at the same time
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.)) +
  geom_smooth(mapping = aes(x = Age, y = Height..cm.))

# Scatter plot with smoothed line with color
ggplot(data = data, mapping = aes(x = Age, y = Height..cm.)) + 
  geom_point(mapping = aes(color = Sport)) + 
  geom_smooth()

# Scatter plot with smoothed line for a specific category
ggplot(data = data, mapping = aes(x = Age, y = Height..cm.)) + 
  geom_point(mapping = aes(color = Gender)) + 
  geom_smooth(data = filter(data, Gender == "specific_Gender"), se = FALSE)

ggplot(data = data, mapping = aes(x = Age, y = Height..cm., color = Gender)) + 
  geom_point() + 
  geom_smooth(se = FALSE)


# First plot, Scatter plot with smoothed line 
ggplot(data = data, mapping = aes(x = Age, y = Height..cm.)) + 
  geom_point() + 
  geom_smooth()

# Second plot: Scatter plot and smoothed line 
ggplot() + 
  geom_point(data = data, mapping = aes(x = Age, y = Height..cm.)) + 
  geom_smooth(data = data, mapping = aes(x = Age, y = Height..cm.))


# Bar plot 
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education))

# Bar plot using stat_count 
ggplot(data = data) + 
  stat_count(mapping = aes(x = Level.of.Education))


library(ggplot2)
library(tibble)  


demo <- tribble(
  ~Level.of.Education, ~freq,
  "High School",       1610,
  "Associate's",       4906,
  "Bachelor's",        12082,
  "Master's",          13791,
  "Doctorate",         21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = Level.of.Education, y = freq), stat = "identity")


# Bar plot with proportions
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))

# Summary plot 
ggplot(data = data) + 
  stat_summary(
    mapping = aes(x = Level.of.Education, y = Height..cm.),
    fun.min = min,
    fun.max = max,
    fun = median
  )

# Bar plot showing proportions of cuts
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, y = after_stat(prop)))

# Bar plot showing proportions of cuts, with by color
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, fill = Sport, y = after_stat(prop)))

# Bar plot with color by cut
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, colour = Level.of.Education))

# Bar plot filled by cut
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, fill = Level.of.Education))



# Bar plot with fill by clarity (or equivalent variable)
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, fill = Sport))

# Bar plot with transparency and identity position
ggplot(data = data, mapping = aes(x = Level.of.Education, fill = Sport)) + 
  geom_bar(alpha = 1/5, position = "identity")

# Bar plot with outline color by clarity (or equivalent variable)
ggplot(data = data, mapping = aes(x = Level.of.Education, colour = Sport)) + 
  geom_bar(fill = NA, position = "identity")

# Bar chart with fill by clarity and fill position
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, fill = Sport), position = "fill")

# Bar chart with fill by clarity and dodge position
ggplot(data = data) + 
  geom_bar(mapping = aes(x = Level.of.Education, fill = Sport), position = "dodge")

# Scatter plot with jitter for displacement vs. highway mileage
ggplot(data = data) + 
  geom_point(mapping = aes(x = Age, y = Height..cm.), position = "jitter")

# Scatter plot for city mileage vs. highway mileage
ggplot(data = data, mapping = aes(x = Age, y = Height..cm.)) + 
  geom_point()

# Boxplot for class vs. highway mileage
ggplot(data = data, mapping = aes(x = Sport, y = Height..cm.)) + 
  geom_boxplot()

# Boxplot with flipped coordinates
ggplot(data = data, mapping = aes(x = Sport, y = Height..cm.)) + 
  geom_boxplot() +
  coord_flip()


# Load necessary libraries
library(ggplot2)
library(maps)
install.packages("maps")

# Load the map data for New Zealand
nz <- map_data("nz")

# Basic map of New Zealand
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "green", colour = "pink")


ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "purple", colour = "green") +
  coord_quickmap()




bar <- ggplot(data = data) + 
  geom_bar(
    mapping = aes(x = Level.of.Education, fill = Level.of.Education), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

# Bar plot with flipped coordinates
bar + coord_flip()

# Bar plot with polar coordinates
bar + coord_polar()

# Scatter plot with a reference line 
ggplot(data = data, mapping = aes(x = City_MPG, y = Highway_MPG)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "green") +  # Reference line
  coord_fixed()

# and finally the end but it was really interesting. Thank you dear Professor.
