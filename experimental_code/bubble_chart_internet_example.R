# ABOUT
# Bubble plot with ggplot2
# 
# 
# 
# 
# This post explains how to build a bubble chart with R and ggplot2. It provides several reproducible examples with explanation and R code.
# 
# 

# Make it pretty
# A few classic improvement:
#   
#   use of the viridis package for nice color palette
# use of theme_ipsum() of the hrbrthemes package
# custom axis titles with xlab and ylab
# add stroke to circle: change shape to 21 and specify color (stroke) and fill


# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Most basic bubble plot
data %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, fill=continent)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")


longi_data <- tribble(
  ~var, ~exam_year,  ~completeness, ~vargroup,
  "var1", 0,  0.8, 'demographics',
  "var1", 2,  0.7, 'demographics',
  "var1", 5,  0.6, 'demographics',
  "var2", 0,  0.7, 'lab',
  "var2", 5,  0.9, 'lab',
  "var3", 5,  0.4, 'echo',
  "var4", 0,  0.1, 'substance',
  "var4", 2,  0.1, 'substance',
  "var4", 5,  0.1, 'substance',
  "var5", 0,  0.9, 'demographics',
  "var5", 2,  0.9, 'demographics',
  "var5", 5,  0.8, 'demographics',
  "var10", 0,  0.1, 'substance',
  "var10", 2,  0.1, 'substance',
  "var10", 5,  0.1, 'substance',
  "var11", 0,  0.9, 'demographics',
  "var11", 2,  0.9, 'demographics',
  "var11", 5,  0.8, 'demographics',
  "var6", 0,  0.8, 'demographics',
  "var6", 2,  0.7, 'demographics',
  "var7", 5,  0.6, 'demographics',
  "var7", 0,  0.7, 'lab',
  "var8", 5,  0.9, 'lab',
  "var9", 5,  0.4, 'echo'
  
)

color_scheme <- c(
  "black", "firebrick3", "goldenrod3", "darkmagenta", "hotpink3", "steelblue4", "mediumpurple3", "forestgreen", "dodgerblue3", "thistle4",
  "indianred4", "yellow4", "mediumseagreen"
)

longi_data %>%
  dplyr::arrange((row_number())) %>%
  #mutate(var = factor(var)) %>%
  mutate(exam_year = factor(exam_year)) %>%
  ggplot(aes(x=exam_year, y=var, size=completeness, fill=vargroup)) +
  geom_point(alpha=0.5, shape=21) +
  scale_size_area(max_size = 15, name="Complete data") +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  #theme_ipsum() +
  theme(legend.position="right") +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  xlab("Exam year") +
  ylab("Variable") +
  labs(fill = 'Var group') +
  scale_fill_manual(values = color_scheme[1:5])
