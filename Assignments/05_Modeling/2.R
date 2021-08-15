#install.packages('ggpubr')

# load libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(plyr)

# load csv
data <- read.csv(file = 'data/murder_suicide.csv')

# Define variables
vars <- c('Sex', 'Race', 'Age', 'EducationReportingFlag',
          'Education1989Revision', 'Education2003Revision',
          'MethodOfDisposition', 'MannerOfDeath')

# Define dataframe
df <- data %>%
  select(vars) %>%
  # Replace MannerOfDeath column strings
  mutate(MannerOfDeath = ifelse(MannerOfDeath == 2, 
                                'Suicide', 
                                'Homicide'),
         # Replace Sex column strings
         Sex = ifelse(Sex == 'F',
                      'Female',
                      'Male')) 

# Replace MethodOfDisposition column strings
from_dis = c('B', 'C', 'O', 'U', 
             'D', 'E', 'R') 
to_dis = c('Burial','Cremation','Other','Unknown',
           'Other','Other','Other')
df$MethodOfDisposition <- mapvalues(df$MethodOfDisposition, 
                                    from=from_dis, 
                                    to=to_dis)

# Replace Race column numbers 
from_rac = c(1, 2, 3, 4, 
             5, 6, 7, 18, 
             28, 38, 48, 58, 
             68, 78) 
to_rac = c('White', 'Black', 'American-Indian', 'Asian', 
           'Asian', 'Hawaiian', 'Asian', 'Asian-Indian',
           'Asian', 'Other', 'Asian', 'Other',
           'Other', 'Other')
df$Race <- mapvalues(df$Race, 
                     from=from_rac,
                     to=to_rac)

# Adapt and Replace Education1989Revision column with Education2003Revision
from_edu = c(0,1,2,3,4,5,6,7,8,
             9,10,11,12,
             13,14,15,16,
             17,99) 
to_edu = c(1,1,1,1,1,1,1,1,1,
           2,2,2,2,
           3,4,5,6,
           7,9)
df$Education1989Revision <- mapvalues(df$Education1989Revision, 
                                    from=from_edu, 
                                    to=to_edu)

# Substitute sum of 2 education column as new Education
df <- df %>%
  mutate(Education = Education2003Revision + Education1989Revision)


#####################       Plots       #####################
# Effect of Age and Sex on MannerOfDeath
g1 <- ggplot(subset(df, Age < 120),
       aes(x = Age, 
           y = after_stat(count),
           fill = MannerOfDeath)) +
  facet_grid(~Sex) +
  geom_density(alpha = 0.3)

# Effect of Sex and Age on MannerOfDeath
g2 <- ggplot(subset(df, Age < 120), 
       aes(x = Age, fill = Sex)) +
  geom_boxplot(size = 0.1, alpha = 0.5) +
  facet_grid(~MannerOfDeath)

# Effect of Race on MannerOfDeath
g3 <- ggplot(subset(df, Age < 120), 
           aes(x = Race, fill = MannerOfDeath)) +
  geom_bar(alpha = 0.7, position = "fill") +
  scale_fill_brewer(palette = "Paired")

# Effect of Education and Sex on MannerOfDeath
g4 <- ggplot(subset(df, Education < 9)) +
  geom_bar(aes(x = Education, fill = MannerOfDeath),
           alpha = 0.7, position = "dodge") + 
  facet_grid(~Sex) + 
  scale_fill_manual(values = c("#FF6600", "#00CC66"))

# Effect of Sex and MethodOfDisposition on MannerOfDeath
g5 <- ggplot(df, aes(Sex, fill = MethodOfDisposition)) + 
  facet_wrap(~MannerOfDeath) + 
  geom_bar(alpha = 0.7)

#####################      Subplots    #####################
ggarrange(g1,g2,g3,g4,g5,
          ncol = 2, nrow = 3)

