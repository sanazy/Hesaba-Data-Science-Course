library(devtools)
#devtools::install_github("andrewheiss/quRan")
library(quRan)
library(dplyr)
library(stringr)

####
q <- quran_ar
##############################################
words = c(
  'يَوْمً',
  'يَوْمٍ',
  'يَوْمَ',
  'يَوْمٌ'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'أَيَّام',
  'يَوْمَيْن'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))
##############################################
words = c(
  'شَهْرٌ',
  'شَهْرُ',
  'شَهْرٍ',
  'شَهْرً',
  'الشَّهْرِ',
  'الشَّهْرُ',
  'الشَّهْرَ'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'عَزْم'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'الدُّنْيَا'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'آخِرَة'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'جَهَنَّم'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'جَنَّات'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'الْمَلَائِكَة'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'الشَّيَاطِين',
  'شَيَاطِين'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'الصَّالِحَات'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'السَّيِّئَات'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'حَيَات'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'مَوْت'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'رَجُل'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'امْرَأَة',
  'امْرَأَت'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'صَلَوَات',
  'الصَّلَوَات'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'إِمَام',
  'أَئِمَّة'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'الصَّيْف'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'الشِّتَا'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

##############################################
words = c(
  'كُفْر'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))

words = c(
  'أَيْمَان'
)
tofind <- paste(words, collapse="|")
sum(str_count(q$text, tofind))