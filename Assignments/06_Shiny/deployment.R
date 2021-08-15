install.packages('rsconnect')

rsconnect::setAccountInfo(name='sanazyyy',
                          token='2F7645FB6579EC007D9EA63DB1BA44F1',
                          secret='fnA0MxrCtv3j7tjHLgXQgP2C2LFgh4DZeQVAxeOD')

library(rsconnect)
# deploy
rsconnect::deployApp()
# redeploy
rsconnect::deployApp(upload=FALSE)
