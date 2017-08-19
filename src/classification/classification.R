################################################
#      Introducción a la Ciencia de Datos      #
#                CLASIFICACIÓN                 #
#                                              #
# (C) Cristian González Guerrero               #
################################################

# Build the workspace
source("../classification/build-workspace.R")

# Some info about the classes
round(prop.table(table(tae$Class)) * 100, digits = 1)

# Normalize numeric data
tae$Size = normalize(tae$Size)
tae.tra = lapply(tae.tra, function(df) {
  df$Size = normalize(df$Size)
  return(df)
})
tae.tst = lapply(tae.tst, function(df) {
  df$Size = normalize(df$Size)
  return(df)
})

# Some plots
plot(tae, col = tae$Class)
tae2 = tae
tae2$Instructor = as.integer(tae$Instructor)
tae2$Course = as.integer(tae$Course)
#ggpairs(tae2, aes(color = Class, alpha = 0.3))


tae_test_pred = knn(
  train = tae.tra[[1]][4:5], 
  test = tae.tst[[1]][4:5], 
  cl = tae.tra[[1]]$Class, 
  k = 21
)

table(tae_test_pred, tae.tst[[1]]$Class)
