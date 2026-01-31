library(dplyr)
library(ggplot2)
library(ca)
library(colorspace)


data <- read.table("Downloads/TV16.csv", header = TRUE,
                                sep = ",", row.names = 1)

# Remove columns that are derived from other columns
data <- data %>% select(-lrelig, -lcograc, -lemprac, -uid)

#data <- data[order(data$state), ]

# Replace states with numbers
data$stateno <- as.integer(factor(data$state))

# Replace race with numbers
data$raceno <- as.integer(factor(data$racef))

sum(is.na(data$votetrump))
sum(data$votetrump != 1 & !is.na(data$votetrump))

# age groups are as follows: "1: 18-24", "2: 25-34", "3: 35-49", "4: 50-74", "5: 75-95"
data$agegroup <- cut(
  data$age,
  breaks = c(18, 24, 34, 49, 74, 95),
  labels = c("1", "2", "3", "4", "5"),
  right = TRUE,  # includes the upper bound in the interval
  include.lowest = TRUE  # includes 18
)
data$agegroup <- as.numeric(data$agegroup)


get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
categorical <- c("votetrump", "stateno", "female", "collegeed", "raceno", "bornagain")
continuous <- c("agegroup", "famincr","ideo",  "pid7na", "religimp", "churchatd", "prayerfreq", "angryracism", "whiteadv", "fearraces", "racerare")

modes <- sapply(data[categorical], get_mode)

modes2 <- sapply(data[continuous], get_mode)
modes2

means <- sapply(na.omit(data[continuous]), mean)
medians <- sapply(na.omit(data[continuous]), median)

variances <- sapply(na.omit(data[continuous]), var)
variances

min <- sapply(na.omit(data[continuous]), min)

modes 
modes2 
means
medians
variances
min


par(mfrow = c(3, 2))
par(mar = c(5, 4, 4, 2))

for (col in c("votetrump", "stateno", "female", "collegeed", "raceno", "bornagain")) {
  # Create frequency table (skip if column is all NAs or unique)
  freq_table <- table(data[[col]])
  label_size <- if (length(freq_table) > 6) 0.8 else 0.9
  # Skip columns with too many unique values or only NAs
  if (length(freq_table) > 1) {  # Adjust upper limit as needed
    pie(freq_table, 
        main = paste("", col),  cex = label_size)
    mtext(paste("Mode: ", get_mode(data[[col]]), " with ", sum(data[[col]] == get_mode(data[[col]]), na.rm = TRUE), " observations. ", "Number of NA values: ", sum(is.na(data[[col]])), "."), side = 1, line = 4, cex = 0.8)
    
  }
}

continuous <- c("agegroup", "famincr","ideo",  "pid7na", "religimp", "churchatd", "prayerfreq", "angryracism", "whiteadv", "fearraces", "racerare")
par(mfrow = c(3, 2))
par(mar = c(5, 4, 4, 2))
for (col in c("agegroup", "famincr","ideo",  "pid7na", "religimp", "churchatd", "prayerfreq", "angryracism", "whiteadv", "fearraces", "racerare")) {
  if (is.numeric(data[[col]])) {
    #col_red <- data_reduced[[col]]
    col_data <- na.omit(data[[col]])
    hist(col_data, main = paste("", col), xlab = col, col= "steelblue", breaks = min(col_data-1):(max(col_data))+0.5)
    #hist(col_red, main = paste("Reduced", col), xlab = col, col= "steelblue", breaks = min(col_red-1):(max(col_red))+0.5)
    mtext(paste( "Number of observations: ", length(data[[col]]), ". Number of NA values: ", sum(is.na(data[[col]]))), side = 1, line = 4, cex = 0.8)

  }
}

target <- data$votetrump
contigency <- function(col){
  d <- as.matrix(table(target, col, useNA = "always"))
  return(d)
}


freq_table <- function(col){
  d <- as.matrix(table(target, col, useNA = "always"))
  f <- proportions(d)
  return(f)
}

attraction_repulsion <- function(col){
  d <- as.matrix(table(target, col, useNA = "always"))
  rowp <- proportions(d, 1)
  colp <- proportions(d, 2)
  v1 <- matrix(rowSums(d), ncol = 1)
  v2 <- matrix(colSums(d), nrow = 1)
  e <- (v1 %*% v2) / sum(d)
  D <- d / e
  return(round(D,2))
}
 

contigency(data$female)
freq_table(data$female)
attraction_repulsion(data$female)

freq_table(data$female)
attraction_repulsion(data$female)

contigency(data$pid7na)
freq_table(data$pid7na)
attraction_repulsion(data$pid7na)

contigency(data$religimp)
freq_table(data$religimp)
attraction_repulsion(data$religimp)

contigency(data$whiteadv)
freq_table(data$whiteadv)
attraction_repulsion(data$whiteadv)

contigency(data$agegroup)
freq_table(data$agegroup)
attraction_repulsion(data$agegroup)

contigency(data$collegeed)
freq_table(data$collegeed)
attraction_repulsion(data$collegeed)

contigency(data$angryracism)
freq_table(data$angryracism)
attraction_repulsion(data$angryracism)

freq_table(data$raceno)
attraction_repulsion(data$raceno)

freq_table(data$fearraces)
attraction_repulsion(data$fearraces)

freq_table(data$racerare)
attraction_repulsion(data$racerare)

freq_table(data$famincr)
attraction_repulsion(data$famincr)

target <- data$angryracism

freq_table(data$agegroup)
attraction_repulsion(data$agegroup)

freq_table(data$raceno)
attraction_repulsion(data$raceno)

freq_table(data$religimp)
attraction_repulsion(data$religimp)

target <- data$collegeed

freq_table(data$ideo)
attraction_repulsion(data$ideo)

freq_table(data$racerare)
attraction_repulsion(data$racerare)

freq_table(data$angryracism)
attraction_repulsion(data$angryracism)

target <- data$racerare

freq_table(data$angryracism)
attraction_repulsion(data$angryracism)

freq_table(data$whiteadv)
attraction_repulsion(data$whiteadv)

freq_table(data$fearraces)
attraction_repulsion(data$fearraces)


freq_table(data$religimp)
attraction_repulsion(data$religimp)


target <- data$religimp

freq_table(data$bornagain)
attraction_repulsion(data$bornagain)

freq_table(data$prayerfreq)
attraction_repulsion(data$prayerfreq)

freq_table(data$ideo)
attraction_repulsion(data$ideo)


target <- data$churchatd

attraction_repulsion(data$angryracism)

attraction_repulsion(data$collegeed)

attraction_repulsion(data$famincr)

par(mfrow = c(1, 1))
plot(ca(freq_table(data$angryracism)), arrows = c(TRUE, TRUE))

?ca
target <- data$fearraces

plot(ca(freq_table(data$famincr)), arrows = c(FALSE, TRUE), map="rowprincipal")




par(mfrow=c(1,1))
plot(ca(freq_table(data$religimp)), arrows = c(TRUE, TRUE), labels = 2)



ca(contigency(data$collegeed))

#data <- data %>% select(votetrump, agegroup, collegeed, angryracism, racerare, racef, ideo, religimp)

### IMPORTANT 
# Reduce unwanted variables
data_mca <- data %>% select(-state, -age, -stateno, -raceno)
names(data_mca)

# Replace NA values by string "NA"
data_mca[is.na(data_mca)] <- "NA"

# perform MCA on columns of
mca <- ca::mjca(data_mca, lambda = "indicator", reti = TRUE)
names(mca)

# Summarize
mca_summary <- summary(mca)
mca_summary

# plotting the variation explained by the components
barplot(mca_summary$scree[, 3], ylim = c(0, 10),
        names.arg = paste("PC", 1:79), las = 2, xlab = "Component",
        ylab = "% of variation explained", col = "skyblue")

# plotting column scores
plot(mca, arrows = c(TRUE, FALSE))

categs <- sapply(data_mca, function(x) length(unique(x)))
nrow(unique(data_mca[2]))
colors <- rep("", sum(categs))
colors
rb <- qualitative_hcl(
  n = 16,
  palette = "Dark 3"  # high-contrast, bold palette
)
k <- 1
for (i in 1:length(data_mca)){
  col <- rb[i]
  for (j in 1:nrow(unique(data_mca[i]))){
    colors[k] <- col
    k <- k+1
  }
}
colors
# Function for scaling values from 0 to 1 (this is for visualization purposes):
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
# Generate the scatter plot. Point size is now scaled according to qlt:
qlt <- mca_summary$columns[, 3]
covariates <- mca$colpcoord[, 1:2]
par(mfrow= c(1,1))
plot(covariates, pch = 21, asp = 1, bg = colors, cex = normalize(qlt) + 1,
     xlab = paste0("Dimension 1", " (", mca_summary$scree[1, 3], "%", ")"),
     ylab = paste0("Dimension 2", " (", mca_summary$scree[2, 3], "%", ")"))
# Add arrows. Slight transparency is added to increase visibility.
arrows(rep(0, 17), rep(0, 17), covariates[,1], covariates[, 2],
       length = 0, col = rgb(0,0,0,0.1))
# "Cross-hair" is added, i.e., dotted lines crossing x and y axis at 0.
abline(h = 0, v = 0, lty = 3)
# Add variable:category names to the plot.
text(covariates, mca$levelnames, pos = 2, cex = normalize(qlt)+0.5, col = colors, srt = 30)

mca$colcoord

names(mca)


barplot(house_summary$scree[, 3], ylim = c(0, 20),
        names.arg = paste("PC", 1:12), las = 2, xlab = "Component",
        ylab = "% of variation explained", col = "skyblue")







house_mca <- ca::mjca(na.omit(data %>% select(agegroup, collegeed, angryracism, racerare, racef, ideo, religimp, famincr)), lambda = "indicator", reti = TRUE)
sum_mca <- summary(house_mca)

sum_mca

par(mfrow = c(1, 1))
barplot(sum_mca$scree[, 3], ylim = c(0, 38),
        names.arg = paste("PC", 1:38), las = 2, xlab = "Component",
        ylab = "% of variation explained", col = "steelblue")
plot(house_mca)

install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

# Run MCA
mca_res <- MCA(data, graph = FALSE)  # graph = FALSE to suppress immediate plots
# Print a summary of the MCA results
summary(mca_res)
?fviz_mca_ind
fviz_mca_ind(mca_res, 
             label = "none",  # or "ind" to label points
             addEllipses = FALSE)

fviz_mca_var(mca_res, 
             repel = TRUE,    # Avoid label overlap
             col.var = "steelblue"
)

mca_res$var$contrib

# replace na="not answered" by 2
data$votetrump[is.na(data$votetrump)] <- 2

# replace na="not answered" by 6
data$angryracism[is.na(data$angryracism)] <- 6
data$racerare[is.na(data$racerare)] <- 6
data$whiteadv[is.na(data$whiteadv)] <- 6
data$fearraces[is.na(data$fearraces)] <- 6

sum(data$votetrump == 2 | data$angryracism == 6)
sum(data$angryracism == 6 & data$racerare==6 & data$whiteadv==6 & data$fearraces==6)
sum(data$angryracism == 6 | data$racerare==6 | data$whiteadv==6 | data$fearraces==6)


sum(data$votetrump == 2 | data$angryracism == 6 | data$racerare==6 | data$whiteadv==6 | data$fearraces==6)

sum(data$votetrump == 4 | data$fearraces == 2)

length(na.omit(data)$votetrump)

data_reduced <- na.omit(data)



data <- data_reduced



data <- data %>% select(-age)
data <- data %>% select(-racef)
data <- data %>% select(-state)


