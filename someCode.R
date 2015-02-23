# # this function takes three levels of percentages and creates a random sample
# expLevels <- function(l1, l2, l3, N){
#   # all levels in one vector
#   exp.levels <- c(rep(1, l1/100*N), rep(2, l2/100*N), rep(3, l3/100*N))
#   
#   # if sum of all levels is different than a hundred then recycle to avoid error
#   if(sum(l1, l2, l3) < 100 | sum(l1, l2, l3) > 100){ 
#     exp.levels <- sample(exp.levels, size = N, replace = TRUE)
#   }
#   else exp.levels <- sample(exp.levels)
#   return(exp.levels)
# }
# 
# interchange2V <- function(v1, v2){
#   as.vector(rbind(v1, v2))
# }
# 
# N <- 20
# 
# value <- vector(mode = 'numeric', length = N/2)
# value2 <- vector(mode = 'numeric', length = N/2)
# transProbMatrix           <- read.csv("disease_transmission_matrix_2.csv", header = FALSE)
# 
# population <- c(rep(1:4, N*0.2) ,rep(5:6, N*0.1))
# population <- sample(population)
# n1 <- population[1:(length(population)/2)]
# n2 <- population[(length(population)/2+1):length(population)]
# 
# # exp.levels <- c(rep(1,3), rep(2, 3), rep(3,4))
# exp.levels <- expLevels(20, 20, 60, (N/2))
# 
# for(i in 1:(N/2)){
#   value[i] <- simDiseaseTrans(recipient = n1[i], exposer =  n2[i], exposure = exp.levels[i], probMatrix = transProbMatrix)
#   value2[i] <- simDiseaseTrans(recipient = n2[i], exposer =  n1[i], exposure = exp.levels[i], probMatrix = transProbMatrix)
# }
# 
# DT<- data.table("recipient" = interchange2V(n1, n2), "exposer" = interchange2V(n2, n1), 
#                  "exposure" = interchange2V(exp.levels, exp.levels), "recipient.new" = interchange2V(value, value2))

#####################
# 
# N <- 100
# population <- c(rep(1:4, N*0.2) ,rep(5:6, N*0.1))
# homeState <- sample(1:3, N, replace = TRUE)
# homeStateWeight <- data.frame("homeState" = 1:3, "homeStateWeight" = c(0, 2, 4))
# df<- cbind(homeState, population)
# df <- as.data.frame(cbind(homeState, population))
# df$homeState   <- as.factor(df$homeState)
# df$population <- as.factor(df$population)
# 
# p <- ggplot(df, aes(x = population, fill = homeState)) + geom_histogram()
# print(p)

# simDT <- fread("HP_HIMSS_Simulated_Interactions (7).csv")
# 
# p1 <- ggplot(simDT, aes(x = as.factor(healthstatus), fill = as.factor(cut2(age, cuts = c(20, 60))))) + geom_bar() 
# p1 <- p1 + xlab("Health Status of Population") + ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n")
# p1 <- p1 + theme_bw()
# p1 <- p1 + theme(legend.position="bottom")
# p1 <- p1 + scale_fill_manual(values=c("#deebf7", "#9ecae1", "#3182bd"), 
#                              name="Population Age: ")
# p1 <- p1 + commonTheme
# 
# p2 <- ggplot(simDT, aes(x = as.factor(healthstatus), fill = as.factor(homestate))) + geom_bar() 
# p2 <- p2 + xlab("Health Status of Population") + ylab("Frequency") + ggtitle("Distribution of Health Status of Population\n")
# p2 <- p2 + theme_bw()
# p2 <- p2 + theme(legend.position="bottom")
# p2 <- p2 + scale_fill_manual(values=c("#e7e1ef", "#c994c7", "#dd1c77"), 
#                              name="Home-State Google Flu Trends: ")
# p2 <- p2 + commonTheme
# 
# p3 <- ggplot(simDT, aes(x = changed, y = exposer.HS, fill = changed)) + geom_boxplot(notch = TRUE)
# p3 <- p3 + xlab("Recipient Health Status Changed") + ylab("Exposer Health Status") 
# p3 <- p3 + ggtitle("Change in Health Status by Exposer's Health Status\n") + scale_y_continuous(breaks = 1:6)
# p3 <- p3 + theme_bw()
# p3 <- p3 + theme(legend.position="None")
# p3 <- p3 + scale_fill_manual(values=c("#deebf7", "#dd1c77"))
# p3 <- p3 + commonTheme
# 
# 
# p4 <- ggplot(simDT, aes(x = changed, y = healthstatus, fill = changed)) + geom_boxplot(notch = TRUE)
# p4 <- p4 + xlab("Recipient Health Status Changed") + ylab("Recipient Health Status") 
# p4 <- p4 + ggtitle("Change in Health Status by Exposer's Health Status\n") + scale_y_continuous(breaks = 1:6)
# p4 <- p4 + theme_bw()
# p4 <- p4 + theme(legend.position="None")
# p4 <- p4 + scale_fill_manual(values=c("#deebf7", "#dd1c77"))
# p4 <- p4 + commonTheme
# 
# grid.arrange(p1, p2, p3, p4, ncol = 2)
# 
# p5 <- ggplot(simDT, aes(x = changed, y = probability, fill = changed)) + geom_boxplot(notch = TRUE)
# p5 <- p5 + xlab("Recipient Health Status Changed") + ylab("Probability") 
# p5 <- p5 + ggtitle("Change in Health Status by Exposer's Health Status\n") #+ scale_y_continuous(breaks = 1:6)
# p5 <- p5 + theme_bw()
# p5 <- p5 + theme(legend.position="None")
# p5 <- p5 + scale_fill_manual(values=c("#deebf7", "#dd1c77"))
# p5 <- p5 + commonTheme
# 
# library(gdata)
# simDT[, expLevel := as.factor(expLevel)]
# simDT$expLevel <- reorder(simDT$expLevel, new.order = c(2, 3, 1))
# 
# p6 <- mosaicplot(table(simDT$expLevel, simDT$changed), main="Mosaic of \"Changed\" by Exposure Levels", 
#                  color=colorspace::rainbow_hcl(3)[-1], cex=1.1, xlab="Exposure Level", ylab="Changed")
# 
# simDT[, comorbidity := as.factor(comorbidity)]
# p7 <- mosaicplot(table(simDT$comorbidity, simDT$changed), main="Mosaic of \"Changed\" by Comorbidity", 
#                  color=colorspace::rainbow_hcl(3)[-1], cex=1.1, xlab="Comorbidity", ylab="Changed")
# 
# simDT[, homestate := as.factor(homestate)]
# simDT$homestate <- reorder(simDT$homestate, new.order = c(2,3,1))
# p8 <- mosaicplot(table(simDT$homestate, simDT$changed), main="Mosaic of \"Changed\" by Home State", 
#                  color=colorspace::rainbow_hcl(3)[-1], cex=1.1, xlab="Home-State Google Flu-Trend Level", ylab="Changed")





flu <- googleFlu()
flu[, state := tolower(state)]
setkey(flu, state)

states_map <- as.data.table(map_data('state'))
#rename
states_map[, state := region]
states_map[, region := NULL]
setkey(states_map, state)


flu_map <- merge(states_map, flu)

p <- ggplot(flu_map, aes(x = long, y = lat, group = group, fill = count)) + geom_polygon(col="black") 
p <- p + theme_bw() + theme(legend.position = "none", text = element_blank(), line = element_blank()) + coord_map("polyconic") 
p <- p + scale_fill_continuous(low="yellow", high="red") 
  
print(p)


N <- 160
foo <- flu[sample(1:nrow(flu), N, replace = TRUE)]
state <- foo[, state]
homestate <- foo[, fluLevel]
