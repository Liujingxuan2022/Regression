set.seed(123)
ID <- sample(10000:99999, 500, replace = TRUE)
#brand <- sample(c("CBS", "Wharton", "HBS", "Stanford", "Kellogg"), 500, replace = TRUE)
prospect <- sample(c(0, 1, 2), 500, replace = TRUE, prob = c(0.2, 0.6, 0.2))
gpa <- rnorm(500, mean = 2.75, sd = 0.5)
research <- sample(c(0, 1, 2), 500, replace = TRUE, prob = c(0.2, 0.6, 0.2))
offers <- rpois(500, 1 + 2 * gpa)
stipend <- 30000 + 5000 * gpa + rnorm(500, sd = 1000) + 800*research^2 #linear regression
ivy <- rbinom(500, size = 1, prob = 0.2)
beta0 <- -5 # Intercept
beta1 <- 5 # Coefficient for GPA
p <- 1 / (1 + exp(-beta0 - beta1 * (gpa - 2.5)))
admitted <- rbinom(500, size = 1, prob = p)

phd <- data.frame(ID, admitted, prospect, gpa, research, offers, stipend, ivy)

phd$prospect <- factor(phd$prospect)
phd$research <- factor(phd$research)

write.csv(phd, "phd.csv")

# multinomial logit data 

id <- rep(1:100, each = 4)
id <- sprintf("%04d", id)
id <- as.factor(paste0("ID", id))

brand <- rep(c("CBS", "Kellogg", "HBS", "Wharton"), 100)
stipend <- rep(c(44000, 47000, 43000, 41000), 100)
brand_number <- rep(c(3, 1, 2, 4), 100)

choice = NULL
number = 1

for(num in c(1:100)){
  choice <- c(choice, sample(c(0,0,0,1), 4, replace = F))
  number = number + 4 + 1
  num = num + 1
}

choice<- ifelse(choice == 0, FALSE, TRUE)

choice_data <- data.frame(id, brand, choice, stipend, brand_number)

individual_data <- choice_data %>%
  filter(choice == T)


individual_data$gpa <-  rnorm(mean = 3, sd = 0.4, 100) + individual_data$brand_number*0.08

final_data <- dplyr::full_join(choice_data, individual_data %>%
                                 select(id, gpa))

write.csv(final_data, "multinomial_dat.csv")