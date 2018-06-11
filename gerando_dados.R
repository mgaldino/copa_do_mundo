library(data.table)
library(dplyr)
library(stringr)
library(rstan)

options(mc.cores = parallel::detectCores())

setwd("C:\\Users\\mgaldino\\2018\\Pessoal\\copa_do_mundo")

# importando dados do ranking da FIFa (parta priori)
ranking <- fread("ranking_fifa.csv") %>%
  mutate(country = str_trim(country),
         country = ifelse(grepl("Iran", country), "Iran", country),
         country = ifelse(grepl("Korea Republic", country), "South Korea", country))

# CPI index
load("dados_corrupcao_2016.Rdata")


# importando dados de jogos (verossimilhança)
last_matches <- fread("last_matches_friendly.csv")

# importando times - seleções
teams <- fread("teams.csv", sep=",") %>%
  mutate(teams = str_trim(teams))

ranking_teams <- ranking %>%
  inner_join(teams, by=c("country" = "teams")) %>%
  arrange(-points)

# filtrando times pro indice da TI
cpi_data <- dados_corrupcao_2016 %>%
  mutate(country_or_territory = 
           ifelse(country_or_territory == "United Kingdom",
                  "England",country_or_territory )) %>%
  inner_join(teams, by=c("country_or_territory" = "teams"))
# England


teams <- ranking_teams$country

paises <- data.frame(country=unique(c(last_matches$V1, last_matches$V3)), id=1)


ranking_teams2 <- ranking_teams %>%
  inner_join(last_matches, by=c("country" = "V1")) %>%
  rename(score1 = V2,
         score2 = V4,
         team1 = country,
         team2 = V3)

ranking_teams3 <- ranking_teams %>%
  inner_join(last_matches, by=c("country" = "V3")) %>%
  rename(score1 = V2,
         score2 = V4,
         team2 = country,
         team1 = V1) %>%
  select(c("position", "team1", "points", "score1", "team2", "score2"))


ranking_teams4 <- ranking_teams2 %>%
  bind_rows(ranking_teams3) %>%
  mutate(dup = paste(team1, score1, team2, score2, sep="_")) %>%
  filter(!duplicated(dup))

lista_paises <- unique(c(ranking_teams4$team1, ranking_teams4$team2))
lista_exclusao <- lista_paises[!lista_paises %in% ranking_teams$country]

ranking_teams5 <- ranking_teams4 %>%
  filter(!team1 %in% lista_exclusao) %>%
  filter(!team2 %in% lista_exclusao)

ngames <- nrow(ranking_teams5)

nteams <- length(teams)

team1 <- match(ranking_teams5$team1, teams)
score1 <- ranking_teams5$score1
team2 <- match (ranking_teams5$team2, teams)
score2 <- ranking_teams5$score2

prior_score <- ranking_teams$points
prior_score <- (prior_score - mean(prior_score))/(2*sd(prior_score))

prior_CPI <- as.numeric(cpi_data$ano_2016)
prior_CPI <- (prior_CPI - mean(prior_CPI))/(2*sd(prior_CPI))


df <- 7

data <- c("nteams","ngames","team1","score1","team2","score2","prior_score","prior_CPI", "df")


fit <- stan("worldcup_v1.stan", data=data, chains=4, iter=5000)
print(fit)

colVars <- function(a) {n <- dim(a)[[1]]; c <- dim(a)[[2]]; return(.colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE)) ^ 2), n, c) * n / (n - 1))}

sims <- extract(fit)
a_sims <- sims$a
a_hat <- colMeans(a_sims)
a_se <- sqrt(colVars(a_sims))
library ("arm")
png ("worldcup1.png", height=500, width=500)
coefplot (rev(a_hat), rev(a_se), CI=1, varnames=rev(teams), main="Team quality (estimate +/- 1 s.e.)\n", cex.var=.9, mar=c(0,4,5.1,2), xlim=c(-1.5,1.5))
dev.off()

sims <- extract (fit)
a_sims <- sims$a
a_hat <- colMeans(a_sims)
a_se <- sqrt(colVars(a_sims))
expected_on_sqrt_scale <- a_hat[team1] - a_hat[team2]
sigma_y_sims <- sims$sigma_y
interval_975 <- median(qt(.975,df)*sigma_y_sims)
signed_square <- function (a) {sign(a)*a^2}
lower <- signed_square(expected_on_sqrt_scale - interval_975)
upper <- signed_square(expected_on_sqrt_scale + interval_975)

png ("worldcup2.png", height=1000, width=500)
coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(lower), upper.conf.bounds=rev(upper), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# checar daqui pra baixo
fit <- stan("worldcup.stan", data=data, chains=4, iter=5000)
print(fit)

sims <- extract (fit)
a_sims <- sims$a
sigma_y_sims <- sims$sigma_y
nsims <- length(sigma_y_sims)
random_outcome <- array(NA, c(nsims,ngames))
for (s in 1:nsims){
  random_outcome_on_sqrt_scale <- (a_sims[s,team1] - a_sims[s,team2]) + rt(ngames,df)*sigma_y_sims[s]
  random_outcome[s,] <- signed_square(random_outcome_on_sqrt_scale)
}
sim_quantiles <- array(NA,c(ngames,2))
for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(random_outcome[,i], c(.025,.975))
}

png ("worldcup3.png", height=1000, width=500)
coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(sim_quantiles[,1]), upper.conf.bounds=rev(sim_quantiles[,2]), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# Do it again, rounding the continuous predictions:

for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(round(random_outcome[,i]), c(.025,.975))
}

png ("worldcup4.png", height=1000, width=500)
coefplot (rev(score1 - score2), sds=rep(0, ngames),
          lower.conf.bounds=rev(sim_quantiles[,1]), upper.conf.bounds=rev(sim_quantiles[,2]), 
          varnames=rev(paste(teams[team1], "vs.", teams[team2])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# Reorder the games in order of predicted score differential
new_order <- order(prior_score[team1] - prior_score[team2])

for (i in 1:ngames){
  sim_quantiles[i,] <- quantile(random_outcome[,i], c(.025,.975))
}

png ("worldcup5.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order], sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1], upper.conf.bounds=sim_quantiles[new_order,2], 
          varnames=paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()

# Flip so expected outocmes are always positive
flip <- ifelse (prior_score[team1] < prior_score[team2], -1, 1)
new_order <- order((prior_score[team1] - prior_score[team2])*flip)
flip <- flip[new_order]

png ("worldcup6.png", height=1000, width=500)
coefplot ((score1 - score2)[new_order]*flip, sds=rep(0, ngames),
          lower.conf.bounds=sim_quantiles[new_order,1]*flip, upper.conf.bounds=sim_quantiles[new_order,2]*flip, 
          varnames=ifelse(flip==1, paste(teams[team1[new_order]], "vs.", teams[team2[new_order]]),
                          paste(teams[team2[new_order]], "vs.", teams[team1[new_order]])),
          main="Game score differentials\ncompared to 95% predictive interval from model\n",
          mar=c(0,7,6,2))
dev.off ()




