library(epitools)
library(ggplot2)

wo_1000 <- function(tourney_category=0,
                    year_i=1975,
                    year_f=2019,
                    surface=0,
                    round_level=0,
                    winner_hand=0, 
                    loser_hand=0,
                    winner_age_i=0, 
                    winner_age_f=0, 
                    loser_age_i=0, 
                    loser_age_f=0,
                    dif_age_i=99,
                    dif_age_f=99,
                    mean_age_i=0,
                    mean_age_f=0,
                    winner_rank_i=0, 
                    winner_rank_f=0,
                    loser_rank_i=0,
                    loser_rank_f=0,
                    dif_rank_i=9999,
                    dif_rank_f=9999,
                    distribution="Binomial",
                    title_wo="")
{
    x <- wta
    if(tourney_category != 0) { x <- x[x$tourney_category == tourney_category,] }
    if(year_i != 0) { x <- x[x$year >= year_i,] }
    if(year_f != 0) { x <- x[x$year <= year_f,] }
    if(surface !=0 ) { x <- x[x$surface == surface,] }
    if(round_level != 0) { x <- x[x$round_level == round_level,] }
    if(winner_hand != 0) { x <- x[x$winner_hand == winner_hand,] } 
    if(loser_hand !=0 ) { x <- x[x$loser_hand == loser_hand,] }
    if(winner_age_i != 0) 
    {
        x <- x[!is.na(x$winner_age),]
        x <- x[x$winner_age >= winner_age_i,]
    } 
    if(winner_age_f != 0) 
    {
        x <- x[!is.na(x$winner_age),]	
        x <- x[x$winner_age < winner_age_f,]
    } 
    if(loser_age_i !=0 ) 
    {
        x <- x[!is.na(x$loser_age),]	
        x <- x[x$loser_age >= loser_age_i,]
    } 
    if(loser_age_f != 0) 
    {
        x <- x[!is.na(x$loser_age),]	
        x <- x[x$loser_age < loser_age_f,]
    }
    if(dif_age_i != 99) 
    {
        x <- x[!is.na(x$dif_age),]
        x <- x[x$dif_age >= dif_age_i,]
    }
    if(dif_age_f != 99) 
    {
        x <- x[!is.na(x$dif_age),]
        x <- x[x$dif_age < dif_age_f,]
    }
    if(mean_age_i != 0) 
    {
        x <- x[!is.na(x$mean_age),]
        x <- x[x$mean_age >= mean_age_i,]
    }
    if(mean_age_f != 0) 
    { 
        x <- x[!is.na(x$mean_age),]
        x <- x[x$mean_age < mean_age_f,]
    }
    if(winner_rank_i != 0) 
    { 
        x <- x[!is.na(x$winner_rank),]
        x <- x[x$winner_rank >= winner_rank_i,]
    } 
    if(winner_rank_f != 0) 
    { 
        x <- x[!is.na(x$winner_rank),]
        x <- x[x$winner_rank < winner_rank_f,]
    }
    if(loser_rank_i != 0) 
    { 
        x <- x[!is.na(x$loser_rank),]
        x <- x[x$loser_rank >= loser_rank_i,]
    }
    if(loser_rank_f != 0) 
    { 
        x <- x[!is.na(x$loser_rank),]
        x <- x[x$loser_rank < loser_rank_f,]
    }
    if(dif_rank_i != 9999) 
    { 
        x <- x[!is.na(x$dif_rank),]
        x <- x[x$dif_rank >= dif_rank_i,]
    }
    if(dif_rank_f != 9999) 
    { 
        x <- x[!is.na(x$dif_rank),]
        x <- x[x$dif_rank < dif_rank_f,]
    }
    n <- nrow(x[x$WalkOver=="WalkOver",]) 
    base <- nrow(x)
    year <- c(year_i:year_f)
    n_years <- (year_f-year_i+1)
    n_year <- rep(0,n_years)	
	base_year <- rep(0,n_years)
    incidence <- rep(0,n_years)
    lower <- rep(0,n_years)
    upper <- rep(0,n_years)	
    var_year <- rep(0,n_years)
    if (distribution=="Poisson")
    {
        ic <- pois.exact(n,base)
        rate <- ic$rate*1000
    }
    if (distribution=="Binomial")
    {
        ic <- binom.exact(n,base)
        rate <- ic$proportion*1000
    }
    for(i in year_i:year_f)
    {
        n_year[i-(year_i-1)] <- nrow(x[x$WalkOver=="WalkOver" & x$year==i,]) 
        base_year[i-(year_i-1)] <- nrow(x[x$year==i,])
        if (n_year[i-(year_i-1)] != 0 | base_year[i-(year_i-1)] != 0)
        {
        if (distribution=="Poisson")
            {
                ic_year <- pois.exact(n_year[i-(year_i-1)],base_year[i-(year_i-1)])
                incidence[i-(year_i-1)] <- ic_year$rate*1000
            }
            if (distribution=="Binomial")
            {
                ic_year <- binom.exact(n_year[i-(year_i-1)],base_year[i-(year_i-1)])
                incidence[i-(year_i-1)] <- ic_year$proportion*1000
            }
            lower[i-(year_i-1)] <- ic_year$lower*1000
            upper[i-(year_i-1)] <- ic_year$upper*1000
            var_year[i-(year_i-1)] <- (n_year[i-(year_i-1)]/(base_year[i-(year_i-1)]^2))*1000*1000
            if(n_year[i-(year_i-1)]==0) { var_year[i-(year_i-1)] <- 0 }
        }
        if (n_year[i-(year_i-1)] == 0 & base_year[i-(year_i-1)] == 0)
        {
            incidence[i-(year_i-1)] <- 0
            lower[i-(year_i-1)] <- 0
            upper[i-(year_i-1)] <- 0
            var_year[i-(year_i-1)] <- 0
        }
    }		
    ci_low <- ic$lower*1000
    ci_up <- ic$upper*1000
    dd <- data.frame(year=year,n=n_year,matches=base_year,incidence=incidence,lower=lower,upper=upper,var_year=var_year)
    wo <- list(n = n,rate = round(rate,2), ci_low = round(ci_low,2), ci_up = round(ci_up,2), dd = dd, title_wo = title_wo)
    class(wo) <- "wo"
    wo
}

print.wo <- function(x, ...)
{
    cat(x$title_wo)
    cat(sep = "\n")
    cat("Walkovers per 1000 matches: ",x$rate,"  ( ",x$ci_low," - ",x$ci_up," )")
    cat(sep = "\n")
    cat(sep = "\n")
}

plot.wo <- function(tourney_category=0,
                    year_i=1975,
                    year_f=2019,
                    surface=0,
                    round_level=0,
                    winner_hand=0, 
                    loser_hand=0,
                    winner_age_i=0, 
                    winner_age_f=0, 
                    loser_age_i=0, 
                    loser_age_f=0,
                    dif_age_i=99,
                    dif_age_f=99,
                    mean_age_i=0,
                    mean_age_f=0,
                    winner_rank_i=0, 
                    winner_rank_f=0,
                    loser_rank_i=0,
                    loser_rank_f=0,
                    dif_rank_i=9999,
                    dif_rank_f=9999,
                    distribution="Binomial",
                    title_wo="Incidence of Walkovers in WTA tennis matches by year")
{
    dd <- wo_1000(tourney_category, year_i, year_f, surface, round_level, winner_hand, loser_hand, winner_age_i, winner_age_f, loser_age_i, loser_age_f, dif_age_i, dif_age_f, mean_age_i, mean_age_f, winner_rank_i, winner_rank_f,  loser_rank_i, loser_rank_f, dif_rank_i, dif_rank_f, distribution)$dd
    result <- ggplot(dd,aes(x=dd$year,y=dd$incidence)) + 
                        geom_point(col='black', size=3) +
						geom_smooth(se=TRUE, fullrange=FALSE, level=0.95) +
						geom_errorbar(aes(ymin = dd$lower, ymax = dd$upper), width = 0.2) +
                        scale_x_continuous(breaks = seq(year_i, year_f, by = 1), limits = c(year_i, year_f)) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +	
                        ylim(0,max(dd$incidence)) + 
                        labs(x="Year", 
                             y="WalkOver Incidence Proportion per 1,000 matches",
                             title=title_wo) +
                        theme(axis.title = element_text(face = 'bold'),
                              axis.text  = element_text(size=15),
                              plot.title = element_text(hjust=0.5,size=20))
    result
}




default_1000 <- function(tourney_category=0,
                         year_i=1975,
                         year_f=2019,
                         surface=0,
                         round_level=0,
                         winner_hand=0, 
                         loser_hand=0,
                         winner_age_i=0, 
                         winner_age_f=0, 
                         loser_age_i=0, 
                         loser_age_f=0,
                         dif_age_i=99,
                         dif_age_f=99,
                         mean_age_i=0,
                         mean_age_f=0,
                         winner_rank_i=0, 
                         winner_rank_f=0,
                         loser_rank_i=0,
                         loser_rank_f=0,
                         dif_rank_i=9999,
                         dif_rank_f=9999,
                         distribution="Binomial",
                         title_default="")
{
  x <- wta
  if(tourney_category != 0) { x <- x[x$tourney_category == tourney_category,] }
  if(year_i != 0) { x <- x[x$year >= year_i,] }
  if(year_f != 0) { x <- x[x$year <= year_f,] }
  if(surface !=0 ) { x <- x[x$surface == surface,] }
  if(round_level != 0) { x <- x[x$round_level == round_level,] }
  if(winner_hand != 0) { x <- x[x$winner_hand == winner_hand,] } 
  if(loser_hand !=0 ) { x <- x[x$loser_hand == loser_hand,] }
  if(winner_age_i != 0) 
  {
    x <- x[!is.na(x$winner_age),]
    x <- x[x$winner_age >= winner_age_i,]
  } 
  if(winner_age_f != 0) 
  {
    x <- x[!is.na(x$winner_age),]	
    x <- x[x$winner_age < winner_age_f,]
  } 
  if(loser_age_i !=0 ) 
  {
    x <- x[!is.na(x$loser_age),]	
    x <- x[x$loser_age >= loser_age_i,]
  } 
  if(loser_age_f != 0) 
  {
    x <- x[!is.na(x$loser_age),]	
    x <- x[x$loser_age < loser_age_f,]
  }
  if(dif_age_i != 99) 
  {
    x <- x[!is.na(x$dif_age),]
    x <- x[x$dif_age >= dif_age_i,]
  }
  if(dif_age_f != 99) 
  {
    x <- x[!is.na(x$dif_age),]
    x <- x[x$dif_age < dif_age_f,]
  }
  if(mean_age_i != 0) 
  {
    x <- x[!is.na(x$mean_age),]
    x <- x[x$mean_age >= mean_age_i,]
  }
  if(mean_age_f != 0) 
  { 
    x <- x[!is.na(x$mean_age),]
    x <- x[x$mean_age < mean_age_f,]
  }
  if(winner_rank_i != 0) 
  { 
    x <- x[!is.na(x$winner_rank),]
    x <- x[x$winner_rank >= winner_rank_i,]
  } 
  if(winner_rank_f != 0) 
  { 
    x <- x[!is.na(x$winner_rank),]
    x <- x[x$winner_rank < winner_rank_f,]
  }
  if(loser_rank_i != 0) 
  { 
    x <- x[!is.na(x$loser_rank),]
    x <- x[x$loser_rank >= loser_rank_i,]
  }
  if(loser_rank_f != 0) 
  { 
    x <- x[!is.na(x$loser_rank),]
    x <- x[x$loser_rank < loser_rank_f,]
  }
  if(dif_rank_i != 9999) 
  { 
    x <- x[!is.na(x$dif_rank),]
    x <- x[x$dif_rank >= dif_rank_i,]
  }
  if(dif_rank_f != 9999) 
  { 
    x <- x[!is.na(x$dif_rank),]
    x <- x[x$dif_rank < dif_rank_f,]
  }
  n <- nrow(x[x$Default=="Default",]) 
  base <- nrow(x)
  year <- c(year_i:year_f)
  n_years <- (year_f-year_i+1)
  n_year <- rep(0,n_years)	
  base_year <- rep(0,n_years)
  incidence <- rep(0,n_years)
  lower <- rep(0,n_years)
  upper <- rep(0,n_years)	
  var_year <- rep(0,n_years)
  if (distribution=="Poisson")
  {
    ic <- pois.exact(n,base)
    rate <- ic$rate*1000
  }
  if (distribution=="Binomial")
  {
    ic <- binom.exact(n,base)
    rate <- ic$proportion*1000
  }
  for(i in year_i:year_f)
  {
    n_year[i-(year_i-1)] <- nrow(x[x$Default=="Default" & x$year==i,]) 
    base_year[i-(year_i-1)] <- nrow(x[x$year==i,])
    if (n_year[i-(year_i-1)] != 0 | base_year[i-(year_i-1)] != 0)
    {
      if (distribution=="Poisson")
      {
        ic_year <- pois.exact(n_year[i-(year_i-1)],base_year[i-(year_i-1)])
        incidence[i-(year_i-1)] <- ic_year$rate*1000
      }
      if (distribution=="Binomial")
      {
        ic_year <- binom.exact(n_year[i-(year_i-1)],base_year[i-(year_i-1)])
        incidence[i-(year_i-1)] <- ic_year$proportion*1000
      }
      lower[i-(year_i-1)] <- ic_year$lower*1000
      upper[i-(year_i-1)] <- ic_year$upper*1000
      var_year[i-(year_i-1)] <- (n_year[i-(year_i-1)]/(base_year[i-(year_i-1)]^2))*1000*1000
      if(n_year[i-(year_i-1)]==0) { var_year[i-(year_i-1)] <- 0 }
    }
    if (n_year[i-(year_i-1)] == 0 & base_year[i-(year_i-1)] == 0)
    {
      incidence[i-(year_i-1)] <- 0
      lower[i-(year_i-1)] <- 0
      upper[i-(year_i-1)] <- 0
      var_year[i-(year_i-1)] <- 0
    }
  }		
  ci_low <- ic$lower*1000
  ci_up <- ic$upper*1000
  dd <- data.frame(year=year,n=n_year,matches=base_year,incidence=incidence,lower=lower,upper=upper,var_year=var_year)
  default <- list(n = n, rate = round(rate,2), ci_low = round(ci_low,2), ci_up = round(ci_up,2), dd = dd, title_default = title_default)
  class(default) <- "default"
  default
}

