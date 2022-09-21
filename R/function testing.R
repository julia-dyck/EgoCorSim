
# Function testing --------------------------------------------------------

library(EgoCor)
library(EgoCorSim)
library(BRISC)
library(sp)

head(birth)
help(package = "EgoCor")


# Testing with check filtering --------------------------------------------

mod = vario.mod(birth, max.dist = 800, nbins = 11, shinyresults = F)
par_uncertainty_check(mod, mod.nr = 1)

# obviously works



# Testing with quantile function ------------------------------------------

unc = par_uncertainty_q(birth, max.dist = 800, nbins = 11)
unc$sds

# seems to work



# Testing with BRISC ------------------------------------------------------

help(package = "BRISC")
View(par_uncertainty_q)

coords = birth[,1:2]
sp::coordinates(coords) = ~x+y
coords = sp::coordinates(coords)

maxdist = max(mod$variog.list[[1]]$dist)

mod_brisc = BRISC_estimation(coords = birth[,1:2], y = birth$birthweight,
                       sigma.sq = var(birth$birthweight), tau.sq = 0, phi = 1/(maxdist/3))
View(BRISC_estimation)


# Testing sampling --------------------------------------------------------

library(EgoCor)

sample = spatial_sampling(cr, 1000, 9)
coords.plot(sample)

help(package = "BRISC")

library(BRISC)

coords = cbind(sample$coords.x1, sample$coords.x2)

estimation_result = BRISC_estimation(coords, y = sample[,3])

par_uncertainty_brisc(estimation_result)



