library(dplyr)
library(tibble)

############# DOUG'S CENTROID FUNS ################

# CALCULATE CENTROIDS
# x1 is plot x species matrix, g1 is group vector which must have the same plots, in the same order, as the matrix
calc0.cen=function(x1,g1){
  r1=matrix(0,nrow=length(unique(g1)),ncol=ncol(x1))
  for(i in 1:ncol(x1)){
    r1[,i]=as.vector(tapply(x1[,i],g1,mean))
  }
  colnames(r1)=colnames(x1)
  rownames(r1)=names(tapply(x1[,1],g1,mean))
  r1
}
# CALCULATE DISTANCE TO CENTROIDS
# m1 is the matrix of plots to be assigned, m2 is the centroid matrix, n1 is the number of groups to be listed
D2C=function(m1,m2,n1){
  r1=vector("numeric",length=nrow(m2))
  r2=matrix(0,nrow=nrow(m1),ncol=n1)
  r3=matrix("",nrow=nrow(m1),ncol=n1)
  for(i in 1:nrow(m1)){
    for(j in 1:nrow(m2)){
      r1[j]=sum(abs(m1[i,]-m2[j,]))/sum((m1[i,]+m2[j,]))
    }
    r2[i,]=sort(r1)[1:n1]
    r3[i,]=rownames(m2)[order(r1)][1:n1]
  }
  rownames(r2)=rownames(m1)
  rownames(r3)=rownames(m1)
  list(dists=r2,groups=r3)
}

###################################################

##### re-written centroid functions
#####
calculate_centres <- function(data, groups) {
  centres <- data.frame(data, group = groups) %>%
    group_by(group) %>%
    summarise_all(funs(mean)) %>%
    as.data.frame()
  row.names(centres) <- centres$group
  as.matrix(centres[,-1])
}

euc_dist <- function(i, j) {
  sum(abs(i - j)) / sum((i + j))
}

dist_to_centroid <- function(site, centres, n) {
  dists <- sort(unlist(lapply(as.data.frame(t(centres)), euc_dist, site)))[1:n]
  groups <- names(dists)
  list(dists = dists, groups = groups)
}
####

# Some fake data to simulate database and groupings
flor_dat <- as.data.frame(t(
  replicate(n = 50000, expr = {rbinom(5000, 1, 0.15)}, simplify = T)
))
sum(colSums(flor_dat) == 0); sum(rowSums(flor_dat) == 0)
names(flor_dat) <- paste0("spp", sample(1:ncol(flor_dat)))

# Sort by column name
flor_dat <- flor_dat[,sort(names(flor_dat))]

# make some fake group names
groups <- sample(rep(letters, length.out = nrow(flor_dat)))

# find the centres 
species_centres <- calculate_centres(flor_dat, groups)

# make soem fake new data to assign, with a subset of the species
new_dat <- as.data.frame(t(
  replicate(n = 500, expr = {rbinom(200, 1, 0.2)}, simplify = T)
))
sum(colSums(new_dat) == 0); sum(rowSums(new_dat) == 0)
names(new_dat) <- sample(names(flor_dat), 200)

# find species not in new data
absense_species <- colnames(species_centres)[!colnames(species_centres) %in% names(new_dat)]
# add the absent species in 
new_dat_absenses <- data.frame(new_dat,
                               as.data.frame(
                                 matrix(data = 0, nrow = nrow(new_dat),
                                        ncol = length(absense_species),
                                        dimnames = list(NULL, absense_species))
                              ))
# sort new data
new_dat_absenses <- new_dat_absenses[,sort(names(new_dat_absenses))]

# check species in new data matches species in centres (result should be zero)
sum(!colnames(species_centres) == names(new_dat_absenses))

# calculate via dougs functions
doug_centres <- calc0.cen(flor_dat, groups)
doug_dists <- D2C(new_dat_absenses, doug_centres, 10)
doug_dists_dist <- doug_dists$dists
doug_dists_group <- doug_dists$groups

# calculate via new functions
mitch_distances <- lapply(as.data.frame(t(new_dat_absenses)), dist_to_centroid, species_centres, 10)
mitch_dists_dist <- t(bind_rows(lapply(mitch_distances, `[[`, 1)))
mitch_dists_group <- t(bind_rows(lapply(mitch_distances, `[[`, 2)))








