
#  These functions are used in the analysis and plotting in all of the other scripts.
#  If you run the other scripts with fresh = f, you can avoid ever using the block
#    bootstrap functions.

require(magrittr)
require(purrr)

#  trail_zero --------
#    This purely aesthetic function adds zeroes that would 
#    otherwise be missing from the end of decimals in tables and plots.

makezerovec = function(x) vapply(x, function(x) rep(0, as.numeric(x)) %>% paste(collapse=""), "a")
trail_zero  = function(x, digits_desired=2){
  digits = nchar(gsub(".+\\.", "", x))
  if(max(digits)>digits_desired) x = round(x, digits_desired)   # round it
  x = as.character(x)
  x[!grepl("\\.", x)] = paste0(x[!grepl("\\.", x)], ".")        # add dropped decimal points
  digits = nchar(gsub(".+\\.", "", x))
  paste0(x, makezerovec(digits_desired - digits))               # add dropped zeros
}

#  BLOCK BOOTSTRAP --------------------

#  I. Component functions

#  Note: you must install the plyr package, but plyr's large number of 
#  conflicts with dplyr makes it inadvisable to load both packages at once.
#  Calling only these functions avoids the conflicts.
llply = plyr::llply
alply = plyr::alply

#  These functions generate Nsim random samples with replacement from the vector
#  of respondent IDs, then loop over them and find all the rows in the data matching
#  each ID number. This allows the function to work properly even when not all
#  responents have the same number of rows.
expand_id_boot  = function(samp, id) map(samp, function(x)which(id %in% x)) %>% unlist
make_bootsample = function(id, n, seed=0){
  set.seed(seed)
  ids = replicate(n, sample(unique(id), replace=T)) 
  alply(ids, 2, (function(x)expand_id_boot(x, id)))
}

#  This function randomly designates one candidate in each choice as candidate 1,
#  then drops candidate 2.
rand_Dplus_cand1 = function(x){  # x is a data.frame
  
  positions = which(x$Type == "D+ vs. D+")
  
  howManyC1 = ceiling(length(positions)/4)
  howManyC2 = floor(length(positions)/4)
  vec1 = sample(c(rep(1, howManyC1), rep(2, howManyC2)), 
                size = howManyC1+howManyC2, 
                replace = F)   # create a vector of 1s and 2s
  vec2 = -1*vec1 + 3           # and another one that replaces every 1 with a 2 and vice versa
  
  newNum = rep(NA_real_, length(positions))
  newNum[(1:(howManyC1+howManyC2))*2 - 1] = vec1  # then assign one to the odd-numbered positions
  newNum[(1:(howManyC1+howManyC2))*2]     = vec2  # and the other to the even-numbered positions
  
  x$candNum[positions] = newNum
  
  filter(x, candNum == 1)  # return x with only only candidate 1
}

#  II. Main function

block_boot = function(df,             #  a data.frame
                      fn,             #  the function that produces your estimates
                      id_var="id",    #  the name of the variable containing unique respondent IDs
                      n,              #  number of simulations
                      cand1_only=T,   #  if FALSE, analysis uses the full data. if TRUE, randomly assigns the D+ vs. D+ candidate 1 and drops candidate 2
                      groups_in_output=NULL,  #  controls which columns will be in the output data.frame along with your estimates
                      returnBoots=F,  #  if FALSE, function returns only a summary table. if TRUE, function returns all the bootstrap estimates
                      needed=NULL,    #  if NULL, function uses every variable in df. if a character vector, function only uses the specified variables
                      seed=0,         #  sets the seed inside make_bootsamples()
                      quantileType=7  #  becomes the type argument in quantile()
                      ){
  
  #  If specified, drop all un-needed variables
  if(!is.null(needed)) df = df[,needed]
  
  #  If we're going to use rand_Dplus_cand1() later, first make sure that...
  if(cand1_only){                         
    if(!("Type" %in% names(df))) stop("Must include variable 'Type' to randomize which D+ vs. D+ is designated candidate 1.")   #   1. we have all necessary variables
    df = arrange(df, id, matchNum, candNum)  #    2. the data are sorted such that all candidate pairs are consecutive rows. In the replication file, this is redundant.
  }
    
  #  Calculate point estimates.
  out = fn(df)
  
  #  Figure out which columns you'll need to merge all the bootstrap replicates together.
  if(is.null(groups_in_output)){     groups = names(out)[1:(ncol(out)-1)]
  } else if(groups_in_output=="df"){ groups = group_vars(df)
  } else if(groups_in_output=="out"){groups = group_vars(out)
  } else                             groups = groups_in_output
  boot = out[,groups,drop=F]   # has the output columns you'll need, and nothing else.
  
  #  Figure out the row numbers you'll need to create each bootstrapped version of the data.frame. 
  #     (This takes a long time. make_bootsample is the slow part of this function.)
  boot_samples = make_bootsample(as.data.frame(df)[,id_var], n, seed)
  
  #  Generate bootstrapped estimates.
  #     (Using left_join() makes sure that if for some reason one of your groups is missing in one of your bootstrap replicates, it'll show up NA instead of breaking the code or causing estimates to go in the wrong row. There is probably a faster way to do this, but make_bootsamples() is what is really slowing things down, so that is the higher priority improvement.)
  if(!cand1_only) booted = llply(boot_samples, (function(rownums) left_join(boot, fn(                 df[rownums,]),  by=groups))) %>% map_df(function(x)t(x[,length(x)]))     
  if(cand1_only)  booted = llply(boot_samples, (function(rownums) left_join(boot, fn(rand_Dplus_cand1(df[rownums,])), by=groups))) %>% map_df(function(x)t(x[,length(x)]))     
  booted = apply(booted, 2, as.numeric)
  
  #  Now we have a matrix of bootstrapped estimates. Each row is an estimated sampling distribution of the corresponding point estimate in "out".
  
  #  Bind your point estimates to the summary stats you want. Using cbind() is safe here because we used left_join() above.
  out = out %>% cbind(
    se    = apply(booted, 1, sd, na.rm=T),
    q2.5  = apply(booted, 1, quantile, .025, na.rm=T, type=quantileType),
    q97.5 = apply(booted, 1, quantile, .975, na.rm=T, type=quantileType),
    median= apply(booted, 1, median, na.rm=T),
    mean  = apply(booted, 1, mean, na.rm=T),
    gt0   = apply(booted, 1, function(x) mean(x>0, na.rm=T)),
    lt0   = apply(booted, 1, function(x) mean(x<0, na.rm=T)),
    NAs   = apply(booted, 1, function(x) sum(is.na(x)))
  ) %>% mutate(Nsim = ncol(booted))
  
  #  Do you want to save all of the bootstrap replicates, or just the summary?
  if(returnBoots)  out = cbind(out, booted)
  
  out
}

#  III. Version for scalars

block_boot_scalar = function(df, fn, id_var="id", n, needed=NULL, seed=0, quantileType=7, cand1_only=F){
  
  if(!is.null(needed)) df = as.data.frame(df[,needed])
  if(cand1_only)     df = arrange(df, id, matchNum, candNum)
  
  boot_samples = make_bootsample(as.data.frame(df)[,id_var], n, seed)
  
  if(!cand1_only) booted = plyr::laply(boot_samples, function(x) fn(df[x,]))
  if(cand1_only)  booted = plyr::laply(boot_samples, function(x) fn(rand_Dplus_cand1(df[x,])))
  
  c(estimate = fn(df),
    se    = sd(booted, na.rm=T),
    quantile(booted, .025, na.rm=T, type=quantileType),
    quantile(booted, .975, na.rm=T, type=quantileType),
    gt0   = mean(booted>0, na.rm=T),
    lt0   = mean(booted<0, na.rm=T),
    NAs   = sum(is.na(booted)),
    Nsim  = length(booted)
  )
}

#  Verification that rand_Dplus_cand1() preserves all choices
if(FALSE){
  dat_cand = read_csv("data/experiment.csv") %>% filter(!grepl("V", Type))
  test = rand_Dplus_cand1(dat_cand)
  unique(paste(test$id, test$matchNum)) %in% unique(paste(dat_cand$id, dat_cand$matchNum)) %>% mean
}

#  OTHER FUNCTIONS ----------------------


#  Vote share function for bootstrap of Figures 2-6
fn_binPref = function(x) x %>% summarize(estimate = weighted.mean(c_win, w = weight, na.rm=T))

#  Difference in means function for bootstrap of Figures 2-6
#  wraps around fn_binPref, must group_by(Type)
fn_DIM = function(x){
  x = fn_binPref(x)
  x$estimate = x$estimate - x$estimate[x$Type=="D+ vs. D+"]
  x
}



