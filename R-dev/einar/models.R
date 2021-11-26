#' Title
#'
#' @param d A data frame with xx
#' @param cs The multiplier on the sigma value of the first Gussian mode (not active)
#' @param minimal A boolean (default TRUE) determining detail of what is returned
#'
#' @return A tibble
#' @export
#'
rb_gaussian_binary_clustering <- function(d, vid, time, lon, lat, cs = 1.96, minimal = TRUE) {
  d <- 
    d %>% 
    # EMbc::stbc expect the order of the data to be time, lon, lat
    tidyr::nest(data = -c({{vid}})) %>% 
    dplyr::mutate(md = purrr::map(data, select, {{ time }}, {{ lon }}, {{ lat }}),
                  # function EMbC throws error on tibble
                  md = purrr::map(md, as.data.frame),
                  md = purrr::map(md, EMbC::stbc, info = -1),
                  rt = purrr::map(md, rb_tidy_bin_clst_path),
                  rt = purrr::map(rt, select, .time = dTm, .turn = turn, 
                                  .speed = speed, .A = A))
  
  if(minimal) {
    d %>% 
      dplyr::select( {{ vid }}, data, rt) %>%
      tidyr::unnest(c(data, rt)) %>%
      dplyr::mutate(.speed = ms2kn(.speed)) %>%
      dplyr::ungroup() %>% 
      return()
  } else {
    return(d)
  }
}


# # get to work inside a function call
# rb_prep_data <- function(d, vid, lon, lat, minimal = TRUE, cs = 1.96) {
#   
#   cd <- c(deparse(substitute(lon)), deparse(substitute(lat)))
#   type <- ifelse(any(cd %in% "x"), "UTM", "LL")
#   mult <- ifelse(type == "UTM", 1, 1000)
#   
#   d <- 
#     d %>% 
#     tidyr::nest(data = -c( {{ vid }} )) %>%
#     dplyr::mutate(data = purrr::map(data, as.data.frame),
#                   pd = purrr::map(data, select, {{ lon }}, {{ lat }} ),
#                   pd = purrr::map(pd, function(x) moveHMM::prepData(x, type = type, coordNames = cd)),
#                   # or?
#                   # assume last step (currently NA) is the same as the second last step
#                   pd = purrr::map(pd, fill, step))
#   return(d)
#   
# }



#' Fit Gaussian mixture model on vessel speed
#'
#' @param d A data frame with xx
#' @param cs The multiplier on the sigma value of the first Gussian mode
#' @param minimal A boolean (default TRUE) determining detail of what is returned
#'
#' @return A tibble containing lower and upper speed threshold of the first Gaussian mode
#' @export
#'
rb_gaussian <- function(d, vid, lon, lat, minimal = TRUE, cs = 1.96) {
  
  cd <- c(deparse(substitute(lon)), deparse(substitute(lat)))
  type <- ifelse(any(cd %in% "x"), "UTM", "LL")
  mult <- ifelse(type == "UTM", 1, 1000)
  
  d <- 
    d %>% 
    tidyr::nest(data = -c( {{ vid }} )) %>%
    # if already grouped, this suffices
    #tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, as.data.frame),
                  pd = purrr::map(data, select, {{ lon }}, {{ lat }} ),
                  pd = purrr::map(pd, function(x) moveHMM::prepData(x, type = type, coordNames = cd)),
                  # or?
                  # assume last step (currently NA) is the same as the second last step
                  pd = purrr::map(pd, fill, step),
                  pd = purrr::map(pd, dplyr::select, step, angle),
                  pd = purrr::map(pd, dplyr::mutate, .speed = ms2kn(step * mult / 60)),
                  md = purrr::map(pd, function(x) mixtools::normalmixEM(x$.speed, mu = c(1,4,8), sigma = c(1,1,1))),
                  threshold.lower = purrr::map_dbl(md, function(x) x$mu[1] - cs * x$sigma[1]),
                  threshold.upper = purrr::map_dbl(md, function(x) x$mu[1] + cs * x$sigma[1]))
  
  if(minimal) {
    d %>%
      dplyr::select( {{vid}} , threshold = threshold.upper) %>%
      return()
  } else {
    return(d)
  }
}


# NOTE: ONLY TESTED USING UTM
#' Hidden Markov model with step only
#'
#' @param d A data frame with xx
#' @param minimal A boolean (default TRUE) determining detail of what is returned
#'
#' @return A tibble
#' @export
#'
rb_hidden_markov_step <- function(d, vid, time, x, y,  mu = c(20, 150, 300), sigma =c(20, 20, 20), minimal = TRUE) {
  
  # Note: These functions should likely not be hardwired in the function call
  
  par_mixEM <- function(o) {
    sigma <- c(o$sigma[1], o$sigma[2] * (1 - 1/4), o$sigma[3] + 2)
    c(o$mu, sigma)
  }
  
  lh_fitHMM <- function(o, par0) {
    moveHMM::fitHMM(data = o,  nbStates = 3, stepPar0 = par0,
                    verbose = 0, stepDist = "gamma", angleDist = "none")
  }
  
  d <- 
    d %>% 
    tidyr::nest(data = -c( vid )) %>%
    # if already grouped, this suffices
    #tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, as.data.frame),
                  pd = purrr::map(data, select, {{ x }}, {{ y }}),
                  pd = purrr::map(pd, function(x) moveHMM::prepData(x, type = "UTM", coordNames = c("x", "y"))),
                  # or?
                  # assume last step (currently NA) is the same as the second last step
                  pd = purrr::map(pd, fill, step)) %>% 
    # get starting values for the parameters
    dplyr::mutate(mixEM = purrr::map(pd, function(x) mixtools::normalmixEM(x$step, mu = mu, sigma = sigma))) %>%
    # extract the parameters - note, should possibly be just one step, all in a single function
    dplyr::mutate(par0 = purrr::map(mixEM, par_mixEM)) %>%
    dplyr::mutate(model = purrr::map2(pd, par0, lh_fitHMM)) %>%
    dplyr::mutate(.ks = purrr::map(model, rb_tidy_movehmm)) %>%
    dplyr::mutate(.vit = purrr::map(model, moveHMM::viterbi))
  
  if(minimal) {
   d %>% 
      mutate(pd = purrr::map(pd, select, .step = step, .angle = angle)) %>% 
      select( {{ vid }}, data, pd, .vit) %>% 
      unnest(c(data, pd, .vit)) %>% 
      return()
  } else {
    return(d)
  }
  
}

#' Hidden Markov model with step and turn
#'
#' @param d A data frame with xx
#' @param minimal A boolean (default TRUE) determining detail of what is returned
#'
#' @return A tibble
#' @export
#'
rb_hidden_markov_step_and_turn <- function(d, vid, time, x, y,  mu = c(20, 150, 300), sigma =c(20, 20, 20), minimal = TRUE) {
  
  # Note: These functions should likely not be hardwired in the function call
  
  par_mixEM <- function(o) {
    sigma <- c(o$sigma[1], o$sigma[2] * (1 - 1/4), o$sigma[3] + 2)
    c(o$mu, sigma)
  }
  
  lh_fitHMM_model5 <- function(d, par0) {
    
    angleMean0 <- c(pi, pi/2, 0) # angle mean
    kappa0 <-     c(0.1, 0.2, 0.8)   # angle concentration
    anglePar0 <- c(angleMean0, kappa0)
    
    moveHMM::fitHMM(data = d,  nbStates = 3, stepPar0 = par0,
                    verbose = 0, stepDist = "gamma", angleDist = "wrpcauchy",
                    anglePar0 = anglePar0)
  }
  
  d <- 
    d %>% 
    tidyr::nest(data = -c( vid )) %>%
    # if already grouped, this suffices
    #tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, as.data.frame),
                  pd = purrr::map(data, select, {{ x }}, {{ y }}),
                  pd = purrr::map(pd, function(x) moveHMM::prepData(x, type = "UTM", coordNames = c("x", "y"))),
                  # or?
                  # assume last step (currently NA) is the same as the second last step
                  pd = purrr::map(pd, fill, step)) %>% 
    # get starting values for the parameters
    dplyr::mutate(mixEM = purrr::map(pd, function(x) mixtools::normalmixEM(x$step, mu = mu, sigma = sigma))) %>%
    # extract the parameters - note, should possibly be just one step, all in a single function
    dplyr::mutate(par0 = purrr::map(mixEM, par_mixEM)) %>%
    dplyr::mutate(model = purrr::map2(pd, par0, lh_fitHMM_model5)) %>%
    dplyr::mutate(.ks = purrr::map(model, rb_tidy_movehmm)) %>%
    dplyr::mutate(.vit = purrr::map(model, moveHMM::viterbi)) 
  
  if(minimal) {
    d %>% 
      mutate(pd = purrr::map(pd, select, .step = step, .angle = angle)) %>% 
      select( {{ vid }}, data, pd, .vit) %>% 
      unnest(c(data, pd, .vit)) %>% 
      return()
  } else {
    return(d)
  }
}

#' tidy_normalmixEM
#'
#' @param o Object (class mixEM) that contains object of class normalmixEM
#'
#' @return a tibble
#' @export
#'
rb_tidy_normalmixEM <- function(o) {
  if(class(o) != "mixEM") stop("The object is not of class 'mixEM'")
  if(o$ft != "normalmixEM") stop("The object is not of class 'normalmixEM'")
  n.modes <- length(o$mu)
  est <-
    tibble::tibble(modes = 1:n.modes,
                   mu = o$mu,
                   lamda = o$lambda,
                   sigma = o$sigma)
  
  pos <-
    tidyr::expand_grid(x = o$x,
                       modes = 1:n.modes) %>%
    dplyr::left_join(est, by = "modes") %>%
    dplyr::mutate(y = plotmm::plot_mix_comps(x, mu, sigma, lamda, normal = TRUE)) %>%
    dplyr::arrange(modes)
  
  return(pos)
  
}

# # usage
# prepdat <-
#   read.table("doi_10.5061_dryad.k80bp46__v1/example_data.txt",
#              header=TRUE, sep=",") %>%
#   # use the prepData function to create step lengths
#   moveHMM::prepData(type = "UTM", coordNames = c("x", "y")) %>%
#   filter(ID == "AR001") %>%
#   filter(!is.na(step)) %>%
#   # convert step length from meters to knots (nautical miles per hour)
#   mutate(speed = step/ 60 * 1.9438)
# normalmixEM(prepdat$speed, mu = c(1,4,8), sigma = c(1,1,1)) %>%
#   tidy_normalmixEM() %>%
#   ggplot() +
#   geom_density(aes(x = x)) +
#   geom_line(aes(x, y, colour = factor(modes)))



#' tidy_bin_clst_path
#'
#' @param o Object of class ...
#'
#' @return a tibble
#' @export
#'
rb_tidy_bin_clst_path <- function(o) {
  o@pth %>%
    dplyr::mutate(spn = o@spn,
                  dst = o@dst,
                  hdg = o@hdg,
                  #W = o@W,
                  A = o@A,
                  turn = EMbC:::getTurns(o),
                  speed = EMbC:::getSpeed(o)) %>%
    tibble::as_tibble()
}

# # usage
# d <-
#   read.table("doi_10.5061_dryad.k80bp46__v1/example_data.txt",
#              header=TRUE, sep=",") %>%
#   mutate(time = lubridate::ymd_hms(date)) %>%
#   # use the prepData function to create step lengths
#   moveHMM::prepData(type = "UTM", coordNames = c("x", "y")) %>%
#   # check if there are any missing step lengths
#   # there will be at least five (=the number of unique IDs) because the first step
#   # length of each trip will be NA
#   # remove missing step lengths
#   filter(!is.na(step)) %>%
#   # convert step length from meters to knots (nautical miles per hour)
#   mutate(speed = step / 60 * 1.9438) %>%
#   as_tibble() %>%
#   sf::st_as_sf(coords = c("x", "y"),
#                crs = '+proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0',
#                remove = FALSE) %>%
#   sf::st_transform(crs = 4326) %>%
#   mutate(lon = sf::st_coordinates(geometry)[,1],
#          lat = sf::st_coordinates(geometry)[,2]) %>%
#   sf::st_drop_geometry()
# o <-
#   d %>%
#   filter(ID == "AR001") %>%
#   select(time, lon, lat) %>%
#   as.data.frame() %>%
#   stbc(., info = -1)
# tidy_bin_clst_path(o)


#' tidy_movehmm
#'
#' @param o Object of class moveHMM
#'
#' @return a tibble
#' @export
#'
rb_tidy_movehmm <- function(o) {
  o$mle %>%
    unlist() %>%
    as.data.frame() %>%
    tibble::as_tibble(rownames = "par")
}



