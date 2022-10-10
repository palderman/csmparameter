#'
#' @export
#'
#' @importFrom magrittr "%>%"
#'
prm_create_prior_density <- function(pmin, pmax, pmu, psigma){
  # Function for creating local constants for prior density function
  p_to_code <- function(name, val) paste0(name, " <- ",
                                          deparse(val, control = c("all", "hexNumeric")))
  prior_density_fun <- c(
    "function(pval){",
    # define local constants
    p_to_code("pmin", pmin),
    p_to_code("pmax", pmax),
    p_to_code("pmu", pmu),
    p_to_code("psigma", psigma),
    #
    "dens_adj <- log(pnorm(pmax, mean = pmu, sd = psigma) - pnorm(pmin, mean = pmu, sd = psigma))",
    "lp <- dnorm(pval, mean = pmu, sd = psigma, log = TRUE) - dens_adj",
    "return(lp)",
    "}") %>%
    parse(text =.) %>%
    eval()

  return(prior_density_fun)

}
