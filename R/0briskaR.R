# 0briskaR.R
# Part of the briskaR package.
#
# Copyright (C) 2019        Virgile Baudrot <virgile.baudrot@posteo.net>
#                           Melen Leclerc <melen.leclerc@inra.fr>
#                           Jean-Francois Rey <jean-francois.rey@inra.fr>
#                           Samuel Soubeyrand <Samuel.Soubeyrand@inra.fr>
#                           Emily Walker <emily.walker@inra.fr>
#                           INRA - BioSP Site Agroparc - 84914 Avignon Cedex 9
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

# Ugly hack to get rid of spurious notes in package check. R is such a sad language!
# if(getRversion() >= "2.15.1")  utils::globalVariables(c(
#   "response", "Nreprocumul", "resp", "Mortality", "qinf95", "qsup95",
#   "transf_conc", "obs", "pred", "..n..", "Points", "conc", "Line", "Nsurv",
#   "time", "Conf.Int", "Cred.Lim", "Obs", "P50", "P2.5", "P97.5", "variable",
#   "value", "jittered_conc", "reproRateInf", "reproRateSup", "curv_conc", "q50",
#   "psurv", "concentration", ".", "time_ID", "i_row", "nbrReplicate_ConcTime",
#   "time_ID_red", "x.pts", "y.pts", "pts.leg", "survRate_key", "survRate_value",
#   "Nsurv_qinf95", "Nsurv_qsup95", "col_range", "Nsurv_q50", "Nprec", "decrease",
#   "time_ID_long", "Nsurv_key", "Nsurv_value", "Nsurv_qinf95_valid", "Nsurv_qsup95_valid",
#   "Nsurv_q50_valid","Nsurv_qinf95_check", "Nsurv_qsup95_check","Nsurv_q50_check",
#   "conf_int_qinf95", "conf_int_qsup95", "kd_log10", "alpha_log10",
#   "kk_log10", "z_log10", "parameters", "density"))

#### BECAUSE AS-CRAN CHECKING ASK FOR !!!
# #' @title Apply a Function over a List or Vector
# #'
# #' @description lapply returns a list of the same length as X, each element of which is the result of applying FUN to the corresponding element of X.
# #'  
# #' @keywords internal
# #' @export
# #' @importFrom base lapply
# NULL

# This dummy function definition is included with the package to ensure that
# 'tools::package_native_routine_registration_skeleton()' generates the required
# registration info for the 'run_testthat_tests' symbol.
# (function() {
#   .Call("run_testthat_tests", PACKAGE = "briskaR")
# })


#' LAMBERT_93
#'
#' SIG projection Lambert_93 references "+init=epsg:2154" PROJ.4
#'
#' @export
LAMBERT_93 <- "+init=epsg:2154"

#BRISKAR_INTERN_PROJECTION <- NULL
## briskaR environnement for package varaibles
.briskar_env <- new.env()
.briskar_env$BRISKAR_INTERN_PROJECTION <- LAMBERT_93

#' @title Load an internal working projection PROJ.4
#' @name briskaRLoadInternProjection
#' @description Will load a projection as internal package working projection
#' @param proj A character string of projection arguments, must be in the PROJ.4 documentation
# @rdname briskaR-methods
#' @export
briskaRSetInternProjection <- function (proj = LAMBERT_93) {
  #unlockBinding("BRISKAR_INTERN_PROJECTION",env=getNamespace("briskaR"))
  #BRISKAR_INTERN_PROJECTION <<- proj
  #lockBinding("BRISKAR_INTERN_PROJECTION",env=getNamespace("briskaR"))
  .briskar_env$BRISKAR_INTERN_PROJECTION <- proj
  tryCatch(
    temp <- CRS(.briskar_env$BRISKAR_INTERN_PROJECTION),
    error = function(cond) {
      message(cond)
      .briskar_env$BRISKAR_INTERN_PROJECTION <- LAMBERT_93
      return(NA)
    },
    warning = function(cond) {
      message(cond)
      .briskar_env$BRISKAR_INTERN_PROJECTION <- LAMBERT_93
      return(NULL)
    },
    finally = {
      message("\nSet BriskaR working projection to ",
              .briskar_env$BRISKAR_INTERN_PROJECTION)
    }
  )
  return(.briskar_env$BRISKAR_INTERN_PROJECTION)
}

#' @title Get the internal working projection PROJ.4
#' @name GetInternProjection
#' @description Will print and return the internal projection of briskaR package
#' @export
briskaRGetInternProjection <- function() {
  message("\nBriskaR working projection is set to ",
          .briskar_env$BRISKAR_INTERN_PROJECTION)
  print(CRS(.briskar_env$BRISKAR_INTERN_PROJECTION))
  return(.briskar_env$BRISKAR_INTERN_PROJECTION)
}
