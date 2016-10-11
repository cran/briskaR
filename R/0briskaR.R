# 0briskaR.R 
# Part of the briskaR package.
#
# Copyright (C) 2015        Melen Leclerc <melen.leclerc@inra.fr>
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

LAMBERT_93 <- "+init=epsg:2154"
BRISKAR_INTERN_PROJECTION <- LAMBERT_93

#' @title Load an internal working projection PROJ.4
#' @name LoadInternProjection
#' @description Will load a projection as internal package working projection
#' @param proj A character string of projection arguments, must be in the PROJ.4 documentation
# @rdname briskaR-methods
#' @export 
briskaRLoadInternProjection <- function (proj)
{
  BRISKAR_INTERN_PROJECTION <<- proj
  tryCatch(CRS(BRISKAR_INTERN_PROJECTION),
           error=function(cond) {
             message(cond)
             BRISKAR_INTERN_PROJECTION = LAMBERT_93
             return(NA)
           },
           warning=function(cond) {
             message(cond)
             BRISKAR_INTERN_PROJECTION = LAMBERT_93
             return(NULL)
           },
           finally={
             message("\nSet BriskaR working projection to ", BRISKAR_INTERN_PROJECTION)
          }
  )
}
