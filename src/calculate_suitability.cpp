
// metabolic_scaling.cpp: c++ implementation of the metabolic scaling
// as descibed in the metabolic theory of ecology (Brown et al. 2004)
//
// Copyright (C) 2023  Stefan Fallert
//
// This file is part of metaRange.
//
// metaRange is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 3.
//
// metaRange is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with metaRange.  If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
using namespace Rcpp;

//' Calculate (estimate) environmental suitability
//'
//' Calculate / estimate the environmental suitability for a given environmental value,
//' based on the three "cardinal" values of the species for that environmental niche.
//'
//' @param vmax `<numeric>` upper (i.e. maximum) tolerable value
//' @param vopt `<numeric>` optimal (i.e. prefered) value
//' @param vmin `<numeric>` lower (i.e. minimum) tolerable value
//' @param venv `<numeric>` environmental value for which to calculate the suitability
//' @return `<numeric>` environmental suitability
//' @details The environmental suitability is calculated based on a beta distribution
//' after a formula provided by Yan & Hunt (1999) (see references paragraph)
//' \deqn{suitability = (\frac{V_{max} - V_{env}}{V_{max} - V_{opt}}) * (\frac{V_{env} - V_{min}}{V_{opt} - V_{min}})^{\frac{V_{opt} - V_{min}}{V_{max} - V_{opt}}}}
//' @note The original formula by Yan & Hunt was only intended to calculate
//' the relative daily growth rate of plants in relation to temperature. The abstraction to
//' use this to A) calculate a niche suitability; and B) use it on other
//' environmental values than temperature might not be valid. However, the assumption that the
//' environmental suitability for one niche dimension is highest at one optimal value and
//' decreases towards the tolerable minumum and maximum values seems reasonable.
//' @references
//' Weikai Yan, L.A. Hunt, (1999)
//' An Equation for Modelling the Temperature Response of Plants using only the Cardinal Temperatures,
//' Annals of Botany,
//' Volume 84, Issue 5,
//' Pages 607-614,
//' ISSN 0305-7364, <doi:10.1006/anbo.1999.0955>
//' @export
// [[Rcpp::export]]
NumericVector calculate_suitability(
        double vmax,
        double vopt,
        double vmin,
        NumericVector venv) {
    if (vmax < vopt || vopt < vmin) {
        stop("Arguments don't meet the following criteria: vmax > vopt > vmin");
    }
    NumericVector result (venv.size());
    for (int i = 0; i < result.size(); i++) {
        if (venv[i] < vmin || venv[i] > vmax) {
            result[i] = 0.0;
            continue;
        }
        result[i] = ((vmax - venv[i]) / (vmax - vopt)) * pow(((venv[i] - vmin) / (vopt - vmin)), ((vopt - vmin) / (vmax - vopt)));
        if (NumericVector::is_na(result[i])) {
            result[i] = 0.0;
        }
    }
    result.attr("dim") = venv.attr("dim");
    return result;
}