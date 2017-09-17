#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Environment lookup_env(List cond, std::string sex, std::string starter) {
    Environment result = new_env();
    List listresult;
    List dfresult;
    Environment bysex = cond[sex];

    if (!bysex.exists(starter)) {
        result.assign("|", 1);
    } else {
        dfresult = bysex[starter];
        listresult = dfresult["data"];
        result = listresult[0];
    }
    return result;
}

// [[Rcpp::export]]
std::string interp(List conds, std::string sex, std::string starter,
                   NumericVector context_sizes,
                   NumericVector weights) {
    std::string result = starter;
    std::string nc = "";

    while (nc != "#") {
        Environment dist = new_env();
        if (nc != "|") result = result.append(nc);
        for (int i = 0; i < conds.size(); i++) {
            std::string start = result.substr(result.size() - context_sizes[i]);
            Environment this_dist = lookup_env(conds[i], sex, start);
            double this_weight = weights[i];
            CharacterVector this_keys = this_dist.ls(false);

            for (int k = 0; k < this_keys.size(); k++) {
                std::string key = std::string(this_keys[k]);
                double this_pct = this_dist[key];
                if (dist.exists(key))
                    dist[key] = double(dist[key]) + this_weight * this_pct;
                else dist[key] = this_weight * this_pct;
            }
        }

        double rnd = R::runif(0,1);
        double sum_so_far = 0;
        CharacterVector candidates = dist.ls(false);

        for (int cand = 0; cand < candidates.size() && sum_so_far < rnd; cand++) {
            std::string this_candidate = std::string(candidates[cand]);
            double this_prob = Rcpp::as<double>(dist.get(this_candidate));
            sum_so_far += this_prob;
            if (rnd <= sum_so_far) nc = this_candidate;
        }
    }
    return result;
}

// CharacterVector csample_char( CharacterVector x,
//                               int size,
//                               bool replace,
//                               NumericVector prob = NumericVector::create()
// ) {
//     CharacterVector ret = RcppArmadillo::sample(x, size, replace, prob) ;
//     return ret ;
// }
