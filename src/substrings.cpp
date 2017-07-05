// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
using namespace Rcpp;

void add_indexes(
        std::unordered_map<std::string, std::unordered_set<int> >* dict,
        std::string* s,
        int idx, int k) {
    int len = s->length();

    if (len < k)
        return;

    for (int i = 0; i < len - k + 1; i++) {
        std::string token = s->substr(i, k);
        (*dict)[token].insert(idx);
    }

}

void create_dictionary(
        std::unordered_map<std::string, std::unordered_set<int> >* res,
        CharacterVector* x,
        int k) {

    for (int i = 0; i < x->size(); i++) {
        if (!CharacterVector::is_na((*x)[i])) {
            std::string s = std::string((*x)[i]);
            add_indexes(res, &s, i + 1, k);
        }
    }
}

// [[Rcpp::export]]
std::vector<std::unordered_set<int> > match_substrings(
        CharacterVector master,
        CharacterVector comparison,
        int k
) {
    std::vector<std::unordered_set<int> > res;

    std::unordered_map<std::string, std::unordered_set<int> > dictionary;
    create_dictionary(&dictionary, &comparison, k);

    for (int i = 0; i < master.size(); i++) {
        std::unordered_set<int> resultset;
        if (!CharacterVector::is_na(master[i])) {
            std::string s = std::string(master[i]);
            int len = s.length();

            for (int string_idx = 0; string_idx < len - k + 1; string_idx++) {
                std::string token = s.substr(string_idx, k);
                if (dictionary.count(token) > 0) {
                    std::unordered_set<int> values = dictionary[token];
                    for(auto val = values.begin(); val != values.end(); ++val) {
                        resultset.insert(*val);
                    }
                }
            }
        }
        res.push_back(resultset);
    }
    return res;
}
