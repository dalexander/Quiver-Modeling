
#define MAX_T 16

#include <Rcpp.h>
#include <iostream>

using std::cout;
using std::endl;
using namespace Rcpp;

template<class T>
int argmax(T lst)
{
    return std::max_element(lst.begin(), lst.end()) - lst.begin();
}

// [[Rcpp::export]]
int mleEstimateC(NumericMatrix loglikelihoodTable,
                 NumericVector obsHpLens)
{
    int nObs = obsHpLens.size();
    NumericVector hypothesisLLs(MAX_T + 1);

    for (int obs = 0; obs < nObs; obs++)
    {
        int r = obsHpLens[obs];
        for (int t = 0; t <= MAX_T; t++)
        {
            hypothesisLLs[t] += loglikelihoodTable(t, r);
        }
    }

    return argmax(hypothesisLLs);
}


// [[Rcpp::export]]
int dsMleEstimateC(List loglikelihoodTableByBase,
                   NumericVector obsHpLens,
                   CharacterVector hpBases)
{
    int nObs = obsHpLens.size();
    if (nObs != hpBases.size())
        stop("Invalid input");

    NumericVector hypothesisLLs(MAX_T + 1);

    // Accessing the list components in the inner loop appears to be
    // really slow.  Not really sure whether it is reentering the R
    // interpreter or it's just a really unoptimized code path.  In
    // any case ... using this hack for now
    NumericMatrix llTblA = as<NumericMatrix>(loglikelihoodTableByBase["A"]);
    NumericMatrix llTblC = as<NumericMatrix>(loglikelihoodTableByBase["C"]);
    NumericMatrix llTblG = as<NumericMatrix>(loglikelihoodTableByBase["G"]);
    NumericMatrix llTblT = as<NumericMatrix>(loglikelihoodTableByBase["T"]);

    for (int obs = 0; obs < nObs; obs++)
    {
        int r = obsHpLens[obs];
        char* base = hpBases[obs];
        for (int t = 0; t <= MAX_T; t++)
        {
            switch(base[0]) {
                case 'A': hypothesisLLs[t] += llTblA(t, r); break;
                case 'C': hypothesisLLs[t] += llTblC(t, r); break;
                case 'G': hypothesisLLs[t] += llTblG(t, r); break;
                case 'T': hypothesisLLs[t] += llTblT(t, r); break;
                default:
                    //print(base[0]);
                    Rcpp::Rcout << base[0] << "\n";
                    stop("Unrecognized base");
                    break;
            }
        }
    }

    return argmax(hypothesisLLs);
}
