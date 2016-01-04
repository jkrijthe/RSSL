/*    Copyright 2006 Vikas Sindhwani (vikass@cs.uchicago.edu)
 SVM-lin: Fast SVM Solvers for Supervised and Semi-supervised Learning

 This file is part of SVM-lin.

 SVM-lin is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any later version.

 SVM-lin is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with SVM-lin (see gpl.txt); if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#include <Rcpp.h>
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <cstring>
#include "ssl.h"
using namespace Rcpp;

struct options *Options = new options[1];
struct data *Data = new data[1];
struct vector_double *Weights = new vector_double[1];
struct vector_double *Outputs = new vector_double[1];
struct vector_double *Labels = new vector_double[1];

// [[Rcpp::export]]
List svmlin_rcpp(S4 X,
                 NumericVector y,
                 int l,
                 int algorithm,
                 double lambda,
                 double lambda_u,
                 int max_switch,
                 double pos_frac,
                 double Cp,
                 double Cn,
                 NumericVector costs,
                 bool verbose) {
  // Set options
  Options->algo = algorithm;
  Options->lambda=lambda;
  Options->lambda_u=lambda_u;
  Options->S=max_switch;
  Options->R=pos_frac;
  Options->epsilon=EPSILON;
  Options->cgitermax=CGITERMAX;
  Options->mfnitermax=MFNITERMAX;
  Options->Cp = Cp;
  Options->Cn = Cn;
  Options->verbose = verbose;

  NumericVector ycop( y.begin(), y.end() );
  NumericVector costcop( costs.begin(), costs.end() );
//   Rprintf("Step 1\n");
//   size_t size = ((DoubleVector)X.slot("x")).length()+((DoubleVector)Xu.slot("x")).length();
//   std::vector<double> vals(size);
//   std::vector<double>::iterator it = vals.begin();
//   vals.insert(it,((DoubleVector)X.slot("x")).begin(),((DoubleVector)Xu.slot("x")).end());
//   it = vals.begin()+((DoubleVector)X.slot("x")).length();
//   vals.insert(it,((DoubleVector)Xu.slot("x")).begin(),((DoubleVector)Xu.slot("x")).end());
//
//   Rprintf("Step 2\n");
//
//   size = ((IntegerVector)X.slot("i")).length()+((IntegerVector)Xu.slot("i")).length();
//   std::vector<int> colinds(size);
//   std::vector<int>::iterator it2 = colinds.begin();
//   colinds.insert(it2,((IntegerVector)X.slot("i")).begin(),((IntegerVector)X.slot("i")).end());
//   it2 = colinds.begin() + ((IntegerVector)X.slot("i")).length();
//   colinds.insert(it2,((IntegerVector)Xu.slot("i")).begin(),((IntegerVector)Xu.slot("i")).end());
//
//   size = ((IntegerVector)X.slot("p")).length()+((IntegerVector)Xu.slot("p")).length();
//   std::vector<int> rowpts(size);
//   it2 = rowpts.begin();
//   rowpts.insert(it2,((IntegerVector)X.slot("p")).begin(),((IntegerVector)X.slot("p")).end());
//   it2 = rowpts.begin() + ((IntegerVector)X.slot("p")).length();
//   rowpts.insert(it2,((IntegerVector)Xu.slot("p")).begin(),((IntegerVector)Xu.slot("p")).end());

  // R data to svmlin data structure
  Data->m=((IntegerVector)X.slot("Dim"))[1];
  Data->l=l;
  Data->u=Data->m-Data->l;
  Data->n=((IntegerVector)X.slot("Dim"))[0];
  Data->nz=((DoubleVector)X.slot("x")).size();
  Data->val=((DoubleVector)X.slot("x")).begin();
  Data->rowptr=((IntegerVector)X.slot("p")).begin();
  Data->colind=((IntegerVector)X.slot("i")).begin();
  Data->Y=ycop.begin();
  Data->C=costcop.begin();

  // TODO: load correct costs for unlabeled data.
  if (Options->verbose) {
    Rcout << "  Input Data Matrix Statistics:" << endl;
    Rcout << "      Examples: " << Data->m << endl;
    Rcout << "      Features: " << Data->n << " (including bias feature)" << endl;
    Rcout << "      Non-zeros:  " << Data->nz << " (including bias features)" << endl;
    Rcout << "      Average sparsity: " << Data->nz*1.0/Data->m << " non-zero features per example." << endl;
  }
    //   for (int i = 0; i<((DoubleVector)X.slot("x")).length();i++) {
//     Rprintf("val: %f \n",Data->val[i]);
//   }
//   for (int i = 0; i<((IntegerVector)X.slot("i")).length();i++) {
//     Rprintf("col: %d \n",Data->colind[i]);
//   }
//   for (int i = 0; i<((IntegerVector)X.slot("p")).length();i++) {
//     Rprintf("row: %d \n",Data->rowptr[i]);
//   }


  // Run
  ssl_train(Data,
            Options,
            Weights,
            Outputs);
//Clear(Data);

  return Rcpp::List::create(Rcpp::Named("Weights") = std::vector<double>(Weights->vec, Weights->vec+Weights->d),
                              Rcpp::Named("Outputs") = std::vector<double>(Outputs->vec, Outputs->vec+Outputs->d)
                              );
}
