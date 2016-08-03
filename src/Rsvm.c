
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include "svm.h"
#define Malloc(type,n) (type *)malloc((n)*sizeof(type))

/*
 * results from cross-validation
 */

struct crossresults
{
    double* results;
    double  total1;
    double  total2;
};

#ifndef _DENSE_REP

struct svm_node ** sparsify (double *x, int r, int c)
{
    struct svm_node** sparse;
    int         i, ii, count;
    
    sparse = (struct svm_node **) malloc (r * sizeof(struct svm_node *));
    for (i = 0; i < r; i++) {
	/* determine nr. of non-zero elements */
	for (count = ii = 0; ii < c; ii++)
	    if (x[i * c + ii] != 0) count++;

	/* allocate memory for column elements */
	sparse[i] = (struct svm_node *) malloc ((count + 1) * sizeof(struct svm_node));

	/* set column elements */
	for (count = ii = 0; ii < c; ii++)
	    if (x[i * c + ii] != 0) {
		sparse[i][count].index = ii + 1;
		sparse[i][count].value = x[i * c + ii];
		count++;
	    }

	/* set termination element */
	sparse[i][count].index = -1;
    }

    return sparse;
}

struct svm_node ** transsparse (double *x, int r, int *rowindex, int *colindex)
{
    struct svm_node** sparse;
    int i, ii, count = 0, nnz = 0;

    sparse = (struct svm_node **) malloc (r * sizeof(struct svm_node*));
    for (i = 0; i < r; i++) {
	/* allocate memory for column elements */
	nnz = rowindex[i+1] - rowindex[i];
	sparse[i] = (struct svm_node *) malloc ((nnz + 1) * sizeof(struct svm_node));

	/* set column elements */
	for (ii = 0; ii < nnz; ii++) {
	    sparse[i][ii].index = colindex[count];
	    sparse[i][ii].value = x[count];
	    count++;
	}

	/* set termination element */
	sparse[i][ii].index = -1;
    }    

    return sparse;
    
}    

#endif

/* Cross-Validation-routine from svm-train */
void do_cross_validation(struct svm_problem *prob,
			 struct svm_parameter *param,
			 int nr_fold,
			 double* cresults,
			 double* ctotal1,
			 double* ctotal2)
{
	int i;
	int total_correct = 0;
	double total_error = 0;
	double sumv = 0, sumy = 0, sumvv = 0, sumyy = 0, sumvy = 0;

	/* random shuffle */
	GetRNGstate();
	for(i=0; i<prob->l; i++)
	{
        int j = i+((int) (unif_rand() * (prob->l-i)))%(prob->l-i);
#ifndef _DENSE_REP
		struct svm_node *tx;
#else
        struct svm_node tx;
#endif
		double ty;
			
		tx = prob->x[i];
		prob->x[i] = prob->x[j];
		prob->x[j] = tx;

		ty = prob->y[i];
		prob->y[i] = prob->y[j];
		prob->y[j] = ty;
    }
	PutRNGstate();

	for(i=0; i<nr_fold; i++)
	{
		int begin = i*prob->l/nr_fold;
		int end = (i+1)*prob->l/nr_fold;
		int j,k;
		struct svm_problem subprob;

		subprob.l = prob->l-(end-begin);
#ifndef _DENSE_REP
        subprob.x = Malloc(struct svm_node*,subprob.l);
#else
        subprob.x = Malloc(struct svm_node,subprob.l);
#endif
		subprob.y = Malloc(double,subprob.l);
			
		k=0;
		for(j = 0; j < begin; j++)
		{
			subprob.x[k] = prob->x[j];
			subprob.y[k] = prob->y[j];
			++k;
		}
		for(j = end; j<prob->l; j++)
		{
			subprob.x[k] = prob->x[j];
			subprob.y[k] = prob->y[j];
			++k;
		}

		if(param->svm_type == EPSILON_SVR ||
		   param->svm_type == NU_SVR)
		{
			struct svm_model *submodel = svm_train(&subprob,param);
			double error = 0;
			for(j=begin;j<end;j++)
			{
#ifndef _DENSE_REP
                double v = svm_predict(submodel,prob->x[j]);
#else
                double v = svm_predict(submodel,&prob->x[j]);
#endif
				double y = prob->y[j];
				error += (v-y)*(v-y);
				sumv += v;
				sumy += y;
				sumvv += v*v;
				sumyy += y*y;
				sumvy += v*y;
			}
			svm_free_and_destroy_model(&submodel);
			/* printf("Mean squared error = %g\n",
			   error/(end-begin)); */
			cresults[i] = error/(end-begin);
			total_error += error;			
		}
		else
		{
			struct svm_model *submodel = svm_train(&subprob,param);
			int correct = 0;
			for(j=begin;j<end;j++)
			{
#ifndef _DENSE_REP
                double v = svm_predict(submodel,prob->x[j]);
#else
                double v = svm_predict(submodel,&prob->x[j]);
#endif
				if(v == prob->y[j])
					++correct;
			}
			svm_free_and_destroy_model(&submodel);
			/* printf("Accuracy = %g%% (%d/%d)\n", */
			/* 100.0*correct/(end-begin),correct,(end-begin)); */
			cresults[i] = 100.0*correct/(end-begin);
			total_correct += correct;
		}

		free(subprob.x);
		free(subprob.y);
	}
	
	if(param->svm_type == EPSILON_SVR || param->svm_type == NU_SVR)
	{
	    /* printf("Cross Validation Mean squared error = %g\n",total_error/prob.l);
	        printf("Cross Validation Squared correlation coefficient = %g\n",
	    	((prob.l*sumvy-sumv*sumy)*(prob.l*sumvy-sumv*sumy))/
	    	((prob.l*sumvv-sumv*sumv)*(prob.l*sumyy-sumy*sumy))
	    	); */
	    *ctotal1 = total_error/prob->l;
	    *ctotal2 = ((prob->l * sumvy - sumv * sumy) *
			(prob->l * sumvy - sumv*sumy))  /
		       ((prob->l * sumvv - sumv * sumv) *
		        (prob->l * sumyy - sumy * sumy));
	}
	else
	    /* printf("Cross Validation Accuracy =
	       %g%%\n",100.0*total_correct/prob.l); */
	    *ctotal1 = 100.0 * total_correct / prob->l;
}

#ifndef _DENSE_REP
void svmtrain (double *x, int *r, int *c,
#else
void svmtraind (double *x, int *r, int *c,
#endif
	       double *y,
	       int    *rowindex, int *colindex,
	       int    *svm_type,
	       int    *kernel_type,
	       int    *degree,
	       double *gamma,
	       double *coef0,
	       double *cost,
	       double *nu,
	       int    *weightlabels,
	       double *weights,
	       int    *nweights,
	       double *cache,
	       double *tolerance,
	       double *epsilon,
	       int    *shrinking,
	       int    *cross,
           int    *sparse,
	       int    *probability,
	       int    *ubound,
	       double *upbound,
	       
	       int    *nclasses,
	       int    *nr,
	       int    *index,
	       int    *labels,
	       int    *nSV,
	       double *rho,
	       double *coefs,
	       double *sigma,
	       double *probA,
	       double *probB,

	       double *cresults,
	       double *ctotal1,
	       double *ctotal2,
	       char   **error,
	       double *obj)
{
    struct svm_parameter par;
    struct svm_problem   prob;
    struct svm_model    *model = NULL;
    int i;
#ifdef _DENSE_REP
    int j, count, maxind;
#endif
    const char* s;
    
    /* set parameters */
    par.svm_type    = *svm_type;
    par.kernel_type = *kernel_type;
    par.degree      = *degree;
    par.gamma       = *gamma;
    par.coef0       = *coef0;
    par.cache_size  = *cache;
    par.eps         = *tolerance;
    par.C           = *cost;
    par.nu          = *nu;
    par.nr_weight   = *nweights;
    par.ubound      = *ubound;
    if (par.nr_weight > 0) {
	par.weight      = (double *) malloc (sizeof(double) * par.nr_weight);
	memcpy(par.weight, weights, par.nr_weight * sizeof(double));
	par.weight_label = (int *) malloc (sizeof(int) * par.nr_weight);
	memcpy(par.weight_label, weightlabels, par.nr_weight * sizeof(int));
    }
    par.p           = *epsilon;
    par.shrinking   = *shrinking;
    par.probability = *probability;

    /* set problem */
    prob.l = *r;
    prob.y = y;
    prob.upbound = upbound;
    
#ifndef _DENSE_REP
    if (*sparse > 0)
        prob.x = transsparse(x, *r, rowindex, colindex);
    else
        prob.x = sparsify(x, *r, *c);
#else
    prob.x = Malloc(struct svm_node, *r);
    for(i = 0; i < *r; i++)
        (prob.x + i)->values = Malloc(double, *c + 1);

    if (*sparse > 0) {
        count = 0;
        for(i = 0; i < *r; i++)
        {
            maxind = 1;
            (prob.x + i)->values[0] = 0.0;
            for(j = 0; j < (rowindex[i + 1] - rowindex[i]); j++) {
                while (maxind < colindex[count]) {
                    (prob.x + i)->values[maxind] = 0.0;
                    maxind++;
                }
                (prob.x + i)->values[maxind] = x[count];
                maxind++;
                count++;
            }
            (prob.x + i)->dim = maxind;
        }
    } else {
        for(i = 0; i < *r; i++)
        {
            (prob.x + i)->dim = *c + 1;
            if (*kernel_type == PRECOMPUTED)
                (prob.x + i)->values[0] = (double)(i + 1);
            else
                (prob.x + i)->values[0] = 0.0;
            for(j = 0; j < *c; j++)
                (prob.x + i)->values[j + 1] = x[i*(*c) + j];
        }
    }
#endif
    
    /* check parameters & copy error message */
    s = svm_check_parameter(&prob, &par);
    if (s) {
	strcpy(*error, s);
    } else {

	/* call svm_train */
	model = svm_train(&prob, &par);
    
	/* set up return values */

	/*	for (ii = 0; ii < model->l; ii++)
	    for (i = 0; i < *r;	i++)
	    if (prob.x[i] == model->SV[ii]) index[ii] = i+1; */
	svm_get_sv_indices(model, index);
	
	*nr  = model->l;
	*nclasses = model->nr_class;
	*obj = model->obj;
	memcpy (rho, model->rho, *nclasses * (*nclasses - 1)/2 * sizeof(double));

	if (*probability && par.svm_type != ONE_CLASS) {
	  if (par.svm_type == EPSILON_SVR || par.svm_type == NU_SVR)
	    *sigma = svm_get_svr_probability(model);
	  else {
	    memcpy(probA, model->probA, 
		    *nclasses * (*nclasses - 1)/2 * sizeof(double));
	    memcpy(probB, model->probB, 
		    *nclasses * (*nclasses - 1)/2 * sizeof(double));
	  }
	}

	for (i = 0; i < *nclasses-1; i++)
	    memcpy (coefs + i * *nr, model->sv_coef[i],  *nr * sizeof (double));
	
	if (*svm_type < 2) {
	    memcpy (labels, model->label, *nclasses * sizeof(int));
	    memcpy (nSV, model->nSV, *nclasses * sizeof(int));
	}
	
    /* Perform cross-validation, if requested */
	if (*cross > 0)
	    do_cross_validation (&prob, &par, *cross, cresults,
				 ctotal1, ctotal2);

	/* clean up memory */
	svm_free_and_destroy_model(&model);
    }
    
    /* clean up memory */
    if (par.nr_weight > 0) {
	free(par.weight);
	free(par.weight_label);
    }

#ifndef _DENSE_REP
    for (i = 0; i < *r; i++) free (prob.x[i]);
#else
    for(i=0; i < *r; ++i)
        free((prob.x+i)->values);
#endif
    free (prob.x);
}

#ifndef _DENSE_REP
void svmpredict  (int    *decisionvalues,
#else
void svmpredictd  (int    *decisionvalues,
#endif
		  int    *probability,

		  double *v, int *r, int *c,
		  int    *rowindex,
		  int    *colindex,
		  double *coefs,
		  double *rho,
		  int    *compprob,
		  double *probA,
		  double *probB,
		  int    *nclasses,
		  int    *totnSV,
		  int    *labels,
		  int    *nSV,
		  int    *sparsemodel,

		  int    *svm_type,
		  int    *kernel_type,
		  int    *degree,
		  double *gamma,
		  double *coef0,

		  double *x, int *xr,
		  int    *xrowindex,
		  int    *xcolindex,
		  int    *sparsex,
		  
		  double *ret,
		  double *dec,
		  double *prob)
{
    struct svm_model m;
    struct svm_node ** train;
    int i;
#ifdef _DENSE_REP
    int j, maxind, maxcol, count;
#endif
    
    /* set up model */
    m.l        = *totnSV;
    m.nr_class = *nclasses;
    m.sv_coef  = (double **) malloc (m.nr_class * sizeof(double*));
    for (i = 0; i < m.nr_class - 1; i++) {
      m.sv_coef[i] = (double *) malloc (m.l * sizeof (double));
      memcpy (m.sv_coef[i], coefs + i*m.l, m.l * sizeof (double));
    }
    
#ifndef _DENSE_REP
    if (*sparsemodel > 0)
        m.SV   = transsparse(v, *r, rowindex, colindex);
    else
        m.SV   = sparsify(v, *r, *c);
#else
    if (*kernel_type == 4) {
        m.SV = Malloc(struct svm_node, m.l);
        for (i = 0; i < m.l; i++) {
            m.SV[i].dim = 1;
            m.SV[i].values = Malloc(double, 1);
            m.SV[i].values[0] = v[i];
        }
    } else {
        if (*sparsemodel > 0) {
            count = 0;
            m.SV = Malloc(struct svm_node, *r);
            for(i = 0; i < *r; i++) {
                maxcol = colindex[rowindex[i + 1] - 2];
                m.SV[i].dim = maxcol + 1;
                m.SV[i].values = Malloc(double, maxcol + 1);
                m.SV[i].values[0] = 0;
                maxind = 1;
                for(j = 0; j < (rowindex[i + 1] - rowindex[i]); j++) {
                    while (maxind < colindex[count]) {
                        m.SV[i].values[maxind] = 0;
                        maxind++;
                    }
                    m.SV[i].values[maxind] = v[count];
                    maxind++;
                    count++;
                }
            }
        } else {
            m.SV = Malloc(struct svm_node, *r);
            for (i = 0; i < *r; i++) {
                m.SV[i].dim = *c + 1;
                m.SV[i].values = Malloc(double, *c + 1);
                m.SV[i].values[0] = 0;
                for (j = 0; j < *c; j++)
                    m.SV[i].values[j+1] = v[i * (*c) + j];
            }
        }
    }
#endif
    
    m.rho      = rho;
    m.probA    = probA;
    m.probB    = probB;
    m.label    = labels;
    m.nSV      = nSV;

    /* set up parameter */
    m.param.svm_type    = *svm_type;
    m.param.kernel_type = *kernel_type;
    m.param.degree      = *degree;
    m.param.gamma       = *gamma;
    m.param.coef0       = *coef0;
    m.param.probability = *compprob;      

    m.free_sv           = 1;
    
#ifndef _DENSE_REP
    /* create sparse training matrix */
    if (*sparsex > 0)
        train = transsparse(x, *xr, xrowindex, xcolindex);
    else
        train = sparsify(x, *xr, *c);
#else
    train = Malloc(struct svm_node *, *xr);
    for (i = 0 ; i < *xr; i++)
        train[i] = Malloc(struct svm_node, 1);

    if (*sparsex > 0) {
        // sparse kernel matrix not supported
        count = 0;
        for(i = 0; i < *xr; i++)
        {
            maxcol = xcolindex[xrowindex[i + 1] - 2];
            train[i]->dim = maxcol + 1;
            train[i]->values = Malloc(double, maxcol + 1);
            train[i]->values[0] = 0;
            maxind = 1;
            for(j = 0; j < (xrowindex[i + 1] - xrowindex[i]); j++) {
                while (maxind < xcolindex[count]) {
                    train[i]->values[maxind] = 0;
                    maxind++;
                }
                train[i]->values[maxind] = x[count];
                maxind++;
                count++;
            }
        }
    } else {
        for(i = 0; i < *xr; i++)
        {
            train[i]->dim = *c + 1;
            train[i]->values = Malloc(double, *c + 1);
            if (*kernel_type == PRECOMPUTED) {
                train[i]->values[0] = (double)(i + 1);
                for(j = 1; j < *c + 1; j++)
                    train[i]->values[j] = 0;
                for(j = 0; j < m.l; j++)
                    train[i]->values[(int) v[j]] = x[i * (*c) + (int) v[j]-1];
            } else {
                train[i]->values[0] = 0;
                for(j = 0; j < *c; j++)
                    train[i]->values[j + 1] = x[i * (*c) + j];
            }
        }
    }
#endif

    /* call svm-predict-function for each x-row, possibly using probability
       estimator, if requested */
    if (*probability && svm_check_probability_model(&m)) {
      for (i = 0; i < *xr; i++)
	ret[i] = svm_predict_probability(&m, train[i], prob + i * *nclasses);
    } else {
      for (i = 0; i < *xr; i++)
	ret[i] = svm_predict(&m, train[i]);
    }

    /* optionally, compute decision values */
    if (*decisionvalues)
      for (i = 0; i < *xr; i++)
	svm_predict_values(&m, train[i], dec + i * *nclasses * (*nclasses - 1) / 2);

    /* clean up memory */

#ifndef _DENSE_REP
    for (i = 0; i < *xr; i++)
        free(train[i]);
    free(train);
    
    for (i = 0; i < *r; i++)
        free(m.SV[i]);
    free(m.SV);
#else
    for (i = 0; i < *xr; i++)
    {
        free(train[i]->values);
        free(train[i]);
    }
    free(train);
    
    if (*kernel_type == PRECOMPUTED) {
        for (i = 0; i < m.l; i++)
            free(m.SV[i].values);
        free(m.SV);
    } else {
        for (i = 0; i < *r; i++)
            free(m.SV[i].values);
        free(m.SV);
    }
#endif

    for (i = 0; i < m.nr_class - 1; i++)
      free(m.sv_coef[i]);
    free(m.sv_coef);
}	     

#ifndef _DENSE_REP

void svmwrite (double *v, int *r, int *c,
		  int    *rowindex,
		  int    *colindex,
		  double *coefs,
		  double *rho,
	          int    *compprob,
	          double *probA,
	          double *probB,
		  int    *nclasses,
		  int    *totnSV,
		  int    *labels,
		  int    *nSV,
		  int    *sparsemodel,

		  int    *svm_type,
		  int    *kernel_type,
		  int    *degree,
		  double *gamma,
		  double *coef0,

		  char **filename) 

{
    struct svm_model m;
    int i;
    char *fname = *filename;    

    /* set up model */
    m.l        = *totnSV;
    m.nr_class = *nclasses;
    m.sv_coef  = (double **) malloc (m.nr_class * sizeof(double*));
    for (i = 0; i < m.nr_class - 1; i++) {
	m.sv_coef[i] = (double *) malloc (m.l * sizeof (double));
	memcpy (m.sv_coef[i], coefs + i*m.l, m.l * sizeof (double));
    }
    
    if (*sparsemodel > 0)
	m.SV   = transsparse(v, *r, rowindex, colindex);
    else
	m.SV   = sparsify(v, *r, *c);
    
    m.rho      = rho;
    m.label    = labels;
    m.nSV      = nSV;
    if (*compprob) {
	m.probA    = probA;
	m.probB    = probB;
    } else {
	m.probA    = NULL;
	m.probB    = NULL;
    }

    /* set up parameter */
    m.param.svm_type    = *svm_type;
    m.param.kernel_type = *kernel_type;
    m.param.degree      = *degree;
    m.param.gamma       = *gamma;
    m.param.coef0       = *coef0;

    m.free_sv           = 1;

    /* write svm model */
    svm_save_model(fname, &m);

    for (i = 0; i < m.nr_class - 1; i++)
	free(m.sv_coef[i]);
    free(m.sv_coef);

    for (i = 0; i < *r; i++)
	free (m.SV[i]);
    free (m.SV);

}

#endif

