/*===================================================*/
/* Index:                                            */
/*  |-------------------+------+------+----------|   */
/*  | function          | NaN  | Edge | Underflow|   */
/*  |-------------------+------+------+----------|   */
/*  | sum_exact         | NA   | NA   | 1024     |   */
/*  | cumsum_exact      | NA   | NA   | 1024     |   */
/*  | run_min           | yes  | yes  |   NA     |   */
/*  | run_max           | yes  | yes  |   NA     |   */
/*  | run_quantile_lite | no   | no   |   NA     |   */
/*  | run_quantile      | yes  | yes  |   NA     |   */
/*  |-------------------+------+------+----------|   */
/*  NaN - means support for NaN and possibly Inf     */
/*  edge - means calculations are done all the way   */
/*         to the edges                              */
/*  underflow - means at maximum how many numbers    */
/*    are used to store results of addition in case  */
/*    of underflow                                   */
/*===================================================*/

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>
#include <float.h>

/* #define DEBBUG */
#ifdef DEBBUG
int R_finite(double x) {
    return ((x) == (x));
}
#define PRINT(x)                  \
    {                             \
        if ((x) == (x))           \
            printf("%04.1f ", x); \
        else                      \
            printf("NaN ");       \
    }
#else
#include <R.h>
#include <Rinternals.h>
#endif

#define notNaN(x) ((x) == (x))
#define isNaN(x) (!((x) == (x)))
#define MIN(y, x) ((x) < (y) && (x) == (x) ? (x) : (y))
#define MAX(y, x) ((x) > (y) && (x) == (x) ? (x) : (y))
#define SQR(x) ((x) * (x))


/*=======================================================================*/
/* Each iteration of an insertion sort removes an element from the input
 * data, inserting it at the correct position in the already sorted list,
 * until no elements are left in the input. For unsorted data is much less
 * efficient than the more advanced algorithms such as Quicksort, Heapsort,
 * or Merge sort, but it is very efficient on data sets which are already
 * substantially sorted (almost O(n)) */
/* References: */
/*   R. Sedgewick: Algorithms. Addison-Wesley (1988) (page 99) */
/*   http://en.wikipedia.org/wiki/Insertion_sort */
/*   http://www.cs.ubc.ca/spider/harrison/Java/sorting-demo.html */
/* Input: */
/*   V    - data array we operate on remains unchanged (we assume that no
 *          number in idx array is longer than V) */
/*   idx  - index numbers of elements in V to be partially sorted */
/*   nIdx - length of idx array */
/* Output: */
/*   idx  - index numbers of sorted elements in V w */
/*=======================================================================*/
void insertion_sort(const double *V, int *idx, const int nIdx) {
    int i, j, id;
    double v;
    for (i = 1; i < nIdx; i++) {
        id = idx[i];
        v = V[id];
        for (j = i; j > 0; j--) {
            if (V[idx[j - 1]] < v) break;
            idx[j] = idx[j - 1];
        }
        idx[j] = id;
    }
}


/*==================================================================*/
/* minimum function applied to moving (running) window              */
/* Input :                                                          */
/*   In   - array to run moving window over will remain umchanged   */
/*   Out  - empty space for array to store the results. Out is      */
/*          assumed to have reserved memory for nIn*nProbs elements */
/*   nIn  - size of arrays In and Out                               */
/*   nWin - size of the moving window (odd)                         */
/* Output :                                                         */
/*   Out  - results of runing moving window over array In and       */
/*          colecting window mean                                   */
/*==================================================================*/
void run_min(double *In, double *Out, const int *nIn,
	     const int *nWin) {
    /* full-blown version with NaN's and edge calculation */
    int i, j, k2, n = *nIn, m = *nWin;
    double ptOut, Min, *in, *out, CST = DBL_MAX;
    double NaN = (0.0 / 0.0);

    k2 = m >> 1;		/* right half of window size */
    in = In;
    out = Out;
    /* step 1 - find min of elements 0:(k2-1) */
    Min = CST;			/* we need to calculate  initial 'Min' */
    for (i = 0; i < k2; i++)
        Min = MIN(Min, in[i]); /* find minimum over a window of length k2 */
    /* step 2 - left edge; start expanding moving window to the right */
    for (i = k2; i < m - 1; i++) {
        Min = MIN(Min, in[i]);               /* cumulative min */
	/* save 'Min' and move window */
        *(out++) = (Min == CST ? NaN : Min);
    }
    /* step 3 - the inner section; window of constant size is moving */
    ptOut = CST;
    for (i = m - 1; i < n; i++) {
        if (ptOut == Min) {
	    /* if point comining out of the window was window's min then we
	     * need to recalculate 'Min' */
            Min = CST;
	    /* find minimum over a window of length m */
            for (j = 0; j < m; j++)
                Min = MIN(Min, in[j]);
        } else {
	    /* if point comining out of the window was NOT window min than
	     * min of window's first m-1 points is still 'Min', so we have
	     * to add a single point */
            Min = MIN(Min, in[m - 1]);
	}
	/* store point comming out of the window for future use and move
	 * window */
        ptOut = *(in++);
	/* save 'Min' and move window */
        *(out++) = (Min == CST ? NaN : Min);
    }
    /* step 4 - right edge; right side reached the end and left is
     * shrinking */
    for (i = 0; i < k2; i++) {
	/* if point comining out of the window was window's extreme then we
	 * need to recalculate 'Min' */
        if (ptOut == Min) {
            Min = CST;
            for (j = 0; j < m - i - 1; j++) {
		/* find minimum over a window of length m */
                Min = MIN(Min, in[j]);
	    }
        }
	/* store point comming out of the window for future use and move
	 * window */
        ptOut = *(in++);
	/* and fill the space with window extreme and move window */
        *(out++) = (Min == CST ? NaN : Min);
    }
}


/*==================================================================*/
/* Maximum function applied to moving (running) window              */
/* Input :                                                          */
/*   In   - array to run moving window over will remain umchanged   */
/*   Out  - empty space for array to store the results. Out is      */
/*          assumed to have reserved memory for nIn*nProbs elements */
/*   nIn  - size of arrays In and Out                               */
/*   nWin - size of the moving window (odd)                         */
/* Output :                                                         */
/*   Out  - results of runing moving window over array In and       */
/*          colecting window mean                                   */
/*==================================================================*/
void run_max(double *In, double *Out, const int *nIn,
	     const int *nWin) {
    /* full-blown version with NaN's and edge calculation */
    int i, j, k2, n = *nIn, m = *nWin;
    double ptOut, Max, *in, *out, CST = -DBL_MAX;
    double NaN = (0.0 / 0.0);

    k2 = m >> 1; /* right half of window size */
    in = In;
    out = Out;
    /* step 1 - find max of elements 0:(k2-1) */
    Max = CST;			/* we need to calculate  initial 'Max' */
    for (i = 0; i < k2; i++) {
	/* find maximum over a window of length k2 */
        Max = MAX(Max, in[i]);
    }
    /* step 2 - left edge; start expanding the moving window to the
     * right */
    for (i = k2; i < m - 1; i++) {
        Max = MAX(Max, in[i]);               /* cumulative max */
	/* save 'Max' and move window */
        *(out++) = (Max == CST ? NaN : Max);
    }
    /* step 3 - inner section; window of constant size is moving */
    ptOut = CST;
    for (i = m - 1; i < n; i++) {
        if (ptOut == Max) {
	    /* if point comaxing out of the window was window's max then we
	     * need to recalculate 'Max' */
            Max = CST;
            for (j = 0; j < m; j++) {
		/* find maximum over a window of length m */
                Max = MAX(Max, in[j]);
	    }
        } else {
	    /* if point comining out of the window was NOT window max than
	     * max of window's first m-1 points is still 'Max', so we have
	     * to add a single point */
            Max = MAX(Max, in[m - 1]);
	}
	/* store point comming out of the window for future use and move
	 * window */
        ptOut = *(in++);
	/* save 'Max' and move window */
        *(out++) = (Max == CST ? NaN : Max);
    }
    /* step 4 - right edge; right side reached the end and left is
     * shrinking */
    for (i = 0; i < k2; i++) {
        if (ptOut == Max) {
	    /* if point comining out of the window was window's extreme the
	     * we need to recalculate 'Max' */
            Max = CST;
            for (j = 0; j < m - i - 1; j++) {
		/* find maximum over a window of length m */
                Max = MAX(Max, in[j]);
	    }
        }
	/* store point comming out of the window for future use and move
	 * window */
        ptOut = *(in++);
	/* and fill the space with window extreme and move window */
        *(out++) = (Max == CST ? NaN : Max);
    }
}


/*=======================================================================*/

/* Calculate element in the sorted array of nWin elements that corresponds
 * to a quantile of 'type' and 'prob' */
/* Input : */
/*   prob - Quantile probability from 0 to 1 */
/*   nWin - how many elements in dataset the quantile will be calculated
 *          on */
/*   type - integer between 1 and 9 indicating type of quantile See
 *          http://mathworld.wolfram.com/Quantile.html */
/* Output : */
/*   return - which element in the sorted array of nWin elements correspond
 *            to the prob.  If non-integer than split into intger (i) and
 *            real(r) parts, then quantile = v[i]*(1-r) + v[i+1]*r
*/
/*=======================================================================*/
double QuantilePosition(double prob, int nWin, int type) {
/* the following code is based on code from R's quantile.default
 * function */
    double a, b, h, nppm, fuzz;
    int j;
    if (type <= 3) {  // Types 1, 2 and 3 are discontinuous sample qs
        if (type == 3)
            nppm = nWin * prob - .5; // n * probs + m; m = -0.5
        else
            nppm = nWin * prob;	// m = 0
        j = (int)floor(nppm);
        switch (type) {
            case 1:
                h = (nppm > j ? 1 : 0);
                break;  // type 1
            case 2:
                h = (nppm > j ? 1 : 0.5);
                break;  // type 2
            case 3:
                h = ((nppm == j) && ((j >> 1) == 0) ? 0 : 1);
                break;  // type 3
            default:
                h = 1;
                break;
        }
    } else {		      // Types 4 through 9 are continuous sample qs
        switch (type) {
            case 4:
                a = 0;
                b = 1;
                break;
            case 5:
                a = b = 0.5;
                break;
            case 6:
                a = b = 0;
                break;
            case 7:
                a = b = 1;
                break;
            case 8:
                a = b = 1.0 / 3.0;
                break;
            case 9:
                a = b = 3.0 / 8.0;
                break;
            default:
                a = b = 1;
                break;
        }
        nppm = a + prob * (nWin + 1 - a - b); // n*probs + m
        fuzz = 4 * DBL_EPSILON;
        j = (int)floor(nppm + fuzz);
        h = nppm - j;
        h = (fabs(h) < fuzz ? 0 : h);
    }
    nppm = j + h;
    nppm = (nppm < 1 ? 1 : nppm);
    nppm = (nppm > nWin ? nWin : nppm);
    return nppm - 1;  // C arrays are zero based
}


/*==================================================================*/
/* quantile function applied to (running) window                    */
/* Input :                                                          */
/*   In   - array to run moving window over will remain umchanged   */
/*   Out  - empty space for array to store the results. Out is      */
/*          assumed to have reserved memory for nIn*nProbs elements */
/*   nIn  - size of arrays In and Out                               */
/*   nWin - size of the moving window                               */
/*   Prob - Array of probabilities from 0 to 1                      */
/*   nProb - How many elements in Probs array?                      */
/*   type - integer between 1 and 9 indicating type of quantile     */
/*          See http://mathworld.wolfram.com/Quantile.html          */
/* Output :                                                         */
/*   Out  - results of runing moving window over array In and       */
/*          colecting window mean                                   */
/*==================================================================*/
void run_quantile_lite(double *In, double *Out, const int *nIn,
		       const int *nWin, const double *Prob,
		       const int *nProb, const int *Type) {
    /* internal region only is calculated. Edges, NaN's are not handled */
    int i, j, k, *idx, d, n = *nIn, m = *nWin, nPrb = *nProb;
    double *Win, *in, *out, r, ip, *prob, pointOut, ext;
    k = m >> 1;			/* half of window size */
    in = In;
    out = Out + k;

    if (nPrb == 1 && (*Prob == 1 || *Prob == 0)) {
	/* trivial case shortcut - if prob is 0 or 1 than wind windows min
	 * or max */
        d = (*Prob == 0 ? -1 : 1); /* run_min d=-1; run_max d=1*/
        pointOut = ext = 0;
        for (i = m - 1; i < n; i++) {
            if (pointOut == ext) {
		/* if point comining out of the window was window's extreme
		 * than in the 2 lines below we do things different when
		 * extreme is a max vs. min */
                ext = in[0];
                if (d == 1) {
                    for (j = 1; j < m; j++) {
                        if (ext < in[j]) {
			    /* find maximum over a window of length m */
                            ext = in[j];
			}
		    }
                } else {
                    for (j = 1; j < m; j++) {
                        if (ext > in[j]) {
			    /* find minimum over a window of length m */
                            ext = in[j];
			}
		    }
                }
            } else {
		/* if point comining out of the window was NOT window
		 * extreme then we know extreme of window's first m-1
		 * points, so we have to add a single point */
                if (ext * d < in[m - 1] * d)
		    ext = in[m - 1];
	    }
	    /* store point coming out of the window for future use and move
	     * window */
            pointOut = *(in++);
	    /* and fill the space with window extreme and move window */
            *(out++) = ext;
        }
    } else {                  /* non-trivial case */
	/* index will hold partially sorted index numbers of Save array */
        idx = R_Calloc(m, int);
	/* stores all points of the current running window */
        Win = R_Calloc(m, double);
        prob = R_Calloc(nPrb, double);
        for (i = 0; i < m; i++) {
            Win[i] = *(in++);	/* initialize running window */
            idx[i] = i;		/* and its index */
        }
        in--;		/* last point of the window will be placed again */
        for (d = 0; d < nPrb; d++) { /* for each probability */
	    /* store common size for speed */
            prob[d] = QuantilePosition(Prob[d], m, *Type);
	}
        for (j = i = m - 1; i < n; i++) {
	    /* Move Win to the right: replace a[i-m] with a[m] point */
            Win[j] = *(in++);
            insertion_sort(Win, idx, m); /* sort current Win */
            for (d = 0; d < nPrb; d++) {
		/* Divide p into its fractional and integer parts */
                r = modf(prob[d], &ip);
		/* k-1 instead of k because in C arrays are 0 based and in
		 * R they are 1 based */
                k = (int)ip - 1;
                if (r)		/* interpolate */
                    r = Win[idx[k]] * (1 - r) + Win[idx[k + 1]] * r;
                else
                    r = Win[idx[k]];
                *(out + d * n) = r;
            }
            out++;
            j = (j + 1) %
                m; /* index goes from 0 to m-1, and back to 0 again  */
        }
        R_Free(Win);
        R_Free(idx);
        R_Free(prob);
    }
}


void run_quantile(double *In, double *Out, const int *nIn, const int *nWin,
		  const double *Prob, const int *nProb, const int *Type) {
    /* full-blown version with NaN's and edge calculation */
    int i, j, k1, k2, *idx, d, n = *nIn, m = *nWin, nPrb = *nProb, mm, k,
	type = *Type, count = 0;
    double *Win, *in, *out, r, ip, Max, p, *prob, BIG = DBL_MAX;
    double NaN = (0.0 / 0.0);

    k2 = m >> 1;		/* right half of window size */
    k1 = m - k2 - 1;		/* left half of window size */
    in = In;
    out = Out;

    if (nPrb == 1 && *Prob == 0) {
	/* trivial case shortcut - if prob is 0 or 1 than find windows
	 * min */
        run_min(In, Out, nIn, nWin);
    } else if (nPrb == 1 && *Prob == 1) {
	/* trivial case shortcut - if prob is 0 or 1 than find windows
	 * max */
        run_max(In, Out, nIn, nWin);
    } else {                  /* non-trivial case */
	/* index will hold partially sorted index numbers of Save array */
        idx = R_Calloc(m, int);
	/* store all points of the current running window */
        Win = R_Calloc(m, double);
        prob = R_Calloc(nPrb, double);
        for (i = 0; i < m; i++) idx[i] = i; /* and its index */
        for (i = 0; i < k2; i++) {
            Win[i] = *(in++);	/* initialize running window */
            if (isNaN(Win[i]))
                Win[i] = BIG;
            else
                count++;
        }
        /* step 1: left edge */
        for (j = k2, i = 0; i <= k1; i++) {
            mm = i + k2 + 1;	/* window width */
            j = mm - 1;
	    /* Move Win to the right: replace a[i-m] with a[m] point  */
            Win[j] = *(in++);
            if (isNaN(Win[j]))
                Win[j] = BIG;
            else
                count++;
            insertion_sort(Win, idx, mm); /* sort current Win */
            for (d = 0; d < nPrb; d++) {  /* for each probability */
                if (count > 0) { /* not all points in the window are NaN*/
                    p = QuantilePosition(Prob[d], count, type);
		    /* Divide p into its fractional and integer parts */
                    r = modf(p, &ip);
		    /* k-1 instead of k because in C arrays are 0 based and
		     * in R they are 1 based */
                    k = (int)ip;
                    if (r)	/* interpolate */
                        r = Win[idx[k]] * (1 - r) + Win[idx[k + 1]] * r;
                    else
                        r = Win[idx[k]];
                } else {
		    /* all points in the window are NaN*/
                    r = NaN;
		}
                out[d * n] = r;
            }
            out++;
	    /* index goes from 0 to m-1, and back to 0 again */
            j = (j + 1) % m;
        }
        /* step 2: inner section */
        for (d = 0; d < nPrb; d++) { /* for each probability */
	    /* store common size for speed */
            prob[d] = QuantilePosition(Prob[d], m, type);
	}
        for (i = m; i < n; i++) {
            if (Win[j] < BIG)
                count--;      /* Point leaving the window was not a NAN */
	    /* Move Win to the right: replace a[i-m] with a[m] point  */
            Win[j] = *(in++);
            if (isNaN(Win[j]))
                Win[j] = BIG;
            else
                count++;
            insertion_sort(Win, idx, m); /* sort current Win */
            for (d = 0; d < nPrb; d++) { /* for each probability */
                if (count > 0) { /* not all points in the window are NaN*/
                    p = (count == m ? prob[d] :
			 QuantilePosition(Prob[d], count, type));
		    /* Divide p into its fractional and integer parts */
                    r = modf(p, &ip);
		    /* k-1 instead of k because in C arrays are 0 based and
		     * in R they are 1 based */
                    k = (int)ip;
                    if (r)	/* interpolate */
                        r = Win[idx[k]] * (1 - r) + Win[idx[k + 1]] * r;
                    else
                        r = Win[idx[k]];
                } else {
		    /* all points in the window are NaN*/
                    r = NaN;
		}
                out[d * n] = r;
            }
            out++;
	    /* index goes from 0 to m-1, and back to 0 again  */
            j = (j + 1) % m;
        }
        /* step 3: right edge */
        Max = Win[idx[m - 1]];	/* store window maximum */
        for (i = 0; i < k2; i++) {
            if (Win[j] < BIG)
                count--;    /* Point leaving the window was not a NAN */
            Win[j] = Max;   /* setting to maximum will push it to the end*/
            mm = m - i - 1; /* window width */
            insertion_sort(Win, idx, mm); /* sort current Win from 1-mm */
            for (d = 0; d < nPrb; d++) {  /* for each probability */
                if (count > 0) { /* not all points in the window are NaN*/
                    p = QuantilePosition(Prob[d], count, type);
		    /* Divide p into its fractional and integer parts */
                    r = modf(p, &ip);
		    /* k-1 instead of k because in C arrays are 0 based and
		     * in R they are 1 based */
                    k = (int)ip;
                    if (r)	/* interpolate */
                        r = Win[idx[k]] * (1 - r) + Win[idx[k + 1]] * r;
                    else
                        r = Win[idx[k]];
                } else
                    r = NaN;	/* all points in the window are NaN*/
                out[d * n] = r;
            }
            out++;
	    /* index goes from 0 to m-1, and back to 0 again */
            j = (j + 1) % m;
        }
        R_Free(Win);
        R_Free(idx);
        R_Free(prob);
    }
}


#undef MIN
#undef MAX
#undef SQR

#ifdef DEBBUG
int main(void) {
    double NaN = (0.0 / 0.0);
    char s[] = "%02i ";  //"%02i ";

    int i, nn = 25, k = 13, np = 3, type = 7;
    double xx[25], yy[3 * 25], y1[25], y2[25], y3[25], y4[25], y5[25], y6[25];
    double p[] = {0, 0.5, 1};
    for (i = 0; i < nn; i++) xx[i] = i;
    for (i = 5; i < 12; i++) xx[i] = i;
    // run_min(xx, y1, &nn, &k);
    // run_max(xx, y2, &nn, &k);
    run_quantile(xx, yy, &nn, &k, p, &np, &type);
    for (i = 0; i < nn; i++) PRINT(xx[i]);
    printf("Original\n");

    getchar();
    return 0;
}
#endif
