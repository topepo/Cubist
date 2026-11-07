#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <setjmp.h>
#include <string.h>

#include "rdata.h"
#include "redefine.h"
#include "rulebasedmodels.h"
#include "strbuf.h"

extern void cubistmain(void);
extern void samplemain(double *outputv);
extern void FreeCases(void);

static void cubist(char **namesv, char **datav, int *unbiased,
                   char **compositev, int *neighbors, int *committees,
                   double *sample, int *seed, int *rules, double *extrapolation,
                   char **modelv, char **outputv) {
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */

  // Announce ourselves for testing
  // Rprintf("cubist called\n");

  // Initialize the globals to the values that the cubist
  // program would have at the start of execution
  initglobals();

  // Set globals based on the arguments.  This is analogous
  // to parsing the command line in the cubist program.
  setglobals(*unbiased, *compositev, *neighbors, *committees, *sample, *seed,
             *rules, *extrapolation);

  // Handles the strbufv data structure
  rbm_removeall();

  // Deallocates memory allocated by NewCase.
  // Not necessary since it's also called at the end of this function,
  // but it doesn't hurt, and I'm feeling paranoid.
  FreeCases();

  // XXX Should this be controlled via an option?
  // Rprintf("Calling setOf\n");
  setOf();

  // Create a strbuf using *namesv as the buffer.
  // Note that this is a readonly strbuf since we can't
  // extend *namesv.
  STRBUF *sb_names = strbuf_create_full(*namesv, strlen(*namesv));

  // Register this strbuf using the name "undefined.names"
  rbm_register(sb_names, "undefined.names", 1);

  // Create a strbuf using *datav and register it as "undefined.data"
  STRBUF *sb_datav = strbuf_create_full(*datav, strlen(*datav));
  // XXX why is sb_datav copied? what that part of my debugging?
  rbm_register(strbuf_copy(sb_datav), "undefined.data", 1);

  /*
   * We need to initialize rbm_buf before calling any code that
   * might call exit/rbm_exit.
   */
  if ((val = setjmp(rbm_buf)) == 0) {
    // Real work is done here
    // Rprintf("Calling cubistmain\n");
    cubistmain();

    // Rprintf("cubistmain finished\n");

    // Get the contents of the the model file
    char *modelString = strbuf_getall(rbm_lookup("undefined.model"));
    char *model = R_alloc(strlen(modelString) + 1, 1);
    strcpy(model, modelString);

    // I think the previous value of *modelv will be garbage collected
    *modelv = model;
  } else {
    Rprintf("cubist code called exit with value %d\n", val - JMP_OFFSET);
  }

  // Close file object "Of", and return its contents via argument outputv
  char *outputString = closeOf();
  char *output = R_alloc(strlen(outputString) + 1, 1);
  strcpy(output, outputString);
  *outputv = output;

  // Deallocates memory allocated by NewCase
  FreeCases();

  // We reinitialize the globals on exit out of general paranoia
  initglobals();
}

static void predictions(char **casev, char **namesv, char **datav,
                        char **modelv, double *predv, char **outputv) {
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */

  // Announce ourselves for testing
  // Rprintf("predictions called\n");

  // Initialize the globals
  initglobals();

  // Handles the strbufv data structure
  rbm_removeall();

  // XXX Should this be controlled via an option?
  // Rprintf("Calling setOf\n");
  setOf();

  STRBUF *sb_cases = strbuf_create_full(*casev, strlen(*casev));
  rbm_register(sb_cases, "undefined.cases", 1);

  STRBUF *sb_names = strbuf_create_full(*namesv, strlen(*namesv));
  rbm_register(sb_names, "undefined.names", 1);

  STRBUF *sb_datav = strbuf_create_full(*datav, strlen(*datav));
  /* XXX why is sb_datav copied? */
  rbm_register(strbuf_copy(sb_datav), "undefined.data", 1);

  STRBUF *sb_modelv = strbuf_create_full(*modelv, strlen(*modelv));
  /* XXX should sb_modelv be copied? */
  rbm_register(sb_modelv, "undefined.model", 1);

  /*
   * We need to initialize rbm_buf before calling any code that
   * might call exit/rbm_exit.
   */
  if ((val = setjmp(rbm_buf)) == 0) {
    // Real work is done here
    // Rprintf("Calling samplemain\n");
    samplemain(predv);

    // Rprintf("samplemain finished\n");
  } else {
    // Rprintf("sample code called exit with value %d\n", val - JMP_OFFSET);
  }

  // Close file object "Of", and return its contents via argument outputv
  char *outputString = closeOf();
  char *output = R_alloc(strlen(outputString) + 1, 1);
  strcpy(output, outputString);
  *outputv = output;

  // We reinitialize the globals on exit out of general paranoia
  initglobals();
}

/*
 * .Call interface: data are passed as R vectors and loaded by rdata.c,
 * with no .names/.data text in between.  All argument checking happens
 * here, before any Cubist globals are touched, so that R errors cannot
 * leave the C code half initialized.  The R side (R/data-spec.R) builds
 * these arguments; the checks below only guard against misuse.
 */

static SEXP list_elt(SEXP list, const char *name) {
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  for (R_xlen_t i = 0; i < Rf_xlength(list); i++) {
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
      return VECTOR_ELT(list, i);
    }
  }
  Rf_error("attribute spec has no element '%s'", name);
  return R_NilValue; /* not reached */
}

static const char *string_copy(SEXP s) {
  const char *src = Rf_translateChar(s);
  char *dst = R_alloc(strlen(src) + 1, 1);
  strcpy(dst, src);
  return dst;
}

static void unpack_spec(SEXP spec, RAttSpec *out) {
  SEXP names = list_elt(spec, "names");
  SEXP discrete = list_elt(spec, "discrete");
  SEXP ordered = list_elt(spec, "ordered");
  SEXP levels = list_elt(spec, "levels");
  SEXP target = list_elt(spec, "target");
  SEXP has_weights = list_elt(spec, "has_weights");

  int p = Rf_length(names);
  if (TYPEOF(names) != STRSXP || TYPEOF(discrete) != LGLSXP ||
      TYPEOF(ordered) != LGLSXP || TYPEOF(levels) != VECSXP ||
      Rf_length(discrete) != p || Rf_length(ordered) != p ||
      Rf_length(levels) != p || TYPEOF(target) != STRSXP ||
      Rf_length(target) != 1 || TYPEOF(has_weights) != LGLSXP ||
      Rf_length(has_weights) != 1) {
    Rf_error("malformed attribute spec");
  }

  const char **att_names = (const char **)R_alloc(p, sizeof(char *));
  int *nlevels = (int *)R_alloc(p, sizeof(int));
  const char ***lvls = (const char ***)R_alloc(p, sizeof(char **));

  for (int j = 0; j < p; j++) {
    att_names[j] = string_copy(STRING_ELT(names, j));
    nlevels[j] = 0;
    lvls[j] = NULL;
    if (LOGICAL(discrete)[j]) {
      SEXP lv = VECTOR_ELT(levels, j);
      if (TYPEOF(lv) != STRSXP) {
        Rf_error("levels for '%s' are not character",
                 att_names[j]);
      }
      nlevels[j] = Rf_length(lv);
      lvls[j] = (const char **)R_alloc(nlevels[j] + 1, sizeof(char *));
      for (int k = 0; k < nlevels[j]; k++) {
        lvls[j][k] = string_copy(STRING_ELT(lv, k));
      }
    }
  }

  out->NPred = p;
  out->Names = att_names;
  out->Discrete = LOGICAL(discrete);
  out->Ordered = LOGICAL(ordered);
  out->NLevels = nlevels;
  out->Levels = lvls;
  out->Target = string_copy(STRING_ELT(target, 0));
  out->HasWeights = LOGICAL(has_weights)[0];
}

static void unpack_cases(SEXP x, SEXP y, SEXP w, const RAttSpec *spec,
                         RCaseData *out) {
  if (TYPEOF(x) != VECSXP || Rf_length(x) != spec->NPred) {
    Rf_error("expected a list of %d columns", spec->NPred);
  }

  int n = -1;
  if (y != R_NilValue) {
    if (TYPEOF(y) != REALSXP) {
      Rf_error("the outcome must be a double vector");
    }
    n = Rf_length(y);
  } else if (spec->NPred > 0) {
    n = Rf_length(VECTOR_ELT(x, 0));
  }
  if (n < 0) {
    Rf_error("cannot determine the number of cases");
  }

  const void **cols = (const void **)R_alloc(spec->NPred + 1, sizeof(void *));
  for (int j = 0; j < spec->NPred; j++) {
    SEXP col = VECTOR_ELT(x, j);
    if (Rf_length(col) != n) {
      Rf_error("column '%s' has %d values, expected %d",
               spec->Names[j], Rf_length(col), n);
    }
    if (spec->Discrete[j]) {
      if (TYPEOF(col) != INTSXP) {
        Rf_error("column '%s' must be integer codes",
                 spec->Names[j]);
      }
      const int *codes = INTEGER(col);
      for (int i = 0; i < n; i++) {
        if (codes[i] != NA_INTEGER &&
            (codes[i] < 1 || codes[i] > spec->NLevels[j])) {
          Rf_error("column '%s' has an invalid code %d",
                   spec->Names[j], codes[i]);
        }
      }
      cols[j] = codes;
    } else {
      if (TYPEOF(col) != REALSXP) {
        Rf_error("column '%s' must be double",
                 spec->Names[j]);
      }
      cols[j] = REAL(col);
    }
  }

  const double *wts = NULL;
  if (w != R_NilValue) {
    if (TYPEOF(w) != REALSXP || Rf_length(w) != n) {
      Rf_error("case weights must be %d doubles", n);
    }
    wts = REAL(w);
  }

  out->N = n;
  out->Cols = cols;
  out->Y = (y == R_NilValue) ? NULL : REAL(y);
  out->W = wts;
}

static SEXP make_string(const char *s) {
  return Rf_ScalarString(Rf_mkChar(s));
}

SEXP C_cubist_fit(SEXP spec, SEXP x, SEXP y, SEXP w, SEXP unbiased,
                  SEXP committees, SEXP sample, SEXP seed, SEXP rules,
                  SEXP extrapolation) {
  RAttSpec att_spec;
  RCaseData train;
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */
  const char *model_string = "";

  if (RDATA_NA_INT != NA_INTEGER) {
    Rf_error("NA_INTEGER does not match RDATA_NA_INT");
  }

  unpack_spec(spec, &att_spec);
  if (y == R_NilValue) {
    Rf_error("the outcome is required to fit a model");
  }
  unpack_cases(x, y, w, &att_spec, &train);

  int c_unbiased = Rf_asLogical(unbiased);
  int c_committees = Rf_asInteger(committees);
  double c_sample = Rf_asReal(sample);
  int c_seed = Rf_asInteger(seed);
  int c_rules = Rf_asInteger(rules);
  double c_extrapolation = Rf_asReal(extrapolation);

  initglobals();

  // "yes" and 1 are the composite and neighbors values that the .C
  // interface has always passed from R; neighbors are chosen at
  // prediction time instead.
  setglobals(c_unbiased, "yes", 1, c_committees, c_sample, c_seed, c_rules,
             c_extrapolation);

  rbm_removeall();
  FreeCases();
  setOf();

  if ((val = setjmp(rbm_buf)) == 0) {
    RCubist(&att_spec, &train);

    // Copy the model text before the strbufs are removed
    const char *mstr = strbuf_getall(rbm_lookup("undefined.model"));
    char *copy = R_alloc(strlen(mstr) + 1, 1);
    strcpy(copy, mstr);
    model_string = copy;
  }
  // When the C code stops (rbm_exit), the reason is in the output; R reports
  // it as an error using the status below
  int status = 0;
  if (val != 0) {
    status = val - JMP_OFFSET;
  }

  const char *ostr = closeOf();
  char *output_string = R_alloc(strlen(ostr) + 1, 1);
  strcpy(output_string, ostr);

  FreeCases();
  rbm_removeall();
  initglobals();

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_VECTOR_ELT(out, 0, make_string(model_string));
  SET_VECTOR_ELT(out, 1, make_string(output_string));
  SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(status));
  SET_STRING_ELT(out_names, 0, Rf_mkChar("model"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("output"));
  SET_STRING_ELT(out_names, 2, Rf_mkChar("status"));
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(2);
  return out;
}

SEXP C_cubist_predict(SEXP spec, SEXP model, SEXP train_x, SEXP train_y,
                      SEXP train_w, SEXP new_x) {
  RAttSpec att_spec;
  RCaseData train, cases;
  int val; /* Used by setjmp/longjmp for implementing rbm_exit */

  if (TYPEOF(model) != STRSXP || Rf_length(model) != 1) {
    Rf_error("the model must be a single string");
  }

  unpack_spec(spec, &att_spec);
  unpack_cases(train_x, train_y, train_w, &att_spec, &train);
  unpack_cases(new_x, R_NilValue, R_NilValue, &att_spec, &cases);

  // GetCommittee() reads the model through a strbuf; give it a private
  // copy since strbuf_create_full() does not copy its buffer.
  const char *mstr = Rf_translateChar(STRING_ELT(model, 0));
  char *model_copy = R_alloc(strlen(mstr) + 1, 1);
  strcpy(model_copy, mstr);

  SEXP pred = PROTECT(Rf_allocVector(REALSXP, cases.N));
  for (int i = 0; i < cases.N; i++) {
    REAL(pred)[i] = NA_REAL;
  }

  initglobals();
  rbm_removeall();
  setOf();

  STRBUF *sb_model = strbuf_create_full(model_copy, strlen(model_copy));
  rbm_register(sb_model, "undefined.model", 1);

  if ((val = setjmp(rbm_buf)) == 0) {
    RPredict(&att_spec, &train, &cases, REAL(pred));
  }
  // See C_cubist_fit()
  int status = 0;
  if (val != 0) {
    status = val - JMP_OFFSET;
  }

  const char *ostr = closeOf();
  char *output_string = R_alloc(strlen(ostr) + 1, 1);
  strcpy(output_string, ostr);

  rbm_removeall();
  initglobals();

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SEXP out_names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_VECTOR_ELT(out, 0, pred);
  SET_VECTOR_ELT(out, 1, make_string(output_string));
  SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(status));
  SET_STRING_ELT(out_names, 0, Rf_mkChar("pred"));
  SET_STRING_ELT(out_names, 1, Rf_mkChar("output"));
  SET_STRING_ELT(out_names, 2, Rf_mkChar("status"));
  Rf_setAttrib(out, R_NamesSymbol, out_names);
  UNPROTECT(3);
  return out;
}

static const R_CallMethodDef callEntries[] = {
    {"C_cubist_fit", (DL_FUNC)&C_cubist_fit, 10},
    {"C_cubist_predict", (DL_FUNC)&C_cubist_predict, 6},
    {NULL, NULL, 0}};

// Declare the type of each of the arguments to the cubist function
static R_NativePrimitiveArgType cubist_t[] = {
    STRSXP,  // namesv
    STRSXP,  // datav
    LGLSXP,  // unbiased
    STRSXP,  // compositev
    INTSXP,  // neighbors
    INTSXP,  // committees
    REALSXP, // sample
    INTSXP,  // seed
    INTSXP,  // rules
    REALSXP, // extrapolation
    STRSXP,  // modelv
    STRSXP   // outputv
};

// Declare the type of each of the arguments to the cubist function
static R_NativePrimitiveArgType predictions_t[] = {
    STRSXP,  // casev
    STRSXP,  // namesv
    STRSXP,  // datav
    STRSXP,  // modelv
    REALSXP, // predv
    STRSXP   // outputv
};

// Declare the cubist function
static const R_CMethodDef cEntries[] = {
    {"cubist", (DL_FUNC)&cubist, 12, cubist_t},
    {"predictions", (DL_FUNC)&predictions, 6, predictions_t},
    {NULL, NULL, 0}};

// Initialization function for this shared object
void R_init_Cubist(DllInfo *dll) {
  // Announce ourselves for testing
  // Rprintf("R_init_Cubist called\n");

  // Register the legacy .C functions "cubist" and "predictions" (used for
  // models fitted before data were passed directly) and the .Call
  // functions "C_cubist_fit" and "C_cubist_predict"
  R_registerRoutines(dll, cEntries, callEntries, NULL, NULL);

  // This should help prevent people from accidentally accessing
  // any of our global variables, or any functions that are not
  // intended to be called from R.  Only the functions "cubist"
  // and predictions  can be accessed, since they're the only ones
  // we registered.
  R_useDynamicSymbols(dll, FALSE);
}
