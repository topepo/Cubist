/*************************************************************************/
/*                                                                       */
/*  Load attributes and cases directly from R vectors.                   */
/*                                                                       */
/*  The original program reads a .names and a .data file.  RDefineAtts  */
/*  replaces GetNames() and RLoadCases replaces GetData() so that no    */
/*  text has to be formatted in R and parsed again here.  Everything    */
/*  after loading (model building, prediction) is shared with the file  */
/*  based code through CubistBuild(), StartData()/KeepCase()/          */
/*  FinishData(), and PredictLoaded().                                  */
/*                                                                       */
/*  Attribute layout matches what makeNamesFile() writes, so that both  */
/*  paths build the same model:                                         */
/*                                                                       */
/*    1            the outcome (ClassAtt)                               */
/*    2..NPred+1   the predictors, in column order                      */
/*    NPred+2      "case weight" (CWtAtt), only if weights are given    */
/*                                                                       */
/*************************************************************************/

#include "defns.h"
#include "extern.h"

#include "rdata.h"
#include "redefine.h"
#include "transform.h"

/*************************************************************************/
/*                                                                       */
/* Set up the attribute globals that GetNames() would set.   */
/*                                                                       */
/* Names and levels are copied as given: there is no escaping,  */
/* whitespace collapsing, length limit, or special meaning for  */
/* values such as "?" or "N/A".      */
/*                                                                       */
/*************************************************************************/

static void RDefineAtts(const RAttSpec *Spec)
/*          -----------  */
{
  Attribute Att, AttCeiling;
  int j, k, NLev;

  ErrMsgs = AttExIn = LineNo = 0;
  LabelAtt = CWtAtt = 0;

  MaxAtt = 1 + Spec->NPred;
  if (Spec->HasWeights) {
    MaxAtt++;
  }
  AttCeiling = MaxAtt + 1;

  AttName = AllocZero(AttCeiling, String);
  MaxAttVal = AllocZero(AttCeiling, DiscrValue);
  AttValName = AllocZero(AttCeiling, String *);
  SpecialStatus = AllocZero(AttCeiling, char);
  AttDef = AllocZero(AttCeiling, Definition);
  AttDefUses = AllocZero(AttCeiling, Attribute *);

  /*  Outcome  */

  ClassAtt = 1;
  AttName[ClassAtt] = strdup(Spec->Target);

  /*  Predictors.  Discrete values are numbered as in ExplicitAtt():
      0 = unknown, 1 = "N/A", 2.. = the levels in order  */

  ForEach(j, 0, Spec->NPred - 1) {
    Att = j + 2;
    AttName[Att] = strdup(Spec->Names[j]);

    if (!Spec->Discrete[j]) {
      continue;
    }

    NLev = Spec->NLevels[j];
    AttValName[Att] = AllocZero(NLev + 3, String);
    AttValName[Att][1] = strdup("N/A");
    ForEach(k, 0, NLev - 1) {
      AttValName[Att][k + 2] = strdup(Spec->Levels[j][k]);
    }
    MaxAttVal[Att] = NLev + 1;

    /*  As in ExplicitAtt(), ordering needs at least three values  */

    if (Spec->Ordered[j] && MaxAttVal[Att] > 3) {
      SpecialStatus[Att] = ORDERED;
    }

    if (MaxAttVal[Att] > MaxDiscrVal) {
      MaxDiscrVal = MaxAttVal[Att];
    }
  }

  /*  Case weights are never used as a predictor  */

  if (Spec->HasWeights) {
    CWtAtt = MaxAtt;
    AttName[CWtAtt] = strdup("case weight");
    SpecialStatus[CWtAtt] |= SKIP;
  }
}

/*************************************************************************/
/*                                                                       */
/* Store a continuous value the way GetDataRec() does: NA and NaN  */
/* are unknown, and non-finite values are reported by CheckValue() */
/*                                                                       */
/*************************************************************************/

static void SetContinuous(DataRec DVec, Attribute Att, double Value)
/*          -------------  */
{
  if (isnan(Value)) {
    CVal(DVec, Att) = UNKNOWN;
    return;
  }

  CVal(DVec, Att) = Value;
  CheckValue(DVec, Att);
}

/*************************************************************************/
/*                                                                       */
/* Load cases into Case[0..MaxCase], as GetData() does for a file  */
/*                                                                       */
/*************************************************************************/

static void RLoadCases(const RAttSpec *Spec, const RCaseData *Data,
                       Boolean Train, Boolean AllowUnknownTarget)
/*          ----------  */
{
  CaseNo i;
  int j, Code;
  Attribute Att;
  DataRec DVec;

  StartData(Train, Data->N);

  ForEach(i, 0, Data->N - 1) {
    Case[MaxCase] = DVec = NewCase(); /* for error reporting */

    if (Data->Y) {
      SetContinuous(DVec, ClassAtt, Data->Y[i]);
    } else {
      CVal(DVec, ClassAtt) = UNKNOWN;
    }

    ForEach(j, 0, Spec->NPred - 1) {
      Att = j + 2;

      if (Spec->Discrete[j]) {
        Code = ((const int *)Data->Cols[j])[i];
        if (Code == RDATA_NA_INT) {
          DVal(DVec, Att) = 0;
        } else {
          DVal(DVec, Att) = Code + 1;
        }
      } else {
        SetContinuous(DVec, Att, ((const double *)Data->Cols[j])[i]);
      }
    }

    if (CWtAtt) {
      if (Data->W) {
        SetContinuous(DVec, CWtAtt, Data->W[i]);
      } else {
        CVal(DVec, CWtAtt) = UNKNOWN;
      }
    }

    Class(DVec) = CVal(DVec, ClassAtt);

    KeepCase(DVec, Train, AllowUnknownTarget);
  }

  FinishData(Train);
}

/*************************************************************************/
/*                                                                       */
/* Hold-out cases for sampled fits (see LoadTestData in construct.c) */
/*                                                                       */
/*************************************************************************/

static const RAttSpec *TestSpec = Nil;
static const RCaseData *TestData = Nil;

static void RLoadTestCases(void)
/*          --------------  */
{
  RLoadCases(TestSpec, TestData, binfalse, binfalse);
}

/*************************************************************************/
/*                                                                       */
/* Build a model; the counterpart of cubistmain()    */
/*                                                                       */
/*************************************************************************/

void RCubist(const RAttSpec *Spec, const RCaseData *Train)
/*   -------  */
{
  double StartTime;

  StartTime = CubistStart();
  PrintHeader("");

  RDefineAtts(Spec);
  fprintf(Of, T_TargetAtt, AttName[ClassAtt]);

  NotifyStage(READDATA);
  Progress(-1.0);

  RLoadCases(Spec, Train, bintrue, binfalse);
  fprintf(Of, TX_ReadData(MaxCase + 1, MaxAtt, FileStem));

  /*  A sampled fit evaluates on the cases that were not sampled,
      which are read again from the same data  */

  TestSpec = Spec;
  TestData = Train;
  LoadTestData = RLoadTestCases;

  CubistBuild(StartTime);

  LoadTestData = Nil;
  TestSpec = Nil;
  TestData = Nil;
}

/*************************************************************************/
/*                                                                       */
/* Predict new cases from the registered model text; the   */
/* counterpart of samplemain()      */
/*                                                                       */
/*************************************************************************/

void RPredict(const RAttSpec *Spec, const RCaseData *Train,
              const RCaseData *New, double *Pred)
/*   --------  */
{
  RRuleSet *CubistModel;

  RDefineAtts(Spec);

  /*  NotifyStage(READDATA) initializes Uf, which InitialiseInstances
      needs  */

  NotifyStage(READDATA);
  Progress(-1.0);

  CubistModel = GetCommittee(".model");

  if (USEINSTANCES) {
    RLoadCases(Spec, Train, bintrue, binfalse);

    InitialiseInstances(CubistModel);
    CopyInstances();

    FreeData(Case);
    Case = Nil;
  }

  /*  Not training, but allow unknown target  */

  RLoadCases(Spec, New, binfalse, bintrue);

  PredictLoaded(CubistModel, Pred);
}
