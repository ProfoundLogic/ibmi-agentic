     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SMC04R - MC Medical Cases Search
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Allows the user to select a Medical Case.  If one is selected, it     *
     *  the DARTS key for that Case, otherwise it returns an error code.      *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     * CALLS  TO : SC916C1 - Program to create a copy of search program into  *
     *                       QTEMP.                                           *
     *             F0269R - Work With Medical Cases (select mode)             *
     *                                                                        *
     * CREATED   : 01/31/2000                                                 *
     * PROGRAMMER: C:113                                                      *
     * PROJECT   : DR095                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
      ** System User Parameter File
     FSMUPL     IF   E           K DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('F0269R')
     D  call                          1a
     D  sub                           5  0
     D  group                         3  0
     D  pol_year                      3  0
     D  case                          7  0
     D  ss#                           9a
     D  accident_date                 8  0
     D  return_code                   1a

     D McCvtApi        PR                   ExtPgm('FA703R')
     D  rtncode                       1a
     D  wcin                          1a
     D  wdck                          1a
     D  company                       3a
     D  subscriber                    5a
     D  group                         3a
     D  policyYear                    3a
     D  case                          7a
     D  key                          40a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pSub#           S              5  0
     D pGroup          S              3  0
     D pPolYear        S              3  0
     D pCase           S              7  0
     D pSS#            S              9a
     D pAccDate        S              8  0
     D pRtnCode        S              1a

     D pProgram        S             10a   Inz('F0269R')

     D wcin            S              1a
     D wdck            S              1a
     D company         S              3a
     D subscriber      S              5a
     D group           S              3a
     D policyYear      S              3a
     D case            S              7a
     D rtnCode         S              1a
     D tempKey         S             40a

     *========================================================================*
     * Data Structure & Array Definitions
     *========================================================================*
      ** Program information structure
     D psds          esds                  extname(szq1p)

     *========================================================================*
     * PLIST Definitions
     *========================================================================*
     C     *entry        PList
     C                   Parm                    eKey
     C                   Parm                    eRtnCode

     *========================================================================*
     * Main
     *========================================================================*
      ** Create a copy of the search program in QTEMP to avoid recursive calls.
     C                   Eval      pCall = '1'
     C                   CALLP     LIBLpgm(pProgram : pCall)

     ** Call Work With Medical Cases for a search.
     C                   Eval      pCall = '2'
     C                   CALLP     SearchPgm(pCall : pSub# : pGroup :
     C                                       pPolYear : pCase : pSS# :
     C                                       pAccDate : pRtnCode)

      ** If we returned with anything, set up the key, else return code.
     C                   If        pRtnCode = '*'
     C     q1user        Chain     PUP01                              60
     C                   Move      UPCO#         company
     C                   Move      pSub#         subscriber
     C                   Move      pGroup        group
     C                   Move      pPolYear      policyYear
     C                   Move      pCase         case
     C                   Eval      wcin = 'Y'
     C                   Eval      wdck = 'Y'
     C                   Eval      rtnCode = 'K'

     C                   CALLP     McCvtApi(rtnCode:
     C                                      wcin:
     C                                      wdck:
     C                                      company:
     C                                      subscriber:
     C                                      group:
     C                                      policyYear:
     C                                      case:
     C                                      tempKey)
     C                   Clear                   eRtnCode
     C                   MoveL     tempKey       eKey

     C                   Else
     C                   Clear                   eKey
     C                   Eval      eRtnCode = '3'
     C                   EndIf

     C                   Eval      *inLR = *on
