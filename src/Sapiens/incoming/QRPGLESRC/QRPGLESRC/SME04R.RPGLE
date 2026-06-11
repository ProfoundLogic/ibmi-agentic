     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
    H/TITLE SME04R - MC Employer Name Search - Copy/Transfer
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Calls a program to allow the user to select an Employer.  If one is   *
     *  selected, the DARTS key is returned for that Employer.                *
     *                                                                        *
     * CALLED BY : VDARTS                                                     *
     * CALLS  TO : SC916C1 - Program to create a copy of search program into  *
     *                       QTEMP.                                           *
     *             F0140R - Work With Employers (select mode)                 *
     *                                                                        *
     * CREATED   : 02/07/2000                                                 *
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
      ** Managed Care Employer Master File
     FFMEMP     IF   E             DISK

     *========================================================================*
     * Prototypes
     *========================================================================*
     D LIBLpgm         PR                  ExtPgm('SC916C1')
     D  program                      10a
     D  call                          1a

     D SearchPgm       PR                  ExtPgm('F0140R')
     D  call                          1a
     D  kc                            1a
     D  kl                            1a
     D  rrn                           9  0
     D  rrn2                          9  0
     D  view                          1  0

     D McCvtApi        PR                   ExtPgm('FA719R')
     D  rtncode                       1a
     D  wcin                          1a
     D  wdck                          1a
     D  company                       3a
     D  subscriber                    5a
     D  group                         3a
     D  employer                      9a
     D  division                      5a
     D  key                          40a

     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
     D eKey            S             40a
     D eRtnCode        S              1a

     D pCall           S              1a
     D pKC             S              1a
     D pKL             S              1a
     D pRRN            S              9  0 Inz(0)
     D pRRN2           S              9  0 Inz(0)
     D pView           S              1  0 Inz(1)

     D pProgram        S             10a   Inz('F0140R')

     D wcin            S              1a
     D wdck            S              1a
     D company         S              3a
     D subscriber      S              5a
     D group           S              3a
     D employer        S              9a
     D division        S              5a
     D empName         S                   Like(EMNAME)
     D rtnCode         S              1a
     D tempKey         S             40a

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
     C                   CALLP     SearchPgm(pCall:pKC:pKL:pRRN:pRRN2:pView)

      ** If F3 was pressed in the search, exit.
     C                   If        pKC = *on
     C                   Eval      eRtnCode = '3'
     C                   Else
    
     C                   If        pRRN > *zero
     C                             AND pKL = *off
     C     pRRN          Chain     FMEMP                              60
    
     C                   If        *in60 = *on
     C                   Eval      eRtnCode = 'D'
     C                   Else
      ** Create key
     C                   Eval      wcin = 'Y'
     C                   Eval      wdck = 'Y'
     C                   Eval      rtnCode = 'K'
     C                   Move      EMCO#         company
     C                   Move      EMSUB#        subscriber
     C                   Move      EMFND         group
     C                   Move      EMEMP#        employer
     C                   Move      EMDIV         division

     C                   CALLP     McCvtApi(rtnCode:
     C                                      wcin:
     C                                      wdck:
     C                                      company:
     C                                      subscriber:
     C                                      group:
     C                                      employer:
     C                                      division:
     C                                      tempKey)
     C                   Clear                   eRtnCode
     C                   MoveL     tempKey       eKey

     C                   EndIf

     C                   Else
     C                   Eval      eRtnCode = 'C'

     C                   EndIf

     C                   EndIf

     C                   Eval      *inLR = *on
