      *====================================================================
      * (c) Copyright Progressive Data Solutions, Inc., 2000
      *====================================================================
     H/TITLE IRPTDMO - DARTMAIL REPORT DEMO PROGRAM
      *====================================================================
      * Program Description:  This program will be invoked from the
      *  DARTMail server to generate a report, convert it to PDF format and
      *  write an entry to a dataq with the location of the report.
      *
      * Called From: DARTMail Server
      *
      * Calls To:
      *
      * Created:     06/02/2000
      * Programmer:  C:113
      * Project:     DR112
      *====================================================================
      * Revisions:
      *
      *
      * ------------------------------------------------------------------
      *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
      * ------------------------------------------------------------------
      *====================================================================
     hdatfmt(*USA)
      *--------------------------------------------------------------------
      * File Specifications
      *--------------------------------------------------------------------

      *--------------------------------------------------------------------
      * Prototypes
      *--------------------------------------------------------------------
     D ACSReport       pr                  extpgm('W7421R')
     D  company                       3a
     D  group                         3a
     D  fromDate                      8a
     D  toDate                        8a
     D  queue                        10a
     D  agent#                        5a
     D  employer                      9a
     D  division                      5a
     D  zeroBal                       1a
     D  outType                       1a

      *--------------------------------------------------------------------
      * Data Structures
      *--------------------------------------------------------------------
     D months          s              9    dim(12) ctdata perrcd(1)

      ** Date and Time data structure
     D                 ds
     D dsDateTime                    12  0
     D  dsTime                        6  0 overlay(dsDateTime)
     D  dsDate                        6  0 overlay(dsDateTime:7)

      ** External program data structure
     D psds          esds                  extname(szq1p)

      *--------------------------------------------------------------------
      * Stand-alone fields
      *--------------------------------------------------------------------
     D fromDate        s               d
     D fromDtNum       s              8  0

     D toDate          s               d
     D toDtNum         s              8  0

     D company         s              3  0
     D group           s              3  0
     D agent#          s              5a   inz('00001')

     D eCompany        s              3a
     D eGroup          s              3a
     D eMonth          s              9a
     D eLocation       s             80a

     D monthAlp        s              2a
     D i               s              2  0
     D dayYear1        s              6a   inz('012000')
     D alphaDate       s              8a

     D pFromDate       s              8a
     D pToDate         s              8a
     D pEmployer       s              9a   inz
     D pDivision       s              5a   inz
     D pQueue          s             10a   inz
     D pOutType        s              1a   inz('P')
     D pZeroBal        s              1a   inz('N')
     D pJobn           s              6a

      *--------------------------------------------------------------------
      * PList definitions
      *--------------------------------------------------------------------
      ** Entry parameters
     C     *entry        PList
     C                   parm                    eCompany
     C                   parm                    eGroup
     C                   parm                    eMonth
     C                   parm                    eLocation
    
      ** Convert program parameters
     C     CvtPgmPl      plist
     C                   parm                    q1pgm
     C                   parm                    q1user
     C                   parm                    q1job
     C                   parm                    pJobn
     C                   parm                    eLocation

      *--------------------------------------------------------------------
      * KList definitions
      *--------------------------------------------------------------------

      *====================================================================
      * Main
      *====================================================================

     C                   callp     ACSReport(eCompany:eGroup:pFromDate:
     C                              pToDate:pQueue:agent#:pEmployer:
     C                              pDivision:pZeroBal:pOutType)

     C                   callb     'IRPTDMOC'    CvtPgmPl

     C                   Move      *on           *inlr

      *====================================================================
      * *inzsr - Program Initialization Subroutine
      *====================================================================
     C     *inzsr        BEGSR

      ** Get the numeric representation of a month
     C     eMonth        lookup    months(i)                              58
     C                   if        *in58
     C                   move      i             monthAlp
     C                   else
     C                   eval      monthAlp = '01'
     C                   endif

      ** Get the starting & ending dates of the given month
     C                   movel     monthAlp      alphaDate
     C                   move      dayYear1      alphaDate
     C                   move      alphaDate     fromDtNum
     C     *USA          move      fromDtNum     fromDate
     C     fromDate      adddur    1:*MONTHS     toDate
     C                   subdur    1:*DAYS       toDate
     C     *USA          move      toDate        toDtNum
     C                   move      toDtNum       pToDate
     C                   move      fromDtNum     pFromDate

     C                   move      q1jobn        pJobn

     C                   ENDSR                                                  *inzsr

      *====================================================================
      * Compile time tables
      *====================================================================
** CTDTA months
JANUARY
FEBRUARY
MARCH
APRIL
MAY
JUNE
JULY
AUGUST
SEPTEMBER
OCTOBER
NOVEMBER
DECEMBER
