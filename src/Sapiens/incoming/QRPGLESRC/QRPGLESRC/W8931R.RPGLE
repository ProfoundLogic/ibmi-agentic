     h/title W8931R - w/w data warehouse extract run & refresh dates
     H*****************************************************************
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* This Program Runs the Extract Control File Programs. First    *
     H* the program Reads the Control File WHXCP and calulates a new  *
     H* Next Extract Date for each Extract File. Then it processes    *
     H* all Files which have a Next Extract Date < = Today's (RUN)    *
     H* date. At end of pgm will again update WHXCP with new dates    *
     H* For all files processed.                                      *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*   W8931C1 - PGMNAM, FILENAME, LAST EXTRACT DATE               *
     H*   W8931C2 - PGMNAM, FILENAME, LAST EXTRACT DATE, CO#          *
     H*                                                               *
     H*  CREATED: 08/30/01  C:132  PROJECT: WC924                     *
     H*  IN BASE: xx/xx/xx  C:132  PROJECT:                           *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H*           - Add start fund year PARM from the Control File's  *
     H*             Field WHXCP/XCXFYR, pass into programs.   CI015   *
     H*                                                               *
     H* 11/04/03 C:132 SUPPORT WA121 38407                            *
     H*                Add routing file, and logic to print routing   *
     H*                page if have any errors.                       *
     H*                                                               *
     H* 04/24/02  C:132  Project: WC924                               *
     H*                - Change to add print correct extract date if  *
     H*                  Extract is run for an extract WH file        *
     H*                                                               *
     H* 03/21/02  C:132  Project: WC924                               *
     H*                - Change to add Third CL to Clear File if not  *
     H*                  going to load with incremental data          *
     H*                                                               *
     H* 03/15/02  C:132  Project: WC924                               *
     H*                - Change to add Semi-Annual Date to *inzsr     *
     H*                                                               *
     H*****************************************************************
     F*
     F*  Extract Dates Files
     fwhxcp     uf   e             disk
      ***** BEG ADD ***** 11/04/03 ***********************************
     FSMORL     IF   E           K DISK
      ***** END ADD ***** 11/04/03 ***********************************
      ***** BEG ADD ***** 11/04/03 ***********************************
      ***** END ADD ***** 11/04/03 ***********************************
     F*  AUDIT REPORT
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOB)
     D*
     d fil             s             52    dim(16) ctdata perrcd(1)             File Names
     D*
     D***************************************************************
     D* D A T A     S T R U C T U R E S                             *
     D***************************************************************
     D*
     D* Frequency Codes - D, W, M, S, Y, I or Blank
     d                 ds
     d  frq                    1     16
     d                                     dim(16)                              Extrct Frequency
     d  xcfr01                 1      1
     d  xcfr02                 2      2
     d  xcfr03                 3      3
     d  xcfr04                 4      4
     d  xcfr05                 5      5
     d  xcfr06                 6      6
     d  xcfr07                 7      7
     d  xcfr08                 8      8
     d  xcfr09                 9      9
     d  xcfr10                10     10
     d  xcfr11                11     11
     d  xcfr12                12     12
     d  xcfr13                13     13
     d  xcfr14                14     14
     d  xcfr15                15     15
     d  xcfr16                16     16
     D* Last Extract Dates from file WHXCP
     d                 ds
     d  lst                    1    112  0
     d                                     dim(16)                              Last Extract Dte
     d  xcld01                 1      7  0
     d  xcld02                 8     14  0
     d  xcld03                15     21  0
     d  xcld04                22     28  0
     d  xcld05                29     35  0
     d  xcld06                36     42  0
     d  xcld07                43     49  0
     d  xcld08                50     56  0
     d  xcld09                57     63  0
     d  xcld10                64     70  0
     d  xcld11                71     77  0
     d  xcld12                78     84  0
     d  xcld13                85     91  0
     d  xcld14                92     98  0
     d  xcld15                99    105  0
     d  xcld16               106    112  0
     D* Next Extract Dates from file WHXCP
     d                 ds
     d  nxt                    1    112  0
     d                                     dim(16)                              Next Extract Dte
     d  xcnd01                 1      7  0
     d  xcnd02                 8     14  0
     d  xcnd03                15     21  0
     d  xcnd04                22     28  0
     d  xcnd05                29     35  0
     d  xcnd06                36     42  0
     d  xcnd07                43     49  0
     d  xcnd08                50     56  0
     d  xcnd09                57     63  0
     d  xcnd10                64     70  0
     d  xcnd11                71     77  0
     d  xcnd12                78     84  0
     d  xcnd13                85     91  0
     d  xcnd14                92     98  0
     d  xcnd15                99    105  0
     d  xcnd16               106    112  0
      *
      * Input Dates Data Sructure
      * WRKDTE 7 Position input Date, INDATE is 6 Position input Date
     d                 ds
     d  wrkdte                 1      7  0 inz
     d**indate                 2      7  0 inz
     d  ##incc                 1      1  0
     d  ##inyy                 2      3  0
     d  ##inmm                 4      5  0
     d  ##indd                 6      7  0
     D*
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d*
     d ##parm          s              1    inz                                  Nbr Parms from Array
     d newdte          s              7  0 inz
     d one             s              1
     d pgmnam          s             10    inz                                  Pgm Name from Array
     d filnam          s             10    inz                                  File Name from Array
     d fildes          s             25    inz                                  File Name from Array
     d pgmone          s             10    inz('W8931C1   ')                    Pgm Name w/One Parm
     d pgmtwo          s             10    inz('W8931C2   ')                    Pgm Name w/Two Parms
      ***** BEG ADD **** 03/21/02 **************************************************************
     d pgmthree        s             10    inz('W8931C3   ')                    Pgm To Clear Files
      ***** END ADD **** 03/21/02 **************************************************************
     d rrn             s              3  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d two             s              1
     d xxfreq          s              1    inz
     d x               s              3  0 inz
     d otcnt           s              3  0 inz
     d nocnt           s              3  0 inz
     d xxdate          s              7  0 inz
     d daydte          s              7  0 inz
     d wkdate          s              7  0 inz
     d mthdte          s              7  0 inz
     d semidte         s              7  0 inz
     d yrdate          s              7  0 inz
     d nmdte8          s              8  0 inz
     d prext8          s              8  0 inz
     d prnxt8          s              8  0 inz
     d pmext8          s              8    inz
     d pmnxt8          s              8    inz
     d pmco#           s              3    inz
      *
      ***** BEG ADD ***** 03/25/04 ***********************************
     d JOBdte          s              8    inz
     d ExtFyr          s              3  0 inz
     d JBDate          s              8  0 inz
     d JBTime          s              6  0 inz
      ***** END ADD ***** 03/25/04 ***********************************
      *
     d usaDate         s               d   datfmt(*usa)
     d newDate         s               d   datfmt(*usa)
     d*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     d optnm           c                   const('CICenter Incrementa-
     d                                     l Extract Processing')
     d*** BEGIN CHG *********** 01/28/10 ************************************
     d*** pdsnam       c                   const('Progressive Data So-
     d***                                  lutions, Inc.')
     d pdsNam          S             40    Inz
     d*** END CHG ************* 01/28/10 ************************************
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     d*
     d fids01        e ds                  extname(szq2p)
     d*
     d psds          esds                  extname(szq1p)
     c*
     c*================================================================
     c*  MAIN  SECTION
     c*================================================================
     c*
     c* If have a WH Control Record, First update Next Extract Date
     c* Then Process for Extraction Programs
     c*
      ***** BEG CHG ***** 03/25/04 ***********************************
     c*****rrn           chain     pxc01                              41
     c     rrn           chain(N)  pxc01                              41
      ***** END CHG ***** 03/25/04 ***********************************
     c*
     c                   if        (not *in41)
     c*
     c* Calculate new Next Extract Dates for the WHXCP file
     c*
     c                   exsr      calnxt                                       Calculate Next Extr
      *
      ***** BEG ADD ***** 03/25/04 ***********************************
     c                   eval      ExtFyr = XCXFYR
      ***** END ADD ***** 03/25/04 ***********************************
     c*
     c*  Now Process for Extractions by looping through the File Extract arrays
     c*
     c                   eval      x = *zeros
     c*
     c                   dou       (x = 16)
     c                   eval      x = (x + 1)
     c*
     c* Substring out the File Name, Program and the Number of Parms for the Program
     c                   eval      pgmnam = %subst(fil(x):41:10)
     c                   eval      ##parm = %subst(fil(x):52:1)
     c                   eval      fildes = %subst(fil(x):15:25)
     c                   eval      filnam = fil(x)
     c*
     c                   eval      prext8 = *zeros
     c                   eval      prnxt8 = *zeros
     c*
     c*  Don't Process any Zero or Blank Dates
     c                   if        lst(x) > *zeros
     c                   eval      srdti = lst(x)
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         prext8
     c                   movel     srdto         pmext8
     c                   eval      srdti = nxt(x)
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         prnxt8
     c                   movel     srdto         pmnxt8
     c                   endif
     c*
     c* If Frequency code not = 'I' or Blank, process
     c                   if        (lst(x) > *zeros) and (frq(x) <> 'I') and
     c                             (frq(x) <> ' ')
     c*
     c* If Next Extract Date >= Current Date, Figure new Next Extract Date
     c                   if        (lst(x) < xxdate) and (nxt(x) <= xxdate)
     c                   eval      wrkdte = xxdate
     c                   eval      xxfreq = frq(x)
     c                   exsr      caldte
     c* Then run Extract Program with correct Parms
     c                   exsr      extpgm
     c* Now update Dates in Record Date Arrays
     c                   eval      lst(x) = xxdate
     c                   eval      nxt(x) = newdte
     c*
     c                   else
     c*
     c* Print Bypass of File Build Detail Line - Not Passed Last Extract Date
     c   ob              except    Head
     c                   except    Detpas
     c                   eval      nocnt = nocnt + 1
      ***** BEG ADD **** 03/21/02 **************************************************************
     c* Clear the file if not processing for it
     c                   call      pgmthree      parm3
      ***** END ADD **** 03/21/02 **************************************************************
     c                   endif                                                   LST,X <=XXDATE
     c*
     c                   ELSE
     c*
     c* Print Bypass of File Build Detail Line - Invalid Freq or Zero Date
     c   ob              except    Head
     c                   except    Detpas
     c                   eval      nocnt = nocnt + 1
      ***** BEG ADD **** 03/21/02 **************************************************************
     c* Clear the file if not processing for it
     c                   call      pgmthree      parm3
      ***** END ADD **** 03/21/02 **************************************************************
     c*
     c                   ENDIF                                                   LST,X <= ZERO
     c*
     c                   enddo
     c                   endif                                                   *IN41 = *OFF
     c*
     c* ====================================================================== *
      ***  TERM TAG ***           END OF CALCULATIONS - Teminate Pgm  *******
     c* ====================================================================== *
     c     term          tag
     c                   time                    dsdttm
     c                   except    Total
      *
      * If have processed any Extract Programs, Then update the Control File
      * It has been Updated Once, so need to Chain before the Update
     c                   if        otcnt > *zeros
     *
     C                   movel(p)  q1pgm         xcipgm
     C                   movel(p)  q1user        xciusr
      ***** BEG CHG ***** 03/25/04 ***********************************
     c***********        eval      xcitim = dstime
     c                   eval      xcitim = JBtime
      *
     C**************     eval      srdti = dsdate
     C                   eval      srdti = JBdate
      ***** END CHG***** 03/25/04 ***********************************
      *
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xcidt
      *
      *  If Maintenance User is Blank, then update Maint Fields
      *  The Maintenance fields are only updated by the W/W pgm W8930R
      *
     c                   if        (xcuser = *blanks)
     C                   movel(p)  q1user        xcuser
     C                   movel(p)  q1pgm         xcpgm
      ***** BEG ADD ***** 03/25/04 ***********************************
     C************       eval      xctime = dstime
     C                   eval      xctime = JBtime
      ***** END ADD ***** 03/25/04 ***********************************
     C                   movel     srdto         xcdt
     c                   endif
    C*
      ***** BEG CHG ***** 03/25/04 ***********************************
      * UnComment  the chain, need to update
     c***  rrn           chain     pxc01                              41
     c     rrn           chain     pxc01                              41
      ***** END CHG ***** 03/25/04 ***********************************
     c                   update    pxc01
     c                   endif
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Smorl_Key SMORL;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
     c                   seton                                        lr
     c                   return
      *
     C*================================================================
     C* EXTPGM - Call Corect Extract Program
     C*================================================================
      *
     c     extpgm        begsr
     c*
     c* If Array Parm = 1, then call CL pgm 1 which clears file and builds file
     c* Parms are Program Name, Warehouse File Name, Last Extract Date
     c                   if        (##parm = one)
     c                   call      pgmone        parm1
     c                   endif
     c*
     c* If Array Parm = 2, then call CL pgm 2 which clears file & builds file (w/CO#)
     c* Parms are Program Name, Warehouse File Name, Last Extract Date, Company #
     c                   if        (##parm = two)
     c                   call      pgmtwo        parm2
     c                   endif
     c*
     c* Now print the Line for Extraction Program
     c*
      ***** BEG ADD **** 04/24/02 **************************************************************
     c* If run Extract Program, update dates on print line
     c                   eval      srdti = xxdate
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         prext8
     c                   eval      srdti = newdte
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         prnxt8
      ***** END ADD **** 04/24/02 **************************************************************
     c                   eval      otcnt = otcnt + 1
     c   ob              except    Head
     c                   except    Detl
     c*
     c                   endsr
      *
     C*================================================================
     C* CALNXT - Calculate New Date
     C*================================================================
     c     CALNXT        begsr
      *
      *  Determine the New Next Extract Date from Last Extract date
      *  based on the Frequency Code, First by converting the 7 position
      *  date to a 8 position date, then adding the correct Duration
      *
     c                   move      XCco#         pmco#
     c*
     c                   eval      x = *zeros
     c*
     c                   dou       (x = 16)
     c                   eval      x = (x + 1)
     c*
     c                   if        lst(x) > *zeros
     c                   eval      xxfreq = frq(x)
     c                   eval      srdti = lst(x)
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         nmdte8
     c                   movel     nmdte8        usaDate
     c*
     c                   SELECT
     c                   When      (xxfreq = 'D')
     c     usaDate       adddur    1:*D          newdate
     c                   move      newdate       nmdte8
     c                   When      (xxfreq = 'W')
     c     usaDate       adddur    7:*D          newdate
     c                   move      newdate       nmdte8
     c                   When      (xxfreq = 'M')
     c     usaDate       adddur    1:*M          newdate
     c                   move      newdate       nmdte8
     c                   When      (xxfreq = 'S')
     c     usaDate       adddur    6:*M          newdate
     c                   move      newdate       nmdte8
     c                   When      (xxfreq = 'Y')
     c     usaDate       adddur    1:*Y          newdate
     c                   move      newdate       nmdte8
     c                   other
     c                   move      *zeros        nmdte8
     c                   endsl
     c*
     c                   eval      srdti = nmdte8
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         nxt(x)                         Dte Next Extract
     c*
     c                   endif
     c*
     c                   enddo
      *
     c                   endsr
      *
     C*================================================================
     C* CALDTE - Calculate New Date
     C*================================================================
     c     caldte        begsr
      *
      *  Detremine the New Next Extract Date Using dates calculated from
      *  today's date in the
      *
     c                   if        (wrkdte <= *zeros)
     c                   eval      newdte = *zeros
     c                   goto      enddte
     c                   endif
     c*
     c                   if        xxfreq = 'D'                                 Daily
     c                   eval      newdte = daydte
     c                   endif
     c*
     c                   if        xxfreq = 'W'                                 Weekly
     c                   eval      newdte = wkdate
     c                   endif
     c*
     c                   if        xxfreq = 'M'                                 Monthly
     c                   eval      newdte = mthdte
     c                   endif
     c*
     c                   if        xxfreq = 'S'                                 Semi-Annually
     c                   eval      newdte = semidte
     c                   endif
     c*
     c                   if        xxfreq = 'Y'                                 Annually
     c                   eval      newdte = yrdate
     c                   endif
      *
     c     enddte        tag
      *
     c                   endsr
      *
     C*================================================================
     C* *INZSR - Initialize the subset fields
     C*================================================================
     c     *inzsr        begsr
     C*
     c                   move      '1'           one
     c                   move      '2'           two
     c                   eval      rrn = 1
     c*
     c                   eval      otcnt = *zeros
     c                   eval      nocnt = *zeros
     C*
     c                   time                    dsdttm
      *
      ***** BEG ADD ***** 03/25/04 ***********************************
     c                   move      dsdate        JobDte
      * Save date & Time for update Control file
     c                   eval      JBdate = dsdate
     c                   eval      JBtime = dstime
      ***** END ADD ***** 03/25/04 ***********************************
     c*
     c* Parms - Called program Parm Lists
     c*
     c     parm1         plist
     c                   parm                    pgmnam
     c                   parm                    filnam
     c                   parm                    pmext8
      ***** BEG ADD ***** 03/25/04 ***********************************
     c                   parm                    JOBdte
     c                   parm                    ExtFyr
      ***** END ADD ***** 03/25/04 ***********************************
     c*
     c     parm2         plist
     c                   parm                    pgmnam
     c                   parm                    filnam
     c                   parm                    pmext8
     c                   parm                    pmco#
      ***** BEG ADD ***** 03/25/04 ***********************************
     c                   parm                    JOBdte
     c                   parm                    ExtFyr
      ***** END ADD ***** 03/25/04 ***********************************
     c*
      ***** BEG ADD **** 03/21/02 **************************************************************
     c* Parm for Clearing file Only
     c     parm3         plist
     c                   parm                    filnam
      ***** END ADD **** 03/21/02 **************************************************************
      ***** BEG ADD **** 01/28/10 **************************************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C                   callp     RtvBrnd('I':brandName)
     C                   eval      pdsnam    = brandname
      ***** END ADD **** 01/28/02 **************************************************************
     c*
     c* Parms - Called Date conversion program's parm lists
     c*
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     C*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     C*
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      '*MDYY'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     c*
     c*  Calculate new Next Extract Dates from Today for Updating Control File after an
     c*  Extract Program has been called.
     c*
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         xxdate
     c*
     c                   move      dsdate        usaDate
     c     usaDate       adddur    1:*D          newdate
     c                   move      newdate       nmdte8
     c                   eval      srdti = nmdte8
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         daydte                         Day Next Extract
     c*
     c     usaDate       adddur    7:*D          newdate
     c                   move      newdate       nmdte8
     c                   eval      srdti = nmdte8
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         wkdate                         Week Next Extract
     c*
     c     usaDate       adddur    1:*M          newdate
     c                   move      newdate       nmdte8
     c                   eval      srdti = nmdte8
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         mthdte                         Month Next Extract
     c***** BEG ADD **** 03/15/02 **************************************
     c*
     c     usaDate       adddur    6:*M          newdate
     c                   move      newdate       nmdte8
     c                   eval      srdti = nmdte8
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         semidte                        Month Next Extract
     c***** END ADD **** 03/15/02 **************************************
     c*
     c     usaDate       adddur    1:*Y          newdate
     c                   move      newdate       nmdte8
     c                   eval      srdti = nmdte8
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         yrdate                         Year Next Extract
     c*
     c                   movel(P)  'IncmtCIC'    prject            8
      *
      ***** BEG ADD ***** 11/04/03 ***********************************
      *
      * GET THE DISTRIBUTION INFORMATION
      *
     C                   Move      '0'           XXPRT
      *
     C     Smorl_Key     Klist
     C                   Kfld                    Q1pgm
     C                   Kfld                    Xxprt             1
      *
     C     Smorl_Key     Chain     Smorl                              60
      *
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         If        *IN60 = *Off
     C**********         Except    Route
     C**********         Endif
      ***** End Del ***** 08/26/14 *************************************************
      ***** END ADD ***** 11/04/03 ***********************************
     c                   except    HEAD
     c                   endsr
     C*================================================================
      ***** BEG ADD ***** 11/04/03 ***********************************
     OQSYSPRT   E            Route          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            Route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            Route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            Route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            Route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            Route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            Route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            Route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            Route          1
     O**********                                 15 'Route TO:'
     O**********             ORDIS1              60
     O**********E            Route          1
     O**********             ORDIS2              60
     O**********E            Route          1
     O**********             ORDIS3              60
     O**********E            Route          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            Route          1
     O                                           15 'Route TO:'
     O                       ORDIS1              60
     O          E            Route          1
     O                       ORDIS2              60
     O          E            Route          1
     O                       ORDIS3              60
     O          E            Route          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            Route          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            Route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 58 'COPIES:'
     O**********             ORCOPY        Z     62
     O**********                                 72 'HOLD:'
     O**********             ORHOLD              77
     O**********E            Route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE          2
     O                                           19 'RUN TIME/DTE:'
     O                       DSDTTM              39 '  :  :  &  /  /    '
      *
     O*** Beg Chg *****  01/28/10  ************************************
     O***       E            Route          1 60
     O***                                        23 '(C) COPYRIGHT PROGRESSI'
     O***                                        46 'VE DATA SOLUTIONS. INC,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
      *
      ***** END ADD ***** 11/04/03 ***********************************
     O*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                       optnm               64
     O                       DSDATE             110 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       pdsnam              60
     O                                          105 'PAGE'
     O                       PAGE1         Z    110
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           17 'Program   '
     O                                           29 'WH File   '
     O                                           41 'Description'
     O                                           68 'Lst Extrct'
     O                                           74 'Freq'
     O                                           86 'Nxt Extrct'
      *
     O          E            Detl           2
     O                       XCco#         4      5
     O                       pgmnam              17
     O                       filnam              29
     O                       fildes              55
     O                       prext8              67 '  /  /    '
     O                       frq(x)              72
     O                       prnxt8              85 '  /  /    '
      *
     O          E            Detpas         2
     O                       XCco#         4      5
     O                       pgmnam              17
     O                       filnam              29
     O                       fildes              55
     O                       prext8              67 '  /  /    '
     O                       frq(x)              72
     O                       prnxt8              85 '  /  /    '
     O                                        +   1 '** NOT PROCESSED/Cleared'
      *
     O          E            TOTAL          1
     O                                           20 'Programs Processed:'
     O                       OTCNT         Z     24
     O                                           50 'Programs NOT Processed:'
     O                       NOCNT         Z     54
     O                                           70 'Completed At:'
     O                       dsdttm              90 '  :  :  &  /  /    '
** File Names   FIL                    | PGM      PARMS
WHPOP      1  Policy Information        W8901R1   01
WHPAP      2  Policy Agent Information  W8902R1   01
WHEXP      3  Exposure by Policy Number W8903R1   01
WHCLP      4  Claims by Claim Number    W8904R1   01
WHPRP      5  Premium Information       W8905R1   01
WHLOP      6  Location Information      W8906R1   01
WHDPP      7  Department Information    W8907R1   01
WHCNP      8  County Information        W8908R1   01
WHOFP      9  Office Information        W8909R1   01
WHFNP     10  Group Information         W8910R1   01
WHUPP     11  User Parm Information     W8911R1   01
WHAGP     12  Agent Information         W8912R1   01
WHAPP     13  Agent Producer Infomation W8913R1   01
WHARP     14  A/R Detail Information    W8914R1   02
WHACP     15  Agent Commission Informtn W8915R1   02
WHCTP     16  Claim Transfers           W8916R1   02
