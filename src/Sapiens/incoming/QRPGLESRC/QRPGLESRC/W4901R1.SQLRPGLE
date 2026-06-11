     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *============================================================================================*
      /title W4901R1 - Create Summary File                                                         *
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H bnddir('WTBNDDIR')                                                                          *
      *============================================================================================*
      * Revisions                                                                                  *
      *                                                                                            *
      * 05/09/17 C:136 Support: 47173                                                              *
      *                Correct Scriptura processing to prevent abend when S4401C1                  *
      *                calls STLETTER for TIMEOUT value.                                           *
      *                                                                                            *
      * 08/05/16 C:65  Project: SY371                                                              *
      *                Recompile for additional fields.
      *                                                                                            *
      * 06/21/16 C:136 Project: SY371                                                              *
      *                Implement Scriptura Phase 2 processing                                      *
      *                                                                                            *
      * 01/04/10 C:190 Project: 25WRE                                                              *
      *                Add Billing Schedule Code and Billing Type (estimated or S/R) to outfile    *
      *                                                                                            *
      * 05/12/09 C:136 Project: 135UMB                                                             *
      *                Add Carrier # to outfile                                                    *
      *                                                                                            *
      * 02/12/09 C:220 Project: 93WHY                                                              *
      *                Additional field - info only out file WFEUP                                 *
      *                                                                                            *
      * 01/19/09 C:136 Support: WC1653                                                             *
      *                Accumulate totals to get assessment amounts because the assessment is       *
      *                written to the first WFEUP record of the policy, not the last.              *
      *                                                                                            *
      * 12/11/08 C:33  Project; WC1769                                                             *
      *                Add 6th and 7th assessments.                                                *
      *                                                                                            *
      * Created by C:136 for project WC1653                                                        *
      *============================================================================================*
     fWFEUL1    if   e           k disk    rename(PEU01 : PEU0L1)
     fWFEUP2    o    e           k disk    rename(PEU01 : FEU0PS)
     fQFEUP2    o    e           k disk    rename(PEU01 : QEU0PS)  usropn
     fQFEUP12   o    e           k disk    rename(PEU01 : QEU0P1S) usropn
     f                                     prefix(Q)
      *** beg add ***** 06/21/16 ***************************
     fXmlPrel1  if   e           k disk    usropn
      *** end add ***** 06/21/16 ***************************
    d*=============================================================================================
    d* Stand Alone Fields
    d*=============================================================================================
     d  kkco#          s                   like(euco#)
     d  date08         s              8  0 inz(0)
     d  dateCYMD       s              7  0 inz(0)
     d  dtCYM          s              5  0 inz(0)
     d  endOfMthFlg    s              1    inz(' ')
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
      *** beg chg ***** 06/21/16 *****************
     d*W4901R1         pr
     d**@@co#                         3
     d**@@mode                        1
     d**@@date                        8
     d*W4901R1         pi
     d**@@co#                         3
     d**@@mode                        1
     d**@@date                        8

     d W4901R1         pr
     d  @@co#                         3
     d  @@mode                        1
     d  @@date                        8
     d  @@xmlFile                    61
     d W4901R1         pi
     d  @@co#                         3
     d  @@mode                        1
     d  @@date                        8
     d  @@xmlFile                    61

     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d processType     s            100    inz
     d strCommand      s            255
     d xmlFile         s             60    inz
     d/copy sPrXML
      *** end chg ***** 06/21/16 *****************
      *** beg add ***** 05/09/17 *****************
     d xmlFlow         s             60    inz
     d xmlTemplate     s             60    inz
      *** end add ***** 05/09/17 *****************

     d/copy sprdate
    d*=============================================================================================
    d* KLIST
    d*=============================================================================================
     c     euky          klist
     c                   kfld                    kkco#

     c     euky1         klist
     c                   kfld                    euco#
     c                   kfld                    eufnd
     c                   kfld                    eufyr
     c                   kfld                    euemp#
     c                   kfld                    eudiv
      *============================================================================================*
      * Initialize
      *============================================================================================*
     c                   move      @@co#         kkco#

     c                   if        @@mode = 'M'
     c                   eval      endOfMthFlg = 'Y'
     c                   eval      date08 = numDate(@@date)
     c                   eval      dateCYMD = dtCYMD(date08)
     c                   open      QFEUP2
     c                   open      QFEUP12

     c                   movel(p)  dateCYMD      EURCYM
     c/exec sql
     c+ delete from QFEUP2 where EURCYM = :EURCYM
     c/end-exec

     c/exec sql
     c+ delete from QFEUP12 where EURCYM = :EURCYM
     c/end-exec

     C*** beg chg ***** 06/21/16 **************************************
     c***********        endif
      /free
       Else;
         Exsr GetXMLPrep;
       Endif;
      /End-free
     C*** end add ***** 06/21/16 **************************************

      *============================================================================================*
      * Main - read the last record of each policy from last to least to get latest values
      *============================================================================================*
      *** beg add ***** 01/04/10 **************************************
     c                   call      'RTVCLTID'
     c                   parm                    cltID             3
      *** end add ***** 01/04/10 **************************************
     c     euky          setgt     peu0l1
     c     euky          readpe    peu0l1
     c                   dow       not(%eof)

     c                   clear                   EUPRDT
     c                   clear                   EUTRDT
     c                   clear                   EUPODT
     c                   clear                   EUTYPE
     c                   clear                   EUCODE
     c                   clear                   EUSEQ#

     c/exec sql
     c+ select sum(EUINPR),
     c+        sum(EUYDWR),
     c+        sum(EUYDEA),
     c+        sum(EUUNEA),
     c+        sum(EUMDWR),
     c+        sum(EUMDEA),
     c+        sum(EUPMUN),
     c+        sum(EUPYWR),
     c+        sum(EUPYEA),
     c+        sum(EUPYUN),
     c+        sum(EUITER),
     c+        sum(EUUNTR),
     c+        sum(EUMDTR),
     c+        sum(EUIMPR),
     c+        sum(EUUNMN),
     c+        sum(EUYDWM),
     c+        sum(EUYDEM),
     c+        sum(EUMDWM),
     c+        sum(EUMDEM),
     c+        sum(EUPMUM),
     c+        sum(EUPYWM),
     c+        sum(EUPYEM),
     c+        sum(EUPYUM),
     c+        sum(EUIVLC),
     c+        sum(EUUVLC),
     c+        sum(EUMVLC),
     c+        sum(EUYDWT),
     c+        sum(EUYDET),
     c+        sum(EUMDET),
     c+        sum(EUPMUT),
     c+        sum(EUPYWT),
     c+        sum(EUPYET),
     c+        sum(EUPYUT),
     c+        sum(EUYDWD),
     c+        sum(EUYDED),
     c+        sum(EUMDWD),
     c+        sum(EUMDED),
     c+        sum(EUPMUD),
     c+        sum(EUPYWD),
     c+        sum(EUPYED),
     c+        sum(EUPYUD),
     c+        sum(EUIDTR),
     c+        sum(EUUDTR),
     c+        sum(EUINEX),
     c+        sum(EUUNEX),
     c+        sum(EUMWEX),
      *** beg add ***** 01/19/09 **********************************************************
     c+        sum(EUAS1),
     c+        sum(EUAS2),
     c+        sum(EUAS3),
     c+        sum(EUAS4),
     c+        sum(EUAS5),
     c+        sum(EUAS6),
     c+        sum(EUAS7)
      *** end add ***** 01/19/09 **********************************************************
     c+   into    :EUINPR,
     c+           :EUYDWR,
     c+           :EUYDEA,
     c+           :EUUNEA,
     c+           :EUMDWR,
     c+           :EUMDEA,
     c+           :EUPMUN,
     c+           :EUPYWR,
     c+           :EUPYEA,
     c+           :EUPYUN,
     c+           :EUITER,
     c+           :EUUNTR,
     c+           :EUMDTR,
     c+           :EUIMPR,
     c+           :EUUNMN,
     c+           :EUYDWM,
     c+           :EUYDEM,
     c+           :EUMDWM,
     c+           :EUMDEM,
     c+           :EUPMUM,
     c+           :EUPYWM,
     c+           :EUPYEM,
     c+           :EUPYUM,
     c+           :EUIVLC,
     c+           :EUUVLC,
     c+           :EUMVLC,
     c+           :EUYDWT,
     c+           :EUYDET,
     c+           :EUMDET,
     c+           :EUPMUT,
     c+           :EUPYWT,
     c+           :EUPYET,
     c+           :EUPYUT,
     c+           :EUYDWD,
     c+           :EUYDED,
     c+           :EUMDWD,
     c+           :EUMDED,
     c+           :EUPMUD,
     c+           :EUPYWD,
     c+           :EUPYED,
     c+           :EUPYUD,
     c+           :EUIDTR,
     c+           :EUUDTR,
     c+           :EUINEX,
     c+           :EUUNEX,
     c+           :EUMWEX,
      *** beg add ***** 01/19/09 **********************************************************
     c+           :EUAS1,
     c+           :EUAS2,
     c+           :EUAS3,
     c+           :EUAS4,
     c+           :EUAS5,
     c+           :EUAS6,
     c+           :EUAS7
      *** end add ***** 01/19/09 ****************
     c+   from QTEMP/WFEUP
     c+   where euco#  = :euco#  and
     c+         eufnd  = :eufnd  and
     c+         euemp# = :euemp# and
     c+         eudiv  = :eudiv  and
     c+         eufyr  = :eufyr
     c/end-exec

      *** beg chg ***** 05/09/17 ****************
      *** beg add ***** 06/21/16 ****************
      //**/free
      //** If %parms >= 4;
      //**    EUxmlFile = xmlFile;
      //** Endif;
      //**/end-free
      *** end add ***** 06/21/16 ****************
      /Free
       If %Parms >= 4;
          EUxmlfile = xmlTemplate;
       Endif;
      /End-Free
      *** end chg ***** 05/09/17 ****************

     c                   write     feu0ps

     c                   if        endOfMthFlg = 'Y'

     c                   movel(p)  dateCYMD      EURCYM
     c                   movel(p)  dateCYMD      QEURCYM

     c                   write     qeu0ps

     c     *usa0         move      @@date        QEUASOF
     c                   if        eupfdt <> 0
     c     *usa          move      eupfdt        QEUPFDT
     c                   else
     c     *usa          move      '01/01/1901'  QEUPFDT
     c                   endif
     c                   if        euptdt <> 0
     c     *usa          move      euptdt        QEUPTDT
     c                   else
     c     *usa          move      '01/01/1901'  QEUPTDT
     c                   endif

     c                   eval      QEUCO#  = EUCO#
     c                   eval      QEUFND  = EUFND
     c                   eval      QEUEMP# = EUEMP#
     c                   eval      QEUDIV  = EUDIV
     c                   eval      QEUFYR  = EUFYR
     c                   eval      QEUNAME = EUNAME
     c                   eval      QEUINPR = EUINPR
     c                   eval      QEUYDWR = EUYDWR
     c                   eval      QEUYDEA = EUYDEA
     c                   eval      QEUUNEA = EUUNEA
     c                   eval      QEUMDWR = EUMDWR
     c                   eval      QEUMDEA = EUMDEA
     c                   eval      QEUPMUN = EUPMUN
     c                   eval      QEUPYWR = EUPYWR
     c                   eval      QEUPYEA = EUPYEA
     c                   eval      QEUPYUN = EUPYUN
     c                   eval      QEUJRST = EUJRST
     c                   eval      QEUCHAP = EUCHAP
     c                   eval      QEUCPDS = EUCPDS
     c                   eval      QEUAS1  = EUAS1
     c                   eval      QEUAS2  = EUAS2
     c                   eval      QEUTAX  = EUTAX
     c                   eval      QEUGNAM = EUGNAM
     c                   eval      QEUCNAM = EUCNAM
     c                   eval      QEUAGT# = EUAGT#
     c                   eval      QEUAGFE = EUAGFE
     c                   eval      QEUAGNM = EUAGNM
     c                   eval      QEUPROD = EUPROD
     c                   eval      QEUPRNM = EUPRNM
     c                   eval      QEURATE = EURATE
     c                   eval      QEUSAGT = EUSAGT
     c                   eval      QEUSFEI = EUSFEI
     c                   eval      QEUSANM = EUSANM
     c                   eval      QEUSPRD = EUSPRD
     c                   eval      QEUSPNM = EUSPNM
     c                   eval      QEUSRAT = EUSRAT
     c                   eval      QEURTCD = EURTCD
     c                   eval      QEUAS3  = EUAS3
     c                   eval      QEURGN# = EURGN#
     c                   eval      QEURGDS = EURGDS
     c                   eval      QEUNWRW = EUNWRW
     c                   eval      QEUINEX = EUINEX
     c                   eval      QEUUNEX = EUUNEX
     c                   eval      QEUMWEX = EUMWEX
     c                   eval      QEUST   = EUST
     C                   eval      QEURCYM = EURCYM
     C                   eval      QEUITER = EUITER
     C                   eval      QEUUNTR = EUUNTR
     C                   eval      QEUMDTR = EUMDTR
     C                   eval      QEUAS4  = EUAS4
     C                   eval      QEUAS5  = EUAS5
     C***  Begin Add  ***  12/11/08  **************************************
     C                   eval      QEUAS6  = EUAS6
     C                   eval      QEUAS7  = EUAS7
     C***  End   Add  ***  12/11/08  **************************************
     C                   eval      QEUIMPR = EUIMPR
     C                   eval      QEUUNMN = EUUNMN
     C                   eval      QEUYDWM = EUYDWM
     C                   eval      QEUYDEM = EUYDEM
     C                   eval      QEUMDWM = EUMDWM
     C                   eval      QEUMDEM = EUMDEM
     C                   eval      QEUPMUM = EUPMUM
     C                   eval      QEUPYWM = EUPYWM
     C                   eval      QEUPYEM = EUPYEM
     C                   eval      QEUPYUM = EUPYUM
     C                   eval      QEUIVLC = EUIVLC
     C                   eval      QEUUVLC = EUUVLC
     C                   eval      QEUMVLC = EUMVLC
     C                   eval      QEUYDWT = EUYDWT
     C                   eval      QEUYDET = EUYDET
     C                   eval      QEUMDET = EUMDET
     C                   eval      QEUPMUT = EUPMUT
     C                   eval      QEUPYWT = EUPYWT
     C                   eval      QEUPYET = EUPYET
     C                   eval      QEUPYUT = EUPYUT
     C                   eval      QEUYDWD = EUYDWD
     C                   eval      QEUYDED = EUYDED
     C                   eval      QEUMDWD = EUMDWD
     C                   eval      QEUMDED = EUMDED
     C                   eval      QEUPMUD = EUPMUD
     C                   eval      QEUPYWD = EUPYWD
     C                   eval      QEUPYED = EUPYED
     C                   eval      QEUPYUD = EUPYUD
     C                   eval      QEUIDTR = EUIDTR
     C                   eval      QEUUDTR = EUUDTR
     C***  Begin Add  ***  02/12/09  **************************************
     C                   eval      QEUMKRG = EUMKRG
     C***  End   Add  ***  02/12/09  **************************************
     C*** beg add ***** 05/12/09 ******************************************
     C                   eval      QEUCAR# = EUCAR#
     C*** end add ***** 05/12/09 ******************************************
     C*** beg add ***** 01/04/10 ******************************************
     c                   if        cltid = '025'
     C                   eval      QEUBSCD = EUBSCD
     C                   eval      QEUSEFL = EUSEFL
     c                   endif                                                  cltid = '025'
     C*** END add ***** 01/04/10 ******************************************
     c                   write     qeu0p1s

     c                   endif

     c     euky1         setll     peu0l1
     c     euky          readpe    peu0l1
     c                   enddo

     c                   if        endOfMthFlg = 'Y'
     c                   close     QFEUP2
     c                   close     QFEUP12
     c                   endif

     c                   eval      *inlr = *on
      *** beg add ***** 06/21/16 *****************
      /free
       Begsr GetXMLPrep;
       If %parms < 4;
          LeaveSr;
       Endif;
       // Don't execute Scriptura unless printed output is desired.
       If @@XmlFile = 'F';
          @@XmlFile = *blanks;
          LeaveSR;
       Endif;
       @@XmlFile = *blanks;
       processType = 'CEU Summary Report';
       outReturn = processType;
       outFldrPrefix = *blanks;
       isUseSpoolOut  = UseSpoolOut(processType);
       isUseScriptura = UseScriptura(processType);
       isUse3rdParty  = Use3rdXml(processType);
       isUseXML = isUseScriptura or isUse3rdParty;
       //** beg chg ***** 05/09/17 *************
       //*If isUseXML;
       //*** Open XmlPrel1;
       //*** outPreString = 'CEUSummary';
       //*** xmlFile = GetXMLFileName(' ':*zeros:outCo#:outFnd:
       //***  outFldrPrefix:outPreString:outReturn);
       //*** Chain xmlFile xp100;
       //*** xmlFile = XPXmlT;
       //*** @@XmlFile = xmlFile + '$';
       //*** Close XmlPrel1;
       //*Endif;
       If isUseXML;
          Open XmlPrel1;
          outPreString = 'CEUSummary';
          xmlFlow = GetXMLFileName(' ':*zeros:outCo#:outFnd:
           outFldrPrefix:outPreString:outReturn);
          Chain xmlFlow xp100;
          xmlTemplate = XPxmlT;
          @@XmlFile = xmlFlow + '$';
          Close XmlPrel1;
       Endif;
       //** end chg ***** 05/09/17 *************
       Endsr;
      /end-free
      *** end add ***** 06/21/16 *****************
