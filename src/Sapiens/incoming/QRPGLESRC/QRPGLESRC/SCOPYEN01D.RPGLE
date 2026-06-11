      /TITLE SCOPYEN01D - /COPY 'D' specs for Endorsement programs
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 01/27/23 C:291 Project: WC2863                                *
      //                Add Approved/Excluded State Flag.              *
      //                                                               *
      // 10/25/22 C: 079 Project: WC2856                               *
      //                 Add Scriptura Type field                      *
      //                                                               *
      // 10/12/22 C: 333 Project: WC2834                               *
      //                 Endorsement Recognition of Cancellation       *
      //                 - Add employer API prototype (WprEmplr)       *
      //                                                               *
      // 01/31/22 C: 079 Project: WC2853                               *
      //                 Add save date field                           *
      //                                                               *
      // 01/20/21 C:136 Support: 93990                                 *
      //                Fix printing from W/W Endorsements for generic *
      //                print programs.                                *
      //                                                               *
      // 10/28/20 C:079 Project: WC2671                                *
      //                Audit Request - Change to a TMPLRQST call.     *
      //                                                               *
      // 02/12/20 C:136 Support: 80310                                 *
      //               -Move form name to different position in LDA    *
      //                to not conflict with Scriptura processing.     *
      //                                                               *
      // 10/23/18 C:291 Project: WC2549                                *
      //                Add Endorsement Form to LDA.                   *
      //                                                               *
      // 01/09/18 C:062 Project: 133WQH                                *
      //                Add edition date work field                    *
      //                                                               *
      // 12/28/16 C:136 Project: SY371                                 *
      //                Configure Scriptura processing for mid-term    *
      //                endorsements                                   *
      //                                                               *
      // 10/11/16 C:33  Project: SY371                                 *
      //                Adjust LDA for XML based on policy page        *
      //                                                               *
      // 07/17/16 C:302 Project: SY371                                 *
      //                Scriptura phase 2                              *
      //                Clean up Program/Copybook                      *
      //                                                               *
      //===============================================================*

     d elfds           ds
     d   elrrn               397    400B 0

     d mufds4          ds
     d   murrn4              397    400B 0

     d crtFrm          s              1
     d qqCall          s              1
     d qqPrds          s             40
     d ##Call          s              1
     d ##Kc            s              1
     d ##Kl            s              1
     d ##Rrn           s              9  0
     d ##Rrn2          s              9  0
     d ##View          s              1  0
     d addMode         s              1n
     d prtMode         s              1n
     d infoMode        s              1n
     d srDti           s              8  0
     d srDto           s             10
     d srErr           s              1
     d srFfmt          s              8
     d srTfmt          s              8
     d svDiv           s                   like(eldiv)
     d svJrst          s                   like(rsjrst)
     d xxAgt#          s                   like(ahagt#)
     d xxCarr          s                   like(fmcarr)
     d xxCo#           s                   like(elco#)
     d xxCpl#          s                   like(fmcpl#)
     d xxCsz           s             40
     d xxDiv           s                   like(eldiv)
     d xxEad1          s                   like(fmead1)
     d xxEad2          s                   like(fmead2)
     d xxEad3          s                   like(fmead3)
     d xxEmp#          s                   like(elemp#)
     d xxEmst          s                   like(fmemst)
     d xxEnd#          s                   like(muend#)
     d xxEnmp          s                   like(elenmp)
     d xxFnd           s                   like(elfnd)
     d xxFnst          s                   like(fnpst)
     d xxFyr           s                   like(elfyr)
     d*** Begin Add *** 01/09/18 ************************************
     d xxEdDt          s                   like(ENedDt)
     d xxSt            s                   like(ESst)
     d*** End   Add *** 01/09/18 ************************************
     d*** Beg Add *** 10/28/20 ***********************************
     D isAudReq        s              1n
     d*** End Add *** 10/28/20 ***********************************
     d xxPfdt          s              8  0
     d xxSeq#          s                   like(museq#)
     d xxForm          s             15A

     d Ae_flag         s              1
     d state_array     s              2    DIM(100)
     d Attach          s              1

      *** beg add ***** 01/20/21 ************************
     d chrGenPgmType   s            100
     d isGenPgmInStak  s               n
     d idxGenPgms      s              1  0
     d #OfGenPgms      c                   Const(5)

     d dsGenPgms       ds
     d  dsPgm1                       10    Inz('SRALTEMP')
     d  dsPgm2                       10    Inz('SRDEDUCT')
     d  dsPgm3                       10    Inz('SRNOTICE')
     d  dsPgm4                       10    Inz('SROFFICER')
     d  dsPgm5                       10    Inz('SRPEOCLT')
     d aryGenPgms                    10    Dim(#OfGenPgms) Overlay(dsGenPgms)

     d dsGenTypes      ds
     d  dsTyp1                      100    Inz('Alternate Employer Endorsement')
     d  dsTyp2                      100    Inz('Deductible Endorsement')
     d  dsTyp3                      100    Inz('Policy Notice Endorsement')
     d  dsTyp4                      100    Inz('Officer Endorsement')
     d  dsTyp5                      100    Inz('PEO Client Endorsement')
     d aryGenType                   100    Dim(#OfGenPgms) Overlay(dsGenTypes)
      *** end add ***** 01/20/21 ************************

     d                 ds
     d xxzip                          9
     d  xxzip1                        5    overlay(xxzip)
     d  xxzip2                        4    overlay(xxzip:6)

     d                 ds
     d dsdat7                         7  0
     d  dscent                        1    overlay(dsdat7)
     d  dsyy                          2    overlay(dsdat7:2)
     d  dsmm                          2    overlay(dsdat7:4)
     d  dsdd                          2    overlay(dsdat7:6)

     d                 ds
     d dsdttm                        12  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        6  0 overlay(dsdttm:7)

     d svlda          Uds
     d ldend#                         5  0
     d  ldenda                        5    overlay(ldend#)
     d ldmyrr                         9  0
     d ldeedt                         8  0
     d ldfyr                          3  0
     d ldpyrl                         1
      *** Beg Add ***** 02/12/20 *****************
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55
      *** End Add ***** 02/12/20 *****************
     d ldaXml                191    250

     d psds          esds                  extname(szq1p)

      *** Beg Add * 07/17/16
     dWriteInCopybook  S               N   Inz(*On)
     dUseDfltAddparms  S               N   Inz(*On)
     D RunningSRText   S               N   Inz(*Off)

     d formName        s             15A
     d PrintingPolicy  S               N

     d isScriptUsed    s              1A   inz('N')
     d isUseSpoolOut   s              1n
     d isUseXML        S              1n
     *** Beg add ***** 01/27/23 ************************
     d isApprExclSt    s               n
     d stateFound      s               n
     *** End add ***** 01/27/23 ************************
     d outCall         s             11
     d InXML           s             60
     d InXML@          s             61
     d InReturn        s            250
     d OutMode         s              1
     d OutRrn          s              9
     d OutPrefix       s              2
     d OutString       s             10
     d OutCo#          s              3
     d OutFnd          s              3
     d pgmInStack      s              1
     d processType     s            250
     d UseScript       s              1n
     d UsePrintOut     s              1n
     d UseXML          S              1n
     d*** Beg Add *** 01/31/22 ******************************************
     d svStRptEfDt     s                   like(elpfdt)
     d*** End Add *** 01/31/22 ******************************************
     d*** Beg Add *** 10/12/22 ******************************************
     d xxCxDate        s                   like(ELpfdt)
     d*** End Add *** 10/12/22 ******************************************
     d*** Beg Add *** 10/25/22 ******************************************
     d ScripturaType   s            100a
     d*** End Add *** 10/25/22 ******************************************

     d* Returns '1' in parm 2 if pgm in stack
     d ChkPgmInStack   pr                  ExtPgm('SRCHKPGM')
     d                               10    const
     d                                1
     d

      * Scripture Processing Program
     d StLetter        pr                  ExtPgm('STLETTER')
     d   outCall_                    11
     d   InXML_                      60
     d   InReturn_                  250    options(*nopass)
     d   OutMode_                     1    const options(*nopass)
     d   OutRrn_                      9    const options(*nopass)
     d   OutPrefix_                   2    const options(*nopass)
     d   OutString_                  10    const options(*nopass)
     d   OutCo#_                      3    const options(*nopass)
     d   OutFnd_                      3    const options(*nopass)

      /copy sPrModule
      /copy sPrXML
      //***** Beg Add *** 10/12/22 ***********************************
      /copy WprEmplr
      //***** End Add *** 10/12/22 ***********************************

     d SendToScriptura...
     d                 pr                  ExtPgm('S4401C')
     d  outXMLString                 61
