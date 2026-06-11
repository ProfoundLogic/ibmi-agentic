     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE A/R Detail Report - Prompt
    H*********************************************************************
    H*  DESCRIPTION:  This program prompts for selection criteria to run *
    H*                the A/R Detail Report.                             *
     H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU                                                           *
     H*                                                                   *
    H*  CALLS TO:                                                        *
    H*    W7380C                                                         *
    H*                                                                   *
    H*  REWRITE: 08/01/00      C:33      PROJECT: WC804                  *
    H*********************************************************************
    H* REVSIONS:                                                         *
     H*                                                                   *
     H* 05/28/14  C:279 Project:129WSV                                    *
     H*                 - Added field 'Copy to Month End File'.           *
     H*                 - Removed Group range fields.                     *
     H*                                                                   *
     H* 12/21/12  C:279 Project:WC2115
     H*                 Field 'Print Emp w/no acti' default to 'N'.
     H*
     H* 01/27/12  C:180 Support WA139 79089
     H*                 Do not allow COMMA or PERIOD on DIV prompt.
     H*
     H* 06/26/07  C:189 Support: 58273                                    *
     H*                 If the restrictions have been set to allow for    *
     H*                 multiple groups, the flag used also must be set.  *
     H*                                                                   *
     H*                                                                   *
     H* 12/07/04  C:136 Project: 129WHU                                   *
     H*                 Add FCC's A/R Detail mods to base (flag           *
     H*                 controlled)                                       *
     H*                                                                   *
    H* 09/07/04  c:53 Project wc1050                                     *
    H*                Add aging rpt option to SCR03                      *
    H*                                                                   *
     H*  04/29/03 C:132 Support: WA065 # 37011                            *
     H*                 Add group restriction to pgm.                     *
     H*                                                                   *
    H* 06/10/02  c:61 Project wc577                                      *
    H*                center program name on prompt screen               *
    H*                                                                   *
     H*  05/16/01  C:136 PROJECT: WC577                               *
     H*                  Display location of output file.             *
    H*                                                                   *
    H*********************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw7380f    cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwtctp     if   e             disk    rename(pct01:pct0p)
      *** beg add ***** 12/07/04 ******************************************
     Fswcpf     if   e             disk
      *** end add ***** 12/07/04 ******************************************

     D/copy sprstring
     D/copy sprdate
     D/copy wprusrprms
     D/copy wprAR
     D/copy wprgroup
     D/copy wpremplr
     D** **/copy wpragent

     D*** beg add ************************ 05/16/01 ************************************************
     DSA030R           PR                  extpgm('SA030R')
     D ##sufx                         3
     D*** end add ************************ 05/16/01 ************************************************

     DS0006R           PR                  extpgm('S0006R')
     D environment                    1    const

      ******* beg add ****** 05/28/14 **************************************
     d CheckLib        pr                  extpgm('SRCHKLIB')
     d ##Lib                         10    const
     d ##Type                         4    const
     d ##AppendSfx                    1    const
     d ##Flag                         1
      ******* end add ****** 05/28/14 **************************************

     DW7380C           PR                  extpgm('W7380C')
     D #@co#                          3
     D ##fnd_fr                       3
     D ##fnd_to                       3
     D ##yr_fr                        3
     D ##yr_to                        3
     D ##ardt_fr                      8
     D ##ardt_to                      8
     D ##EmpNoAct                     1
     D ##sort                         1
     D ##agt#                         5
     D ##emp#                         9
     D ##div                          5
     D ##plan                         1
     D ##ctyp                        10
     D ##AR_code                      1
     D ##arcd                        60
     D ##report                       1
     D ##output                       1
     D ##outq                        10    const
     D*** beg add *** 09/07/04 *********************************
     D ##arpt                         1
     D*** beg end *** 09/07/04 *********************************
      *** beg add ***** 12/07/04 ***********************************************
     D ##billtyp                      1
      *** end add ***** 12/07/04 ***********************************************
      ******** beg add ****** 05/28/14 ****************************************
     D ##MnthEnd                      1
      ******** end add ****** 05/28/14 ****************************************

     D/copy scopypr01d

     D srchCtgrys      s             10    dim(10) ctdata perrcd(1)
     D srchCodes       s             10    dim(30) ctdata perrcd(1)

     D  scctyp_all     ds
     D  scctyp                 1     10    dim(10) ascend
     D  scctyp_1               1      1
     D  scctyp_2               2      2
     D  scctyp_3               3      3
     D  scctyp_4               4      4
     D  scctyp_5               5      5
     D  scctyp_6               6      6
     D  scctyp_7               7      7
     D  scctyp_8               8      8
     D  scctyp_9               9      9
     D  scctyp_10             10     10

     D  scarcd_all     ds
     D  scarcd                 1     60    dim(30) ascend
     D  scarcd_1               1      2
     D  scarcd_2               3      4
     D  scarcd_3               5      6
     D  scarcd_4               7      8
     D  scarcd_5               9     10
     D  scarcd_6              11     12
     D  scarcd_7              13     14
     D  scarcd_8              15     16
     D  scarcd_9              17     18
     D  scarcd_10             19     20
     D  scarcd_11             21     22
     D  scarcd_12             23     24
     D  scarcd_13             25     26
     D  scarcd_14             27     28
     D  scarcd_15             29     30
     D  scarcd_16             31     32
     D  scarcd_17             33     34
     D  scarcd_18             35     36
     D  scarcd_19             37     38
     D  scarcd_20             39     40
     D  scarcd_21             41     42
     D  scarcd_22             43     44
     D  scarcd_23             45     46
     D  scarcd_24             47     48
     D  scarcd_25             49     50
     D  scarcd_26             51     52
     D  scarcd_27             53     54
     D  scarcd_28             55     56
     D  scarcd_29             57     58
     D  scarcd_30             59     60

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D optnm           c                   const('A/R Detail Report')
     D comapi          c                   const('WA001R')
      *** beg add ***** 01/27/12 ***********************************************
     D DIGITS          C                   ' 0123456789'
      *** END add ***** 01/27/12 ***********************************************
     D @@call          s              1
     D @@prds          s             40
     D xxcall          s                   like(@@call)
     D ##co#           s              3  0
     D ##fnam          s             40
     D ##fnd           s              3  0
     D svEmp#          s                   like(scEmp#)
     D svDiv           s                   like(scDiv)
     D srchCtgry       s                   like(*in60)
     D srchCode        s                   like(*in60)
     D x               s              3  0
     D #@co#           s              3
     D ##fnd_fr        s              3
     D ##fnd_to        s              3
     D ##yr_fr         s              3
     D ##yr_to         s              3
     D ##ardt_fr       s              8
     D ##ardt_to       s              8
     D*** beg add ************************* 05/16/01 ***********************************************
     D ##sufx          s              3
     D xxrptl          s             23
     D*** end add ************************* 05/16/01 ***********************************************
     D ##agt#          s              5
     D ##emp#          s              9
     D group           s              3  0
     D FNrrn           s              9  0
     D EMrrn           s              9  0
     D CTrrn           s              9  0
     D ARCode          s             10
     D error           s              1
      ******* beg add ****** 06/10/02 *********************************
     D xxtmp           s             40
      ******* end add ****** 06/10/02 *********************************
      ******* beg del ****** 05/28/14 *********************************
      ******* BEG ADD ****** 04/29/03 *********************************
     D*grpflg****      s              1    inz
     D*usrGrp****      s              3  0 inz
      ******* END ADD ****** 04/29/03 *********************************
      *** beg add ***** 01/27/12 ***********************************************
     D*@EmpOK***       S               N   INZ(*OFF)
     D*SCDIV#***       S              5  0 INZ
     D*EmpRRN***       S              9  0 INZ
      *** end add ***** 01/27/12 ***********************************************
      ******* end del ****** 05/28/14 *********************************
      ******* beg add ****** 05/28/14 *********************************
     d @@flag          s              1
      ******* end add ****** 05/28/14 *********************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@prds
     C                   parm                    @@call
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
     C/COPY SCOPYPR01C
    C*
    C*====================================================================
    C* DSCAPI - Call API for getting descriptions (use 91 on error)
    C*====================================================================
    C*
     C     DSCAPI        begsr
    C*
     C                   call      comapi        compl                  91
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     ERRCK1        begsr
     C*
      ********* beg del ********* 05/28/14 ***********************************
     C**n90****          if        scFnd_fr <> *zero and
     C*********                     validGrp(scFnd_fr) = *off
     C*********          seton                                        71  90
     C*********          endif
     C*
     C**n90****          if        scFnd_to <> *zero and
     C*********                     validGrp(scFnd_to) = *off
     C*********          seton                                        72  90
     C*********          endif
     C*
     C**n90****          if        scFnd_to < scFnd_fr
     C*********          seton                                        73  90
     C*********          endif
      ********* end del ********* 05/28/14 ***********************************
     C*
     C  n90              if        scYr_fr < *zero
     C                   seton                                        74  90
     C                   endif
     C*
     C  n90              if        scYr_to < scYr_fr
     C                   seton                                        75  90
     C                   endif
     C*
     C                   eval      scARdt_fr = dtCheck(scARdt_fr)
     C*
     C  n90              if        scARdt_fr = *zero
     C                   seton                                        76  90
     C                   endif
     C*
     C                   eval      scARdt_to = dtCheck(scARdt_to)
     C*
     C  n90              if        scARdt_to = *zero
     C                   seton                                        77  90
     C                   endif
     C*
     C  n90              if        cvtToDate(scARdt_to) < cvtToDate(scARdt_fr)
     C                   seton                                        78  90
     C                   endif
     C*
     C  n90              if        scEmpNoAct <> 'Y' and scEmpNoAct <> 'N'
     C                   seton                                        79  90
     C                   endif
     C*
     C  n90              if        scSort <> 'A' and scSort <> 'E' and
     C                             scSort <> 'N'
     C                   seton                                        80  90
     C                   endif
     C** **
     C**n90              if        scAgt# <> *zero and validAgt#(scAgt#)
     C** **              seton                                        81  90
     C** **              endif
     C** **
     C**n90              if        (scEmp# <> *zero or scDiv <> *zero) and
     C** **                         validEmp(scEmp#:scDiv) = *off
     C** **              seton                                        82  90
     C** **              endif
     C*
      *** beg add ***** 01/27/12 ***********************************************
     C  N90DIGITS        CHECK     scdiv                                  82
     C   82              SETON                                        90
      ********* beg del ******* 05/28/14 ***********************************
     C**N90****          MOVE      scdiv         scdiv#
      /FREE
       //   If *in90 = *off and (scemp# <> *zero or scdiv <> *blank) and
       //      scfnd_fr <> *zero and scfnd_fr = scfnd_to;
       //      eval  @EmpOk = ValidEmp(scemp#:scdiv#:EmpRRN:scfnd_fr);
       //      if @EmpOK = *OFF;
       //         eval *in82=*on;
       //         eval *in90=*on;
       //      endif;
       //   endif;
       //****** end del ******* 05/28/14 ***********************************
      /END-FREE
      *** END add ***** 01/27/12 ***********************************************
     C  n90              if        scEmp# = *zero and scDiv <> *blank
     C                   seton                                        82  90
     C                   endif
     C*
     C  n90              if        (scEmp# <> svEmp# or scDiv <> svDiv)
     C                              and scDiv = *blank
     C                   seton                                        83  90
     C                   eval      svEmp# = scEmp#
     C                   eval      svDiv  = scDiv
     C                   endif
     C*
     C  n90              if        scCommPlan <> 'A' and scCommPlan <> 'E'
     C                              and scCommPlan <> 'C'
     C                   seton                                        84  90
     C                   endif
     C*
     C                   if        not *in90
     C                   exsr      process_2
     C                   eval      *inkl = *off
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
    C*
     C     ERRCK2        begsr
     C*
     C  n90              do        10            x
     C                   if        scctyp(x) <> *blank
     C                              and validCat(scctyp(x)) = *off
     C                   eval      *in(x + 20) = *on
     C                   seton                                        71  90
     C                   leave
     C                   endif
     C                   enddo
     C*
     C  n90              if        scctyp_all <> *blank and
     C                              (scarcd_all <> *blank or
     C                               scARCodes  <> *blank)
     C                   seton                                        74  90
     C                   endif
     C*
     C  n90              if        scARCodes <> *blank and
     C                              scARCodes <> 'I' and scARCodes <> 'E' and
     C                              scARCodes <> 'C'
     C                   seton                                        72  90
     C                   endif
     C*
     C  n90              if        scarcd_all <> *blank and
     C                              (scARCodes <> 'I' and scARCodes <> 'E')
     C                   seton                                        75  90
     C                   endif
     C*
     C  n90              do        30            x
     C                   if        scarcd(x) <> *blank
     C                              and validCode(scarcd(x)) = *off
     C                   eval      *in(x + 20) = *on
     C                   seton                                        73  90
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
    C*
     C     ERRCK3        begsr
     C*
     C*** beg add *** 09/07/04 *********************************
     C*
     C  n90              if        (scarpt <> 'Y' and scarpt <> 'N')
     C                   seton                                        73  90
     C                   endif

     C  n90              if        (scarpt = 'Y' and scCommPlan <> 'E')
     C                   seton                                        75  90
     C                   endif
     C*** end add *** 09/07/04 *********************************
     C*
     C  n90              if        scOutput <> 'P' and scOutput <> 'F'
     C                              and scOutput <> 'B'
     C                   seton                                        71  90
     C                   endif
     C*
     C  n90              if        (scOutput = 'P' or scOutput = 'B') and
     C                             (scReport <> 'D' and scReport <> 'S' and
     C                              scReport <> 'B')
     C                   seton                                        72  90
     C                   endif
     C*
      *** beg add ***** 12/07/04 ***********************************************
     C  n90              if        scbilltyp <> *blank

     C                   if        scbilltyp <> 'D' and
     C                             scbilltyp <> 'A' and
     C                             scbilltyp <> 'P'
     C                   seton                                        74  90
     C                   endif                                                  scbype <> 'D'

     C                   endif                                                  scbtyp <> *blank
      *** end add ***** 12/07/04 ***********************************************
    C*
     C                   endsr
    C*====================================================================
    C* KJDO   - Processing for F10
    C*====================================================================
    C*
     C     KJDO          begsr
     C*
     C     hfld          lookup    srchCtgrys                             60
     C                   eval      srchCtgry = *in60
     C*
     C     hfld          lookup    srchCodes                              60
     C                   eval      srchCode = *in60
    C*
     C                   select
     C*____________________________________________________________________
    C*
      ******** beg del ******** 05/28/14 **************************************
     C********           when      hfld = 'SCFND_FR' or hfld = 'SCFND_TO'
     C*
     C********           callp     grpSrch(*inkc:*inkl:group)
     C*
     C********           eval      *in89 = *on
     C*
     C********           if        group <> *zero
     C*
     C********           select
     C********           when      hfld = 'SCFND_FR'
     C********           eval      scFnd_Fr = group
     C********           eval      *in72 = *on
     C********           when      hfld = 'SCFND_TO'
     C********           eval      scFnd_To = group
     C********           eval      *in74 = *on
     C********           endsl
     C*
     C********           else
     C*
     C********           select
     C********           when      hfld = 'SCFND_FR'
     C********           eval      *in71 = *on
     C********           when      hfld = 'SCFND_TO'
     C********           eval      *in72 = *on
     C********           endsl
     C*
     C********           endif
     C*____________________________________________________________________
    C*
     C********           when      hfld = 'SCEMP#' or hfld = 'SCDIV'
     C********                      and SCFND_fr = SCFND_to
     C*
     C********           eval      EMrrn = EmpSrch(*inkc:*inkl:error:
     C********                                     SCFND_fr)
     C*
     C********           eval      *in89 = *on
     C*
     C********           if        error = *on
     C*
     C********           eval      *in71 = *on
     C*
     C********           else
     C*
     C*****EMrrn         chain     pem0p                              60
     C*
     C********           if        EMrrn <> *zero and not *in60
     C********           eval      scemp# = EMEMP#
     C********           move      EMDIV         scdiv
     C********           eval      *in84 = *on
     C********           else
     C********           eval      *in82 = *on
     C********           endif
     C*
     C********           endif
      ******** end del ******** 05/28/14 **************************************
     C*____________________________________________________________________
    C*
     C                   when      srchCtgry = *on
     C*
     C                   eval      x = 1
     C     hfld          lookup    srchCtgrys(x)                          60
     C*
     C                   eval      CTrrn = ARCatSrch(*inkc:*inkl)
     C*
     C                   eval      *in81 = *on
     C*
     C     CTrrn         chain     pct0p                              60
     C*
     C                   if        *in60 = *off
     C                   eval      scctyp(x) = CTCTYP
     C                   eval      *in(x + 20 + 1) = *on
     C                   else
     C                   eval      *in(x + 20) = *on
     C                   endif
     C*____________________________________________________________________
    C*
     C                   when      srchCode = *on
     C*
     C                   eval      x = 1
     C     hfld          lookup    srchCodes(x)                           60
     C*
     C                   eval      ARCode = ARCodeSrch(*inkc:*inkl)
     C*
     C                   eval      *in83 = *on
     C*
     C                   if        ARCode <> *blanks
     C                   eval      scarcd(x) = ARCode
     C                   eval      *in(x + 20 + 1) = *on
     C                   else
     C                   eval      *in(x + 20) = *on
     C                   endif
     C*____________________________________________________________________
    C*
     C                   endsl
     C*
     C                   endsr
    C*
    C*====================================================================
    C* KLDO - Processing for F12
    C*====================================================================
    C*
     C     KLDO          begsr
     C                   endsr
    C*
    C*====================================================================
    C* KMDO - Processing for F13
    C*====================================================================
    C*
     C     KMDO          begsr
    C*
     C                   callp     S0006R('W')
     C*
     C                   exsr      getcof
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     OTHFKS        begsr
    C*
     C     *inkj         caseq     *on           kjdo                     90
     C     *inkl         caseq     *on           kldo                     90
     C     *inkm         caseq     *on           kmdo                     90
     C                   endcs
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     PGMEND        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Used for any start of program processing (one-time)
    C*====================================================================
    C*
     C     PGMSTR        begsr
    C*
      ******* beg del ****** 06/10/02 *********************************
     C***********        if        %parms >= 1 and @@prds <> *blanks
     C***********        eval      scprds = @@prds
     C***********        else
     C***********        eval      scprds = optnm
     C***********        endif
      ******* end del ****** 06/10/02 *********************************
      ******* beg add ****** 06/10/02 *********************************
     C                   if        %parms >= 1 and @@prds <> *blanks
     c                   eval      xxtmp = %trim(@@prds)
     C                   movel(p)  xxtmp         scprds
     C                   else
     C                   movel(p)  optnm         scprds
     C                   endif
      ******* end add ****** 06/10/02 *********************************
     C*
      ******* beg del ******* 05/28/14 ********************************
      ******* BEG ADD ****** 04/29/03 *********************************
     c*******            eval      grpflg = UsrRstFlg(q1user)
      ******* BEG ADD ****** 06/26/07 *********************************
     c*******            select
     c*****grpflg        wheneq    'M'
     c*****              Eval      *in46 = '1'
     c*****grpflg        wheneq    'S'
     c*******            Eval      *in46 = '1'
     c*****grpflg        wheneq    ' '
     c*******            Eval      *in44 = '1'
     c*****grpflg        wheneq    'N'
     c*******            Eval      *in44 = '1'
     c*****grpflg        wheneq    'Y'
     c*******            Eval      *in44 = '1'
      ******* BEG ADD ****** 06/26/07 *********************************
      ******* BEG CHG ****** 06/26/07 *********************************
     c****************   if        grpflg = 'Y'
      ******* END CHG ****** 06/26/07 *********************************
     c*******            eval      usrGrp = UserGrp(q1user)
     c*******            eval      ScFnd_to = usrGrp
     c*******            eval      ScFnd_fr = usrGrp
     c*******            seton                                            69
      ******* BEG CHG ****** 06/26/07 *********************************
     c************       endif
     c*******            endsl
      ******* END CHG ****** 06/26/07 *********************************
      ******* END ADD ****** 04/29/03 *********************************
      ******* end del ******* 05/28/14 ********************************
     C*
     C                   eval      scprds = centerFld(scprds)
    C*
     C                   clear                   xxcall
    C*
     C                   if        %parms >= 2
     C                   eval      xxcall = @@call
     C                   endif
    C*
     C                   clear                   svEmp#
     C                   clear                   svDiv
    C*
     C*** beg add *** 09/07/04 *********************************
     C                   eval      scarpt = 'N'
     C*** end add *** 09/07/04 *********************************
     C*** beg add ***** 12/07/04 *******************************
     C                   read      swcpf
     C                   if        WCFL23 = 'Y' or
     C                             WCFL24 = 'Y'
     C                   seton                                        51
     C                   else
     C                   setoff                                       51
     C                   endif
     C*** end add ***** 12/07/04 *******************************
     C*** beg add ***** 12/21/12 *******************************
     C                   eval      scempnoact = 'N'
     C*** end add ***** 12/21/12 *******************************
      /free
       //****** beg add ********** 05/28/14 ********************
       dspMnthEnd = *off;
       CheckLib('PDSMTH':'*USR':'1':@@flag);

       If @@flag = '1';
          dspMnthEnd = *on;
       Endif;
       //****** end add ********** 05/28/14 ********************
      /end-free

     C                   endsr
    C*====================================================================
    C* PREDSP - Pre-processing before displaying SCR01 (every time)
    C*====================================================================
    C*
     C     PREDSP        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP_2 - Pre-processing before displaying SCR02 (every time)
    C*====================================================================
    C*
     C     PREDSP_2      begsr
     C                   endsr
    C*
    C*====================================================================
    C* PREDSP_3 - Pre-processing before displaying SCR03 (every time)
    C*====================================================================
    C*
     C     PREDSP_3      begsr
     C*
     C*** beg add ****** 05/16/01 *****************************************************************
     C                   callp     SA030R(##sufx)
     C                   if        ##sufx = *blanks
     C                   eval      xxrptl = 'PDSRPT' + ', if selected.'
     C                   else
     C                   eval      xxrptl = 'PDSRPT' + ##sufx + ', if selected.'
     C                   endif
     C                   movel     xxrptl        scrptl
     C*** end add ****** 05/16/01 *****************************************************************
     C*** beg add *** 09/07/04 *********************************
     C                   movel     xxrptl        scrpt2
     C*** end add *** 09/07/04 *********************************
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Process_2 = Process SCR02
    C*====================================================================
    C*
     C     process_2     begsr
    C*
B01  C                   dou       *inkc = *on or *inkl = *on
     C                              or *in90 = *off
    C*
     C                   exsr      predsp_2
    C*
     C                   exfmt     scr02
    C*
     C                   movea     '0000000000'  *in(21)
     C                   movea     '0000000000'  *in(31)
     C                   movea     '0000000000'  *in(41)
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
    C*
B02  C                   if        *inkc = *off
    C*
     C                   exsr      othfks
    C*
     C  n90              exsr      errck2
    C*
     C                   if        not *in90
     C                   exsr      process_3
     C                   eval      *inkl = *off
     C                   endif
    C*
E02  C                   endif
    C*
E01  C                   enddo
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Process_3 = Process SCR03
    C*====================================================================
    C*
     C     process_3     begsr
    C*
B01  C                   dou       *inkc = *on or *inkl = *on
     C                              or *in90 = *off
    C*
     C                   exsr      predsp_3
    C*
     C                   exfmt     scr03
    C*
     C                   movea     '0000000000'  *in(71)
     C                   movea     '0000000000'  *in(81)
    C*
B02  C                   if        *inkc = *off
    C*
     C                   exsr      othfks
    C*
     C  n90              exsr      errck3
    C*
E02  C                   endif
    C*
E01  C                   enddo
    C*
     C                   endsr
    C*
    C*====================================================================
    C* PRSCTL - Process control after no errors found (Call Submitter)
    C*====================================================================
    C*
     C     PRSCTL        begsr
     C*
     C                   if        not *inkc
     C*
     C                   move      ##co#         #@co#
      ******** beg del ******* 05/28/14 **************************************
     C********           move      scfnd_fr      ##fnd_fr
     C********           move      scfnd_to      ##fnd_to
      ******** end del ******* 05/28/14 **************************************
     C                   move      scyr_fr       ##yr_fr
     C                   move      scyr_to       ##yr_to
     C                   move      scardt_fr     ##ardt_fr
     C                   move      scardt_to     ##ardt_to
     C                   move      scAgt#        ##agt#
     C                   move      scEmp#        ##emp#
    C*
      ******** beg chg ******* 05/28/14 **************************************
    C********           callp     W7380C(#@co#:##fnd_fr:##fnd_to:##Yr_fr:
    C********                       ##Yr_to:##ARdt_fr:##ARdt_to:scEmpNoAct:
    C********                       scSort:##Agt#:##Emp#:scDiv:scCommPlan:
    C********                       scctyp_all:scARcodes:scarcd_all:
     C*** beg chg *** 09/07/04 *********************************
    C************                   scReport:scOutput:GetUsrOutq(q1user))
    C********                       scReport:scOutput:GetUsrOutq(q1user):
      *** beg chg ***** 12/07/04 ***********************************************
    C************                   scarpt)
    C********                       scarpt:scbilltyp)
      *** end chg ***** 12/07/04 ***********************************************
     C*** end chg *** 09/07/04 *********************************
      /free
       ##fnd_fr = '000';
       ##fnd_to = '000';

       W7380C(#@co#:##fnd_fr:##fnd_to:##Yr_fr:##Yr_to:##ARdt_fr:##ARdt_to:
        scEmpNoAct:scSort:##Agt#:##Emp#:scDiv:scCommPlan:scctyp_all:scARcodes:
        scarcd_all:scReport:scOutput:GetUsrOutq(q1user):scarpt:scbilltyp:
        scMnthEnd);
      /end-free
      ******** end chg ******* 05/28/14 **************************************
     C*
     C                   endif
    C*
     C                   endsr
    C*





** srchCtgrys
SCCTYP_1
SCCTYP_2
SCCTYP_3
SCCTYP_4
SCCTYP_5
SCCTYP_6
SCCTYP_7
SCCTYP_8
SCCTYP_9
SCCTYP_10
** srchCodes
SCARCD_1
SCARCD_2
SCARCD_3
SCARCD_4
SCARCD_5
SCARCD_6
SCARCD_7
SCARCD_8
SCARCD_9
SCARCD_10
SCARCD_11
SCARCD_12
SCARCD_13
SCARCD_14
SCARCD_15
SCARCD_16
SCARCD_17
SCARCD_18
SCARCD_19
SCARCD_20
SCARCD_21
SCARCD_22
SCARCD_23
SCARCD_24
SCARCD_25
SCARCD_26
SCARCD_27
SCARCD_28
SCARCD_29
SCARCD_30
