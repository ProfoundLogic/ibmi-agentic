     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H debug(*yes)
    H*********************************************************************
    H/TITLE W0050R17 - TRANSFER/DELETE QUOTE EMPLOYER
    H*********************************************************************
    H*  DESCRIPTION:  THIS PROGRAM WILL ADD, DELETE OR VALIDATE A RECORD *
    H*                IN THE QUOTE TRANSFER FILE.                        *
    H*                                                                   *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W0050R - WORK WITH EMPLOYERS                                   *
    H*    W0050R02 - EMPLOYER MASTER INFORMATION                         *
    H*    W0050C17 - TRANSFER QUOTE EMPLOYERS - OVERRIDE                 *
    H*                                                                   *
    H*  CALLS TO:                                                        *
    H*                                                                   *
    H*                                                                   *
    H*  CREATED: 10/30/97  PROGRAMMER: C:33  PROJECT: WC577              *
    H*********************************************************************
    H* REVISIONS:
     H*
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 02/26/14 C:283 Support: WA099 84926
     H*                SIC Code Required edit for Entity/Location
     H*                when transferring a quote to Live.
     H*
     H* 10/23/12 C:33  Proejct: WC2116
     H*                Account for Tranform
     H*
     H* 12/30/11 C:136 Support: WA133 78726
     H*                Add edit that requires NAIC code to be entered on
     H*                location record if Ratings and Statistics requires
     H*                it for the address on that record.
     H*
     H* 07/28/11  C:255  Project: WC1788 Priss#18
     H*                  Add an edit to check if there is difference in
     H*                  rate of Quote and Live employe.
     A*
     H* 03/07/11  c:220  Support: 74510
     H*                  Add an edit that checks for cancelled policies
     H*                  prior to allowing transfer of QUOTE to LIVE.
     A*
     A* 09/13/10 C:61  Project: sy190 priss 157
     A*                Chain to correct routing record.
     H*
     H* 01/28/10  c:189  Project: SY215
     H*                  Brand Name Change
     H*                                                                   *
     H* 11/16/09  C:198 Project: 132WNC                                   *
     H*                 Field(s) is required when attempting to transfer  *
     H*                 the employer from Quote.                          *
     H*                                                                   *
     H* 09/28/09  C:33  Project: WC1788                                   *
     H*                 Check the agent on the live side vs quote side.   *
     A*                                                                   *
     A* 07/08/09 C:189 Project: 114WUD                                    *
     A*                CHANGE RATES DEFAULT                               *
     H*                                                                   *
     H* 04/28/09  C:191 Project: WC1794                                   *
     H*                 Check the Stat Rpt/Rating file for Risk ID        *
     H*                 requirements information.                         *
     H*                                                                   *
     H* 03/28/08  C:65  Project: CI019                                    *
     H*                 Only allow an online s/r bscd when changing       *
     H*                 the bscd if valid ach info is present.            *
     H*                                                                   *
     H* 03/27/08 C:136 Project: 81WZB                                     *
     H*                Make customer ID a mandatory field for transfer    *
     H*                just like FEIN                                     *
     H*          C:33  Implement into base using CLTID                    *
     H*                                                                   *
     H* 07/12/07 C:189 PROJECT: WC1416                                    *
     H*               ALLOW THE EDITS TO BE ACTIVE FOR THE DIVISIONS      *
     H*               WHICH HAS BEEN ADDED IN QUOTE ANEWEL QUOTE          *
     H*               FROM MSRD WHICH DO NOT EXIST IN LIVE.               *
     H*                                                                   *
     H*                                                                   *
     H* 01/25/07  C:33   Project: WC1299 PRISS #11                        *
     H*                  Change report headings.                          *
     H*                                                                   *
     H* 12/12/06  C:33   Project: WC1299                                  *
     H*                  Add processing for default agent rate.           *
     H*                                                                   *
     H* 07/18/05  C:33   Project: 132WBE   WA132/48113                    *
     H*                  Use correct field to edit "NCCI State Risk ID"   *
     H*                                                                   *
     H* 06/23/05  C:33   Project: WC1017   WA099/47207                    *
     H*                 -Do not allow roll to live if live employer has   *
     H*                   been cancelled.                                 *
     H*                 -Implement edits for NAIC. (WC1287)               *
     H*                                                                   *
     H* 11/16/04  C:33   Project: WC1017   WA093/44570                    *
     H*                  Load QOTEMP for renewals.                        *
     H*                                                                   *
     H* 10/07/04 C:132 Project: 132WBE                                    *
     H*                Add edits for State Risk ID#, NCCI Risk ID# if     *
     H*                flags in WMEXP file are set to include these.      *
     H*                                                                   *
     H* 06/03/04 C:136 Project: WC1208                                    *
     H*                Add option to transfer all divisions at once.      *
     H*                                                                   *
     H* 01/23/04  C:33   Project: WC1017   WA072/40986                    *
     H*                  Do not use external procedure to get agent #.    *
     H*                                                                   *
     H* 10/10/03  C:33   Project: WC1017                                  *
     H*                  Update for use by mass transfer.                 *
    H*                                                                   *
    H* 12/19/02  C:132  PROJECT: WC577   Support: WA109 Seq: 35221       *
    H*                  Added Check on Agent producer cancellation date  *
    H*                  against system date, Error if <= system date     *
    H*                                                                   *
    H* 03/07/02  C:132  PROJECT: WC1071                                  *
    H*                  Added Check on Agent producer cancellation date  *
    H*                  against system date, Error if <= system date     *
    H*                                                                   *
    H* 10/11/01  C:35   PROJECT: WC577                                   *
    H*                  Changed @@err to 1 character field instead of an *
    H*                  indicator so that program would compile under    *
    H*                  new version.                                     *
    H*                                                                   *
    H*  02/01/01  C:33  PROJECT: WC919                                   *
    H*                  VERIFY REGION ENTERED.                           *
    H*                                                                   *
    H*  01/18/01  C:33  PROJECT: WC919   SEQ#: WA109/25844               *
    H*                  DO NOT CHECK INACTIVE AGENT HISTORY RECORDS.     *
    H*                                                                   *
    H*  02/09/00  C:33  PROJECT: WC919                                   *
    H*                 -CONVERT PROGRAM TO RPGIV AND REMOVE COMMENTS.    *
    H*                 -ADD SUBFILE PROCESSING FOR VERIFING EMPLOYER INFO*
    H*                  PRIOR TO TRANSFER.                               *
    H*                                                                   *
    H*  03/07/98  C:33  PROJECT: WC577    WA065/10506                    *
    H*                  IF EMPLOYER #'S ARE NOT AUTO ASSIGNED, NEED      *
    H*                  TO PROMPT FOR ENTRY OF EMPLOYER #.               *
    H*********************************************************************
      /IF DEFINED (WEB)
     H*DFTACTGRP(*NO)
      /ENDIF
     Fw0050f17  cf   e             workstn infds(fids01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     usropn
     F                                     sfile(scr01s:sf_rrn_1)
     Fsmcol     if   e           k disk    rename(pco01:pco00)
     Fwaqop     o  a e             disk    rename(pqo01:dbrcdf)
     Fwaqol     uf   e           k disk
      * Logical to check for assigned Emp# in quote transfer file
     Fwaqol1    If   e           k disk    rename(pqo01:pqo02) prefix(QO2:2)
      *
    F* WMEML will be overrridden in W0050C17 to reference live file
    F*       to validate that entered employer #'s are not already
    F*       assigned.
     Fwmeml     if   e           k disk    rename(pem01:pem00)  prefix(Live)
     Fwmemlq    if   e           k disk    rename(pem01:pem00q)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwmall     if   e           k disk    rename(pal01:pal00)
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwmahl     if   e           k disk    rename(pah01:pah00)
     Fwmahlq    if   e           k disk    rename(pah01:pah00q) usropn
     Fwmaal1    if   e           k disk
     Fwmapl     if   e           k disk    rename(pap01:pap00)
     Fwtabl     if   e           k disk
     Fwtgtl     if   e           k disk
     Fwdrrl1    if   e           k disk
     Fwmgcl     if   e           k disk    rename(pgc01:pgc00)
     Fwaqtl7    if   e           k disk    rename(pqt01:pqt07)
     Fwmupl     if   e           k disk    rename(pup01:pup00)
     Fsmorl     if   e           k disk    rename(por01:por00) usropn
     Fqsysprt   o    f  132        printer oflind(*inOF)       usropn           Output
     FWMEML3    IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:DBRCDF3C)
    D*
     D psds          esds                  extname(szq1p)
     D fids01        e ds                  extname(szq2p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D chg_pgm         c                   'SA020R'
     D pgm_title       c                   'Required Employer Elements'
     D pds_name        S             40    Inz
     D Title           c                   'Mass Transfer Exception Report'
     D xfer            c                   const('will be transferred')
     D xemp1           c                   const('Employer')
     D xemp2           c                   const('from quote employer')
     D address         c                   const('COMPLETE mailing or -
     D                                     physical address not entered.')
     D county          c                   const('County code not entered.')
     D region          c                   const('Region number not entered.')
     D sic_code        c                   const('SIC code not entered.')
     D fed_id_#        c                   const('Federal id number not -
     D                                     entered.')
     D customerID      c                   const('Customer ID not entered')
     D ach_online      c                   const('No valid ACH setup for -
     D                                     online S/R processing.')
     D type_busi       c                   const('Type of business not -
     D                                     valid for ')
     D bill_sched      c                   const('Billing schedule code -
     D                                     not valid for year ')
     D agt_rate        c                   const('Agent rate not entered -
     D                                     for year ')
     D producer        c                   const('Producer not valid for -
     D                                     year ')
     D quoteType       c                   'Agent Quote record has invalid -
     D                                     type or was not found.'
     D cancelled       c                   'Employer has been cancelled in -
     D                                     the LIVE environment.'
     D prod_nmbr       c                   const('Producer ')
     D prod_canc       c                   const('is cancelled, as of date ')
     D State_Risk      c                   'State Risk ID# is required and -
     D                                     is missing from Emp#.'
     D NCCI_Risk       c                   'NCCI Risk ID # is required and -
     D                                     is missing from Emp#.'
     D TZEROS          c                   const('000000000')
     D XXRISK          s              9    inz
      *
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D sf_rrn_1        s              4  0
     D ##data          s             80
     D ##leng          s              3  0
     D @@co#           s                   like(qoco#)
     D @@div           s                   like(qodiv)
     D @@emp#          s                   like(qoemp#)
     D @@err           s              1
     D @@fnd           s                   like(qofnd)
     D @@mode          s              1
     D empAgt#         s                   like(QTAGT#)
     D empRT#          s                   like(AHRT#)
     D prtEmpNm        s             32
     D prtEmpFlg       s                   like(*in60)
     D prtOpened       s                   like(*in60)
     D prtTitle        s                   like(ORTITL)
     D xsysdt          s                   like(apcxdt)
     D xxed            s             15
     D xxedn           s             80
     D zzdiv           s                   like(emdiv)
     D zzemp#          s                   like(ememp#)
     D zzname          s                   like(emname)
     D xxkey           s                   like(BSKEY)
     D xxtbid          s                   like(GTTBID)
     D xxnum           s             80
     D xxType          s              1
     D xxQte           s                   like(QTSTAT) inz('Q')
     D xxQue           s                   like(UPOUTQ)
     D xxPrt           s                   like(ORFIL#)
     D xxPgm           s                   like(ORpnam)
     D xxRRST          s                   like(RRST)
     D xxerror         s                   like(@@err)
     D edit_mask       s             10
     D usa_date_n      s             10
     D usa_date        s               d   datfmt(*usa)
     D lib             s             10
     D modLib          s             10
     D liveAHAGT#      s                   like(AHAGT#)
     D liveahrt#       s                   like(AHRT#)
     D liveAHEFDT      s                   like(AHEFDT)
     D currAHEFDT      s                   like(AHEFDT)
     D openWMAHLQ      s              1n
     D errAgt#         s              1n
     D cmd             s            255
     D f1              s              3  0
     D i1              s              3  0
     D strPopMsg       s            256
     D isMassRun       s              1n
     D isAllDiv        s              1n
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D QCA             S             55    DIM(3) CTDATA PERRCD(1)              CMD EXEC/QCAEXEC
     D GETLIB          C                   CONST('RTVQTELIB')
    D/copy sPrRunCmd
    C/copy sPrString
    C/copy sPrGenPop
    C/copy wPrAgent
    C/copy wPrEmplr
     DCheckAllDiv      pr             1n
     DConfirmTransfer  pr             1n
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)
     DCxDate           PR             7  0
     DGetAgtDftRt      pr                  like(AADRT#)
     DMassRun          pr             1n
     D*
     DCheckCallStack   pr                  extPgm('SRCHKPGM')
     D pgmInStack                    10    const
     D inStack                        1n
     D*
     DRtvEnvLib        pr                  extPgm('RTVENVLIB')
     D requestType                   10    const
     D libName                       10
     D modLibName                    10
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
    C*
    C/copy scopydtpl
    C*
    C*  QUOTE TRANSFER FILE
    C*
     C     qoky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    @@emp#
     C                   kfld                    @@div
    C*
    C*  QUOTE TRANSFER FILE - Logical for existing Assigned Emp#
     C     qoky1         klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    sctemp
     C                   kfld                    @@div
     C*
     C     keyWAQOL      klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C*
     C     keySMORL      klist
     C                   kfld                    xxpgm
     C                   kfld                    xxPrt
     C*
     C     keyWMUPL      klist
     C                   kfld                    q1user
     C*
     C     keyWAQTL7     klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C                   kfld                    xxQte
     C                   kfld                    empAgt#
     C*
     C     key1WAQTL7    klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C                   kfld                    xxQte
     C     MPKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    EMFND
     c*
    C*  EMPLOYER MASTER FILE - LIVE
    C*
     C     qemky         klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    @@emp#
    C*
     C     emky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    sctemp
    C*
     C     emky_1        klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    @@emp#
     C                   kfld                    @@div
    C*
    C*  GROUP MASTER FILE
    C*
     C     fnky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
    C*
     C     fnky_1        klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
    C*
     C     exky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
    C*
     C     alky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
    C*
     C     elky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
    C*
     C     ahky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
     C                   kfld                    ELFYR
    C*
     C     ahky_2        klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    EMEMP#
     C                   kfld                    EMDIV
    C*
     C     aaky          klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    AHAGT#
    C*
     C     apky          klist
     C                   kfld                    EMCO#
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C                   kfld                    AHPROD
    C*
     C     keyWDRRL1     klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    xxRRST
    C*
     C     keyWMGCL      klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
    C*
     C     tbky          klist
     C                   kfld                    EMCO#
     C                   kfld                    xxkey
    C*
     C     gtky          klist
     C                   kfld                    EMCO#
     C                   kfld                    xxtbid
     C                   kfld                    xxkey
     C*____________________________________________________________________
    C*
     C     chg_pl        plist
     C                   parm                    xxnum
     C                   parm      *blanks       edit_mask
    C*
     C     LIBPRM        PLIST
     C                   PARM      '*LIV '       RQSTYP            4
     C                   PARM                    LIBNAM           10
    C*
     C     *entry        plist
     C                   parm                    @@mode
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@err
    C*
    C*  NOTE:  @@ERR WILL ONLY NEED TO BE PASSED IF @@MODE IS 'V' or 'M'
     C*     @@MODE:
     C*       'V' - Validate a single division
     C*       'W' - Validate all divisions
     C*       'M' - Validate mass transfer
     C*       'E' - Delete transfer request
     C*       'T' - Add transfer request for single division
     C*       'X' - Add transfer request for all divisions
     C*       '*' - Called from 'Transform' for validation and transfer
    C*
    C*_______________________________________________________________
    C*
     C                   call      'RTVCLTID'
     C                   parm                    cltID             3
    C*
     C                   time                    dsdttm
     C                   eval      prtEmpFlg = *on
     C                   clear                   ELFYR
     C*
      /Free
       //Callp PopOneBtn('Quote Transfer':'Hold!':'Go');
       isMassRun = MassRun;
       Exsr CheckMode;
      /End-free
    C*
     C                   if        @@mode <> 'M'
     C                   open      W0050F17
     C                   endif

     C* Override to live agent history file
     C                   callp     RtvEnvLib('*OTHER':lib:modLib)
     C                   eval      cmd = 'OVRDBF FILE(WMAHLQ) TOFILE('
     C                                 + %trim(lib) + '/WMAHLQ)'
     C                   callp     RunCmd(cmd:%len(cmd))
     C                   if        openWMAHLQ = *off
     C                   open      WMAHLQ
     C                   eval      openWMAHLQ = *on
     C                   endif
    C*
    C*  MAIN LINE PROGRAM
    C*
     C     @@mode        caseq     'E'           delete
     C     @@mode        caseq     'T'           trnsfr
     C     @@mode        caseq     'V'           valid
     C     @@mode        caseq     'M'           valid
     C                   endcs
      *
      /Free
       If @@mode = '*' and not isAllDiv;
          Exsr Valid;
          If xxError = *off and ConfirmTransfer(@@emp#:@@div);
             Exsr Trnsfr;
          Endif;
       Endif;

       // Transfer all divisions
       If @@mode = 'W'
        or @@mode = 'X'
        or @@mode = '*' and isAllDiv;
          Exsr ProcessAllDiv;
       Endif;
      /End-free
    
     C                   if        @@mode <> 'M'
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If PrtOpened = *on;
          xxPrt = '0';
          Chain keySMORL smorl;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-Free
     C                   move      *on           *inlr
     C                   endif
    C*
     C                   return
    C*_______________________________________________________________

      /Free
      //==================================================================
      // CheckMode - Change mode, if required
      //==================================================================
       Begsr CheckMode;

       Select;
       When @@mode = '*' and DivCount(@@co#:@@fnd:@@emp#) > 1;
          isAllDiv = CheckAllDiv;
       Endsl;

       Endsr;
      /End-free
     C*
    C*====================================================================
    C* ChkAgtDftRt - Check Agent Default Rate processing
    C*====================================================================
     C     ChkAgtDftRt   begsr
     C*
     C                   if        not isMassRun
     C                   exsr      get_record
     C                   exsr      chkWAQTP
     C                   endif
     C*
     C                   eval      agtDftRt = GetAgtDftRt
     C*
     C                   if        EXAAFL = 'Y' and empAgt# <> 0
     C                              and agtDftRt <> *zero
     C                              and agtDftRt <> currRt#
     C                   select
     C                   when      not isMassRun
     C                   dou       not *in71 or *inkl
     C                   exfmt     AgtRtWdw
     C                   eval      *in71 =
     C                              (scRaFl <> 'Y' and scRaFl <> *blanks)
     C                   enddo
     C                   if        *inkl
     C                   eval      @@err = 'L'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C                   eval      QORAFL = scRaFl
     C                   when      isMassRun
     C                   eval      scData = 'Quote rate ' +
     C                                      %trim(%editc(currRt#:'Z')) +
     C                                      ' will be reset to default' +
     C                                      ' rate ' +
     C                                      %trim(%editc(agtDftRt:'Z'))
     C                   exsr      error
     C                   eval      QORAFL = 'Y'
     C                   endsl
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* ChxCx - Check cancellation status of live employer
    C*====================================================================
     C     ChkCx         begsr
     C*
     C                   if        CxDate <> 0
     C                   eval      scData = cancelled
     C                   eval      xxError = *on
     C                   exsr      error
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* chkWAQTP - Check quote status of employer
    C*====================================================================
     C     chkWAQTP      begsr
     C*
     C* Get live agent
     C                   clear                   liveAHAGT#
     C                   clear                   liveAHRT#
     C                   clear                   liveAHEFDT
     C     ahky_2        setgt     pah00q
     C                   dou       %eof or AHSTAT <> 'I'
     C     ahky_2        readpe    pah00q
     C                   enddo
     C*
     C                   select
     C                   when      not %eof and AHAGRC <> *zero
     C                   eval      liveAHAGT# = AHAGRC
     C                   eval      liveAHRT# = AHRTRC
     C                   eval      liveAHEFDT = AHEFDT
     C                   when      not %eof
     C                   eval      liveAHAGT# = AHAGT#
     C                   eval      liveAHRT# = AHRT#
     C                   eval      liveAHEFDT = AHEFDT
     C                   endsl
     C*
     C                   eval      empAgt# = *zero
     C                   eval      empRT# = *zero
     C                   eval      currRt# = *zero
     C                   clear                   currAHEFDT
     C*
     C     ahky_2        setgt     pah00
     C*
     C                   dou       %eof or AHSTAT <> 'I'
     C     ahky_2        readpe    pah00
     C                   enddo
     C*
     C                   if        not %eof
     C                   eval      empAgt# = AHAGT#
     C                   eval      empRT#   = AHRT#
     C                   eval      currRt# = AHRT#
     C                   eval      currAHEFDT = AHEFDT
     C                   endif
     C*
     C     keyWAQTL7     setgt     waqtl7
     C     key1WAQTL7    readpe    waqtl7
     C*
     C                   eval      xxType = upper(qttype)
     C*
     C                   select
     C                   when      %eof or QTAGT# <> empAgt#
     C                   eval      xxTYPE = *blanks
     C                   when      xxTYPE = 'T' and QTXFER <> *blanks
     C                   eval      xxTYPE = upper(QTXFER)
     C                   endsl
     C*
     C                   if            xxTYPE <> 'P'
     C                             and xxTYPE <> 'Q'
     C                             and xxTYPE <> 'B'
     C                             and xxTYPE <> 'M'
     C                             and xxTYPE <> 'F'
     C                             and xxTYPE <> 'G'
     C                             and xxTYPE <> 'U'
     C                   eval      scdata = quoteType
     C                   eval      xxerror = *on
     C                   exsr      error
     C                   endif
     C*
     C                   endsr
    C*
    C*====================================================================
    C* CHK_EMPLR - Check employer for complete entries
    C*====================================================================
    C*
     C     chk_emplr     begsr
    C*
     C                   exsr      get_record
    C*
     C                   if        @@mode <> 'M'
     C                   exsr      clr_sfl
     C                   endif
    C*
     C                   exsr      load_sfl
    C*
     C                   if        xxerror = *on
     C                   if         @@mode <> 'M'
     C                   eval      scrrn1 = 1
     C                   write     scr01
     C                   exfmt     scr01c
     C                   endif
     C                   eval      @@err = '2'
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* CHK_WDELP - Check WDELP records for complete entries
    C*====================================================================
    C*
     C     chk_WDELP     begsr
     C*
     C     elky          chain     pel00                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      prtEmpFlg = *on
     C*
     C                   movel(p)  ELBSCD        xxkey
     C*
     C     tbky          chain     pbs01                              99
     C*
     C                   if        *in99 = *on
     C                   movel(p)  ELFYR         xxnum
     C                   call      chg_pgm       chg_pl
     C                   eval      scdata = bill_sched + %trim(xxnum) + '.'
     C                   exsr      error
     c                   else
     C*
     c                   if        bsonli = 'Y'
     C*
     c                   move      elco#         q3co#
     c                   move      elfnd         q3fnd
     c                   move      elemp#        q3emp#
     c                   move      eldiv         q3div
     c                   move      elfyr         q3fyr
     C                   z-add     elpfdt        srdti
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         q3efdt
     c                   move      '1'           q3mode
     c                   move      *blank        q3err
     c                   move      *blanks       q3data
     c*
     c                   call      'SRONLI'
     C                   PARM                    Q3CO#             3
     C                   PARM                    Q3FND             3
     C                   PARM                    Q3EMP#            9
     C                   PARM                    Q3DIV             5
     C                   PARM                    Q3FYR             3
     C                   PARM                    Q3EFDT            8
     C                   PARM                    Q3MODE            1
     C                   PARM                    Q3ERR             1
     C                   PARM                    Q3DATA          100
     c*
     c                   if        q3err <> '0'
     C                   eval      scdata = ach_online
     C                   exsr      error
     c                   endif
     c*
     c                   endif
     C                   endif
     C*
     C                   if        exerw1 = 'Y' and elffd1 = ' '
     C                   eval      scdata = exehd1 + ' not entered'
     C                   exsr      error
     C                   endif
     C*
     C                   if        exerw2 = 'Y' and elffd2 = ' '
     C                   eval      scdata = exehd2 + ' not entered'
     C                   exsr      error
     C                   endif
     C                   exsr      chk_WMAHP
     C*
     C     elky          reade     pel00                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* CHK_WMAHP - Check WMAHP records for complete entries
    C*====================================================================
    C*
     C     chk_WMAHP     begsr
    C*
     C                   eval      errAgt# = *off
     C*
     C     ahky          chain     pah00                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        AHSTAT <> 'I'
     C* Check for difference between test and live agent
     C                   select
     C                   when      liveAHAGT# <> *zero
     C                              and liveAHAGT# <> empAgt#
     C                              and liveAHEFDT <= currAHEFDT
     C                              and not errAgt#
     C                   eval      scData = 'Quote agent '
     C                                    + %trim(%editc(empAgt#:'Z'))
     C                                    + ' is different from Live agent '
     C                                    + %trim(%editc(liveAHAGT#:'Z'))
     C                                    + '.'
     C                   eval      errAgt# = *on
     C*
     C                   select
     C                   when      not isMassRun
     C                   eval      scAgtWdw1 = CenterFld(scData:%len(scAgtWdw1))
     C                   dou       not *in71 or *inkl
     C                   exfmt     AgtWdw
     C                   eval      *in71 = scUseQte <> 'Y'
     C                                 and scUseQte <> 'N'
     C                   enddo
     C                   if        *inkl
     C                   eval      @@err = 'L'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C                   endsl
     C*
     C                   if        isMassRun or scUseQte = 'N'
     C                   exsr      error
     C                   endif
     C*
     C                   when      liveAHAGT# <> *zero
     C                              and liveAHRT# <> empRT#
     C                              and liveAHEFDT <= currAHEFDT
     C                              and not errAgt#
     C                   eval      scData = 'Quote agent rate '
     C                                    + %trim(%editc(empRT#:'Z'))
     C                               + ' is different from Live agent rate '
     C                                    + %trim(%editc(liveAHRT#:'Z'))
     C                                    + '.'
     C                   eval      errAgt# = *on
     C*
     C                   select
     C                   when      not isMassRun
     C                   eval      scAgtWdw1 = CenterFld(scData:%len(scAgtWdw1))
     C                   dou       not *in71 or *inkl
     C                   exfmt     AgtWdw
     C                   eval      *in71 = scUseQte <> 'Y'
     C                                 and scUseQte <> 'N'
     C                   enddo
     C                   if        *inkl
     C                   eval      @@err = 'L'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C                   endsl
     C*
     C                   if        isMassRun or scUseQte = 'N'
     C                   exsr      error
     C                   endif
     C                   endsl
     C*
     C     *cymd         move      AHEFDT        usa_date
     C                   move      usa_date      usa_date_n
     C*
     C                   if        AHRT# = *zero
     C                   movel(p)  ELFYR         xxnum
     C                   call      chg_pgm       chg_pl
     C                   eval      scdata = agt_rate + %trim(xxnum) + ','
     C                   movel(p)  AHAGT#        xxnum
     C                   call      chg_pgm       chg_pl
     C                   eval      scdata = %trim(scdata) + ' agent '
     C                             + %trim(xxnum) + ' & effective date '
     C                             + usa_date_n + '.'
     C                   exsr      error
     C                   endif
     C*
     C     aaky          chain     paa01                              99
     C  n99apky          chain     pap00                              99
     C*
     C                   if        *in99 = *on
     C                   movel(p)  ELFYR         xxnum
     C                   call      chg_pgm       chg_pl
     C                   eval      scdata = producer + %trim(xxnum) + ','
     C                   movel(p)  AHAGT#        xxnum
     C                   call      chg_pgm       chg_pl
     C                   eval      scdata = %trim(scdata) + ' agent '
     C                             + %trim(xxnum) + ' & effective date '
     C                             + usa_date_n + '.'
     C                   exsr      error
     C                   endif
      * If found Producer w/Cancel Date, Check it against system date
     C                   if        *in99 = *OFF and APCXDT <> *zeros
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xsysdt
     C     *cymd         move      APCXDT        usa_date
     C                   move      usa_date      usa_date_n
     C                   movel(p)  APPROD        xxnum
     C                   call      chg_pgm       chg_pl
     C                   if        (apcxdt > *zeros) AND (apcxdt <= xsysdt)
     C                   eval      scdata = prod_nmbr + %trim(xxnum) + ' '
     C                             + prod_canc + usa_date_n + '.'
     C                   exsr      error
     C                   endif
     C                   endif
     C*
     C                   endif
     C*
     C     ahky          reade     pah00                                  99
     C*
     C                   enddo
     C*
     C                   endsr
    C*
    C*====================================================================
    C* CHK_WMALP - Check WMALP records for complete entries
    C*====================================================================
     C     chk_WMALP     begsr

     C     alky          chain     pal00                              99
     C                   dow       *in99 = *off

     C                   eval      xxtbid = 'BUSI'
     C                   eval      xxkey  = ALTPOB
     C     gtky          chain     pgt01                              60
     C                   if        ALELTP = 'E' and *in60 = *on
     C                   eval      scdata = type_busi + 'entity ' + ALELID
     C                              + '.'
     C                   exsr      error
     C                   endif
      *
      * Ensure ALNAIC is populated if Ratings and Statistics for state
      * requires it
      *
     c                   select
     c                   when      ALPST <> *blanks
     c                   eval      xxRRST = ALPST
     c                   when      ALMST <> *blanks
     c                   eval      xxRRST = ALMST
     c                   endsl

     c     keyWDRRL1     chain     prr01
     c                   if        %found

     c                   if        RRNAIC = 'Y' and
     c                             ALNAIC = *blanks
     c                   move      ALELID        xxELID           10 0
     c                   eval      scdata = 'NAIC required for location ID ' +
     c                             %trim(%editc(xxELID : 'Z')) + '.'
     c                   exsr      error
     c                   endif

     c                   endif
      *** beg add ***** 02/26/14 *******************************************
     c                   if        ALSIC = *Blanks and
     c                             EXSICR = 'Y'
     c                   eval      scdata = 'SIC Code is required for +
     c                             location ID ' +
     c                             %trim(%editc(xxELID : 'Z')) + '.'
     c                   exsr      error
     c                   endif
      *** end add ***** 02/26/14 *******************************************

     C     alky          reade     pal00                                  99
     C                   enddo

     C                   endsr
    C*====================================================================
    C* CLR_SFL - Clear subfile; initializes relative record fields
    C*====================================================================
    C*
     C     clr_sfl       begsr
    C*
     C                   clear                   sf_rrn_1
     C                   eval      *in03 = *on
     C                   write     scr01c
     C                   eval      *in03 = *off
    C*
     C                   endsr
    C*
    C*====================================================================
    C* DELETE - SUBROUTINE FOR PRIOR TO DELETING RECORD
    C*====================================================================
    C*
     C     delete        begsr
    C*
     C                   dou       *in99 = *on
     C     qoky          delete    pqo01                              99
     C                   enddo                                                  *IN99=*ON
    C*
     C                   endsr                                                  DELETE
    C*
    C*====================================================================
    C* ERRCKW - SUBROUTINE FOR ERROR CHECKING WDWEMP#
    C*====================================================================
    C*
     C     errckw        begsr
    C*
    C*  THE EMPLOYER NUMBER CANNOT BE EQUAL TO ZERO
    C*
     C  n90              if        sctemp <= *zero
     C                   eval      *in20 = *on
     C                   eval      *in90 = *on
     C                   endif                                                  SCTEMP<=*ZERO
    C*
     C                   if        *in90 = *off
     C     emky          chain     pem00                              60
     C                   endif
     C                   if        *in90 = *off
     C                             and *in60 = *off
     C                   eval      *in21 = *on
     C                   eval      *in90 = *on
     C                   endif
    C*
     C                   if        *in90 = *off
     C     qoky1         chain     pqo02                              60
     C                   endif
     C                   if        *in90 = *off
     C                             and *in60 = *off
     C                   eval      *in23 = *on
     C                   eval      *in90 = *on
     C                   endif
     C*
     C                   endsr                                                  ERRCKW
    C*
    C*====================================================================
    C* ERROR - Error with the record
    C*====================================================================
    C*
     C     error         begsr
    C*
     C                   select
     C                   when      @@mode <> 'M'
     C                   eval      sf_rrn_1 = sf_rrn_1 + 1
     C                   write     scr01s
     C                   when      @@mode = 'M'
     C                   exsr      Print
     C                   endsl
     C                   eval      xxerror = *on
    C*
     C                   endsr
    C*
    C*====================================================================
    C* GET_RECORD - Gets the record from the RRN passed in
    C*====================================================================
    C*
     C     get_record    begsr
    C*
     C     emky_1        chain     pem00q                             99
     C  n99EMCO#         chain     pco00                              99
    C*
B01  C                   if        *in99 = *on
     C                   callp     RtvBrnd('I':brandName)
     C*
     C                   eval       ##data   = brandname
     C                   else
     C                   eval      ##data = CONAME
E01  C                   endif
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scconm
    C*
     C                   eval      ##data = pgm_title
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      40            ##leng
    C*
     C                   movel     ##data        scprds
    C*
     C                   call      'WA005R'                             99
     C                   parm      EMEMP#        zzemp#
     C                   parm      EMDIV         zzdiv
     C                   parm      EMNAME        zzname
     C                   parm                    xxedn
     C*
     C                   eval      scedn = xxedn
     C                   eval      scfnd = EMFND
     C*
     C     fnky_1        chain     pfn00                              99
     C                   eval      scfnam = FNNAME
    C*
     C                   clear                   EXFIFG
     C                   clear                   EXSICR
     C     exky          chain     pex00                              60
     C*
     C                   select
     C                   when      EMPST <> *blanks
     C                   eval      xxRRST = EMPST
     C                   other
     C                   eval      xxRRST = EMMST
     C                   endsl
     C*
     C                   clear                   RRNAIC
     C                   clear                   RRURFL
     C     keyWDRRL1     chain     prr01
     C*
     C                   clear                   GCFRCN
     C                   clear                   GCTOCN
     C     keyWMGCL      chain     pgc00
    C*
     C                   endsr
    C*
    C*====================================================================
    C* LOAD_SFL - Gets records for subfile
    C*====================================================================
    C*
     C     load_sfl      begsr
    C*
     C                   eval      *in05 = *off
     C                   eval      *in07 = *off
     C                   eval      xxerror = *off
     C*
     C*  Do not check master level records for accounts that are renewed
     C*
     C                   MOVE      *BLANKS       LIBNAM
     C                   MOVE      *BLANKS       CMDST1
     C*
     C                   CALL      GETLIB        LIBPRM
     C     LIBNAM        IFNE      *BLANKS
     C                   MOVEL(P)  QCA(1)        CMDST1
     C                   CAT(P)    LIBNAM:0      CMDST1
     C                   CAT(P)    QCA(2):0      CMDST1
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDST1           55
     C                   PARM                    LENGTH           15 5
     C                   OPEN      WMEML3
     C*
     C     MPKY1         CHAIN     WMEML3                             70
     C*
     C                   MOVEL(P)  QCA(3)        CMDST1
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    CMDST1           55
     C                   PARM                    LENGTH           15 5
     C                   CLOSE     WMEML3
     C                   ENDIF
     C                   if        *IN70  =  '1'
    C*
     C                   select
     C                   when      EMMPFL = 'P'
     C                              and (EMPAD1 = *blank or EMPCTY = *blank
     C                                or EMPST  = *blank or EMPZIP = *zeros)
     C                   eval      xxerror = *on
     C                   when      EMMPFL = 'M'
     C                              and (EMMAD1 = *blank or EMMCTY = *blank
     C                                or EMMST  = *blank or EMMZIP = *zeros)
     C                   eval      xxerror = *on
     C                   when      EMMPFL <> 'P' and EMMPFL <> 'M'
     C                   eval      xxerror = *on
     C                   endsl
    C*
     C                   if        xxerror = *on
     C                   eval      scdata = address
     C                   exsr      error
     C                   endif
    C*
     C                   if        EMFEIN = *zeros and EXFIFG = 'Y'
     C                   eval      scdata = fed_id_#
     C                   exsr      error
     C                   endif
    C*
     C                   select
     C                   when      cltID = '081'
     C                   if        EMCUST = *blanks and EXFIFG = 'Y'
     C                   eval      scdata = customerID
     C                   exsr      error
     C                   endif
     C                   endsl
    C*
     C                   if        EMSIC = *blanks and EXSICR = 'Y'
     C                   eval      scdata = sic_code
     C                   exsr      error
     C                   endif
      *
      * Check the State Risk ID# and NCCI Risk ID# if flags are set for
      * these to be required.
      *
     C                   movel(p)  emsemp        XXRISK
     C                   If        emsemp = *blanks AND (EXSRRB = 'Y' OR
     C                             RRSRRB= 'Y') OR
     C                             XXRISK = TZEROS AND (EXSRRB = 'Y'  OR
     C                             RRSRRB= 'Y')
     C                   EVAL      SCDATA = State_Risk
     C                   exsr      error
     C                   Endif
      *
     C                   movel(p)  emrsk#        XXRISK
     C                   If        EMRSK# = *zeros and (EXNRRB = 'Y' OR
     C                             RRNRRB='Y') OR
     C                             xxrisk = TZEROS and (EXNRRB = 'Y' OR
     C                             RRNRRB='Y')
     C                   EVAL      SCDATA = NCCI_Risk
     C                   exsr      error
     C                   Endif
    C*
     C                   eval      xxtbid = 'BUSI'
     C                   eval      xxkey  = EMTPOB
     C     gtky          chain     pgt01                              60
     C*
     C                   if        *in60 = *on
     C                   eval      scdata = type_busi + 'employer.'
     C                   exsr      error
     C                   endif
    C*
     C                   if        EMCNTY = *blanks
     C                   eval      scdata = county
     C                   exsr      error
     C                   endif
     C*
     C                   if        EMRGN# = *zeros
     C                   eval      scdata = region
     C                   exsr      error
     C                   endif
     C*
     C                   if        RRURFL = 'Y'
     C                              and EMUE# = *blanks
     C                   eval      scData = 'State unemployment # required +
     C                              for quote transfer.'
     C                   exsr      error
     C                   endif
     C*
     C                   if        RRNAIC = 'Y'
     C                              and EMNAIC = *blanks
     C                   eval      scData = 'The NAIC code is required +
     C                              for quote transfer.'
     C                   exsr      error
     C                   endif
     C*
     C                   if        GCFRCN <> *zero and GCTOCN <> *zero
     C*
     C     keyWAQOL      setll     waqol
     C                   dou       %eof
     C     keyWAQOL      reade(n)  waqol
     C                   eval      GCNACN = GCNACN + 1
     C                   enddo
     C     qoky          chain     waqol
     C*
     C                   if        GCNACN < GCFRCN
     C                              or GCNACN > GCTOCN
     C                   eval      scData = 'The next available carrier # +
     C                              is not within the valid range.'
     C                   exsr      error
     C                   endif
     C*
     C                   endif
     C*
     C                   exsr      chk_WMALP
     C*
     C                   endif
     C*
     C                   exsr      chkWAQTP
     C*
     C                   exsr      chk_WDELP
     C*
     C                   exsr      ChkCx
    C*
     C                   endsr
    C*
    C*====================================================================
    C* Print - Write records for printing the exception report
    C*====================================================================
    C*
     C     Print         begsr
     C*
     C                   if        prtOpened = *off
     C                   open      qsysprt
     C                   exsr      Routing
     C                   eval      prtOpened = *on
     C                   eval      *inOF = *on
     C                   endif
     C*
     C                   if        *inOF = *on
     C                   except    prtHeader
     C                   eval      *inOF = *off
     C                   endif
     C*
     C                   if        prtEmpFlg = *on
     C                   eval      prtEmpFlg = *off
     C                   eval      prtEmpNm = EMNAME
     C                   except    prtEmplr
     C                   endif
     C*
     C                   except    prtDetail
     C*
     C                   endsr
     /Free
      //==================================================================
      // Process all divisions
      //==================================================================
      Begsr ProcessAllDiv;

       Select;
       When @@mode = '*';
          f1 = 2;
       Other;
          f1 = 1;
       Endsl;

       For i1 = 1 to f1;

          If i1 = 2 and not ConfirmTransfer;
             Leavesr;
          Endif;

          Setll qemky pem00q;
          Reade qemky pem00q;

          Dow not %eof;

             @@div = EMdiv;

             Select;
             When @@mode = '*' and i1 = 1;
                Exsr Valid;
                If xxError = *on;
                   Leavesr;
                Endif;
             When @@mode = '*' and i1 = 2;
                Exsr Trnsfr;
             When @@mode = 'W';
                Exsr Valid;
                If @@err = '0' or @@err = '2';
                   *inlr = *on;
                   Return;
                Endif;
             When @@mode = 'X';
                Exsr Trnsfr;
             Endsl;

             Reade qemky pem00q;

          Enddo;

       Endfor;

       Endsr;

      /End-free
    C*
    C*====================================================================
    C* Routing -  Processing routing for QSYSPRT
    C*====================================================================
    C*
     C     Routing       begsr
     C*
     C     keyWMUPL      chain     wmupl
     C*
     C                   if        %found(wmupl)
     C                   eval      xxQue = UPOUTQ
     C                   eval      *in09 = *on
     C                   else
     C                   eval      *in08 = *on
     C                   endif

     C                   open      smorl
     C                   eval      xxPgm = 'W8718R'
     C                   eval      xxPrt = '0'
     C     keySMORL      chain     smorl
     C*
     C                   if        ORTITL = *blanks
     C                   eval      ORTITL = title
     C                   endif
     C*
     C                   eval      ORTITL = leftJstify(ORTITL)
     C*
     C                   if        %found(smorl)
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
     C*
     C                   close     smorl
     C*
     C                   eval      prtTitle = centerFld(ORTITL:%size(prtTitle))
     C*
     C                   endsr
    C*
    C*====================================================================
    C* TRNSFR - SUBROUTINE FOR ADDING THE EMPLOYER TO BE TRANSFERED
    C*====================================================================
    C*
     C     trnsfr        begsr
    C*
     C     fnky          chain     pfn00                              60
     C     emky_1        chain     pem00q
     C*
     C                   exsr      ChkAgtDftRt
     C*
     C                   select
     C*____________________________________________________________________
     C*
     C*  If "Renew to Quote" load the current emp# to the 'Active Emp#'
     C                   when      EMRQFL = 'Y'
     C*
     C                   eval      QOTEMP = @@emp#
    C*
    C*  IF AUTO ASSIGN FLAG IS NOT 'Y', PROMPT FOR EMPLOYER #
    C*
     C                   when      fnaute <> 'Y'
    C*
     C                   eval      scfnd = @@fnd
     C                   eval      scemp# = @@emp#
     C                   eval      scdiv = @@div
     C                   eval      sctdiv = @@div
     C                   clear                   sctemp
    C*
     C                   dou       *in90 = *off
    C*
     C                   move      *off          *in90
    C*
     C                   exfmt     wdwemp#
    C*
     C                   if        *inkl = *off
     C                   exsr      errckw
     C                   endif                                                  *INKL=*OFF
    C*
     C                   enddo
    C*
     C                   if        *inkl = *on
     C                   move      *on           *inlr
     C                   return
     C                   endif                                                  *INKL=*ON
    C*
     C                   call      'WA005R'
     C                   parm      sctemp        zzemp#
     C                   parm      scdiv         zzdiv
     C                   parm                    zzname
     C                   parm                    xxedn
    C*
     C                   movel(p)  xxedn         xxed
    C*
     C     xemp1         cat(p)    xxed:1        scqte1
     C                   cat       xfer:1        scqte1
    C*
     C                   movel(p)  scqte1        ##data
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      50            ##leng
    C*
     C                   movel(p)  ##data        scqte1
    C*
     C                   call      'WA005R'
     C                   parm      scemp#        zzemp#
     C                   parm      scdiv         zzdiv
     C                   parm                    zzname
     C                   parm                    xxedn
    C*
     C                   movel(p)  xxedn         xxed
    C*
     C     xemp2         cat(p)    xxed:1        scqte2
    C*
     C                   movel(p)  scqte2        ##data
    C*
     C                   call      'SRCENTER'
     C                   parm                    ##data
     C                   parm      50            ##leng
    C*
     C                   movel(p)  ##data        scqte2
    C*
     C                   exfmt     qtewdw
    C*
     C                   eval      qotemp = sctemp
    C*
     C                   endsl                                                  FNAUTE<>Y
    C*
     C                   eval      qoco# = @@co#
     C                   eval      qofnd = @@fnd
     C                   eval      qoemp# = @@emp#
     C                   eval      qodiv = @@div
    C*
     C                   movel     q1user        qouser
     C                   movel     q1pgm         qopgm
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         qodt
    C*
     C                   movel     dstime        qotime
    C*
     C                   write     dbrcdf
    C*
     C                   endsr                                                  TRNSFR
    C*
    C*====================================================================
    C* VALID - SUBROUTINE FOR CHECKING IF EMP HAS ALREADY BEEN REQUESTED
    C*====================================================================
    C*
     C     valid         begsr
    C*
     C     qoky          chain(n)  pqo01                              99
     C*
     C                   eval      @@err = *in99
     C*
     C                   if        *in99 = *on
     C                   exsr      chk_emplr
     C                   endif
     C*
     C                   if        xxError = *off and isMassRun
     C                   exsr      ChkAgtDftRt
     C                   endif
    C*
     C                   endsr                                                  VALID
    C*
      /eject
     O/COPY SCOPYROUT
     Oqsysprt   e            prtHeader      1  6
     O                       q1pgm               10
     O                       scCoNm              81
     O                       q1user             132
     O          e            prtHeader      2
     O                       dsDate              10 '  /  /    '
     O                       prtTitle            91
     O                                          126 'Page'
     O                       page          4    132
     O          e            prtHeader      0
     O                                            9 ' EMPLOYER'
     O                                          + 1 '  DIV'
     O                                          + 1 ' YR'
     O                                          + 1 'EMPLOYER NAME'
     O                                           78 'REASONS FOR NON-TRANSFER'
     O                                          +00 '/EXCEPTIONS'
     O          e            prtEmplr    1  0
     O                       EMEMP#        z      9
     O                       EMDIV         z    + 1
     O                       ELFYR         z    + 1
     O                       prtEmpNm           + 1
     O          e            prtDetail      1
     O                       scData             132





    P*====================================================================
    P* Determine if all divisions should be processed
    P*====================================================================
     PCheckAllDiv      b
     DCheckAllDiv      pi             1n

      /Free
       Return PopTwoBtn('Transfer all divisions?':
        'Multiple Divisions Exist':'Yes':'No');
      /End-free

     PCheckAllDiv      e





    P*====================================================================
    P* Confirm transfer
    P*====================================================================
     PConfirmTransfer  b
     DConfirmTransfer  pi             1n
     D inEmp#                         9  0 const options(*nopass)
     D inDiv                          5  0 const options(*nopass)

     D strPopMsg       s            256

      /Free
       Select;
       When %parms >= 1 and inEmp# <> 0;
          strPopMsg = 'Employer ' + %trim(%char(inEmp#)) + ' ' +
           %trim(%char(inDiv));
       Other;
          strPopMsg = 'All divisions';
       Endsl;

       strPopMsg = %trim(strPopMsg) + ' will be transferred during the ' +
        'daily batch process.';

       Return PopTwoBtn(strPopMsg:'Confirm Quote Transfer':'Ok':'Cancel');
      /End-free

     PConfirmTransfer  e





    P*====================================================================
    P* CxDate    - Get cancellation date
    P*====================================================================
    P*
     PCxDate           b                   export
    P*
     DCxDate           PI             7  0

     D cx_date         s              7  0

     C                   clear                   cx_date

     C                   select
     C*
     C                   when      EMEFD5 <> *zero
     C                   eval      cx_date = EMCAD5
     C                   when      EMEFD4 <> *zero
     C                   eval      cx_date = EMCAD4
     C                   when      EMEFD3 <> *zero
     C                   eval      cx_date = EMCAD3
     C                   when      EMEFD2 <> *zero
     C                   eval      cx_date = EMCAD2
     C                   when      EMEFD1 <> *zero
     C                   eval      cx_date = EMCAD1
     C*
     C                   endsl
     C*
     C                   return    cx_date
     C*
     PCxDate           e





    P*====================================================================
    P* GetAgtDftRt - Get the agent default rate
    P*====================================================================
     PGetAgtDftRt      b                   export
     DGetAgtDftRt      pi                  like(AADRT#)

     C     keyWMAAL1     klist
     C                   kfld                    EMCO#
     C                   kfld                    EMFND
     C                   kfld                    empAgt#

     C                   clear                   AADRT#
     C     keyWMAAL1     chain     wmaal1
     C                   return    AADRT#

     PGetAgtDftRt      e





    P*====================================================================
    P* MassRun - Determine if this is a mass run
    P*====================================================================
     PMassRun          b                   export
     DMassRun          pi             1n

     D inStack         s              1n

     C                   callp     CheckCallStack('W8718R':inStack)

     C                   return    inStack

     PMassRun          e

**  QCAEXEC TABLE
OVRDBF FILE(WMEML3) TOFILE(
/WMEML3)
DLTOVR FILE(WMEML3)
