     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
    H/TITLE W7169R1 - REPORT - Auto CX confirm
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H BNDDIR('WTBNDDIR')
    H*****************************************************************
    H*                                                               *
    H* CALLED FROM: W7169C1 - OVERRIDE - Auto CX confirmation        *
    H*                                                               *
    H* CALLS TO:    W0170C  - SUBMIT - CX confirmation by fund       *
    H*                                                               *
    H* CREATED: 09/15/00  C:61  PROJECT  109BM                       *
    H*****************************************************************
    H* REVISIONS:                                                    *
     F*                                                               *
     F* 06/25/21 C:079 Project: WC2671 from 99/96415                  *
     F*                Add parms to W0170C call for Audit Request     *
     F*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/14/14 C:242 Support Seq# 81271                             *
     H*                Change the loop reading through wtcrl so the   *
     H*                latest Cx Reason record is read first, all Cx  *
     H*                detail records effective as of the Cx Reason   *
     H*                are processed, and then continue reading thru  *
     H*                previous Cx Reason records by effective date.  *
     H*                                                               *
     H* 11/15/13 C:295 WC136 84126                                    *
     H*                Fix the issue with the report for printing     *
     H*                wrong Cancellation date.                       *
     H*                                                               *
     H* 02/04/11 C:061 WA099 75085 WC1992                             *
     H*                Add column to report "Auto Confirm" with value *
     H*                'Yes' or *blank.                               *
     H*                                                               *
     H* 09/27/10 C:246 Support: 045  Seq#: WA045/74193                *
     H*                Fix the issue with the report for not printing *
     H*                the cancellation polcies.                      *
     H*                                                               *
     H* 05/03/10  C:220  PROJECT: WC1881                              *
     H*                  Add Ctrl flag to Final Audit Request Form    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 07/28/09  C:189  PROJECT: 93WIF                               *
     H*                  Change SSN Display on Audit Request          *
     H*                  Added a dummy parameter                      *
     H*                                                               *
     H* 03/12/08 C:33  Project: 71WIQ   Seq#: WA045/62639             *
     H*                Add new parms for CX Confirmations.            *
     H*                                                               *
     H* 09/20/07 C:33  Project: 109WDZ  Seq#: WA045/60034             *
     H*                Notify the confirmation process that this is   *
     H*                 it's being called from Auto Confirmations.    *
     H*                                                               *
     H* 01/03/07 C:33  Seq#: WA099/44610                              *
     H*                Release record lock                            *
    H*                                                               *
    H* 06/24/05 C:33  Project: 109WDZ                                *
    H*               -Copied from client environment.                *
    H*               -Converted to RPGIV.                            *
    H*****************************************************************
     *
     * System Company
     Fsmcol     if   e           k disk
     *
     * User Parameter
     Fsmupl     if   e           k disk
     *
     * System Output Routing
     Fsmorl     if   e           k disk
     *
     * Cancellation Reason Code
     Fwtcrl     if   e           k disk
     *
     * Cancellation Detail
     F***  Begin Chg  ***  01/03/07  **************************************
     F**********wdcxl17   uf   e           k disk
     Fwdcxl17   if   e           k disk    rename(pcx01:pcx17)
     F                                     infds(cxfds17)
     F*
     FWDCXP     uf   e             disk    rename(pcx01:pcx0p)
     F***  End   Chg  ***  01/03/07  **************************************
     *
     **** WDCXL16 Logical File
     F***  Begin Add  ***  09/27/10  **************************************
     Fwdcxl16   if   e           k disk    rename(pcx01:pcx16)
     F***  End   Chg  ***  09/27/10  **************************************
     *
     *  Admin / Darts control
     Fwmexl     if   e           k disk
     *
     *  Cancellation confirmation control
     Fwmcfl     if   e           k disk
     *
     * printed output - Employer Auto CX confirmed
     Fqsysprt   o    f  120        printer oflind(*inoa)
     *
     D***  Begin Add  ***  01/03/07  **************************************
     D cxfds17         ds
     D  cxrrn17              397    400b 0
     D***  End   Add  ***  01/03/07  **************************************
     *
     D fns             s              1    dim(999)
     *
     *================================================================
     * CONSTANTS
     *================================================================
     *
     D xtitl           c                   const('Employer Auto Cancel-
     D                                      Confirmed Report')
     *
     *
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##adtk          s              1
     D ##aurq          s              5
     D ##co#           s              3
     D ##data          s             80
     D ##days          s              5  0
     D ##err           s              1
     D ##fnd           s              3
     D ##grdt          s              8  0
     D ##leng          s              3  0
     D ##mode          s              1
     D ##pchl          s              1
     D ##que           s             10
     D***  Begin Add  ***  03/12/08  **************************************
     D ##AFTY          s              1
     D ##DDUE          s              8
     D***  End   Add  ***  03/12/08  **************************************
     D @@co#           s              3
     D @@date          s              8
     D kkfnd           s                   like(cxco#)
     D kkprt           s              1
     D line            s            120
     D meetpr          s              1
     D n               s              3  0
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D sysdat          s              7  0
     D sysDate         s               d   datfmt(*usa)
     D sysTime         s               t   timfmt(*usa)
     D xxco#           s                   like(cxco#)
     D xxdata          s             60
     D x7date          s              7  0
     D x8date          s              8  0
     D xxgrdt          s              7  0
     D xxkey           s                   like(crkey)
     D xxrsn           s                   like(cxrsn)
     D xxst            s                   like(crst)
     D x1              s              3  0
     D prtCXCXDT       s               d   datfmt(*usa)
     D prtDate         s               d   datfmt(*usa)
     D x2              s              4  0
     D x2co#           s                   like(cxco#)
     D x2cxdt          s                   like(cxcxdt)
     D x2emp#          s                   like(cxemp#)
     D x2fnd           s                   like(cxfnd)
     D x2rsn           s                   like(cxrsn)
     D x2rst           s                   like(cxrst)
     D***  Begin Add  ***  01/28/10  **************************************
     ******* beg add ***** 02/04/11 **********************************
     D xxAutoCF        s              4
     ******* end add ***** 02/04/11 **********************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
      *
     D/copy sprString
     D/copy sprDate
     *
     *================================================================
     *   KEY LISTS
     *================================================================
     *
     *   Output Routing
     *
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    kkprt
     *
     *   Company
     *
     C     coky          klist
     C                   kfld                    xxco#
     *
     *   User Parameter
     *
     C     upky          klist
     C                   kfld                    q1user
     *
     *  Cancellation Reason Code
     *
     C     crky          klist
     C                   kfld                    xxco#
     *
     C     crky1         klist
     C                   kfld                    xxco#
     C                   kfld                    xxst
     C                   kfld                    xxkey
     *
     *  Cancellation Detail
     *
     C     cxky          klist
     C                   kfld                    xxco#
     C                   kfld                    crst
     C                   kfld                    xxrsn
     *
     *  Admin/DARTS control file
     *
     C     exky          klist
     C                   kfld                    xxco#
     C                   kfld                    kkfnd
     *
     *   Cancellation confirmation control
     *
     C     cfky          klist
     C                   kfld                    cxco#
     C                   kfld                    cxfnd
     *
     *================================================================
     *   PARAMETER LISTS INCLUDING ENTRY PARMS
     *================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@date
     *
     *   Initialize
     *
     C                   move      *all'_'       line
     C                   move      *on           meetpr
     *
     *  PGMSTR executes first
     *
     C                   exsr      pgmstr
     *
     *  Print Routing Page
     *
     C                   exsr      srrte
     *
     C                   eval      *inoa = *on
     *
     *  Read CX reason code file
     *
      /free
       // *** Begin Chg  ***  01/14/14  *******************************   ****
       // *** crky          chain     pcr01                              91
       // ***               dow       *in91 = *off
       Setgt (xxco#) pcr01;
       Readpe (xxco#) pcr01;
       Dow not(%eof);
       // *** End   Chg  ***  01/14/14  *******************************   ****
      /end-free
     *
     C                   if        crefdt <= x7date
     C                   if        cracnf = 'Y'
     *
     *  Get Grace period date
     *
      /free
       // *** Begin Chg  ***  01/14/14  ***********************************
       // ***               eval      xxgrdt = DtCYMD(
       // ***                                 DtPast(DtMDYY(sysDat):CRCDAY))
       xxgrdt = DtCYMD(DtPast(DtMDYY(x7date):CRCDAY));
       // *** End   Chg  ***  01/14/14  ***********************************
      /end-free
     *
     * Read CX Detail file
     *
     C                   movel(p)  crkey         xxrsn
     C***  Begin Chg  ***  01/03/07  **************************************
     C*****cxky*         chain     pcx01                              92
     C     cxky          chain(n)  pcx17                              92
     C***  End   Chg  ***  01/03/07  **************************************
     *
     C                   dow       *in92 = *off
     *
      /free
       // *** Begin Chg  ***  01/14/14  ***********************************
       if cxcxdt >= crefdt;
       // *** End   Chg  ***  01/14/14  ***********************************
      /end-free
     C     cfky          chain     pcf01                              60
     *
     * If outside grace period and CX date >= CX start date,
     * auto cancel confirm
     *
     C                   if        cxcxdt <= xxgrdt
     C                             and cxcxdt >= cfcfsd
     C***  Begin Add  ***  01/03/07  **************************************
     C     cxrrn17       chain     wdcxp
     C***  End   Add  ***  01/03/07  **************************************
     *
     * Update Cancellation Detail with CX confirm date
     *
     C                   eval      cxcfdt = x7date
     C                   move      q1user        cxuser
     C                   move      q1pgm         cxpgm
     C                   eval      cxdt   = DateTo7(SystemDate)
     C                   eval      cxtime = TimeTo6(SystemTime)
     C***  Begin Del  ***  11/15/13  **************************************
     C********           eval      prtCXCXDT = CvtToDate(CXCXDT)
     C***  End   Del  ***  11/15/13  **************************************
     C***  Begin Del  ***  09/27/10  **************************************
     C********           if        *inoa = *on
     C********           except    head
     C********           endif
     C********           if        *inoa = *on
     C********           eval      *inoa = *off
     C********           endif
     C********           except    det
     C***  End   Del  ***  09/27/10  **************************************
     C                   eval      *in97 = *on
     *
     C***  Begin Chg  ***  01/03/07  **************************************
     C**********         update    pcx01
     C                   update    pcx0p
     C***  End   Chg  ***  01/03/07  **************************************
     *
     C                   eval      x1 = cxfnd
     C                   move      'Y'           fns(x1)
     C                   endif
     *
      /free
       // *** Begin Chg  ***  01/14/14  ***********************************
       Endif;
       // *** End   Chg  ***  01/14/14  ***********************************
      /end-free
     C***  Begin Chg  ***  01/03/07  **************************************
     C*****cxky*         reade     pcx01                                  92
     C     cxky          reade(n)  pcx17                                  92
     C***  End   Chg  ***  01/03/07  **************************************
     *
     C                   enddo
     *
     C                   endif
     *
     C***  Begin Del  ***  01/14/14  **************************************
     C***                movel(p)  crst          xxst
     C***                movel(p)  crkey         xxkey
     C***  crky1         setgt     pcr01
     C***  End   Del  ***  01/14/14  **************************************
     C                   endif
     *
      /free
       // *** Begin Chg  ***  01/14/14  ***********************************
       // *** crky          reade     pcr01                               91
       // ***               enddo
          Readpe (xxco#) pcr01;
       Enddo;
       // *** End   Chg  ***  01/14/14  ***********************************
      /end-free
     C***  Begin Add  ***  09/27/10  **************************************
      * Print the report
     C     crky          setll     pcx16
     c     crky          reade     pcx16                                  79
     c     *in79         doweq     *off
     c                   if        cxcffl<> 'Y' and
     c                             cxcfdt <> *zeros
     C                   if        *inoa = *on
     C                   except    head
     C                   endif
     C                   if        *inoa = *on
     C                   eval      *inoa = *off
     C                   endif
     C***  Begin Add  ***  11/15/13  **************************************
     C                   eval      prtCXCXDT = CvtToDate(CXCXDT)
     C***  End   Add  ***  11/15/13  **************************************
      ******* beg add ***** 02/04/11 **********************************
      *
     C                   if        cxpgm = 'W7169R1'
     c                   eval      xxAutoCF = 'Yes'
     c                   else
     c                   eval      xxAutoCF = *blanks
     c                   endif
      *
     C                   eval      *in97 = *on
      ******* end add ***** 02/04/11 **********************************
     C                   except    det
     C                   endif
     c     crky          reade     pcx16                                  79
     c                   enddo
     C***  End   Add  ***  09/27/10  **************************************
     *
     *  Process auto confirmations by fund
     *
     C                   eval      x2 = 1
     *
     C                   dow       x2 <= 999
     *
     C                   if        fns(x2) = 'Y'
     C                   move      @@co#         ##co#
     C                   move      x2            ##fnd
     C                   eval      kkfnd = x2
     C     exky          chain     pex01                              60
     C                   if        *in60 = *off
     C                   move      exaurq        ##aurq
     C                   endif
     C                   if        *in60 = *on
     C                   clear                   ##aurq
     C                   endif
     C                   move      oroutq        ##que
     C***  Begin Add  ***  09/20/07  **************************************
     C                   select
     C                   when      ##que = *blanks
     C                   eval      ##que = 'QPRINT'
     C                   endsl
     C***  End   Add  ***  09/20/07  **************************************
     *
     C                   call      'W0170C'
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C***  Begin Chg  ***  09/20/07  **************************************
     C**********         parm      *blank        ##adtk
     C                   parm      '*'           ##adtk
     C***  End   Chg  ***  09/20/07  **************************************
     C                   parm                    ##aurq
     C                   parm      *blank        ##pchl
     C                   parm                    ##que
     C                   parm      'B'           ##mode
     C***  Begin Add  ***  03/12/08  **************************************
     C                   parm      'B'           ##AFTY
     C                   parm      '00000000'    ##DDUE
     C***  End   Add  ***  03/12/08  **************************************
     C*** BEG ADD *** 07/28/09 ***************************************
     C                   parm      *blanks       ##ssf             1
     C*** END ADD *** 07/28/09 ***************************************
     C***  BEG   ADD  ***  05/03/10  **************************************
     C                   parm      cfsrfl        @@srfl            1
     C***  END   ADD  ***  05/03/10  **************************************
     C***  Beg Add *** 06/25/21 *******************************************
     C                   Parm      *blanks       @@G301E           1
     C                   Parm      *blanks       @@G311E           1
     C                   Parm      *blanks       @@GasRd           1
     C***  End Add *** 06/25/21 *******************************************
     *
     C                   endif
     *
     C                   eval      x2 = x2 + 1
     C                   enddo
     *
     *  End of Processing
     *
     C                   if        *inoa = *on
     C                             and *in97 = *off
     C                   except    head
     C                   endif
     C                   if        *in97 = *off
     C                   except    norec
     C                   endif
     *
     C     end           tag
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   move      *on           *inlr
     *
     *
     *
     *================================================================
     * PGMSTR - Set-up and Initialize
     *================================================================
     C     pgmstr        begsr
     *
     C                   eval      sysDate = SystemDate
     C                   eval      sysTime = SystemTime
     *
     C                   eval      sysdat = DateTo7(sysDate)
     *
     C                   move      @@co#         xxco#
     C                   move      @@date        x8Date
     *
     C                   eval      prtDate = CvtToDate(x8Date)
     C                   eval      x7date  = DateTo7(prtDate)
     *
     C                   eval      *in97 = *off
     C                   clear                   fns
     *
     C                   endsr
     *
     *
     *
     *================================================================
     * SRRTE - Print routing page
     *================================================================
     C     srrte         begsr
     *
     *   GET COMPANY, USER, INFO.
     *
     C     coky          chain     pco01                              60
     C     upky          chain     pup01                              60
     *
     *   Output Routing - detail report 0
     *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     c*
     C                   move      '0'           kkprt
     C     orky          chain     por01                              60
     *
     C                   if        *in60 = *off
     *
     * Left Justify Title in Field
     *
     C                   if        ORTITL <> *blanks
     C************       eval      ORTITL = LeftJstify(ORTITL)
     C                   eval      ORTITL = %Trim(ORTITL)
     C                   endif
     *
     *  User Name if no Distribution specified in routing
     *
     C                   if        ordis1 = *blanks
     C                             and ordis2 = *blanks
     C                             and ordis3 = *blanks
     C                             and ordis4 = *blanks
     C                   movel(p)  upusnm        ordis1
     C                   endif
     *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     *
     C                   endif
     *
     C                   if        ORTITL = *blanks
     C                   movel(p)  xtitl         ORTITL
     C                   endif
     *
     *  Center Title in Field
     *
     C                   eval      ORTITL = CenterFld(ORTITL)
     *
     * Center Company name in display field
     *
     C                   eval      CONAME = CenterFld(CONAME)
     *
     C                   endsr
     *
     *================================================================
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     **********
     O**********e            route          1
     O**********                                 15 'ROUTE TO:'
     O**********             ordis1              56
     **********
     O**********e            route          1
     O**********             ordis2              56
     **********
     O**********e            route          1
     O**********             ordis3              56
     **********
     O**********e            route          1
     O**********             ordis4              56
     **********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O                       ordis1              56
     **********
     O          e            route          1
     O                       ordis2              56
     **********
     O          e            route          1
     O                       ordis3              56
     **********
     O          e            route          1
     O                       ordis4              56
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 ************************************
     **********
     O**********e            route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             orpnam              30
     O**********                                 36 'FILE:'
     O**********             orfil#              38
     O**********                                 46 'COPIES:'
     O**********             orcopy        z     50
     O**********                                 56 'HOLD:'
     O**********             orhold              61
     **********
     O**********e            route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             oroutq              30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 ************************************
     *
     O          e            route       2  2
     O                                           24 'REPORT SELECTIONS:'
     *
     O          e            route          2
     O                                           26 'COMPANY . . . . :'
     O                       xxco#         z   +001
     *
     O          e            route          2
     O                                           26 'DATE  . . . . . :'
     O                       prtDate           +001
     *
     O*** Beg Chg *****  01/28/10  ************************************
     O***       e            route          1 60
     O***                                        25 '(C) COPYRIGHT PROGRESSI'
     O***                                      +000 'VE DATA SOLUTIONS. INC.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** End Chg *****  01/28/10  ************************************
     *
     *
     *
     O          e            head           1 03
     O                       sysTime              8
     O                       coname              80
     O                       sysDate            120
     *
     O          e            head           1
     O                       q1pgm               10
     O                       ortitl              90
     O                                          110 'PAGE:'
     O                       page          z    120
     *
     O          e            head        1  1
     O                                         +000 'Rating'
     O                                         +002 'Cancel'
     O                                         +032 'Policy'
     O                                         +004 'Cancel'
     ******* beg add ***** 02/04/11 **********************************
     O                                         +008 'Auto'
     ******* end add ***** 02/04/11 **********************************
     *
     O          e            head        0  0
     O                       line              +000
     *
     O          e            head           2
     O                                         +000 'State'
     O                                         +003 'Reason'
     O                                         +002 'Group'
     O                                         +004 'Employer'
     O                                         +002 'Division'
     O                                         +004 'Year'
     O                                         +006 'Date'
     ******* beg add ***** 02/04/11 **********************************
     O                                         +008 'Confirm'
     ******* end add ***** 02/04/11 **********************************
     *
     O          e            det            1
     O                       cxrst             +002
     O                       cxrsn             +006
     O                       cxfnd         4   +005
     O                       cxemp#        4   +005
     O                       cxdiv         4   +002
     O                       cxfyr         4   +006
     O                       prtCXCXDT         +005
     ******* beg add ***** 02/04/11 **********************************
     O                       xxAutoCF          +006
     ******* end add ***** 02/04/11 **********************************
     *
     *
     O          e            norec       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
     *
     *
