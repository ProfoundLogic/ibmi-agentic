      * (c) 2020 Sapiens. All Rights Reserved.                             *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W7166R - BUILD WORKFILE FOR CANCELLATION LETTER
     H***************************************************************
     H**  PROGRAM DESCRIPTION:
     H**     THIS PROGRAM BUILDS WORKFILE WXCHP WHICH IS USED BY
     H**     OFFICE DOCUMENTS FOR CANCELLATION & REINSTATEMENT
     H**     LETTERS TO CERTIFICATE HOLDERS.
     H**  CALLED FROM:
     H**     W7166C
     H**  CALLS TO:
     H**     NONE
     H**  CREATED: 02/23/95       PROGRAMMER: 11     PROJECT: WC396 *
     H***************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/10/20 C:298 Project: WC2704                                *
     H*                Cancellation Output Configuration - Print      *
     H*                for All Divisions or State Report Only.        *
     H*                                                               *
     H* 02/26/18 C:33  Support: 64588                                 *
     H*                Always check for spooled output.               *
     H*                                                               *
     H* 01/25/17 C:65  Project: SY371                                 *
     H*                Populate extra fields in work file.            *
     H*                                                               *
     H* 12/19/16 C:65  Project: SY371                                 *
     H*                Check WSCRP for "All Groups" and "All States"  *
     H*                records when not finding setup records for     *
     H*                the specific group or state.                   *
     H*                                                               *
     H* 11/04/16 C:136 Project: SY371                                 *
     H*                Disable spool and DTM output of processing     *
     H*                Scriptura only                                 *
     H*                                                               *
     H* 10/06/16 C:65  Project: SY371                                 *
     H*                Call Scriptura api for a template request if   *
     H*                running under Scriptura.                       *
     H*                                                               *
     H* 01/22/13 C:287 Seq#: SY266_OVR                                *
     H*                Added DFTACTGRP and ACTGRP compile options     *
     H*                                                               *
     H* 07/20/10 C:190 SUPPORT: 73448                                 *
     H*                Print the policy services representative's     *
     H*                name (EMPSRV) while printing certificate       *
     H*                holder letter.                                 *
     H*                                                               *
     H* 04/04/09 C:61  PROJECT WC1304                                 *
     H*                Allow for printing "Out of Country"            *
     H*                Certficate Holder address.                     *
     H*                                                               *
     H* 11/15/06 C:61  PROJECT 139WCI                                 *
     H*                Print PEO name if PEO Group.                   *
     H*                                                             *
     H* 03/07/05 C:65 PROJECT: WC1202                               *
     H*               STORE EXTENDED EMPLOYER NAMES FOR REPORTING.  *
     H*                                                             *
     H* 07/01/02  C:35   PROJECT WC1028/SUPPORT WA045 #33245        *
     H*                  PRINT GRP/EMP#/FYR IF NO CARRIER POLICY    *
     H*                  NUMBER EXISTS.                             *
     H*                                                             *
     H* 08/10/01 C:90  PROJECT: WC1028                              *
     H*                Load CRPOL# to determine whether to print    *
     H*                "File #" or "Policy #".                      *
     H*                                                             *
     H* 12/06/99 C:118 SUPPORT: WA048/20840                           *
     H*                Modify the test of the carrier policy number   *
     H*                to see if it is an old-format PDS policy       *
     H*                number.                                        *
     H*                                                               *
     H* 10/01/98  C:87   PROJECT: WC762                             *
     H*                  ADD PROCESSING TO LOAD CHCFFL (CANX        *
     H*                  CONFIRMATION FLAG) JUST ADDED TO WXCHP     *
     H*                                                             *
     H* 08/29/98  C:35   PROJECT: WC862                             *
     H*                  USE CARRIER POLICY NUMBER IN PLACE OF PDS  *
     H*                  POLICY NUMBER IF RECORD EXISTS AND IT IS   *
     H*                  NOT BLANK.  IF CARRIER POLICY NUMBER IS    *
     H*                  NOT USED, LOAD PDS POLICY NUMBER IN PLACE  *
     H*                  OF CARRIER POLICY NUMBER.  IF CARRIER      *
     H*                  POLICY NUMBER IS AN OLD-FORMAT PDS POLICY  *
     H*                  NUMBER, THEN LOAD 0 TO CHEMP#, SO IT DOES  *
     H*                  NOT PRINT ON THE DOCUMENT.                 *
     H*                                                             *
     H***************************************************************
     H**  INDICATOR USAGE:
     H**    INDICATOR   FUNCTION DESCIPTION
     H**       60       FILE CHAIN
     H**
     H***************************************************************
     F*
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDCXL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMCEL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWXCHP     O  A E           K DISK
     FWMUPL1    IF   E           K DISK
      **** BEGIN ADD **** 08/29/98 ************************************
     FWMRPL     IF   E           K DISK
     F***** BEGIN DEL **** 12/06/99 ***********************************
     F********** WDELL   IF  E           K        DISK
     F***** END   DEL **** 12/06/99 ***********************************
      **** END   ADD **** 08/29/98 ************************************
     F***** BEGIN ADD **** 12/06/99 ***********************************
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F***** END   ADD **** 12/06/99 ***********************************
      *
     D*** begin add ***  10/06/16  ****************************************
     F                                     infds(dbfds)
     FWMRSL     IF   E           K DISK
     FWSCRL     IF   E           K DISK    rename(pcr01:pcr00)
     F                                     prefix(ST)
     FWXCHP1    O  A E           K DISK    rename(pch01:pch0p)
     FXMLTEML1  UF   E           K DISK
      ***** Beg Add ***** 02/10/20 ***************************************
     FXMLPREL1  UF   E           K DISK
     FWXC7P     O    E             DISK
     FWTGTL     IF   E           K DISK
     FWXC#L3    IF   E           K DISK
     F                                     RENAME(PCR01:PC#01)
     FWDRRL     IF   E           K DISK    RENAME(PRR01:PRR00)
      ***** End Add ***** 02/10/20 ***************************************
     d*
     D***  Begin Add  ***  02/26/18  *************************************
     DstrProcess       s             50    inz
     D***  End   Add  ***  02/26/18  *************************************
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('CC')
     d templateFile    s                   like(XTXMLT)
     d xxTemplate      s             60
     d xxPath          s            250
     d processTypeC    c                   const('Cancellations')
     d processTypeR    c                   const('Reinstatements')

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x#RRN           s              9  0 inz(0)
     D*
     D svlda           ds          1024    dtaara(*lda)
     D ldOutputFile                  60    Overlay(svlda:191)
      ***** Beg Add ***** 02/10/20 ***************************
     D ldMassFile                    60    Overlay(svlda:129)
      ***** End Add ***** 02/10/20 ***************************
     D*** end add *****  10/06/16  *****************************************
      *** beg add ***** 11/04/16 ***************************
     D isUseSpoolOut   s              1n   inz
      ***** Beg Add ***** 02/10/20 ***************************
     D isUseXML        S              1n   inz
     D isUseScript     S              1n   inz
     D isCanCnf        S              1n   inz
     D isWxc7pWrt      S              1n   inz
     d  masscx         s              1
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1

     d ObjExists       pr             1n
     d  object                       10    const
     d  objType                       7    const

     d S4401C          pr                  ExtPgm('S4401C')
     d   xmlName                     61    const


     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlLink         s                   like(XPLink)
     d xmlFile         s                   like(XPXML)
     d  isFound        s              1
     d isMassReq       s              1n   inz
     d massFlag        s              1
      ***** End Add ***** 02/10/20 ***************************
     D/copy sPrXML
      *** end add ***** 11/04/16 ***************************

     D CHGNUM          C                   CONST('WA003R')
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXEFDT                 1      6  0
     D  YY                     1      2  0
     D  MM                     3      4  0
     D  DD                     5      6  0
      **** BEGIN ADD **** 08/29/98 ************************************
     D                 DS
     D***** BEGIN ADD **** 12/06/99 ***********************************
     D  XXPOL                  1     18
     D***** END   ADD **** 12/06/99 ***********************************
     D  XXOPOL                 1     11
     D  XXOFND                 1      3
     D  XXOEMP                 4      8
     D  XXOFYR                 9     11
     D***** BEGIN ADD **** 12/06/99 ***********************************
     D  XOBLNK                12     18
     D***** END   ADD **** 12/06/99 ***********************************
     D                 DS
     D  XXCPOL                 1     15
     D  XXCFND                 1      3
     D  XXCEMP                 4     12
     D  XXCFYR                13     15
      **** END   ADD **** 08/29/98 ************************************
      **** Beg Add ***** 02/10/20 *************************************
     D  amn                    1    540    dim(9)
     D  ##amn1               001    060
     D  ##amn2               061    120
     D  ##amn3               121    180
     D  ##amn4               181    240
     D  ##amn5               241    300
     D  ##amn6               301    360
     D  ##amn7               361    420
     D  ##amn8               421    480
     D  ##amn9               481    540
      **** End Add ***** 02/10/20 *************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
      ***** Beg Add ***** 02/10/20 ************************************
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C
     C     XCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
      *
     C     XCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
      ***** End Add ***** 02/10/20 ************************************
     C*
     C*  CANCELLATION FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXREQ#
     C*
     C*  CERTIFICATE HOLDERS FILE
     C*
     C     CEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      * KEY LIST FOR THE USER PARAMETERS FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    XXSTAF
      **** BEGIN ADD **** 08/29/98 ************************************
      *
      * KEY LIST FOR THE CARRIER/STATE REPORTING FILE
      *
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** begin chg ***  10/06/16  ************************************
     C*******            KFLD                    XXEMP#
     C*******            KFLD                    XXDIV
     C*******            KFLD                    XXFYR
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*** end chg *****  10/06/16  ************************************
      **** END   ADD **** 08/29/98 ************************************
     C*
     C***** BEGIN ADD **** 12/06/99 ***********************************
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C***** END   ADD **** 12/06/99 ***********************************
     C*
      ***** Beg Add ***** 02/10/20 ************************************
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      ***** End Add ***** 02/10/20 ************************************
     C*
     C     *LIKE         DEFINE    CXCO#         XXCO#
     C     *LIKE         DEFINE    CXFND         XXFND
     C     *LIKE         DEFINE    CXFYR         XXFYR
     C     *LIKE         DEFINE    CXEMP#        XXEMP#
     C     *LIKE         DEFINE    CXDIV         XXDIV
     C     *LIKE         DEFINE    CXREQ#        XXREQ#
     C     *LIKE         DEFINE    UPSTAF        XXSTAF

      ***** Beg Add ***** 02/10/20 ************************************
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
      ***** End Add ***** 02/10/20 ************************************
     C*****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQREQ#            5
     C                   PARM                    QQNREC            1            NO RECORD Y/N
     C*** begin add ***  10/06/16  *********************************************
     C                   PARM                    QQType            2
     C*** end add *****  10/06/16  *********************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      * EDIT MASK API PARAMETER LIST
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      ***** BEG ADD ******* 11/15/06 **********************************
      *
      *  LINE FORMAT
      *
     C     LINEPL        PLIST
     C                   PARM                    ##LINE          256
     C                   PARM                    ##LEN             3 0
     C                   PARM                    ##REM           256
      ***** END ADD ******* 11/15/06 **********************************
      *
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQREQ#        XXREQ#
     C*
     C                   TIME                    DSDTTM
      *
       //***  Begin Add  ***  02/26/18  *******************
      /Free
       Select;
       When qqType = 'RE';
          strProcess = %trim(processTypeR);
       Other;
          strProcess = %trim(processTypeC);
       Endsl;
       isUseSpoolOut = UseSpoolOut(strProcess);
      /End-free
       //***  End   Add  ***  02/26/18  *******************
      ***** Beg Add ***** 02/10/20 ***************************
      /free
       isUseScript = UseScriptura(strProcess);
       isUseXML = UseScriptura(strProcess)
        or Use3rdXml(strProcess);

       Clear xmlFileName;
       Clear xmlLink;

       ChkStack('W0164C':masscx);

       If masscx  <> *on;
          isMassReq = *off;
          massFlag = ' ';
       Else;
          isMassReq = *on;
          massFlag = 'Y';
       Endif;

       ChkStack('W0171C':isFound);

       If isFound <> *on;
          isCanCnf  = *off;
       Else;
          isCanCnf  = *on;
          isMassReq = *on;
          massFlag = 'Y';
       Endif;

      /end-free
      ***** End Add ***** 02/10/20 ***************************
     C*
     C*   GET CANCELLATION DETAIL RECORD
     C*
      ***** Beg Del ***** 02/10/20 ***************************
     C*****CXKY          CHAIN     PCX01                              60
     C******IN60         IFEQ      *ON
     C*****              GOTO      TERM
     C*****              ENDIF                                                  CXKY
      ***** End Del ***** 02/10/20 ***************************
      ***** Beg Add ***** 02/10/20 ***************************
      /Free
       If not isCanCnf;
          Setll emky pem01;
          Reade emky pem01;
       Else;
          Setll emky1 pem01;
          Reade emky1 pem01;
       Endif;
       Dow not %eof(wmeml);
          If EMstrp = 'Y';
             Chain (xxco#:xxfnd:xxfyr:xxemp#:EMdiv:xxreq#) pcx01;
          Else;
             Chain (xxco#:xxfnd:xxfyr:xxemp#:EMdiv) pcx01;
          Endif;
          If %Found(wdcxl);
      ***** End Add ***** 02/10/20 ***************************
     C*** begin add ***  10/06/16  *****************************************
     C     *lock         in        *dtaara
     C                   out       *dtaara
     C                   Clear                   templateFile
      ***** Beg Add ***** 02/10/20 ***************************
      /Free
       isWxc7pWrt = *off;
       xxdiv = EMdiv;
      /End-free
      ***** End Add ***** 02/10/20 ***************************
     C*
     C     elky          Chain     pel02
     C                   eval      x#RRN = dbRRN
     C     rsky          Chain     prs01
     C*** end add *****  10/06/16  *****************************************
     C*
      ***** Beg Add ***** 02/10/20 ***************************
     C                   move      xxco#         #1co#
     C                   move      xxfnd         #1fnd
     C                   move      xxemp#        #1emp#
     C                   move      EMdiv         #1div
     C                   move      ' '           ##odr
     C                   move      'F'           ##nodr
     C                   move      'M'           ##aodr
     C                   clear                   ##emn1
     C                   clear                   ##emn2
     C                   clear                   ##emn3
     C                   clear                   ##emn4
     C                   clear                   ##emn5
     C                   clear                   ##emn6
     C                   clear                   ##emn7
     C                   clear                   ##amn1
     C                   clear                   ##amn2
     C                   clear                   ##amn3
     C                   clear                   ##amn4
     C                   clear                   ##amn5
     C                   clear                   ##emn6
     C                   clear                   ##emn7
     C                   clear                   ##amn1
     C                   clear                   ##amn2
     C                   clear                   ##amn3
     C                   clear                   ##amn4
     C                   clear                   ##amn5
     C                   clear                   ##amn6
     C                   clear                   ##amn7
     C                   clear                   ##amn8
     C                   clear                   ##amn9
     C                   Clear                   QQPEOExt
     C     XCKY          Setgt     PRR00
     C     XCKY2         Readpe(n) PRR00                                  60
     C*
     C                   select
     C                   when      FNPEOF = *blanks
     C                   clear                   ##pgfl
     C                   when      rrst <> '  '
     C                   eval      ##pgfl = RRPGFL
     c                   if        ##pgfl = *blank
     c                   move      'B'           ##pgfl
     c                   endif
     C                   Eval      QQPEOExt = RRPEOA
     C                   other
     C                   clear                   ##pgfl
     C                   endsl
     C                   Eval      ##dbaf = ' '

     C                   call      'SRADDR'
     C                   parm                    #1co#             3
     C                   parm                    #1fnd             3
     C                   parm                    #1emp#            9
     C                   parm                    #1div             5
     C                   parm                    ##odr             1
     C                   parm                    ##nodr            1
     C                   parm                    ##aodr            1
     C                   parm                    ##emn1           50
     C                   parm                    ##emn2           50
     C                   parm                    ##emn3           50
     C                   parm                    ##emn4           50
     C                   parm                    ##emn5           50
     C                   parm                    ##emn6           50
     C                   parm                    ##emn7           50
     C                   parm      *zero         ##adrl            3 0
     C                   parm      *zero         ##naml            3 0
     C                   parm      *zero         ##fedl            3 0
     C                   parm      *blank        ##peo1           57
     C                   parm      *blank        ##peo2           57
     C                   parm                    ##pgfl            1
     C                   parm                    ##conl            3 0
     C                   parm                    ##amn1
     C                   parm                    ##amn2
     C                   parm                    ##amn3
     C                   parm                    ##amn4
     C                   parm                    ##amn5
     C                   parm                    ##amn6
     C                   parm                    ##amn7
     C                   parm                    ##amn8
     C                   parm                    ##amn9
     C                   parm      *zero         ##peol            3 0
     C                   parm                    ##dbaf            1
     C                   parm      *zero         ##al2             3 0
     C                   parm      *zero         ##nl2             3 0
     C                   parm      *zero         ##fl2             3 0
     C                   parm      *zero         ##pl2             3 0
     C                   parm      *zero         ##cl2             3 0
     C                   PARM                    QQPEOExt         10
     C                   PARM      *blanks       QQname1         120
     C                   PARM      *blanks       QQname2         120
     C                   PARM      *blanks       QQname3         120
     C                   PARM      *blanks       QQname4         120
      ***** End Add ***** 02/10/20 ***************************
     C*
     C                   CLEAR                   PCH01
     C                   Z-ADD     XXCO#         CHCO#
     C                   Z-ADD     XXFND         CHFND
     C                   Z-ADD     XXFYR         CHFYR
     C                   Z-ADD     XXEMP#        CHEMP#
     C                   Z-ADD     XXDIV         CHDIV
      *
      **** BEGIN ADD **** 08/29/98 ************************************
     C                   MOVE      XXFND         XXCFND
     C                   MOVE      XXEMP#        XXCEMP
     C                   MOVE      XXFYR         XXCFYR
      **** BEGIN CHG **** 07/01/02 ************************************
     C**** BEGIN CHG **** 08/10/01 ************************************
     C******************** MOVELXXCPOL    CHCAP#    P
     C******************** MOVELXXCPOL    CHPOL#    P
     C**** END   CHG **** 08/10/01 ************************************
     C                   MOVEL(P)  XXCPOL        CHCAP#
      **** END   CHG **** 07/01/02 ************************************
      *
      * CHECK FOR A CARRIER/STATE REPORTING RECORD
      *
     C     RPKY          CHAIN     PRP01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     RPCAP#        IFNE      *BLANKS
      *
     C                   MOVEL(P)  RPCAP#        CHCAP#
     C***** BEGIN CHG **** 12/06/99 ***********************************
     C**********           MOVELRPCAP#    XXOPOL
     C                   MOVEL(P)  RPCAP#        XXPOL
     C***** END   CHG **** 12/06/99 ***********************************
      *
      * IF CARRIER POLICY NUMBER IS NUMERIC, CHECK TO SEE IF IT IS A
      * PDS OLD-FORMAT POLICY NUMBER
      *
     C                   TESTN                   XXOPOL               61
     C     *IN61         IFEQ      *ON
     C***** BEGIN ADD **** 12/06/99 ***********************************
     C     XOBLNK        IFEQ      *BLANKS
     C                   Z-ADD     *ZEROS        XXEMP#
     C***** END   ADD **** 12/06/99 ***********************************
     C                   MOVE      XXOFND        XXFND
     C                   MOVE      XXOEMP        XXEMP#
     C                   MOVE      XXOFYR        XXFYR
      *
     C***** BEGIN ADD **** 12/06/99 ***********************************
     C********** ELKY      CHAINPEL01                60
     C     ELKY2         CHAIN     PEL02                              60
     C***** END   ADD **** 12/06/99 ***********************************
      *
     C     *IN60         IFEQ      *ON
      *
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQFYR         XXFYR
      *
     C                   ELSE
      *
     C                   Z-ADD     0             CHEMP#
      *
     C                   ENDIF                                                  *IN60 IFEQ *ON
      *
     C***** BEGIN ADD **** 12/06/99 ***********************************
     C                   ENDIF                                                  XOBLNK=BLKS
     C***** END   ADD **** 12/06/99 ***********************************
     C*
     C                   ENDIF                                                  *IN61 IFEQ *OFF
      **** BEGIN ADD **** 07/01/02 *********************************************
     C                   ELSE
     C                   Z-ADD     0             CHEMP#
      **** END   ADD **** 07/01/02 *********************************************
      *
     C                   ENDIF                                                  RPCAP# IFNE *BLANKS
      **** BEGIN ADD **** 07/01/02 *********************************************
     C                   ELSE
     C                   Z-ADD     0             CHEMP#
      **** END   ADD **** 07/01/02 *********************************************
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      **** END   ADD **** 08/29/98 ************************************
     C                   Z-ADD     CXCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CHCDAT
      *
     C                   Z-ADD     CXCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CHCXDT
      *
     C                   Z-ADD     CXAGT#        CHAGT#
      *
     C                   Z-ADD     CXRDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CHRDAT
      *
     C                   Z-ADD     CXREDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         CHREDT
     C*
     C*   GET AGENT NAME
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C     AGKY          CHAIN     PAG01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AGNAME        CHANAM
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        CHUSER
     C                   MOVE      Q1PGM         CHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHDT
     C                   Z-ADD     DSTIME        CHTIME
     C*
     C*  LOAD EMPLOYERS NAME
     C*
     C*****EMKY          CHAIN     PEM01                              60
      *
     C******IN60         IFEQ      *OFF
      *
     C                   MOVEL     EMNAME        CHENAM
     C*** BEGIN ADD ***  03/07/05  ************************************
     C                   MOVEL(P)  EMNAME        CHNAM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        CHNAM1
     C                   MOVE(P)   EMNAM2        CHNAM2
     C*
     C                   MOVEL(P)  EMFDNM        CHFDN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        CHFDN1
     C                   MOVE(P)   EMFDN2        CHFDN2
     C*** END ADD *****  03/07/05  ************************************
      ***** BEG ADD ******* 11/15/06 **********************************
      *
     C                   MOVEL(P)  CHNAM1        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHNAM1
      *
     C                   MOVE(P)   CHNAM2        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHNAM2
      *
     C                   MOVEL(P)  CHFDN1        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHFDN1
      *
     C                   MOVE(P)   CHFDN2        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHFDN2
      *
     C     FNKY          CHAIN     PFN01                              60
      *
      * Get PEO Information if PEO Group
      *
     C     *IN60         IFEQ      *OFF
     C     FNPEOF        ANDNE     *BLANK
     C                   MOVE      QQCO#         #5CO#
     C                   MOVE      QQFND         #5FND
     C                   MOVE      QQEMP#        #5EMP#
     C                   MOVE      QQDIV         #5DIV
     C                   MOVE      QQFYR         #5FYR
      *
     C                   CALL      'SRADD1'
     C                   PARM                    #5CO#             3 0
     C                   PARM                    #5FND             3 0
     C                   PARM                    #5EMP#            9 0
     C                   PARM                    #5DIV             5 0
     C                   PARM                    #5FYR             3 0
     C                   PARM                    #5NAME          100
     C                   PARM                    #5FNAM          100
     C                   PARM                    #5EAD1           40
     C                   PARM                    #5EAD2           40
     C                   PARM                    #5ECSZ           40
     C                   PARM                    #5CTY            25
     C                   PARM                    #5ST              2
     C                   PARM                    #5ZIP             9 0
     C                   PARM                    #5FEIN            9 0
     C                   PARM                    #5EADR           90
      *
     C                   MOVEL(P)  #5NAME        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHNAM1
      *
     C                   MOVE(P)   #5NAME        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHNAM2
      *
     C                   MOVEL(P)  #5FNAM        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHFDN1
      *
     C                   MOVE(P)   #5FNAM        XXTMP4           50
     C     ##REM         CAT(P)    XXTMP4        ##LINE
     C                   Z-ADD     50            ##LEN
     C                   CALL      'SRLINE'      LINEPL
     C                   MOVEL(P)  ##LINE        CHFDN2
      *
     C                   MOVEL(P)  #5NAME        CHENAM
     C                   ENDIF                                                  FNPEOF
      *
      ***** END ADD ******* 11/15/06 **********************************
      ***** BEG CHG ******* 07/20/10 **********************************
     C                   MOVE      EMPSRV        XXSTAF
     C*************        MOVE EMPACC    XXSTAF
      ***** END CHG ******* 07/20/10 **********************************
      *
     C     UPKY          CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPUSNM        CHCLRK
     C                   MOVE      UPEXT#        CHCLRX
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C*****              ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C*  GET COMPANY NAME & ADDRESS
     C*
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CONAME        CHCONM
     C                   MOVEL     COADR1        CHCAD1
     C                   MOVEL     COADR2        CHCAD2
     C                   MOVEL     COZIP         XXZIP5            5
     C                   MOVE      COZIP         XXZIP4            4
     C     COCTY         CAT       ',':0         CHCCSZ
     C                   CAT       COST:1        CHCCSZ
     C                   CAT       XXZIP5:1      CHCCSZ
     C     XXZIP4        IFNE      *ZEROS
     C                   CAT       '-':0         CHCCSZ
     C                   CAT       XXZIP4:0      CHCCSZ
     C                   ENDIF
     C                   ENDIF
     C*
     C*  GET FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        CHFNNM
     C                   ENDIF
     C*
     C*   START READING THE CERTIFICATE HOLDERS FILE & WRITE A RECORD
     C*   FOR EACH CERTIFICATE HOLDER
     C*
     C                   MOVE      'Y'           QQNREC                         NO RECORDS YET
     C     CEKY          CHAIN     PCE01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     CESTAT        IFEQ      'A'
     C                   Z-ADD     CECER#        CHCER#
     C                   MOVEL     CENAME        CHCNAM
     C                   MOVEL     CEADR1        CHADR1
     C                   MOVEL     CEADR2        CHADR2
     C                   MOVEL     CEZIP         XXZIP5
     C                   MOVE      CEZIP         XXZIP4
     C*** begin add ***  10/06/16  ************************************
     C                   Movel(p)  CECty         CHCity
     C                   Movel(p)  CESt          CHSt
     C                   Z-add     CEZip         CHZip
     C*** end add *****  10/06/16  ************************************
      ***** BEG ADD ******* 04/04/09 **********************************
      *
     C     CECTY         IFNE      *BLANKS
      ***** END ADD ******* 04/04/09 **********************************
     C     CECTY         CAT(P)    ',':0         CHCSZ
     C                   CAT       CEST:1        CHCSZ
     C                   CAT       XXZIP5:1      CHCSZ
     C*
     C     XXZIP4        IFNE      *ZERO
     C                   CAT       '-':0         CHCSZ
     C                   CAT       XXZIP4:0      CHCSZ
     C                   ENDIF
      ***** BEG ADD ******* 04/04/09 **********************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  CEPROV        CHCSZ
     C                   ENDIF
      *
     C                   MOVEL(P)  CEPROV        CHPROV
     C                   MOVEL(P)  CECTRY        CHCTRY
      *
      ***** END ADD ******* 04/04/09 **********************************
     C*
     C***  BEGIN ADD  ***  10/01/98  **********************************
     C                   MOVE      CXCFFL        CHCFFL
     C***  END   ADD  ***  10/01/98  **********************************
     C*** begin add ***  01/25/17  ************************************
     C                   Eval      CHRev# = CERev#
     C                   Eval      CHEOFL = CEEOFL
     C                   Eval      CH3Pty = CE3Pty
     C                   Eval      CHRLFL = CERLFL
     C                   Eval      CHType = CEType
     C                   Eval      CHAttn = CEAttn
     C                   Eval      CHRem1 = CERem1
     C                   Eval      CHRem2 = CERem2
     C                   Eval      CHRem3 = CERem3
     C                   Eval      CHRem4 = CERem4
     C                   Eval      CHRem5 = CERem5
     C*** end add *****  01/25/17  ************************************
     C*
     C                   WRITE     PCH01
     C                   MOVE      'N'           QQNREC                         WORKFILE NOT EMPTY
     C*** begin add ***  10/06/16  ************************************
      ***** Beg Chg ***** 02/10/20 ************************************
     C******             If        ldOutputFile <> ' '
     C                   If        isUseXml
     C                   Exsr      ScriptXML
     C                   Endif
      ***** End Chg ***** 02/10/20 ************************************
     C*** end add *****  10/06/16  ************************************
      *** beg add ***** 11/04/16 **********************
      /Free
       If not(isUseSpoolOut);
          qqNRec = 'Y';
       Endif;
      /End-free
      *** end add ***** 11/04/16 **********************
     C                   ENDIF                                                  CESTAT
     C*
     C     CEKY          READE     PCE01                                  60
     C                   ENDDO
     C*
      ***** Beg Add ***** 02/10/20 *****************************************
      /Free
             If ldOutputFile = ' ' and massFlag = ' ';
                Select;
                When xmlFileName = *blank;
                When objExists('PDSDEV':'*LIB');
                   Monitor;
                   S4401C(xmlFileName + '$');
                   On-error;
                   Endmon;
                Other;
                   S4401C(xmlFileName + '$');
                Endsl;
             Endif;
          Endif;
          If not isCanCnf;
             Reade emky pem01;
          Else;
             Reade emky1 pem01;
          Endif;
       Enddo;
      /End-free
      ***** End Add ***** 02/10/20 *****************************************
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*** begin add ***  10/06/16  ************************************
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       If templateFile = ' ';
          Clear xxTemplate;
          Clear xxPath;

          Setgt (xxCO#:xxFND:RSJrSt:ELPfdt) pcr00;
          Readpe (xxCO#:xxFND:RSJrSt) pcr00;
          If not %eof;
       //*** begin chg ***  12/19/16  *********************************************
       //*****      Select;
       //*****      When QQType = 'CX';
       //*****         If STCRCXCrC <> ' ';
       //*****            xxTemplate = %trim(STCRCXCrC);
       //*****            xxPath = %trim(STCRCXCrPC);
       //*****         Else;
       //*****            xxTemplate = %trim(STCRCXCr);
       //*****            xxPath = %trim(STCRCXCrP);
       //*****         Endif;
       //*****      When QQType = 'CF';
       //*****         If STCRCFCrC <> ' ';
       //*****            xxTemplate = %trim(STCRCFCrC);
       //*****            xxPath = %trim(STCRCFCrPC);
       //*****         Else;
       //*****            xxTemplate = %trim(STCRCFCr);
       //*****            xxPath = %trim(STCRCFCrP);
       //*****         Endif;
       //*****      When QQType = 'RE';
       //*****         If STCRRECrC <> ' ';
       //*****            xxTemplate = %trim(STCRRECrC);
       //*****            xxPath = %trim(STCRRECrPC);
       //*****         Else;
       //*****            xxTemplate = %trim(STCRRECr);
       //*****            xxPath = %trim(STCRRECrP);
       //*****         Endif;
       //*****      Endsl;
       //*****   Endif;
          Exsr ScriptTemplate;
          Else;
          Setgt (xxCO#:xxFnd:'  ':ELPfdt) pcr00;
          Readpe (xxCO#:xxFnd:'  ') pcr00;
          If not %eof;
             Exsr ScriptTemplate;
          Else;
             Setgt (xxCO#:000:RSJrst:ELPfdt) pcr00;
             Readpe (xxCO#:000:RSJrst) pcr00;
             If not %eof;
                Exsr ScriptTemplate;
             Else;
                Setgt (xxCO#:000:'  ':ELPfdt) pcr00;
                Readpe (xxCO#:000:'  ') pcr00;
                If not %eof;
                   Exsr ScriptTemplate;
                Endif;
             Endif;
          Endif;
          Endif;
       //*** end chg *****  12/19/16  *********************************************

          If xxTemplate = ' ';
             Leavesr;
          Endif;

          If QQType = 'RE';
             message = %trim(processTypeR);
          Else;
             message = %trim(processTypeC);
          Endif;

       //*** beg add ***** 11/04/16 ***********************
          isUseSpoolOut = UseSpoolOut(message);
       //*** end add ***** 11/04/16 ***********************

      ***** Beg Add ***** 02/10/20 *****************************************

       If ldOutputFile = ' ' or (isCanCnf and EMstrp <> 'Y');
          If isMassReq;

             In *Lock *dtaara;
             Out *dtaara;

             If ldMassFile <> ' ';
                templateFile = %trim(ldMassFile);
             Else;
                StLetter('XMLREQUEST':templateFile:message:' ':'000000000':
                 ' ':'MassCancel':'000':'000':massFlag:xmlLink);
                In *Lock *dtaara;
                ldMassFile = %trim(templateFile);
                Out *dtaara;

                Chain (templateFile) xp100;
                XPCat = 'PS';
                XPData = 'Y';
                Update xp100;

             Endif;

             isMassReq = *off;
             xmlFileName = templateFile;
             xmllink = templateFile;
          Endif;

          StLetter('XMLREQUEST':templateFile:message:'E':%editc(x#RRN:'X'):
           xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

          If xmlFileName = ' ';
             xmlFileName = templateFile;
          Endif;

          If xmlLink = ' ';
             Chain (templateFile) xp100;
             templateFile = XPXmlT;

          Else;
             Chain (xmlFileBlank:xmlLink:templateFile) xp100;
          Endif;

          Select;
          When QQType = 'RE';
             XPType = 'RE Cert Letter';
          When QQType = 'CX';
             XPType = 'CX Cert Letter';
          When QQType = 'CF';
             XPType = 'CF Cert Letter';
          Endsl;

          XPDcid = xxTemplate;
          XPPath = xxPath;
          XPData = 'Y';

          Update xp100;

       Else;

          templateFile = ldoutputfile;

          StLetter('TEMPLRQST':templateFile:message:'E':%editc(x#RRN:'X'):
           xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'));

          Chain (templateFile) xt100;

          Select;
          When QQType = 'RE';
             XTType = 'RE Cert Letter';
          When QQType = 'CX';
             XTType = 'CX Cert Letter';
          When QQType = 'CF';
             XTType = 'CF Cert Letter';
          Endsl;

          XTDcid = xxTemplate;
          XTPath = xxPath;

          Update xt100;

       Endif;
       Endif;

       If templateFile <> ' ';
          CHXML = templateFile;
          Write pch0p;
          If xmlFileName <> ' ' and isWxc7pWrt = *off;
             Exsr Upd_Wxc7p;
          Endif;
       Endif;

       Endsr;
      ***** End Add ***** 02/10/20 *****************************************

       //***** Beg Add ***** 02/10/20 *************************************
       Begsr Upd_Wxc7p;

          If QQType = 'RE';
             Setgt (CXco#:CXfnd:CXemp#:CXfyr:CXrdat) pc#01;
             Readpe (CXco#:CXfnd:CXemp#:CXfyr:CXrdat) pc#01;
          Endif;

          If QQType = 'CX' or QQType = 'CF';
             Setgt (CXco#:CXfnd:CXemp#:CXfyr:CXcdat) pc#01;
             Readpe (CXco#:CXfnd:CXemp#:CXfyr:CXcdat) pc#01;
          Endif;

       If %found(WXC#L3);
          Clear pc701;

          C7XML = templateFile;

          C7Co#  = xxCo#;
          C7Fnd  = xxFnd;
          C7Emp# = xxEmp#;
          C7Div  = xxDiv;
          C7Fyr  = xxFyr;

          C7UNID = CRUNID;
          C7POL# = CRPOL#;
          C7PFRD = CRPFRD;
          C7PTOD = CRPTOD;
          C7CARR = CRCARR;
          C7CAP# = CRCAP#;
          C7Fein = %editc(CRFein:'X');
          C7CAND = CRCAND;
          C7CISD = CRCISD;
          C7CXRN = CRCXRN;
          C7DESC = CRDESC;
          C7CRSN = CRCRSN;
          C7RESN = CRRESN;
          C7RECD = CXRECD;
          xxTbid = 'RECD';
          xxKey = C7RECD;
          Chain(n) gtky pgt01;
          If %found;
             C7REDS = %trim(GTDESC);
          Endif;
          C7COM1 = CRCOM1;
          C7COM2 = CRCOM2;
          C7ISUD = CRISUD;
          C7CNC# = CRCNC#;

          C7ERR  = CRERR;

          C7OPOL = CROPOL;
          C7OPED = CROPED;
          C7GSI# = CRGSI#;
          C7OCR  = CROCR;

          C7ForNam1 = QQName1;
          C7ForNam2 = QQName2;
          C7ForNam3 = QQName3;
          C7ForNam4 = QQName4;
          C7ForAdr1 = %trim(amn(##adrl));
          C7ForAdr2 = %trim(amn(##adrl+1));
          C7ForAdr3 = %trim(amn(##adrl+2));

          C7Fein = %editc(CRFein:'X');

          C7Pgm  = Q1PGM;
          C7User = Q1USER;
          C7Time = CHtime;
          C7Dt   = CHdt;

          Write pc701;
          isWxc7pWrt = *on;

       Endif;

       Endsr;
       //***** End Add ***** 02/10/20 **************************************

       //*** begin add ***  12/19/16  *********************************************
       Begsr ScriptTemplate;

          Select;
          When QQType = 'CX';
             If STCRCXCrC <> ' ';
                xxTemplate = %trim(STCRCXCrC);
                xxPath = %trim(STCRCXCrPC);
             Else;
                xxTemplate = %trim(STCRCXCr);
                xxPath = %trim(STCRCXCrP);
             Endif;
          When QQType = 'CF';
             If STCRCFCrC <> ' ';
                xxTemplate = %trim(STCRCFCrC);
                xxPath = %trim(STCRCFCrPC);
             Else;
                xxTemplate = %trim(STCRCFCr);
                xxPath = %trim(STCRCFCrP);
             Endif;
          When QQType = 'RE';
             If STCRRECrC <> ' ';
                xxTemplate = %trim(STCRRECrC);
                xxPath = %trim(STCRRECrPC);
             Else;
                xxTemplate = %trim(STCRRECr);
                xxPath = %trim(STCRRECrP);
             Endif;
          Endsl;

       Endsr;
       //*** end add *****  12/19/16  *********************************************

      /end-free
     P*============================================
     P* Determine if the object exists
     P*============================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
     C*** end add *****  10/06/16  ************************************
