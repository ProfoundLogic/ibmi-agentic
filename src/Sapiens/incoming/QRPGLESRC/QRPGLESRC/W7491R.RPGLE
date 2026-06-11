     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/title CMS Log History Information
     H*====================================================================
     H*  Created: 08/14/09   C:232  Project: WC1819 Phase 5.
     H*====================================================================
    H*  Revisions:
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*
     H*  12/23/21   C:062  Project: WC2822(99985(146))
     H*                    Monitor numeric fields.
     H*
     H*  12/16/21   C:053  Project: WC2822 - 99985
     H*                    Changes for Blanks in fields
     H*
     H*  10/11/21   C:298  Project: WC2822
     H*                    CMS Response File Format Change
     H*
     H*  09/09/19   C:53   Support: 82044
     H*                    P&K populating QRDCN2 instead of QRDCN1 with our ID.
     H*                                                                                    *
     H*  01/30/19   C:53   Support: 75510
     H*                    Add maint fields to WMCDP (partialy applied)                    *
     H*
     H*  02/14/11   C:198  Project: WC1879 priss 163
     H*                    Send diary to claims only when CMMBS = ' ' and disp code = '01'
     H*                    and when CMMBS = 'Y' and disp code = '51'
     H*
     H*  02/14/11a  C:53   Support: 75864
     H*                    Add specific field size for claim id.
     H*
     H*  01/21/11   C:53   Support: 75566
     H*                    Ignore Specialty data in the file.
     H*
     H*  01/13/11   C:53   Project: WC1879 Priss 147
     H*                    Correct field being used for claim key.
     H*
     H*  12/18/10   C:198  Project: WC1879 Priss 119/120/122
     H*                    Fixed the issue where query response diary remark was
     H*                    going to the wrong user.
     H*
     H*  12/13/10   C:198  Project: WC1963
     H*                    updated HIC# in the WMCDP and populate records in WXM1P.
     H*
     H*  09/09/10   C:198  Project: WC1879 Priss 34/61
     H*                    Fixed the issue where DOB was blank in response file.
     H*
     H*  05/18/10   C:198  Project: WC1879
     H*                    Added processing of DCN2.
     H*
     H*  04/16/10   C:198  Project: WC1819 Phase 5 Priss 112/73
     H*                    Corrected loading of folder id/de key when writing
     H*                    to DARTS file (SDCMP).
     H*                    Removed restriction of only Open lost time/Medical
     H*                    only claims.
     H*
     H*  02/19/10 C:198 Project: WC1819 Phase 5 - Priss 69
     H*                 Added logic to get HIC# updated when response is
     H*                 received.
     H*
     H*====================================================================
     F*** Beg Chg *** 09/09/10 ****************************************
     F******WXQRP     IF   E             DISK
     FWXQRP     UF   E             DISK
     F*** End Chg *** 09/09/10 ****************************************
     FWMCXL     IF   E           K DISK
     F*** Beg Chg *** 02/19/10 ****************************************
     F*** Beg Chg *** 12/13/10 ****************************************
     F******WMCMLA    UF   E           K DISK
     FWMCMLA    IF   E           K DISK
     F*** End Chg *** 12/13/10 ****************************************
     F*** End Chg *** 02/19/10 ****************************************
     FWMCDL     UF   E           K DISK
     FWTGTL     IF   E           K DISK
    F*  Physical file definition (for maintaining record)

     FWDD1L     UF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F*** Beg Add *** 04/16/10 ****************************************
     Fstctl1    if   e           k disk
     F*** End Add *** 04/16/10 ****************************************
     F*** Beg Add *** 12/13/10 ****************************************
     Fwxm1l     if a e           k disk
     F*** End Add *** 12/13/10 ****************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     DMED2_DS        e ds                  extname(MED2NGQR)
      *
     D                 DS
     D DSKEY                         40
     d  DSCO#                         3  0 OVERLAY(DSKEY)
     d  DSFND                         3  0 OVERLAY(DSKEY:4)
     d  DSFYR                         3  0 OVERLAY(DSKEY:7)
     d  DSCASE                        7  0 OVERLAY(DSKEY:10)

     D** Date & Time structure
     D                 ds
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
     D*** Beg Add *** 12/13/10 ****************************************
     D XXDCN#                        15
     d  XXFND                         3  0 OVERLAY(XXDCN#)
     d  XXFYR                         3  0 OVERLAY(XXDCN#:4)
     d  XXCASE                        7  0 OVERLAY(XXDCN#:7)
     D*** End Add *** 12/13/10 ****************************************

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##CO#           s              3  0
     DCurrDate         S               d   DatFmt(*Iso)
     D@@RRN            s             10  0
     D*** Beg Add *** 12/13/10 ****************************************
     D kkco#           S                   like(m1co#)
     D kkfnd           S                   like(m1fnd)
     D kkfyr           S                   like(m1fyr)
     D kkcase          S                   like(m1case)
     D*** End Add *** 12/13/10 ****************************************
     D*** Beg Add *** 09/09/19 ****************************************
     d CltID           s              3
     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d CltID                          3
     D*** End Add *** 09/09/19 ****************************************
      *
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
    C*
    C*  Company file/fund (group) retrieve program parameter list
     C     *ENTRY        PLIST
     C                   PARM                    CO#               3
     C                   PARM                    QUE              10
    C*
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

     C     DTCYMD1       PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR

     C     DATCHK        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR

    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND

     C*  KEY LIST FOR Extract Log File
     C     D1KY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    xxexdt            7 0

     C     D1KY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE

     C*  KEY LIST FOR Extract Log File
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY

     C*** Beg Add *** 12/13/10 ****************************************
     C     m1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C*** End Add *** 12/13/10 ****************************************
    C*================================================================
     C                   Movel     *date         CurrDate
     C                   Movel     co#           xxco#             3 0
     C*
     C*** Beg Add *** 09/09/19 ****************************************
         /Free
          // Get the client id
          GetClientID(CltID);
         /End-free
     C*** End Add *** 09/09/19 ****************************************
     C*** Beg Add *** 09/09/10 ****************************************
     C                   read      pqr01                                  80
     C                   dow       *in80 = *off
     C*
     C                   exsr      chkfld
     C*
     C*** End Del *** 12/16/21 ****************************************
     C*****              if        %subst(mrdata:20:8) = ' '
     C***** moved code   eval      %subst(mrdata:20:8) = '00000000'
     C*****              update    pqr01
     C*****              endif
     C*** End Del *** 12/16/21 ****************************************
     C*** Beg Add *** 12/16/21 ****************************************
     C                   update    pqr01
     C*** End Add *** 12/16/21 ****************************************
     C                   read      pqr01                                  80
     C                   enddo
     C     1             setll     pqr01
     C*** End Add *** 09/09/10 ****************************************
     C                   READ      PQR01                                  80
     C                   DOW       *IN80 = *OFF
     C*
     C*Load into MED2NGQR data structure defined exactly like the
     C*layout of WXQRP
     C*
     C                   EVAL      MED2_DS = MRDATA
     C*
     C*** Beg Del *** 09/09/19 ****************************************
     C* Deleted and copied code to be more readable w/new logic.  C**
     C*** Beg Add *** 12/13/10 ****************************************
     C*** Beg Chg *** 01/13/11 ****************************************
     C***********        movel     qrdcn#        xxdcn#
     C*** Beg Add *** 01/21/11 ****************************************
     C*** Beg Add *** 02/14/11a ****************************************
     C**                 movel     qrdcn1        xadcn1           13
     C*** END Add *** 02/14/11a ****************************************
     C*** Beg Chg *** 02/14/11a ****************************************
     C***********        TESTN                   qrdcn1               50
     C**                 TESTN                   xadcn1               50
     C*** End Chg *** 02/14/11a ****************************************
     C**   *IN50         CABEQ     *OFF          SKIP
     C*** End Add *** 01/21/11 ****************************************
     C**                 movel     qrdcn1        xxdcn#
     C*** End Chg *** 01/13/11 ****************************************
     C*** End Del *** 09/09/19 ****************************************
     C*** Beg Add *** 09/09/19 ****************************************
     C                   select
     C     CltID         wheneq    '152'
     C                   movel     qrdcn2        xxdcn#
     C                   movel     qrdcn2        xadcn1           13
     C*
     C                   other
     C                   movel     qrdcn1        xxdcn#
     C                   movel     qrdcn1        xadcn1           13
     C                   endsl
     C*
     C                   TESTN                   xadcn1               50
     C     *IN50         CABEQ     *OFF          SKIP
     C*** End Add *** 09/09/19 ****************************************
     C                   eval      kkco#  = xxco#
     C                   eval      kkfnd  = xxfnd
     C                   eval      kkfyr  = xxfyr
     C                   eval      kkcase = xxcase
     C     m1ky          chain     pm101                              81
     C                   if        *in81 = *on
     C                   eval      m1co#  = kkco#
     C                   eval      m1fnd  = kkfnd
     C                   eval      m1fyr  = kkfyr
     C                   eval      m1case = kkcase
     C                   time                    dsdttm
     C                   move      dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   movel     srdto         m1rsdt
     C                   write     pm101
     C                   endif
     C*** End Add *** 12/13/10 ****************************************
     C     QRSS#         CHAIN     PCD01                              81
     C                   IF        *IN81 = *OFF
     C                   SELECT
     C                   WHEN      QRCODE = '01'
     C                   EVAL      CDMDBN = 'Y'
     C                   WHEN      QRCODE = '51'
     C                   EVAL      CDMDBN = ' '
     C                   ENDSL
     C*** Beg Add *** 12/13/10 *******************************************
     C                   movel     qrhic#        cdhicn
     C*** End Add *** 12/13/10 *******************************************
     C*** Beg Add *** 01/30/19 ****************************************
     C* Maintenance information
     C                   time                    DSDTTM
     C                   move      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   movel     SRDTO         CDdt
     C                   movel     Q1pgm         CDpgm
     C                   movel     DStime        CDtime
     C                   movel     Q1user        CDuser
     C*** End Add *** 01/30/19 ****************************************
     C                   UPDATE    PCD01
     C                   ENDIF
     C**
      *
     C     QRSS#         SETLL     PCM01                              81
     C     QRSS#         READE     PCM01                                  81
      *
     C                   DOW       *IN81 = *OFF
      *
     C*** Beg Chg *** 02/19/10 **********************************
     C*********          IF        *IN80 = *OFF AND (CMLTFL = 'I' OR
     C                   IF        *IN81 = *OFF AND (CMLTFL = 'I' OR
     C*** End Chg *** 02/19/10 **********************************
     C*** Beg Chg *** 04/16/10 **********************************
     C*******                       CMLTFL = 'M') AND CMSTAT = 'O'
     C                              CMLTFL = 'M')
     C*** End Chg *** 04/16/10 **********************************
     C*** Beg Add *** 12/13/10 ****************************************
     C                   IF        CMCO# = KKCO# and CMFND = KKFND and
     C                             CMFYR = KKFYR and CMCASE = KKCASE
      *
     C*** End Add *** 12/13/10 ****************************************
     C*** Beg Chg *** 12/13/10 ****************************************
     C********           IF        QRCODE = '01' AND CMMBS = ' '
     C                   select
     C                   when      QRCODE = '01' AND CMMBS = ' '
     C*** End Chg *** 12/13/10 ****************************************
      *
     c     FNKY          chain     PCX01                              82
      *
     C                   IF        CXQRSD <>  *BLANKS
     C                   EXSR      writeSDCMP
     C                   ENDIF
      *
     C*** Beg Chg *** 12/13/10 ****************************************
     C*******            ENDIF
     C
     C*******            IF        QRCODE = '51' AND CMMBS = 'Y'
     C                   when      QRCODE = '51' AND CMMBS = 'Y'
     C*** End Chg *** 12/13/10 ****************************************
      *
     c     FNKY          chain     PCX01                              82
      *
     C                   IF        CXQRSD <>  *BLANKS
     C                   EXSR      writeSDCMP
     C                   ENDIF
      *
     C*** Beg Chg *** 12/13/10 ****************************************
     C*******            ENDIF
     C*** Beg Del *** 02/14/11 ****************************************
     C****               Other
     C****               EXSR      writeSDCMP
     C*** End Del *** 02/14/11 ****************************************
     C                   endsl
     C*** End Add *** 12/13/10 ****************************************
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         XXEXDT
      *
     C*** Beg Chg *** 02/19/10 *******************************************
     C**** D1KY1         SETGT     PD101
     C     D1KY2         SETGT     PD101
     C*** End Chg *** 02/19/10 *******************************************
     C     D1KY2         READPE    PD101                                  83
      *
     C*** Beg Chg *** 02/19/10 *******************************************
     C*********          IF        *IN83 = *off
     C                   DOW       *IN83 = *off
     C*
     C                   if        d1type = 'Q'
     C*** End Chg *** 02/19/10 *******************************************
      *
     C                   EVAL      D2HIC# = QRHIC#
     C                   EVAL      D2LSN  = QRLSN
     C                   EVAL      D2FSN  = QRFSN
     C                   MOVE      QRDTOB        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         D2DTOB
     C                   EVAL      D2SEX  = QRSEX
     C                   EVAL      D2SS#  = QRSS#
     C*** Beg Add *** 05/18/10 **********************************
     C                   movel     qrdcn2        D2dcn2
     C*** End Add *** 05/18/10 **********************************
     C                   EVAL      D1CODE = QRCODE
     C                   EVAL      D1DCN# = QRDCN#
      //***** Beg Add ***** 10/11/21 *******************************
      /Free
       Exsr LoadPaidFlds;
      /End-free
      //***** End Add ***** 10/11/21 *******************************
     C
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    dtcymd1
     C                   MOVEL     SRDTO         D1RSDT
      *
     C                   movel     SRDTO         D1DT
     C                   MOVEL     'W7491R    '  D1PGM
     C                   movel     Q1USER        D1USER
     C                   movel     DSTIME        D1TIME
      *
     C                   UPDATE    PD101
      *
     C*** End Chg *** 02/19/10 *******************************************
     C******             ENDIF                                                  *in83 = *off
     C                   leave
     C                   else
     C     D1KY2         READPE    PD101                                  83
     C                   ENDIF                                                  d1type = 'Q'
     C*
     C                   Enddo                                                  *in83 = *off
     C*** End Chg *** 02/19/10 *******************************************
     C*** Beg Add *** 12/13/10 ****************************************
     C                   ENDIF                                                  cmclaim = qrclaim
     C*** End Add *** 12/13/10 ****************************************
      *
     C                   ENDIF                                                  CMLTFL = I/M
      *
     C*** Beg Add *** 02/19/10 **********************************
     C*** Beg Del *** 12/13/10 ****************************************
     C*********          eval      cmhic# = qrhic#
     C*********          update    pcm01
     C*** End Del *** 12/13/10 ****************************************
     C*** End Add *** 02/19/10 **********************************
     C     QRSS#         READE     PCM01                                  81
     C                   ENDDO
      *
     C*** Beg Add *** 01/21/11 ****************************************
     C     SKIP          TAG
     C*** End Add *** 01/21/11 ****************************************
     C                   READ      PQR01                                  80
     C                   ENDDO
      *
     C                   TIME                    DSDTTM
     C                   MOVE      DSDATE        SRDTI
     C                   call      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8
      *
     C                   CALL      'W7490R1'
     C                   PARM                    CO#
     C                   PARM                    QUE
     C                   PARM                    XXDATE
    C*================================================================
     C                   EVAL      *INLR = '1'
    C*
      ****** Beg Add ***** 10/11/21 ***********************************
     *================================================================
     * LoadPaidFlds - Loading new added fields
     *================================================================
      /Free
      *** Begin Chg *** 12/23/21 **************************************
       //********** Begsr LoadPaidFlds;
       //**********  D1MRPAEFD = QRMRPAEFD;
       //**********  D1MRPATRD = QRMRPATRD;
       //**********  D1MRPBEFD = QRMRPBEFD;
       //**********  D1MRPBTRD = QRMRPBTRD;
       //**********  D1MRPCPC# = QRMRPCPC#;

       //**********  D1MRPCPED = QRMRPCPED;
       //**********  D1MRPCPTD = QRMRPCPTD;
       //**********  D1MRPCPCN = QRMRPCPCN;

       //**********  D1MRPCBP# = QRMRPCBP#;
       //**********  D1MRPCAD1 = QRMRPCAD1;
       //**********  D1MRPCAD2 = QRMRPCAD2;
       //**********  D1MRPCCTY = QRMRPCCTY;
       //**********  D1MRPCST  = QRMRPCST ;
       //**********  D1MRPCZIP = QRMRPCZIP;
       //**********  D1PPCPC#  = QRPPCPC# ;

       //**********  D1PPCPED  = QRPPCPED ;
       //**********  D1PPCPTD  = QRPPCPTD ;
       //**********  D1PPCPCN  = QRPPCPCN ;
       //**********  D1PPCBP#  = QRPPCBP# ;
       //**********  D1PPCAD1  = QRPPCAD1 ;
       //**********  D1PPCAD2  = QRPPCAD2 ;
       //**********  D1PPCCTY  = QRPPCCTY ;
       //**********  D1PPCST   = QRPPCST  ;
       //**********  D1PPCZIP  = QRPPCZIP ;
       //**********  D1PPCPC#1 = QRPPCPC#1;

       //**********  D1PPCPED1 = QRPPCPED1;
       //**********  D1PPCPTD1 = QRPPCPTD1;
       //**********  D1PPCPCN1 = QRPPCPCN1;

       //**********  D1PPCBP#1 = QRPPCBP#1;
       //**********  D1PPCAD11 = QRPPCAD11;
       //**********  D1PPCAD21 = QRPPCAD21;
       //**********  D1PPCCTY1 = QRPPCCTY1;
       //**********  D1PPCST1  = QRPPCST1 ;
       //**********  D1PPCZIP1 = QRPPCZIP1;
       //**********  D1PPCPC#2 = QRPPCPC#2;

       //**********  D1PPCPED2 = QRPPCPED2;
       //**********  D1PPCPTD2 = QRPPCPTD2;
       //**********  D1PPCPCN2 = QRPPCPCN2;

       //**********  D1PPCBP#2 = QRPPCBP#2;
       //**********  D1PPCAD12 = QRPPCAD12;
       //**********  D1PPCAD22 = QRPPCAD22;
       //**********  D1PPCCTY2 = QRPPCCTY2;
       //**********  D1PPCST2  = QRPPCST2 ;
       //**********  D1PPCZIP2 = QRPPCZIP2;
       //**********  D1PPCPC#3 = QRPPCPC#3;

       //**********  D1PPCPED3 = QRPPCPED3;
       //**********  D1PPCPTD3 = QRPPCPTD3;
       //**********  D1PPCPCN3 = QRPPCPCN3;

       //**********  D1PPCBP#3 = QRPPCBP#3;
       //**********  D1PPCAD13 = QRPPCAD13;
       //**********  D1PPCAD23 = QRPPCAD23;
       //**********  D1PPCCTY3 = QRPPCCTY3;
       //**********  D1PPCST3  = QRPPCST3 ;
       //**********  D1PPCZIP3 = QRPPCZIP3;
       //**********  D1PPCPC#4 = QRPPCPC#4;

       //**********  D1PPCPED4 = QRPPCPED4;
       //**********  D1PPCPTD4 = QRPPCPTD4;
       //**********  D1PPCPCN4 = QRPPCPCN4;

       //**********  D1PPCBP#4 = QRPPCBP#4;
       //**********  D1PPCAD14 = QRPPCAD14;
       //**********  D1PPCAD24 = QRPPCAD24;
       //**********  D1PPCCTY4 = QRPPCCTY4;
       //**********  D1PPCST4  = QRPPCST4 ;
       //**********  D1PPCZIP4 = QRPPCZIP4;
       //**********  D1PPCPC#5 = QRPPCPC#5;

       //**********  D1PPCPED5 = QRPPCPED5;
       //**********  D1PPCPTD5 = QRPPCPTD5;
       //**********  D1PPCPCN5 = QRPPCPCN5;

       //**********  D1PPCBP#5 = QRPPCBP#5;
       //**********  D1PPCAD15 = QRPPCAD15;
       //**********  D1PPCAD25 = QRPPCAD25;
       //**********  D1PPCCTY5 = QRPPCCTY5;
       //**********  D1PPCST5  = QRPPCST5 ;
       //**********  D1PPCZIP5 = QRPPCZIP5;
       //**********  D1PPCPC#6 = QRPPCPC#6;

       //**********  D1PPCPED6 = QRPPCPED6;
       //**********  D1PPCPTD6 = QRPPCPTD6;
       //**********  D1PPCPCN6 = QRPPCPCN6;

       //**********  D1PPCBP#6 = QRPPCBP#6;
       //**********  D1PPCAD16 = QRPPCAD16;
       //**********  D1PPCAD26 = QRPPCAD26;
       //**********  D1PPCCTY6 = QRPPCCTY6;
       //**********  D1PPCST6  = QRPPCST6 ;
       //**********  D1PPCZIP6 = QRPPCZIP6;
       //**********  D1PPCPC#7 = QRPPCPC#7;

       //**********  D1PPCPED7 = QRPPCPED7;
       //**********  D1PPCPTD7 = QRPPCPTD7;
       //**********  D1PPCPCN7 = QRPPCPCN7;

       //**********  D1PPCBP#7 = QRPPCBP#7;
       //**********  D1PPCAD17 = QRPPCAD17;
       //**********  D1PPCAD27 = QRPPCAD27;
       //**********  D1PPCCTY7 = QRPPCCTY7;
       //**********  D1PPCST7  = QRPPCST7 ;
       //**********  D1PPCZIP7 = QRPPCZIP7;
       //**********  D1PPCPC#8 = QRPPCPC#8;

       //**********  D1PPCPED8 = QRPPCPED8;
       //**********  D1PPCPTD8 = QRPPCPTD8;
       //**********  D1PPCPCN8 = QRPPCPCN8;

       //**********  D1PPCBP#8 = QRPPCBP#8;
       //**********  D1PPCAD18 = QRPPCAD18;
       //**********  D1PPCAD28 = QRPPCAD28;
       //**********  D1PPCCTY8 = QRPPCCTY8;
       //**********  D1PPCST8  = QRPPCST8 ;
       //**********  D1PPCZIP8 = QRPPCZIP8;
       //**********  D1PPCPC#9 = QRPPCPC#9;

       //**********  D1PPCPED9 = QRPPCPED9;
       //**********  D1PPCPTD9 = QRPPCPTD9;
       //**********  D1PPCPCN9 = QRPPCPCN9;

       //**********  D1PPCBP#9 = QRPPCBP#9;
       //**********  D1PPCAD19 = QRPPCAD19;
       //**********  D1PPCAD29 = QRPPCAD29;
       //**********  D1PPCCTY9 = QRPPCCTY9;
       //**********  D1PPCST99 = QRPPCST9 ;
       //**********  D1PPCZIP9 = QRPPCZIP9;
       //**********  D1MRPDPC# = QRMRPDPC#;

       //**********  D1MRPDPED = QRMRPDPED;
       //**********  D1MRPDPTD = QRMRPDPTD;
       //**********  D1MRPDPCN = QRMRPDPCN;

       //**********  D1MRPDBP# = QRMRPDBP#;
       //**********  D1MRPDAD1 = QRMRPDAD1;
       //**********  D1MRPDAD2 = QRMRPDAD2;
       //**********  D1MRPDCTY = QRMRPDCTY;
       //**********  D1MRPDST  = QRMRPDST ;
       //**********  D1MRPDZIP = QRMRPDZIP;
       //**********  D1PPDPC#1 = QRPPDPC#1;

       //**********  D1PPDPED1 = QRPPDPED1;
       //**********  D1PPDPTD1 = QRPPDPTD1;
       //**********  D1PPDPCN1 = QRPPDPCN1;

       //**********  D1PPDBP#1 = QRPPDBP#1;
       //**********  D1PPDAD11 = QRPPDAD11;
       //**********  D1PPDAD21 = QRPPDAD21;
       //**********  D1PPDCTY1 = QRPPDCTY1;
       //**********  D1PPDST1  = QRPPDST1 ;
       //**********  D1PPDZIP1 = QRPPDZIP1;
       //**********  D1PPDPC#2 = QRPPDPC#2;

       //**********  D1PPDPED2 = QRPPDPED2;
       //**********  D1PPDPTD2 = QRPPDPTD2;
       //**********  D1PPDPCN2 = QRPPDPCN2;

       //**********  D1PPDBP#2 = QRPPDBP#2;
       //**********  D1PPDAD12 = QRPPDAD12;
       //**********  D1PPDAD22 = QRPPDAD22;
       //**********  D1PPDCTY2 = QRPPDCTY2;
       //**********  D1PPDST2  = QRPPDST2 ;
       //**********  D1PPDZIP2 = QRPPDZIP2;
       //**********  D1PPDPC#3 = QRPPDPC#3;

       //**********  D1PPDPED3 = QRPPDPED3;
       //**********  D1PPDPTD3 = QRPPDPTD3;
       //**********  D1PPDPCN3 = QRPPDPCN3;

       //**********  D1PPDBP#3 = QRPPDBP#3;
       //**********  D1PPDAD13 = QRPPDAD13;
       //**********  D1PPDAD23 = QRPPDAD23;
       //**********  D1PPDCTY3 = QRPPDCTY3;
       //**********  D1PPDST3  = QRPPDST3 ;
       //**********  D1PPDZIP3 = QRPPDZIP3;
       //**********  D1PPDPC#4 = QRPPDPC#4;

       //**********  D1PPDPED4 = QRPPDPED4;
       //**********  D1PPDPTD4 = QRPPDPTD4;
       //**********  D1PPDPCN4 = QRPPDPCN4;

       //**********  D1PPDBP#4 = QRPPDBP#4;
       //**********  D1PPDAD14 = QRPPDAD14;
       //**********  D1PPDAD24 = QRPPDAD24;
       //**********  D1PPDCTY4 = QRPPDCTY4;
       //**********  D1PPDST4  = QRPPDST4 ;
       //**********  D1PPDZIP4 = QRPPDZIP4;
       //**********  D1PPDPC#5 = QRPPDPC#5;

       //**********  D1PPDPED5 = QRPPDPED5;
       //**********  D1PPDPTD5 = QRPPDPTD5;
       //**********  D1PPDPCN5 = QRPPDPCN5;

       //**********  D1PPDBP#5 = QRPPDBP#5;
       //**********  D1PPDAD15 = QRPPDAD15;
       //**********  D1PPDAD25 = QRPPDAD25;
       //**********  D1PPDCTY5 = QRPPDCTY5;
       //**********  D1PPDST55 = QRPPDST55;
       //**********  D1PPDZIP5 = QRPPDZIP5;
       //**********  D1PPDPC#6 = QRPPDPC#6;

       //**********  D1PPDPED6 = QRPPDPED6;
       //**********  D1PPDPTD6 = QRPPDPTD6;
       //**********  D1PPDPCN6 = QRPPDPCN6;

       //**********  D1PPDBP#6 = QRPPDBP#6;
       //**********  D1PPDAD16 = QRPPDAD16;
       //**********  D1PPDAD26 = QRPPDAD26;
       //**********  D1PPDCTY6 = QRPPDCTY6;
       //**********  D1PPDST6  = QRPPDST6 ;
       //**********  D1PPDZIP6 = QRPPDZIP6;
       //**********  D1PPDPC#7 = QRPPDPC#7;

       //**********  D1PPDPED7 = QRPPDPED7;
       //**********  D1PPDPTD7 = QRPPDPTD7;
       //**********  D1PPDPCN7 = QRPPDPCN7;

       //**********  D1PPDBP#7 = QRPPDBP#7;
       //**********  D1PPDAD17 = QRPPDAD17;
       //**********  D1PPDAD27 = QRPPDAD27;
       //**********  D1PPDCTY7 = QRPPDCTY7;
       //**********  D1PPDST7  = QRPPDST7 ;
       //**********  D1PPDZIP7 = QRPPDZIP7;
       //**********  D1PPDPC#8 = QRPPDPC#8;

       //**********  D1PPDPED8 = QRPPDPED8;
       //**********  D1PPDPTD8 = QRPPDPTD8;
       //**********  D1PPDPCN8 = QRPPDPCN8;

       //**********  D1PPDBP#8 = QRPPDBP#8;
       //**********  D1PPDAD18 = QRPPDAD18;
       //**********  D1PPDAD28 = QRPPDAD28;
       //**********  D1PPDCTY8 = QRPPDCTY8;
       //**********  D1PPDST8  = QRPPDST8 ;
       //**********  D1PPDZIP8 = QRPPDZIP8;
       //**********  D1PPDPC#9 = QRPPDPC#9;

       //**********  D1PPDPED9 = QRPPDPED9;
       //**********  D1PPDPTD9 = QRPPDPTD9;
       //**********  D1PPDPCN9 = QRPPDPCN9;

       //**********  D1PPDBP#9 = QRPPDBP#9;
       //**********  D1PPDAD19 = QRPPDAD19;
       //**********  D1PPDAD29 = QRPPDAD29;
       //**********  D1PPDCTY9 = QRPPDCTY9;
       //**********  D1PPDST9  = QRPPDST9 ;
       //**********  D1PPDZIP9 = QRPPDZIP9;
       //**********  D1PPDPC#A = QRPPDPC#A;

       //**********  D1PPDPEDA = QRPPDPEDA;
       //**********  D1PPDPTDA = QRPPDPTDA;
       //**********  D1PPDPCNA = QRPPDPCNA;

       //**********  D1PPDBP#A = QRPPDBP#A;
       //**********  D1PPDAD1A = QRPPDAD1A;
       //**********  D1PPDAD2A = QRPPDAD2A;
       //**********  D1PPDCTYA = QRPPDCTYA;
       //**********  D1PPDSTA  = QRPPDSTA ;
       //**********  D1PPDZIPA = QRPPDZIPA;
       //**********  D1PPDPC#B = QRPPDPC#B;

       //**********  D1PPDPEDB = QRPPDPEDB;
       //**********  D1PPDPTDB = QRPPDPTDB;
       //**********  D1PPDPCNB = QRPPDPCNB;

       //**********  D1PPDBP#B = QRPPDBP#B;
       //**********  D1PPDAD1B = QRPPDAD1B;
       //**********  D1PPDAD2B = QRPPDAD2B;
       //**********  D1PPDCTYB = QRPPDCTYB;
       //**********  D1PPDSTB  = QRPPDSTB ;
       //**********  D1PPDZIPB = QRPPDZIPB;

       //**********  D1PPDPC#C = QRPPDPC#C;

       //**********  D1PPDPEDC = QRPPDPEDC;
       //**********  D1PPDPTDC = QRPPDPTDC;
       //**********  D1PPDPCNC = QRPPDPCNC;

       //**********  D1PPDBP#C = QRPPDBP#C;
       //**********  D1PPDAD1C = QRPPDAD1C;
       //**********  D1PPDAD2C = QRPPDAD2C;
       //**********  D1PPDCTYC = QRPPDCTYC;
       //**********  D1PPDSTC  = QRPPDSTC ;
       //**********  D1PPDZIPC = QRPPDZIPC;

       //********** Endsr;
       //**********
       Begsr LoadPaidFlds;

        Monitor;
           D1MRPAEFD = QRMRPAEFD;
        On-Error;
        EndMon;

        Monitor;
           D1MRPATRD = QRMRPATRD;
        On-Error;
        EndMon;

        Monitor;
           D1MRPBEFD = QRMRPBEFD;
        On-Error;
        EndMon;

        Monitor;
           D1MRPBTRD = QRMRPBTRD;
        On-Error;
        EndMon;

        D1MRPCPC# = QRMRPCPC#;
        //------------------------------
        Monitor;
           D1MRPCPED = QRMRPCPED;
        On-Error;
        EndMon;

        Monitor;
           D1MRPCPTD = QRMRPCPTD;
        On-Error;
        EndMon;
        //------------------------------
        D1MRPCPCN = QRMRPCPCN;
        //------------------------------
        D1MRPCBP# = QRMRPCBP#;
        D1MRPCAD1 = QRMRPCAD1;
        D1MRPCAD2 = QRMRPCAD2;
        D1MRPCCTY = QRMRPCCTY;
        D1MRPCST  = QRMRPCST ;
        D1MRPCZIP = QRMRPCZIP;
        D1PPCPC#  = QRPPCPC# ;
        //------------------------------
        Monitor;
           D1PPCPED  = QRPPCPED ;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD  = QRPPCPTD ;
        On-Error;
        EndMon;

        D1PPCPCN  = QRPPCPCN ;
        D1PPCBP#  = QRPPCBP# ;
        D1PPCAD1  = QRPPCAD1 ;
        D1PPCAD2  = QRPPCAD2 ;
        D1PPCCTY  = QRPPCCTY ;
        D1PPCST   = QRPPCST  ;
        D1PPCZIP  = QRPPCZIP ;
        D1PPCPC#1 = QRPPCPC#1;
        //------------------------------
        Monitor;
           D1PPCPED1 = QRPPCPED1;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD1 = QRPPCPTD1;
        On-Error;
        EndMon;

        D1PPCPCN1 = QRPPCPCN1;
        //------------------------------
        D1PPCBP#1 = QRPPCBP#1;
        D1PPCAD11 = QRPPCAD11;
        D1PPCAD21 = QRPPCAD21;
        D1PPCCTY1 = QRPPCCTY1;
        D1PPCST1  = QRPPCST1 ;
        D1PPCZIP1 = QRPPCZIP1;
        D1PPCPC#2 = QRPPCPC#2;
        //------------------------------
        Monitor;
        D1PPCPED2 = QRPPCPED2;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD2 = QRPPCPTD2;
        On-Error;
        EndMon;

        D1PPCPCN2 = QRPPCPCN2;
        //------------------------------
        D1PPCBP#2 = QRPPCBP#2;
        D1PPCAD12 = QRPPCAD12;
        D1PPCAD22 = QRPPCAD22;
        D1PPCCTY2 = QRPPCCTY2;
        D1PPCST2  = QRPPCST2 ;
        D1PPCZIP2 = QRPPCZIP2;
        D1PPCPC#3 = QRPPCPC#3;
        //------------------------------
        Monitor;
           D1PPCPED3 = QRPPCPED3;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD3 = QRPPCPTD3;
        On-Error;
        EndMon;

        D1PPCPCN3 = QRPPCPCN3;
        //------------------------------
        D1PPCBP#3 = QRPPCBP#3;
        D1PPCAD13 = QRPPCAD13;
        D1PPCAD23 = QRPPCAD23;
        D1PPCCTY3 = QRPPCCTY3;
        D1PPCST3  = QRPPCST3 ;
        D1PPCZIP3 = QRPPCZIP3;
        D1PPCPC#4 = QRPPCPC#4;
        //------------------------------
        Monitor;
           D1PPCPED4 = QRPPCPED4;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD4 = QRPPCPTD4;
        On-Error;
        EndMon;

        D1PPCPCN4 = QRPPCPCN4;
        //------------------------------
        D1PPCBP#4 = QRPPCBP#4;
        D1PPCAD14 = QRPPCAD14;
        D1PPCAD24 = QRPPCAD24;
        D1PPCCTY4 = QRPPCCTY4;
        D1PPCST4  = QRPPCST4 ;
        D1PPCZIP4 = QRPPCZIP4;
        D1PPCPC#5 = QRPPCPC#5;
        //------------------------------
        Monitor;
           D1PPCPED5 = QRPPCPED5;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD5 = QRPPCPTD5;
        On-Error;
        EndMon;

        D1PPCPCN5 = QRPPCPCN5;
        //------------------------------
        D1PPCBP#5 = QRPPCBP#5;
        D1PPCAD15 = QRPPCAD15;
        D1PPCAD25 = QRPPCAD25;
        D1PPCCTY5 = QRPPCCTY5;
        D1PPCST5  = QRPPCST5 ;
        D1PPCZIP5 = QRPPCZIP5;
        D1PPCPC#6 = QRPPCPC#6;
        //------------------------------
        Monitor;
           D1PPCPED6 = QRPPCPED6;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD6 = QRPPCPTD6;
        On-Error;
        EndMon;

        D1PPCPCN6 = QRPPCPCN6;
        //------------------------------
        D1PPCBP#6 = QRPPCBP#6;
        D1PPCAD16 = QRPPCAD16;
        D1PPCAD26 = QRPPCAD26;
        D1PPCCTY6 = QRPPCCTY6;
        D1PPCST6  = QRPPCST6 ;
        D1PPCZIP6 = QRPPCZIP6;
        D1PPCPC#7 = QRPPCPC#7;
        //------------------------------
        Monitor;
           D1PPCPED7 = QRPPCPED7;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD7 = QRPPCPTD7;
        On-Error;
        EndMon;

        D1PPCPCN7 = QRPPCPCN7;
        //------------------------------
        D1PPCBP#7 = QRPPCBP#7;
        D1PPCAD17 = QRPPCAD17;
        D1PPCAD27 = QRPPCAD27;
        D1PPCCTY7 = QRPPCCTY7;
        D1PPCST7  = QRPPCST7 ;
        D1PPCZIP7 = QRPPCZIP7;
        D1PPCPC#8 = QRPPCPC#8;
        //------------------------------
        Monitor;
           D1PPCPED8 = QRPPCPED8;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD8 = QRPPCPTD8;
        On-Error;
        EndMon;

        D1PPCPCN8 = QRPPCPCN8;
        //------------------------------
        D1PPCBP#8 = QRPPCBP#8;
        D1PPCAD18 = QRPPCAD18;
        D1PPCAD28 = QRPPCAD28;
        D1PPCCTY8 = QRPPCCTY8;
        D1PPCST8  = QRPPCST8 ;
        D1PPCZIP8 = QRPPCZIP8;
        D1PPCPC#9 = QRPPCPC#9;
        //------------------------------
        Monitor;
           D1PPCPED9 = QRPPCPED9;
        On-Error;
        EndMon;

        Monitor;
           D1PPCPTD9 = QRPPCPTD9;
        On-Error;
        EndMon;

        D1PPCPCN9 = QRPPCPCN9;
        //------------------------------
        D1PPCBP#9 = QRPPCBP#9;
        D1PPCAD19 = QRPPCAD19;
        D1PPCAD29 = QRPPCAD29;
        D1PPCCTY9 = QRPPCCTY9;
        D1PPCST99 = QRPPCST9 ;
        D1PPCZIP9 = QRPPCZIP9;
        D1MRPDPC# = QRMRPDPC#;
        //------------------------------
        Monitor;
           D1MRPDPED = QRMRPDPED;
        On-Error;
        EndMon;

        Monitor;
           D1MRPDPTD = QRMRPDPTD;
        On-Error;
        EndMon;

        D1MRPDPCN = QRMRPDPCN;
        //------------------------------
        D1MRPDBP# = QRMRPDBP#;
        D1MRPDAD1 = QRMRPDAD1;
        D1MRPDAD2 = QRMRPDAD2;
        D1MRPDCTY = QRMRPDCTY;
        D1MRPDST  = QRMRPDST ;
        D1MRPDZIP = QRMRPDZIP;
        D1PPDPC#1 = QRPPDPC#1;
        //------------------------------
        Monitor;
           D1PPDPED1 = QRPPDPED1;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD1 = QRPPDPTD1;
        On-Error;
        EndMon;

        D1PPDPCN1 = QRPPDPCN1;
        //------------------------------
        D1PPDBP#1 = QRPPDBP#1;
        D1PPDAD11 = QRPPDAD11;
        D1PPDAD21 = QRPPDAD21;
        D1PPDCTY1 = QRPPDCTY1;
        D1PPDST1  = QRPPDST1 ;
        D1PPDZIP1 = QRPPDZIP1;
        D1PPDPC#2 = QRPPDPC#2;
        //------------------------------
        Monitor;
           D1PPDPED2 = QRPPDPED2;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD2 = QRPPDPTD2;
        On-Error;
        EndMon;

        D1PPDPCN2 = QRPPDPCN2;
        //------------------------------
        D1PPDBP#2 = QRPPDBP#2;
        D1PPDAD12 = QRPPDAD12;
        D1PPDAD22 = QRPPDAD22;
        D1PPDCTY2 = QRPPDCTY2;
        D1PPDST2  = QRPPDST2 ;
        D1PPDZIP2 = QRPPDZIP2;
        D1PPDPC#3 = QRPPDPC#3;
        //------------------------------
        Monitor;
           D1PPDPED3 = QRPPDPED3;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD3 = QRPPDPTD3;
        On-Error;
        EndMon;

        D1PPDPCN3 = QRPPDPCN3;
        //------------------------------
        D1PPDBP#3 = QRPPDBP#3;
        D1PPDAD13 = QRPPDAD13;
        D1PPDAD23 = QRPPDAD23;
        D1PPDCTY3 = QRPPDCTY3;
        D1PPDST3  = QRPPDST3 ;
        D1PPDZIP3 = QRPPDZIP3;
        D1PPDPC#4 = QRPPDPC#4;
        //------------------------------
        Monitor;
           D1PPDPED4 = QRPPDPED4;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD4 = QRPPDPTD4;
        On-Error;
        EndMon;

        D1PPDPCN4 = QRPPDPCN4;
        //------------------------------
        D1PPDBP#4 = QRPPDBP#4;
        D1PPDAD14 = QRPPDAD14;
        D1PPDAD24 = QRPPDAD24;
        D1PPDCTY4 = QRPPDCTY4;
        D1PPDST4  = QRPPDST4 ;
        D1PPDZIP4 = QRPPDZIP4;
        D1PPDPC#5 = QRPPDPC#5;
        //------------------------------
        Monitor;
           D1PPDPED5 = QRPPDPED5;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD5 = QRPPDPTD5;
        On-Error;
        EndMon;

        D1PPDPCN5 = QRPPDPCN5;
        //------------------------------
        D1PPDBP#5 = QRPPDBP#5;
        D1PPDAD15 = QRPPDAD15;
        D1PPDAD25 = QRPPDAD25;
        D1PPDCTY5 = QRPPDCTY5;
        D1PPDST55 = QRPPDST55;
        D1PPDZIP5 = QRPPDZIP5;
        D1PPDPC#6 = QRPPDPC#6;
        //------------------------------
        Monitor;
           D1PPDPED6 = QRPPDPED6;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD6 = QRPPDPTD6;
        On-Error;
        EndMon;

        D1PPDPCN6 = QRPPDPCN6;
        //------------------------------
        D1PPDBP#6 = QRPPDBP#6;
        D1PPDAD16 = QRPPDAD16;
        D1PPDAD26 = QRPPDAD26;
        D1PPDCTY6 = QRPPDCTY6;
        D1PPDST6  = QRPPDST6 ;
        D1PPDZIP6 = QRPPDZIP6;
        D1PPDPC#7 = QRPPDPC#7;
        //------------------------------
        Monitor;
           D1PPDPED7 = QRPPDPED7;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD7 = QRPPDPTD7;
        On-Error;
        EndMon;

        D1PPDPCN7 = QRPPDPCN7;
        //------------------------------
        D1PPDBP#7 = QRPPDBP#7;
        D1PPDAD17 = QRPPDAD17;
        D1PPDAD27 = QRPPDAD27;
        D1PPDCTY7 = QRPPDCTY7;
        D1PPDST7  = QRPPDST7 ;
        D1PPDZIP7 = QRPPDZIP7;
        D1PPDPC#8 = QRPPDPC#8;
        //------------------------------
        Monitor;
           D1PPDPED8 = QRPPDPED8;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD8 = QRPPDPTD8;
        On-Error;
        EndMon;

        D1PPDPCN8 = QRPPDPCN8;
        //------------------------------
        D1PPDBP#8 = QRPPDBP#8;
        D1PPDAD18 = QRPPDAD18;
        D1PPDAD28 = QRPPDAD28;
        D1PPDCTY8 = QRPPDCTY8;
        D1PPDST8  = QRPPDST8 ;
        D1PPDZIP8 = QRPPDZIP8;
        D1PPDPC#9 = QRPPDPC#9;
        //------------------------------
        Monitor;
           D1PPDPED9 = QRPPDPED9;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTD9 = QRPPDPTD9;
        On-Error;
        EndMon;

        D1PPDPCN9 = QRPPDPCN9;
        //------------------------------
        D1PPDBP#9 = QRPPDBP#9;
        D1PPDAD19 = QRPPDAD19;
        D1PPDAD29 = QRPPDAD29;
        D1PPDCTY9 = QRPPDCTY9;
        D1PPDST9  = QRPPDST9 ;
        D1PPDZIP9 = QRPPDZIP9;
        D1PPDPC#A = QRPPDPC#A;
        //------------------------------
        Monitor;
           D1PPDPEDA = QRPPDPEDA;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTDA = QRPPDPTDA;
        On-Error;
        EndMon;

        D1PPDPCNA = QRPPDPCNA;
        //------------------------------
        D1PPDBP#A = QRPPDBP#A;
        D1PPDAD1A = QRPPDAD1A;
        D1PPDAD2A = QRPPDAD2A;
        D1PPDCTYA = QRPPDCTYA;
        D1PPDSTA  = QRPPDSTA ;
        D1PPDZIPA = QRPPDZIPA;
        D1PPDPC#B = QRPPDPC#B;
        //------------------------------
        Monitor;
           D1PPDPEDB = QRPPDPEDB;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTDB = QRPPDPTDB;
        On-Error;
        EndMon;

        D1PPDPCNB = QRPPDPCNB;
        //------------------------------
        D1PPDBP#B = QRPPDBP#B;
        D1PPDAD1B = QRPPDAD1B;
        D1PPDAD2B = QRPPDAD2B;
        D1PPDCTYB = QRPPDCTYB;
        D1PPDSTB  = QRPPDSTB ;
        D1PPDZIPB = QRPPDZIPB;
        //------------------------------
        D1PPDPC#C = QRPPDPC#C;
        //------------------------------
        Monitor;
           D1PPDPEDC = QRPPDPEDC;
        On-Error;
        EndMon;

        Monitor;
           D1PPDPTDC = QRPPDPTDC;
        On-Error;
        EndMon;

        D1PPDPCNC = QRPPDPCNC;
        //------------------------------
        D1PPDBP#C = QRPPDBP#C;
        D1PPDAD1C = QRPPDAD1C;
        D1PPDAD2C = QRPPDAD2C;
        D1PPDCTYC = QRPPDCTYC;
        D1PPDSTC  = QRPPDSTC ;
        D1PPDZIPC = QRPPDZIPC;

       Endsr;
      /End-free
      ****** End Add ***** 10/11/21 ***********************************
      *** End   Chg *** 12/23/21 **************************************
      /End-free
      ****** End Add ***** 10/11/21 ***********************************
    
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK4 - Error checking for srceen 4 (SCR04)
    C*================================================================
     C     errck4        begsr
     C                   endsr
    C*
    C*================================================================
    C* ERRCK5 - Error checking for srceen 5 (SCR05)
    C*================================================================
     C     errck5        begsr
     C                   endsr
     C*** Beg Add *** 12/16/21 ****************************************
    C*================================================================
    C* chkfld - ITITIALIZE FIELD
    C*================================================================
     C     chkfld        begsr
     C
     C                   if        %subst(mrdata:20:8) = ' '
     C                   eval      %subst(mrdata:20:8) = '00000000'
     C                   endif
     c                   if        %subst(mrdata:177:8)=*blanks
     c                   eval      %subst(mrdata:177:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:185:8)=*blanks
     c                   eval      %subst(mrdata:185:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:193:8)=*blanks
     c                   eval      %subst(mrdata:193:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:201:8)=*blanks
     c                   eval      %subst(mrdata:201:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:214:8)=*blanks
     c                   eval      %subst(mrdata:214:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:222:8)=*blanks
     c                   eval      %subst(mrdata:222:8)='00000000'
     c                   endif
     C
     c                   if        %subst(mrdata:439:8)=*blanks
     c                   eval      %subst(mrdata:439:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:447:8)=*blanks
     c                   eval      %subst(mrdata:447:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:664:8)=*blanks
     c                   eval      %subst(mrdata:664:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:672:8)=*blanks
     c                   eval      %subst(mrdata:672:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:889:8)=*blanks
     c                   eval      %subst(mrdata:889:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:897:8)=*blanks
     c                   eval      %subst(mrdata:897:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1114:8)=*blanks
     c                   eval      %subst(mrdata:1114:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1122:8)=*blanks
     c                   eval      %subst(mrdata:1122:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1339:8)=*blanks
     c                   eval      %subst(mrdata:1339:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1347:8)=*blanks
     c                   eval      %subst(mrdata:1347:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1564:8)=*blanks
     c                   eval      %subst(mrdata:1564:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1572:8)=*blanks
     c                   eval      %subst(mrdata:1572:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1789:8)=*blanks
     c                   eval      %subst(mrdata:1789:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:1797:8)=*blanks
     c                   eval      %subst(mrdata:1797:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2014:8)=*blanks
     c                   eval      %subst(mrdata:2014:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2022:8)=*blanks
     c                   eval      %subst(mrdata:2022:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2239:8)=*blanks
     c                   eval      %subst(mrdata:2239:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2247:8)=*blanks
     c                   eval      %subst(mrdata:2247:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2464:8)=*blanks
     c                   eval      %subst(mrdata:2464:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2472:8)=*blanks
     c                   eval      %subst(mrdata:2472:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2689:8)=*blanks
     c                   eval      %subst(mrdata:2689:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2697:8)=*blanks
     c                   eval      %subst(mrdata:2697:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2914:8)=*blanks
     c                   eval      %subst(mrdata:2914:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:2922:8)=*blanks
     c                   eval      %subst(mrdata:2922:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3139:8)=*blanks
     c                   eval      %subst(mrdata:3139:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3147:8)=*blanks
     c                   eval      %subst(mrdata:3147:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3364:8)=*blanks
     c                   eval      %subst(mrdata:3364:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3372:8)=*blanks
     c                   eval      %subst(mrdata:3372:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3589:8)=*blanks
     c                   eval      %subst(mrdata:3589:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3597:8)=*blanks
     c                   eval      %subst(mrdata:3597:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3814:8)=*blanks
     c                   eval      %subst(mrdata:3814:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:3822:8)=*blanks
     c                   eval      %subst(mrdata:3822:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4039:8)=*blanks
     c                   eval      %subst(mrdata:4039:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4047:8)=*blanks
     c                   eval      %subst(mrdata:4047:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4264:8)=*blanks
     c                   eval      %subst(mrdata:4264:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4272:8)=*blanks
     c                   eval      %subst(mrdata:4272:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4489:8)=*blanks
     c                   eval      %subst(mrdata:4489:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4497:8)=*blanks
     c                   eval      %subst(mrdata:4497:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4714:8)=*blanks
     c                   eval      %subst(mrdata:4714:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4722:8)=*blanks
     c                   eval      %subst(mrdata:4722:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4939:8)=*blanks
     c                   eval      %subst(mrdata:4939:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:4947:8)=*blanks
     c                   eval      %subst(mrdata:4947:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:5164:8)=*blanks
     c                   eval      %subst(mrdata:5164:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:5172:8)=*blanks
     c                   eval      %subst(mrdata:5172:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:5389:8)=*blanks
     c                   eval      %subst(mrdata:5389:8)='00000000'
     c                   endif
     c                   if        %subst(mrdata:5397:8)=*blanks
     c                   eval      %subst(mrdata:5397:8)='00000000'
     c                   endif
     C                   endsr
     C*** End Add *** 12/16/21 ****************************************
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   endsr
    C*
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
     C                   endsr
    C*
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
    C*
     C                   endsr
    C*
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     c*
     C                   endsr
    C*
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C                   endsr
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDFLD        BEGSR
     C                   ENDSR
    C*
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   endsr
      *=============================================================================================
      * writeSDCMP - Add/Updates DARTS file
      *=============================================================================================
     c     writeSDCMP    begsr
     c********************************************
     c                   eval      dskey  = *blanks

     c                   eval      dsco#  = CMCO#
     c                   eval      dsfnd  = CMFND
     c                   eval      dsfyr  = CMFYR
     c                   eval      dscase = CMCASE

      * Find the user name from WTGTL file from code CXQRDU.

     C                   if        cxqrdu <> ' '
     C                   MOVEL     'AXDU'        XXTBID            4
     C                   MOVEL     CXQRDU        XXKEY            10
     C*** Beg Chg *** 12/18/10 **********************************
     C**** GTKY          CHAIN     PGT01
     C********           IF        %FOUND
     C********           MOVEL     GTUSER        XXUSER           10
     C********           ENDIF
     C                   select
     C                   when      CXQRDU = '01'
     C     CMCLEX        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER           10
     C                   ENDIF
     C                   when      CXQRDU = '02'
     C     CMSUPR        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   when      CXQRDU = '03'
     C     CMAMAN        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   when      CXQRDU = '04'
     C     CMUMAN        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   when      CXQRDU = '05'
     C     CMNRSE        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   when      CXQRDU = '06'
     C     CMFEAD        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   when      CXQRDU = '07'
     C     CMSDFA        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   when      CXQRDU = '08'
     C     CMESUP        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   endsl
     C*** End Chg *** 12/18/10 **********************************
     C                   else
     C     CXQRSU        chain     pup01
     C                   IF        %FOUND
     C                   MOVEL     upusr         XXUSER
     C                   ENDIF
     C                   endif                                                  cxqrdu <> ' '
      *
     C*** Beg Add *** 04/16/10 **********************************
     C     cxqrsd        chain     pct01                              90
     C                   if        *in90 = *off
     C                   movel     ctdesc        Q#COMM
     C                   movel     ctdepg        Q#DCID
     C                   movel     ctfl01        Q#DIFL
     C                   movel     dskey         Q#key
     C                   if        Q#DCID <> ' '
     C                   movel     Q#KEY         Q#FOLD
     C                   movel     'Y'           Q#FL01
     C                   else
     C                   eval      Q#FOLD =  ' '
     C                   eval      Q#FL01 =  ' '
     C                   endif
     C                   if        CTDPRG <> 0
     C                   movel     'Y'           Q#FL02
     C                   else
     C                   eval      Q#FL02 =  ' '
     C                   endif
     C                   endif
     C*
     C*** End Add *** 04/16/10 **********************************
     C                   time                    DSDTTM
     C                   EVAL      SRDTI = dsdate
     C                   call      'SRCVTDAT'    DATCHK
     C                   movel     SRDTO         XXIDT             8

     C*          Call API to write a DARTS Remark

     C                   CALL      'DA700R'
     C                   PARM      DSKEY         Q#KEY            40
     C*** Beg Chg *** 04/16/10 **********************************
     C******             PARM      *BLANKS       Q#COMM          256
     C                   PARM                    Q#COMM          256
     C*** End Chg *** 04/16/10 **********************************
     C                   PARM      XXIDT         Q#CDAT            8
     C*** Beg Chg *** 04/16/10 **********************************
     C******             PARM      *BLANKS       Q#DCID           12
     C                   PARM                    Q#DCID           12
     C*** End Chg *** 04/16/10 **********************************
     C                   PARM      'W0070R'      Q#PNAM           10
     C                   PARM      *BLANKS       Q#SECL            3
     C                   PARM      XXUSER        Q#USER           10
     C                   PARM      Q1PGM         Q#PGM            10
     C*** Beg Chg *** 04/16/10 **********************************
     C******             PARM      *BLANKS       Q#FOLD           80
     C******             PARM      *BLANKS       Q#DIFL            1
     C                   PARM                    Q#FOLD           80
     C                   PARM                    Q#DIFL            1
     C*** End Chg *** 04/16/10 **********************************
     C                   PARM      Q1PGM         Q#IPGM           10
     C*** Beg Chg *** 04/16/10 **********************************
     C******             PARM      *BLANKS       Q#FL01            1
     C******             PARM      *BLANKS       Q#FL02            1
     C                   PARM                    Q#FL01            1
     C                   PARM                    Q#FL02            1
     C*** End Chg *** 04/16/10 **********************************
     C                   PARM      CXQRSD        Q#RMCD            5
     C                   PARM      '00000'       Q#SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      *BLANKS       Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANKS       Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2

     c                   ENDSR                                                  writeWMCDP
      *
      *=============================================================================================
