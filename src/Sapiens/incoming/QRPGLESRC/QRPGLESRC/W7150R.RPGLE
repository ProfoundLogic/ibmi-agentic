     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W7150R - CLAIM PROFILE REPORT
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program produces a claims profile                 **
     H**                                                            **
     H**   CALLED FROM:                                             **
     H**     W6000C - Daily Batch Processing                        **
     H**                                                            **
     H**   CALLS TO:                                                **
     H**     NONE                                                   **
     H**                                                            **
     H**   PROGRAMMER:                                              **
     H**   CREATED:                  PROJECT:                       **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 03/11/14 C:282 PROJECT 129WSC Priss 28                        *
     H*                Aligning the 'Heading for Legal Reserve'       *
     H*                with the other reserves.                       *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltidt 129 only.                           *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use the new logical(s) to get the description  *
     H*                 by extension.                                 *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 06/02/04 C:53 PROJECT: WC1180                                 *
     H*               EXPAND FROM 2 DEC POS TO 4                      *
     H*                                                               *
     H* 10/20/03 C:53 Project: WC909                                  *
     H*               Added country info.                             *
     H*                                                               *
     H* 07/30/02 C:15  Support: WC045/33549                           *
     H*                Changed pgm to load the correct entry user.    *
     H*                                                               *
     H* 08/25/00 C:61  SUPPORT WC033 24081                            *
     H*              - Correct output of user name on routing page    *
     H*                                                               *
     H* 12/13/99 C:118 PROJECT: WC577                                 *
     H*                Change xxyr from 2.0 to 3.0 and modify output *
     H*                for 3 digits in clm#.                          *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H*  08/11/99  C:33  PROJECT: WC577   WC072/18973                 *
     H*                  LOWER THE EMPLOYER ADDRESS SO THAT IT WILL   *
     H*                  FIT IN AN ENVELOPE WINDOW.                   *
     H*                                                               *
     H*  07/26/99  C:33  SEQ#: WC090/18996                            *
     H*                  IF THE DOB IS NOT ENTERED CLEAR ALL FIELDS   *
     H*                  THAT HAVE ASSOCIATED VALUES WITH THE DOB.    *
     H*                                                               *
     H*  09/23/98  C:15  Support: WC088/13047                         *
     H*                  Changed XXSS# to alphanumeric.  This will    *
     H*                  prevent data decimal errors if the claim SS# *
     H*                  is not numeric.                              *
     H*                                                               *
     H*  09/14/98  C:15  Support: WC088/13047                         *
     H*                 -Fixed code which loads the daily rate.       *
     H*                 -Load the defense and plantiff attorney names *
     H*                  from the payee file, if blank.               *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*
     H*   03/10/98 C:58  SUPPORT: WC087 / sequence # 10555
     H*              Force "to case" parameter to all 9's if called
     H*              from daily batch.
     H*
     H*===============================================================
     H** REVISIONS before V3:                                       **
     H**  09/26/94 *** C:31 *** WC267 ***                           **
     H**           1. Replaced report with new format.              **
     H**           2. Added documentation.                          **
     H**           3. Added files WMPDL, WMPRL, WMUPL1.             **
     H**                                                            **
     H**  09/28/94 *** C:31 *** WC370 ***                           **
     H**           1. Added range selection support.                **
     H**           2. Added file WMCML1.                            **
     H**                                                            **
     H**  03/20/95 *** C:31 *** WC370 ***                           **
     H**           1. Added non print of zero dates.                **
     H**                                                            **
     H**  05/02/95 *** C:33 *** WC267 ***                           **
     H**           1. ADDED THREE SPACE TO EACH OF THE OUTPUT SPECS **
     H**              SO THAT IT WOULD BE READABLE ON A 4028 PRINTER**
     H**                                                            **
     H**  09/23/96 C:56  PROJECT: WC582                             **
     H**                 MOVE LEFT MARGIN OF CLAIM PROFILE REPORT   **
     H**                 2 BYTES TO THE RIGHT                       **
     H**                                                            **
     H**  10/18/96 C:62  PROJECT: WC587                             **
     H**                 ADDED RATE CODE TO GET PRDESC.             **
     H**                                                            **
     H**  05/15/97 C:62  PROJECT WC640                              **
     H**                 "CAT1" TO "LOCATION"; "CAT2" = CXCAT2;     **
     H*                  "ADJUSTER" TO CXADJ.                       **
     H**                                                            **
     H**  11/05/97 C:80  PROJECT WC746                              **
     H**                 CHANGED TO EXECUTE A SETLL AND READE       **
     H*                  UNTIL TOCASE IS PASSED.  CHANGED CODE TO   **
     H*                  GET DESCRIPTION FOR NCCI CODE.             **
     H*                                                             **
     H*  REVISIONS   ==V3==    Recent changes on top of list ======
     H*                                                             **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**                                                            **
     H**      11       First pass indicator                         **
     H**      25       Run selection from batch using data          **
     H**      42       No Record PFN01/WMFNL - Fund Master File     **
     H**      43       No Record PCD01/WMCDL - Claimant Demographic **
     H**      44       No Record PEM01/WMEML - Employer Master File **
     H**      50       General Usage                                **
     H**      51       Prints routing page once                     **
     H**      60       General Usage                                **
     H**      80       Print entry user                             **
     H**      90       Run date is zero                             **
     H**      91       Entry date is zero                           **
     H**      92       Received date is zero                        **
     H**      93       Date reported is zero                        **
     H**      94       Contact date is zero                         **
     H**      95       Date of birth is zero                        **
     H**      96       Accident date is zero                        **
     H**      97       Date last worked is zero                     **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H** FILES:                                                     **
     H**    SMCOL  (PC001): System Company File For Work Comp       **
     H**    WDF1L  (PF101): Fund Detail Record                      **
     H**    WMCDL  (PCD01): Claimant Demographics File              **
     H**    WMCML1 (PCM02): Claim File By Co/Fnd/Fyr/Case           **
     H**    WMCML3 (PCM01): Claim File By Entry Date                **
     H**    WMEML  (PEM01): Employer Master File                    **
     H**    WMFNL  (PFN01): Fund Master File                        **
     H**    WMLLL  (PLL01): Claim Legal File                        **
     H**    WMPDL  (PPD01): Payee Demographics                      **
     H**    WMPRL1 (PPR01): Payroll classification File             **
     H**    WMUPL1 (PUP02): User Parameters - Staff                 **
     H****************************************************************
     F*
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FSMORL     IF   E           K DISK
     FWMCXL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMCML3    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMPDL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWMEML     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMLLL     IF   E           K DISK
     F*** BEGIN DLT *** 09/14/98 **************************************
     F*****WMRSL   IF  E           K        DISK
     F*****WDF1L   IF  E           K        DISK
     F*** END DLT ***** 09/14/98 **************************************
     FWDRBL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F*** BEGIN ADD ***  08/11/98  ************************************
      **** BEGIN CHG **** 10/06/99 ************************************
     F******WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 10/06/99 ************************************
      **** BEG   CHG **** 04/20/09 ************************************
     F**************WMEPL7  IF  E           K        DISK
     FWMEPL7E   IF   E           K DISK
      **** END   CHG **** 04/20/09 ************************************
     F*** END ADD *****  08/11/98  ************************************
      **** BEGIN ADD **** 10/06/99 ************************************
     FWMRSL     IF   E           K DISK
      **** END   ADD **** 10/06/99 ************************************
     F*      Sustem Table Value File
     FQSYSPRT   O    F   82        PRINTER OFLIND(*INOF)
     F                                     USROPN
     F*

     D*
     D INC             S              9  2 DIM(4)                               INCURRED AMT
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D RTA             S              9  2 DIM(4)                               RESVR TYPE AMT
     D PTA             S              9  2 DIM(4)                               PAYMT TYPE AMT
     D TTL             S              9  2 DIM(4)                               TYPE TOTAL
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *** End Add ***  11/25/13  ******************************************************
     D*
     D*
     D* CLAIM EXAMINER HEADING
     D*
     D XXADJ           C                   CONST('Clm Ex             :')
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              RESERVE CODE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  PA                     1     90  2
     D                                     DIM(10)                              PAYMENT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RA                     1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D EPHONE          DS
     D  XXPHNA                 1      1    INZ('(')
     D  XXAREA                 2      4  0
     D  XXPHNB                 5      6    INZ(') ')
     D  XXPHN1                 7      9  0
     D  XXPHNC                10     10    INZ('-')
     D  XXPHN2                11     14  0
     D*** BEGIN ADD *** 09/23/98 ***************************************
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D*** BEGIN ADD *** 09/23/98 ***************************************
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***  BEG ADD  ***  11/25/13  *********************************
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      ***  END ADD  ***  11/25/13  *********************************
      *** Beg Add ***  03/11/14  ******************************************************
     D/copy sprString
      *** End Add ***  03/11/14  ******************************************************

     C/EJECT
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   PARTIAL KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDATE
      *** BEGIN ADD ***  08/11/98  ************************************
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   KFLD                    XXST
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    EPEFDT
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*** END ADD *****  06/01/06  ************************************
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      **** END   ADD **** 10/06/99 ************************************
      *
      * PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMSPEC
      *** BEG ADD *****  04/20/09  ************************************
     C                   KFLD                    CMEXT
      *** END ADD *****  04/20/09  ************************************
      *** END ADD *****  08/11/98  ************************************
     C*
     C*  KEY FOR CLAIMS FILE WHEN RUN ON DEMAND
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXFCAS
     C*
     C*  KEY FOR CLAIMS FILE WHEN RUN ON DEMAND
     C*
     C     CMKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE LEGAL MASTER
     C*
     C     LLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  KEY FOR SYSTEM TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  KEY FOR PAYROLL CLASS FILE - TO GET CLASS DESCRIPTION
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    CMCLAS
     C                   KFLD                    ELPTDT
     C*
     C*
     C     PRKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    CMCLAS
     C*
     C*
     C*  KEY FOR PAYEE DEMOGRAPHIC FILE - TO GET ATTORNEY NAMES
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  KEY FOR NCCI CODE TABLE FILE - TO GET DESCRIPTION
     C*
     C     NCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  KEY FOR NATURE CODE TABLE FILE - TO GET DESCRIPTION
     C*
     C     NAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY FOR PART CODE TABLE FILE - TO GET DESCRIPTION
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  KEY FOR CAUSE CODE TABLE FILE - TO GET DESCRIPTION
     C*
     C     CAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
     C*
     C*** BEGIN DLT *** 09/14/98 ***************************************
     C*  RATE SET CODE FILE
     C***********
     C***********RSKY      KLIST
     C***********          KFLD           XXCO#
     C***********          KFLD           XXRTCD  3
     C*** END DLT ***** 09/14/98 ***************************************
     C*
     C*  STATE COMP FILE
     C*
     C     RBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXSTE             2
     C                   KFLD                    XXEFDT
     C*
     C     RBKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXSTE             2
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE FUND  MASTER RECORD
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C*
     C*** BEGIN DLT *** 09/14/98 ***************************************
     C*   KEY LIST FOR THE FUND  DETAIL FILE
     C***********
     C***********FNKY1     KLIST
     C***********          KFLD           XXCO#
     C***********          KFLD           CMFND
     C***********          KFLD           CMFYR
     C*** END DLT ***** 09/14/98 ***************************************
     C*
     C*   KEY LIST FOR THE RESERVE TYPE TABLE FILE
     C*
     C     RTKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRKEY
     C*
     C*   CLAIMS/DARTS CONTROL KEY LIST
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** End Add ***  11/25/13  ******************************************************
     C*
     C/EJECT
     C*
     C     *LIKE         DEFINE    CMDT          XXDATE
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    CMDEFA        XXFEIN
     C     *LIKE         DEFINE    CMDEFM        XXMOD
     C     *LIKE         DEFINE    CXADJ         X1ADJ
      **** BEGIN ADD **** 10/06/99 ************************************
     C     *LIKE         DEFINE    PXST          XXST
      **** END   ADD **** 10/06/99 ************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQFCAS            7
     C                   PARM                    QQTCAS            7
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
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQFCAS        XXFCAS            7 0
     C                   MOVE      QQTCAS        XXTCAS            7 0
     C                   MOVE      *ALL'='       LINEX            75
     C*
     C*  RETRIEVE ADJUSTER HEADING FROM CLAIMS/DARTS CONTROL FILE.
     C*
     C     CXKY          CHAIN     PCX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     CXCAT2        ANDNE     *BLANKS
     C*
     C                   MOVEL     CXADJ         X1ADJ
     C                   ELSE
     C*
     C                   MOVEL     XXADJ         X1ADJ
     C*
     C                   ENDIF

     C*
     C* * * GET TIME DATE INFORMATION
     C                   TIME                    DSDTTM
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       charLGRH = *Blanks;
       *IN71 = *Off;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                charLGRH = CXlgrh;
             Else;
                charLGRH = 'LEGAL';
             EndIf;
       // *** Beg Add ***  03/11/14  ***************************************
             charLGRH = RightJstfy(charLGRH:10);
       // *** End Add ***  03/11/14  ***************************************
          *IN71 = *On;
          Endif;
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C/EJECT
     C*
     C*  DETERMINE IF RUN ON DEMAND OR IN DAILY BATCH
     C*
     C     QQFND         IFEQ      '000'
     C     QQFYR         ANDEQ     '000'
     C                   MOVE      '1'           *IN25                            25 = BATCH
     C*** BEGIN ADD **** 03/10/98 *************************************
     C                   MOVE      9999999       XXTCAS
     C*** END ADD ****** 03/10/98 *************************************
     C                   ELSE
     C                   MOVE      '0'           *IN25                           N25= DEMAND
     C                   END
     C*
     C   25CMKEY         CHAIN     PCM01                              60
     C  N25CMKEY2        SETLL     PCM02                                  60
     C  N25CMKEY3        READE     PCM02                                  60
     C*
     C*   IF NO RECORDS FOUND
     C*
     C     *IN60         IFEQ      *ON
     C     CMCASE        ORGT      XXTCAS
     C*
     C                   EXSR      SR1ST
     C                   EXSR      ROUT
     C                   EXCEPT    NOREC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C*   GET INFORMATION FOR EACH CASE NUMBER AND PRINT FORM
     C*
     C     *IN60         DOWEQ     '0'
     C     CMCASE        ANDLE     XXTCAS
     C*
     C                   Z-ADD     CMENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2DATE            6 0
     C*
     C                   Z-ADD     CMENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2DAT8            8 0
     C*
     C     CMENTD        COMP      XXDATE                                 80
     C*
     C  N11              EXSR      SR1ST
     C*
     C* * * DETAIL ROUTINE
     C*
     C* * * GET FUND NAME FROM FUND MASTER FILE
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C*
     C* If alternate name (FNANAM) isn't blank, print it in lieu of fund name.
     C*
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN45
     C*
     C* Center Group Name
     C*
     C                   MOVEL     FNANAM        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXANAM           50
     C*
     C*
     C                   ELSE
     C*
     C                   MOVEL     FNSHNM        XXFSNM           30
     C*
     C                   ENDIF                                                  FNANAM IFNE *BLANKS
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXFSNM
     C*
     C                   ENDIF                                                  *IN42 IFEQ *ZERO
     C*
     C*   CENTER FUND NAME
     C*
     C     XXFSNM        IFNE      *BLANKS
     C                   Z-ADD     *ZEROS        XXCNT             2 0
     C                   MOVE      *BLANK        XXWK1             1
     C                   MOVE      XXFSNM        XXWK30           30
     C     XXWK1         DOWEQ     *BLANK
     C                   MOVE      XXWK30        XXWK1
     C                   MOVEL     XXWK30        XXWK29           29
     C                   MOVE      XXWK29        XXWK30
     C                   ADD       1             XXCNT
     C                   ENDDO
     C     XXCNT         IFGT      1
     C     XXCNT         DIV       2             XXCNT
     C     XXCNT         DOULE     *ZERO
     C                   MOVEL     XXFSNM        XXWK29
     C                   MOVE      XXWK29        XXFSNM
     C                   MOVEL     ' '           XXFSNM
     C                   SUB       1             XXCNT
     C                   ENDDO
     C                   END
     C                   END
     C*
     C* * * GET FUND DETAIL INFORMATION * * *
     C*** BEGIN DLT *** 09/14/98 **************************************
     C********** FNKY1     CHAINPF101                50
     C**N50***** ELKY1     CHAINPEL01                50
     C**********           MOVE ELRTCD    XXRTCD
     C**********           MOVE CMACDT    XXEFDT
     C**N50***** RSKY      CHAINPRS01                50
     C**********           MOVE RSJRST    XXSTE
     C*** END DLT ***** 09/14/98 **************************************
     C*** BEGIN ADD *** 09/14/98 **************************************
     C                   MOVEL     CMJRST        XXSTE
     C                   MOVE      CMACDT        XXEFDT
     C*** END ADD ***** 09/14/98 **************************************
     C  N50RBKY          SETGT     PRB01
     C  N50RBKY2         READPE    PRB01                                  50
     C     *IN50         IFEQ      '1'
      *** BEG CHG *** 06/02/04 ****************************************
     C**********           MOVE *ZERO     RBREPC
     C                   MOVE      *ZERO         RBCMPR
      *** END CHG *** 06/02/04 ****************************************
     C                   MOVE      *ZERO         RBPACP
     C                   MOVE      *ZERO         RBMNRT
     C                   END
     C*
     C     CDKEY         CHAIN     PCD01                              43
     C     *IN43         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFMN            15            SAJ 5-12-88
     C                   MOVE      *BLANKS       QQFSN            15            |
     C                   MOVE      *BLANKS       QQMID            56            |
     C                   MOVE      '1'           QQTERM            1            |
     C                   MOVEL     CDFSN         QQFSN                          |
     C                   MOVEL     CDMID         QQMID                          |
     C                   CALL      'SRFRLS'                                     |
     C                   PARM                    QQMID                          |
     C                   PARM                    QQFSN                          |
     C                   PARM                    QQTERM                         |
     C                   MOVE      *BLANKS       QQFNAM           15            |
     C                   MOVEL     QQMID         QQFNAM                         |
     C*
     C                   MOVE      '1'           QQTERM                         |
     C                   MOVE      *BLANKS       QQLNAM                         |
     C                   MOVEL     CDLSN         QQLNAM           56            |
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME           36
     C*
     C*  ADD CLAIMANT ADDRESS
     C*
     C                   MOVE      *BLANKS       XCADR1           40
     C                   MOVE      *BLANKS       XCADR2           40
     C                   MOVE      *BLANKS       XCCSZ            40                        , ZIPCOD
     C                   MOVE      *BLANKS       XXSTZ            13
     C                   MOVE      *BLANKS       XXCITY           25
     C                   MOVE      *BLANKS       XXZIP1            5
     C                   MOVE      *BLANKS       XXZIP2            4
     C                   MOVEL     CDADR1        XCADR1
     C                   MOVEL     CDADR2        XCADR2
     C                   MOVEL     CDCTY         XXCITY
     C                   MOVEL     CDST          XXSTZ
     C                   MOVEL     CDZIP         XXZIP1
     C                   MOVE      CDZIP         XXZIP2
     C                   EXSR      LOADCZ
     C                   MOVEL     XXCSZ         XCCSZ
     C*** BEG ADD *** 10/20/03 *****************************
     C     CDCTRY        IFNE      *BLANKS
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        XCCSZ
     C                   MOVE      CDCTRY        XCCSZ
     C                   END
     C*** END ADD *** 10/20/03 *****************************
     C     XCADR2        IFEQ      *BLANKS
     C                   MOVE      XCCSZ         XCADR2
     C                   MOVE      *BLANKS       XCCSZ
     C                   END
     C*
     C*
     C*     CALL PGM TO CALCULATE AGE
     C*
     C     CDDTOB        IFNE      *ZERO
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         CDMDY             6 0
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTB8            8 0
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         C1MDY             8 0
     C*
     C                   MOVE      C1MDY         QQDOB             8 0
     C*
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      ' '           QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQDOB
     C                   PARM                    XXDT8
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   MOVE      QQDAYS        XXDAYS            5 0
     C     XXDAYS        DIV       365.25        XXAGE             3 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     *ZERO         XXAGE
     C***  BEGIN ADD  ***  07/26/99  *********************************
     C                   CLEAR                   CDMDY
     C                   CLEAR                   XXDTB8
     C                   CLEAR                   C1MDY
     C***  END   ADD  ***  07/26/99  *********************************
     C*
     C                   ENDIF                                                  CDDTOB<>*ZERO
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       XXNAME
     C*
     C                   ENDIF                                                  *IN43=*OFF
     C*
     C***** BEGIN CHG **** 12/13/99 ***********************************
     C**********           MOVE CMFYR     XXYR    20
     C                   MOVE      CMFYR         XXYR              3 0
     C***** END   CHG **** 12/13/99 ***********************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACD8            8 0
     C*
     C                   MOVEL     CMACDS        XXACDS           60
     C                   MOVEL     CMACD2        XXACD2           60
     C*
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTLW            6 0
     C*
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDLW8            8 0
     C*
     C   25ELKEY         CHAIN     PEL01                              50
     C  N25ELKY1         CHAIN     PEL01                              50
     C     *IN50         IFEQ      '0'
     C     PRKEY         SETGT     PPR01
     C     PRKEY1        READPE    PPR01                                  50
     C     *IN50         IFEQ      '1'
     C                   MOVEL     'BLANKS'      PRDESC
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN CHG *** 09/23/98 ***************************************
     C******************** MOVE CMSS#     XXSS#   90
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C*** END CHG ***** 09/23/98 ***************************************
     C*** BEGIN ADD ***  08/11/98  ************************************
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C     EPEXT         ANDNE     *BLANKS
      **** BEGIN ADD **** 10/06/99 ************************************
     C                   MOVE      *BLANKS       XXST
      **** END   ADD **** 10/06/99 ************************************
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL(P)  PXDESC        PRDESC
      **** BEGIN ADD **** 10/06/99 ************************************
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N61
     CANN60              MOVEL(P)  PXDESC        PRDESC
      **** END   ADD **** 10/06/99 ************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  08/11/98  ************************************
     C*
     C     EMKEY         CHAIN     PEM01                              44
     C     *IN44         IFEQ      '0'
     C*
     C*  ADD EMPLOYER ADDRESS
     C*
     C                   MOVEL     EMNAME        XXESNM           40
     C                   MOVE      *BLANKS       XMADR1           40
     C                   MOVE      *BLANKS       XMADR2           40
     C                   MOVE      *BLANKS       XMCSZ            40                        , ZIPCOD
     C                   MOVE      *BLANKS       XXSTZ            13
     C                   MOVE      *BLANKS       XXCITY           25
     C                   MOVE      *BLANKS       XXZIP1            5
     C                   MOVE      *BLANKS       XXZIP2            4
     C                   MOVEL     EMMAD1        XMADR1
     C                   MOVEL     EMMAD2        XMADR2
     C                   MOVEL     EMMCTY        XXCITY
     C                   MOVEL     EMMST         XXSTZ
     C                   MOVEL     EMMZIP        XXZIP1
     C                   MOVE      EMMZIP        XXZIP2
     C                   EXSR      LOADCZ
     C                   MOVEL     XXCSZ         XMCSZ
     C     XMADR2        IFEQ      *BLANKS
     C                   MOVE      XMCSZ         XMADR2
     C                   MOVE      *BLANKS       XMCSZ
     C                   END
     C*
     C*  ADD EMPLOYER PHONE
     C*
     C                   MOVE      EMAREA        XXAREA
     C                   MOVEL     EMPHN#        XXPHN1
     C                   MOVE      EMPHN#        XXPHN2
     C                   ELSE
     C                   MOVEL     *BLANKS       XXESNM
     C                   Z-ADD     *ZEROS        XXAREA
     C                   Z-ADD     *ZEROS        XXPHN1
     C                   Z-ADD     *ZEROS        XXPHN2
     C                   END
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRCVD            6 0
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCD8            8 0
     C*
     C                   Z-ADD     CMDTIC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTIC            6 0
     C*
     C                   Z-ADD     CMDTIC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTC8            8 0
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDTRP            6 0
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTP8            8 0
     C*
     C                   MOVE      *BLANKS       XXSTAT            6
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'CLOSED'      XXSTAT
     C                   END
     C     CMSTAT        IFEQ      'O'
     C                   MOVE      'OPEN'        XXSTAT
     C                   END
     C*
     C*  GET NCCI CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       GTDESC
     C*
     C*     - Must be in Table file
     C                   MOVEL(P)  CMNCCI        XXKEY
     C                   MOVE(P)   'NC'          XXTBID            4
     C*
     C*     - First try state table
     C                   MOVEL     CMJRST        XXTBID
     C     GTKY          CHAIN     PGT01                              50
     C*
     C*     - If not found, try generic NCCI table
     C     *IN50         IFEQ      '1'
     C     CMJRST        ANDNE     *BLANK
     C                   MOVEL     '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              50
     C                   ENDIF
     C*
     C*  GET NATURE CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMNATR        XXKEY
     C     NAKY1         CHAIN     PNA01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANKS       NADESC
     C                   END
     C*
     C*  GET PART DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMPART        XXKEY
     C     PAKY1         CHAIN     PPA01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANKS       PADESC
     C                   END
     C*
     C*  GET CAUSE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     CMCAUS        XXKEY
     C     CAKY1         CHAIN     PCA01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANKS       CADESC
     C                   END
     C*
     C*** BEGIN DLT *** 09/14/98 **************************************
     C*  GET DEFENSE ATTORNEY NAME
     C*
     C**********           MOVE *BLANK    XXDATT 35
     C**********           MOVE CMDEFA    XXFEIN
     C**********           MOVE CMDEFM    XXMOD
     C********** PDKEY     CHAINPPD01                50
     C********** *IN50     IFEQ '0'
     C**********           MOVE '1'       QQTERM
     C**********           MOVE *BLANKS   QQLNAM
     C**********           MOVE *BLANKS   QQFNAM
     C**********           MOVELPDNAME    QQLNAM 56
     C**********           MOVELPDFSN     QQFNAM 15
     C**********           CALL 'SRNAME'
     C**********           PARM           QQLNAM
     C**********           PARM           QQFNAM
     C**********           PARM           QQTERM
     C**********           MOVELQQLNAM    XXDATT
     C**********           END
     C*
     C*  GET PLAINTIFF ATTORNEY NAME
     C*
     C**********           MOVE *BLANK    XXPATT 35
     C**********           MOVE CMPLAA    XXFEIN
     C**********           MOVE CMPLAM    XXMOD
     C********** PDKEY     CHAINPPD01                50
     C********** *IN50     IFEQ '0'
     C**********           MOVE '1'       QQTERM
     C**********           MOVE *BLANKS   QQLNAM
     C**********           MOVE *BLANKS   QQFNAM
     C**********           MOVELPDNAME    QQLNAM 56
     C**********           MOVELPDFSN     QQFNAM 15
     C**********           CALL 'SRNAME'
     C**********           PARM           QQLNAM
     C**********           PARM           QQFNAM
     C**********           PARM           QQTERM
     C**********           MOVELQQLNAM    XXPATT
     C**********           END
     C*** END DLT ***** 09/14/98 **************************************
     C*
     C*  GET SUPERVISOR NAME
     C*
     C                   MOVE      *BLANK        XXSUPR           28
     C     CMSUPR        CHAIN     PUP02                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXSUPR
     C                   END
     C*
     C*  GET EXAMINER NAME
     C*
     C                   MOVE      *BLANK        XXCLEX           28
     C     CMCLEX        CHAIN     PUP02                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXCLEX
     C                   END
     C*
     C*  GET ADJUSTER NAME
     C*
     C                   MOVE      *BLANK        XXFEAD           28
     C     CMFEAD        CHAIN     PUP02                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXFEAD
     C                   END
     C*
     C* * * CALCULATE THE RATE.
     C*
     C*** BEGIN ADD *** 09/14/98 ****************************************
     C     CMCRT         IFEQ      *ZERO
     C*** END ADD ***** 09/14/98 ****************************************
      *** BEG CHG *** 06/02/04 ****************************************
     C********** RBREPC    DIV  100       XXREPC  54
     C********** XXREPC    MULT CMAWW     XXRATE  72H
     C     RBCMPR        DIV(H)    100           XXCMPR            7 4
     C     XXCMPR        MULT(H)   CMAWW         XXRATE            7 2
      *** BEG CHG *** 06/02/04 ****************************************
     C*
     C     XXRATE        IFGT      RBPACP
     C                   Z-ADD     RBPACP        XXRATE
     C                   END
     C*
     C     XXRATE        IFLT      RBMNRT
     C                   Z-ADD     RBMNRT        XXRATE
     C                   END
     C*
     C*** BEGIN ADD *** 09/14/98 ****************************************
     C                   Z-ADD     XXRATE        CMCRT
     C                   END
     C*** END ADD ***** 09/14/98 ****************************************
     C*
     C* * * CALCULATE THE DAILY AMOUNT.
     C*
     C*** BEGIN ADD *** 09/14/98 ****************************************
     C     CMDAYS        IFEQ      *ZERO
     C                   Z-ADD     5             CMDAYS
     C                   ENDIF
     C*** END ADD ***** 09/14/98 ****************************************
     C     CMCRT         IFGT      *ZERO
     C     CMDAYS        ANDGT     *ZERO
     C     CMCRT         DIV       CMDAYS        XXDALY            7 2
     C                   END
     C*
     C*  GET DEFENSE AND PLAINTIFF ATTY NAMES
     C*
     C                   MOVEL     *BLANKS       XXDEFN           30
     C                   MOVEL     *BLANKS       XXPLAN           30
     C     LLKY          CHAIN     PLL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     LLDEFN        XXDEFN           30
     C                   MOVEL     LLPLAN        XXPLAN           30
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C*** BEGIN ADD *** 09/14/98 **************************************
     C     XXDEFN        IFEQ      *BLANKS
     C     LLDEFA        ANDNE     *ZEROS
     C                   MOVE      LLDEFA        XXFEIN
     C                   MOVE      LLDEFM        XXMOD
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL(P)  PDNAME        XXDEFN
     C     PDFSN         IFNE      *BLANKS
     C                   CAT       ',':0         XXDEFN
     C                   CAT       PDFSN:1       XXDEFN
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXPLAN        IFEQ      *BLANKS
     C     LLPLAA        ANDNE     *ZEROS
     C                   MOVE      LLPLAA        XXFEIN
     C                   MOVE      LLPLAM        XXMOD
     C     PDKEY         CHAIN     PPD01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL(P)  PDNAME        XXPLAN
     C     PDFSN         IFNE      *BLANKS
     C                   CAT       ',':0         XXPLAN
     C                   CAT       PDFSN:1       XXPLAN
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 09/14/98 **************************************
     C*
     C*  PAID AND RESERVE CATEGORY TOTALS
     C*
     C                   Z-ADD     *ZEROS        RTA
     C                   Z-ADD     *ZEROS        PTA
     C                   DO        10            X                 5 0
     C     RT(X)         IFGT      *BLANK
     C                   MOVEL     RT(X)         XXRKEY           10
     C*
     C     RTKY1         CHAIN     PRT01                              50
     C     RTTYPE        IFEQ      'C'
     C                   ADD       RA(X)         INC(C)
     C                   ADD       PA(X)         INC(C)
     C                   ADD       RA(X)         RTA(C)
     C                   ADD       PA(X)         PTA(C)
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   ADD       RA(X)         INC(M)
     C                   ADD       PA(X)         INC(M)
     C                   ADD       RA(X)         RTA(M)
     C                   ADD       PA(X)         PTA(M)
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   ADD       RA(X)         INC(L)
     C                   ADD       PA(X)         INC(L)
     C                   ADD       RA(X)         RTA(L)
     C                   ADD       PA(X)         PTA(L)
     C                   END
     C     RTTYPE        IFEQ      'E'
     C                   ADD       RA(X)         INC(E)
     C                   ADD       PA(X)         INC(E)
     C                   ADD       RA(X)         RTA(E)
     C                   ADD       PA(X)         PTA(E)
     C                   END
     C*
     C                   END
     C                   ENDDO
     C*
     C                   XFOOT     RTA           XXRTOT            9 2
     C                   XFOOT     PTA           XXPTOT            9 2
     C                   XFOOT     INC           XXTOTL            9 2
     C     X1DATE        COMP      *ZEROS                                 90
     C     X2DATE        COMP      *ZEROS                                 91
     C     XXRCVD        COMP      *ZEROS                                 92
     C     XXDTRP        COMP      *ZEROS                                 93
     C     XXDTIC        COMP      *ZEROS                                 94
     C     CDMDY         COMP      *ZEROS                                 95
     C     XXACDT        COMP      *ZEROS                                 96
     C     XXDTLW        COMP      *ZEROS                                 97
     C*
     C*
     C* * * PRINT THE FORM.
     C  N51              EXSR      ROUT
     C                   MOVE      *ON           *IN51
     C                   EXCEPT    DETL
     C*
     C* * *  CLEAR ALL WORK FIELDS FOR USER LINE
     C                   MOVE      *ZERO         INC
     C                   MOVE      *ZERO         XXTOTL
     C*                    END
     C*
     C*
     C   25CMKEY         READE     PCM01                                  60
     C  N25CMKEY3        READE     PCM02                                  60
     C*
     C                   MOVE      *OFF          *IN45
     C*
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
       xxPrt = '0';
       Chain ORKY POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C/EJECT
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   ROUT          BEGSR
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF
     C                   MOVE      *ON           *INOF
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C* * * SETUP WORK FIELDS
     C                   Z-ADD     1             C                 5 0
     C                   Z-ADD     2             M                 5 0
     C                   Z-ADD     3             L                 5 0
     C                   Z-ADD     4             E                 5 0
     C*
     C*  GET THE COMPANY RECORD.
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C* * * OPEN PRINT FILES
     C*
     C                   OPEN      QSYSPRT
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*  LOADCZ--SUBROUTINE TO LOAD THE CITY, STATE, & ZIP CODE
     C*==================================================================*
     C     LOADCZ        BEGSR
     C                   MOVE      *BLANKS       XXCSZ            40            =CITY, STATE, ZIPCOD
     C                   MOVEA     XXSTZ         SZ
     C                   MOVEA     XXZIP1        SZ(4)
     C                   MOVE      XXZIP2        XXZP2             4 0
     C     XXZP2         IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVEA     XXZIP2        SZ(10)
     C                   END
     C*
     C                   MOVEA     SZ            XXSTZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXCITY        XXLAST
     C                   MOVEL     XXSTZ         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C*
     C                   ENDSR
     O/EJECT
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      ***** BEG DEL ******* 08/25/00 **********************************
     O******* E  1             ROUTE
     O*******                            15 'ROUTE TO:'
     O*******                  UPUSNM    67
     O******* E  1             ROUTE
      ***** END DEL ******* 08/25/00 **********************************
      ***** BEG ADD ******* 08/25/00 **********************************
      *
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
      **********
     O**********E            ROUTE          1
     O**********             ORDIS2              60
      **********
     O**********E            ROUTE          1
     O**********             ORDIS3              60
      **********
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      **********
      ***** END ADD ******* 08/25/00 **********************************
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
      **********
     O          E            ROUTE          1
     O                       ORDIS2              60
      **********
     O          E            ROUTE          1
     O                       ORDIS3              60
      **********
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          1
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          1
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          1
     O                                           18 'POLICY YEAR:'
     O                       XXFYR               23
     O                                           49 'CASE NUMBER RANGE:'
     O                       QQFCAS              57
     O                                           59 '-'
     O                       QQTCAS              67
     O**   SELECTION CRITERIA
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS, INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
     O*
     OQSYSPRT   E            DETL           1 03
     O              N45      XXFSNM              56
     O               45      XXANAM              66
     O*
     O          E            NOREC             4
     O                                           46 'CLAIM PROFILE'
     O          E            NOREC          1  7
     O                       LINEX               81
     O          E            NOREC          1
     O                                           26 'CLAIMANT INFORMATION'
     O          E            NOREC          1
     O                       LINEX               81
     O          E            NOREC            31
     O                                           39 'NO RECORDS MEET'
     O                                           58 'SELECTION CRITERIA'
     O*
     O          E            DETL           2
     O                                           46 'CLAIM PROFILE'
     O          E            DETL           1
     O                                           12 'GROUP:'
     O                       FNFND               16
     O                                           18 '-'
     O*                        FNNAME    79
     O              N45      XXFSNM              56
     O               45      XXANAM              66
     O          E            DETL           1
     O                       LINEX               81
     O          E            DETL           1
     O                                           26 'CLAIMANT INFORMATION'
     O          E            DETL           1
     O                       LINEX               81
     O          E            DETL           1
     O                                           12 'CLM #:'
     O                       CMFND               17
     O                                           18 '-'
     O***** BEGIN CHG **** 12/13/99 ***********************************
     O**********               XXYR      20
     O**********                         21 '-'
     O**********               CMCASE    28
     O                       XXYR                21
     O                                           22 '-'
     O                       CMCASE              29
     O***** END   CHG **** 12/13/99 ***********************************
     O                                           69 'RUN DATE   :'
     O              N90      XXDT8               80 '  /  /    '
     O          E            DETL           1
     O                                           12 'SS#  :'
     O*** BEGIN DLT *** 09/23/98 ***************************************
     O************************ XXSS#     25 ' 0 -  -    '
     O*** END DLT ***** 09/23/98 ***************************************
     O*** BEGIN ADD *** 09/23/98 ***************************************
     O                       XXSS1               17
     O                                           18 '-'
     O                       XXSS2               20
     O                                           21 '-'
     O                       XXSS3               25
     O*** END ADD ***** 09/23/98 ***************************************
     O                                           69 'ENTRY DATE :'
     O              N91      X2DAT8              80 '  /  /    '
     O          E            DETL           1
     O                                           12 'CLMNT:'
     O                       XXNAME              50
     O*** Begin dlt *** 07/30/02 ***************************************
     O**************** 80                71 'ENTRY USER :'
     O**************** 80      CMUSER    80
     O*** End dlt ***** 07/30/02 ***************************************
     O*** Begin add *** 07/30/02 ***************************************
     O                                           69 'ENTRY USER :'
     O                       CMIUSR              80
     O*** End add ***** 07/30/02 ***************************************
     O          E            DETL           1
     O                       XCADR1              54
     O                                           69 'RECEIVED DT:'
     O              N92      XXRCD8              80 '  /  /    '
     O          E            DETL           1
     O                       XCADR2              54
     O                                           69 'DT REPORTED:'
     O              N93      XXDTP8              80 '  /  /    '
     O          E            DETL           2
     O                       XCCSZ               54
     O                                           69 'CONTACT DT :'
     O              N94      XXDTC8              80 '  /  /    '
     O          E            DETL           2
     O                                           10 'DOB:'
     O              N95      XXDTB8              22 '  /  /    '
     O                                           33 'AGE:'
     O                       XXAGE         Z     36
     O          E            DETL           1
     O                                           20 'ACCIDENT DATE:'
     O              N96      XXACD8              32 '  /  /    '
     O          E            DETL           1
     O                                           20 'ACCIDENT TIME:'
     O                       CMACTM              30 '  :  :  '
     O          E            DETL           1
     O                                           20 'ACCIDENT DESC:'
     O                       XXACDS              80
     O          E            DETL           2
     O                       XXACD2              80
     O          E            DETL           1
     O                                           21 'LOST TIME     :'
     O                       CMLTFL              24
     O          E            DETL           2
     O                                           21 'DT LAST WORKED:'
     O              N97      XXDLW8              32 '  /  /    '
     O          E            DETL           2
     O                                           17 'JOB CLASS :'
     O                       CMCLAS              25
      **** BEG   ADD **** 04/20/09 ************************************
     O                       CMSPEC              30
     O                       CMEXT               36
      **** END   ADD **** 04/20/09 ************************************
      **** BEG   CHG **** 04/20/09 ************************************
     O*************            PRDESC    57
     O                       PRDESC              67
      **** END   CHG **** 04/20/09 ************************************
     O          E            DETL           1
     O                                           17 'NCCI  CODE:'
     O                       CMNCCI              25
     O                       GTDESC              67
     O          E            DETL           1
     O                                           17 'NATR  CODE:'
     O                       CMNATR              25
     O                       NADESC              57
     O          E            DETL           1
     O                                           17 'PART  CODE:'
     O                       CMPART              25
     O                       PADESC              57
     O          E            DETL           1
     O                                           17 'CAUSE CODE:'
     O                       CMCAUS              25
     O                       CADESC              57
     O          E            DETL           2
     O                                           17 'STATUS    :'
     O                       XXSTAT              25
     O          E            DETL           1
     O                                           12 'AWW  :'
     O                       CMAWW         3     25
     O                                           49 'Supervisor         :'
     O                       CMSUPR              53
     O                       XXSUPR              82
     O          E            DETL           1
     O                                           12 'RATE :'
     O                       CMCRT         3     25
     O                       X1ADJ               48
     O                                           49 ':'
     O                       CMCLEX              53
     O                       XXCLEX              82
     O          E            DETL           2
     O                                           12 'DAILY:'
     O                       XXDALY        3     25
     O                                           49 'Outside Adjuster   :'
     O                       CMFEAD              53
     O                       XXFEAD              82
     O          E            DETL           1
     O                                           25 'FEIN'
     O                                           32 'MOD'
     O                                           49 'ATTORNEY NAME'
     O          E            DETL           1
     O                                           15 'DEF ATTY:'
     O                       LLDEFA        Z     25
     O                       LLDEFM        Z     32
     O                       XXDEFN              66
     O          E            DETL           2
     O                                           15 'PLF ATTY:'
     O                       LLPLAA        Z     25
     O                       LLPLAM        Z     32
     O                       XXPLAN              66
     O          E            DETL           1
     O                       LINEX               81
     O          E            DETL           1
     O                                           25 'RESERVE INFORMATION'
     O          E            DETL           1
     O                       LINEX               81
     O          E            DETL           1
     O                                           28 'COMPENSATION'
     O                                           41 'MEDICAL'
      *** Beg Add ***  11/25/13  ******************************************************
     O*****                                      54 'LEGAL'
     O              N71                          54 'LEGAL'
     O               71      charLGRH            54
      *** End Add ***  11/25/13  ******************************************************
     O                                           67 'EXPENSE'
     O                                           80 'TOTAL'
     O          E            DETL           1
     O                                           14 'RESV:'
     O                       RTA(C)        J     28
     O                       RTA(M)        J     41
     O                       RTA(L)        J     54
     O                       RTA(E)        J     67
     O                       XXRTOT        J     80
     O          E            DETL           1
     O                                           14 'PAID:'
     O                       PTA(C)        J     28
     O                       PTA(M)        J     41
     O                       PTA(L)        J     54
     O                       PTA(E)        J     67
     O                       XXPTOT        J     80
     O          E            DETL           1
     O                                           28 '----------'
     O                                           41 '----------'
     O                                           54 '----------'
     O                                           67 '----------'
     O                                           80 '----------'
     O          E            DETL           2
     O                                           14 'TOT INC:'
     O                       INC(C)        J     28
     O                       INC(M)        J     41
     O                       INC(L)        J     54
     O                       INC(E)        J     67
     O                       XXTOTL        J     80
     O          E            DETL           1
     O                       LINEX               81
     O          E            DETL           1
     O                                           26 'EMPLOYER INFORMATION'
     O          E            DETL           1
     O                       LINEX               81
     O          E            DETL           1
     O                                           14 'EMP/DIV:'
     O                       CMEMP#              24
     O                                           25 '/'
     O                       CMDIV               30
     O***  BEGIN ADD  ***  08/11/99  *********************************
     O********E* 1             DETL
     O          E            DETL        2  1
     O***  END   ADD  ***  08/11/99  *********************************
     O                       XXESNM              56
     O          E            DETL           1
     O                       XMADR1              56
     O          E            DETL           1
     O                       XMADR2              56
     O          E            DETL           1
     O                       XMCSZ               56
     O          E            DETL           1
     O                       EPHONE              30
