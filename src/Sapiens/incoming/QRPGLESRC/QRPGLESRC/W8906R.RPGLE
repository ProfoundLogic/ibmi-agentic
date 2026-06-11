     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8906R - WORK FILE BUILD - WHLOP
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - DIVISION LOCATIONS FILE - WHLOP            *
     H*                                                               *
     H* CREATED: 04/04/00  C:61                                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/04/11  C:191  Project: WC1856 PRISS #10                    *
     H*                  Make processing backward compatible          *
     H*                                                               *
     H* 11/25/10  C:220  Project: WC1856                              *
     H*                  Implement WA726R API to retrieve policy      *
     H*                  start policy year of the group.              *
     H*                                                               *
     H* 08/12/09 C:33  Project: CI077                                 *
     H*                Expand name.                                   *
     H*                                                               *
     H* 01/23/08 C:61  SUPPORT CI132 61860                            *
     H*                - Add mailing address fields.                  *
     H*                                                               *
     H* 07/25/06 C: 132   CI031                                       *
     H*           - Remove the code to insert Carrier Policy number   *
     H*             in WHLOP/PRLONR field, use Grp-Emp#-Fyr as Policy *
     H*             number.                                           *
     H*                                                               *
     H* 08/12/05 C: 132 CI093 - 47984                                 *
     H*             Retrieve the Client ID and if is KRMS (093) then  *
     H*             bypass the check on WDEHP and always write record *
     H*                                                               *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 11/15/04 C: 132 CI021                                         *
     H*             Add check on WDEHP file when a policy start date  *
     H*             in WDELL is in the future. If no WDEHP record     *
     H*             for future start dates, do not write the record.  *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHLOP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909     (ADDED 03/12/04)       *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 10/09/03 C:132 PROJECT:  CI010                                *
     H*                - ADD incoming Parm Date, Use for ASOF Date    *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*
     H* 05/15/00  C:58 PROJECT WC924
     H*                 ADD field: LOAODT - Data AS OF date.
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * Entity Location
     FWMALL     IF   E           K DISK
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     F*
      * County
     FWTCNL1    IF   E           K DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
      *
      * Output - Division Locations
     FWHLOP     O    E             DISK
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * Employer Detail History
     FWDEHL1    IF   E           K DISK
      ****** END ADD ***** 11/15/04 ************************************
      *
      ****** BEG DEL ***** 07/25/06 ************************************
     F**** Begin Add ****  07/22/05  **************************************
     F******   Employer Carrier File
     F******WMRPL   IF  E           K        DISK
     F**** End Add ******  07/22/05  **************************************
      ****** END DEL ***** 07/25/06 ************************************
     F*

     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
     D*
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(50)                              HEX CNVT ARRY
      ***** END ADD ***** 02/27/04 ************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      **** BEGIN ADD *** 05/15/00 ************************************
     D*
     D*    8 and 10 digit dates, numeric and character
     D                 DS
     D  DN                     1      8
     D                                     DIM(8)                               8 digit num
     D  D8X                    1      8
     D  D8N                    1      8  0
     D                 DS
     D  DX                     1     10
     D                                     DIM(10)                              10 dig edited
     D  D10X                   1     10
     D  D10N                   1     10  0
     D*
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
      **** END ADD ***** 05/15/00 ************************************
      *
      *===========================================================
      *
      *  PLIST

     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      ****** BEG ADD ***** 11/15/04 ************************************
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
      ****** END ADD ***** 11/15/04 ************************************
      *
      ***** BEG CHG ******* 10/09/03 **********************************
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8            mmddccyy
      ***** END CHG ******* 10/09/03 **********************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
      *
      *  KLIST
      *
      *  Employer
      *
     C     EMKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    ALFND
     C                   KFLD                    ALEMP#
     C                   KFLD                    ALDIV
      *
      *  Table
      *
     C     TBKY          KLIST
     C                   KFLD                    ALCO#
     C                   KFLD                    KKKEY
      *
      *  Additional Entitiy / Location
      *
     C     ALKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C**** END ADD ***** 06/10/00 ************************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C*
      ****** BEG ADD ***** 11/15/04 ************************************
     C**** Begin Add ****  07/22/05  **************************************
      *
      ****** BEG DEL ***** 07/25/06 ************************************
     C***********RPKEY     KLIST
     C***********          KFLD           ELCO#
     C***********          KFLD           ELFND
     C***********          KFLD           ELEMP#
     C***********          KFLD           ELDIV
     C***********          KFLD           ELFYR
      ****** END DEL ***** 07/25/06 ************************************
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
      *
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Check if WA726R exists in environment
     C                   MOVEL(P)  'WA726R'      XXPGM
     C                   MOVEL     '*PGM'        XXTYP             7
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM            10
     C                   PARM                    XXTYP
     C                   PARM                    XXOLDF            1
     C* If XXOLDF = '1' then use old process
     C* If XXOLDF = '0' then use new API WA726R
     C*** END   ADD ****** 01/04/11 ************************************
     C***** BEG ADD ******* 11/25/10 **********************************
     C                   MOVEL     @@STYR        @@YRS             3
     C***** END ADD ******* 11/25/10 **********************************
     C                   TIME                    DSDTTM
     C                   MOVEL     QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ELSE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDIF                                                  SRERR
     C*
      ****** END ADD ***** 11/15/04 ************************************
      *
      *===========================================================
      *
      *   *LIKE defines
      *
     C     *LIKE         DEFINE    CNKEY         KKKEY
     C**** BEGIN ADD *** 06/10/00 ************************************
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
     C     *LIKE         DEFINE    ELPFDT        XXDATE
      ****** END ADD ***** 11/15/04 ************************************
      *
      *===========================================================
      *  Begin
      *===========================================================
      *
      *  READ Employer policy year file
      *
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C***** BEG ADD ******* 11/25/10 **********************************
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Process using API only if exists in environment. Else assume old process
     C     XXOLDF        IFEQ      '0'
     C*** END   ADD ****** 01/04/11 ************************************
     C*
     C* Retrieve start policy year based on extarct start date.
     C*
     C                   MOVEL     *ZEROS        @@STYR
     C                   MOVEL     *BLANKS       XXSTYR
     C                   MOVEL     ELCO#         X1CO#             3
     C                   MOVEL     ELFND         X1FND             3
     C*
     C                   CALL      'WA726R'
     C                   PARM                    X1CO#
     C                   PARM                    X1FND
     C                   PARM                    @@YRS
     C                   PARM                    XXSTYR            3
     C                   PARM                    @@RTCD            1
     C*
     C     @@RTCD        IFEQ      'Y'
     C*
     C                   MOVEL     XXSTYR        @@STYR            3 0
     C                   ELSE
     C*
     C                   MOVEL     000           @@STYR
     C                   ENDIF
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C                   ENDIF                                                  XXOLDF='0'
     C*** END   ADD ****** 01/04/11 ************************************
     C***** END ADD ******* 11/25/10 **********************************
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*   Get group master.
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   ENDIF                                                  .CO/FND <>
     C*
     C     FNTEST        IFEQ      'Y'
     C**** BEGIN ADD **** 10/02/01 *************************************
     C     FNCICF        ORNE      'Y'
     C**** END ADD ****** 10/02/01 *************************************
     C*
     C                   GOTO      NEXTEL
     C                   ELSE
     C**** END ADD ***** 06/10/00 ************************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *OFF
     C                   GOTO      NEXTEL
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
      *
     C**** BEGIN DLT *** 06/10/00 ************************************
     C********** ELCO#     IFNE *ZERO
     C********** ELFND     ANDNE999
     C**** END DELETE ** 06/10/00 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ELFYR         IFLT      @@STYR
     C                   GOTO      NEXTEL
     C                   ENDIF                                                   FYR>=@@STYR
     C***** END ADD ******* 03/10/04 **********************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * If Policy start date > run date, check Detail History to see if
      * has been billed (WDEHP), bypass if no recordin WDEHP.
     C     ELPFDT        IFGT      XXDATE
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *ON
      ****** BEG ADD ***** 08/12/05 ************************************
     C     CLTID         ANDNE     '093'
      ****** END ADD ***** 08/12/05 ************************************
     C                   GOTO      NEXT
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                   ELPFDT<>XXDATE
      ****** END ADD ***** 11/15/04 ************************************
      *
      *  READ Entity Location file
      *
     C     ALKY          CHAIN     PAL01                              60
     C     *IN60         DOWEQ     '0'
      *
     C**** BEGIN DLT *** 06/10/00 ************************************
     C********** ALCO#     IFEQ *ZERO
     C********** ALFND     OREQ 999
     C**********           GOTO NEXT
     C**********           ENDIF                           ALCO# EQ ZERO
     C**** END DELETE ** 06/10/00 ************************************
      *
     C     ALEFDT        IFGE      ELPTDT
     C                   GOTO      NEXT
     C                   ENDIF                                                  ALEFDT
      *
     C     ALDLDT        IFNE      *ZERO
     C     ALDLDT        ANDLE     ELPFDT
     C                   GOTO      NEXT
     C                   ENDIF                                                  ALCO# EQ ZERO
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C     ALKY          READE     PAL01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  ELCO#
      *
     C**** BEGIN ADD *** 06/10/00 ************************************
     C     NEXTEL        TAG
     C                   Z-ADD     ELCO#         SVCO#
     C                   Z-ADD     ELFND         SVFND
     C**** END ADD ***** 06/10/00 ************************************
     C                   READ      PEL01                                  60
     C                   ENDDO
      *
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * SRPHN - Format phone number for output
      *================================================================
     C     SRPHN         BEGSR
      *
     C     XXAREA        IFNE      '000'
      *
     C                   MOVEL(P)  '('           XXPHON           20
     C                   CAT(P)    XXAREA:0      XXPHON
     C                   CAT(P)    ')':0         XXPHON
     C                   CAT(P)    XXPH1:1       XXPHON
     C                   CAT(P)    '-':0         XXPHON
     C                   CAT(P)    XXPH2:0       XXPHON
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       XXPHON
     C                   ENDIF
      *
     C                   ENDSR                                                  SRPHN
      *
      *
      **** BEGIN ADD *** 05/15/00 ************************************
     C*================================================================
     C*      EDDAT      EDIT DATE  8 digit to MM/DD/YYYY
     C*================================================================
     C     EDDAT         BEGSR
     C*
     C                   Z-ADD     1             W                 2 0
     C                   DO        10            X                 2 0
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           DX(X)
     C                   ELSE
     C                   MOVEL     DN(W)         DX(X)
     C                   ADD       1             W
     C                   ENDIF                                                  .X=3/6
     C*
     C                   ENDDO                                                  .DO 10
     C*
     C                   ENDSR
     C*
     C*================================================================
      **** END ADD ***** 05/15/00 ************************************
      *
      *================================================================
      * SROUT - Write record to output file WHLOP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PLO01
      *
      *  Policy Number
      *
      ****** BEG DEL ***** 07/25/06 ************************************
     C*****Begin*Add ****  07/22/05  **************************************
     C***********          MOVEL*BLANKS   LOPONR    P
     C***********CLTID     IFEQ '135'
     C***********RPKEY     CHAINPRP01                62
     C************IN62     IFEQ *OFF
     C***********RPCAP#    ANDNE*BLANK
     C***********          MOVELRPCAP#    LOPONR    P
     C***********          ENDIF                           e 62 OFF...&
     C***********          ENDIF                           e CLTID=135
     C***********LOPONR    IFEQ *BLANK
     C*****End*Add ******  07/22/05  **************************************
      ****** END DEL ***** 07/25/06 ************************************
      *
     C                   MOVEL(P)  ALFND         LOPONR
     C                   MOVEL(P)  ALEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       LOPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       LOPONR
      ****** BEG DEL ***** 07/25/06 ************************************
     C**** Begin Add ****  07/22/05  **************************************
     C**************       ENDIF                           e PONR=BLANK
     C**** End Add ******  07/22/05  **************************************
      ****** END DEL ***** 07/25/06 ************************************
      *
     C                   Z-ADD     ALDIV         LODIV
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALELID        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALELID
      ***** END ADD ***** 02/27/04 ************************************
     C                   MOVEL(P)  ALELID        LOLOC
      *
      *  Address info
      *
      ***** BEG DEL ******* 01/23/08 **********************************
     C*********  ALPAD1    IFNE *BLANKS
      ***** END DEL ******* 01/23/08 **********************************
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALPAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALPAD1
      *
     C     EMPAD2        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALPAD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALPAD2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALPCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALPCTY
      ***** END ADD ***** 02/27/04 ************************************
     C                   MOVEL(P)  ALPAD1        LOADR1
     C                   MOVEL(P)  ALPAD2        LOADR2
     C                   MOVEL(P)  ALPCTY        LOCTY
     C                   MOVEL(P)  ALPST         LOST
      *
     C                   MOVEL     ALPZIP        XXZIP5            5
     C                   MOVE      ALPZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         LOZIP
     C                   CAT(P)    XXZIP4:0      LOZIP
      *
      ***** BEG DEL ******* 01/23/08 **********************************
     C******               MOVE 'P'       LOTYPE
      ******
     C******               ELSE
      ******
      ******BEG ADD ***** 02/27/04 ************************************
      **Clear array then Check for Invalid Hex characters
     C******               MOVEA*BLANKS   HX
     C******               MOVEAALMAD1    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        ALMAD1
      ******
     C******     ALMAD2    IFGT *BLANKS
     C******               MOVEA*BLANKS   HX
     C******               MOVEAALMAD2    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        ALMAD2
     C******               ENDIF
      ******
     C******               MOVEA*BLANKS   HX
     C******               MOVEAALMCTY    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        ALMCTY
      ******END ADD ***** 02/27/04 ************************************
      ******
     C******               MOVELALMAD1    LOADR1    P
     C******               MOVELALMAD2    LOADR2    P
     C******               MOVELALMCTY    LOCTY     P
     C******               MOVELALMST     LOST      P
      ******
     C******               MOVELALMZIP    XXZIP5  5
     C******               MOVE ALMZIP    XXZIP4  4
     C******     XXZIP5    CAT  '-':0     LOZIP     P
     C******               CAT  XXZIP4:0  LOZIP     P
      ******
     C******               MOVE 'M'       LOTYPE
     C******               ENDIF                           ALPAD1
      ***** END DEL ******* 01/23/08 **********************************
      ***** BEG ADD ******* 01/23/08 **********************************
     C                   MOVE      *BLANKS       LOTYPE
      *
      * Clear array then Check for Invalid Hex characters
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALMAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALMAD1
      *
     C     ALMAD2        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALMAD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALMAD2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALMCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALMCTY
      *
     C                   MOVEL(P)  ALMAD1        LOMAD1
     C                   MOVEL(P)  ALMAD2        LOMAD2
     C                   MOVEL(P)  ALMCTY        LOMCTY
     C                   MOVEL(P)  ALMST         LOMST
      *
     C                   MOVEL     ALMZIP        XXZIP5            5
     C                   MOVE      ALMZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         LOMZIP
     C                   CAT(P)    XXZIP4:0      LOMZIP
      ***
      *
     C     LOADR1        IFNE      *BLANKS
     C                   MOVE      'P'           LOMPFL
     C                   ELSE
     C                   MOVE      'M'           LOMPFL
     C                   ENDIF
      *
     C     LOADR1        IFEQ      *BLANK
     C                   MOVE      LOMAD1        LOADR1
     C                   MOVE      LOMAD2        LOADR2
     C                   MOVE      LOMCTY        LOCTY
     C                   MOVE      LOMST         LOST
     C                   MOVE      LOMZIP        LOZIP
     C                   ENDIF
      *
     C     LOMAD1        IFEQ      *BLANK
     C                   MOVE      LOADR1        LOMAD1
     C                   MOVE      LOADR2        LOMAD2
     C                   MOVE      LOCTY         LOMCTY
     C                   MOVE      LOST          LOMST
     C                   MOVE      LOZIP         LOMZIP
     C                   ENDIF
      *
      ***** END ADD ******* 01/23/08 **********************************
      *
      * Get County description
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     EMPST         IFNE      *BLANKS
     C     EMPST         CAT       EMCNTY:0      ZZKEY             4
      *
     C                   ELSE
      *
     C     EMMST         CAT       EMCNTY:0      ZZKEY
     C                   ENDIF                                                  SCPST<>*BLANKS
      *
     C                   MOVEL(P)  ZZKEY         KKKEY
     C     TBKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        LOCNTY
     C                   ENDIF                                                  *IN60 = *OFF
      *
      * Fax and phone numbers
      *
     C                   MOVE      ALFARA        XXAREA            3
     C                   MOVEL     ALFAX#        XXPH1             3
     C                   MOVE      ALFAX#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        LOFAX
      *
     C                   MOVE      ALAREA        XXAREA            3
     C                   MOVEL     ALPHN#        XXPH1             3
     C                   MOVE      ALPHN#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        LOPHN
      *
      *
     C                   Z-ADD     ALCO#         LOCO#
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALNAME        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALNAME
      ***** END ADD ***** 02/27/04 ************************************
     C***  Begin Add  ***  08/12/09  **************************************
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     ALNAM2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            ALNAM2
     C***  End   Add  ***  08/12/09  **************************************
     C*
     C***  Begin Chg  ***  08/12/09  **************************************
     C**********           MOVELALNAME    LONAME    P
     C     ALNAME        CAT(P)    ALNAM2        LONAME
     C***  End   Chg  ***  08/12/09  **************************************
      *
      **** BEGIN ADD *** 05/15/00 ************************************
      *      As of date
     C                   MOVEL     *BLANKS       D10X
      ****** BEG DEL ***** 11/15/04 ************************************
     C*************        TIME           DSDTTM
      ****** END DEL ***** 11/15/04 ************************************
      ***** BEG CHG **** 10/09/03 ********************************
      * Change the Parm Date into the AS OF Date for output record
     C                   MOVEL     QQDATE        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
     C*************        Z-ADDDSDATE    SRDTI
      ***** END CHG **** 10/09/03 ********************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
     C                   ENDIF
     C                   MOVE      D10X          LOAODT
      **** END ADD ***** 05/15/00 ************************************
      *
      *
      *
      *  Write to output file
      *
     C                   WRITE     PLO01
     C                   ENDSR                                                  SROUT
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *         The Alpha field is loaded into the arrary w/MOVEA
      *         then Loop thru array & replace w/blank if out of
      *         is not a valid HEX Character. After exiting subr
      *         the array is moved back to the alpha field w/MOVEA
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        50            K                 3 0
      *
     C     HX(K)         IFLT      X'40'
     C     HX(K)         OREQ      X'41'
     C     HX(K)         ORGT      X'F9'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SROUT
      *================================================================
      ***** END ADD ***** 02/27/04 ************************************
