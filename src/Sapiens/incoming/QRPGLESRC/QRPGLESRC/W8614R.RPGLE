     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W8614R - Duplicate Payment Print / Download Program
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     Prints Dup Pmt Report from workfile during Daily        **
     H**     Batch procedure and Group Check Batch process.          **
     H**     Builds file of report data for download.                **
     H**     Also prints Demand Report.                              **
     H**     Input PARM indicates whether to Print, Download, or Both**
     H**     Shows detail of all dups found. Page break by Adjuster, **
     H**     Reserve Type and Grand Total. Gives total of WARPP      **
     H**     records that have Dups or Possible Dups when called from**
     H**     batch. Gives count of all dups on demand run.           **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W8614C                                                  **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C: 30                                         **
     H**   CREATED  12/26/95   Project: WC457                        **
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/01/21 C:318 Support: 94631                                 *
     H*                Mask SSN                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 03/11/14 C:282 PROJECT 129WSC Priss 28                        *
     H*                Aligning the 'Heading for Legal Reserve'       *
     H*                with the other reserves.                       *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/23/01 C:61  SUPPORT 110AQ  WC113  26014                    *
     H*                Include suffix when listing output file        *
     H*                library.  PDSRPTxxx                            *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                                                               *
     H*  01/26/00 C:118 PROJECT: WC577                                *
     H*           C:61  ADD GROUP TO ROUTING PAGE.                    *
     H*                                                               *
     H*  12/18/97 C:30 Project: WC577                                 *
     H*           Re-arrange Routing lines and Headings lines to      *
     H*           same presentation as prompt screen.                 *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   INDICATOR            FUNCTION DESCRIPTION                   *
     H*                                                               *
     H*      26           LOKUP INDICATOR                             *
     H*      31           COMP RESERVE TYPE                           *
     H*      32           MEDICAL RESERVE TYPE                        *
     H*      33           EXPENSE RESERVE TYPE                        *
     H*      34           LEGAL RESERVE TYPE                          *
     H*      39           ALL RESERVE TYPE SELECTED                   *
     H*      40           NO VALID RECORDS FOR REPORT                 *
     H*      41           CNTRL PRINTING OF FUND NAME LINE            *
     H*      42           BATCH TYPE = GROUP BATCH                    *
     H*      43           CNTRL LINE PRINTING                         *
     H*      44           DEMAND REPORT                               *
     H*      60-62        GENERAL CHAINING                            *
     H*      80           INPUT PARM AMOUNTS ARE ZERO                 *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**                                                             **
     H**    WXDU2L (PDU02): KEY (40 BYTES)                          **
     H**    WFDUP  (PDU01): Download File                           **
     H**    SMCOL  (PCO01): CO#                                      **
     H*     SMORL  (POR01): ORPNAM,ORFIL#                            **
     H**    WMUPL  (PUP01): USER                                     **
     H**    WMFNL  (PFN01): CO#/FND                                  **
     H**    WMUPL1 (PUP02): STAFF                                    **
     H**    WTABL  (PB201): CO#/KEY                                  **
     H*****************************************************************
     F*
     F* Workfile
     FWXDU2L    IF   E           K DISK
     F*
     F* Download file
     FWFDUP     O    E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Table File
     FWTABL     IF   E           K DISK
     F*
     F* Batch Routing File
     FSMORL     IF   E           K DISK
     F*
     F* Company File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter File
     FWMUPL     IF   E           K DISK
     F*
     F* User Parameter File
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *** Beg Add ***  11/25/13  ***********************************************
     FWMCXL     IF   E           K DISK
      *** End Add ***  11/25/13  ***********************************************
     F*
     F* Duplicate Payments Report
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F*

     D*****************************************************************
     D R               S              1    DIM(4)
     D RD              S              1    DIM(4)
     D RS              S             12    DIM(4)
     D TABA            S              1    DIM(4) CTDATA PERRCD(4) ASCEND
     D TABB            S             12    DIM(4) ALT(TABA)
     D N               S             12    DIM(4) CTDATA PERRCD(1)
     D DS              S             20    DIM(2) CTDATA PERRCD(1)
     D TO              S             30    DIM(3) CTDATA PERRCD(1)
     D*****************************************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXKEY                  1     40
     D  XCO#                   1      3  0
     D  XSORT                  4      4
     D  XFND                   5      7  0
     D  XADJ                   8     10
     D  XFYR                  11     13  0
     D  XCASE                 14     20  0
     D  XFILE                 21     21
     D  XSEQ#                 22     26  0
     D***  Begin Chg  ***  03/01/21  *******************************
     D* XFEIN                 27     35  0
     D  XFEIN                 27     35
     D***  End Chg  ***  03/01/21  *******************************
     D  XMOD                  36     38  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***  11/25/13  ***********************************************
     D charLGRH        s             10
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************
      *** Beg Add ***  03/11/14  ***********************************************
     D/copy sprString
      *** End Add ***  03/11/14  ***********************************************

     C*****************************************************************
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    UPUSNM        XXUSNM
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    B2KEY         XXKY
     C     *LIKE         DEFINE    DU2TDT        XXDT
     C*
     C*****************************************************************
     C*   KEY LISTINGS
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   Key List for the Fund Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   Key List for the Table file
     C*
     C     B2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKY
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY2         KLIST
     C                   KFLD                    XADJ
     C*
     C****************************************************************
      *** Beg Add ***  11/25/13  ***********************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** End Add ***  11/25/13  ***********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE1            8
     C                   PARM                    QQDTE2            8
     C                   PARM                    QQOTYP            1
     C                   PARM                    QQTDUP            1
     C                   PARM                    QQAMT1           11
     C                   PARM                    QQAMT2           11
     C                   PARM                    QQTYP             1
     C                   PARM                    QQRST1            1
     C                   PARM                    QQRST2            1
     C                   PARM                    QQRST3            1
     C                   PARM                    QQRST4            1
     C                   PARM                    QQRUN             1
     C                   PARM                    QQDATE            8
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C****************************************************************
     C*** Beg Add ***  11/25/13  ***********************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ***********************************************
     C*
     C                   Z-ADD     *ZEROS        XXADUP            3 0
     C                   Z-ADD     *ZEROS        XXRDUP            3 0
     C                   Z-ADD     *ZEROS        XXGDUP            3 0
     C                   Z-ADD     *ZEROS        XXDATE            6 0
     C                   Z-ADD     *ZEROS        X1DATE            8 0
     C                   Z-ADD     *ZEROS        XXDT
     C                   Z-ADD     *ZEROS        X1DTE1            8 0
     C                   Z-ADD     *ZEROS        X1DTE2            8 0
     C                   Z-ADD     *ZEROS        XXDTE1            6 0
     C                   Z-ADD     *ZEROS        XXDTE2            6 0
     C                   Z-ADD     *ZEROS        XXAMT1           11 2
     C                   Z-ADD     *ZEROS        XXAMT2           11 2
     C                   Z-ADD     *ZEROS        X                 1 0
     C                   Z-ADD     *ZEROS        Y                 1 0
     C                   Z-ADD     *ZEROS        Z                 1 0
     C                   MOVE      '1'           XXPRT             1
     C                   MOVE      *BLANKS       XXCNAM           20
     C                   MOVE      *BLANKS       XXPNAM           20
     C                   MOVE      *BLANKS       XXTITL           60
     C                   MOVE      *BLANKS       XXRSV            12
     C                   MOVE      *BLANKS       XXRSV1           12
     C                   MOVE      *BLANKS       XXRSV2           12
     C                   MOVE      *BLANKS       XXRSV3           12
     C                   MOVE      *BLANKS       XXRSV4           12
     C                   MOVE      *BLANKS       XX1STT            8
     C                   MOVE      *BLANKS       XX2STT            8
     C*
     C                   MOVE      QQRST1        R(1)
     C                   MOVE      QQRST2        R(2)
     C                   MOVE      QQRST3        R(3)
     C                   MOVE      QQRST4        R(4)
     C                   MOVEA     'CMEL'        RD
     C*
     C                   Z-ADD     *ZERO         SVFYR             3 0
     C                   Z-ADD     *ZERO         SVCASE            7 0
     C                   Z-ADD     *ZERO         SVSEQ#            5 0
     C*
     C                   SELECT
     C     QQOTYP        WHENEQ    'F'
     C                   Z-ADD     1             X1                1 0
     C     QQOTYP        WHENEQ    'P'
     C                   Z-ADD     2             X1                1 0
     C     QQOTYP        WHENEQ    'B'
     C                   Z-ADD     3             X1                1 0
     C                   ENDSL
     C*
     C                   SELECT
     C     QQTDUP        WHENEQ    'Y'
     C                   Z-ADD     1             X2                1 0
     C     QQTDUP        WHENEQ    'N'
     C                   Z-ADD     2             X2                1 0
     C                   ENDSL
     C*
     C     QQOTYP        COMP      'F'                                    55
     C     QQOTYP        COMP      'P'                                    56
     C*
      ***** BEG ADD ******* 01/23/01 **********************************
      *
     C     *IN56         IFEQ      *OFF
      *
      *  Get output library/file name
      *
     C                   CALL      'SA030R'
     C                   PARM                    ##EXT             3
      *
     C     'PDSRPT'      CAT(P)    ##EXT:0       XXOFIL           30
     C                   CAT(P)    '/WFDUP':0    XXOFIL
     C                   ENDIF
      *
      ***** END ADD ******* 01/23/01 **********************************
      *
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        X1DATE
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C     QQTYP         COMP      'G'                                    42
     C     QQRUN         COMP      'D'                                    44
     C                   MOVE      QQDTE1        X1DTE1
     C*
     C                   Z-ADD     X1DTE1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDTE1
     C*
     C                   MOVE      QQDTE2        X1DTE2
     C*
     C                   Z-ADD     X1DTE2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   MOVE      QQAMT1        XXAMT1
     C                   MOVE      QQAMT2        XXAMT2
     C*
     C     XXAMT1        IFEQ      *ZERO
     C     XXAMT2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN80
     C                   ENDIF
     C*
     C*  Get the Company
     C*
     C     COKY          CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*  Get the Fund
     C*
     C                   MOVE      QQFND         XXFND
      ***** BEGIN ADD **** 01/26/00 ***********************************
     C     XXFND         COMP      *ZERO                                  71
      ***** END ADD ****** 01/26/00 ***********************************
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF
     C                   ENDIF
     C***** END   ADD **** 04/10/00 ***********************************
     C*** Beg Add ***  11/25/13  ***********************************************
      /Free
       charLGRH = *Blanks;
       *IN72 = *Off;
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
             *IN72 = *On;
          Endif;
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ***********************************************
     C*
     C*  GET THE USER NAME
     C*
     C     UPKY          CHAIN     WMUPL                              60
     C*
     C*  GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL
     C                   MOVEL     ORTITL        XXTITL
     C                   END
     C*
     C*  PRINT THE DISTRIBUTION INFORMATION
     C*
     C                   Z-ADD     1             Z
     C                   DO        4             X
     C                   Z-ADD     1             Y
     C     R(X)          LOOKUP    RD(Y)                                  26
     *** Beg Chg ***  11/25/13  ************************************************
     C                   IF        *In72 = *On And *In26 = *On And Y = 4
     C                   Eval      RS(Z) = charLGRH
     C                   Else
     *** End Chg ***  11/25/13  ************************************************
     C   26              MOVEL     N(Y)          RS(Z)
     *** Beg Chg ***  11/25/13  ************************************************
     C                   EndIf
     *** End Chg ***  11/25/13  ************************************************
     C   26              ADD       1             Z
     C   2650            ADD       Y             NN                3 0
     C   26              MOVE      *ON           *IN(NN)
     C                   END
     C*
     C     QQRST1        IFEQ      *BLANKS
     C     QQRST2        ANDEQ     *BLANKS
     C     QQRST3        ANDEQ     *BLANKS
     C     QQRST4        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN39
     C                   MOVE      *ON           *IN51
     C                   MOVE      *ON           *IN52
     C                   MOVE      *ON           *IN53
     C                   MOVE      *ON           *IN54
     C                   END
     C*
      ***** Beg Del ***** 08/26/14 ********************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ********************************************
     C*
     C*  READ WORKFILE
     C*
     C                   READ      PDU02                                  60
     C*
     C                   MOVE      DUKEY         XXKEY
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN40
     C                   GOTO      NORCDS
     C                   ELSE
     C                   Z-ADD     XFND          SVFND             3 0
     C                   MOVEL     XADJ          SVADJ             3
     C                   MOVEL     XSORT         SVSORT            1
     C                   END
     C*
     C  N55              EXCEPT    HEAD
     C*
     C*  Check for Reserve Types
     C*
     C     XSORT         COMP      '1'                                    31
     C     XSORT         COMP      '2'                                    32
     C     XSORT         COMP      '3'                                    33
     C     XSORT         COMP      '4'                                    34
     C*
     C     *IN34         IFEQ      '1'
     C   51              MOVEL     N(1)          XXRSV
     C  N55
     CAN 51              EXCEPT    RSVHD
     C  N55
     CAN 51              EXCEPT    NODATA
     C  N55
     CAN 51              EXCEPT    HEAD
     C*
     C   52              MOVEL     N(2)          XXRSV
     C  N55
     CAN 52              EXCEPT    RSVHD
     C  N55
     CAN 52              EXCEPT    NODATA
     C  N55
     CAN 52              EXCEPT    HEAD
     C*
     C   53              MOVEL     N(3)          XXRSV
     C  N55
     CAN 53              EXCEPT    RSVHD
     C  N55
     CAN 53              EXCEPT    NODATA
     C  N55
     CAN 53              EXCEPT    HEAD
     C                   END
     C*
     C     *IN33         IFEQ      '1'
     C   51              MOVEL     N(1)          XXRSV
     C  N55
     CAN 51              EXCEPT    RSVHD
     C  N55
     CAN 51              EXCEPT    NODATA
     C  N55
     CAN 51              EXCEPT    HEAD
     C*
     C   52              MOVEL     N(2)          XXRSV
     C  N55
     CAN 52              EXCEPT    RSVHD
     C  N55
     CAN 52              EXCEPT    NODATA
     C  N55
     CAN 52              EXCEPT    HEAD
     C                   END
     C*
     C     *IN32         IFEQ      '1'
     C   51              MOVEL     N(1)          XXRSV
     C  N55
     CAN 51              EXCEPT    RSVHD
     C  N55
     CAN 51              EXCEPT    NODATA
     C  N55
     CAN 51              EXCEPT    HEAD
     C                   END
     C*
     C*  Get Reserve Desc
     C*
     *** Beg Add ***  11/25/13  ************************************************
     C     XSORT         IFEQ      '4'
     C     CLTID         ANDEQ     '129'
     C                   MOVEL(P)  charLGRH      XXRSV
     C                   ELSE
     *** End Add ***  11/25/13  ************************************************
     C     XSORT         LOOKUP    TABA          TABB                     26
     C     *IN26         IFEQ      '1'
     C                   MOVEL     TABB          XXRSV
     C                   END
     *** Beg Add ***  11/25/13  ************************************************
     C                   ENDIF
     *** End Add ***  11/25/13  ************************************************
     C                   MOVE      XSORT         SVSORT
     C*
     C  N55              EXCEPT    RSVHD
     C*
     C*  Get Adjuster Name
     C*
     C     UPKY2         CHAIN     PUP02                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C*
     C  N55              EXCEPT    ADJHD
     C  N55              EXCEPT    HEAD1
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     DUPNAM        XXPNAM
     C                   MOVEL     DUCNAM        XXCNAM
     C*
     C*  For a given Payment, only print Case Demographics on First Detail line
     C*
     C     XFYR          IFNE      SVFYR
     C     XCASE         ORNE      SVCASE
     C     XSEQ#         ORNE      SVSEQ#
     C                   MOVE      '1'           *IN43
     C                   Z-ADD     XFYR          SVFYR
     C                   Z-ADD     XCASE         SVCASE
     C                   Z-ADD     XSEQ#         SVSEQ#
     C                   ELSE
     C                   MOVE      '0'           *IN43
     C                   ENDIF
     C*
     C*  CHECK FOR CHANGE IN RESERVE TYPE
     C*
     C     XSORT         IFNE      SVSORT
     C  N55              EXCEPT    ADJTOT
     C                   ADD       XXADUP        XXRDUP
     C                   CLEAR                   XXADUP
     C  N55              EXCEPT    HEAD
     C  N55              EXCEPT    RSVHD
     C*
     C     XXRDUP        IFEQ      *ZEROS
     C  N55              EXCEPT    NODATA
     C                   ELSE
     C  N55              EXCEPT    RSVTOT
     C                   ADD       XXRDUP        XXGDUP
     C                   CLEAR                   XXRDUP
     C                   END
     C*
     C  N55              EXCEPT    HEAD
     C*
     C*  Check for missing Reserve codes
     C*
     C     XSORT         IFEQ      '3'
     C     SVSORT        ANDEQ     '1'
     C   52              MOVE      N(2)          XXRSV
     C  N55
     CAN 52              EXCEPT    RSVHD
     C  N55
     CAN 52              EXCEPT    NODATA
     C  N55
     CAN 52              EXCEPT    HEAD
     C                   END
     C*
     C     XSORT         IFEQ      '4'
     C     SVSORT        ANDEQ     '1'
     C   52              MOVE      N(2)          XXRSV
     C  N55
     CAN 52              EXCEPT    RSVHD
     C  N55
     CAN 52              EXCEPT    NODATA
     C  N55
     CAN 52              EXCEPT    HEAD
     C   53              MOVE      N(3)          XXRSV
     C  N55
     CAN 53              EXCEPT    RSVHD
     C  N55
     CAN 53              EXCEPT    NODATA
     C  N55
     CAN 53              EXCEPT    HEAD
     C                   END
     C*
     C     XSORT         IFEQ      '4'
     C     SVSORT        ANDEQ     '2'
     C   52              MOVE      N(2)          XXRSV
     C  N55
     CAN 52              EXCEPT    RSVHD
     C  N55
     CAN 52              EXCEPT    NODATA
     C  N55
     CAN 52              EXCEPT    HEAD
     C                   END
     C*
     C*  Get new Reserve description
     C*
     C*** Beg Add ***  11/25/13  ***********************************************
     C     XSORT         IFEQ      '4'
     C     CLTID         ANDEQ     '129'
     C                   MOVEL(P)  charLGRH      XXRSV
     C                   ELSE
     C*** End Add ***  11/25/13  ***********************************************
     C     XSORT         LOOKUP    TABA          TABB                     26
     C     *IN26         IFEQ      '1'
     C                   MOVEL     TABB          XXRSV
     C                   END
     C*** Beg Add ***  11/25/13  ***********************************************
     C                   ENDIF
     C*** End Add ***  11/25/13  ***********************************************
     C                   MOVE      XSORT         SVSORT
     C*
     C  N55              EXCEPT    RSVHD
     C*
     C*  Get new Adjuster Name
     C*
     C     UPKY2         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C  N55              EXCEPT    ADJHD
     C                   MOVEL     XADJ          SVADJ
     C  N55              EXCEPT    HEAD1
     C                   END
     C*
     C*  CHECK FOR ADJUSTER CHANGE
     C*
     C     XADJ          IFNE      SVADJ
     C  N55              EXCEPT    ADJTOT
     C                   ADD       XXADUP        XXRDUP
     C                   CLEAR                   XXADUP
     C                   MOVEL     XADJ          SVADJ
     C*
     C*  Get new Adjuster Name
     C*
     C     UPKY2         CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   ELSE
     C                   MOVE      *BLANKS       XXUSNM
     C                   END
     C  N55              EXCEPT    HEAD
     C  N55              EXCEPT    RSVHD
     C  N55              EXCEPT    ADJHD
     C  N55              EXCEPT    HEAD1
     C                   END
     C*
     C*  PRINT DETAIL LINE
     C*
     C*  Convert Workfile Dates
     C*
     C                   Z-ADD     DU1SDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XX1SDT            6 0
     C*
     C                   Z-ADD     DU1FDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XX1FDT            6 0
     C*
     C                   Z-ADD     DU1TDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XX1TDT            6 0
     C*
     C                   Z-ADD     DU2SDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XX2SDT            6 0
     C*
     C                   Z-ADD     DU2FDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XX2FDT            6 0
     C*
     C                   Z-ADD     DU2TDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XX2TDT            6 0
     C*
     C*  Determine status
     C*
     C                   SELECT
     C     DU1STT        WHENEQ    'R'
     C                   MOVEL     'RELEASED'    XX1STT
     C     DU1STT        WHENEQ    'P'
     C                   MOVEL     'PENDED  '    XX1STT
     C     DU1STT        WHENEQ    'Y'
     C                   MOVEL     'PAID    '    XX1STT
     C                   OTHER
     C                   MOVE      *BLANKS       XX1STT
     C                   ENDSL
     C*
     C                   SELECT
     C     DU2STT        WHENEQ    'R'
     C                   MOVEL     'RELEASED'    XX2STT
     C     DU2STT        WHENEQ    'P'
     C                   MOVEL     'PENDED  '    XX2STT
     C     DU2STT        WHENEQ    'Y'
     C                   MOVEL     'PAID    '    XX2STT
     C                   OTHER
     C                   MOVE      *BLANKS       XX2STT
     C                   ENDSL
     C*
     C     QQOTYP        IFNE      'P'
     C                   EXSR      SRWRIT
     C                   ENDIF
     C*
     C***  Begin Add  ***  03/01/21  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'XFEIN'       qqname           25
     C     XFEIN         parm      XFEIN         qqval           100
     C***  End Add  ***  03/01/21  *******************************
     C  N55              EXCEPT    DETAIL
     C***  Begin Add  ***  03/01/21  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'XFEIN'       qqname           25
     C     XFEIN         parm      XFEIN         qqval           100
     C***  End Add  ***  03/01/21  *******************************
     C*
     C     *INOF         IFEQ      '1'
     C  N55              EXCEPT    HEAD
     C  N55              EXCEPT    RSVHD
     C  N55              EXCEPT    ADJHD
     C  N55              EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* Accumulate Totals
     C*
     C                   ADD       1             XXADUP
     C*
     C* READ NEXT WORKFILE RECORD
     C*
     C     NXT           TAG
     C*
     C                   READ      PDU02                                  60
     C                   MOVE      DUKEY         XXKEY
     C*
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C  N55              EXCEPT    ADJTOT
     C                   ADD       XXADUP        XXRDUP
     C  N55              EXCEPT    HEAD
     C  N55              EXCEPT    RSVHD
     C*
     C     XXRDUP        IFEQ      *ZEROS
     C  N55              EXCEPT    NODATA
     C                   ELSE
     C  N55              EXCEPT    RSVTOT
     C                   ADD       XXRDUP        XXGDUP
     C                   END
     C*
     C  N55              EXCEPT    HEAD
     C*
     C*  Check for missing Reserve Types
     C*
     C     XSORT         IFEQ      '1'
     C   52              MOVEL     N(2)          XXRSV
     C  N55
     CAN 52              EXCEPT    RSVHD
     C  N55
     CAN 52              EXCEPT    NODATA
     C  N55
     CAN 52              EXCEPT    HEAD
     C   53              MOVEL     N(3)          XXRSV
     C  N55
     CAN 53              EXCEPT    RSVHD
     C  N55
     CAN 53              EXCEPT    NODATA
     C  N55
     CAN 53              EXCEPT    HEAD
     *** Beg Add ***  11/25/13  ************************************************
     C*****   54              MOVEL     N(4)          XXRSV
     C  N72
     CAN 54              MOVEL     N(4)          XXRSV
     C   72
     CAN 54              MOVEL(P)  charLGRH      XXRSV
     *** End Add ***  11/25/13  ************************************************
     C  N55
     CAN 54              EXCEPT    RSVHD
     C  N55
     CAN 54              EXCEPT    NODATA
     C  N55
     CAN 54              EXCEPT    HEAD
     C                   ELSE
     C     XSORT         IFEQ      '2'
     C   53              MOVEL     N(3)          XXRSV
     C  N55
     CAN 53              EXCEPT    RSVHD
     C  N55
     CAN 53              EXCEPT    NODATA
     C  N55
     CAN 53              EXCEPT    HEAD
     *** Beg Add ***  11/25/13  ************************************************
     C*****   54              MOVEL     N(4)          XXRSV
     C  N72
     CAN 54              MOVEL     N(4)          XXRSV
     C   72
     CAN 54              MOVEL(P)  charLGRH      XXRSV
     *** End Add ***  11/25/13  ************************************************
     C  N55
     CAN 54              EXCEPT    RSVHD
     C  N55
     CAN 54              EXCEPT    NODATA
     C  N55
     CAN 54              EXCEPT    HEAD
     C                   ELSE
     C     XSORT         IFEQ      '3'
     *** Beg Add ***  11/25/13  ************************************************
     C*****   54              MOVEL     N(4)          XXRSV
     C  N72
     CAN 54              MOVEL     N(4)          XXRSV
     C   72
     CAN 54              MOVEL(P)  charLGRH      XXRSV
     *** End Add ***  11/25/13  ************************************************
     C  N55
     CAN 54              EXCEPT    RSVHD
     C  N55
     CAN 54              EXCEPT    NODATA
     C  N55
     CAN 54              EXCEPT    HEAD
     C                   END
     C                   END
     C                   END
     C*
     C  N55              EXCEPT    GRNTOT
     C*
     C                   END
     C*
     C     NORCDS        TAG
     C*
     C     *IN40         IFEQ      '1'
     C  N55              EXCEPT    HEAD
     C  N55              EXCEPT    NODATA
     C                   END
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ***************************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ***************************************
      /End-free
     C*  End of Program
     C*
     C                   MOVE      '1'           *INLR
     C*
     C***************************************************************
     C*  SRWRIT - Subroutine to write WFDUP record
     C***************************************************************
     C     SRWRIT        BEGSR
     C*
     C                   MOVE      XCO#          DUCO#
     C                   MOVE      XFND          DUFND
     C                   MOVEL     FNNAME        DUFNNM
     C                   MOVE      XADJ          DUADJ
     C                   MOVEL     XXUSNM        DUADNM
     C                   MOVE      XFYR          DUFYR
     C                   MOVE      XCASE         DUCASE
     C                   MOVE      XFEIN         DUFEIN
     C                   MOVE      XMOD          DUMOD
     C*
     C*  Get Payment Value 1 Description for First Payment
     C*
     C                   MOVE      *BLANK        XXKY
     C                   MOVEL     DU1BR2        XXKY
     C     B2KY          CHAIN     PB201                              60
     C                   MOVE      *BLANK        DU1PVD
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     B2DESC        DU1PVD
     C                   ENDIF
     C*
     C*  Get Payment Value 1 Description for Second Payment
     C*
     C                   MOVEL     DU1PVD        DU2PVD
     C*
     C     DU1BR2        IFNE      DU2BR2
     C                   MOVE      *BLANK        XXKY
     C                   MOVEL     DU2BR2        XXKY
     C     B2KY          CHAIN     PB201                              60
     C                   MOVE      *BLANK        DU2PVD
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     B2DESC        DU2PVD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   WRITE     PDU01
     C*
     C                   ENDSR
     C*
     C***************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ********************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          2
     O*********                                  15 'ROUTE TO:'
     O*********              UPUSNM              67
      ***** End Del ***** 08/26/14 **************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2
      ***** Beg Del ***** 08/26/14 **************************************
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          2
     O*********                                  19 'OUTPUT QUEUE:'
     O*********              OROUTQ              30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 **************************************
     O***** BEGIN ADD **** 01/26/00 ***********************************
     O          E            ROUTE       3  2
     O                                           23 'REPORT SELECTIONS'
     O          E            ROUTE          2
     O                                           26 'GROUP  . . . . . . :'
     O              N71      XXFND               31
     O               71                          32 '*ALL'
     O***** END   ADD **** 01/26/00 ***********************************
      *** BEGIN MOVE *** 12/18/97 **********************************************
     O*
     O          E            ROUTE          2
     O                                           26 'REPORT TYPE  . . . :'
     O               42N44                       38 'GROUP BATCH'
     O              N42N44                       39 'DAILY BATCH'
     O                  44                       39 'DEMAND     '
      *** END   MOVE *** 12/18/97 **********************************************
      *** BEGIN CHG  *** 12/18/97 **********************************************
     O*
     O          E            ROUTE          2
     O              N44                          26 'BATCH DATE . . . . :'
     O              N44      XXDATE        Y     35
     O               44                          26 'FROM/TO DATES  . . :'
     O               44      XXDTE1        Y     35
     O               44                          37 '-'
     O               44      XXDTE2        Y     46
     O*
     O          E    44      ROUTE          2
     O                                           26 'AMOUNT RANGE . . . :'
     O               80                          39 'ALL AMOUNTS'
     O              N80      XXAMT1        3     41
     O              N80                          44 '-'
     O              N80      XXAMT2        3     59
      *** END   CHG  *** 12/18/97 **********************************************
      *** BEGIN MOVE *** 12/18/97 **********************************************
     O*
     O          E            ROUTE          2
     O                                           26 'DUPLICATE SELECTION:'
     O                       DS(X2)              48
      *** END   MOVE *** 12/18/97 **********************************************
     O*
      *** BEGIN CHG  *** 12/18/97 **********************************************
     O          E    44      ROUTE          2
     O                                           26 'GROUP TYPE . . . . :'
     O              N42                          37 'NON-GROUP'
     O               42                          33 'GROUP'
     O*
     O          E            ROUTE          2
     O                                           26 'TYPE OF OUTPUT . . :'
     O                       TO(X1)              58
      ***** BEG DEL ******* 01/23/01 **********************************
     O*************   N56                83 'LIB/FILE:  PDSRPT/WFDUP'
      ***** END DEL ******* 01/23/01 **********************************
      ***** BEG ADD ******* 01/23/01 **********************************
     O              N56                          70 'LIB/FILE:'
     O              N56      XXOFIL            +001
      ***** END ADD ******* 01/23/01 **********************************
     O*
     O          E            ROUTE          1
     O                                           26 'RESERVE TYPE . . . :'
     O              N39      RS(1)               40
     O               39                          40 '*ALL        '
     O*
     O          E            ROUTE          1
     O                       RS(2)               40
     O*
     O          E            ROUTE          1
     O                       RS(3)               40
     O*
     O          E            ROUTE          1
     O                       RS(4)               40
      *** END   CHG  *** 12/18/97 **********************************************
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  155           ROUTE
     O***                                27 '(C) PROGRESSIVE DATA '
     O***                                43 'SOLUTIONS, INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 04
     O                       DSTIME              13 '  :  :  '
     O                       CONAME             120
     O                       DSDATE        Y    197
     O*
     O          E            HEAD           1
     O                       Q1PGM               15
     O                       XXTITL             124
     O              N44                         156 'BATCH DATE:'
     O              N44      XXDATE             165 '0  /  /  '
     O               44                         156 'SERVICE DATE RANGE:'
     O               44      XXDTE1             165 '0  /  /  '
     O               44                         168 'TO'
     O               44      XXDTE2             177 '0  /  /  '
     O                                          191 'PAGE:'
     O                       PAGE          Z    197
     O*
     O          E            HEAD           1
     O               42N44                       97 'GROUP BATCH'
     O              N42N44                       97 'DAILY BATCH'
     O                  44                       98 'DEMAND REPORT'
      *** BEGIN ADD *** 12/18/97 **********************************************
     O                                          156 'AMOUNT RANGE:'
     O               80                         168 'ALL AMOUNTS'
     O              N80      XXAMT1        3    168
     O              N80                         170 '-'
     O              N80      XXAMT2        3    183
      *** END   ADD *** 12/18/97 **********************************************
     O*
     O          E            HEAD           1
     O                                          156 'DUPLICATE SELECT:'
     O                       DS(X2)             177
      *** BEGIN ADD *** 12/18/97 **********************************************
     O*
     O          E            HEAD           1
      *** BEGIN ADD *** 12/18/97 **********************************************
     O                                           25 'NOTE:  *  MEANS EXACT DU'
     O                                           32 'PLICATE'
      *** END   ADD *** 12/18/97 **********************************************
     O                                          156 'TYPE:'
     O              N42                         166 'NON-GROUP'
     O               42                         162 'GROUP'
     O*
     O          E            HEAD           1
     O                                          156 'TYPE OF OUTPUT:'
     O                       TO(X1)             187
     O*
     O          E            HEAD           1
     O                                          156 'RESERVE TYPES:'
     O                       QQRST1             158
     O                       QQRST2             161
     O                       QQRST3             164
     O                       QQRST4             167
      *** END   ADD *** 12/18/97 **********************************************
     O*
     O          E            HEAD1          1
     O                                           55 'TRANS'
     O                                           89 'FEDERAL'
     O                                          103 'PMT'
     O                                          155 'RSV'
     O                                          160 'RSV'
     O                                          186 'GROUP'
     O*
     O          E            HEAD1          2
     O                                            8 'GRP'
     O                                           12 'YR'
     O                                           20 'CASE'
     O                                           34 'CLAIMANT NAME'
     O                                           54 'DATE'
     O                                           63 'PAID TO'
     O                                           86 'ID'
     O                                           96 'MOD'
     O                                          104 'VAL1'
     O                                          120 'FROM-TO-DATES'
     O                                          136 'AMOUNT'
     O                                          148 'EOB#'
     O                                          156 'TYPE'
     O                                          159 'CD'
     O                                          169 'CHECK #'
     O                                          178 'STATUS'
     O                                          185 'TYPE'
     O                                          197 'REQUESTOR'
     O*
     O* Reservs Heading
     O*
     O          E            RSVHD          1
     O                                           18 'RESERVE TYPE:'
     O                       XXRSV               31
     O***                               156 'AMOUNT RANGE:'
     O***              80               168 'ALL AMOUNTS'
     O***             N80      XXAMT13  168
     O***             N80               170 '-'
     O***             N80      XXAMT23  183
     O*
     O* Reserve Totals
     O*
     O          E            RSVTOT      1
     O                                           12 'TOTALS:'
     O                                           32 'Dups/Possible Dups:'
     O                       XXRDUP        3     37
     O*
     O* Adjuster Heading
     O*
     O          E            ADJHD          2
     O                                           14 'ADJUSTER:'
     O                       XADJ                22
     O                       XXUSNM              74
     O*
     O* Adjuster Totals
     O*
     O          E            ADJTOT      1
     O                                           21 'ADJUSTER TOTALS:'
     O                                           41 'Dups/Possible Dups:'
     O                       XXADUP        3     45
     O*
     O          E            NODATA      1
     O                                           29 '*** No records meet sele'
     O                                           47 'ction criteria ***'
     O               44 40                      161 'AMOUNT RANGE:'
     O               44 40   XXAMT1        3    179
     O               44 40                      181 '-'
     O               44 40   XXAMT2        3    197
     O*
     O* Detail Line (First Payment)
     O*
     O          E    43      DETAIL         1
     O                       XFND          Z      8
     O                       XFYR          Z     12
     O                       XCASE         Z     20
     O                       XXCNAM              41
     O                       XX1SDT        Y     55
     O                       XXPNAM              76
     O***  Begin Chg  ***  03/01/21  *******************************
     O*                      XFEIN         3     90
     O                       XFEIN               90
     O***  End Chg  ***  03/01/21  *******************************
     O                       XMOD          3     96
     O                       DU1BR2             103
     O                       XX1FDT        Y    113
     O                       XX1TDT        Y    122
     O                       DU1AMT        K    137
     O                       DU1EOB        3    151
     O                       DU1RTP             154
     O                       DU1RCD             159
     O                       DU1CK#        3    169
     O                       XX1STT             180
     O                       DU1GTP             184
     O                       DU1RQT             198
     O*
     O* Detail Line (Second Payment
     O*
     O          E            DETAIL         1
     O                       DUAST               45
     O                       XX2SDT        Y     55
     O                       XXPNAM              76
     O***  Begin Chg  ***  03/01/21  *******************************
     O*                      XFEIN         3     90
     O                       XFEIN               90
     O***  End Chg  ***  03/01/21  *******************************
     O                       XMOD          3     96
     O                       DU2BR2             103
     O                       XX2FDT        Y    113
     O                       XX2TDT        Y    122
     O                       DU2AMT        K    137
     O                       DU2EOB        3    151
     O                       DU2RTP             154
     O                       DU2RCD             159
     O                       DU2CK#        3    169
     O                       XX2STT             180
     O                       DU2GTP             184
     O                       DU2RQT             198
     O          E            GRNTOT      1  2
     O                                           18 'GRAND TOTALS:'
     O                                           38 'Dups/Possible Dups:'
     O                       XXGDUP        3     42
     O               44                         161 'AMOUNT RANGE:'
     O               44      XXAMT1        3    179
     O               44                         181 '-'
     O               44      XXAMT2        3    197
** TABS & TABB **
1COMPENSATION2MEDICAL     3EXPENSE     4LEGAL
** RESERVE NAMES **
COMPENSATION
MEDICAL
EXPENSE
LEGAL
** DUPLICATE SELECTION **
EXACT
EXACT AND POSSIBLE
** TYPE OF OUTPUT **
DOWNLOAD FILE
PRINT REPORT
DOWNLOAD FILE / PRINT REPORT
