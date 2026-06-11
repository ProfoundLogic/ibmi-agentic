     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W8614R1 - Duplicate Payment Report - Build workfile
     H*****************************************************************
     H*    PROGRAM DESCRIPTION:                                       *
     H*      Build workfile of data for Duplicate Payment Report.     *
     H*    CALLED FROM:                                               *
     H*      W8614C                                                   *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*    PROGRAMMER: C: 30                                          *
     H*    CREATED  12/26/95   Project: WC457                         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H*  04/24/03 C:15 Support: WC113/36983                           *
     H*                Fixed code to ignore adjustments, stops, and   *
     H*                voids.                                         *
     H*                                                               *
     H*  01/25/01 C:15 Support: WC025/26044                           *
     H*                Added code to get the WDRPP record using a key *
     H*                without the TRDT, if the record can not be     *
     H*                retrieved using the TRDT.                      *
     H*                                                               *
     H*  05/19/00 C:15 Support: WC034/20089                           *
     H*               -Added the TRDT to key list for WDRPP.          *
     H*               -Changed pgm to get the correct pmt requester.  *
     H*                                                               *
     H*  08/05/99 C:33 SEQ#: WC025/18226                              *
     H*                INGNORE PAYMENTS THAT HAVE CLEAR DATES FOR THEM*
     H*                                                               *
     H*  04/07/99 C:15 Support: WC090/17156                           *
     H*                Removed PATRDT from key list used to retrieve  *
     H*                records from WDRPP due to incorrect date being *
     H*                loaded into RPTRDT.                            *
     H*                                                               *
     H*  12/30/97 C:30 Project: WC577                                 *
     H*           When accessing WDPAP, if Duplicate Payment Cleared  *
     H*           Date is not zero, skip dup check.                   *
     H*           Proceed with next WDPAP record.                     *
     H*                                                               *
     H*  09/30/96 C:30 Project: WC583                                 *
     H*           When 'Duplicate' or 'Possible' Duplicate Payment is *
     H*           detected in the Payment Detail, logic added to check*
     H*           if payment is 'VOID'.  If so, then the payment is   *
     H*           not a duplicate.  Continue Payment Detail scan.     *
     H*                                                               *
     H*  04/15/96  c:30  Project: WC457 Correction                    *
     H*            Two Changes:  1) Remove Date Range check for WARPP *
     H*            records;  2) Add Amount Range check for possible   *
     H*            duplicates for both WARPP & WDPAP                  *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   INDICATOR            FUNCTION DESCRIPTION                   *
     H*                                                               *
     H*      40           TYPE RUN IS 'DEMAND'                        *
     H*      42           BATCH TYPE = GROUP BATCH                    *
     H*      60-62        GENERAL CHAINING                            *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**                                                             **
     H**    WARPL  (PRP01): CO#/FND/FYR/CASE/SEQ                     **
     H**    WDRPL  (PRP02): CO#/FND/FYR/CASE/CHK#/TRDT               **
     H**    WDPAL  (PPA01): CO#/FND/FYR/CASE/SEQ#                    **
     H**    WMEML  (PEM01): CO#/FND/EMP#/DIV                         **
     H**    WMCML1 (PCM01): CO#/FND/FYR/CASE                         **
     H**    WMCDL  (PCD01): CO#/SS#                                  **
     H**    WXDU2P (PDU02):                                          **
     H*****************************************************************
     F*
     F* Request Payment File
     FWARPL     IF   E           K DISK
     F*
     F* Request Payment Detail File
     FWDRPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     F*** End add ***** 05/22/03 **************************************
     F*
     F* Payment Detail File
     FWDPAL     IF   E           K DISK
     F*
     F* Claim Master File
     FWMCML1    IF   E           K DISK
     F*
     F* Demographic File
     FWMCDL     IF   E           K DISK
     F*
     F* Workfile
     FWXDU2P    O    E           K DISK
     F*

     D*****************************************************************
     D*** End add ***** 05/22/03 **************************************
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
     D  XFEIN                 27     35  0
     D  XMOD                  36     38  0
     D*
     D                 DS
     D  RC                     1      4
     D                                     DIM(4)                               Reserve Type
     D  XXRES1                 1      1
     D  XXRES2                 2      2
     D  XXRES3                 3      3
     D  XXRES4                 4      4
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*****************************************************************
     I*  Rename the duplicate field names
     IPRP02
     I              RPCO#                       RDCO#
     I              RPFND                       RDFND
     I              RPFYR                       RDFYR
     I              RPCASE                      RDCASE
     I              RPCHK#                      RDCHK#
     I              RPTRDT                      RDTRDT
     I              RPPNAM                      RDPNAM
     I              RPUSER                      RDUSER
     I*** Begin add *** 05/22/03 **************************************
     IPRP03
     I              RPCO#                       RDCO#
     I              RPFND                       RDFND
     I              RPFYR                       RDFYR
     I              RPCASE                      RDCASE
     I              RPCHK#                      RDCHK#
     I              RPTRDT                      RDTRDT
     I              RPPNAM                      RDPNAM
     I              RPUSER                      RDUSER

     C*****************************************************************
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    RPFND         SVFND
     C     *LIKE         DEFINE    RPFYR         XXFYR
     C     *LIKE         DEFINE    RPCASE        XXCASE
     C     *LIKE         DEFINE    RPSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    PASEQ#        XXSEQ
     C     *LIKE         DEFINE    PASEQ#        SVSQ#
     C     *LIKE         DEFINE    RPDT          XXFROM
     C     *LIKE         DEFINE    RPDT          XXTO
     C*
     C*****************************************************************
     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE DEMOGRAPHIC FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  KEY LIST FOR PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ
      *
     C*
     C*  KEY LIST FOR REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR REQUEST PAYMENT FILE
     C*
     C     RPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     RPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C*  KEY LIST FOR REQUEST PAYMENT DETAIL FILE (WDRPP)
     C*
     C     DRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    PACHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   KFLD                    PATRDT
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 04/07/99 ***********************************************
     C******************** KFLD           PATRDT
     C*** END DLT ***** 04/07/99 ***********************************************
     C*
     C*** BEGIN ADD *** 01/25/01 **************************************
     C     DRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    PACHK#
     C*** END ADD ***** 01/25/01 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C     R3KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PASEQ#
     C*** End add ***** 05/22/03 **************************************
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C****************************************************************
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
     C                   PARM                    QQRES1            1
     C                   PARM                    QQRES2            1
     C                   PARM                    QQRES3            1
     C                   PARM                    QQRES4            1
     C                   PARM                    QQRUN             1
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
     C*
     C                   Z-ADD     *ZEROS        XXDATE            6 0
     C                   MOVE      *BLANKS       QQTERM            1
     C                   MOVE      *BLANKS       QQLAST           56
     C                   MOVE      *BLANKS       QQFRST           15
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQRES1        XXRES1
     C                   MOVE      QQRES2        XXRES2
     C                   MOVE      QQRES3        XXRES3
     C                   MOVE      QQRES4        XXRES4
     C*
     C                   MOVE      QQDTE1        X1FROM            8 0
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM
     C*
     C                   MOVE      QQDTE2        X1TO              8 0
     C*
     C                   Z-ADD     X1TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO
     C*
     C                   MOVE      QQAMT1        XXAMT1           11 2
     C                   MOVE      QQAMT2        XXAMT2           11 2
     C*
     C     QQTYP         COMP      'G'                                    42     GRP BATCH
     C*
     C     QQRUN         COMP      'D'                                    40     DEMAND
     C*
     C*  Read Request Payment File
     C*
     C   40RPKY          CHAIN     PRP01                              60         DEMAND
     C  N40COKY          CHAIN     PRP01                              60         BATCH
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C*  Do Not Process Deleted Records
     C*
     C     RPSTAT        IFEQ      'D'
     C                   GOTO      NXTRP
     C                   ENDIF
     C*
     C*  For Group Batch Process Group Checks Only
     C*
     C     QQTYP         IFEQ      'G'
     C     RPPGTP        ANDNE     'G'
     C                   GOTO      NXTRP
     C                   ENDIF
     C*
     C*  For NON-Group Batch Process NON-Group Checks Only
     C*
     C     QQTYP         IFEQ      *BLANKS
     C     RPPGTP        ANDEQ     'G'
     C                   GOTO      NXTRP
     C                   ENDIF
     C*
     C*  Determine if Reserve Type is in the Request List.
     C*
     C                   Z-ADD     1             X                 3 0
     C     RPRETP        LOOKUP    RC(X)                                  30
     C     *IN30         IFEQ      *OFF
     C                   GOTO      NXTRP
     C                   ENDIF
     C*
     C*  Determine if Request Amount is within Request Range.
     C*
     C     XXAMT1        IFGT      *ZERO
     C     XXAMT2        ANDGT     *ZERO
     C     XXAMT1        IFGT      RPAMT
     C     XXAMT2        ORLT      RPAMT
     C                   GOTO      NXTRP
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*  Determine if Payment Type is 'M' or 'P'.
     C*
     C     RPTYPE        IFNE      'M'
     C     RPTYPE        ANDNE     'P'
     C                   GOTO      NXTRP
     C                   ENDIF
     C*
     C*  "Passed" Request Selection Criteria
     C*
     C                   EXSR      SRRP1
     C*
     C*  Position the Request Payment file at the Next Record and Check
     C*  the remaining records for possible Duplication.
     C*
     C     RPKY2         READE     PRP01                                  61    CO/FND/FYR/CASE
     C*
     C     *IN61         DOWEQ     '0'                                          WARPP Rec Found
     C*
     C*  Skip record if Payment Type is Not 'M' or 'P'.
     C*
     C     RPTYPE        IFNE      'M'
     C     RPTYPE        ANDNE     'P'
     C                   GOTO      NXTRP1
     C                   ENDIF
     C*
     C*  Determine if Request Amount is within Request Range.
     C*
     C     XXAMT1        IFGT      *ZERO
     C     XXAMT2        ANDGT     *ZERO
     C     XXAMT1        IFGT      RPAMT
     C     XXAMT2        ORLT      RPAMT
     C                   GOTO      NXTRP1
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Payment Request is an Exact Duplicate if the Previous Request has
     C*  the Same Payment Reserve Code, Provider, Dates of Service and
     C*  Payment Amount.
     C*
     C     RPRECD        IFEQ      DU1RCD
     C     RPFEIN        ANDEQ     XFEIN
     C     RPFRDT        ANDEQ     DU1FDT
     C     RPTODT        ANDEQ     DU1TDT
     C     RPAMT         ANDEQ     DU1AMT
     C     RPSTAT        ANDNE     'D'
     C     RPSEQ#        ANDNE     DU1SQ#
     C*
     C                   MOVE      '*'           DUAST
     C                   EXSR      SRRP2
     C                   MOVE      XXKEY         DUKEY
     C                   WRITE     PDU02                                        Exact Dup
     C*
     C                   ELSE
     C*
     C*  Payment Request is a Possible Duplicate if a Previous Request
     C*  has the Same Reserve Code, Provider, and the Dates of Service
     C*  for the NEW Request Overlap Dates of Service for the Previous
     C*  Request.
     C*
     C     QQTDUP        IFEQ      'N'
     C     RPRECD        ANDEQ     DU1RCD
     C     RPFEIN        ANDEQ     XFEIN
     C     RPSTAT        ANDNE     'D'
     C     RPSEQ#        ANDNE     DU1SQ#
     C*
     C     RPFRDT        IFGE      DU1FDT
     C     RPFRDT        ANDLE     DU1TDT
     C*
     C     RPTODT        ORGE      DU1FDT
     C     RPTODT        ANDLE     DU1TDT
     C*
     C     RPFRDT        ORLT      DU1FDT
     C     RPTODT        ANDGT     DU1TDT
     C*
     C                   MOVE      *BLANK        DUAST
     C                   EXSR      SRRP2
     C                   MOVE      XXKEY         DUKEY
     C                   WRITE     PDU02                                        Possible Dup
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     NXTRP1        TAG
     C*
     C     RPKY2         READE     PRP01                                  61
     C                   ENDDO
     C*
     C*  Read All Payment Detail records for Case
     C*
     C     PAKY          CHAIN     PPA01                              61        CO/FND/FYR/CASE
     C*
     C     *IN61         DOWEQ     '0'                                          WDPAP Rec Found
     C*
     C*  Skip record if Payment Type is Not 'M' or 'P'.
     C*
     C     PATYPE        IFNE      'M'
     C     PATYPE        ANDNE     'P'
     C*** Begin add *** 04/24/03 **************************************
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C*** End add ***** 04/24/03 **************************************
     C                   GOTO      NXTPA1
     C                   ENDIF
     C*
     C*** Begin add *** 04/24/03 **************************************
     C*  Skip record if payment has been adjusted, stopped or voided.
     C*
     C     PABASQ        IFNE      *ZERO
     C     PAADST        ORNE      *BLANKS
     C                   GOTO      NXTPA1
     C                   ENDIF
     C*** End add ***** 04/24/03 **************************************
     C*
     C*  Determine if Payment Amount is within Request Range.
     C*
     C     XXAMT1        IFGT      *ZERO
     C     XXAMT2        ANDGT     *ZERO
     C     XXAMT1        IFGT      PAAMT
     C     XXAMT2        ORLT      PAAMT
     C                   GOTO      NXTPA1
     C                   ENDIF
     C                   ENDIF
     C***  BEGIN ADD  ***  08/05/99  *********************************
     C*
     C*  Determine if Payment has a "Duplicate Pmt Clear Date"
     C*
     C     PADPCD        IFNE      *ZERO
     C                   GOTO      NXTPA1
     C                   ENDIF                                                  PADPCD<>*ZERO
     C***  END   ADD  ***  08/05/99  *********************************
     C*
     C*  Payment Detail is an Exact Duplicate if the Previous Request has
     C*  the Same Payment Reserve Code, Provider, Dates of Service and
     C*  Payment Amount.
     C*
     C     PARECD        IFEQ      DU1RCD
     C     PAFEIN        ANDEQ     XFEIN
     C     PAFRDT        ANDEQ     DU1FDT
     C     PATODT        ANDEQ     DU1TDT
     C     PAAMT         ANDEQ     DU1AMT
     C     PASTAT        ANDNE     'D'
     C     PASEQ#        ANDNE     DU1SQ#
     C*
     C                   Z-ADD     PASEQ#        SVSQ#
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           EXSR SRVOID
     C********** XXVOID    IFEQ *OFF
     C*** End dlt ***** 04/24/03 **************************************
      *
     C                   MOVE      '*'           DUAST
     C                   EXSR      SRPA2
     C                   MOVE      XXKEY         DUKEY
     C                   WRITE     PDU02                                        Exact Dup
      *
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           ENDIF
     C*** End dlt ***** 04/24/03 **************************************
     C*
     C                   ELSE
     C*
     C*  Payment Detail is a Possible Duplicate if a Previous Request
     C*  has the Same Reserve Code, Provider, and the Dates of Service
     C*  for the NEW Request Overlap Dates of Service for the Previous
     C*  Request.
     C*
     C     QQTDUP        IFEQ      'N'
     C     PARECD        ANDEQ     DU1RCD
     C     PAFEIN        ANDEQ     XFEIN
     C     PASTAT        ANDNE     'D'
     C     PASEQ#        ANDNE     DU1SQ#
     C*
     C     PAFRDT        IFGE      DU1FDT
     C     PAFRDT        ANDLE     DU1TDT
     C*
     C     PATODT        ORGE      DU1FDT
     C     PATODT        ANDLE     DU1TDT
     C*
     C     PAFRDT        ORLT      DU1FDT
     C     PATODT        ANDGT     DU1TDT
      *
     C                   Z-ADD     PASEQ#        SVSQ#
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           EXSR SRVOID
     C********** XXVOID    IFEQ *OFF
     C*** End dlt ***** 04/24/03 **************************************
     C*
     C                   MOVE      *BLANK        DUAST
     C                   EXSR      SRPA2
     C                   MOVE      XXKEY         DUKEY
     C                   WRITE     PDU02                                        Possible Dup
     C*
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           ENDIF
     C*** End dlt ***** 04/24/03 **************************************
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     NXTPA1        TAG
     C*
     C     PAKY          READE     PPA01                                  61
     C                   ENDDO
     C*
     C*  Re-position WARPP to read record after DU1SQ#
     C*
     C                   Z-ADD     DU1SQ#        XXSEQ#
     C     RPKY3         SETGT     PRP01
     C*
     C     NXTRP         TAG
     C*
     C   40RPKY          READE     PRP01                                  60     DEMAND
     C  N40COKY          READE     PRP01                                  60     BATCH
     C*
     C                   ENDDO                                                  *IN60
     C*
     C*  If program execution is a "BATCH" run, go to End of Job
     C*
     C     QQRUN         IFNE      'D'
     C                   GOTO      EOJ
     C                   ENDIF
     C*
     C*  P r o c e s s     P a y m e n t     D e t a i l     F i l e
     C*
     C     RPKY          CHAIN     PPA01                              60
     C*
     C     *IN60         DOWEQ     '0'
      *
      *** BEGIN ADD *** 12/30/97 ***********************************************
      *
      *  If "Cleared" Date is not zero, skip record.
      *
     C     PADPCD        IFNE      *ZERO
     C                   GOTO      NXTPA
     C                   ENDIF
      *
      *** END   ADD *** 12/30/97 ***********************************************
     C*
     C*  Do Not Process Deleted Records
     C*
     C     PASTAT        IFEQ      'D'
     C                   GOTO      NXTPA
     C                   ENDIF
     C*
     C*  For Group Batch Process Group Checks Only
     C*
     C     QQTYP         IFEQ      'G'
     C     PAPGTP        ANDNE     'G'
     C                   GOTO      NXTPA
     C                   ENDIF
     C*
     C*  For NON-Group Batch Process NON-Group Checks Only
     C*
     C     QQTYP         IFEQ      *BLANKS
     C     PAPGTP        ANDEQ     'G'
     C                   GOTO      NXTPA
     C                   ENDIF
     C*
     C*  Determine if Reserve Type is in the Request List.
     C*
     C                   Z-ADD     1             X                 3 0
     C     PARETP        LOOKUP    RC(X)                                  30
     C     *IN30         IFEQ      *OFF
     C                   GOTO      NXTPA
     C                   ENDIF
     C*
     C*  Determine if Request Amount is within Request Range.
     C*
     C     XXAMT1        IFGT      *ZERO
     C     XXAMT2        ANDGT     *ZERO
     C     XXAMT1        IFGT      PAAMT
     C     XXAMT2        ORLT      PAAMT
     C                   GOTO      NXTPA
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Determine if From and To Dates are within Request Range.
     C*
     C     XXFROM        IFGT      *ZERO
     C     XXTO          ANDGT     *ZERO
     C     XXFROM        IFGT      PATODT
     C     XXTO          ORLT      PAFRDT
     C                   GOTO      NXTPA
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Determine if Payment Type is 'M' or 'P'.
     C*
     C     PATYPE        IFNE      'M'
     C     PATYPE        ANDNE     'P'
     C*** Begin add *** 04/24/03 **************************************
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C*** End add ***** 04/24/03 **************************************
     C                   GOTO      NXTPA
     C                   ENDIF
     C*
     C*** Begin add *** 04/24/03 **************************************
     C*  Skip record if payment has been adjusted, stopped or voided.
     C*
     C     PABASQ        IFNE      *ZERO
     C     PAADST        ORNE      *BLANKS
     C                   GOTO      NXTPA
     C                   ENDIF
     C*** End add ***** 04/24/03 **************************************
     C*
     C*  "Passed" Request Selection Criteria
     C*
     C                   EXSR      SRPA1
     C*
     C*  Position the Payment Detail file at the Next Record and Check
     C*  the remaining records for possible Duplication.
     C*
     C     PAKY          READE     PPA01                                  61    CO/FND/FYR/CASE
     C*
     C*  Read All Payment Detail records for Case
     C*
     C     *IN61         DOWEQ     '0'                                          WDPAP Rec Found
     C*
     C*  Skip record if Payment Type is Not 'M' or 'P'.
     C*
     C     PATYPE        IFNE      'M'
     C     PATYPE        ANDNE     'P'
     C*** Begin add *** 04/24/03 **************************************
     C     PATYPE        ANDNE     'A'
     C     PATYPE        ANDNE     'Z'
     C*** End add ***** 04/24/03 **************************************
     C                   GOTO      NXTPA2
     C                   ENDIF
     C*
     C*** Begin add *** 04/24/03 **************************************
     C*  Skip record if payment has been adjusted, stopped or voided.
     C*
     C     PABASQ        IFNE      *ZERO
     C     PAADST        ORNE      *BLANKS
     C                   GOTO      NXTPA2
     C                   ENDIF
     C*** End add ***** 04/24/03 **************************************
     C*
     C***  BEGIN ADD  ***  08/05/99  *********************************
     C*  Determine if Payment has a "Duplicate Pmt Clear Date"
     C*
     C     PADPCD        IFNE      *ZERO
     C                   GOTO      NXTPA2
     C                   ENDIF                                                  PADPCD<>*ZERO
     C***  END   ADD  ***  08/05/99  *********************************
     C*
     C*  Determine if Payment Amount is within Request Range.
     C*
     C     XXAMT1        IFGT      *ZERO
     C     XXAMT2        ANDGT     *ZERO
     C     XXAMT1        IFGT      PAAMT
     C     XXAMT2        ORLT      PAAMT
     C                   GOTO      NXTPA2
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Payment Detail is an Exact Duplicate if the Previous Request has
     C*  the Same Payment Reserve Code, Provider, Dates of Service and
     C*  Payment Amount.
     C*
     C     PARECD        IFEQ      DU1RCD
     C     PAFEIN        ANDEQ     XFEIN
     C     PAFRDT        ANDEQ     DU1FDT
     C     PATODT        ANDEQ     DU1TDT
     C     PAAMT         ANDEQ     DU1AMT
     C     PASTAT        ANDNE     'D'
     C     PASEQ#        ANDNE     DU1SQ#
      *
     C                   Z-ADD     PASEQ#        SVSQ#
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           EXSR SRVOID
     C********** XXVOID    IFEQ *OFF
     C*** End dlt ***** 04/24/03 **************************************
     C*
     C                   MOVE      '*'           DUAST
     C                   EXSR      SRPA2
     C                   MOVE      XXKEY         DUKEY
     C                   WRITE     PDU02                                        Exact Dup
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           ENDIF
     C*** End dlt ***** 04/24/03 **************************************
     C*
     C                   ELSE
     C*
     C*  Payment Detail is a Possible Duplicate if a Previous Request
     C*  has the Same Reserve Code, Provider, and the Dates of Service
     C*  for the NEW Request Overlap Dates of Service for the Previous
     C*  Request.
     C*
     C     QQTDUP        IFEQ      'N'
     C     PARECD        ANDEQ     DU1RCD
     C     PAFEIN        ANDEQ     XFEIN
     C     PASTAT        ANDNE     'D'
     C     PASEQ#        ANDNE     DU1SQ#
     C*
     C     PAFRDT        IFGE      DU1FDT
     C     PAFRDT        ANDLE     DU1TDT
     C*
     C     PATODT        ORGE      DU1FDT
     C     PATODT        ANDLE     DU1TDT
     C*
     C     PAFRDT        ORLT      DU1FDT
     C     PATODT        ANDGT     DU1TDT
      *
     C                   Z-ADD     PASEQ#        SVSQ#
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           EXSR SRVOID
     C********** XXVOID    IFEQ *OFF
     C*** End dlt ***** 04/24/03 **************************************
     C*
     C                   MOVE      *BLANK        DUAST
     C                   EXSR      SRPA2
     C                   MOVE      XXKEY         DUKEY
     C                   WRITE     PDU02                                        Possible Dup
     C*
     C*** Begin dlt *** 04/24/03 **************************************
     C**********           ENDIF
     C*** End dlt ***** 04/24/03 **************************************
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     NXTPA2        TAG
     C*
     C     PAKY          READE     PPA01                                  61
     C                   ENDDO
     C*
     C*  Re-position WDPAP to read record after DU1SQ#
     C*
     C                   Z-ADD     DU1SQ#        XXSEQ#
     C     RPKY3         SETGT     PPA01
     C*
     C     NXTPA         TAG
     C*
     C     RPKY          READE     PPA01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C*  End of Program
     C*
     C     EOJ           TAG
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*****************************************************************
     C*  SRSS# - SUBROUTINE TO GET CLAIMAMT NAME
     C*****************************************************************
     C*
     C     SRSS#         BEGSR
     C*
     C                   MOVE      *BLANKS       XADJ
     C*
     C     CMKY          CHAIN     PCM01                              62
     C*
     C     *IN62         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#
     C                   MOVEL     CMCLEX        XADJ
     C*
     C*    Get Claimant Demographic file
     C*
     C     CDKY          CHAIN     PCD01                              62
     C*
     C     *IN62         IFEQ      '0'
     C*
     C*    Call program to Rearrange Claimant Name
     C*
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        DUCNAM
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRRP1 - Subroutine to Assign WARPP data to Work file First Payment
     C**********************************************************************
     C*
     C     SRRP1         BEGSR
     C*
     C                   Z-ADD     RPCO#         XXCO#
     C                   Z-ADD     RPFND         XXFND
     C                   Z-ADD     RPFYR         XXFYR
     C                   Z-ADD     RPCASE        XXCASE
     C                   MOVE      'A'           XFILE
     C*
     C*  Determine XSORT
     C*
     C                   SELECT
     C     RPRETP        WHENEQ    'C'
     C                   MOVE      '1'           XSORT
     C     RPRETP        WHENEQ    'M'
     C                   MOVE      '2'           XSORT
     C     RPRETP        WHENEQ    'E'
     C                   MOVE      '3'           XSORT
     C     RPRETP        WHENEQ    'L'
     C                   MOVE      '4'           XSORT
     C                   OTHER
     C                   MOVE      '2'           XSORT
     C                   ENDSL
     C*
     C                   Z-ADD     RPFND         XFND
     C                   Z-ADD     RPCO#         XCO#
     C                   Z-ADD     RPFYR         XFYR
     C                   Z-ADD     RPCASE        XCASE
     C                   Z-ADD     RPFEIN        XFEIN
     C                   Z-ADD     RPMOD         XMOD
     C                   Z-ADD     RPSEQ#        XSEQ#
     C                   Z-ADD     RPSEQ#        DU1SQ#
     C                   Z-ADD     RPTRDT        DU1SDT
     C                   MOVEL     RPPNAM        DUPNAM
     C                   MOVEL     RPBR2         DU1BR2
     C                   Z-ADD     RPFRDT        DU1FDT
     C                   Z-ADD     RPTODT        DU1TDT
     C                   Z-ADD     RPAMT         DU1AMT
     C                   Z-ADD     RPEOB#        DU1EOB
     C                   MOVEL     RPRETP        DU1RTP
     C                   Z-ADD     RPCHK#        DU1CK#
     C                   MOVEL     RPPGTP        DU1GTP
     C*** BEGIN CHG *** 05/19/00 **************************************
     C******************** MOVELRPUSER    DU1RQT
     C                   MOVEL     RPRQST        DU1RQT
     C*** END CHG **** 05/19/00 **************************************
     C                   MOVEL     RPRECD        DU1RCD
     C*
     C                   SELECT
     C     RPSTAT        WHENEQ    *BLANK
     C                   MOVE      'R'           DU1STT                         Released
     C     RPSTAT        WHENEQ    'P'
     C                   MOVE      'P'           DU1STT                         Pended
     C                   OTHER
     C                   MOVE      RPSTAT        DU1STT
     C                   ENDSL
     C*
     C                   MOVE      *ON           *IN29                          Determine Claimant
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRRP2 - Subroutine to Assign WARPP data to Work file Second Payment
     C**********************************************************************
     C*
     C     SRRP2         BEGSR
     C*
     C     *IN29         IFEQ      *ON
     C                   MOVE      *OFF          *IN29
     C                   EXSR      SRSS#
     C                   ENDIF
     C*
     C                   Z-ADD     RPSEQ#        DU2SQ#
     C                   Z-ADD     RPTRDT        DU2SDT
     C                   MOVEL     RPBR2         DU2BR2
     C                   Z-ADD     RPFRDT        DU2FDT
     C                   Z-ADD     RPTODT        DU2TDT
     C                   Z-ADD     RPAMT         DU2AMT
     C                   Z-ADD     RPEOB#        DU2EOB
     C                   MOVEL     RPRETP        DU2RTP
     C                   Z-ADD     RPCHK#        DU2CK#
     C                   MOVEL     RPPGTP        DU2GTP
     C*** BEGIN CHG *** 05/19/00 **************************************
     C******************** MOVELRPUSER    DU2RQT
     C                   MOVEL     RPRQST        DU2RQT
     C*** END CHG **** 05/19/00 **************************************
     C                   MOVEL     RPRECD        DU2RCD
     C*
     C                   SELECT
     C     RPSTAT        WHENEQ    *BLANK
     C                   MOVE      'R'           DU2STT                         Released
     C     RPSTAT        WHENEQ    'P'
     C                   MOVE      'P'           DU2STT                         Pended
     C                   OTHER
     C                   MOVE      RPSTAT        DU2STT
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRPA1 - Subroutine to Assign WDPAP data to Work file First Payment
     C**********************************************************************
     C*
     C     SRPA1         BEGSR
     C*
     C                   Z-ADD     PACO#         XXCO#
     C                   Z-ADD     PAFND         XXFND
     C                   Z-ADD     PAFYR         XXFYR
     C                   Z-ADD     PACASE        XXCASE
     C                   MOVE      'B'           XFILE
     C*
     C*** BEGIN DLT *** 05/19/00 **************************************
     C* Get Requestor Name
     C********************
     C******************** MOVE *BLANKS   DU2RQT
     C******************** MOVELPAUSER    DU2RQT
     C*** END DLT ***** 05/19/00 **************************************
     C*
     C*** Begin add *** 05/22/03 **************************************
     C     R3KY          CHAIN     PRP03                              61
     C     *IN61         IFEQ      *ON
     C*** End add ***** 05/22/03 **************************************
     C     DRKY          CHAIN     PRP02                              61
     C*** BEGIN ADD *** 01/25/01 **************************************
     C   61DRKY1         CHAIN     PRP02                              61
     C   61              MOVE      *BLANKS       DUPNAM
     C*** END ADD ***** 01/25/01 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C                   ENDIF
     C*** End add ***** 05/22/03 **************************************
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     RDPNAM        DUPNAM
     C*** BEGIN DLT *** 05/19/00 **************************************
     C********** RDUSER    IFNE *BLANKS
     C**********           MOVE *BLANKS   DU2RQT
     C**********           MOVELRDUSER    DU2RQT
     C**********           ENDIF
     C*** END DLT ***** 05/19/00 **************************************
     C                   ENDIF
     C*
     C*  Determine XSORT
     C*
     C                   SELECT
     C     PARETP        WHENEQ    'C'
     C                   MOVE      '1'           XSORT
     C     PARETP        WHENEQ    'M'
     C                   MOVE      '2'           XSORT
     C     PARETP        WHENEQ    'E'
     C                   MOVE      '3'           XSORT
     C     PARETP        WHENEQ    'L'
     C                   MOVE      '4'           XSORT
     C                   OTHER
     C                   MOVE      '2'           XSORT
     C                   ENDSL
     C*
     C                   Z-ADD     PAFND         XFND
     C                   Z-ADD     PACO#         XCO#
     C                   Z-ADD     PAFYR         XFYR
     C                   Z-ADD     PACASE        XCASE
     C                   Z-ADD     PAFEIN        XFEIN
     C                   Z-ADD     PAMOD         XMOD
     C                   Z-ADD     PASEQ#        XSEQ#
     C                   Z-ADD     PASEQ#        DU1SQ#
     C                   Z-ADD     PATRDT        DU1SDT
     C                   MOVEL     PABR2         DU1BR2
     C                   Z-ADD     PAFRDT        DU1FDT
     C                   Z-ADD     PATODT        DU1TDT
     C                   Z-ADD     PAAMT         DU1AMT
     C                   Z-ADD     PAEOB#        DU1EOB
     C                   MOVEL     PARETP        DU1RTP
     C                   Z-ADD     PACHK#        DU1CK#
     C                   MOVEL     PAPGTP        DU1GTP
     C*** BEGIN CHG *** 05/19/00 **************************************
     C******************** MOVELPAUSER    DU1RQT
     C                   MOVEL     PARQST        DU1RQT
     C*** END CHG **** 05/19/00 **************************************
     C                   MOVEL     PARECD        DU1RCD
     C*
     C                   SELECT
     C     PASTAT        WHENEQ    *BLANK
     C                   MOVE      'Y'           DU1STT                         Paid
     C                   OTHER
     C                   MOVE      PASTAT        DU1STT
     C                   ENDSL
     C*
     C                   MOVE      *ON           *IN29                          Determine Claimant
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C*  SRPA2 - Subroutine to Assign WDPAP data to Work file Second Payment
     C**********************************************************************
     C*
     C     SRPA2         BEGSR
     C*
     C*** BEGIN DLT *** 05/19/00 **************************************
     C* Get Requestor Name
     C**********
     C**********           MOVE *BLANKS   DU2RQT
     C**********           MOVELPAUSER    DU2RQT
     C**********
     C********** DRKY      CHAINPRP02                61
     C********** *IN61     IFEQ *OFF
     C********** RDUSER    ANDNE*BLANKS
     C**********           MOVE *BLANKS   DU2RQT
     C**********           MOVELRDUSER    DU2RQT
     C**********           ENDIF
     C*** END DLT ***** 05/19/00 **************************************
     C*
     C     *IN29         IFEQ      *ON
     C                   MOVE      *OFF          *IN29
     C                   EXSR      SRSS#
     C                   ENDIF
     C*
     C                   Z-ADD     PASEQ#        DU2SQ#
     C                   Z-ADD     PATRDT        DU2SDT
     C                   MOVEL     PABR2         DU2BR2
     C                   Z-ADD     PAFRDT        DU2FDT
     C                   Z-ADD     PATODT        DU2TDT
     C                   Z-ADD     PAAMT         DU2AMT
     C                   Z-ADD     PAEOB#        DU2EOB
     C                   MOVEL     PARETP        DU2RTP
     C                   Z-ADD     PACHK#        DU2CK#
     C                   MOVEL     PAPGTP        DU2GTP
     C*** BEGIN CHG *** 05/19/00 **************************************
     C******************** MOVELPAUSER    DU2RQT
     C                   MOVEL     PARQST        DU2RQT
     C*** END CHG **** 05/19/00 **************************************
     C                   MOVEL     PARECD        DU2RCD
     C*
     C                   SELECT
     C     PASTAT        WHENEQ    *BLANK
     C                   MOVE      'Y'           DU2STT                         Paid
     C                   OTHER
     C                   MOVE      PASTAT        DU2STT
     C                   ENDSL
     C*
     C                   ENDSR
      *
     C*** Begin dlt *** 04/24/03 **************************************
      ******************************************************************
      *  SRVOID - Determine if a Dup Payment has been VOIDed.
      ******************************************************************
     C********** SRVOID    BEGSR
      **********
     C********** *LIKE     DEFN PAAMT     XXAMT
     C********** *LIKE     DEFN PACHK#    SVCHK#
     C**********           Z-ADDPASEQ#    SVSQ#
     C**********           Z-ADDPAAMT     XXAMT
     C**********           Z-ADDPACHK#    SVCHK#
     C**********
     C********** PAKY      READEPPA01                    60
     C**********
      *  Scan Remaining Payment Detail recs for same Check # looking for
      *  'Void', 'Stop', 'Reverse', 'Adjustment', or 'Recovery'
      *  transactions to determine a Final Payment Amount.
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** PACHK#    IFEQ SVCHK#
     C**********
      *  Process Specific Transaction.  XXAMT reflects Final Payment Amount.
     C**********
     C**********           SELEC
     C**********
     C**********           "VOID" or "STOP" or "RECOVERY"
     C********** PATYPE    WHEQ 'V'
     C********** PATYPE    OREQ 'S'
     C********** PATYPE    OREQ 'R'
     C********** XXAMT     SUB  PAAMT     XXAMT
     C**********
     C**********           "REVERSE" or "ADJUSTMENT"
     C********** PATYPE    WHEQ 'A'
     C********** PATYPE    OREQ 'Z'
     C********** XXAMT     ADD  PAAMT     XXAMT
     C**********
     C**********           ENDSL
     C**********           ENDIF
     C**********
     C********** PAKY      READEPPA01                    60
     C**********           ENDDO
     C**********
      *  If positive, then Payment is a duplicate.
      *  If zero, then Payment is not a duplicate.
     C**********
     C**********           MOVE *ON       XXVOID  1
     C**********
     C********** XXAMT     IFGT *ZERO
     C**********           MOVE *OFF      XXVOID
     C**********           ENDIF
     C**********
      *  Re-position at Orignal Payment Detail record
     C**********
     C**********           Z-ADDSVSQ#     XXSEQ
     C********** PAKY1     CHAINPPA01                60
     C**********
     C**********           ENDSR
     C*** End dlt ***** 04/24/03 **************************************
      *
     C*****************************************************************
