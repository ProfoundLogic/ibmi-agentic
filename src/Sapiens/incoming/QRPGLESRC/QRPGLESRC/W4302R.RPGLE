     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W4302R - CLAIMANT STAFF CODE DIARY TRANSFER
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM TRANSFERS DARTS DIARY      *
     H*  RECORDS FOR ALL CLAIMANT AND EMPLOYER STAFF CODES CHANGES.   *
     H*    CALLED FROM:                                               *
     H*      W6000C                                                   *
     H*    CALLS TO:                                                  *
     H*      NONE                                                     *
     H*    PROGRAMMER:  15                                            *
     H*    CREATED:  01/25/93       PROJECT:  AOA                     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 02/10/99 C:15 Project: WC577                                  *
     H*               Fixed code which turns *ON *IN37 which causes   *
     H*               'NO DARTS REMARKS CHANGED' to print.            *
     H*                                                               *
     H*   10/30/97 C:79  PROJECT: WC707                               *
     H*       UPDATED TO HANDLE EMPLOYER STAFF CODE DIARY TRANSFER    *
     H*                                                               *
     H*   10/07/97 C:47  PROJECT: WC659.3                             *
     H*       UPDATED TO VERSION 3 SPECS                              *
     H*                                                               *
     H* ***************************************************************
     H* INDICATOR USAGE                                               *
     H*  21  SUPERVISOR CHANGED                                       *
     H*  22  CLAIMS EXAMINER CHANGED                                  *
     H*  23  FIELD ADJUSTOR CHANGED                                   *
     H*  24  UNIT MANAGER CHANGED                                     *
     H*  25  ACCOUNT MANAGER CHANGED                                  *
     H*  26  CASE NURSE CHANGED                                       *
     H*  27  SDF ADJUSTER CHANGED                                     *
     H*  28  EXCESS SUPERVISOR CHANGED                                *
     H*  29  UNDERWRITER CHANGED                                      *
     H*  30  POLICY SERVICE REP CHANGED                               *
     H*  31  PREMIUM ACCOUNT REP CHANGED                              *
     H*  32  AUDIT REVIEWER CHANGED                                   *
     H*  33  AUDIT MANAGER CHANGED                                    *
     H*  34  LOSS CONTROL REP CHANGED                                 *
     H*  36  FIRST TIME THROUGH DARTS LOOP                            *
     H*  37  NO DIARIES WERE TRANSFERRED                              *
     H*  60  GENERAL CHAIN                                            *
     H*  61  END OF FILE (WADTP)                                      *
     H*  62  END OF FILE (SDCMP)                                      *
     H*  63  END OF FILE (SMARP)                                      *
     H*  99  QQCALL=C (CLAIMS STAFF CODES)                            *
     H* N99  QQCALL=E (EMPLOYER STAFF CODES)                          *
     H* ***************************************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FSMORL     IF   E           K DISK
      * DIARY TRANSFER LOGICAL (CO#/FND/FYR/CASE/CTGY)
     FWADTL     UF   E           K DISK
      * DIARY TRANSFER LOGICAL (CO#/FND/EMP#/DIV/CTGY)
     FWADTL1    UF   E           K DISK
     F                                     RENAME(PDT01:PDT02)
     FWMUPL1    IF   E           K DISK
     F*
     FSDCMLA    UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FSMARL     UF   E           K DISK
     FWMDDL     UF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QCKEY                  1     40
     D  XXCO#                  1      3  0
     D  XXFND                  4      6  0
     D  XXFYR                  7      9  0
     D  XXCASE                10     16  0
     D                 DS
     D  QEKEY                  1     40
     D  DTCO#                  1      3  0
     D  DTFND                  4      6  0
     D  DTEMP#                 7     15  0
     D  DTDIV                 16     20  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     IPCM02
     I              CMKEY                       C2KEY
     I              CMCSID                      C2CSID
     I              CMCOMM                      C2COMM
     I              CMCDAT                      C2CDAT
     I              CMCODE                      C2CODE
     I              CMDCID                      C2DCID
     I              CMPNAM                      C2PNAM
     I              CMTRDT                      C2TRDT
     I              CMTRTM                      C2TRTM
     I              CMSEQ#                      C2SEQ#
     I              CMSECL                      C2SECL
     I              CMSBYL                      C2SBYL
     I              CMSTAT                      C2STAT
     I              CMUSER                      C2USER
     I              CMPGM                       C2PGM
     I              CMTIME                      C2TIME
     I              CMDT                        C2DT
     I              CMIUSR                      C2IUSR
     I              CMFOLD                      C2FOLD
     I              CMDCDT                      C2DCDT
     I              CMDIFL                      C2DIFL
     I              CMAPID                      C2APID
     I              CMIDOC                      C2IDOC
     I              CMIQUE                      C2IQUE
     I              CMIMGF                      C2IMGF
     I              CMIPTH                      C2IPTH
     I              CMPIDX                      C2PIDX
     I              CMQPTY                      C2QPTY
     I              CMDCTP                      C2DCTP
     I              CMIPGM                      C2IPGM
     I              CMSMCD                      C2SMCD
     I              CMFL01                      C2FL01
     I              CMFL02                      C2FL02
     I              CMFL03                      C2FL03
     I              CMFL04                      C2FL04
     I              CMFL05                      C2FL05
     I              CMRMCD                      C2RMCD
     I              CMIDT                       C2IDT
      *
      *  KEY LIST FOR DARTS ADDITIONAL REMARKS FILE
      *

     C     ARKY          KLIST
     C                   KFLD                    C2KEY
     C                   KFLD                    C2SEQ#
      *
      *  CLAIMANT DEMOGRAPHICS FILE
      *
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  COMPANY MASTER FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  KEY LIST FOR DARTS REMARK FILE
      *
     C     C2KY          KLIST
     C                   KFLD                    XXKEY
      *
      *  DIARY MASTER FILE (CLAIMS)
      *
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      *  KEY LIST FOR THE DIARY TRANSFER FILE
      *
     C     DTKY          KLIST
     C                   KFLD                    XXCO#
      *
      *  FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  KEY LIST FOR THE OVERRIDE FILE.
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQCALL            1
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *================================================================
      *  VARIABLE DEFINITIONS
      *
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    UPFND         SVFND
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    C2KEY         XXKEY
     C     *LIKE         DEFINE    C2USER        XFUSER
     C     *LIKE         DEFINE    C2USER        XTUSER
      *================================================================
     C                   MOVE      QQCO#         XXCO#
     C                   TIME                    DSDTTM
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKY          CHAIN     PCO01                              60
      *
      *  REMOVE LEADING BLANKS & CENTER NAME
      *
     C                   MOVEL(P)  CONAME        ##DATA
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        XXCONM           40
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   GET THE DISTRIBUTION INFORMATION AND PRINT
      *
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
      *
      *   SET INDICATOR FOR CLAIMS OR EMPLOYER STAFF CODES
      *
     C     QQCALL        COMP      'C'                                    99
      *================================================================
      *  START READING WORK FILE
      *
     C   99DTKY          CHAIN     PDT01                              61
     C  N99DTKY          CHAIN     PDT02                              61
      *
     C     *IN61         DOWEQ     '0'
      *
      *  LOAD THE WORK FIELDS
      *
     C                   Z-ADD     DTCO#         XXCO#
     C                   Z-ADD     DTFND         XXFND
     C                   Z-ADD     DTFYR         XXFYR
     C                   Z-ADD     DTCASE        XXCASE
     C                   Z-ADD     DTEMP#        XXEMP#
     C                   Z-ADD     DTDIV         XXDIV
      *
     C     SVFND         IFNE      XXFND
     C                   EXSR      SRFND
     C                   END
      *
      *  IF THE CLAIMS EXAMINER WAS CHANGED, UPDATE THE DIARY DATE FILE
      *  WITH THE NEW CLAIMS EXAMINER.
      *
     C   99DTCTGY        IFEQ      'C'
     C     DDKY          CHAIN     PDD01                              60
     C  N60              MOVEL     DTNSTF        DDCLEX
     C  N60              UPDATE    PDD01
     C                   END
      *
      *  GET THE CLAIMANT NAME
      *
     C                   MOVE      *BLANKS       XXCMNM           30
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
      *
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQLAST           56
     C                   MOVE      *BLANKS       QQFRST           15
     C                   MOVEL     CDLSN         QQLAST
     C                   MOVEL     CDFSN         QQFRST
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXCMNM
     C                   END
      *
     C                   END
      *
      *  GET THE EMPLOYER NAME
      *
     C                   MOVE      *BLANKS       XXNAME
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        XXNAME           24
     C                   ENDIF                                                  *IN60=OFF
      *
      *  CLAIMANT STAFF CODE CATEGORY DESCRIPTIONS
      *
     C                   MOVEA     '00000000'    *IN(21)
     C     DTCTGY        COMP      'S'                                    21
     C     DTCTGY        COMP      'C'                                    22
     C     DTCTGY        COMP      'F'                                    23
     C     DTCTGY        COMP      'U'                                    24
     C     DTCTGY        COMP      'A'                                    25
     C     DTCTGY        COMP      'N'                                    26
     C     DTCTGY        COMP      'D'                                    27
     C     DTCTGY        COMP      'E'                                    28
      *
      *  EMPLOYER STAFF CODE CATEGORY DESCRIPTIONS
      *
     C                   MOVEA     '000000'      *IN(29)
     C     DTCTGY        COMP      'W'                                    29
     C     DTCTGY        COMP      'P'                                    30
     C     DTCTGY        COMP      'T'                                    31
     C     DTCTGY        COMP      'R'                                    32
     C     DTCTGY        COMP      'M'                                    33
     C     DTCTGY        COMP      'L'                                    34
      *
      *  GET THE USER FROM THE STAFF CODE
      *
     C                   MOVE      *BLANKS       XFUSER
     C                   MOVE      *BLANKS       XTUSER
     C     DTOSTF        CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSR         XFUSER
     C     DTNSTF        CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSR         XTUSER
      *
      *  OUTPUT CLAIMS OR EMPLOYER RECORDS HERE
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '1'           *IN36
     C                   MOVE      '1'           *IN37
      *
      *  TRANSFER THE OLD USER'S DARTS RECORDS TO THE NEW USER
      *
     C   99              MOVEL     QCKEY         XXKEY                          Claim key
     C  N99              MOVEL     QEKEY         XXKEY                          Employer key
      *
     C     C2KY          CHAIN     PCM02                              62
     C     *IN62         DOWEQ     '0'
      *
     C     XFUSER        IFEQ      C2USER
     C     C2DIFL        ANDEQ     'Y'
     C     C2DCDT        ANDEQ     *ZEROS
     C     DTNSTF        ANDNE     *BLANKS
      *
     C                   MOVE      *BLANKS       C2USER
     C                   MOVEL     XTUSER        C2USER
     C                   MOVE      *BLANKS       XXCOMM           98
     C                   MOVEL     C2COMM        XXCOMM
     C                   Z-ADD     C2CDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCDAT            8 0
     C                   MOVEL     Q1PGM         C2PGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C2DT
     C                   Z-ADD     DSTIME        C2TIME
     C                   UPDATE    PCM02
      *
     C     ARKY          CHAIN     PAR01                              63
     C     *IN63         DOWEQ     '0'
     C                   MOVE      *BLANKS       ARUSER
     C                   MOVEL     C2USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C                   Z-ADD     DSTIME        ARTIME
     C                   UPDATE    PAR01
     C     ARKY          READE     PAR01                                  63
     C                   ENDDO
      *
      *    OUTPUT DARTS RECORD HERE
      *
     C*** BEGIN ADD *** 02/10/99 **************************************
     C                   MOVE      '0'           *IN37
     C*** END ADD ***** 02/10/99 **************************************
     C   36              EXSR      SRPRNT
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXSR      SRPRNT
     C                   END
      *
     C                   EXSR      SRPRT1
     C                   MOVE      '0'           *IN36
     C*** BEGIN DLT *** 02/10/99 **************************************
     C******************** MOVE '0'       *IN37
     C*** END DLT ***** 02/10/99 **************************************
      *
     C                   END
      *
     C     C2KY          READE     PCM02                                  62
     C                   ENDDO
      *
     C   37              EXSR      SRPRNT
      *
      *   DELETE THE CURRENT RECORD
      *
     C   99              DELETE    PDT01
     C  N99              DELETE    PDT02
      *
      *  Retrieve the next transfer record.
      *
     C   99DTKY          READE     PDT01                                  61
     C  N99DTKY          READE     PDT02                                  61
     C                   ENDDO
      *
      *  END THE PROGRAM
      *
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      * ***********************************************************************
      *  THE SUBROUTINE PERFORMS A PAGE BREAK BETWEEN FUNDS
      * ***********************************************************************
     C     SRFND         BEGSR
      *
     C                   MOVE      *BLANKS       XXFNAM
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        XXFNAM
      *
     C                   EXCEPT    HEAD
     C                   Z-ADD     XXFND         SVFND
      *
     C                   ENDSR
      * ***********************************************************************
      *  THE SUBROUTINE PRINTS THE DETAIL RECORDS
      * ***********************************************************************
     C     SRPRNT        BEGSR
      *
     C                   SELECT
      *
     C     *IN99         WHENEQ    *ON
     C     DTCASE        ANDNE     *ZERO
      *
     C                   EXCEPT    DET
      *
     C     *IN99         WHENEQ    *OFF
     C     DTEMP#        ANDNE     *ZERO
      *
     C                   EXCEPT    DET
      *
     C                   ENDSL
      *
     C                   ENDSR
      * ***********************************************************************
      *  THE SUBROUTINE PRINTS THE DATE, CODE AND DARTS COMMENT
      * ***********************************************************************
     C     SRPRT1        BEGSR
      *
     C                   SELECT
      *
     C     *IN99         WHENEQ    *ON
     C     DTCASE        ANDNE     *ZERO
      *
     C                   EXCEPT    DET1
      *
     C     *IN99         WHENEQ    *OFF
     C     DTEMP#        ANDNE     *ZERO
      *
     C                   EXCEPT    DET1
      *
     C                   ENDSL
      *
     C                   ENDSR
     O* ***********************************************************************
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
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                13 '(C) COPYRIGHT'
     O***                                30 'PROGRESSIVE DATA'
     O***                                46 'SOLUTIONS, INC.'
     O***                      *YEAR     51
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END ADD ****** 01/28/10 ***********************************
     O* **************************************************************
     O* REPORT DETAIL
     O* **************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              86
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       XXFNAM              71
     O          E            HEAD           1
     O               99                          03 'YR'
     O               99                          09 'CASE'
     O              N99                          11 'EMP#'
     O              N99                          18 'DIV'
     O               99                          27 'CLAIMANT NAME'
     O              N99                          33 'EMPLOYER NAME'
     O                                           62 'CATEGORY CHANGED'
     O                                           74 'OLD USER'
     O                                           85 'NEW USER'
     O                                          101 'REQUESTED BY'
     O          E            HEAD           2
     O                                           25 'REMARK DATE'
     O                                           31 'CODE'
     O                                           41 'COMMENT'
     O*
     O*  DETAIL - CLAIMANT
     O*
     O          E            DET            1
     O               99      XXFYR         Z     03
     O               99      XXCASE        Z     12
     O              N99      DTEMP#        Z     11
     O              N99      DTDIV         Z     18
     O               99      XXCMNM              44
     O              N99      XXNAME              44
     O               21                          56 'SUPERVISOR'
     O               22                          61 'CLAIMS EXAMINER'
     O               23                          60 'FIELD ADJUSTER'
     O               24                          58 'UNIT MANAGER'
     O               25                          61 'ACCOUNT MANAGER'
     O               26                          56 'CASE NURSE'
     O               27                          58 'SDF ADJUSTER'
     O               28                          63 'EXCESS SUPERVISOR'
     O               29                          57 'UNDERWRITER'
     O               30                          64 'POLICY SERVICE REP'
     O               31                          65 'PREMIUM ACCOUNT REP'
     O               32                          60 'AUDIT REVIEWER'
     O               33                          59 'AUDIT MANAGER'
     O               34                          62 'LOSS CONTROL REP'
     O                       XFUSER              76
     O                       XTUSER              87
     O                       DTUSER             100
     O               37                         124 '** NO DARTS REMARKS'
     O               37                         132 'CHANGED'
     O*
     O*  DETAIL - CLAIMANT DARTS REMARKS
     O*
     O          E            DET1           1
     O                       XXCDAT              22 '  /  /    '
     O                       C2RMCD              32
     O                       XXCOMM             132
