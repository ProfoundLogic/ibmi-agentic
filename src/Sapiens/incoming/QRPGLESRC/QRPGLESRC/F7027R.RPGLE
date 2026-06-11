     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*********************************************************************
     H* PROGRAM:  F7027R                                                  *
     H* DESCRIPTION:  UPDATE FAEOP RECORD WITH WORKERS COMP CHECK #       *
     H*********************************************************************
     H* INDICATOR USAGE:                                                  *
     H* 50     CHAIN/READE DOWEQ ON WARPL/PRP01                           *
     H* 60     CHAIN/READE DOWEQ ON WACPL/PCP01                           *
     H* 70     CHAIN ON FAEOL4/PEO01                                      *
     H* 75     FBAL04, POS1 = 'Y'                                         *
     H*********************************************************************
     H* REVISIONS:                                                    *
     H* 07/10/15 C:180     SUPPORT 34096                              *
     H*                    Add extra level of fein verification       *
     H*                    when assigning check# to EOB               *
     H*                                                               *
     H* 12/17/99 C:62      PROJECT: Y2K                               *
     H*                    FIX DATE CONVERSIONS.                      *
     H*                                                               *
     H* 10/11/99 C:62      PROJECT: MC212                             *
     H*                    ADD LOGIC FOR PBILLS.                      *
     H*                                                               *
     H* 02/23/98 C:53      REMOVE QUERY                               *
     H* 12/04/96 C:59      UPDATE FROM FS060                          *
     H* 01/25/95 C:20      REQ/PROJ: 12/60MAL                         *
     H*                    1. IF BILL REVIEW CHECK HAS BEEN PROCESSED *
     H*                       UPDATE EOBLFL = 'P' AND EOHRFL = 'R'    *
     H*                                                               *
     H* 05/12/95 C:20      REQ/PROJ: 12/60MAL                         *
     H*                    1. REMOVE UPDATE OF EOUSER FIELD           *
     H*****************************************************************
     FWARPL     IF   E           K DISK
     FWACPL     IF   E           K DISK
     FFAEOL4    UF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL1    IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)
     FFARPP     O    E           K DISK
     F                                     RENAME(PRP01:PRF01)
     FFAE1L     UF   E           K DISK
     F*** BEGIN ADD *** 10/11/99 **************************************
     FFAEAL1    UF   E           K DISK
     F*** END   ADD *** 10/11/99 **************************************
     FWMNRL     IF   E           K DISK

     D*
     D*** BEG DEL *** 02/23/98 ***************************
     D********            QCA     1   1 56               CMD EXEC/QCMDEXC
     D*** END DEL *** 02/23/98 ***************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CPFND
     C*
     C*  CHECK PROCESSING KEY LIST
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDATE
     C*
     C*
     C* Xref to WCMS Fund # to get WCMC Sub#/Fund
     C     FNKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C*
     C*   KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C*  EOMB KEY LIST
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C                   KFLD                    EORCN
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C*  EOMB KEY LIST
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C*
     C*  EOMB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C                   KFLD                    RPEOB#
     C*
     C*  REVIEW/NETWORK FILE
     C*
     C     NRKEY         KLIST
     C                   KFLD                    RPCO#
     C                   KFLD                    RPFEIN
     C                   KFLD                    RPMOD
     C*
     C*  SYSTEM CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
     C                   PARM                    QQTYPE            1
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
     C*  DEFINE WORK FIELDS
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPDT          XXDATE
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    FNSUB#        XXSUB#
     C     *LIKE         DEFINE    FNSUB#        MCHSUB
     C*** BEGIN ADD *** 10/11/99 **************************************
     C*
     C* FIND OUT IF PROVIDER BILLS IS INSTALLED
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 10/11/99 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        X1DATE            8 0
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   TIME                    DSDTTM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              51
     C*
     C*  START READING THE CHECK PROCESSING FILE, WE MUST HAVE
     C*  A RECORD FOR THIS DATE
     C*
     C     CPKEY         CHAIN     PCP01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C*  THE CHECK PROCESSING FLAG MUST BE FLIPPED
     C*
     C     CPSTA0        IFEQ      'Y'
     C     CPSTA1        ANDEQ     'Y'
     C*
     C*  START READING ALL PAYMENT RECORDS
     C*
     C     RPKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  IF RECORD PROCESSED, UPDATE EOMB FILE WITH THE WC CHECK#
     C*
     C     RPSTAT        IFEQ      'Y'
     C*
     C* BEFORE WE CHAIN TO EOMB FILE WE MUST TRANSLATE WC FUND INTO
     C* WCMC SUB#/FND
     C*
     C* Go to FMFNP to get xref between WCMS Fund and WCMC Subscriber/Fund
     C     FNKEY2        CHAIN     PFN02                              51        WCFND Key
     C     *IN51         IFEQ      '0'
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C                   ELSE                                                   ELSE 0 *IN51
     C                   Z-ADD     RPCO#         RPCO#
     C                   Z-ADD     RPFND         RPFND
     C                   Z-ADD     RPFYR         RPFYR
     C                   Z-ADD     RPCASE        RPCASE
     C                   Z-ADD     RPEOB#        RPEOB#
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   WRITE     PRF01
     C                   END                                                    END 0 *IN51
     C*
     C*
     C*  UPDATE THE EOMB FILE WITH THE WORKERS COMP CHECK #
     C*
     C     EOKEY         CHAIN     PEO01                              70
     C     *IN70         IFEQ      '0'
     C*
     C*** BEGIN CHG *** 10/11/99 **************************************
     C********** E1KEY     CHAINPE101                71
     C  N16E1KEY         CHAIN     PE101                              71
     C   16EAKY          CHAIN     PEA01                              71
     C*** END   CHG *** 10/11/99 **************************************
     C     *IN71         IFEQ      '0'
     C*
     C     EOFEIN        IFEQ      RPFEIN
     C     EOMOD         ANDEQ     RPMOD
     C*** Begin Add *** 07/10/15 ************************************
     C     *IN16         ANDEQ     *OFF
      *
     C     EOFEIN        OREQ      RPFEIN
     C     EOMOD         ANDEQ     RPMOD
     C     *IN16         ANDEQ     *ON
     C     EOFEIN        ANDEQ     EAFEIN
     C     EOMOD         ANDEQ     EAMOD
     C*** End   Add  *** 07/10/15 ***********************************
      *
     C                   Z-ADD     RPCHK#        EOWCCK
     C*** BEGIN ADD *** 10/11/99 **************************************
     C   16              Z-ADD     RPCHK#        EAWCPC
     C*** END   ADD *** 10/11/99 **************************************
     C*** BEGIN CHG *** 10/11/99 **************************************
     C  N16              Z-ADD     CPPRDT        E1ENDT
     C*** BEGIN CHG *** 12/17/99 **************************************
     C***16*****           Z-ADDCPPRDT    EAWCPD
     C   16              Z-ADD     CPPRDT        SRDTI
     C   16              CALL      'SRCVTDAT'    DTMDYY
     C   16              MOVEL     SRDTO         EAWCPD
     C*** END   CHG *** 12/17/99 **************************************
     C*** END   CHG *** 10/11/99 **************************************
     C                   END
     C*
     C*  CHECK THE GROUP PROVIDER FILE
     C*
     C     NRKEY         CHAIN     PNR01                              72
     C     *IN72         IFEQ      '0'
     C*
     C*  UPDATE THE BILL REVIEW INFORMATION
     C*
     C     NRGRTY        IFEQ      'B'
     C*** BEGIN CHG *** 10/11/99 **************************************
     C  N16              MOVEL     RPCHK#        E110A2
     C  N16              Z-ADD     CPPRDT        E1DT5
     C   16              MOVEL     RPCHK#        EAWCBC
     C*** BEGIN CHG *** 12/17/99 **************************************
     C***16*****           Z-ADDCPPRDT    EAWCBD
     C   16              Z-ADD     CPPRDT        SRDTI
     C   16              CALL      'SRCVTDAT'    DTMDYY
     C   16              MOVEL     SRDTO         EAWCBD
     C*** END   CHG *** 12/17/99 **************************************
     C*** END   CHG *** 10/11/99 **************************************
     C*
     C*** BEGIN ADD *** 01/25/95 ***********************************************
     C                   MOVE      'R'           EOHRFL
     C                   MOVE      'B'           EOBLFL
     C*** BEGIN ADD *** 10/11/99 **************************************
     C   16              MOVE      'R'           EABHRF
     C   16              MOVE      'B'           EABBLF
     C*** END   ADD *** 10/11/99 **************************************
     C*** END ADD *** 01/25/95 *************************************************
     C                   END                                                      END EQ B
     C*
     C*  UPDATE THE NETWORK INFORMATION
     C*
     C     NRGRTY        IFEQ      'N'
     C*** BEGIN CHG *** 10/11/99 **************************************
     C  N16              MOVEL     RPCHK#        E110A1
     C  N16              Z-ADD     CPPRDT        E1DT4
     C  N16              Z-ADD     RPAMT         E1AMT1
     C   16              MOVEL     RPCHK#        EAWCNC
     C*** BEGIN CHG *** 12/17/99 **************************************
     C***16*****           Z-ADDCPPRDT    EAWCND
     C   16              Z-ADD     CPPRDT        SRDTI
     C   16              CALL      'SRCVTDAT'    DTMDYY
     C   16              MOVEL     SRDTO         EAWCND
     C*** END   CHG *** 12/17/99 **************************************
     C   16              Z-ADD     RPAMT         EANAMT
     C*** END   CHG *** 10/11/99 **************************************
     C                   END
     C*
     C*  UPDATE OTHER GROUP CHECK INFORMATION
     C*
     C     NRGRTY        IFEQ      'O'
     C*** BEGIN CHG *** 10/11/99 **************************************
     C  N16              MOVEL     RPCHK#        E110A2
     C  N16              Z-ADD     CPPRDT        E1DT5
     C   16              MOVEL     RPCHK#        EAWCBC
     C*** BEGIN CHG *** 12/17/99 **************************************
     C***16*****           Z-ADDCPPRDT    EAWCBD
     C   16              Z-ADD     CPPRDT        SRDTI
     C   16              CALL      'SRCVTDAT'    DTMDYY
     C   16              MOVEL     SRDTO         EAWCBD
     C*** END   CHG *** 12/17/99 **************************************
     C*** END   CHG *** 10/11/99 **************************************
     C                   END
     C*
     C*** BEGIN ADD *** 01/25/95 ***********************************************
     C**********           MOVE Q1USER    E1USER           DEL 05/12/95
     C*** BEGIN CHG *** 10/11/99 **************************************
     C  N16              MOVE      Q1PGM         E1PGM
     C  N16              Z-ADD     DSDATE        SRDTI
     C  N16              CALL      'SRCVTDAT'    DTCYMD
     C  N16              MOVEL     SRDTO         E1DT
     C  N16              Z-ADD     DSTIME        E1TIME
     C   16              MOVE      Q1PGM         EAPGM
     C   16              Z-ADD     DSDATE        SRDTI
     C   16              CALL      'SRCVTDAT'    DTCYMD
     C   16              MOVEL     SRDTO         EADT
     C   16              Z-ADD     DSTIME        EATIME
     C*** END   CHG *** 10/11/99 **************************************
     C*** END ADD *** 01/25/95 *************************************************
     C*
     C                   END
     C*
     C*** BEGIN ADD *** 01/25/95 ***********************************************
     C**********           MOVE Q1USER    EOUSER           DEL 05/12/95
     C                   MOVE      Q1PGM         EOPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C                   Z-ADD     DSTIME        EOTIME
     C*** END ADD *** 01/25/95 *************************************************
     C*  UPDATE EOB FILES
     C*
     C*** BEGIN CHG *** 10/11/99 **************************************
     C**********           UPDATPE101
     C  N16              UPDATE    PE101
     C   16              UPDATE    PEA01
     C*** END   CHG *** 10/11/99 **************************************
     C                   END
     C                   UPDATE    PEO01
     C                   END                                                    END 0 *IN70
     C*
     C                   END                                                    END RPSTAT=Y
     C*
     C     RPKEY         READE     PRP01                                  60
     C                   END                                                    END DOWEQ 60
     C*
     C                   END                                                    END STA0/STA1=Y
     C*
     C     CPKEY         READE     PCP01                                  50
     C                   END                                                    END DOWEQ 50
     C*
     C*** BEG DEL *** 02/23/98 ***************************
     C* CALL QUERY TO PRINT ANY THAT DID NOT FIND GROUP XREF
     C********
     C********             Z-ADD56        LENGTH
     C********             CALL 'QCMDEXC'                60
     C********             PARM           QCA,1
     C********             PARM           LENGTH 155
     C*** END DEL *** 02/23/98 ***************************
     C*
     C                   MOVE      '1'           *INLR
     C*** BEG DEL *** 02/23/98 ***************************
     C** **  QCMDEXC TABLE
     C** RUNQRY QRY(*LIBL/QRYFARPP)
     C*** END DEL *** 02/23/98 ***************************
