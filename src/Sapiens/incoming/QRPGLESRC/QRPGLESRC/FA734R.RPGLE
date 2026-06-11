     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA734R - Checks to see if Prudent Buy Discount applies.  DATA
     H*
     H*****************************************************************
     H* Created on 03/06/2002 by C:62 for Project 114MDE
     H*****************************************************************
     H*                                                               *
     H*    CALLED FROM: N9600R                                        *
     H*    CALLS TO: NONE                                             *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/08/14 C:79 SUPPORT: 87533                                 *
     H*                Use new data fields for percentages            *
     H*                                                               *
     H*  06/28/05 C:79 SUPPORT: 47835                                 *
     H*                SKIP INACTIVE RECORDS                          *
     H*                                                               *
     H*  03/21/05 C:62 PROJECT 114MJD                                 *
     H*                INCORPORATE MADIGAN DISCOUNT                   *
     H*                                                               *
     H*  05/14/04 C:62 PROJECT 114MIC                                 *
     H*                ADD INPATIENT % PROCESSING                     *
     H*                                                               *
     H*  07/24/03 C:62 PROJECT MC114 38209                            *
     H*                CHANGE CONDITION TO AN AND NOT OR.             *
     H*                                                               *
     H*  04/15/03 C:62 PROJECT 114MFG                                 *
     H*                CHECK AGAINST % OFF MRA                        *
     H*                                                               *
     H*  03/27/03 C:79 SUPPORT MC114 36638                            *
     H*                PROCESS HOS AND CAH LIKE OUTPATIENT            *
     H*                                                               *
     H*  10/30/02 C:62 PROJECT 114MEV                                 *
     H*                PROCESS REHAB INPATIENT LIKE OUTPATIENT        *
     H*                                                               *
     H*  05/21/02 C:62 PROJECT:MC114 32641                            *
     H*                GET RID OF RECORD LOCK                         *
     H*                                                               *
     H*  05/02/02 C:62 PROJECT:114MDE                                 *
     H*                -CHANGE HOW RETURN CODE IS POPULATED           *
     H*                -POPULATE EOFLG3                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*****************************************************************
     F*** BEGIN CHG *** 05/02/02 **************************************
     F**********FAEOL4  IF  E           K        DISK
     FFAEOL4    UF   E           K DISK
     F*** END   CHG *** 05/02/02 **************************************
     FFAEUL     IF   E           K DISK
     FFMS4L2    IF   E           K DISK
     FNMBDL1    IF   E           K DISK
     FFDCALR    IF   E           K DISK
     F*** BEGIN CHG *** 05/02/02 **************************************
     FFDDUL     IF   E           K DISK
     F*** END   CHG *** 05/02/02 **************************************
     F*

     D*** BEGIN ADD *** 05/02/02 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** END   ADD *** 05/02/02 **************************************

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@SUB#
     C                   PARM                    @@FND
     C                   PARM                    @@FYR
     C                   PARM                    @@CASE
     C                   PARM                    @@EOB#
     C                   PARM                    @@NWCD
     C                   PARM                    @@CODE
     C                   PARM                    @@EFDT
     C                   PARM                    @@RTCD            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C*
     C     BDKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKBGDT
     C*
     C     CAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** BEGIN CHG *** 05/02/02 **************************************
     C     DUKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*** END   CHG *** 05/02/02 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C*
     C     EUKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     S4KY1         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    KKNWCD
     C*
     C     S4KY2         KLIST
     C                   KFLD                    EOFEIN
     C                   KFLD                    EOMOD
     C                   KFLD                    KKNWCD
     C                   KFLD                    KKBGDT
     C*
     C     *LIKE         DEFINE    EOCO#         @@CO#
     C     *LIKE         DEFINE    EOSUB#        @@SUB#
     C     *LIKE         DEFINE    EOFND         @@FND
     C     *LIKE         DEFINE    EOFYR         @@FYR
     C     *LIKE         DEFINE    EOCASE        @@CASE
     C     *LIKE         DEFINE    EOEOB#        @@EOB#
     C     *LIKE         DEFINE    BDCODE        @@CODE
     C     *LIKE         DEFINE    S4NWCD        @@NWCD
     C     *LIKE         DEFINE    S4BGDT        KKBGDT
     C     *LIKE         DEFINE    BDCODE        KKCODE
     C     *LIKE         DEFINE    BDTYPE        KKTYPE
     C     *LIKE         DEFINE    BDEFDT        @@EFDT
     C     *LIKE         DEFINE    S4NWCD        KKNWCD
     C*** BEGIN ADD *** 04/15/03 **************************************
     C*** Beg Chg *** 12/08/14 ***************************************
     C**** *LIKE         DEFINE    BDPC02        XXPC02
     C     *LIKE         DEFINE    BDAM13        XXPC02
     C*** End Chg *** 12/08/14 ***************************************
     C*** END   ADD *** 04/15/03 **************************************
     C*** BEGIN ADD *** 05/02/02 **************************************
     C                   MOVEL(P)  *BLANKS       @@RTCD
     C*** END   ADD *** 05/02/02 **************************************
     C*
     C* MUST BE HOSPITAL BILL
     C*
     C     EOKY          CHAIN     PEO01                              99
B01  C     *IN99         IFEQ      *OFF
     C*** BEGIN CHG *** 05/21/02 **************************************
     C********** EOSYS     ANDEQ'H'
     C     EOSYS         IFEQ      'H'
     C*** END   CHG *** 05/21/02 **************************************
     C*
     C* IS THIS A PRUDENT BUY BILL
     C*
     C                   MOVEL(P)  EONTWK        KKNWCD
     C                   Z-ADD     EOFRDT        KKBGDT
     C*
     C     S4KY2         SETGT     PS401
     C*** BEG ADD *** 06/28/05 ****************************************
     C     NXTS4         TAG
     C*** END ADD *** 06/28/05 ****************************************
     C     S4KY1         READPE    PS401                                  99
B02  C     *IN99         IFEQ      *OFF
     C     S4ENDT        ANDGE     KKBGDT
     C*** BEG ADD *** 06/28/05 ****************************************
     C     S4STAT        ANDNE     'I'
     C*** END ADD *** 06/28/05 ****************************************
     C     *IN99         OREQ      *OFF
     C     S4ENDT        ANDEQ     *ZEROS
     C*** BEG ADD *** 06/28/05 ****************************************
     C     S4STAT        ANDNE     'I'
     C*** END ADD *** 06/28/05 ****************************************
     C                   MOVEL(P)  S4DISC        KKCODE
     C                   MOVEL(P)  'DH'          KKTYPE
     C*
     C     BDKY2         SETGT     PBD01
     C     BDKY1         READPE    PBD01                                  99
     C     *IN99         DOWEQ     *OFF
     C     BDSTAT        IFNE      'I'
     C                   LEAVE
     C                   ENDIF
     C     BDKY1         READPE    PBD01                                  99
     C                   ENDDO
     C*
B03  C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C                   MOVEL(P)  'S'           @@RTCD
     C*
     C* IS THIS AN OUTPATIENT BILL
     C*
     C     EUKY          CHAIN     PEU01                              99
B04  C     *IN99         IFEQ      *OFF
     C                   MOVEL     EUBLTY        XXBLTY            2
B05  C     XXBLTY        IFEQ      '13'
     C     XXBLTY        OREQ      '14'
     C     XXBLTY        OREQ      '23'
     C     XXBLTY        OREQ      '33'
     C     XXBLTY        OREQ      '74'
     C     XXBLTY        OREQ      '75'
     C     XXBLTY        OREQ      '84'
     C*** BEGIN ADD *** 10/30/02 **************************************
     C     EOSPCD        OREQ      'REH'
     C*** END   ADD *** 10/30/02 **************************************
     C*** BEGIN ADD *** 03/27/03 **************************************
     C     EOSPCD        OREQ      'CAH'
     C     XXBLTY        OREQ      '83'
     C*** BEGIN CHG *** 07/24/03 **************************************
     C********** EOSPCD    OREQ 'HOS'
     C     EOSPCD        ANDEQ     'HOS'
     C*** END   CHG *** 07/24/03 **************************************
     C*** END   ADD *** 03/27/03 **************************************
     C*
     C* IS THIS A RECONSIDERED BILL
     C*
B06  C     EORCN         IFGT      *ZEROS
     C*** BEGIN DEL *** 05/02/02 **************************************
B07  C********** EONDIS    IFNE *ZEROS
     C*** END   DEL *** 05/02/02 **************************************
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C*** BEGIN CHG *** 05/02/02 **************************************
     C**********           MOVEL'P'       @@RTCD    P
     C                   MOVEL(P)  EOFLG3        @@RTCD
     C*** END   CHG *** 05/02/02 **************************************
     C*** BEGIN DEL *** 05/02/02 **************************************
E07  C**********           ENDIF
     C*** END   DEL *** 05/02/02 **************************************
S06  C                   ELSE
     C*** BEGIN ADD *** 03/21/05 **************************************
     C*
     C* IS THERE A MADIGAN DISCOUNT?
     C*
     C     BDMADG        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'M'           @@RTCD
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDMADG        @@CODE
     C*
     C                   ELSE
     C*** END   ADD *** 03/21/05 **************************************
     C*
     C* SUM UP FEE SCHEDULE VALUES AND PRUDENT BUY VALUES
     C*
     C                   Z-ADD     *ZEROS        XXPAID           11 2
     C                   Z-ADD     *ZEROS        XXCHRG           11 2
     C                   Z-ADD     *ZEROS        XXPRBY           11 2
     C*** BEGIN ADD *** 04/15/03 **************************************
     C*** Beg Chg *** 12/08/14 ***************************************
     C**** 1             SUB       BDPC02        XXPC02
     C                   eval      xxPc02 = 1 - (BDAm13/100)
     C*** End Chg *** 12/08/14 ***************************************
     C*** END   ADD *** 04/15/03 **************************************
     C*
     C     CAKY          CHAIN     PCA01                              99
B08  C     *IN99         DOWEQ     *OFF
B09  C     CAMRAA        IFLT      CACHRG
     C                   ADD       CAMRAA        XXPAID
S09  C                   ELSE
     C                   ADD       CACHRG        XXPAID
E09  C                   ENDIF
     C                   ADD       CACHRG        XXCHRG
     C     CAKY          READE     PCA01                                  99
E08  C                   ENDDO
     C*
     C*** Beg Chg ***  12/08/14 **************************************
     C**** XXCHRG        MULT      BDPC01        XXPRBY
     C                   Eval      xxPrBy = xxChrg * (BDAm12/100)
     C*** End Chg ***  12/08/14 **************************************
     C*** BEGIN ADD *** 04/15/03 **************************************
     C                   MULT      XXPC02        XXPAID
     C*** END   ADD *** 04/15/03 **************************************
B10  C     XXPRBY        IFLT      XXPAID
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C                   MOVEL(P)  'P'           @@RTCD
     C*** BEGIN ADD *** 05/02/02 **************************************
     C                   ELSE
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C                   MOVEL(P)  'S'           @@RTCD
     C*** END   ADD *** 05/02/02 **************************************
E10  C                   ENDIF
E06  C                   ENDIF
     C*** BEGIN ADD *** 03/21/05 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/21/05 **************************************
     C*
     C* INPATIENT
     C*
S05  C                   ELSE
     C     EORCN         IFEQ      0
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C*** BEGIN ADD *** 05/14/04 **************************************
     C*** Beg Chg *** 12/08/14 ****************************************
     C**** BDPC03        IFNE      *ZEROS
     C     BDAm14        IFNE      *ZEROS
     C*** End Chg *** 12/08/14 ****************************************
     C*
     C* SUM UP FEE SCHEDULE VALUES AND PRUDENT BUY VALUES
     C*
     C                   Z-ADD     *ZEROS        XXPAID           11 2
     C                   Z-ADD     *ZEROS        XXCHRG           11 2
     C                   Z-ADD     *ZEROS        XXPRBY           11 2
     C*
     C*** Beg Chg *** 12/08/14 **************************************
     C**** 1             SUB       BDPC02        XXPC02
     C                   eval      xxPc02= 1 - (BDAm13/100)
     C*** End Chg *** 12/08/14 **************************************
     C*
     C*
     C     CAKY          CHAIN     PCA01                              99
B08  C     *IN99         DOWEQ     *OFF
B09  C     CAMRAA        IFLT      CACHRG
     C                   ADD       CAMRAA        XXPAID
S09  C                   ELSE
     C                   ADD       CACHRG        XXPAID
E09  C                   ENDIF
     C                   ADD       CACHRG        XXCHRG
     C     CAKY          READE     PCA01                                  99
E08  C                   ENDDO
     C*
     C*** Beg Chg *** 12/08/14 *************************************
     C**** XXCHRG        MULT      BDPC03        XXPRBY
     C                   Eval      xxPrBy = xxChrg * (BDAm14/100)
     C*** End Chg *** 12/08/14 *************************************
     C                   MULT      XXPC02        XXPAID
     C*
B10  C     XXPRBY        IFLT      XXPAID
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C                   MOVEL(P)  'P'           @@RTCD
     C                   ELSE
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C                   MOVEL(P)  'S'           @@RTCD
E10  C                   ENDIF
     C*
     C                   ELSE
     C*** END   ADD *** 05/14/04 **************************************
     C*** BEGIN CHG *** 05/02/02 **************************************
     C***********          MOVEL'P'       @@RTCD    P
     C     CAKY          CHAIN     PCA01                              99
     C  N99DUKY          CHAIN     PDU01                              99
     C     BDAM01        MULT      DUUNTS        XXAMT            11 2
     C     CACHRG        IFGT      XXAMT
     C*** BEGIN ADD *** 04/15/03 **************************************
     C********** CAPAID    IFGT XXAMT
     C*** Beg Chg *** 12/08/14 ***************************************
     C*****1             SUB       BDPC02        XXPC02
     C                   eval      xxPc02 = 1 - (BDAm13/100)
     C*** End Chg *** 12/08/14 ***************************************
     C     CAPAID        MULT      XXPC02        XXPAID
     C     XXPAID        IFGT      XXAMT
     C*** END   ADD *** 04/15/03 **************************************
     C                   MOVEL(P)  'P'           @@RTCD
     C                   ELSE
     C                   MOVEL(P)  'S'           @@RTCD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL(P)  'S'           @@RTCD
     C                   ENDIF
     C*** BEGIN ADD *** 05/14/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/14/04 **************************************
     C*** END   CHG *** 05/02/02 **************************************
     C                   ELSE
     C*** BEGIN DEL *** 05/02/02 **************************************
     C********** EONDIS    IFNE *ZEROS
     C*** END   DEL *** 05/02/02 **************************************
     C                   MOVEL(P)  S4NWCD        @@NWCD
     C                   MOVEL(P)  BDCODE        @@CODE
     C                   Z-ADD     BDEFDT        @@EFDT
     C*** BEGIN CHG *** 05/02/02 **************************************
     C**********           MOVEL'P'       @@RTCD    P
     C                   MOVEL(P)  EOFLG3        @@RTCD
     C*** END   CHG *** 05/02/02 **************************************
     C*** BEGIN DEL *** 05/02/02 **************************************
     C**********           ENDIF
     C*** END   DEL *** 05/02/02 **************************************
     C                   ENDIF
E05  C                   ENDIF
E04  C                   ENDIF
E03  C                   ENDIF
     C*** BEG ADD *** 06/28/05 ****************************************
     C                   ELSE
     C     S4STAT        IFEQ      'I'
     C     *IN99         ANDEQ     *OFF
     C                   GOTO      NXTS4
     C                   ENDIF
     C*** END ADD *** 06/28/05 ****************************************
E02  C                   ENDIF
     C*** BEGIN ADD *** 05/02/03 **************************************
     C                   MOVEL(P)  @@RTCD        EOFLG3
     C                   MOVEL(P)  Q1USER        EOUSER
     C                   MOVEL(P)  Q1PGM         EOPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*** BEGIN ADD *** 05/21/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/21/02 **************************************
     C                   UPDATE    PEO01
     C*** END   ADD *** 05/02/03 **************************************
E01  C                   ENDIF
     C*
     C                   RETURN
