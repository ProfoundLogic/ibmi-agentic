     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0493R - Complete/Pend selection
     H*****************************************************************
     H*                                                               *
     H*  CALLED BY: F0490R                                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*  03/25/14 C:212 Project: MC376
     H*                 ANSI X12 Medical EDI : call SR 837 for NC
     H*
     H*  08/17/11 C:251 Project: TX075
     H*                 ALLOW F1 = CANCEL FOR TX BILL
     H*
     H*  06/24/11 C:079 Project: SY255
     H*                 ADD SRCA SUBROUTINE
     H*
     H*  06/13/08 C:079 Project: FL061
     H*                 CHECK TO SEE IF IN CAPTURE POLICY STATE MODE
     H*                                                               *
     H*  05/03/07 C:188 PROJECT: FL061                                *
     H*                 USE EOPOST IF NOT BLANKS, ELSE EOAST          *
     H*                                                               *
     H*  11/15/06 C:079 SUPPORT: 55638                                *
     H*                 TX 837 IMPLEMENTATION                         *
     H*                                                               *
     H*  11/11/05 C:163 PROJECT: FL052                                *
     H*                 FL EDI CHANGES 2005                           *
     H*                                                               *
     H*  08/03/05 C:79 SUPPORT: 48400                                 *
     H*                CORRECT CHAINING/UPDATING FAEOP                *
     H*                                                               *
     H*  03/07/05 C:79 SUPPORT: 45991                                 *
     H*                SET EOTAPE = B FOR BYPASSING                   *
     H*                                                               *
     H*  04/15/04 C:79 PROJECT FL046                                  *
     H*                FL: SET EOTAPE = R FOR RESUBMISSION W FOR      *
     H*                WITHDRAWAL                                     *
     H*                                                               *
     H*  07/18/02 C:79 PROJECT TX017                                  *
     H*                ALLOW TEXAS HOSPITAL BILLS TO BE RESUBMITTED   *
     H*                                                               *
     H*  02/08/02 C:62 PROJECT TX017                                  *
     H*                ADD PROCESSING FOR TEXAS RESUBMISSIONS         *
     H*                                                               *
     H*****************************************************************
     F*
     FF0493F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     FFAEOL32   UF   E           K DISK
     F*** BEG ADD *** 04/15/04 ****************************************
     F                                     INFDS(DBFD1)
     F*** END ADD *** 04/15/04 ****************************************
     FFELGL1    IF   E           K DISK
     F* WCMC Case Lockout File
     FFXLOP     UF   E           K DISK
     F*** BEGIN ADD *** 02/08/02 **************************************
     FFAEAL1    UF   E           K DISK
     FFAEHL     UF   E           K DISK
     FFAEUL     UF   E           K DISK
     F*** END   ADD *** 02/08/02 **************************************
     F*

     D*** BEG ADD *** 04/15/04 ****************************************
     D FKY             S             54    DIM(2) CTDATA PERRCD(1)
     D*** END ADD *** 04/15/04 ****************************************
     D*** BEGIN ADD *** 02/08/02 **************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** END   ADD *** 02/08/02 **************************************
     D OPTNM           C                   CONST('Reset EOMB for Data -
     D                                     Submission')
     D*** BEGIN ADD *** 02/08/02 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   ADD *** 02/08/02 **************************************
     D*
     D*** BEG ADD *** 04/15/04 ****************************************
     D DBFD1           DS
     D  DBRRN1               397    400B 0

     I*** END ADD *** 04/15/04 ****************************************

     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CNTL           20
     C                   PARM                    @@CONM           40
     C                   PARM                    @@USER           10
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@ST              2
     C*** BEGIN ADD *** 02/08/02 **************************************
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
     C     EAKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EHKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*
     C     EUKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCNTL
     C                   KFLD                    KKEOB#
     C*
     C*  FELGL - EDI Log File
     C*
     C     LGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     *LIKE         DEFINE    EOCO#         @@CO#
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOCNTL        KKCNTL
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C*** BEG ADD *** 05/03/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 05/03/07 ****************************************
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     *LIKE         DEFINE    EAESDT        XXESDT
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C                   MOVEL     @@CONM        SCCONM
     C                   MOVEL     @@USER        SCUSER
     C                   MOVEL     OPTNM         SCPRDS
     C                   MOVEL     @@CO#         KKCO#
     C                   MOVEL(P)  @@CNTL        KKCNTL
     C                   Z-ADD     @@EOB#        KKEOB#
     C*** BEG CHG *** 08/03/05 ****************************************
     C********** EOKY      CHAINPEO01                60
     C     EOKY          CHAIN(N)  PEO01                              60
     C*** END CHG *** 08/03/05 ****************************************
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C*** BEGIN ADD *** 02/08/02 **************************************
     C                   SETOFF                                       1690
     C*** BEG ADD *** 05/03/07 ****************************************
     C     EOPOST        IFNE      *BLANKS
     C*** BEG ADD *** 06/13/08 ****************************************
     C     EOCPST        ANDEQ     'Y'
     C*** END ADD *** 06/13/08 ****************************************
     C                   MOVE(P)   EOPOST        EDITST
     C                   ELSE
     C                   MOVE(P)   EOAST         EDITST
     C                   ENDIF
     C*** END ADD *** 05/03/07 ****************************************
     C*** BEG CHG *** 05/03/07 ****************************************
     C*****      EOAST     IFEQ 'TX'
     C     EDITST        IFEQ      'TX'
     C*** END CHG *** 05/03/07 ****************************************
     C*** BEG DEL *** 07/18/02 ****************************************
     C********** EOSYS     ANDNE'H'
     C*** END DEL *** 07/18/02 ****************************************
     C*** BEG DEL *** 11/15/06 ****************************************
     C******************** SETON                     16
     C*** END DEL *** 11/15/06 ****************************************
     C                   ENDIF
     C*** BEG ADD 04/15/04 ********************************************
     C*
     C* LOAD THE FUNCTION KEY LINE(S)
     C*
     C                   SETOFF                                       17
     C*
     C                   SELECT
     C*
     C     @@ST          WHENEQ    'FL'
     C     EOTAPE        ANDNE     'Y'
     C*
     C                   MOVEL(P)  FKY(2)        SCFKY1
     C                   SETON                                        17
     C*
     C                   OTHER
     C                   MOVEL(P)  FKY(1)        SCFKY1
     C*
     C                   ENDSL
     C*** END ADD 04/15/04 ********************************************
     C*
     C     *IN90         DOUEQ     *OFF
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C                   EXFMT     SCR01
     C*** BEGIN ADD *** 02/08/02 **************************************
     C                   SETOFF                                       90
     C     *IN16         IFEQ      *ON
     C     *INKL         ANDEQ     *OFF
     C                   EXSR      ERRCK
     C                   ENDIF
     C                   ENDDO
     C*** END   ADD *** 02/08/02 **************************************
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @@ST          CASEQ     'FL'          SRFL
     C*** BEG CHG *** 11/15/06 ****************************************
     C*********  @@ST      CASEQ'KY'      SRKY
     C*********  @@ST      CASEQ'TX'      SRTX
     C     @@ST          CASEQ     'KY'          SR837
     C     @@ST          CASEQ     'TX'          SR837
     C*** END CHG *** 11/15/06 ****************************************
     C*** BEG ADD *** 06/24/11 ****************************************
     C     @@ST          CASEQ     'CA'          SR837
     C*** END ADD *** 06/24/11 ****************************************
     C***** beg add *** 03/25/14 **************************************
     C     @@ST          CASEQ     'NC'          SR837
     C***** end add *** 03/25/14 **************************************
     C                   ENDCS
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   SETON                                        LR
     C                   MOVE      *ON           *INKC
     C*
     C* REMOVE RECORD FROM LOCKOUT FILE
     C*
     C     LOKY          CHAIN     PLO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   RETURN
     C*
     C*** BEGIN ADD *** 02/08/02 **************************************
     C*================================================================
     C* ERRCK - ERRCK SUBROUTINE
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C* DATE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCESDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCESDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C* MUST BE VALID EDI SUBMISSION DATE
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL     *OFF          FOUND             1
     C     EAKY          CHAIN(N)  PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     SCESDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXESDT
     C*
     C     EAESDT        IFEQ      XXESDT
     C                   MOVEL     *ON           FOUND
     C                   LEAVE
     C                   ENDIF
     C*
     C     EAKY          READE(N)  PEA01                                  99
     C                   ENDDO
     C*
     C     FOUND         IFEQ      *OFF
     C                   SETON                                        7290
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 02/08/02 **************************************
     C*================================================================
     C* SRFL - Subroutine for FL resubmission process
     C*================================================================
     C     SRFL          BEGSR
     C*
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C*** BEG DEL *** 04/15/04 ****************************************
     C**********
     C********** EOTAPE    IFEQ 'Y'
     C**********           MOVE *BLANKS   EOSBDT
     C**********           ELSE
     C**********           MOVE *BLANKS   EOSBDT
     C**********           MOVE *BLANKS   EOCNTL
     C**********           ENDIF                           EOTAPE=Y
     C*** END DEL *** 04/15/04 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C                   SELECT
     C*
     C     *INKB         WHENEQ    *ON
     C                   MOVE      'W'           EOTAPE
     C*** BEG ADD *** 03/07/05 ****************************************
     C*
     C     *INKD         WHENEQ    *ON
     C                   MOVE      'B'           EOTAPE
     C*** END ADD *** 03/07/05 ****************************************
     C*** BEG ADD *** 11/11/05 ****************************************
     C*
     C     *INKF         WHENEQ    *ON
     C                   MOVE      'D'           EOTAPE
     C*** END ADD *** 11/11/05 ****************************************
     C*
     C                   OTHER
     C*
     C     EOTAPE        IFEQ      'Y'
     C                   MOVE      *BLANKS       EOSBDT
     C                   ELSE
     C                   MOVE      'R'           EOTAPE
     C                   ENDIF
     C*
     C                   ENDSL
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C                   MOVE      'C'           EOBLST
     C*** BEG ADD *** 08/03/05 ****************************************
     C                   UPDATE    PEO01
     C*** END ADD *** 08/03/05 ****************************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** BEG DEL *** 08/03/05 ****************************************
     C******************** UPDATPEO01
     C*** END DEL *** 08/03/05 ****************************************
     C*** BEG ADD *** 04/15/04 ****************************************
     C*
     C* UPDATE THE EDI FIELDS IN THE PROVIDER BILLS HEADER FILES
     C*
     C                   CALL      'FA740R'
     C                   PARM      DBRRN1        ##RRN             9 0
     C                   PARM                    ##RTCD            1
     C*** END ADD *** 04/15/04 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SR837 - Subroutine for 837 resubmission process
     C*================================================================
     C*** BEG CHG *** 11/15/06 ****************************************
     C********** SRKY      BEGSR
     C     SR837         BEGSR
     C*** END CHG *** 11/15/06 ****************************************
     C*
     C     EOKY          CHAIN     PEO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     LGKY          SETGT     PLG01
     C     LGKY          READPE(N) PLG01                                  99
     C*
     C                   MOVEL     KKCO#         XXCO#
     C                   MOVEL     KKSUB#        XXSUB#
     C                   MOVEL     KKFND         XXFND
     C                   MOVEL     EOFYR         XXFYR
     C                   MOVEL     EOCASE        XXCASE
     C                   MOVEL     KKEOB#        XXEOB#
     C*
     C*** Beg Add *** 08/17/11 *******************************************
     C*
     C                   MOVE      LGRCN         XXRCN
     C*
     C*** If F1 =  *On, Call program with action falg = 'D'
     C                   SELECT
     C     *INKA         WHENEQ    *ON
     C*
     C                   CALL      'F6250R'
     C                   PARM                    XXCO#             3            COMPANY
     C                   PARM                    XXSUB#            5            SUBSCRIBER #
     C                   PARM                    XXFND             3            GROUP #
     C                   PARM                    XXFYR             3            POLICY YR
     C                   PARM                    XXCASE            7            CASE/CLAIM
     C                   PARM                    XXEOB#            9            EOMB#
     C                   PARM                    XXRCN             3            RECON #
     C                   PARM      'D'           XXACTF            1            ACTION FLAG
     C                   PARM      ' '           XXRTCD            1
     C*
     C                   OTHER
     C*** End Add *** 08/17/11 *******************************************
     C     LGMTC         IFEQ      '00'
     C     LGSTS         ANDNE     'TA'
     C     LGSTS         ANDNE     'TE'
     C                   CALL      'F6250R'
     C                   PARM                    XXCO#             3            COMPANY
     C                   PARM                    XXSUB#            5            SUBSCRIBER #
     C                   PARM                    XXFND             3            GROUP #
     C                   PARM                    XXFYR             3            POLICY YR
     C                   PARM                    XXCASE            7            CASE/CLAIM
     C                   PARM                    XXEOB#            9            EOMB#
     C*** Beg Chg *** 08/17/11 ******************************************
     C******               PARM '000'     XXRCN   3        RECON #
     C                   PARM                    XXRCN             3            RECON #
     C*** End Chg *** 08/17/11 ******************************************
     C                   PARM      ' '           XXACTF            1            ACTION FLAG
     C                   PARM      ' '           XXRTCD            1
     C                   ELSE
     C                   CALL      'F6250R'
     C                   PARM                    XXCO#             3            COMPANY
     C                   PARM                    XXSUB#            5            SUBSCRIBER #
     C                   PARM                    XXFND             3            GROUP #
     C                   PARM                    XXFYR             3            POLICY YR
     C                   PARM                    XXCASE            7            CASE/CLAIM
     C                   PARM                    XXEOB#            9            EOMB#
     C*** Beg Chg *** 08/17/11 ******************************************
     C******               PARM '000'     XXRCN   3        RECON #
     C                   PARM                    XXRCN             3            RECON #
     C*** End Chg *** 08/17/11 ******************************************
     C                   PARM      'C'           XXACTF            1            ACTION FLAG
     C                   PARM      ' '           XXRTCD            1
     C                   ENDIF
     C*
     C*** Beg Add *** 08/17/11 ******************************************
     C                   ENDSL
     C*** End Add *** 08/17/11 ******************************************
     C                   ENDSR
     C*================================================================
     C* SRTX - Subroutine for TX resubmission process
     C*================================================================
     C     SRTX          BEGSR
     C*** BEGIN ADD *** 02/08/02 **************************************
     C     *IN16         IFEQ      *ON
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     SCESDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXESDT
     C*
     C     EAESDT        IFEQ      XXESDT
     C                   MOVEL     'R'           EAEDI
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ENDIF
     C*
     C                   UPDATE    PEA01
     C     EAKY          READE     PEA01                                  99
     C                   ENDDO
     C*
     C     EOBTYP        IFEQ      'N'
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'R'           EHEDI
     C                   MOVEL     Q1PGM         EHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
     C                   UPDATE    PEH01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOBTYP        IFEQ      'H'
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'R'           EUEDI
     C                   MOVEL     Q1PGM         EUPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C                   UPDATE    PEU01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'R'           EOTAPE
     C                   UPDATE    PEO01
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C                   ENDIF                                                  *IN16 = *ON
     C*
     C*** END   ADD *** 02/08/02 **************************************
     C                   ENDSR
**  FKY
F1= EOMB Cancellation   F12=Cancel
F2=Withdraw   F4=Bypass   F6=Bypass Dup   F12=Cancel
