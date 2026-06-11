     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE F0057R - NJ CARVE OUT CONVERSION PROGRAM
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    F0057C - NJ Fee Schedule Override                          *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*****************************************************************
     H*  Created on 11/30/09 by C:79 for Project 114MVC               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/01/10  C:079  Project:114MVC                               *
     H*                  Upload/Automate NJ Contracted Rates for      *
     H*                  Injections and Immunizations: North/South    *
     H*                                                               *
     H* 02/09/10  C:227  Project:114MVO                               *
     H*                  Upload/Automate NJ Contracted ASC Rates      *
     H*                  Added functionality for NJA discount code    *
     H*                                                               *
     H* 02/01/10  C:220  Project:114MVR                               *
     H*                  Added Rollover and DME processing            *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS:                                                   *
     H*   SCR01:                                                      *
     H*   99  READ  - FXNNP, FXSNP, and FXBJP                         *
     H*   99  CHAIN - NDCOL2 and NMBDL                                *
     H*****************************************************************
     F*
     FFXNNP     IF   E             DISK
     FFXSNP     IF   E             DISK
     FFXBJP     IF   E             DISK
     F***BEG ADD *** 02/09/10 **********************************************
     FFXASP     IF   E             DISK
     F***END ADD *** 02/09/10 **********************************************
     F****** BEG CHG ****** 02/01/10 ***************************************
     F********* NDCOL2    IF A E           K DISK
     F********* NMBDL     IF A E           K DISK
     FNDCOL     IF A E           K DISK    RENAME(PCO01:PCO02)
     FNDCOL2    UF A E           K DISK
     FNMBDL     UF A E           K DISK
     F****** END CHG ****** 02/01/10 ***************************************
     F****** BEG ADD ****** 02/01/10 ***************************************
     FFXDMP     IF   E             DISK
     F****** END ADD ****** 02/01/10 ***************************************
     D*
     D****** BEG ADD ****** 02/01/10 ***************************************
     D XXAMT           S             11  2 INZ(*ZEROS)
     D XXFAMT          S             11  2 INZ(*ZEROS)
     D****** END ADD ****** 02/01/10 ***************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  DATE                   1     10
     D  MONTH                  1      2
     D  HYPH1                  3      3    INZ('/')
     D  DAY                    4      5
     D  HYPH2                  6      6    INZ('/')
     D  YEAR                   7     10
     D*
     D                 DS
     D  XXEFDT                 1      8
     D  XXMNTH                 1      2
     D  XXDAY                  3      4
     D  XXYEAR                 5      8
     D*
     D COMM1           C                   CONST('NORTH NEW JERSEY -
     D                                     FEE SCHEDULE')
     D COMM2           C                   CONST('SOUTH NEW JERSEY -
     D                                     FEE SCHEDULE')
     D*** BEG ADD *** 02/09/10 ****************************************
     D COMM4           C                   CONST('NEW JERSEY ASC -
     D                                     FEE SCHEDULE')
     D*** END ADD *** 02/09/10 ****************************************
     C*
     C*  BILLING CODE TABLE FILE
     C*
     C     BDKY          KLIST
     C                   KFLD                    KKCODE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
     C****** BEG ADD ****** 02/01/10 ***************************************
     C     BDKY2         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KDEFDT            7 0
     C****** END ADD ****** 02/01/10 ***************************************
     C*
     C*  CARVE-OUT TRANSACTION CODE FILE
     C*
     C     COKY1         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKPMOD
     C                   KFLD                    KKEFDT
     C****** BEG ADD ****** 02/01/10 ***************************************
     C     COKY2         KLIST
     C                   KFLD                    KKCODE
     C*
     C     COKY3         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKPMOD
     C                   KFLD                    KDEFDT            7 0
     C*
     C     COKY4         KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKPMOD
     C*
     C****** END ADD ****** 02/01/10 ***************************************
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
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
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
     C*  LIKE DEFINES
     C*
     C     *LIKE         DEFINE    COCODE        KKCODE
     C     *LIKE         DEFINE    COEFDT        KKEFDT
     C     *LIKE         DEFINE    COCPT4        KKCPT4
     C     *LIKE         DEFINE    COPMOD        KKPMOD
     C*
     C*** BEG ADD *** 03/01/10 *********************************************
     C                   EXSR      CLRMOD
     C*** END ADD *** 03/01/10 *********************************************
     C*
     C* CREATE NNJ NMBDP RECORDS
     C*
     C                   EXSR      LODNNJ
     C*
     C* CREATE SNJ NMBDP RECORDS
     C*
     C                   EXSR      LODSNJ
     C*
     C* CREATE BJZ NMBDP RECORDS
     C*
     C****** BEG DEL ****** 03/01/10 ***************************************
     C                   EXSR      LODBJZ
     C****** END DEL ****** 03/01/10 ***************************************
     C*
     C****** BEG ADD ****** 02/01/10 ***************************************
     C* CREATE DME NMBDP RECORDS
     C*
     C                   EXSR      LODDME
     C*
     C****** END ADD ****** 02/01/10 ***************************************
     C*** BEG ADD *** 02/09/10 *********************************************
     C*
     C* CREATE NJA NMBDP RECORDS
     C*
     C                   EXSR      LODASC
     C*
     C*** END ADD *** 02/09/10 *********************************************
     C*
     C*  END THE PROGRAM
     C*
     C                   SETON                                        LR
     C*
     C**********************************************************************
     C* Subroutine CLRMOD - Clear modifier 00 from the table
     C**********************************************************************
     C*
     C     CLRMOD        BEGSR
     C*
     C                   READ      PCO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     COPMOD        IFEQ      '00'
     C     COPMOD        OREQ      ' 0'
     C     COPMOD        OREQ      '0 '
     C*
     C                   MOVE      *BLANKS       COPMOD
     C*
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   UPDATE    PCO01
     C*
     C                   ENDIF
     C*
     C                   READ      PCO01                                  99
     C*
     C                   ENDDO
     C
     C                   ENDSR
     C*
     C**********************************************************************
     C* Subroutine LODNNJ - Create NNJ Records
     C**********************************************************************
     C*
     C     LODNNJ        BEGSR
     C*
     C*** BEG ADD *** 03/01/10 *********************************************
     C*
     C* Rollover subroutine
     C*
     C                   EXSR      ROLLNNJ
     C*** END ADD *** 03/01/10 *********************************************
     C*
     C                   READ      FXNNP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  NNTYPE        KKCODE
     C*
     C                   MOVE      NNEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C* LOAD OUTPUT FIELDS FOR NDCOP AND NMBDP
     C*
     C                   EXSR      WRITEB
     C*
     C                   READ      FXNNP                                  99
     C                   ENDDO
     C*
     C* CREATE NNJ NDCOP RECORDS
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PNN01
     C                   READ      FXNNP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  NNCPT4        KKCPT4
     C*
     C     NNMOD         IFEQ      '00'
     C     NNMOD         OREQ      ' 0'
     C     NNMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  NNMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      NNEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C*
     C*** BEG DEL *** 03/01/10 **********************************************
     C****
     C**** NNEXDT        IFLT      9999999
     C****               MOVE      NNEXDT        X1EXDT            6 0
     C****               Z-ADD     X1EXDT        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYM2
     C****               MOVEL     SRDTO         EXDT              7 0
     C****               ELSE
     C*** END DEL *** 03/01/10 **********************************************
     C                   Z-ADD     NNEXDT        EXDT
     C*** BEG DEL *** 03/01/10 **********************************************
     C****               ENDIF
     C*** END DEL *** 03/01/10 **********************************************
     C*
     C*** BEG CHG *** 03/01/10 **********************************************
     C****************** EXSR      WRITEC
     C*
     C                   Z-ADD     NNAMT         XXAMT
     C                   Z-ADD     NNFAMT        XXFAMT
     C*
     C                   EXSR      WRITED
     C*** END CHG *** 03/01/10 **********************************************
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      FXNNP                                  99
     C                   ENDDO
     C     BDKY          READE     PBD01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C* Subroutine LODSNJ - Create SNJ Records
     C**********************************************************************
     C*
     C     LODSNJ        BEGSR
     C*
     C*** BEG ADD *** 03/01/10 *********************************************
     C*
     C* Rollover subroutine
     C*
     C                   EXSR      ROLLSNJ
     C*** END ADD *** 03/01/10 *********************************************
     C*
     C                   READ      FXSNP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  SNTYPE        KKCODE
     C*
     C                   MOVE      SNEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   EXSR      WRITEB
     C*
     C                   READ      FXSNP                                  99
     C                   ENDDO
     C*
     C* CREATE SNJ NDCOP RECORDS
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PSN01
     C                   READ      FXSNP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  SNCPT4        KKCPT4
     C*
     C     SNMOD         IFEQ      '00'
     C     SNMOD         OREQ      ' 0'
     C     SNMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  SNMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      SNEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C*
     C*** BEG DEL *** 03/01/10 *********************************************
     C****
     C**** SNEXDT        IFNE      99999999
     C****               MOVE      SNEXDT        X1EXDT            6 0
     C****               Z-ADD     X1EXDT        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYM2
     C****               MOVEL     SRDTO         EXDT              7 0
     C****               ELSE
     C*** END DEL *** 03/01/10 *********************************************
     C                   Z-ADD     SNEXDT        EXDT
     C*** BEG DEL *** 03/01/10 *********************************************
     C****               ENDIF
     C*** END DEL *** 03/01/10 *********************************************
     C*
     C*** BEG CHG *** 03/01/10 **********************************************
     C****************** EXSR      WRITEC
     C*
     C                   Z-ADD     SNAMT         XXAMT
     C                   Z-ADD     SNFAMT        XXFAMT
     C*
     C                   EXSR      WRITED
     C*** END CHG *** 03/01/10 **********************************************
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      FXSNP                                  99
     C                   ENDDO
     C     BDKY          READE     PBD01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C**********************************************************************
     C* Subroutine LODBJZ - Create BJZ Records
     C**********************************************************************
     C*
     C     LODBJZ        BEGSR
     C*
     C****** BEG ADD ****** 03/01/10 ***************************************
     C                   MOVE      'Y'           XXBJZ             1
     C****** END ADD ****** 03/01/10 ***************************************
     C*
     C****** BEG ADD ****** 02/01/10 ***************************************
     C* Rollover subroutine
     C*
     C                   EXSR      ROLLBJZ
     C****** END ADD ****** 02/01/10 ***************************************
     C*
     C* CREATE NNJ NDCOP RECORDS IF THEY DONT EXIST
     C*
     C                   MOVEL     'NNJ'         KKCODE
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PBJ01
     C                   READ      FXBJP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  BJCPT4        KKCPT4
     C*
     C     BJMOD         IFEQ      '00'
     C     BJMOD         OREQ      ' 0'
     C     BJMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  BJMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      BJEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C*
     C*** BEG DEL *** 03/01/10 *********************************************
     C****
     C**** BJEXDT        IFNE      99999999
     C****               MOVE      BJEXDT        X1EXDT            6 0
     C****               Z-ADD     X1EXDT        SRDTI
     C****               CALL      'SRCVTDAT'    DTCYM2
     C****               MOVEL     SRDTO         EXDT              7 0
     C****               ELSE
     C*** END DEL *** 03/01/10 *********************************************
     C                   Z-ADD     BJEXDT        EXDT
     C*** BEG DEL *** 03/01/10 *********************************************
     C****               ENDIF
     C*** END DEL *** 03/01/10 *********************************************
     C*
     C****** BEG ADD ****** 02/01/10 ***************************************
     C                   Z-ADD     BJAMT         XXAMT
     C                   Z-ADD     BJFAMT        XXFAMT
     C****** END ADD ****** 02/01/10 ***************************************
     C*
     C                   EXSR      WRITED
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      FXBJP                                  99
     C                   ENDDO
     C*
     C     BDKY          READE     PBD01                                  99
     C*
     C                   ENDDO
     C*
     C* CREATE SNJ NDCOP RECORDS IF THEY DONT EXIST
     C*
     C                   MOVEL     'SNJ'         KKCODE
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PBJ01
     C                   READ      FXBJP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  BJCPT4        KKCPT4
     C*
     C     BJMOD         IFEQ      '00'
     C     BJMOD         OREQ      ' 0'
     C     BJMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  BJMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      BJEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C*
     C*** BEG DEL *** 03/01/10 *********************************************
     C***
     C***  BJEXDT        IFNE      99999999
     C***                MOVE      BJEXDT        X1EXDT            6 0
     C***                Z-ADD     X1EXDT        SRDTI
     C***                CALL      'SRCVTDAT'    DTCYM2
     C***                MOVEL     SRDTO         EXDT              7 0
     C***                ELSE
     C*** END DEL *** 03/01/10 *********************************************
     C                   Z-ADD     BJEXDT        EXDT
     C*** BEG DEL *** 03/01/10 *********************************************
     C***                ENDIF
     C*** END DEL *** 03/01/10 *********************************************
     C*
     C****** BEG ADD ****** 02/01/10 ***************************************
     C                   Z-ADD     BJAMT         XXAMT
     C                   Z-ADD     BJFAMT        XXFAMT
     C****** END ADD ****** 02/01/10 ***************************************
     C*
     C*
     C                   EXSR      WRITED
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      FXBJP                                  99
     C                   ENDDO
     C*
     C     BDKY          READE     PBD01                                  99
     C*
     C                   ENDDO
     C*
     C****** BEG ADD ****** 03/01/10 ***************************************
     C                   MOVE      *BLANK        XXBJZ             1
     C****** END ADD ****** 03/01/10 ***************************************
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 02/09/10 **************************************
     C***************************************************************
     C* Subroutine LODASC - Create NJA Records
     C***************************************************************
     C*
     C     LODASC        BEGSR
     C*
     C                   READ      FXASP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  ASTYPE        KKCODE
     C*
     C                   MOVE      ASEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C* LOAD OUTPUT FIELDS FOR NDCOP AND NMBDP
     C*
     C                   EXSR      WRITEB
     C*
     C                   READ      FXASP                                  99
     C                   ENDDO
     C*
     C* CREATE NJA NDCOP RECORDS
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PAS01
     C                   READ      FXASP                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  ASCPT4        KKCPT4
     C*
     C     ASMOD         IFEQ      '00'
     C     ASMOD         OREQ      ' 0'
     C     ASMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  ASMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      ASEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C*
     C     ASEXDT        IFNE      99999999
     C                   MOVE      ASEXDT        X1EXDT            6 0
     C                   Z-ADD     X1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EXDT              7 0
     C                   ELSE
     C                   Z-ADD     ASEXDT        EXDT
     C                   ENDIF
     C*
     C                   EXSR      WRITEC
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      FXASP                                  99
     C                   ENDDO
     C     BDKY          READE     PBD01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*** END ADD *** 02/09/10 **************************************
     C***************************************************************
     C* SUBROUTINE TO WRITE DISCOUNT MASTER FILE
     C***************************************************************
     C     WRITEB        BEGSR
     C*
     C     BDKY1         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PBD01
     C*
     C                   MOVEL(P)  KKCODE        BDCODE
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'NNJ'
     C     DATE          CAT(P)    COMM1:1       BDCOM1
     C     KKCODE        WHENEQ    'SNJ'
     C     DATE          CAT(P)    COMM2:1       BDCOM1
     C*** BEG ADD *** 02/09/10 ****************************************
     C     KKCODE        WHENEQ    'NJA'
     C     DATE          CAT(P)    COMM4:1       BDCOM1
     C*** END ADD *** 02/09/10 ****************************************
     C                   ENDSL
     C*
     C                   MOVEL     'D7'          BDTYPE
     C                   MOVEL(P)  'D'           BDRCDT
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1USER        BDIUSR
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO WRITE CARVE-OUT TRANSACTION FILE
     C***************************************************************
     C     WRITEC        BEGSR
     C*
     C     EFDT          IFLE      KKEFDT
     C     EXDT          ANDGT     KKEFDT
     C*
     C     COKY1         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PCO01
     C*
     C                   MOVEL(P)  KKCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  KKCPT4        COCPT4
     C                   MOVEL(P)  KKPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C*** BEG CHG *** 02/09/10 ****************************************
     C******* COCODE        IFEQ      'SNJ'
     C*******            Z-ADD     SNAMT         COFEE
     C*******            Z-ADD     SNFAMT        COFFEE
     C*******            ELSE
     C*******            Z-ADD     NNAMT         COFEE
     C*******            Z-ADD     NNFAMT        COFFEE
     C*******            ENDIF
     C                   SELECT
     C     COCODE        WHENEQ    'SNJ'
     C                   Z-ADD     SNAMT         COFEE
     C                   Z-ADD     SNFAMT        COFFEE
     C     COCODE        WHENEQ    'NNJ'
     C                   Z-ADD     NNAMT         COFEE
     C                   Z-ADD     NNFAMT        COFFEE
     C     COCODE        WHENEQ    'NJA'
     C                   Z-ADD     ASAMT         COFEE
     C                   Z-ADD     ASFAMT        COFFEE
     C                   ENDSL
     C*** END CHG *** 02/09/10 ****************************************
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1USER        COIUSR
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   MOVEL(P)  Q1PGM         COIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C                   Z-ADD     DSTIME        COITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   MOVEL     SRDTO         COIDT
     C*
     C                   WRITE     PCO01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C***************************************************************
     C* SUBROUTINE TO WRITE CARVE-OUT TRANSACTION FILE
     C***************************************************************
     C     WRITED        BEGSR
     C*
     C     EFDT          IFLE      KKEFDT
     C     EXDT          ANDGT     KKEFDT
     C*
     C     COKY1         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PCO01
     C*
     C                   MOVEL(P)  KKCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  KKCPT4        COCPT4
     C                   MOVEL(P)  KKPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C****** BEG CHG ****** 02/01/10 ***************************************
     C*************      Z-ADD     BJAMT         COFEE
     C*************      Z-ADD     BJFAMT        COFFEE
     C                   Z-ADD     XXAMT         COFEE
     C                   Z-ADD     XXFAMT        COFFEE
     C****** END CHG ****** 02/01/10 ***************************************
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1USER        COIUSR
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   MOVEL(P)  Q1PGM         COIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C                   Z-ADD     DSTIME        COITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   MOVEL     SRDTO         COIDT
     C*
     C                   WRITE     PCO01
     C****** BEG ADD ****** 02/01/10 ***************************************
     C                   ELSE
     C****** BEG ADD ****** 03/01/10 ***************************************
     C*
     C* We dont update any records from the BJZ file
     C*
     C     XXBJZ         IFNE      'Y'
     C****** END ADD ****** 03/01/10 ***************************************
     C*
     C                   Z-ADD     XXAMT         COFEE
     C                   Z-ADD     XXFAMT        COFFEE
     C*
     C********* MAINTENANCE FIELDS ************************************
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1USER        COIUSR
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   MOVEL(P)  Q1PGM         COIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C                   Z-ADD     DSTIME        COITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C                   MOVEL     SRDTO         COIDT
     C*
     C                   UPDATE    PCO01
     C*
     C****** END ADD ****** 02/01/10 ***************************************
     C*
     C****** BEG ADD ****** 03/01/10 ***************************************
     C                   ENDIF                                                  XXBJZ NE 'Y'
     C****** END ADD ****** 03/01/10 ***************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C****** BEG ADD ****** 02/01/10 ***************************************
     C**********************************************************************
     C* Subroutine LODDME - Create DME Records
     C**********************************************************************
     C*
     C     LODDME        BEGSR
     C*
     C* Rollover subroutine
     C*
     C                   EXSR      ROLLDME
     C*
     C* CREATE DME NDCOP RECORDS IF THEY DONT EXIST
     C*
     C                   MOVEL     'NNJ'         KKCODE
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PDM01
     C                   READ      PDM01                                  99     FXDMP
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C* MOVE NEW EFFECTIVE DATE
     C                   Z-ADD     DMEFDT        KKEFDT
     C*
     C                   MOVEL(P)  DMCPT4        KKCPT4
     C*
     C     DMMOD         IFEQ      '00'
     C     DMMOD         OREQ      ' 0'
     C     DMMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  DMMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      DMEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C                   Z-ADD     EFDT          KKEFDT
     C*
     C     DMEXDT        IFNE      99999999
     C                   MOVE      DMEXDT        X1EXDT            6 0
     C                   Z-ADD     X1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EXDT              7 0
     C                   ELSE
     C                   Z-ADD     DMEXDT        EXDT
     C                   ENDIF
     C*
     C                   Z-ADD     DMAMT         XXAMT
     C                   Z-ADD     DMFAMT        XXFAMT
     C*
     C                   EXSR      WRITED
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      PDM01                                  99     FXDMP
     C                   ENDDO
     C*
     C     BDKY          READE     PBD01                                  99
     C*
     C                   ENDDO
     C*
     C* CREATE SNJ NDCOP RECORDS IF THEY DONT EXIST
     C*
     C                   MOVEL     'SNJ'         KKCODE
     C*
     C     BDKY          CHAIN     PBD01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     1             SETLL     PDM01
     C                   READ      PDM01                                  99     FXDMP
     C     *IN99         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BDCODE        KKCODE
     C                   Z-ADD     BDEFDT        KKEFDT
     C                   MOVEL(P)  DMCPT4        KKCPT4
     C*
     C     DMMOD         IFEQ      '00'
     C     DMMOD         OREQ      ' 0'
     C     DMMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       KKPMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  DMMOD         KKPMOD
     C*
     C                   ENDIF
     C*
     C                   MOVE      DMEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EFDT              7 0
     C                   Z-ADD     EFDT          KKEFDT
     C*
     C     DMEXDT        IFNE      99999999
     C                   MOVE      DMEXDT        X1EXDT            6 0
     C                   Z-ADD     X1EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         EXDT              7 0
     C                   ELSE
     C                   Z-ADD     DMEXDT        EXDT
     C                   ENDIF
     C*
     C                   Z-ADD     DMAMT         XXAMT
     C                   Z-ADD     DMFAMT        XXFAMT
     C*
     C                   EXSR      WRITED
     C*
     C* GET NEXT FILE RECORD
     C*
     C                   READ      PDM01                                  99             FXDMP
     C                   ENDDO
     C*
     C     BDKY          READE     PBD01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR                                                           LODDME
     C*
     C**********************************************************************
     C* Subroutine ROLLBJZ- BJZ rollover
     C**********************************************************************
     C*
     C     ROLLBJZ       BEGSR
     C*
     C     1             SETLL     PBJ01
     C                   READ      PBJ01                                  99             FXBJP
     C     *IN99         IFEQ      *OFF
     C*
     C*** Get the previous Effective Date for NNJ
     C*
     C                   MOVEL     'NNJ'         KKCODE
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY          READPE    PBD01                                  98
     C                   IF        *IN98 = *OFF
     C*
     C                   MOVE      BJEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   IF        BDEFDT <> KKEFDT
     C*
     C                   Z-ADD     BDEFDT        KDEFDT
     C                   Z-ADD     BDEFDT        XPEFDT            7 0
     C                   MOVEL(P)  BJCPT4        K1CPT4            5
     C*
     C     BJMOD         IFEQ      '00'
     C     BJMOD         OREQ      ' 0'
     C     BJMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       K1PMOD            2
     C*
     C                   ELSE
     C                   MOVEL(P)  BJMOD         K1PMOD
     C*
     C                   ENDIF
     C*
     C     BDKY2         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  KKCODE        BDCODE
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'NNJ'
     C     DATE          CAT(P)    COMM1:1       BDCOM1
     C                   ENDSL
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1USER        BDIUSR
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF                                                  NDCOP for NNJ
     C*
     C**** Process records for the NDCOP
     C*
     C     COKY2         CHAIN     PCO02                              97
     C                   DOW       *IN97 = *OFF
     C*
     C                   MOVEL(P)  COCODE        KKCODE
     C                   MOVEL     COCPT4        KKCPT4
     C                   MOVEL     COPMOD        KKPMOD
     C                   MOVEL     *HIVAL        KDEFDT
     C*
     C     COKY3         SETGT     PCO01
     C     COKY4         READPE    PCO01                                  95
     C*
     C**********         IF        COEFDT <> KKEFDT
     C                   IF        COEFDT =  XPEFDT
     C*
     C** Chain to check existance of record
     C*
     C     COKY1         CHAIN     PCO01                              94
     C*
     C                   IF        *IN94 = *ON
     C*
     C                   MOVEL(P)  COCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  COCPT4        COCPT4
     C                   MOVEL(P)  COPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C                   Z-ADD     COFEE         COFEE
     C                   Z-ADD     COFFEE        COFFEE
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   WRITE     PCO01
     C*
     C                   ENDIF                                                   TO CHECK EXISTANCE
     C*
     C                   ENDIF                                                   COEFDT <> KKEFDT
     C*
     C     COKY2         READE     PCO02                                  97
     C                   ENDDO                                                   IF NDCOP RECORDS
     C*
     C                   ENDIF                                                   BDEFDT <> KKEFDT
     C                   ENDIF                                                   LAST Upload date
     C*
     C*** Get the previous Effective Date for SNJ *****************************
     C*
     C                   MOVEL     'SNJ'         KKCODE
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY          READPE    PBD01                                  98
     C                   IF        *IN98 = *OFF
     C*
     C                   MOVE      BJEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   IF        BDEFDT <> KKEFDT
     C*
     C                   Z-ADD     BDEFDT        KDEFDT
     C                   Z-ADD     BDEFDT        XPEFDT
     C                   MOVEL(P)  DMCPT4        K1CPT4
     C*
     C     DMMOD         IFEQ      '00'
     C     DMMOD         OREQ      ' 0'
     C     DMMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       K1PMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  DMMOD         K1PMOD
     C*
     C                   ENDIF
     C*
     C     BDKY2         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  KKCODE        BDCODE
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'SNJ'
     C     DATE          CAT(P)    COMM2:1       BDCOM1
     C                   ENDSL
     C*
     C**      * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF                                                  NDCOP for SNJ
     C*
     C**** Process records for the NDCOP
     C*
     C     COKY2         CHAIN     PCO02                              97
     C                   DOW       *IN97 = *OFF
     C*
     C                   MOVEL(P)  COCODE        KKCODE
     C                   MOVEL     COCPT4        KKCPT4
     C                   MOVEL     COPMOD        KKPMOD
     C                   MOVEL     *HIVAL        KDEFDT
     C*
     C     COKY3         SETGT     PCO01
     C     COKY4         READPE    PCO01                                  95
     C*
     C**********         IF        COEFDT <> KKEFDT
     C                   IF        COEFDT =  XPEFDT
     C*
     C** Chain to check existance of record
     C*
     C     COKY1         CHAIN     PCO01                              94
     C*
     C                   IF        *IN94 = *ON
     C*
     C                   MOVEL(P)  COCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  COCPT4        COCPT4
     C                   MOVEL(P)  COPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C                   Z-ADD     COFEE         COFEE
     C                   Z-ADD     COFFEE        COFFEE
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   WRITE     PCO01
     C                   ENDIF                                                   TO CHECK EXISTANCE
     C*
     C                   ENDIF                                                   COEFDT <> KKEFDT
     C     COKY2         READE     PCO02                                  97
     C                   ENDDO                                                   BDEFDT <> KKEFDT
     C*
     C                   ENDIF                                                   IF NDCOP RECORDS
     C                   ENDIF                                                   LAST Upload date
     C*
     C**                 READ      PBJ01                                  99     FXBJP
     C                   ENDIF
     C*
     C                   ENDSR                                                   ROLLBJZ
     C**********************************************************************
     C* Subroutine ROLLDME- DME rollover
     C**********************************************************************
     C*
     C     ROLLDME       BEGSR
     C*
     C     1             SETLL     PDM01
     C                   READ      PDM01                                  99             FXDMP
     C     *IN99         IFEQ      *OFF
     C*
     C*** Get the previous Effective Date for NNJ
     C*
     C                   MOVEL     'NNJ'         KKCODE
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY          READPE    PBD01                                  98
     C                   IF        *IN98 = *OFF
     C*
     C                   MOVE      DMEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   IF        BDEFDT <> KKEFDT
     C*
     C                   Z-ADD     BDEFDT        KDEFDT
     C                   Z-ADD     BDEFDT        XPEFDT
     C                   MOVEL(P)  DMCPT4        K1CPT4
     C*
     C     DMMOD         IFEQ      '00'
     C     DMMOD         OREQ      ' 0'
     C     DMMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       K1PMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  DMMOD         K1PMOD
     C*
     C                   ENDIF
     C*
     C     BDKY2         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'NNJ'
     C     DATE          CAT(P)    COMM1:1       BDCOM1
     C                   ENDSL
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF                                                   NMBDP NNJ
     C*
     C**** Process records for the NDCOP
     C*
     C     COKY2         CHAIN     PCO02                              97
     C                   DOW       *IN97 = *OFF
     C*
     C                   MOVEL(P)  COCODE        KKCODE
     C                   MOVEL     COCPT4        KKCPT4
     C                   MOVEL     COPMOD        KKPMOD
     C                   MOVEL     *HIVAL        KDEFDT
     C*
     C     COKY3         SETGT     PCO01
     C     COKY4         READPE    PCO01                                  95
     C*
     C**********         IF        COEFDT <> KKEFDT
     C                   IF        COEFDT =  XPEFDT
     C*
     C** Chain to check existance of record
     C*
     C     COKY1         CHAIN     PCO01                              94
     C*
     C                   IF        *IN94 = *ON
     C*
     C                   MOVEL(P)  COCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  COCPT4        COCPT4
     C                   MOVEL(P)  COPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C                   Z-ADD     COFEE         COFEE
     C                   Z-ADD     COFFEE        COFFEE
     C                   Z-ADD     KKEFDT        COEFDT
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   WRITE     PCO01
     C                   ENDIF                                                   TO CHECK EXISTANCE
     C*
     C                   ENDIF                                                   COEFDT <> KKEFDT
     C     COKY2         READE     PCO02                                  97
     C                   ENDDO                                                   NDCOP LOOP
     C*
     C                   ENDIF                                                   NDCOP NNJ RCD
     C                   ENDIF                                                   LAST Upload date
     C*
     C*** Get the previous Effective Date for SNJ *****************************
     C*
     C                   MOVEL     'SNJ'         KKCODE
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY          READPE    PBD01                                  98
     C                   IF        *IN98 = *OFF
     C*
     C                   MOVE      DMEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   IF        BDEFDT <> KKEFDT
     C*
     C                   Z-ADD     BDEFDT        KDEFDT
     C                   Z-ADD     BDEFDT        XPEFDT
     C                   MOVEL(P)  DMCPT4        K1CPT4
     C*
     C     DMMOD         IFEQ      '00'
     C     DMMOD         OREQ      ' 0'
     C     DMMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       K1PMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  DMMOD         K1PMOD
     C*
     C                   ENDIF
     C*
     C     BDKY2         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'SNJ'
     C     DATE          CAT(P)    COMM2:1       BDCOM1
     C                   ENDSL
     C*
     C**      * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF                                                  NDCOP for SNJ
     C*
     C**** Process records for the NDCOP
     C*
     C     COKY2         CHAIN     PCO02                              97
     C                   DOW       *IN97 = *OFF
     C*
     C                   MOVEL(P)  COCODE        KKCODE
     C                   MOVEL     COCPT4        KKCPT4
     C                   MOVEL     COPMOD        KKPMOD
     C                   MOVEL     *HIVAL        KDEFDT
     C*
     C     COKY3         SETGT     PCO01
     C     COKY4         READPE    PCO01                                  95
     C*
     C**********         IF        COEFDT <> KKEFDT
     C                   IF        COEFDT =  XPEFDT
     C*
     C** Chain to check existance of record
     C*
     C     COKY1         CHAIN     PCO01                              94
     C*
     C                   IF        *IN94 = *ON
     C*
     C                   MOVEL(P)  COCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  COCPT4        COCPT4
     C                   MOVEL(P)  COPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C                   Z-ADD     COFEE         COFEE
     C                   Z-ADD     COFFEE        COFFEE
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   WRITE     PCO01
     C                   ENDIF                                                   TO CHECK EXISTANCE
     C*
     C                   ENDIF                                                   COEFDT <> KKEFDT
     C     COKY2         READE     PCO02                                  97
     C                   ENDDO                                                   NDCOP LOOP
     C*
     C                   ENDIF                                                   IF NDCOP RECORDS
     C                   ENDIF                                                   LAST Upload date
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                        ROLLDME
     C*** BEG ADD *** 03/01/10 *********************************************
     C**********************************************************************
     C* Subroutine ROLLNNJ- NNJ rollover
     C**********************************************************************
     C*
     C     ROLLNNJ       BEGSR
     C*
     C     1             SETLL     PNN01
     C                   READ      PNN01                                  99             FXDMP
     C     *IN99         IFEQ      *OFF
     C*
     C*** Get the previous Effective Date for NNJ
     C*
     C                   MOVEL     'NNJ'         KKCODE
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY          READPE    PBD01                                  98
     C                   IF        *IN98 = *OFF
     C*
     C                   MOVE      NNEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   IF        BDEFDT <> KKEFDT
     C*
     C                   Z-ADD     BDEFDT        KDEFDT
     C                   Z-ADD     BDEFDT        XPEFDT
     C                   MOVEL(P)  NNCPT4        K1CPT4
     C*
     C     NNMOD         IFEQ      '00'
     C     NNMOD         OREQ      ' 0'
     C     NNMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       K1PMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  NNMOD         K1PMOD
     C*
     C                   ENDIF
     C*
     C     BDKY2         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'NNJ'
     C     DATE          CAT(P)    COMM1:1       BDCOM1
     C                   ENDSL
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF                                                   NMBDP NNJ
     C*
     C**** Process records for the NDCOP
     C*
     C     COKY2         CHAIN     PCO02                              97
     C                   DOW       *IN97 = *OFF
     C*
     C                   MOVEL(P)  COCODE        KKCODE
     C                   MOVEL     COCPT4        KKCPT4
     C                   MOVEL     COPMOD        KKPMOD
     C                   MOVEL     *HIVAL        KDEFDT
     C*
     C     COKY3         SETGT     PCO01
     C     COKY4         READPE    PCO01                                  95
     C*
     C**********         IF        COEFDT <> KKEFDT
     C                   IF        COEFDT =  XPEFDT
     C*
     C** Chain to check existance of record
     C*
     C     COKY1         CHAIN     PCO01                              94
     C*
     C                   IF        *IN94 = *ON
     C*
     C                   MOVEL(P)  COCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  COCPT4        COCPT4
     C                   MOVEL(P)  COPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C                   Z-ADD     COFEE         COFEE
     C                   Z-ADD     COFFEE        COFFEE
     C                   Z-ADD     KKEFDT        COEFDT
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   WRITE     PCO01
     C                   ENDIF                                                   TO CHECK EXISTANCE
     C*
     C                   ENDIF                                                   COEFDT <> KKEFDT
     C     COKY2         READE     PCO02                                  97
     C                   ENDDO                                                   NDCOP LOOP
     C*
     C                   ENDIF                                                   NDCOP NNJ RCD
     C                   ENDIF                                                   LAST Upload date
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                        ROLLDME
     C**********************************************************************
     C* Subroutine ROLLSNJ- SNJ rollover
     C**********************************************************************
     C*
     C     ROLLSNJ       BEGSR
     C*
     C     1             SETLL     PSN01
     C                   READ      PSN01                                  99             FXDMP
     C     *IN99         IFEQ      *OFF
     C*
     C*** Get the previous Effective Date for NNJ
     C*
     C                   MOVEL     'SNJ'         KKCODE
     C*
     C     BDKY          SETGT     PBD01
     C     BDKY          READPE    PBD01                                  98
     C                   IF        *IN98 = *OFF
     C*
     C                   MOVE      SNEFDT        X1EFDT            6 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   IF        BDEFDT <> KKEFDT
     C*
     C                   Z-ADD     BDEFDT        KDEFDT
     C                   Z-ADD     BDEFDT        XPEFDT
     C                   MOVEL(P)  SNCPT4        K1CPT4
     C*
     C     SNMOD         IFEQ      '00'
     C     SNMOD         OREQ      ' 0'
     C     SNMOD         OREQ      '0 '
     C                   MOVEL(P)  *BLANKS       K1PMOD
     C*
     C                   ELSE
     C                   MOVEL(P)  SNMOD         K1PMOD
     C*
     C                   ENDIF
     C*
     C     BDKY2         CHAIN     PBD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     KKEFDT        BDEFDT
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8
     C*
     C                   MOVE      XXMNTH        MONTH
     C                   MOVE      XXDAY         DAY
     C                   MOVE      XXYEAR        YEAR
     C*
     C                   SELECT
     C     KKCODE        WHENEQ    'SNJ'
     C     DATE          CAT(P)    COMM2:1       BDCOM1
     C                   ENDSL
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           BDSTAT
     C                   MOVEL(P)  Q1USER        BDUSER
     C                   MOVEL(P)  Q1PGM         BDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        BDTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   WRITE     PBD01
     C                   ENDIF                                                   NMBDP NNJ
     C*
     C**** Process records for the NDCOP
     C*
     C     COKY2         CHAIN     PCO02                              97
     C                   DOW       *IN97 = *OFF
     C*
     C                   MOVEL(P)  COCODE        KKCODE
     C                   MOVEL     COCPT4        KKCPT4
     C                   MOVEL     COPMOD        KKPMOD
     C                   MOVEL     *HIVAL        KDEFDT
     C*
     C     COKY3         SETGT     PCO01
     C     COKY4         READPE    PCO01                                  95
     C*
     C**********         IF        COEFDT <> KKEFDT
     C                   IF        COEFDT =  XPEFDT
     C*
     C** Chain to check existance of record
     C*
     C     COKY1         CHAIN     PCO01                              94
     C*
     C                   IF        *IN94 = *ON
     C*
     C                   MOVEL(P)  COCODE        COCODE
     C                   Z-ADD     KKEFDT        COEFDT
     C                   MOVEL(P)  COCPT4        COCPT4
     C                   MOVEL(P)  COPMOD        COPMOD
     C                   MOVEL(P)  'C'           COFL01
     C                   MOVEL(P)  'A'           COFL02
     C*
     C                   Z-ADD     COFEE         COFEE
     C                   Z-ADD     COFFEE        COFFEE
     C                   Z-ADD     KKEFDT        COEFDT
     C*
     C***     * MAINTENANCE FIELDS *
     C*
     C                   MOVEL(P)  'A'           COSTAT
     C                   MOVEL(P)  Q1USER        COUSER
     C                   MOVEL(P)  Q1PGM         COPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        COTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CODT
     C*
     C                   WRITE     PCO01
     C                   ENDIF                                                   TO CHECK EXISTANCE
     C*
     C                   ENDIF                                                   COEFDT <> KKEFDT
     C     COKY2         READE     PCO02                                  97
     C                   ENDDO                                                   NDCOP LOOP
     C*
     C                   ENDIF                                                   NDCOP NNJ RCD
     C                   ENDIF                                                   LAST Upload date
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                        ROLLDME
     C*** END ADD *** 03/01/10 *********************************************
