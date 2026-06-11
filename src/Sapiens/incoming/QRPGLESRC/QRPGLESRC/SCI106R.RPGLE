     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE SCI106R  UPDATE THE DOCUMENT AND FOLDER INFO FROM OBJECT IMPORT
     H*****************************************************************
     H*                                                               *
     H* * * UPDATE THE DI RECORD WITH THE NEXT AVAILABLE DOC ID IDX   *
     H*       AND THE NEXT BATCH ID IF NEEDED.                        *
     H*                                                               *
     H* * * UPDATE THE FB RECORD WITH THE CUR # OF DOCS IN SUBFLR     *
     H*       AND PROVIDE THE CREATE OF NEW SUBFLR IF MAX HAS BEEN    *
     H*       REACHED                                                 *
     H*
     H*                                                               *
     H*    CALLS TO:                                                  *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/30/08 C:62 PROJECT DR257                                   *
     H*               ADD ROLLOVER PROCESSING                         *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     F*
      * DoCument INDEX CONTROL FILE
     FIMDIL     UF   E           K DISK
     F*
     F*
      * FOLDER NAMING CONTROL FILE
     FIMFBL1    UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*** DOCUMENT INDEX CTL FILE
     C*
     C     DIKY1         KLIST
     C                   KFLD                    QQWSID
     C*
     C*** FOLDER BALANCE FILE
     C*
     C     FBKY1         KLIST
     C                   KFLD                    QQCIF
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2              SCAN STA ID
     C                   PARM                    QQDSQ#            6              DOC SEQ#
     C                   PARM                    QQBCHS            4              BATCH SEQ #
     C                   PARM                    QQCIF             8              CURR IMAGE FLR
     C                   PARM                    QQRTCD            1
     C*
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   MOVEL     *BLANKS       QQRTCD
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
      *================================================================
     C*
      *================================================================
      * * * ====================================================== * * *
      * * * ====================================================== * * *
     C*
     C     DIKY1         CHAIN     PDI01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      QQDSQ#        ##DSQ#            6 0
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     DISEQ#        IFEQ      999999
     C                   Z-SUB     1             DISEQ#
     C                   ELSE
     C     DISEQ#        IFEQ      -1
     C                   MOVEL     'S'           QQRTCD
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   ADD       1             DISEQ#
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C     QQBCHS        IFNE      *BLANKS
     C                   MOVE      QQBCHS        ##BCHS            4 0
     C     ##BCHS        ADD       1             DIBCHS
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        DILUSR
     C                   MOVEL     Q1PGM         DILPGM
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      DILDT
     C                   Z-ADD     DSTIME        DILTIM
     C*
     C                   UPDATE    PDI01
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     QQRTCD        IFEQ      'S'
     C                   GOTO      EXIT
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - DOCUMENT IDX  RCD NOT FOUND
     C                   ENDIF
     C*
     C* * * FOLDER BALANCE ROUTINE
     C*
     C     FBKY1         CHAIN     PFB01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C**
     C                   ADD       1             FBC#DS
     C*
     C     FBC#DS        IFGE      FBM#DS
     C                   EXSR      NEWSFL
     C*
     C     QQRTNC        IFNE      *BLANKS
     C                   MOVE      '3'           QQRTCD
     C                   GOTO      EXIT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   UPDATE    PFB01
     C*
     C                   ELSE
     C*
     C                   MOVE      '2'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - FOLDER BALANCE RCD NOT EXIST
     C                   ENDIF
     C*
     C*
      *
     C*
     C     TERM          TAG
     C                   RETURN
     C     EXIT          TAG
     C                   MOVE      '1'           *INLR
     C*
      *** =========================================================== ***
     C     NEWSFL        BEGSR
      *** =========================================================== ***
     C*
     C                   MOVE      FBCISF        XXSUFX            3 0
     C                   ADD       1             XXSUFX
     C                   MOVEL     FBCISF        XXPRE             9
     C                   MOVE      XXSUFX        XXSUF             3
     C     XXPRE         CAT       XXSUF         FBCISF
     C*
     C*** CALL CL PGM TO CREATE NEW SUBFOLDER
     C*
     C                   CALL      'ISADDF'
     C                   PARM                    FBCIF
     C                   PARM                    FBCISF
     C                   PARM                    QQRTNC            1
     C*
     C                   Z-ADD     0             FBC#DS
     C*
     C*
     C*
     C                   ENDSR
     C*
