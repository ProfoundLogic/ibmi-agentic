     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS403R
     H*****************************************************************
     H*                                                               *
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
     H***                                                             *
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
     F*
      * FOLDER NAMING CONTROL FILE
     FIMFBL     UF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D*** FROM PDS DARTS FAX
     D FXDCID         UDS
     D  #XDCID                 1      7
      *

     C*
     C*** FOLDER BALANCE FILE
     C*
     C     FBKY1         KLIST
     C                   KFLD                    XXSDT
     C                   KFLD                    XXSTM
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C*
     C                   PARM                    QQDCID           12              DOC ID
     C                   PARM                    QQCIF             8              FOLDER
     C                   PARM                    QQCISF           12              SUB FOLDER
     C                   PARM                    QQRTCD            1
     C*
     C                   UNLOCK    FXDCID
     C*
     C                   MOVEL     *BLANKS       QQDCID
     C                   MOVEL     *BLANKS       QQCIF
     C                   MOVEL     *BLANKS       QQCISF
     C                   MOVEL     *BLANKS       QQRTCD
     C                   TIME                    DSDTTM
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C                   Z-ADD     *ZEROS        XXSDT             6 0
     C                   Z-ADD     *ZEROS        XXSTM             6 0
     C*
     C*
     C     *DTAARA       DEFINE                  FXDCID
     C*
     C     *LOCK         IN        FXDCID
     C*
     C                   MOVE      #XDCID        ##DOC             7 0
     C                   ADD       1             ##DOC
     C                   MOVE      ##DOC         #XDCID
     C                   OUT       FXDCID
     C     'F'           CAT       #XDCID        ##8A              8
     C     ##8A          CAT       '.FAX'        QQDCID
     C*
     C*
     C     DSDATE        MULT      10000.01      XXDT              6 0          YY/MM/DD
     C                   MOVE      DSTIME        XXTM              6 0
     C*
     C                   MOVE      XXDT          ##DATE            6
     C*
     C*
      * * * ====================================================== * * *
     C*
     C* * * FOLDER BALANCE ROUTINE
     C*
     C* * * FOLDER BALANCE ROUTINE
     C     FBKY1         SETLL     PFB01
     C                   READ      PFB01                                  60
     C*
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
     C                   MOVEL     FBCIF         QQCIF
     C                   MOVEL     FBCISF        QQCISF
     C                   Z-ADD     XXDT          FBLDT                            LAST MAINT DT.
     C                   Z-ADD     XXTM          FBLTIM                           LAST MAINT TM.
     C*
     C                   UPDATE    PFB01
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       QQCIF             8              CURR IMAGE FLR
     C                   MOVEL     *BLANKS       QQCISF           12              CURR IMAGE SFLR
     C*
     C                   MOVE      '2'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - FOLDER BALANCE RCD NOT EXIST
     C                   ENDIF
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
     C                   PARM      *BLANKS       QQRTNC            1
     C*
     C                   Z-ADD     0             FBC#DS
     C*
     C*
     C*
     C                   ENDSR
     C*
