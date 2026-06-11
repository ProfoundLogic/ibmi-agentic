     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SCI107R  Get DOCUMENT NAME AND FOLDER INFO W/FLR BALANCING
     H*****************************************************************
     H*                                                               *
     H* * * GET THE DI AND FB RCDS, DETERMINE THE ING PROGRAM     *
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
      * Document INDEX CONTROL FILE
     FIMDIL     IF   E           K DISK
     F*
     F*
      * FOLDER NAMING CONTROL FILE
     FIMFBL     UF   E           K DISK
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*
     C*** DOCUMENT INDEX CTL FILE
     C*
     C     DIKY1         KLIST
     C                   KFLD                    QQWSID
     C*
     C*** FOLDER BALANCE FILE
     C*
     C     FBKY1         KLIST
     C                   KFLD                    XXSDT
     C                   KFLD                    XXSTM
     C*
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQWSID            2              SCAN STA ID
     C                   PARM                    QQSEQ#            6              DOC SEQ#
     C                   PARM                    QQSUFX            3              DOC SUFFIX
     C                   PARM                    QQBCHS            4              BATCH SEQ #
     C                   PARM                    QQDCID           12              BUILT DOC ID
     C                   PARM                    QQBCHI           12              BUILT BTCH ID
     C                   PARM                    QQCIF             8              CURR IMAGE FLR
     C                   PARM                    QQCISF           12              CURR IMAGE SFLR
     C                   PARM                    QQEXTN            4              FILE EXTN  SFLR
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
     C*
     C     DIKY1         CHAIN     PDI01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD *** 05/30/08 **************************************
     C     DISEQ#        IFEQ      -1
     C                   MOVE      *ZEROS        QQSEQ#            6              DOC SEQ#
     C                   MOVEL     *BLANKS       QQSUFX            3              DOC SUFFIX
     C                   MOVE      *ZEROS        QQBCHS            4              BATCH SEQ #
     C                   MOVEL     *BLANKS       QQDCID           12              BUILT DOC ID
     C                   MOVEL     *BLANKS       QQBCHI           12              BUILT BTCH ID
     C                   MOVEL     'S'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - ROLLOVER
     C                   ELSE
     C*** END   ADD *** 05/30/08 **************************************
     C                   MOVE      DISEQ#        QQSEQ#                           DOC ID SEQ#
     C                   MOVEL     DISUFX        QQSUFX            3              DOC ID SUFX
     C                   MOVE      DIBCHS        QQBCHS            4              BTCH SEQ #
     C     DIWSID        CAT       QQSEQ#        XX8BYT            8
     C     '.'           CAT       QQSUFX        XX4BYT            4
     C     XX8BYT        CAT       QQEXTN        QQDCID                           BUILT DOC ID
     C*
     C     DIWSID        CAT       ##DATE        XX8BYT
     C     XX8BYT        CAT       QQBCHS        QQBCHI                           BUILT BCH ID
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 05/30/08 **************************************
     C*
     C                   ELSE
     C                   MOVE      *ZEROS        QQSEQ#            6              DOC SEQ#
     C                   MOVEL     *BLANKS       QQSUFX            3              DOC SUFFIX
     C                   MOVE      *ZEROS        QQBCHS            4              BATCH SEQ #
     C                   MOVEL     *BLANKS       QQDCID           12              BUILT DOC ID
     C                   MOVEL     *BLANKS       QQBCHI           12              BUILT BTCH ID
     C*
     C                   MOVE      '1'           QQRTCD
     C                   GOTO      EXIT
      * * * ERROR - DOCUMENT IDX  RCD NOT FOUND FOR INPUT KEY
     C                   ENDIF
     C*
     C* * * FOLDER BALANCE ROUTINE
     C     FBKY1         SETLL     PFB01
     C                   READ      PFB01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     FBCIF         QQCIF                            CURR IMAGE FOLD
     C                   MOVEL     FBCISF        QQCISF                           CURR IMG SUB FL
     C                   Z-ADD     XXDT          FBLDT                            LAST MAINT DT.
     C                   Z-ADD     XXTM          FBLTIM                           LAST MAINT TM.
     C                   UPDATE    PFB01
     C*
     C                   ELSE
     C                   MOVEL     *BLANKS       QQCIF             8              CURR IMAGE FLR
     C                   MOVEL     *BLANKS       QQCISF           12              CURR IMAGE SFLR
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
