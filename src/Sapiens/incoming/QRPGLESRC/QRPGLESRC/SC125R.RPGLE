     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SC125R - UPDATE SAIFP INCOMING FAX FILE
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     FSAIFP     O    E             DISK
     F*
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D                 DS
     D  Q#LFM                  1     30
     D  Q#LIBN                 1     10
     D  Q#DBFN                11     20
     D  Q#MBRN                21     30
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FC6             C                   CONST('                    -
     D                                        ')

     C*
     C*
     C*
     C*  TRANSACTION FILE
     C*
     C*
     C*
     C*** PARAMERTER LISTS FOR EXTERNAL CALLS
     C*
     C*   PARM LIST FOR "RSNDFXC" CL PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    QQPHN#           32
     C*
     C     PLSFXE        PLIST
     C                   PARM                    QQPHN#
     C                   PARM                    QQRTCD            1
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQLIBN           10            SPFL JOB NAME
     C                   PARM                    QQDBFN           10
     C                   PARM                    QQMBRN           10            SPLF JOB NUMBER
     C                   PARM                    QQ#PGS            5            SPLF NAME
     C                   PARM                    QQTSI#           20            SPLF NUMBER
     C                   PARM                    QQSTS             1
     C                   PARM                    QQFXCN            8
     C                   PARM                    QQFXPT            4
     C                   PARM                    QQFDTE            6
     C                   PARM                    QQFTIM            6
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           63
     C                   PARM                    QQRTCD            1
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
     C                   MOVEL     QQLIBN        Q#LIBN
     C                   MOVEL     QQDBFN        Q#DBFN
     C                   MOVEL     QQMBRN        Q#MBRN
     C*
     C                   MOVE      QQ#PGS        ###PGS            5 0
     C                   MOVE      QQFDTE        ##FDTE            6 0
     C                   MOVE      QQFTIM        ##FTIM            6 0
     C                   MOVEL     *BLANKS       QQDCID
     C                   MOVEL     *BLANKS       QQFOLD
     C                   MOVEL     *BLANKS       QQRTCD
     C*
     C                   TIME                    DSDTTM
     C*
      *================================================================
     C*
     C*** GET NT DCID/FOLD
     C                   CALL      'FXNXTID'
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C                   PARM                    QQRTCD
     C*
     C*** PUT CODE HERE TO RECOVER FROM ERROR ???????????????????????????
     C*
     C****
     C*
     C*** CALL PGM TO FILE FAX **
     C*
     C                   CALL      'FILFAXC'
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C                   PARM                    QQLIBN
     C                   PARM                    QQDBFN
     C                   PARM                    QQMBRN
     C*
     C*** PUT CODE HERE TO RECOER FROM ERROR ???????????????????????????
     C*
     C*** WRITE DATA BASE RECORD SAIFP ***
     C*
     C                   MOVEL     QQLIBN        IFLIBN
     C                   MOVEL     QQDBFN        IFDBFN
     C                   MOVEL     QQMBRN        IFMBRN
     C*
     C                   MOVEL     Q#LFM         IFLFM
     C*
     C                   MOVEL     QQ#PGS        IF#PGS
     C                   MOVEL     QQTSI#        IFTSI
     C*
     C                   MOVEL     QQSTS         IFCSTS
     C                   MOVEL     QQFXCN        IFCNAM
     C                   MOVEL     QQFXPT        IFPORT
     C                   MOVEL     QQFDTE        IFRDAT
     C                   MOVEL     QQFTIM        IFRTIM
     C     QQSTS         IFEQ      'S'
     C                   MOVEL     'COMPLETE'    IFSTS
     C                   ELSE
     C                   MOVEL     'PARTIAL '    IFSTS
     C                   ENDIF
     C                   MOVEL     QQDCID        IFDCID
     C                   MOVEL     QQFOLD        IFFOLD
     C                   MOVEL     'A'           IFSTAT
     C                   MOVEL     Q1USER        IFUSER
     C                   MOVEL     Q1PGM         IFPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         IFDT
     C*
     C                   Z-ADD     DSTIME        IFTIME
     C                   MOVEL     Q1USER        IFIUSR
     C*
     C                   WRITE     PIF01
      *
     C*
      *================================================================
     C* END OF THE PGM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
