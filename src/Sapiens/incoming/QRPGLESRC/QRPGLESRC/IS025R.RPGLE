     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
      *** ACCEPT INPUT PARMS WITH SPOOLED FILE SPECIFICS,
      *** GET THE RECIPIENT FILE INFO TO ALLOW SEND OF FAX
      *      IF NO RECIPIENT INFO, CALL A CL (OR FROM HERE) THAT WILL
      *      PUT THE SPOOLED FILE ON HOLD WITH *ERROR STATUS, //OR//
      *      MOVE TO ANOTHER OUTQ? ,
      *
      *
      *** SHOULD I CREATE A DARTS RCD, IF SO HOW DO I CREATE
      *** THE DOC AND FOLDER.... THESE ARE SPLF'S .... I WOULD HAVE TO
      *** DO SOME ADDITIONAL PROCESSING.... HMMMMMMM.
      *
      *****************************************************************
      *
      *
     FIMFRL     IF   E           K DISK
     FSAFTP     O    E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** LDA ***
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D LDA            UDS
     D  DUMMY                  1     51
     D  CPCM                 121    160
      ****
     D SNDFX           DS
     D  FRRCID                 1     10
     D  FRDESC                11     50
     D  FROUTC                51     52
     D  FRFPH#                53     84
     D  FRCPTI                85    124
     D  FRCPT1               125    164
     D  FRCPT2               165    204
     D  FRCPT3               205    244
     D  FRCPF1               245    284
     D  FRCPF2               285    324
     D  FRCPF3               325    364
     D  FRCPCM               365    404
      *
      *

     C     *DTAARA       DEFINE    *LDA          LDA
     C     RCKY          KLIST
     C                   KFLD                    USRDTA
     C*
     C*   PARM LIST FOR "SENDFAX" 33 PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    FRFPH#
     C                   PARM                    FRCPT1
     C                   PARM                    FRCPT2
     C                   PARM                    FRCPT3
     C                   PARM                    SPLFNM
     C                   PARM                    QQJOB#            6
     C                   PARM                    QQUSRN           10
     C                   PARM                    QQJOBN           10               '''''20
     C                   PARM                    QQSPL#            3
     C                   PARM                    FRCPTI
     C                   PARM                    FRCPF1
     C                   PARM                    FRCPF2
     C                   PARM                    FRCPF3
     C                   PARM                    FRCPCM
     C                   PARM      *ALL'0'       QQSDTM            6
     C                   PARM      *ALL'0'       QQSDDT            6
     C                   PARM      ' '           QQRTCD            1
     C                   PARM      'DFTCVR  '    Q@CVPF            8
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
      *
     C*
     C     *ENTRY        PLIST
     C                   PARM                    USRDTA           10
     C                   PARM                    SPLFNM           10
     C                   PARM                    JOBNM            26
     C                   PARM                    SPL#              4 0
     C                   PARM                    RTCD              1
     C***
     C                   MOVE      SPL#          QQSPL#            3
     C     10            SUBST     JOBNM:1       QQJOBN           10
     C     10            SUBST     JOBNM:11      QQUSRN           10
     C     6             SUBST     JOBNM:21      QQJOB#            6
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C***
     C     RCKY          CHAIN     PFR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*** HIT, WE CAN DO SEND FAX ETC. FOR THIS ONE.
     C*
     C*** SEND FAX HERE ====
     C*
     C                   MOVEL     FRCPCM        CPCM
     C                   OUT       LDA
     C*
     C                   CALL      'ISNDFXC'     PLSEND
     C     QQRTCD        IFNE      *BLANKS
      * ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ***
     C                   MOVE      '1'           RTCD
     C                   GOTO      TERM
     C                   ENDIF
      *** WRITE TRANSACTION RECORD ====??? DOTHIS ??? =======
      *
     C                   EXSR      WTRN
      *
      *** WRITE DARTS RECORDS HERE *** IF REQUIRED *** ====
      *
     C*
     C                   ELSE
     C*
     C*** NO HIT, WE MUST DO SOMETHING WITH THIS FAX.....
     C*           MAYBE MOVE TO OTHER OUTQ, SPECIFY "INVRECP" AS FORM
     C*** TYPE???
     C*
     C                   ENDIF
     C*
     C     RETN          TAG
     C                   RETURN
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
      *===============================================================
     C*
     C     WTRN          BEGSR
     C*
     C                   CLEAR                   PFT01
     C*
     C                   MOVEL     FRRCID        FTRCID                         RECIP ID
     C                   MOVEL     FRDESC        FTDESC                         DESCRIPTION2
     C                   MOVEL     FROUTC        FTOUTC                         OUTPUT CLASS
     C                   MOVEL     FRFPH#        FTFPH#                         TO FAX PHONE
     C                   MOVEL     FRCPTI        FTCPTI
     C                   MOVEL     FRCPT1        FTCPT1                         TO--LINES 1-
     C                   MOVEL     FRCPT2        FTCPT2
     C                   MOVEL     FRCPT3        FTCPT3
     C                   MOVEL     FRCPF1        FTCPF1                         CHANGED TO C
     C                   MOVEL     FRCPF2        FTCPF2                          LINES INSTE
     C                   MOVEL     FRCPF3        FTCPF3                           FROM LINES
     C                   MOVEL     FRCPCM        FTCPCM                         CHGD TO FROM
     C                   MOVE      *ZEROS        FTFCDT
     C                   MOVE      *ZEROS        FTFCTM
     C                   MOVEL     *BLANKS       FTFAXC
     C*
     C*** JOB AND SPOOLED FILE INFO HERE ****
     C                   MOVE      QQJOB#        FTJOB#
     C                   MOVEL     QQUSRN        FTUSRN
     C                   MOVEL     QQJOBN        FTJOBN
     C                   MOVEL     SPLFNM        FTFILN
     C                   MOVE      QQSPL#        FTFIL#
     C                   MOVE      *ZEROS        FTRCNT
     C*
     C                   MOVEL     QQUSRN        FTSUSR
     C                   MOVEL     QQJOBN        FTSJOB
     C*
     C                   MOVE      'A'           FTSTAT
     C                   MOVE      Q1USER        FTUSER
     C                   MOVE      Q1PGM         FTPGM
     C                   Z-ADD     DSTIME        FTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FTDT
     C*
     C                   MOVEL     *BLANKS       FTKEY
     C                   MOVE      *ZEROS        FTSEQ#
     C*
     C                   WRITE     PFT01
     C*
     C                   ENDSR
