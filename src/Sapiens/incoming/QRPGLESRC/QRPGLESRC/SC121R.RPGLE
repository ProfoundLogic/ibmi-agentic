     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC121R - FAX DOC SELECT                                  *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR02:                                                        *
     H*  21    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  22    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  23    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  24    INVALID CHARACTER USED TO INDICATE A SELECTED RECORD   *
     H*  25    INVALID RECIPIENT ID ENTERED                           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/06/01  C:35   PROJECT: SUPPORT FX081 #27399                *
     H*                  MOVED CHANGE MADE ON 11/18/99 SO THAT LDA    *
     H*                  WOULD ALWAYS BE CLEAR, REGARDLESS OF ERROR.  *
     H*                                                               *
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Remove writes to SMARP
     H*
     H* 11/18/99  C:113 PROJECT: SUPPORT #20740                       *
     H*                 Clear the local data area when through to     *
     H*                 avoid conflicts with the midterm endorsement  *
     H*                 process in WCMS.                              *
     H*****************************************************************
     FSAFTP     O    E             DISK
     F*** BEGIN DEL *** 04/25/00 **************************************
     F*****SMARL   IF  E           K        DISK                      A
     F********SDCMLA  IF  E           K        DISK
     F*** END DEL *** 04/25/00 ****************************************
     F*
     F*
     F*** BEGIN DEL *** 04/25/00 **************************************

     D********            FC      1   5 23
     F*** END DEL *** 04/25/00 ****************************************
     D*
     D SNDFX           DS
     D  SCRCID                 1     10
     D  SCDESC                11     50
     D  SCOUTC                51     52
     D  SCFPH#                53     84
     D  SCCPTI                85    124
     D  SCCPT1               125    164
     D  SCCPT2               165    204
     D  SCCPT3               205    244
     D  SCCPF1               245    284                                           T
     D  SCCPF2               285    324
     D  SCCPF3               325    364
     D  SCCPCM               365    404                                            1
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** LDA ***
     D LDA            UDS
     D  DUMMY                  1     51
     D  CPCM                 121    160
      ****
     D FC6             C                   CONST('                    -
     D                                        ')

     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   ARKY      KLIST
     C********             KFLD           XXKEY
     C********             KFLD           XXSEQ#
     C********             KFLD           XXASQ#
     C********
     C********   ARKY2     KLIST
     C********             KFLD           XXKEY
     C********             KFLD           XXSEQ#
     C********
     C********
     C********   CMKY      KLIST
     C********             KFLD           XXKEY
     C********             KFLD           XXSEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C*
     C*
     C*
     C     *DTAARA       DEFINE    *LDA          LDA
     C*
     C     *LIKE         DEFINE    Q2RECF        SVSCRN
     C*
     C*   PARM LIST FOR "SENDFAX" 33 PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    SCFPH#
     C                   PARM                    SCCPT1
     C                   PARM                    SCCPT2
     C                   PARM                    SCCPT3
     C                   PARM      'FAXPRT  '    QQFILN           10
     C                   PARM      Q#JOB#        QQJOB#            6
     C                   PARM      Q#USRN        QQUSRN           10
     C                   PARM      Q#JOBN        QQJOBN           20
     C                   PARM      '001'         QQFIL#            3
     C                   PARM                    SCCPTI
     C                   PARM                    SCCPF1
     C                   PARM                    SCCPF2
     C                   PARM                    SCCPF3
     C                   PARM                    SCCPCM
     C                   PARM                    QQSDTM            6
     C                   PARM                    QQSDDT            6
     C                   PARM      ' '           QQRTCD            1
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SNDFX
     C                   PARM                    QQSDDT            6
     C                   PARM                    QQSDTM            6
     C                   PARM                    QQSUSR           10
     C                   PARM                    QQSJOB           10
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ             5
     C                   PARM                    QQRTN             1
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
     C                   MOVE      QQSEQ         XXSEQ#            5 0
     C                   MOVE      QQKEY         XXKEY            40
     C*
     C                   TIME                    DSDTTM
     C*
     C**** MOVE THE FROM VALUE INTO THE FIELD HERE ****
     C                   MOVEL     CPCM          SCCPCM
     C*
     C*
      ***===***=== PRINT FAX TO TEMP OUTQ   ========================
     C*
     C                   CALL      'STPRTF'
     C                   PARM                    QQDCID
     C                   PARM                    QQFOLD
     C                   PARM      *ZEROS        Q#JOB#            6
     C                   PARM      *BLANKS       Q#USRN           10
     C                   PARM      *BLANKS       Q#JOBN           10
     C                   PARM      *BLANKS       Q#RTCD            1
     C*
     C     Q#RTCD        IFNE      *BLANKS
      * ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR
     C                   MOVE      '1'           QQRTN
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*** SEND FAX HERE ====
     C*
     C                   OUT       LDA
     C*
     C                   CALL      'SNDFXC'      PLSEND
     C     QQRTCD        IFNE      *BLANKS
      * ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ERROR ******
     C                   MOVE      '1'           QQRTN
     C                   GOTO      TERM
     C                   ENDIF
      *
      *** WRITE TRANSACTION RECORD ==========================
      **** BEGIN DEL **** 07/06/01 *********************************************
      *** BEGIN ADD *** 11/18/99 ***********************************************
      ** Clear the LDA to avoid data decimal errors in the future.
     C******************** MOVE *BLANKS   LDA
     C******************** OUT  LDA
      *** END   ADD *** 11/18/99 ***********************************************
      **** END   DEL **** 07/06/01 *********************************************
      *
     C                   EXSR      WTRN
      *
      *** WRITE DARTS RECORDS HERE *** ====================
      *
      *** REMOVED WRITING TO DARTS PER TLB 9/25/95 *************
     C*** BEGIN DEL *** 04/25/00 **************************************
     C**N60*60             EXSR WDRT
     C*** END DEL *** 04/25/00 ****************************************
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
      **** BEGIN ADD **** 07/06/01 *********************************************
      *
      *  Clear the LDA to avoid data decimal errors in the future.
      *
     C                   MOVE      *BLANKS       LDA
     C                   OUT       LDA
      **** END   ADD **** 07/06/01 *********************************************
     C                   MOVE      '1'           *INLR
     C*
      *===============================================================
     C*
     C     WTRN          BEGSR
     C*
     C                   CLEAR                   PFT01
     C*
     C                   MOVEL     SCRCID        FTRCID                         RECIP ID
     C                   MOVEL     SCDESC        FTDESC                         DESCRIPTION2
     C                   MOVEL     SCOUTC        FTOUTC                         OUTPUT CLASS
     C                   MOVEL     SCFPH#        FTFPH#                         TO FAX PHONE#
     C                   MOVEL     SCCPTI        FTCPTI
     C                   MOVEL     SCCPT1        FTCPT1                         TO--LINES 1-3
     C                   MOVEL     SCCPT2        FTCPT2
     C                   MOVEL     SCCPT3        FTCPT3
     C                   MOVEL     SCCPF1        FTCPF1                         CHANGED TO COMMENT
     C                   MOVEL     SCCPF2        FTCPF2                          LINES INSTEAD OF
     C                   MOVEL     SCCPF3        FTCPF3                           FROM LINES
     C                   MOVEL     SCCPCM        FTCPCM                         CHGD TO FROM LINE 1
     C                   MOVE      *ZEROS        FTFCDT
     C                   MOVE      *ZEROS        FTFCTM
     C                   MOVEL     *BLANKS       FTFAXC
     C*
     C*** JOB AND SPOOLED FILE INFO HERE ****
     C                   MOVE      Q#JOB#        FTJOB#
     C                   MOVEL     Q#USRN        FTUSRN
     C                   MOVEL     Q#JOBN        FTJOBN
     C                   MOVEL     'FAXPRT  '    FTFILN
     C                   MOVE      '001'         FTFIL#
     C                   MOVE      *ZEROS        FTRCNT
     C*
     C                   MOVEL     QQSUSR        FTSUSR
     C                   MOVEL     QQSJOB        FTSJOB
     C*
     C                   MOVE      'A'           FTSTAT
     C                   MOVE      Q1USER        FTUSER
     C                   MOVE      Q1PGM         FTPGM
     C                   Z-ADD     DSTIME        FTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FTDT
     C*
     C                   MOVEL     QQKEY         FTKEY
     C                   MOVE      QQSEQ         FTSEQ#
     C*
     C                   WRITE     PFT01
     C*
     C                   ENDSR
     C*** BEGIN DEL *** 04/25/00 **************************************
      *===================WDRT========================================*
     C********   WDRT      BEGSR
     C********
     C********   CMKY      CHAINPCM01                60
     C********   *IN60     CABEQ*ON       ENDDRT
     C********
     C********             Z-ADD*HIVAL    XXASQ#
     C********   ARKY      SETGTPAR01
     C********   ARKY2     REDPEPAR01                    60
     C********   *IN60     IFEQ *OFF
     C********   1         ADD  ARASQ#    XXASQ#  50
     C********             ELSE
     C********             Z-ADD1         XXASQ#
     C********             ENDIF
     C********
     C********             MOVELXXKEY     ARKEY
     C********             MOVEL*BLANKS   ARCM75
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********             MOVE DSDATE    X#DT6   6
     C********   X#DT6     CAT  '  Fax#: 'XXWK15 15
     C********   FC,1      CAT  XXWK15    XXWK38 38
     C********   XXWK38    CAT  SCFPH#    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC,2      CAT  SCCPTI    ARCM75
     C********
     C********             MOVELXXKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC,3      CAT  SCCPT1    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  SCCPT2    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  SCCPT3    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC,4      CAT  SCCPF1    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  SCCPF2    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  SCCPF3    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC,5      CAT  SCCPCM    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********             ADD  1         XXASQ#
     C********
     C********             MOVELXXKEY     ARKEY
     C********             MOVEL*BLANKS   ARCM75
     C********             MOVE XXSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM           12/31/91 CHG
     C********             TIME           DSDTTM
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARTRDT
     C********
     C********             MOVE DSTIME    ARTRTM
     C********             MOVELCMSECL    ARSECL
     C********             MOVELCMSBYL    ARSBYL
     C********             MOVELCMSTAT    ARSTAT
     C********             MOVELQ1USER    ARUSER
     C********             MOVELQ1PGM     ARPGM
     C********             MOVE DSTIME    ARTIME
     C********
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     ARDT
     C********
     C********             MOVE ARDT      ARIDT
     C********             MOVELQ1USER    ARIUSR
     C********
     C********             WRITEPAR01
     C********
     C********   ENDDRT    ENDSR
**********   RMRK ENTRIES
********Fax sent:   Date    :
********            Heading :
********            To      :
********            Comments:
********            From    :
     C*** END DEL *** 04/25/00 ****************************************
