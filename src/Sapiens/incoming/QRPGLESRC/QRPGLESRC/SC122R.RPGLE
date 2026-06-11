     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC122R - UPDATE TRANSACTION RECORDS FRM DTAQ
     H*****************************************************************
     H*                                                               *
     H*    CALLS TO:                                                 **
     H*                                                              **
     H*****************************************************************
     H** REVISIONS:
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SMARP with API DA701R
     H*
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  GENERAL CHAIN INDICATOR                                   *
     H*****************************************************************
     H** FILES:                                                      **
     H*****************************************************************
     FSAFTL     UF   E           K DISK
     F*** BEGIN DEL *** 04/25/00 **************************************
     F*****SMARL   IF  E           K        DISK                      A
     F*** END DEL *** 04/25/00 ****************************************
     FSDCMLA    IF   E           K DISK
     FSAFEP     O    E             DISK
     F*
     F*

***  D #ER             S             70    DIM(8) CTDATA PERRCD(1)
     D FC              S             23    DIM(5) CTDATA PERRCD(1)
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FC6             C                   CONST('                    -
     D                                        ')

     C*
     C*
     C*
     C*  TRANSACTION FILE
     C*
     C     FTKY          KLIST
     C                   KFLD                    QQJOBN
     C                   KFLD                    QQUSRN
     C                   KFLD                    QQJOB#
     C                   KFLD                    QQFILN
     C                   KFLD                    QQFIL#
     C*** DARTS FILE
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C********   ARKY      KLIST
     C********             KFLD           FTKEY
     C********             KFLD           FTSEQ#
     C********             KFLD           XXASQ#
     C********
     C********   ARKY2     KLIST
     C********             KFLD           FTKEY
     C********             KFLD           FTSEQ#
     C*** END DEL *** 04/25/00 ****************************************
     C*
     C*
     C     CMKY          KLIST
     C                   KFLD                    FTKEY
     C                   KFLD                    FTSEQ#
     C*
     C*
     C*** PARAMERTER LISTS FOR EXTERNAL CALLS
     C*
     C*   PARM LIST FOR "RSNDFXC" CL PGM
     C*
     C     PLSEND        PLIST
     C                   PARM                    QQPHN#           32
     C                   PARM                    FTCPT1           40
     C                   PARM                    FTCPT2           40
     C                   PARM                    FTCPT3           40
     C                   PARM                    QQFILN           10
     C                   PARM                    QQJOB#            6
     C                   PARM                    QQUSRN           10
     C                   PARM                    QQJOBN           10
     C                   PARM                    QQFIL#            3
     C                   PARM                    FTCPTI           40
     C                   PARM                    FTCPF1           40
     C                   PARM                    FTCPF2           40
     C                   PARM                    FTCPF3           40
     C                   PARM                    FTCPCM           40
     C                   PARM                    QQSDTM            6
     C                   PARM                    QQSDDT            6
     C                   PARM                    QQRTCD            1
     C*
     C     PLSFXE        PLIST
     C                   PARM                    QQPHN#
     C                   PARM                    FTCPT1           40
     C                   PARM                    QQFILN           10
     C                   PARM                    QQJOB#            6
     C                   PARM                    QQUSRN           10
     C                   PARM                    QQJOBN           10
     C                   PARM                    QQFIL#            3
     C                   PARM                    FTKEY            40
     C                   PARM                    QQSEV             3 0
     C                   PARM                    QQDESC
     C                   PARM                    QQSTAT
     C                   PARM                    FTSUSR
     C                   PARM                    QQMSGI
     C                   PARM      *ZEROS        QQMSV             2
     C                   PARM      *BLANKS       QQMDSC          140
     C                   PARM                    QQRTCD            1
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQJOBN           10            SPFL JOB NAME
     C                   PARM                    QQUSRN           10
     C                   PARM                    QQJOB#            6            SPLF JOB NUMBER
     C                   PARM                    QQFILN           10            SPLF NAME
     C                   PARM                    Q1FIL#            4            SPLF NUMBER
     C                   PARM                    QQSTS             1
     C                   PARM                    QQMSGI            7
     C                   PARM                    QQPHN#           32
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
     C                   MOVE      Q1FIL#        QQFIL#            3
     C                   TIME                    DSDTTM
     C*
     C*** GET FAX TRANS RECORD
     C*
     C     FTKY          CHAIN     PFT01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     FTKEY         QQKEY            40
     C*** IF NOT A SUCCESSFUL FAX
     C     QQSTS         IFNE      'S'
     C*** DO ERROR RECOVERY ROUTINE
     C*** CAN CHECK HERE FOR SPECIFIC MESSAGES THAT ARE RECOVERABLE***
     C     FTRCNT        IFGE      3
     C*** NO MORE RE-SCHEDULES,  LIST AS AN ERROR
     C*
     C                   MOVEL     #ER(1)        QQDESC          140
     C                   MOVE      #ER(2)        QQDESC
     C                   MOVE      099           QQSEV             3 0
     C                   MOVEL     QQMSGI        QQMSID            7
     C*
     C*** CALL PGM TO SEND MESSAGE TO WORK STATION
     C*
     C                   MOVE      'E'           QQSTAT            1
     C*
     C*
     C                   CALL      'SFXERR'      PLSFXE
     C*
     C     QQMSGI        CASNE     *BLANKS       WRTFE
     C                   ENDCS
     C*
     C*** SET UP FOR STATUS CHANGE
     C                   GOTO      STSUPD
     C*
     C                   ELSE                                                   NOT 3 RETIRES
     C*
     C*** CALL SND FAX PGM TO PROCESS RESEND
     C*
     C     DSMIN         IFGE      45
     C     DSMIN         SUB       45            NEWMIN            2 0
     C     DSTIME        ADD       010000        XXTIM             6 0
     C                   MOVE      *ZEROS        XXT4              4 0
     C                   MOVEL     NEWMIN        XXT4
     C                   MOVE      XXT4          XXTIM
     C     XXTIM         IFGT      235959
     C                   MOVE      *ZEROS        QQSDTM
     C                   MOVE      *ZEROS        QQSDDT
     C                   ELSE
     C                   MOVE      XXTIM         QQSDTM
     C                   MOVE      DSDATE        QQSDDT
     C                   END
     C                   ELSE
     C     DSTIME        ADD       001500        XXTIM
     C                   MOVE      XXTIM         QQSDTM
     C                   MOVE      DSDATE        QQSDDT
     C                   END
     C*
     C                   MOVE      ' '           QQRTCD                         NO COVER ON RSND
     C                   CALL      'RSNDFXC'     PLSEND
     C*
     C*** IF SND IS IN ERROR, LIST THIS AS AN ERROR
     C     QQRTCD        IFEQ      'E'
     C*** SET UP FOR STATUS CHANGE
     C                   MOVE      'E'           QQSTAT            1
     C*** NO MORE RE-SCHEDULES,  LIST AS AN ERROR
     C*
     C*** CALL PGM TO SEND MESSAGE TO MSGQ ABOUT ERROR
     C*
     C                   MOVEL     #ER(3)        QQDESC
     C                   MOVE      #ER(4)        QQDESC
     C                   MOVE      099           QQSEV
     C                   MOVEL     QQMSGI        QQMSID
     C*
     C*** CALL PGM TO WRITE TRANSACTION LOG
     C*
     C                   CALL      'SFXERR'      PLSFXE
     C*
     C     QQMSGI        CASNE     *BLANKS       WRTFE
     C                   ENDCS
     C*
     C                   GOTO      STSUPD
     C                   END
     C*
     C                   ADD       1             FTRCNT
     C                   MOVE      'R'           QQSTAT
     C*
     C                   GOTO      STSUPD
     C*
     C                   END
     C*
     C                   ELSE                                                   SUCESSFUL FAX
     C*
     C*** CALL PGM TO SEND MESSAGE
     C*
     C                   MOVEL     #ER(7)        QQDESC
     C                   MOVE      #ER(8)        QQDESC
     C                   MOVE      050           QQSEV
     C                   MOVEL     QQMSGI        QQMSID
     C*
     C*
     C                   MOVE      'C'           QQSTAT
     C                   CALL      'SFXERR'      PLSFXE
     C*
     C                   GOTO      STSUPD
     C*
     C*
     C                   END
     C*
     C     STSUPD        TAG
      *
      *** UPDATE THE FT RECORD HERE WITH STATUS
      *
     C                   MOVE      QQSTAT        FTFAXC
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FTFCDT
     C*
     C                   MOVE      DSTIME        FTFCTM
      *
     C                   UPDATE    PFT01
     C*
     C*** CREATE DARTS RECORD FOR COMPLETED NORMAL FAX
     C*
     C     QQSTAT        IFEQ      'C'
     C***        QQSTAT    OREQ 'E' *** ONLY COMPLETED FOR DARTS
     C*** CREATE DARTS
     C                   EXSR      WDRT
     C*
     C                   ENDIF
     C*
     C                   END
     C*
     C* END OF THE PGM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
      *================== WRTFE ======================================*
      *
     C     WRTFE         BEGSR
     C*
     C     QQMDSC        IFNE      *BLANKS
     C                   MOVEL     QQMDSC        FEDESC
     C                   ELSE
     C                   MOVEL     #ER(1)        FEDESC
     C                   ENDIF
     C*
     C                   MOVEL     QQMSGI        FEMSGI
     C*
     C                   MOVE      FTJOB#        FEJOB#
     C                   MOVEL     FTUSRN        FEUSRN
     C                   MOVEL     FTJOBN        FEJOBN
     C                   MOVEL     FTFILN        FEFILN
     C                   MOVE      FTFIL#        FEFIL#
     C*
     C                   MOVE      FTFCDT        FEFCDT
     C                   MOVE      FTFCTM        FEFCTM
     C*
     C*
     C                   MOVEL     'A'           FESTAT
     C                   MOVEL     Q1USER        FEUSER
     C                   MOVEL     Q1PGM         FEPGM
     C                   MOVE      DSTIME        FETIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         FEDT
     C*
     C*
     C                   WRITE     PFE01
     C*
     C     ENDFE         ENDSR
     C*
      *===================WDRT========================================*
     C*
     C     WDRT          BEGSR
     C*
     C*** BEGIN DEL *** 04/25/00 **************************************
     C****BYPASS THIS *** DO SEND CODE ONLY ON COMPLETTION!!!!
     C****PER*TLB 9/25/95 *****
     C********             GOTO NXT1
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
     C********             MOVELFTKEY     ARKEY
     C********             MOVEL*BLANKS   ARCM75
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********
     C********             MOVE DSDATE    X#DT6   6
     C********   X#DT6     CAT  '  Fax#: 'XXWK15 15
     C********   QQSTAT    IFEQ 'C'
     C********   FC,1      CAT  XXWK15    XXWK38 38
     C********             ELSE
     C********   FC,4      CAT  XXWK15    XXWK38 38
     C********             ENDIF
     C********   XXWK38    CAT  FTFPH#    ARCM75
     C********
     C********             MOVELFTKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********   FC,3      CAT  FTCPT1    ARCM75
     C********
     C********             MOVELFTKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
     C********             MOVE XXASQ#    ARASQ#
     C********             MOVELCMCSID    ARCSID
     C********             MOVE CMCDAT    ARCDAT
     C********             MOVELCMRMCD    ARRMCD
     C********             MOVELQ1PGM     ARPNAM
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
     C********   FC6       CAT  QQDESC    ARCM75
     C********
     C********             MOVELFTKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C****BYPASS TAG
     C********   NXT1      TAG
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
     C********             MOVELFTKEY     ARKEY
     C********             MOVEL*BLANKS   ARCM75
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********   XXWK38    CAT  FTFPH#    ARCM75
     C********
     C********             MOVELFTKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********   FC,2      CAT  FTCPTI    ARCM75
     C********
     C********             MOVELFTKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********   FC,3      CAT  FTCPT1    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********   FTCPT2    IFNE *BLANKS
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  FTCPT2    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********             ENDIF                             IF BLANKS
     C********
     C********
     C********   FTCPT3    IFNE *BLANKS
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  FTCPT3    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********             ENDIF                             IF BLANKS
     C********
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC,4      CAT  FTCPF1    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********   FTCPF2    IFNE *BLANKS
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  FTCPF2    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********             ENDIF
     C********
     C********
     C********   FTCPF3    IFNE *BLANKS
     C********             MOVEL*BLANKS   ARCM75
     C********   FC6       CAT  FTCPF3    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********             ENDIF
     C********
     C********
     C********             MOVEL*BLANKS   ARCM75
     C********   FC,5      CAT  FTCPCM    ARCM75
     C********
     C********             MOVELQQKEY     ARKEY
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********             MOVELFTKEY     ARKEY
     C********             MOVEL*BLANKS   ARCM75
     C********             MOVE FTSEQ#    ARSEQ#
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
     C********
     C********   ENDDRT    ENDSR
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         CABEQ     *ON           ENDDRT
     C*
     C*
     C*          Call API to write a DARTS Additional Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  FTKEY         Q#KEY            40
     C                   MOVE      FTSEQ#        Q#SEQ#            5
     C                   MOVEL(P)  *BLANKS       Q#CM75           75
     C                   MOVEL(P)  Q1PGM         Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  CMRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#ASQ#            5
     C                   MOVEL     CMCSID        Q#CSID            2
     C                   MOVEL     CMCDAT        Q#CDAT            7
     C                   MOVEL     CMCODE        Q#CODE            2
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C                   MOVEL     *BLANKS       Q#CM75
     C                   MOVE      DSDATE        X#DT6             6
     C     X#DT6         CAT       '  Fax#: '    XXWK15           15
     C     FC(1)         CAT       XXWK15        XXWK38           38
     C     XXWK38        CAT       FTFPH#        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC(2)         CAT       FTCPTI        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC(3)         CAT       FTCPT1        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C     FTCPT2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC6           CAT       FTCPT2        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C                   ENDIF                                                  CPT2 <> blanks
     C*
     C     FTCPT3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC6           CAT       FTCPT3        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C                   ENDIF                                                  CPT3 <> blanks
     C*
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC(4)         CAT       FTCPF1        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C     FTCPF2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC6           CAT       FTCPF2        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C                   ENDIF                                                  CPF2 <> blanks
     C*
     C     FTCPF3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC6           CAT       FTCPF3        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C                   ENDIF                                                  CPF3 <> blanks
     C*
     C                   MOVEL     *BLANKS       Q#CM75
     C     FC(5)         CAT       FTCPCM        Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C                   MOVEL     *BLANKS       Q#CM75
     C                   MOVE      '00000'       Q#ASQ#
     C                   CALL      'DA701R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#CM75
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#ASQ#
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#CODE
     C*
     C     ENDDRT        ENDSR
     C*** END ADD *** 04/25/00 ****************************************
**
=*ERROR*= Document did not complete the Facsimile transmission.

Unable to Re-Send the Facsimile.  Error Occured on Re-transmission.

Facsimile was unable to complete transmission.  This Fax is being resc
heduled.
Facsimile processing completed normally.
**   RMRK ENTRIES
Fax sent:   Date    :
            Heading :
            To      :
            Comments:
            From    :
