     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE IS407R - UPDATE TRANSACTION RECORDS FRM DTAQ
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
     FSAFTL     UF   E           K DISK
     FIAFTL     UF   E           K DISK
     F                                     RENAME(PFT01:PFT02)
     FSMARL     IF A E           K DISK
     FSDCMLA    IF   E           K DISK
     FSAFEP     O    E             DISK
     FIMCVL     IF   E           K DISK
     F*
     F*

***  D #ER             S             70    DIM(8) CTDATA PERRCD(1)
     D FC              S             23    DIM(6) CTDATA PERRCD(1)
     D DN              S             75    DIM(12)                              DOCN's
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSMIN                  3      4  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FC6             C                   CONST('                    -
     D                                        ')
     D****
     D DDAT            C                   CONST('Date:')
     D DTIM            C                   CONST('Time:')
     D DREA            C                   CONST('*** Outgoing Fax ***')

     I*
     I*
     IPFT02
     I              FTFCTM                      FTFCT2

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
     C     ARKY          KLIST
     C                   KFLD                    FTKEY
     C                   KFLD                    FTSEQ#
     C                   KFLD                    XXASQ#
     C*
     C     ARKY2         KLIST
     C                   KFLD                    FTKEY
     C                   KFLD                    FTSEQ#
     C*
     C*
     C     CVKY          KLIST
     C                   KFLD                    Q@CVRI
     C*
     C     CMKY          KLIST
     C                   KFLD                    FTKEY
     C                   KFLD                    FTSEQ#
     C*
     C*
     C*** PARAMERTER LISTS FOR EXTERNAL CALLS
     C*
     C*   PARM LIST FOR "IRSNDFXC" CL PGM
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
     C                   PARM                    Q@CVPF            8
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
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      Q1FIL#        QQFIL#            3
     C                   TIME                    DSDTTM
     C*
     C*** GET FAX TRANS RECORD
     C*
     C     FTKY          CHAIN     PFT02                              60
     C     FTKY          CHAIN     PFT01                              61
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     FTCVRI        Q@CVRI            2
     C*
     C     CVKY          CHAIN     PCV01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CVPRTF        Q@CVPF            8
     C                   ELSE
     C                   MOVEL     'DFTCVR  '    Q@CVPF
     C                   ENDIF
     C*
     C                   MOVEL     FTKEY         QQKEY            40
     C*** IF NOT A SUCCESSFUL FAX
     C     QQSTS         IFNE      'S'
     C*** SEE IF MSG ID EXISTS IN THE RETRY DATA BASE. IF SO,
     C*** RETRY IS ALLOWED, ELSE DONT RETRY
     C*** TEMPORARY  *** IF MSG IS FAXC703, THEN DONT RETRY
     C     QQMSGI        IFEQ      'FAXC703'
     C                   ADD       3             FTRCNT
     C                   ENDIF
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
     C                   CALL      'ISFXERR'     PLSFXE
     C*
     C     QQMSGI        CASNE     *BLANKS       WRTFE
     C                   ENDCS
     C*
     C*** SET UP FOR STATUS CHANGE
     C                   GOTO      STSUPD
     C*
     C                   ELSE                                                   NOT 3 RETIRES
     C*
     C                   MOVEL     #ER(1)        QQDESC          140
     C                   MOVE      #ER(2)        QQDESC
     C                   MOVE      099           QQSEV             3 0
     C                   MOVEL     QQMSGI        QQMSID            7
     C                   MOVE      '*'           QQSTAT            1
     C*
     C                   CALL      'ISFXERR'     PLSFXE
     C*
     C*
     C     QQMSGI        CASNE     *BLANKS       WRTFE
     C                   ENDCS
     C*
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
     C                   MOVE      DSDATE        QQSDDT            6
     C                   END
     C                   ELSE
     C     DSTIME        ADD       001500        XXTIM
     C                   MOVE      XXTIM         QQSDTM
     C                   MOVE      DSDATE        QQSDDT
     C                   END
     C*
     C     FTCPT1        IFNE      *BLANKS
     C                   MOVE      ' '           QQRTCD
     C                   ELSE
     C                   MOVEL     'N'           QQRTCD
     C                   ENDIF
     C                   CALL      'IRSNDFXC'    PLSEND
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
     C                   CALL      'ISFXERR'     PLSFXE
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
     C                   CALL      'ISFXERR'     PLSFXE
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
     C                   MOVE      DSTIME        FTFCT2
      *
     C                   UPDATE    PFT02
     C  N61              UPDATE    PFT01
     C*
     C*** CREATE DARTS RECORD FOR COMPLETED NORMAL FAX
     C*
     C     QQSTAT        IFEQ      'C'
     C***        QQSTAT    OREQ 'E' *** ONLY COMPLETED FOR DARTS
     C*** CREATE DARTS
     C                   SELECT
     C     FTFL01        WHENEQ    'D'
     C                   EXSR      WDRT
     C                   OTHER
     C                   EXSR      DNOT
     C                   ENDSL
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
     C*
     C     FTRCNT        ADD       1             ##CNT             1 0
     C                   MOVE      ##CNT         @@CNT             1
     C     @@CNT         CAT       '. ':0        @@C3              3
     C*
     C     QQMDSC        IFNE      *BLANKS
     C     @@C3          CAT(P)    QQMDSC:0      FEDESC
     C******************** MOVELQQMDSC    FEDESC
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
     C*** BYPASS THIS *** DO SEND CODE ONLY ON COMPLETTION!!!!
     C*** PER TLB 9/25/95 *****
     C                   GOTO      NXT1
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         CABEQ     *ON           ENDDRT
     C*
     C                   Z-ADD     *HIVAL        XXASQ#
     C     ARKY          SETGT     PAR01
     C     ARKY2         READPE    PAR01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       ARASQ#        XXASQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXASQ#
     C                   ENDIF
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVEL     *BLANKS       ARCM75
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C                   ADD       1             XXASQ#
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C*
     C                   MOVE      DSDATE        X#DT6             6
     C     X#DT6         CAT       '  Fax#: '    XXWK15           15
     C     QQSTAT        IFEQ      'C'
     C     FC(1)         CAT       XXWK15        XXWK38           38
     C                   ELSE
     C     FC(4)         CAT       XXWK15        XXWK38           38
     C                   ENDIF
     C     XXWK38        CAT       FTFPH#        ARCM75
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C     FC(3)         CAT       FTCPT1        ARCM75
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C     FC6           CAT       QQDESC        ARCM75
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*** BYPASS TAG
     C     NXT1          TAG
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         CABEQ     *ON           ENDDRT
     C*
     C                   Z-ADD     *HIVAL        XXASQ#
     C     ARKY          SETGT     PAR01
     C     ARKY2         READPE    PAR01                                  60
     C     *IN60         IFEQ      *OFF
     C     1             ADD       ARASQ#        XXASQ#            5 0
     C                   ELSE
     C                   Z-ADD     1             XXASQ#
     C                   ENDIF
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVEL     *BLANKS       ARCM75
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C                   ADD       1             XXASQ#
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C                   MOVE      DSDATE        X#DT6             6
     C     X#DT6         CAT       '  Fax#: '    XXWK15           15
     C     FC(1)         CAT       XXWK15        XXWK38           38
     C     XXWK38        CAT       FTFPH#        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C     FC(2)         CAT       FTCPTI        ARCM75
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C*
     C                   ADD       1             XXASQ#
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C     FC(3)         CAT       FTCPT1        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C*
     C*
     C     FTCPT2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       ARCM75
     C     FC6           CAT       FTCPT2        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C                   ENDIF                                                    IF BLANKS
     C*
     C*
     C     FTCPT3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       ARCM75
     C     FC6           CAT       FTCPT3        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C                   ENDIF                                                    IF BLANKS
     C*
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C     FC(4)         CAT       FTCPF1        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C*
     C*
     C     FTCPF2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       ARCM75
     C     FC6           CAT       FTCPF2        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C                   ENDIF
     C*
     C*
     C     FTCPF3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       ARCM75
     C     FC6           CAT       FTCPF3        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C                   ENDIF
     C*
     C*
     C                   MOVEL     *BLANKS       ARCM75
     C     FC(5)         CAT       FTCPCM        ARCM75
     C*
     C                   MOVEL     QQKEY         ARKEY
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C                   ADD       1             XXASQ#
     C*
     C                   MOVEL     FTKEY         ARKEY
     C                   MOVEL     *BLANKS       ARCM75
     C                   MOVE      FTSEQ#        ARSEQ#
     C                   MOVE      XXASQ#        ARASQ#
     C                   MOVEL     CMCSID        ARCSID
     C                   MOVE      CMCDAT        ARCDAT
     C                   MOVEL     CMRMCD        ARRMCD
     C                   MOVEL     Q1PGM         ARPNAM                         12/31/91 CHG
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   MOVE      DSTIME        ARTRTM
     C                   MOVEL     CMSECL        ARSECL
     C                   MOVEL     CMSBYL        ARSBYL
     C                   MOVEL     CMSTAT        ARSTAT
     C                   MOVEL     Q1USER        ARUSER
     C                   MOVEL     Q1PGM         ARPGM
     C                   MOVE      DSTIME        ARTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   MOVE      ARDT          ARIDT
     C                   MOVEL     Q1USER        ARIUSR
     C*
     C                   WRITE     PAR01
     C*
     C*
     C*
     C     ENDDRT        ENDSR
     C*
      *===================DNOT========================================*
     C*
     C     DNOT          BEGSR
     C*** FILL ALL DOC NOTE FIELDS
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *BLANKS       XX75             75
     C                   Z-ADD     1             J
     C*
     C                   Z-ADD     FTFCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQCDAT            8
     C*
     C*
     C                   MOVEL     QQCDAT        XXM               2
     C                   MOVE      QQCDAT        XXY               4
     C     2             SUBST     QQCDAT:3      XXD               2
     C     XXM           CAT(P)    '/'           XX3A              3
     C     XX3A          CAT(P)    XXD           XX5A              5
     C     XX5A          CAT(P)    '/'           XX6A              6
     C     XX6A          CAT(P)    XXY           XXMDYY           10
     C*
     C                   MOVE      DSTIME        Q$TIME            6
     C                   MOVEL     Q$TIME        XXH               2
     C                   MOVE      Q$TIME        XXS               2
     C     2             SUBST     Q$TIME:3      XXM               2
     C     XXH           CAT(P)    ':'           XX3A              3
     C     XX3A          CAT(P)    XXM           XX5A              5
     C     XX5A          CAT(P)    ':'           XX6A              6
     C     XX6A          CAT(P)    XXS           XXHMS             8
      *
     C     DDAT          CAT(P)    XXMDYY:1      XX75
     C                   CAT(P)    DTIM:1        XX75
     C                   CAT(P)    XXHMS:1       XX75
     C                   CAT(P)    DREA:5        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C*
     C                   MOVEL     *BLANKS       XX75
     C     FC(6)         CAT(P)    FTFPH#        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C*
     C                   MOVEL     *BLANKS       XX75
     C     FC(2)         CAT       FTCPTI        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C*
     C                   MOVEL     *BLANKS       XX75
     C     FC(3)         CAT       FTCPT1        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J                 2 0
     C*
     C     FTCPT2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XX75
     C     FC6           CAT       FTCPT2        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C                   ENDIF
     C*
     C     FTCPT3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XX75
     C     FC6           CAT       FTCPT3        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       XX75
     C     FC(4)         CAT       FTCPF1        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C*
     C*
     C     FTCPF2        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XX75
     C     FC6           CAT       FTCPF2        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C                   ENDIF
     C*
     C*
     C     FTCPF3        IFNE      *BLANKS
     C                   MOVEL     *BLANKS       XX75
     C     FC6           CAT       FTCPF3        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C                   ENDIF
     C*
     C*
     C                   MOVEL     *BLANKS       XX75
     C     FC(5)         CAT       FTCPCM        XX75
     C                   MOVEL     XX75          DN(J)
     C                   ADD       1             J
     C*
     C                   MOVEL     *BLANKS       DN(J)
      *
     C*
     C*** CALL PGM TO WRITE DOC NOTES
     C*
      * * * DOW LOOP HERE TO PROCESS MULTIPLE ITEMS.  SETUP ITEMS
      * * * AS COMMENTS, OR ARRAY ELEMENTS
     C*
     C*
     C                   DO        J             G                 3 0
     C*
     C                   MOVEL     FTDCID        $#DCID           12
     C                   MOVEL     FTDCID        QQDCID           12
     C                   MOVEL     DN(G)         QQCM75           75
     C*
     C     G             IFGE      J
     C                   MOVE      '1'           Q$RTC             1
     C                   ELSE
     C                   MOVE      ' '           Q$RTC
     C                   ENDIF
     C*
     C                   CALL      'ISDOCNR'
     C                   PARM                    QQCM75           75              COMMENT
     C                   PARM                    QQDCID           12              DOCID
     C                   PARM                    QQCDAT            8              NOTE DATE
     C                   PARM      Q1PGM         QQPNAM           10              CALLING PGM
     C                   PARM      'H'           QQPRFL            1              PROTECT FLG
     C                   PARM                    Q$RTC             1              RTN CODE
     C*
     C                   ENDDO
     C*
     C*
     C*
     C     ENDNOT        ENDSR
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
            Fax #   :
