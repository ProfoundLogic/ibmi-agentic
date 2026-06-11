     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H* *****************************************************************
     H/TITLE FA029R - DELETE WCMC EMPLOYER
     H* *****************************************************************
     H*
     H*    CREATED: 04/11/00  C:53  PROJECT: MC189
     H*    DESCRIPTION: DELETE EMPLOYER
     H*
     H* *****************************************************************
     H*
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*  SEND TO THIS PGM:  IF @@RTCD = BLANK, CALL DARTS
     H*                     IF @@RTCD = 'K' (KEY ONLY), BUILD DARTS KEY
     H*                       AND RETURN WITHOUT CALLING DARTS +
     H*                     WCMS INTERFACE FLAG-WCMS DARTS KEY FLAG +
     H*                     CO#-SUB#-GROUP-YEAR-CASE
     H*                                                                 *
     H*  SEND BACK       :  IF @@RTCD = BLANK, NOTHING
     H*                     IF @@RTCD = 'K' (KEY ONLY):
     H*                           @@RTCD WILL BE 'W' IF USING WCMS KEY
     H*                       OR  @@RTCD WILL BE 'M' IF USING WCMC KEY
     H*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     H*                                                                 *
     H*  Called From:                                                   *
     H*    W0050R02                                                     *
     H*    F0140R                                                       *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    NONE                                                         *
     H*                                                                 *
     H* *****************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*  00/00/00  C:00 PROJ:
     H* *****************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    60 - General File Access                                     *
     H*
     FFMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FFMEML     UF   E           K DISK
     FFDENL1    UF   E           K DISK
     F*
     FSDCMLA    UF   E           K DISK
     FSMARL     UF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  QWKEY                  1     40
     D  QWCO#                  1      3  0
     D  QWFND                  4      6  0
     D  QWEMP#                 7     15  0
     D  QWDIV                 16     20  0
     D                 DS
     D  QMKEY                  1     40
     D  QMCO#                  1      3  0
     D  QMSUB#                 4      8  0
     D  QMFND                  9     11  0
     D  QMEMP#                12     20  0
     D  QMDIV                 21     25  0
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     EMKEY         KLIST
     C                   KFLD                    KYCO#
     C                   KFLD                    KYSUB#
     C                   KFLD                    KYFND
     C                   KFLD                    KYEMP#
     C                   KFLD                    KYDIV
     C*
     C*
     C*  DARTS REMARK DETAIL FILE
     C*
     C     CMKYM         KLIST
     C                   KFLD                    QMKEY
     C*
     C     CMKYW         KLIST
     C                   KFLD                    QWKEY
     C*
     C*  DARTS ADDITIONAL REMARK FILE
     C*
     C     ARKY          KLIST
     C                   KFLD                    CMKEY
     C                   KFLD                    CMSEQ#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@RRN#            9 0
     C*
     C     *LIKE         DEFINE    EMCO#         KYCO#
     C     *LIKE         DEFINE    EMSUB#        KYSUB#
     C     *LIKE         DEFINE    EMFND         KYFND
     C     *LIKE         DEFINE    EMEMP#        KYEMP#
     C     *LIKE         DEFINE    EMDIV         KYDIV
     C*
     C*** GET SYSTEM VALUES
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C                   TIME                    DSDTTM
     C*
     C*** IF CALLED FROM F0140R01 THE RRN# WILL BE PASSED IN.
     C*** RETRIEVE KEY FIELDS FOR OTHER CHAINS.
     C*
     C     @@RRN#        IFNE      *ZEROS
     C     @@RRN#        CHAIN     PEM02                              60
     C                   MOVE      EMCO#         KYCO#
     C                   MOVE      EMSUB#        KYSUB#
     C                   MOVE      EMFND         KYFND
     C                   MOVE      EMEMP#        KYEMP#
     C                   MOVE      EMDIV         KYDIV
     C*
     C                   ELSE
     C*
     C*** CALL API FOR SUB OR GROUP XLATE IF CALLED FROM WCMS
     C*
     C     @@SUB#        IFEQ      *BLANKS
     C     @@SUB#        OREQ      *ZEROS
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@CO#         ##CO#             3
     C                   PARM      @@FND         ##WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##SUB#        KYSUB#
     C                   MOVE      ##FND         KYFND
     C*** ##RTCD = 'F'
     C                   ELSE
     C                   GOTO      EOJ
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      @@CO#         KYCO#
     C                   MOVE      @@EMP#        KYEMP#
     C                   MOVE      @@DIV         KYDIV
     C*
     C*** CALLED FROM WCMS, DELETE MASTER ALSO.  FOR WCMC F0140R01
     C*** WILL DELETE IN THE NORMAL "WORK WITH" PROCESS.
     C*
     C     @@SUB#        IFEQ      *BLANKS
     C     @@SUB#        OREQ      *ZEROS
     C     EMKEY         DELETE    PEM01                              60
     C                   ENDIF
     C                   ENDIF                                                  @@RRN#
     C*
     C*** DELETE SECONDARY FILES
     C*
     C*** EMPLOYER NETWORK FILE
     C*
     C     *IN60         DOUEQ     *ON
     C     EMKEY         DELETE    PEN01                              60
     C                   ENDDO
     C*
     C*** INACTIVATE DARTS IF CALLED FROM WCMC.  F0140R WILL DO
     C*** THE CHECKING FOR STANDALONE.
     C*
     C     @@RRN#        IFNE      *ZEROS
     C*
     C*  INACTIVATE DARTS REMARKS AND THE CORRESPONDING ADDT'L RMKS
     C*  AND LOAD DIARY COMPLETION DATE TO DIARY ITEMS
     C*
     C*    ESTABLISH KEY AND GET RECORD
     C*
     C                   CLEAR                   QWKEY
     C                   CLEAR                   QMKEY
     C*
     C     ##WCDK        IFEQ      'Y'
     C                   Z-ADD     EMCO#         QWCO#
     C                   Z-ADD     EMFND         QWFND
     C                   Z-ADD     EMEMP#        QWEMP#
     C                   Z-ADD     EMDIV         QWDIV
     C*
     C     CMKYW         CHAIN     PCM01                              60
     C*
     C                   ELSE
     C                   Z-ADD     EMCO#         QMCO#
     C                   Z-ADD     EMFND         QMFND
     C                   Z-ADD     EMSUB#        QMSUB#
     C                   Z-ADD     EMEMP#        QMEMP#
     C                   Z-ADD     EMDIV         QMDIV
     C*
     C     CMKYM         CHAIN     PCM01                              60
     C*
     C                   ENDIF
     C*
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     CMDIFL        IFEQ      'Y'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDCDT
     C*
     C                   ELSE
     C                   MOVE      'I'           CMSTAT
     C*
     C*  CHAIN TO ADDITIONAL REMARKS FILE
     C*
     C     ARKY          CHAIN     PAR01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      'I'           ARSTAT
     C                   MOVEL     Q1PGM         ARPGM
     C                   Z-ADD     DSTIME        ARTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   UPDATE    PAR01
     C*
     C     ARKY          READE     PAR01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF/ARKY
     C*
     C                   ENDIF                                                  CMDIFL=Y
     C*
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   UPDATE    PCM01
     C*
     C     ##WCDK        IFEQ      'Y'
     C     CMKYW         READE     PCM01                                  60
     C                   ELSE                                                                    =Y
     C     CMKYM         READE     PCM01                                  60
     C                   ENDIF                                                                   =Y
     C*
     C                   ENDDO                                                  READ CM
     C*
     C                   ENDIF                                                  RRN#             =Y
     C*
     C*
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     EOJ           TAG
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
