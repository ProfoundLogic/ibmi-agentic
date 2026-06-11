     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F0006R1 - USER PARAMETER FILE MAINTENANCE
     H*****************************************************************
     H*
     H*****************  IMPORTANT ***********************************
     H*  THIS PROGRAM IS TO BE USED TO CHANGE THE WORKERS COMP FUND# ONLY
     H*
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM AUTOMATICALLY UPDATES THE  *
     H*   USER PROFILE WITH THE FUND PASSED                           *
     H* CALLED FROM:                                                  *
     H*   F0086R,  F0269R                                             *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: 05                                                *
     H* CREATED: 10/07/92               PROJECT: 99AAG                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 06/07/99  C:53 MC145
     H*                REMOVE WMUP* AND ADD A CALL TO UPDATE THE FILE
     H*                IF PDS WORKERS COMP IS INSTALLED.
     H*
     H*****************************************************************
     F*** BEG DEL *** 06/07/99 *****************************
     F**WMUPL***UF  E ********* K        DISK
     F*** END DEL *** 06/07/99 *****************************
     F*
     FSMUPL     UF   E           K DISK
     F                                     RENAME(PUP01:PUP02)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR            10
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
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
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      Q1USER        XXUSR
     C*** BEG DEL *** 06/07/99 *****************************
     C********** UPKEY     CHAINPUP01                40     WMUPP
     C*** END DEL *** 06/07/99 *****************************
     C     UPKEY         CHAIN     PUP02                              45         SMUPP
     C*
     C                   Z-ADD     XXFND         UPFND
     C                   MOVE      Q1USER        UPUSER
     C                   MOVE      Q1PGM         UPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   Z-ADD     DSTIME        UPTIME
     C*
     C*** BEG DEL *** 06/07/99 *****************************
     C********** *IN40     IFEQ '0'
     C**********           UPDATPUP01
     C**********           ELSE
     C**********           MOVE '1'       QQRTCD
     C**********           END
     C*** END DEL *** 06/07/99 *****************************
     C*
     C     *IN45         IFEQ      '0'
     C                   UPDATE    PUP02
     C*
     C*** BEG ADD *** 06/07/99 *****************************
     C                   FEOD      SMUPL
     C*
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   CALL      'WA025R'                             99
     C                   PARM                    UPUSR
     C                   PARM                    ##DLT             1
     C                   ENDIF
     C*
     C*** END ADD *** 06/07/99 *****************************
     C                   ELSE
     C                   MOVE      '1'           QQRTCD
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
