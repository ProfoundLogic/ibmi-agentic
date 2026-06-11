     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F0007R - UPDATE SMUPP W/ SYSTEM DATE
     H****************************************************************
     H* MODIFICATIONS:
     H*  06/15/99 C:53 MC145
     H*                REMOVE WMUP* AND ADD A CALL TO UPDATE THE FILE
     H*                IF PDS WORKERS COMP IS INSTALLED.
     H*
     H*              LJQ  8-23-91   ADDED WORK COMP USER PARM FILE.
     H*                             FILE NEEDS TO BE UPDATED WHEN
     H*                             SYSTEM USER PARM IS UPDATED
     H****************************************************************
     H*
     FSMUPL     UF   E           K DISK
     F*** BEG DEL *** 06/15/99 *****************************
     F**WMUPL * UF  E ********* K        DISK
     F*********** PUP01 *******                     KRENAMEPUP02
     F*** END DEL *** 06/15/99 *****************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
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
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD *** 06/15/99 *************
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
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
     C*** END ADD *** 06/15/99 *************
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C*** BEG DEL *** 06/15/99 *************
     C********** UPKEY     CHAINPUP02                65    WMUPP
     C*** END DEL *** 06/15/99 *************
     C     UPKEY         CHAIN     PUP01                              60        SMUPP
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPPRDT
     C*
     C                   UPDATE    PUP01                                        |
     C                   END
     C*** BEG ADD *** 06/15/99 *************
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'
     C                   CALL      'WA025R'                             99
     C                   PARM                    UPUSR
     C                   PARM                    ##DLT             1
     C                   ENDIF
     C*
     C*** END ADD *** 06/15/99 *************
     C*** BEG DEL *** 06/15/99 *************
     C********** *IN65     IFEQ '0'                        LJQ 8-23-91
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     UPPRDT
     C**********
     C**********           UPDATPUP02                      |
     C**********           END                             |
     C*** END DEL *** 06/15/99 *************
     C*
     C                   MOVE      '1'           *INLR
     C*
