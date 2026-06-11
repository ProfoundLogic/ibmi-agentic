     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_0VR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE F0006R1 - USER PARAMETER FILE MAINTENANCE
     H*
     H*****************  IMPORTANT ***********************************
     H*  THIS PROGRAM IS TO BE USED TO CHANGE THE SUB/GROUP ONLY
     H*
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM AUTOMATICALLY UPDATES THE  *
     H*   USER PROFILE WITH THE FUND PASSED                           *
     H* CALLED FROM:                                                  *
     H*   F0086R                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: 05                                                *
     H* CREATED: 06/29/94               PROJECT: MC110                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/17/99  C:53 MC145
     H*                QUALIFY WCMS CALLS ON NON-PDS WC FLAG
     H*
     H* 04/12/99 C:56  Project: MC152                                 *
     H*                Add call to api WA025R to update WMUPP         *
     H*                Remove call to WMUPL                           *
     H*****************************************************************
     F*** BEG DEL *** 04/12/99 ****************************************
     F**********WMUPL   UF  E           K        DISK
     F*** END DEL *** 04/12/99 ****************************************
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
     C                   PARM                    QQSUB#            5
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
     C                   MOVE      QQSUB#        XXSUB#            5 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      Q1USER        XXUSR
     C*
     C*** BEG DEL *** 04/12/99 ****************************************
     C********** UPKEY     CHAINPUP01                40     WMUPP
     C*** END DEL *** 04/12/99 ****************************************
     C     UPKEY         CHAIN     PUP02                              45         SMUPP
     C*
     C                   Z-ADD     XXSUB#        UPSUB#
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
     C*** BEG DEL *** 04/12/99 ****************************************
     C********** *IN40     IFEQ '0'
     C**********           UPDATPUP01
     C**********           ELSE
     C**********           MOVE '1'       QQRTCD
     C**********           END
     C*** END DEL *** 04/12/99 ****************************************
     C*
     C     *IN45         IFEQ      '0'
     C                   UPDATE    PUP02
     C                   ELSE
     C                   MOVE      '1'           QQRTCD
     C                   END
     C*
     C*** BEG ADD *** 04/12/99 ****************************************
     C*
     C*  GET SYSTEM FLAGS
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
     C*** BEG ADD *** 06/17/99 *****************************
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 06/17/99 *****************************
     C*
     C* CALL API TO UPDATE WORKER'S COMP USER PARM IF WCMS IS INSTALLED
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/17/99
     C                   MOVEL(P)  Q1USER        ##USER
     C                   CALL      'WA025R'                             99
     C                   PARM                    ##USER           10
     C                   PARM      ' '           ##DLT             1
     C                   ENDIF
     C*
     C*** END ADD *** 04/12/99 ****************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
