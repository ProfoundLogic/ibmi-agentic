     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F7010R - PROMPT FOR TABLE FILE LISTING
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/15/00 C:62 PROJECT: MC215
     H*               REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H* 06/29/98 C:94 PROJECT: MC145
     H*               CHANGED A/R FLAG CHECK TO FBAL04 POS 5
     H*               AND CHANGED ACTIVITY CODE FLAG CHECK TO FBAL01
     H*               POS 9.
     H*****************************************************************
     FF7010F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 ***********************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 ***********************************************

     D*
     D                 DS
     D  FLAGS                  1     15
     D  SCEBCD                 1      1
     D  SCPMCD                 2      2
     D  SCCSCD                 3      3
     D  SCPACD                 4      4
     D  SCSPCD                 5      5
     D  SCRCCD                 6      6
     D  SCFTCD                 7      7
     D  SCCCCD                 8      8
     D  SCCVCD                 9      9
     D  SCCLCD                10     10
     D  SCMCCD                11     11
     D  SCSSCD                12     12
     D  SCPSCD                13     13
     D  SCARCD                14     14
     D  SCACCD                15     15
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR STATE FILE
     C     STKY          KLIST
     C                   KFLD                    XXKEY
     C*
     C*  FS BASE CONTROL FILE
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 ***********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQST              2
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
     C*** BEGIN ADD *** 08/15/00 ***********************************************
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
     C*** END   ADD *** 08/15/00 ***********************************************
     C                   MOVE      *BLANK        SCST
     C                   MOVE      QQST          SCST
     C*
     C     *LIKE         DEFINE    STKEY         XXKEY
     C*
     C                   MOVEL     SCST          XXKEY
     C     STKY          CHAIN     PST01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     STNAME        SCSTNM
     C                   END
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      QQST          SCST
     C*
     C*   CHECK TO SEE IF NETWORK/PPO INSTALLED SFBPF/FBFL06 = 'Y'
     C*   IF NOT INSTALLED DO NOT DISPLAY A/R TRANACTION CODE LISTING
     C*   OPTION ON SCREEN.
     C*
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** BEG ADD *** 06/29/98 **************************************
     C**********           SUBSTFBAL04:5  XXAR    1
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEGIN CHG *** 08/15/00 ***********************************************
     C********** XXAR      IFNE 'Y'
     C     ##AR          IFNE      'Y'
     C*** END   CHG *** 08/15/00 ***********************************************
     C********** FBFL06    IFNE 'Y'
     C                   MOVE      '1'           *IN99
     C                   ELSE
     C                   MOVE      '0'           *IN99
     C                   END                                                    END FBAL04
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           SUBSTFBAL01:9  XXACT   1
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEGIN CHG *** 08/15/00 ***********************************************
     C********** XXACT     IFNE 'Y'
     C     ##MED         IFNE      'Y'
     C*** END   CHG *** 08/15/00 ***********************************************
     C                   MOVE      '1'           *IN98
     C                   ELSE
     C                   MOVE      '0'           *IN98
     C                   END                                                    END FBAL01
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           END                             END *IN60
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** END ADD *** 06/29/98 **************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C**********************************
     C*   F12 = PREVIOUS               *
     C**********************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      '*'           QQCALL
     C                   GOTO      TERM
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C     FLAGS         IFNE      *BLANKS
     C                   MOVE      UPCO#         X@CO#             3
     C                   MOVE      SCST          QQST              2
     C                   MOVE      SCSTNM        QQSTNM           40
     C                   CALL      'F7010C'
     C                   PARM                    X@CO#
     C                   PARM                    FLAGS
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   PARM                    QQST
     C                   PARM                    QQSTNM
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
