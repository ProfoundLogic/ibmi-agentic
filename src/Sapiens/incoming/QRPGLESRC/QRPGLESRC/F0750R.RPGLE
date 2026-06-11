     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* PROGRAM: F0750R                                               *
     H* DESCRIPTION:  REQUEST LETTER - SCREEN PROGRAM                 *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20 C:212 Project : SY420
     H*                 Mask SSN
     H*
     H*  01/10/02 C:62 PROJECT: MC234  BASE ONLY
     H*                - COMPILE ONLY.
     H*
     H*  12/28/01 C:62 PROJECT: MC234  BASE ONLY
     H*                - ADD PROCESSING FOR SECOND PROVIDER ADRESS.
     H*
     H*  03/23/01 C:62  Project: FSMNT  BASE ONLY
     H*                 COMPILE ONLY
     H*
     H*  12/16/99 C:56  Project: Y2K
     H*                 Remove definition of SCFYR.  It's defined in
     H*                 display file.
     H*
     H*  10/25/99 C:31  Support 20174
     H*                 Change location of 1st cursor
     H*                 1. Compile only
     H*
     H*  07/01/99 C:56 Project: MC202                                 *
     H*                Add entry parms EOMB# and RCN#                 *
     H*
     H*  10/02/89 LJD      - LOAD 30+ DAYS FROM SYSTEM TO DIARY DATE  *
     H*  PROJ #: I0030       BECAUSE USER DOES NOT WANT 2ND REQUEST   *
     H*                      LETTER TO PRINT.                         *
     H*                      F0770R (DIARY DATE PRINT) WILL LOOK AT   *
     H*                      LTFLG1 "1" IN 3RD REQUEST INSTEAD OF "2" *
     H*                                                               *
     H*  07/22/92 KRR        ADDED SUB# TO ALL PROCESSING             *
     H*   PROJ: 99AAC                                                 *
     H*                                                               *
     H*  10/05/92 KRR      - ADDED QQPRTY TO ENTRY PARMS AND TO UPDATE*
     H*                      FALTP FILE LTPRTY                        *
     H*                                                               *
     H*  12/02/92 C:20     - ADDED QQPRNT TO ENTRY PARMS IF QQPRNT IS *
     H*                      EQUAL TO "N" SET PRINT FLAG TO N.  THIS  *
     H*                      2ILL WRITE AN FALTP RECORD BUT NOT PRINT *
     H*                                                               *
     H*  04/13/93 C:20     - REMOVED WORDS BLOCK FROM SCREEN          *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H* 20  -  INVALID FROM SERVICE DATE                              *
     H* 21  -  INVALID TO   SERVICE DATE                              *
     H* 22  -  SERVICE DATES MUST BE IN ASCENDING ORDER               *
     H* 23  -  DIARY DATE REQUIRED                                    *
     H* 24  -  INVALID DIARY DATE                                     *
     H* 25  -  INVALID BATCH/IMMED FLAG (MUST BE B OR I OR N)         *
     H* 40  -  PROTECT SCBTIM AND DO NOT ALLOW CHANGE FROM AN 'N'     *
     H***********************************************************************
     H* FIELDS:                                                             *
     H* QQRTCD FROM F0750R TO F0070R:                                       *
     H*      A = END OF JOB (CMD 1)              (CHRGS PENDED)             *
     H*      N = PREVIOUS SCREEN (CMD 2)         (DON'T PEND CHRGS)         *
     H*      * = SUCCESSFUL COMPLETION OF F0750R (LTR CREATED/CHRGS PENDED) *
     H***********************************************************************
     F*
     F*
     FF0750F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMC1L     UF   E           K DISK
     FFALTP     O    E             DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D DS              S             30    DIM(2) CTDATA PERRCD(1)              LETTER DESC
     D*
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  FL                     1     17
     D                                     DIM(1)                               REASON BLOCKS
     D  SCFL01                 1      1
     D  SCFL02                 2      2
     D  SCFL03                 3      3
     D  SCFL04                 4      4
     D  SCFL05                 5      5
     D  SCFL06                 6      6
     D  SCFL07                 7      7
     D  SCFL08                 8      8
     D  SCFL09                 9      9
     D  SCFL10                10     10
     D  SCFL11                11     11
     D  SCFL12                12     12
     D  SCFL13                13     13
     D  SCFL14                14     14
     D  SCFL15                15     15
     D  SCFL16                16     16
     D  SCFL17                17     17
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
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   KFLD                    SCSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR THE PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFSDT            8
     C                   PARM                    QQTSDT            8
     C                   PARM                    QQTTLC            9
     C                   PARM                    QQLTR#            9
     C*** BEGIN ADD *** 10/05/92 **************************************
     C                   PARM                    QQPRTY            1
     C*** END ADD *** 10/05/92 ****************************************
     C*** BEGIN ADD *** 12/02/92 **************************************
     C                   PARM                    QQPRNT            1
     C*** END ADD *** 12/02/92 ****************************************
     C                   PARM                    QQRTCD            1
     C*** BEG ADD *** 07/01/99 ****************************************
     C                   PARM                    QQEOB#            9 0
     C                   PARM                    QQRCN             3 0
     C*** END ADD *** 07/01/99 ****************************************
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
     C                   MOVE      QQSUB#        SCSUB#            5 0
     C                   MOVE      QQFND         SCFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C*** BEG CHG *** 12/16/99 ****************************************
     C**********           MOVE QQFYR     SCFYR   20
     C                   CLEAR                   SCFYR
     C                   MOVE      QQFYR         SCFYR
     C*** END CHG *** 12/16/99 ****************************************
     C                   MOVE      QQCASE        SCCASE            7 0
     C                   MOVE      QQFEIN        SCFEIN            9 0
     C                   MOVE      QQMOD         SCMOD             3 0
     C                   MOVE      QQFSDT        SCFSDT
     C                   MOVE      QQTSDT        SCTSDT
     C*** BEG DEL *** 05/11/98 ***********************
     C***                  Z-ADDXXFSDT    SRDTI
     C***                  CALL 'SRCVTDAT'DTMDYY
     C***                  MOVELSRDTO     SCFSDT  80
     C***                  Z-ADDXXTSDT    SRDTI
     C***                  CALL 'SRCVTDAT'DTMDYY
     C***                  MOVELSRDTO     SCTSDT  80
     C*** BEG DEL *** 05/11/98 ***********************
     C*
     C     QQCALL        COMP      '1'                                    09    INQUIRY
     C     QQCALL        COMP      '2'                                    08    ANOTHER PGM
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
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE LAST LETTER # USED FROM COMPANY AUXILIARY RECORD
     C*
     C     C1KEY         CHAIN     PC101                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      C1LTR#        XXLTR#            9 0          LAST # USED
     C                   ADD       1             XXLTR#
     C                   MOVE      XXLTR#        C1LTR#
     C                   UPDATE    PC101
     C                   END
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*** BEGIN ADD *** 10/02/89 *************************************
     C*  SET SCREEN DIARY DATE 30 DAYS FROM SYSTEM DATE
     C                   Z-ADD     DSDATE        XXDATE            8 0
     C                   MOVE      XXDATE        SRDATE            8 0
     C                   Z-ADD     30            SRFUTR            5 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD            1
     C                   Z-ADD     SRDATE        SCDIDT
     C*** END ADD ***** 10/02/89 *************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*  GET THE PATIENT SS#/ACDT
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANKS       SCSS#
     C                   Z-ADD     *ZEROS        SCACDT
     C                   END
     C*
     C*  GET THE PATIENT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCLNM
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   END
     C*
     C*  GET THE PROVIDER NAME/ADDRESS
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN CHG *** 12/28/01 **************************************
     C**********           MOVELPDNAME    SCPRNM
     C**********           MOVELPDADR1    SCADR1
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        SCPRNM
     C                   MOVEL(P)  PDADR2        SCADR1
     C                   MOVEL(P)  *BLANKS       SCADR2
     C                   ELSE
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        SCPRNM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      SCPRNM
     C                   ENDIF
     C                   MOVEL(P)  PDADR1        SCADR1
     C                   MOVEL(P)  PDADR2        SCADR2
     C                   ENDIF                                                  PDDBA1 = 'Y'
     C*** END   CHG *** 12/28/01 **************************************
     C                   MOVEL     PDCTY         SCCITY
     C                   MOVEL     PDST          SCST
     C                   Z-ADD     PDZIP         SCZIP
     C                   ELSE
     C                   MOVE      *BLANKS       SCPRNM
     C                   MOVE      *BLANKS       SCADR1
     C*** BEGIN ADD *** 12/28/01 **************************************
     C                   MOVE      *BLANKS       SCADR2
     C*** END   ADD *** 12/28/01 **************************************
     C                   MOVE      *BLANKS       SCCITY
     C                   MOVE      *BLANKS       SCST
     C                   Z-ADD     *ZEROS        SCZIP
     C                   END
     C*
     C                   MOVE      QQTTLC        SCAMT
     C**********           Z-ADD*ZEROS    SCDIDT            10/02/89 DEL
     C*** BEGIN ADD *** 12/02/92 **************************************
     C                   MOVE      '0'           *IN40
     C     QQPRNT        IFEQ      'N'
     C                   MOVEL     'N'           SCBTIM
     C                   MOVE      '1'           *IN40
     C                   ELSE
     C                   MOVEL     'B'           SCBTIM
     C                   END
     C*** END ADD *** 12//02/92 ***************************************
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C******************************************************
     C*   COMMAND 03 = END OF JOB                          *
     C******************************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C******************************************************
     C*   COMMAND 12 = PREVIOUS SCREEN                     *
     C******************************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      'N'           QQRTCD
     C                   GOTO      TERM
     C                   END                                                    ENDIF SCR01
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR02
     C                   END                                                    ENDIF *INKL
     C******************************************************
     C*   PROCESS HELP REQUESTS                            *
     C******************************************************
     C     *IN10         IFEQ      '1'
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
     C                   END                                                    END *IN10
     C****************************************************
     C*   COMMAND 13 - CHANGE USER PARMS                 *
     C****************************************************
     C     *INKM         IFEQ      '1'
     C                   CALL      'F0006R'
     C                   WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKM
     C*
     C                   END                                                    END CMD KEYS
     C*****************************************************************
     C*   GOTO THE APPROPRIATE SCREENS
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   VALIDATE THE FROM DATE ENTERED
     C*
     C*
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFSDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     20    INVLD FROM
     C*
     C*   VALIDATE THE TO DATE ENTERED
     C*
     C*
     C                   Z-ADD     SCTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTSDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     21    INVLD TO
     C*
     C*   VALIDATE THE FROM DATE NOT GREATER THAN THE TO DATE
     C*
     C                   MOVE      SCFSDT        SRFRDC            8 0
     C                   MOVE      SCTSDT        SRTODC            8 0
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDC
     C                   PARM                    SRTODC
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C     SRERR         CABEQ     '1'           STRT                     22    WRONG ORDER
     C*
     C*   VALIDATE THE DIARY DATE ENTERED
     C*
     C     SCDIDT        IFLE      *ZERO
     C                   MOVE      '1'           *IN23                          DIARY REQD
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDIDT
     C*
     C     SRERR         CABEQ     '1'           STRT                     24    INVLD DIARY
     C*
     C*   VALIDATE THE BATCH/IMMED FLAG
     C*
     C     SCBTIM        IFNE      'B'
     C     SCBTIM        ANDNE     'I'
     C     SCBTIM        ANDNE     'N'
     C                   MOVE      '1'           *IN25                          INVLD BT/IM
     C                   GOTO      STRT
     C                   END
     C*
     C*   ALL EDITS HAVE BEEN PASSED  - SET-UP SCREEN 02
     C*
     C                   MOVE      *BLANK        SCFL01
     C                   MOVE      *BLANK        SCFL02
     C                   MOVE      *BLANK        SCFL03
     C                   MOVE      *BLANK        SCFL04
     C                   MOVE      *BLANK        SCFL05
     C                   MOVE      *BLANK        SCFL06
     C                   MOVE      *BLANK        SCFL07
     C                   MOVE      *BLANK        SCFL08
     C                   MOVE      *BLANK        SCFL09
     C                   MOVE      *BLANK        SCFL10
     C                   MOVE      *BLANK        SCFL11
     C                   MOVE      *BLANK        SCFL12
     C                   MOVE      *BLANK        SCFL13
     C                   MOVE      *BLANK        SCFL14
     C                   MOVE      *BLANK        SCFL15
     C                   MOVE      *BLANK        SCFL16
     C                   MOVE      *BLANK        SCFL17
     C                   MOVE      *BLANK        SCBL24
     C                   MOVE      *BLANK        SCOTH
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   SCREEN 02 - REASON FOR LETTER
     C*
     C     SCR02T        TAG
     C*
     C*   ALL EDITS HAVE BEEN PASSED  - UPDATE THE FILE
     C*
     C                   Z-ADD     XXLTR#        LTLTR#
     C                   Z-ADD     UPCO#         LTCO#
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   Z-ADD     SCSUB#        LTSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   Z-ADD     SCFND         LTFND
     C                   Z-ADD     SCFYR         LTFYR
     C                   Z-ADD     SCCASE        LTCASE
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   MOVEL     SCSS#         LTSS#
     C                   MOVEL     CDLSN         LTLNM
     C                   MOVEL     CDFSN         LTFNM
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTACDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTTRDT
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTFSDT
     C                   Z-ADD     SCTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTTSDT
     C                   Z-ADD     SCFEIN        LTFEIN
     C                   Z-ADD     SCMOD         LTMOD
     C                   Z-ADD     SCAMT         LTAMT
     C                   MOVEA     FL            LTREAS
     C                   MOVEL     SCBL24        LTBL24
     C                   MOVEL     SCB224        LT24D2
     C                   MOVEL     SCOTH         LTOTH
     C                   MOVEL     SCOT2         LTOTD2
     C                   MOVE      *BLANKS       LTDESC
     C                   MOVE      *BLANKS       LTSTAT
     C*** BEGIN ADD *** 10/05/92 **************************************
     C                   MOVE      QQPRTY        LTPRTY
     C*** END ADD *** 10/05/92 ****************************************
     C                   MOVE      Q1USER        LTUSER
     C                   MOVE      Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C                   MOVE      Q1USER        LTIUSR
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTIDT
     C*
     C     SCBTIM        IFEQ      'N'                                          LTR BYPASSED
     C                   Z-ADD     *ZEROS        LTDIDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTPRDT
     C                   MOVE      *BLANKS       LTBTIM
     C                   MOVE      'B'           LTSTAT
     C                   MOVEL     DS(1)         LTDESC
     C                   ELSE
     C                   Z-ADD     SCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDIDT
     C                   Z-ADD     *ZEROS        LTPRDT
     C                   MOVE      SCBTIM        LTBTIM
     C                   MOVEL     DS(2)         LTDESC
     C                   END                                                    ENDIF *IN15
     C*
     C*** BEG ADD *** 07/01/99 ****************************************
     C     Q1PRMS        IFGE      17
     C                   Z-ADD     QQEOB#        LTEOB#
     C                   Z-ADD     QQRCN         LTRCN
     C                   ENDIF                                                  Q1PRMS >= 17
     C*** END ADD *** 07/01/99 ****************************************
     C                   WRITE     PLT01
     C*
     C*  IF BATCH/IMMED FLAG = 'I', CALL SUBMITTER TO LETTER PRINT PGM
     C*
     C*** QQLTR# NEEDED FOR RETURN TO F0070R
     C*** AND IF CALL MADE TO F0766C
     C                   MOVE      LTLTR#        QQLTR#            9
     C*
     C     SCBTIM        IFEQ      'I'
     C                   MOVE      UPCO#         QQCO#             3
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   MOVE      LTSUB#        QQSUB#            5
     C*** END ADD *** 07/22/92 ****************************************
     C                   MOVE      LTFND         QQFND             3
     C                   MOVE      LTFYR         QXFYR             3
     C                   MOVE      LTCASE        QQCASE            7
     C                   CALL      'F0766C'
     C                   PARM                    QQCO#
     C                   PARM                    QQSUB#                         ADD 07/22/92
     C                   PARM                    QQFND
     C                   PARM                    QXFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQLTR#
     C                   PARM      'I'           QQFLAG            1
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C                   END                                                    ENDIF 'I'
     C*
     C*  END THE PROGRAM
     C*
     C                   MOVE      '*'           QQRTCD
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
** DS (LETTER DESCRIPTION)
PRINTED LETTER BYPASSED
FEE SCHEDULE REQUEST LETTER
