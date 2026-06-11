     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H* PROGRAM: F0751R                                               *
     H* DESCRIPTION:  REQUEST LETTER MAINTENANCE - SCREEN PROGRAM     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/03/25  C:342 120199 (SY458)                               *
     H*                  DSPF changes:                                *
     H*                  1. Added read only indicator for Dairy Date  *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*  01/10/02 C:62 PROJECT: MC234  BASE ONLY                      *
     H*                - COMPILE ONLY.                                *
     H*                                                               *
     H*  12/28/01 C:62 PROJECT: MC234  BASE ONLY                      *
     H*                - ADD PROCESSING FOR SECOND PROVIDER ADRESS.   *
     H*                                                               *
     H*  06/24/99 C:62 PROJECT: MC202                                 *
     H*                - MAKE SURE THAT FROM F0399R, GOES TO SCR02.   *
     H*                - ADDED PARM TO PASS FALTP RECORD (RRN).       *
     H*                - NO EDITS IN DISPLAY MODE.                    *
     H*                                                               *
     H*  04/13/93 C:20     - REMOVED WORDS BLOCK FROM SCREEN          *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H* SCR01:                                                        *
     H* 20  -  INVALID FUND                                           *
     H* 21  -  INVALID CASE                                           *
     H* 22  -  INVALID LETTER #                                       *
     H* 23  -  POSITION CURSOR AT FUND                                *
     H* 24  -  POSITION CURSOR AT FUND YEAR                           *
     H* 25  -  POSITION CURSOR AT LETTER #                            *
     H* 30  -  INVALID SUBSCRIBER NUMBER                              *
     H* 23  -  POSITION CURSOR AT SUBSCRIBER                          *
     H* SCR02:                                                        *
     H* 20  -  INVALID FROM SERVICE DATE                              *
     H* 21  -  INVALID TO   SERVICE DATE                              *
     H* 22  -  SERVICE DATES MUST BE IN ASCENDING ORDER               *
     H* 23  -  DIARY DATE REQUIRED                                    *
     H* 24  -  INVALID DIARY DATE                                     *
     H*****************************************************************
     FF0751F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFALTL1    UF   E           K DISK
     F*** BEGIN ADD *** 07/22/92 **************************************
     FFMSML     IF   E           K DISK
     F*** END ADD *** 07/22/92 ****************************************
     F*** BEGIN ADD *** 06/24/99 **************************************
     FFALTP     IF   E             DISK
     F                                     RENAME(PLT01:PLT02)
     F*** END   ADD *** 06/24/99 **************************************

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D*
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
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   KFLD                    SCSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   KFLD                    SCFND
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
     C                   KFLD                    SCSS#
     C*
     C*   KEY LIST FOR THE PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    SCFEIN
     C                   KFLD                    SCMOD
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     LTKEY         KLIST
     C                   KFLD                    UPCO#
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   KFLD                    SCSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCLTR#
     C*
     C*** BEGIN ADD *** 07/22/92 **************************************
     C*
     C*   KEY LIST FOR THE SUBSCRIBER
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   PARM                    QQRRN             9 0
     C*
     C* TRANSLATE THE QQCALL WHEN CALLED FROM F0399R.
     C*
     C                   SELECT
     C*
     C     QQCALL        WHENEQ    'D'
     C                   MOVEL(P)  '1'           QQCALL
     C                   SETON                                        16
     C*
     C     QQCALL        WHENEQ    'C'
     C                   MOVEL(P)  ' '           QQCALL
     C                   SETON                                        16
     C*
     C                   OTHER
     C                   SETOFF                                       16
     C*
     C                   ENDSL
     C*** END   ADD *** 06/24/99 **************************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     QQCALL        COMP      '1'                                    09    INQUIRY
     C     QQCALL        COMP      '2'                                    08    ANOTHER PGM
     C*
     C     TOP           TAG
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET COMPANY NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       SCCONM
     C                   ELSE
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*******            MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*** BEGIN ADD *** 06/24/99 **************************************
     C*
     C* DEFAULT THE SCREEN VALUES AS FOLLOWS.
     C*
     C*** END   ADD *** 06/24/99 **************************************
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPSUB#        SCSUB#
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCLTR#
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN24
     C*** BEGIN ADD *** 06/24/99 **************************************
     C*
     C* IF THIS PROGRAM IS CALLED FROM ANOTHER PROGRAM (F0399R)
     C* THEN THE RELATIVE RECORD NUMBER OF FALTP IS PASSED AND
     C* SCSUB#, SCFND, SCFYR, SCCASE AND SCLTR# WILL BE ASSIGNED
     C* VALUES FROM LTSUB#, LTFND, LTFYR, LTCASE AND LTLTR#.
     C* REPLACING THE DEFAULT SCREEN VALUES.
     C*
     C     Q1PRMS        IFGT      2
     C*
     C     QQRRN         CHAIN     PLT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     LTFND         SCFND
     C                   Z-ADD     LTSUB#        SCSUB#
     C                   Z-ADD     LTFYR         SCFYR
     C                   Z-ADD     LTCASE        SCCASE
     C                   Z-ADD     LTLTR#        SCLTR#
     C                   GOTO      SCR01T
     C                   MOVE      '1'           *IN24
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
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
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C******************************************************
     C*   COMMAND 03 = END OF JOB                          *
     C******************************************************
     C     *INKC         IFEQ      '1'
     C                   GOTO      TERM
     C                   END                                                    ENDIF *INKC
     C******************************************************
     C*   COMMAND 12 = PREVIOUS SCREEN                     *
     C******************************************************
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*** BEGIN ADD *** 06/24/99 **************************************
     C     Q1PRMS        IFGT      2
     C*
     C                   GOTO      TERM
     C*
     C                   ELSE
     C*** END   ADD *** 06/24/99 **************************************
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR02
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END                                                    ENDIF SCR03
     C                   END                                                    ENDIF *INKL
     C******************************************************
     C*   COMMAND 05 = RESTART                             *
     C******************************************************
     C     *INKE         IFEQ      '1'
     C                   Z-ADD     *ZEROS        SCFND
     C                   MOVE      UPFND         SCFND
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   MOVE      UPSUB#        SCSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCLTR#
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END                                                    ENDIF *INKE
     C**********************************
     C*   F6 = SUBSCRIBER # SEARCH     *
     C**********************************
     C     *INKF         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   CALL      'F0545R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCSUB#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCSUB#        OREQ      *ZERO
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      '1'           *IN31
     C                   GOTO      STRT
     C                   END                                                    END EQUAL B
     C*
     C     QQRTC         IFEQ      '*'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END SCR01
     C                   END                                                    END *INKF
     C**********************************
     C*   COMMAND 07 = FUND  SEARCH    *
     C**********************************
     C     *INKG         IFEQ      '1'
     C*
     C     SCSUB#        IFEQ      *ZEROS
     C                   MOVE      UPSUB#        SCSUB#
     C                   END
     C                   Z-ADD     SCSUB#        X1SUB#            5 0
     C                   Z-ADD     SCFND         X1FND             3 0
     C                   CALL      'F0508R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    X1SUB#
     C                   PARM                    X1FND
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     X1FND         OREQ      *ZERO
     C                   MOVE      '1'           *IN23                          POS CUR FUND
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN24                          POS CUR FYR
     C                   MOVE      X1SUB#        SCSUB#
     C                   MOVE      X1FND         SCFND             3 0
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    ENDIF *INKG
     C**********************************
     C*   COMMAND 08 = CASE # SEARCH   *
     C**********************************
     C     *INKH         IFEQ      '1'
     C     SCFND         IFEQ      *ZEROS
     C                   MOVE      UPFND         SCFND
     C                   END
     C                   Z-ADD     SCFND         X1FND             3 0
     C                   CALL      'F0570R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    XXSS#             9
     C                   PARM                    QQRTCD            1
     C                   PARM                    X1FND
     C                   MOVE      X1FND         SCFND
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     SCFYR         OREQ      *ZERO
     C                   MOVE      '1'           *IN24                          POS CUR FYR
     C                   GOTO      STRT
     C                   END                                                    END IF QQRTC
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      '1'           *IN25                          POS CUR LTR#
     C                   GOTO      STRT
     C                   END
     C*
     C                   END                                                    END IF *INKH
     C***************************************
     C*  COMMAND 09 - REQUEST LETTER SEARCH *
     C***************************************
     C     *INKI         IFEQ      '1'
     C*
     C                   MOVE      SCSUB#        QQSUB#            5
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      SCLTR#        QQLTR#            9
     C                   CALL      'F0775R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQSUB#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQLTR#
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTCD        IFEQ      'B'
     C     QQLTR#        OREQ      *ZERO
     C                   MOVE      '1'           *IN25                          POS CUR LTR#
     C                   GOTO      STRT
     C                   END                                                    ENDIF 'B'
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQSUB#        SCSUB#
     C                   MOVE      QQFND         SCFND
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQLTR#        SCLTR#
     C                   GOTO      SCR01T
     C                   END                                                    ENDIF '*'
     C*
     C                   END                                                    ENDIF *INKI
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
     C                   GOTO      TOP
     C                   END                                                    ENDIF *INKM
     C*
     C                   END                                                    END CMD KEYS
     C*****************************************************************
     C*   GOTO THE APPROPRIATE SCREENS
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*****************************************************************
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*** BEGIN ADD *** 07/22/92 **************************************
     C*
     C*  VALIDATE THE GROUP
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN30                          INVALID SUB#
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*** END ADD *** 07/22/92 ****************************************
     C*
     C*  VALIDATE THE GROUP
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20                          INVALID FUND
     C                   GOTO      STRT
     C                   END                                                    END IF *IN60
     C*
     C*   VALIDATE THE CASE
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21                          INVALID CASE
     C                   GOTO      STRT
     C                   END                                                    ENDIF *IN60
     C*
     C*   VALIDATE THE LETTER #
     C*
     C     LTKEY         CHAIN     PLT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22                          INVALID LTR#
     C                   GOTO      STRT
     C                   END
     C*
     C*   ALL EDITS HAVE BEEN PASSED, LOAD SCR02
     C*
     C                   MOVEL     LTSS#         SCSS#
     C                   Z-ADD     LTACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C                   Z-ADD     LTFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFSDT
     C                   Z-ADD     LTTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTSDT
     C                   Z-ADD     LTDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDIDT
     C                   Z-ADD     LTFEIN        SCFEIN
     C                   Z-ADD     LTMOD         SCMOD
     C                   Z-ADD     LTAMT         SCAMT
     C                   MOVEL     LTREAS        FL
     C                   MOVEL     LTBL24        SCBL24
     C                   MOVEL     LT24D2        SC24D2
     C                   MOVEL     LTOTH         SCOTH
     C                   MOVEL     LTOTD2        SCOTD2
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
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       scSS# = MaskData('SSN':'LTSS#':LTSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
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
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   PROCESS INFORMATION SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*   VALIDATE THE FROM DATE ENTERED
     C*
     C*** BEGIN ADD *** 06/24/99 **************************************
     C     *IN09         IFEQ      *OFF
     C*** END   ADD *** 06/24/99 **************************************
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
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRFRDC            6
     C                   Z-ADD     SCTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         SRTODC            6
     C*
     C                   CALL      'SRDDIF'
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
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   SCREEN 03 - REASON FOR LETTER
     C*
     C     SCR03T        TAG
     C*
     C*   ALL EDITS HAVE BEEN PASSED  - UPDATE THE FILE
     C*
     C     LTKEY         CHAIN     PLT01                              60
     C                   Z-ADD     SCFSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTFSDT
     C                   Z-ADD     SCTSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTTSDT
     C                   Z-ADD     SCAMT         LTAMT
     C                   Z-ADD     SCDIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDIDT
     C                   MOVEA     FL            LTREAS
     C                   MOVEL     SCBL24        LTBL24
     C                   MOVEL     SC24D2        LT24D2
     C                   MOVEL     SCOTH         LTOTH
     C                   MOVEL     SCOTD2        LTOTD2
     C                   MOVE      Q1USER        LTUSER
     C                   MOVE      Q1PGM         LTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LTTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LTDT
     C*
     C  N60              UPDATE    PLT01
     C*
     C                   Z-ADD     *ZEROS        SCFND
     C*** BEGIN ADD *** 07/22/92 **************************************
     C                   Z-ADD     *ZEROS        SCSUB#
     C*** END ADD *** 07/22/92 ****************************************
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCLTR#
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEGIN ADD *** 06/24/99 **************************************
     C     *IN16         IFEQ      *OFF
     C*** END   ADD *** 06/24/99 **************************************
     C                   GOTO      STRT
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
