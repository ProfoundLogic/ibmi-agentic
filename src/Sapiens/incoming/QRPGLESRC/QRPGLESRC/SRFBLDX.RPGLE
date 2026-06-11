     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*  PROGRAM SUBROUTINE SRFBLDX- Clone of SRFBLD Parms Changed
     H*       External Application Folder Build Subprogram
     H*
     H*****************************************************************
     H* NOTICE:::::::
     H*  PROGRAM MUST BE PASSED THE AREA OF SERVICE.  (SRSER)
     H*  PROGRAM MUST BE PASSED THE REMARK TYPE.      (SRSUB)
     H*   IF AREA OF SERVICE IS BLANK, SRTERM IS RETURNED = '1'
     H*   IF SUB-AREA IS BLANK, SRTERM IS RETURNED = '2'
     H*   IF FOLDER COULD NOT BE CREATED, SRTERM IS RETURNED = '3'
     H*****************************************************************
     H*
     H*****************************************************************
     H*  THIS SUBPROGRAM WILL TAKE THE INCOMING PARMS OF FUND, AREA OF
     H*  SERVICE, SUB-AREA (COMMENT CODE), FOLDER (CXXXXXXX, AXXXXX,
     H*  MXXXXX) AND RETURN A COMPLETE FOLDER PATH.
     H*
     H*  FOLDER IS DETERMINED BY DARTS KEY.
     H*
     H*****************************************************************
     H*  PROGRAMMER:  LORI J. QUINN
     H*  DATE:        JUNE 26, 1991
     H*  CLIENT:      003
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/08/04 C:62 Project: SY100                                 *
     H*                Add additional logic to handle client folders  *
     H*                                                               *
     H*  09/13/00 C:128 PROJECT: 81WRK                                *
     H*                 Changed the Folder entry parm from 8 to 12    *
     H*                 to allow External Applications maximum folder *
     H*                 size for more meaningful names.               *
     H*
     H*  06/12/97 C:69 PROJECT: WC577                                 *
     H*                Rewrote program to automatically add necessary *
     H*                lower-level folders                            *
     H*****************************************************************
     H*

     C*===============================================================
     C*  Parameter lists
     C*===============================================================
     C     *ENTRY        PLIST
     C                   PARM                    SRFND             8            FUND
     C                   PARM                    SRSER             8            AREA OF SERVICE
     C                   PARM                    SRSUB             8            SUB-ARA(COM CD)
     C                   PARM                    SRFOLD           12            FOLDER
     C                   PARM                    SRFPTH           40            FULL FOLDER NAM
     C                   PARM                    SRTERM            1            SET LR FLAG.
      *
     C     AFPRMS        PLIST
     C                   PARM                    Q1INFL           63
     C                   PARM                    Q1FLR            12
     C                   PARM                    Q1RTC             1
     C*===============================================================
     C*  LIKE definitions
     C*===============================================================
     C     *LIKE         DEFINE    SRFPTH        WRKPTH
     C     *LIKE         DEFINE    SRFPTH        XXWPTH
     C*** BEGIN ADD *** 06/08/04 **************************************
     C*
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##SUF             3
     C                   PARM      *BLANKS       ##PFLR           80
     C*** END   ADD *** 06/08/04 **************************************
     C*
     C* SET-UP AND INITIALIZE
     C*
     C                   MOVE      *BLANKS       SRFPTH
     C                   MOVE      *BLANKS       SRTERM
     C*
     C* VALIDATE THAT AREA OF SERVICE AND SUB-AREA ARE <> *BLANKS
     C*
     C     SRSER         IFEQ      *BLANKS
     C                   MOVE      '1'           SRTERM
     C                   ELSE
     C*
     C     SRSUB         IFEQ      *BLANKS
     C                   MOVE      '2'           SRTERM
     C                   ELSE
     C*
     C                   EXSR      BLDALL
     C*
     C* VALIDATE IN-FOLDER EXISTS (INFLR=FUND/AREA OF SERV/SUB-AREA)
     C*
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         Q2INFL
     C                   CAT       WRKPTH:0      Q2INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  WRKPTH        Q2INFL           80
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVE      *BLANKS       Q2RTC             1
     C                   CALL      'SVALFLR'
     C                   PARM                    Q2INFL
     C                   PARM                    Q2RTC
     C     Q2RTC         IFNE      *BLANKS
     C     Q2RTC         IFEQ      '1'                                          Doesn't exist
     C                   EXSR      CRTPTH
     C                   ELSE
     C                   MOVE      '3'           SRTERM
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C* MOVE FULL FOLDER NAME TO OUT-GOING PARM
     C*
     C                   MOVE      WRKPTH        SRFPTH
     C*
     C                   MOVE      *ON           *INLR
     C*===============================================================
     C* BLDALL - Build entire path name for validation
     C*===============================================================
     C     BLDALL        BEGSR
     C                   MOVEL(P)  SRFND         WRKPTH
     C                   CAT       '/':0         WRKPTH
     C                   CAT       SRSER:0       WRKPTH
     C                   CAT       '/':0         WRKPTH
     C                   CAT       SRSUB:0       WRKPTH
     C     SRFOLD        IFNE      *BLANKS
     C                   CAT       '/':0         WRKPTH
     C                   CAT       SRFOLD:0      WRKPTH
     C                   ENDIF
     C                   ENDSR
     C*===============================================================
     C* CRTPTH - Create necessary pieces of path
     C*===============================================================
     C     CRTPTH        BEGSR
     C*
     C*  Create the fund folder if neccessary
     C*
     C                   MOVEL(P)  SRFND         Q1FLR
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C                   MOVEL(P)  ##PFLR        Q1INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  '*NONE'       Q1INFL
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVE      *BLANKS       Q1RTC
     C                   CALL      'SADDFLR'     AFPRMS
     C*
     C* IF RETURN CODE = 1 THEN FOLDER ALREADY EXISTED
     C* IF RETURN CODE IS ANYTHING OTHER THAN 1 OR *BLANKS THEN WE
     C*  COULD NOT CREATE THE FOLDER- RETURN TO CALLING PGM WITH
     C*  SRTERM = 3
     C*
     C     Q1RTC         IFNE      *BLANKS
     C     Q1RTC         ANDNE     '1'
     C                   MOVE      '3'           SRTERM
     C                   ELSE
     C*
     C*  Create the area of service folder if neccessary
     C*
     C                   MOVEL(P)  SRSER         Q1FLR
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         Q1INFL
     C                   CAT       SRFND:0       Q1INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  SRFND         Q1INFL
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVE      *BLANKS       Q1RTC
     C                   CALL      'SADDFLR'     AFPRMS
     C     Q1RTC         IFNE      *BLANKS
     C     Q1RTC         ANDNE     '1'
     C                   MOVE      '3'           SRTERM
     C                   ELSE
     C*
     C*  Create the sub-ara folder if neccessary
     C*
     C                   MOVEL(P)  SRFND         XXWPTH
     C                   CAT       '/':0         XXWPTH
     C                   CAT       SRSER:0       XXWPTH
     C                   MOVEL(P)  SRSUB         Q1FLR
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         Q1INFL
     C                   CAT       XXWPTH:0      Q1INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  XXWPTH        Q1INFL
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVE      *BLANKS       Q1RTC
     C                   CALL      'SADDFLR'     AFPRMS
     C     Q1RTC         IFNE      *BLANKS
     C     Q1RTC         ANDNE     '1'
     C                   MOVE      '3'           SRTERM
     C                   ELSE
     C*
     C*  Create the specific type folder if neccessary
     C*
     C                   CAT       '/':0         XXWPTH
     C                   CAT       SRSUB:0       XXWPTH
     C                   MOVEL(P)  SRFOLD        Q1FLR
     C*** BEGIN ADD *** 06/08/04 **************************************
     C     ##PFLR        IFNE      *BLANKS
     C     ##PFLR        CAT(P)    '/':0         Q1INFL
     C                   CAT       XXWPTH:0      Q1INFL
     C                   ELSE
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVEL(P)  XXWPTH        Q1INFL
     C*** BEGIN ADD *** 06/08/04 **************************************
     C                   ENDIF
     C*** END   ADD *** 06/08/04 **************************************
     C                   MOVE      *BLANKS       Q1RTC
     C                   CALL      'SADDFLR'     AFPRMS
     C     Q1RTC         IFNE      *BLANKS
     C     Q1RTC         ANDNE     '1'
     C                   MOVE      '3'           SRTERM
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
