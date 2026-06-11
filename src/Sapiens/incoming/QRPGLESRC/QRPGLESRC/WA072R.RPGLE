     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* API - WA072R - FIND NEXT WORKING DAY DATE                     *
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM: WA072R                                              *
     H*  CREATED: 05/01/09  C:220 PROJECT: WC1766                     *
     H*  DESCRIPTION: DETERMINE IF A DAY IS WORKING DAY               *
     H*                                                               *
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*  Mode 1:                                                      *
     H*  It will validate a date passed in date to edit against       *
     H*  weekend dates so they cannot be entered.                     *
     H*  Mode 2:                                                      *
     H*  It will calculate the next available workday for the check   *
     H*  mailing date                                                 *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     F*
     FWTCBL     IF   E           K DISK
     F*

     C*****************************************************************
     C*    Key list / Pram list  definations                          *
     C*****************************************************************
     C*
     C     KCB01         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXDATE            7 0
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@DCO#            3
     C                   PARM                    @@DTI             8
     C                   PARM                    @@SRD             5
     C                   PARM                    @@MOD             1
     C                   PARM                    @@DTO             8
     C                   PARM                    @@RTCD            1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*********************************************************************
     C* Initializing the variables
     C*********************************************************************
     C*
     C                   MOVE      *BLANK        @@RTCD
     C                   MOVE      *BLANK        HLDFLG
     C*
     C                   MOVE      @@DCO#        XXCO#
     C*
     C                   MOVEL     @@DTI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVEL     @@SRD         SRDAY             5 0
     C                   MOVEL     @@DTI         SRDATI            8 0
     C                   Z-ADD     SRDATI        SRDATE            8 0
     C*
     C********************************************************************
     C*                       Main Line Code                             *
     C*      SELECT MODE OF THE CALL OF API
     C*      1). RETURNS FLAG FOR THE WEEKDAY DATE
     C*      2). RETURNS DATE FOR THE NEXT WORKING DAY
     C*
     C********************************************************************
     C*
     C                   SELECT
     C     @@MOD         WHENEQ    '1'
     C                   EXSR      WKDAT
     C*
     C     @@MOD         WHENEQ    '2'
     C*
     C*  Find future date if increment is defined for mailing date
     C*
     C     SRDAY         IFGT      0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAY
     C                   PARM                    SRERR
     C                   ENDIF                                                  SRDAY>0
     C*
     C                   EXSR      SRNXDT
     C*
     C                   ENDSL
     C*
     C                   SETON                                        LR
     C*
     C**************************************************************************
     C* SRNXDT - Calculate next working date after no. of days from current date
     C*          Input/Output: SRDATE in format (MMDDYYYY)
     C*                        recieve date value and return next working day
     C**************************************************************************
     C*
     C     SRNXDT        BEGSR
     C*
     C* Determine if date is weekend date or not
     C*
     C                   EXSR      WKDAT
     C*
     C* If SRDAYS > 0 then add to get next working date
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C                   MOVE      SRDATE        @@DTO
     C*
     C                   ENDSR                                                  SRNXDT
     C*
     C     WKDAT         BEGSR
     C*
     C* Calculated day should not be weekend
     C* If date is saturday add 2, if sunday add 1
     C* Take base date as known saturday
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   Z-ADD     08011998      SRFRDT            8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDT            8 0
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C     SRDAYS        DIV       7             XXDAYS            5 0
     C                   MVR                     XXDAYS
     C* XXDAYS: 0=Sat, 1=Sun, 2=Mon, 3=Tue, 4=Wed, 5=Thu, 6=Fri
     C                   SELECT
     C     XXDAYS        WHENEQ    0
     C* its Saturday, should return monday
     C                   Z-ADD     2             SRDAYS
     C                   MOVEL     '0'           @@RTCD
     C* Add logic for the holiday check
     C     @@MOD         IFEQ      '2'
     C                   EXSR      HDNXT
     C                   ENDIF
     C     XXDAYS        WHENEQ    1
     C* its Sunday, should return monday
     C                   Z-ADD     1             SRDAYS
     C                   MOVEL     '0'           @@RTCD
     C* Add logic for the holiday check
     C     @@MOD         IFEQ      '2'
     C                   EXSR      HDNXT
     C                   ENDIF
     C                   OTHER
     C* other weekday, return next day
     C                   Z-ADD     0             SRDAYS
     C                   MOVEL     '1'           @@RTCD
     C* Add logic for the holiday check
     C     @@MOD         IFEQ      '2'
     C                   EXSR      HDNXT
     C                   ENDIF
     C*
     C                   ENDSL                                                  XXDAYS=6
     C*
     C                   ENDSR                                                  WKDAT
     C*
     C**************************************************************************
     C*** Check next day if holidy or not
     C**************************************************************************
     C*
     C     HDNXT         BEGSR
     C*
     C     HLDFLG        DOUEQ     *OFF                                         CHECK FOR NEXT DATE
     C*
     C* If SRDAYS > 0 then add to get next no weekend date
     C*
     C     SRDAY1        IFGT      0
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAY1
     C                   PARM                    SRERR
     C                   Z-ADD     0             SRDAY1
     C                   ENDIF                                                  SRDAY1>0
     C*
     C* Check SRDATE is holidy or not until gets a working day
     C*
     C                   MOVEL     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C     KCB01         CHAIN     PCB01                              60
     C*                                                    IS WORKING DAY
     C     *IN60         IFEQ      '0'
     C                   ADD       1             SRDAY1
     C*
     C                   ELSE
     C*
     C* Calculated day should not be weekend
     C* If date is saturday add 2, if sunday add 1
     C* Take base date as known saturday.
     C                   Z-ADD     0             SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   Z-ADD     08011998      SRFRDT            8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    SRFRDT            8 0
     C                   PARM                    SRDATE
     C                   PARM                    SRDAY1
     C                   PARM                    SRERR
     C*
     C     SRDAY1        DIV       7             XXDAYS            5 0
     C                   MVR                     XXDAYS
     C* XXDAYS: 0=Sat, 1=Sun, 2=Mon, 3=Tue, 4=Wed, 5=Thu, 6=Fri
     C                   SELECT
     C     XXDAYS        WHENEQ    0
     C* its Saturday, should return monday
     C                   Z-ADD     2             SRDAY1            5 0
     C                   MOVEL     '0'           @@RTCD
     C* Add logic for the holiday check
     C     XXDAYS        WHENEQ    1
     C* its Sunday, should return monday
     C                   Z-ADD     1             SRDAY1
     C                   MOVEL     '0'           @@RTCD
     C*
     C                   OTHER
     C*
     C                   MOVEL     '1'           @@RTCD
     C*
     C                   ENDSL                                                  XXDAYS=6
     C*
     C     @@RTCD        IFEQ      '1'
     C*
     C                   MOVE      *OFF          HLDFLG            1
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  CHECK FOR NEXT DATE
     C*
     C                   ENDSR                                                  HDNXT
     C*
