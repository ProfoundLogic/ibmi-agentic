     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE SRLOGCLAS -Return Logging Class Multipliers
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*  Return Logging Class Multipliers                             *
     H*                                                               *
     H*  CALLED BY:                                                   *
     H*   W0660R1                                                     *
     H*   W7351R                                                      *
     H*   W7413R                                                      *
     H*   W7720R                                                      *
     H*   W8010R                                                      *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: RANDHIR SINGH                                    *
     H*  CREATED:    03/11/10          PROJECT: 102WFC                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     DCLASS            S              4  0 DIM(3) CTDATA PERRCD(1)
      *
     DPREFIX           S              1  0 DIM(3) CTDATA PERRCD(1)
     DPAYROLL          S              7  5 DIM(3) ALT(PREFIX)
      *
     DPYROLL           S              7  5 DIM(3) CTDATA PERRCD(1)
     DRATE             S              7  5 DIM(3) ALT(PYROLL)
      *
     DCLAS             S              4  0
     DChCLAS           S              5
     D##PRFX           S              1  0
     DQQCLAS           S              5  0
     DQQFOUND          S              1N
     DQQMPAYR          S              7  5
     DQQMRATE          S              7  5
     DQQRPTCLS         S              5  0
     C     *ENTRY        PLIST
     C                   PARM                    QQCLAS
     C                   PARM                    QQFOUND
     C                   PARM                    QQMPAYR
     C                   PARM                    QQMRATE
     C                   PARM                    QQRPTCLS
     C                   IF        QQCLAS <> *ZEROS
     C                   MOVE      QQCLAS        CHCLAS
     C                   MOVEL     CHCLAS        ##PRFX
     C                   MOVE      CHCLAS        CLAS
     C                   Z-ADD     1             I                 2 0
     C                   Z-ADD     1             J                 2 0
     C     CLAS          LOOKUP    CLASS(1)                               99
     C                   IF        %FOUND
     C     ##PRFX        LOOKUP    PREFIX(I)                              99
     C                   IF        %FOUND
     C                   MOVE      PAYROLL(I)    QQMPAYR
     C     QQMPAYR       LOOKUP    PYROLL(J)                              99
     C                   IF        %FOUND
     C                   MOVEA     RATE(J)       QQMRATE
     C                   EVAL      QQFOUND = *ON
     C                   ENDIF
     C                   ENDIF
     C                   EVAL      QQRPTCLS = %DEC(CLAS)
      *
     C                   ELSE
     C                   EVAL      QQFOUND = *OFF
     C                   ENDIF
     C                   ELSE
     C                   EVAL      QQFOUND = *OFF
     C                   ENDIF
      *
     C                   RETURN
** CLASS ******
2702
2705
2709
** PREFIX *******
10800000
20292920
30100000
** PYROLL*******
08000001250000
02929203375000
01000000024000
