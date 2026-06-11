     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0350R98 - CONTOLLING PGM FOR PBILLS
     H*****************************************************************
     H*  F0350R98 - CONTROLS PGM FLOW                                 *
     H*  Created: 09/21/99  C:117 Project: FL021                      *
     H*                                                               *
     H*****************************************************************
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  08/04/00 C:117 PROJECT MCMNT
     H*                 Added a PARM to call F0350C01 for the hold
     H*                 Total Charge Amt.
     H*
     H*****************************************************************
     FFMBEL     IF   E           K DISK
     FFAEOP     UF   E             DISK
     F* WCMC Case Lockout File
     FFXLOP     UF   E           K DISK

     D*
     D                 DS
     D  XXA101                 1     10
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C     *ENTRY        PLIST
     C     KKAST         PARM                    @@AST             2            ADJ STATE
     C     KKSYS         PARM                    @@SYS             1            SYSTEM
     C     KKBTYP        PARM                    @@BTYP            1            BILL TYPE
     C     KKSTYP        PARM                    @@STYP            3            BILL SUB TYPE
     C*
     C                   PARM      *INKC         @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@MODE            1            ADD CHG DIS
     C                   PARM                    @@RRN             9 0          for FAEHP
     C                   PARM                    @@RRN2            9 0          for FAEOP
     C                   PARM                    @@RRN3            9 0          for FDDHP
     C                   PARM                    @@NWCD            5            NETWORK CODEE
     C                   PARM                    @@CHRG            9            HELD TOTAL CHARGE
     C*
     C     BEKY          KLIST
     C                   KFLD                    KKAST             2
     C                   KFLD                    KKSYS             1
     C                   KFLD                    KKBTYP            1
     C                   KFLD                    KKSTYP            3
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     PGMH          PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@RRN2
     C*
     C     PGMD          PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN3
     C                   PARM                    @@RRN2
     C                   PARM                    @@RRN4            9 0          FDCAP
     C*
     C     PGMW          PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN4            9 0          FDCAP
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RRN3            9 0
     C                   PARM                    @@CNTL           20
     C                   PARM                    @@PGM            10
     C*
     C     *LIKE         DEFINE    BEEDHD        XXEDHD
     C     *LIKE         DEFINE    BEEDDT        XXEDDT
     C     *LIKE         DEFINE    BEEDDT        XXWWDE
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C*
     C                   MOVE      '0'           *INKC
     C                   MOVE      '0'           *INKL
     C*
     C*** CHAIN WITH STATE. IF NO HIT CHAIN WITHOUT FOR DEFAULT.
     C*
     C     BEKY          CHAIN     FMBEL                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       KKAST
     C     BEKY          CHAIN     FMBEL                              60
     C     *IN60         CABEQ     *ON           EOJ
     C                   ENDIF
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              60
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C                   MOVE      EOCNTL        @@CNTL
     C                   MOVE      BEEDHD        XXEDHD
     C                   MOVE      BEEDDT        XXEDDT
     C                   MOVEL(P)  BEEDDT        @@PGM
     C                   MOVEL(P)  'F0490R'      XXWWDE
     C*
     C*** CALL HEADER PGM
     C*
     C     PGM1          TAG
     C*
     C                   CALL      XXEDHD        PGMH
     C********** *INKL     CABEQ*ON       EOJ
     C     @@MODE        IFNE      'D'
     C     *INKL         CABEQ     *ON           EOJ
     C                   ENDIF
     C     @@MODE        IFEQ      'D'
     C     *INKL         CABEQ     *ON           EOJ
     C                   ENDIF
     C     *INKC         CABEQ     *ON           EOJ
     C*
     C*** ON CHG OR DSPLY CALL THE W/W DETAIL
     C*
     C     @@MODE        IFNE      'A'
     C                   CALL      XXWWDE        PGMW
     C     *INKL         CABEQ     *ON           PGM1
     C                   ENDIF
     C*
     C*** CALL DETAIL PGM
     C*
C:62 C     @@MODE        IFEQ      'A'
     C                   CALL      XXEDDT        PGMD
     C     *INKC         CABEQ     *ON           EOJ
     C     *INKL         CABEQ     *ON           PGM1
C:62 C                   ENDIF
     C*
     C*** ON ADD CALL THE W/W DETAIL
     C*
     C     @@MODE        IFEQ      'A'
     C                   CALL      XXWWDE        PGMW
     C     *INKL         CABEQ     *ON           PGM1
     C                   ENDIF
     C*
     C     EOJ           TAG
     C     @@RRN2        CHAIN     PEO01                              60
     C*** BEG ADD *** 08/02/00 ****************************************
     C                   MOVE      @@CHRG        EOCHRG
     C*** END ADD *** 08/02/00 ****************************************
     C                   MOVEL     'C'           EOBLST
     C                   UPDATE    PEO01
     C*
     C* CALL F0395R01 TO WRITE BILL STATUS HISTORY RECORD
     C*
     C*** BEG CHG *** 10/17/00 ****************************************
     C* ADD A CONDITION AROUND CALL
     C*
     C     *INKL         IFEQ      *OFF
     C     *INKC         ANDEQ     *OFF
     C                   Z-ADD     0             ##RRN             9 0
     C                   Z-ADD     @@RRN2        ##RRN2            9 0          faeop
     C                   MOVE      EOBLST        ##BLST            1
     C                   MOVE      *BLANKS       ##PGM             8
     C                   CALL      'F0395R01'
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      'H'           ##MODE            1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##BLST
     C                   PARM                    ##STCD            3
     C                   PARM                    ##REAS           60
     C                   PARM                    ##PGM
     C                   PARM      0             #1LTR#            9 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             ##RRN             9 0
     C                   Z-ADD     @@RRN2        ##RRN2            9 0          faeop
     C                   MOVE      'B'           ##BLST            1
     C                   MOVE      *BLANKS       ##PGM             8
     C                   CALL      'F0395R01'
     C     *INKC         PARM      ' '           ##KC              1
     C     *INKL         PARM      ' '           ##KL              1
     C                   PARM      'F'           ##MODE            1
     C     ##RRN         PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   PARM                    ##BLST
     C                   PARM                    ##STCD            3
     C                   PARM                    ##REAS           60
     C                   PARM                    ##PGM
     C                   PARM      0             #1LTR#            9 0
     C*
     C                   ENDIF
     C*** END CHG *** 10/17/00 ****************************************
     C                   MOVE      *ON           *INLR
     C*
     C* REMOVE RECORD FROM LOCKOUT FILE
     C*
     C     LOKY          CHAIN     PLO01                              99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    PLO01
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   RETURN
     C*
