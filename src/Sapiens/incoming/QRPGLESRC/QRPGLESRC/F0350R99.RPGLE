     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     FFMBEL     IF   E           K DISK

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
     C     PGMH          PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@RRN2
     C                   PARM                    @@NWCD                                     E
     C                   PARM                    @@CHRG
     C********             PARM           @@SCRN  1
     C*
     C     PGMD          PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN3
     C                   PARM                    @@RRN2
C:62 C                   PARM                    @@RRN4            9 0
     C                   PARM                    @@NWCD                                     E
     C********             PARM           @@SCRN  1
     C*
     C     PGMW          PLIST
     C                   PARM                    @@MODE
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@RRN2
C:62 C**********           PARM 'F0353R'  @@PGM  10
     C                   PARM                    @@PGM            10
C:62 C                   PARM      *BLANKS       @@PGM2           10
     C*
     C     *LIKE         DEFINE    BEHEPG        XXHEPG
     C     *LIKE         DEFINE    BEDEPG        XXDEPG
     C     *LIKE         DEFINE    BEDEPG        XXWWDE
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
     C                   MOVE      BEHEPG        XXHEPG
     C                   MOVE      BEDEPG        XXDEPG
     C                   MOVEL(P)  BEDEPG        @@PGM
     C                   MOVEL(P)  'F0390R'      XXWWDE
     C*
     C*** CALL HEADER PGM
     C*
     C     PGM1          TAG
     C*
     C                   CALL      XXHEPG        PGMH
     C********** *INKL     CABEQ*ON       EOJ
     C     @@MODE        IFNE      'D'
     C     *INKL         CABEQ     *ON           PGM1
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
     C                   CALL      XXDEPG        PGMD
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
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
