     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H*  F8042R - Upload Bills - Transfer Program 2                   *
     H*****************************************************************
     H*                                                               *
     H*  Called from: F8041R  - Upload Bills - Transfer Pgm 1         *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/2004 by c:62 for project 51MFN              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  04/23/07 C:62 PROJECT: 139MHG                                *
     H*                ADD MCMC PROCESSING.                           *
     H*                                                               *
     H*  12/12/06 C:62 PROJECT: 144MAB                                *
     H*                ADD TML PROCESSING.                            *
     H*                                                               *
     H*  07/14/06 C:79 PROJECT: MC279                                 *
     H*                USE 4 NEW EXPANDED EOB CODE/NOTE FIELDS        *
     H*                                                               *
     H*  06/30/06 C:62 PROJECT 132MFO                                 *
     H*                ADD ADDITIONAL EDIT TO CHECK DATE AGAINST      *
     H*                LINE ITEMS.                                    *
     H*                                                               *
     H*  01/24/06 C:79 PROJECT 132MDN                                 *
     H*                CHECK FOR GROUP PROVIDERS - MAKE THIS BASE     *
     H*                                                               *
     H*  08/15/05 C:62 PROJECT 132MDN                                 *
     H*                ADD CLT132 SPECIFIC CHANGES                    *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FFAU2P     IF   E           K DISK
     FFAEAL37   IF   E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     F*
      /COPY SCOPYWW01D
     D CLM             S              1    DIM(20)
     D*
     D***PY SCOPYWW01I
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D REAS1           C                   CONST('Completed by -
     D                                     bill upload')
     D REAS2           C                   CONST('Reconsidered by -
     D                                     bill upload')
     D XXNOT1          C                   CONST('NETWORK DISCOUNT AP-
     D                                     PLIED')
     D CODEDS          C                   CONST('MEDICINE DISPENSED -
     D                                     BY PHYSICIA')
     D TRANS           C                   CONST('Transaction ID:')
     D DIGITS          C                   CONST('0123456789')
     D USERID          C                   CONST('PDSPGMR   ')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  CLAIM#                 1     20
     D  GROUP                  1      3
     D  YEAR                   4      6
     D  CASE                   7     13
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3    INZ('000')
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11    INZ('000')
     D*
     D                 DS
     D  XXDXR                  1      4  0
     D  DX1                    1      1  0
     D  DX2                    2      2  0
     D  DX3                    3      3  0
     D  DX4                    4      4  0
     D                 DS
     D  DATA                   1     50
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D*
     D                 DS
     D  VALUE                  1     11
     D  SIGN                   1      1
     D  AMOUNT                 2     11  2
     D                 DS
     D  CHRG1                  1     11
     D  CHRG2                  2     11  2
     D                 DS
     D  MRA1                   1     11
     D  MRA2                   2     11  2
     D                 DS
     D  NDIS1                  1     11
     D  NDIS2                  2     11  2
     D                 DS
     D  PAID1                  1     11
     D  PAID2                  2     11  2

     I*================================================================
     I*  /COPY required constants
     I*================================================================

     C*================================================================
     C*  PLIST definitions
     C*================================================================
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
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     DEKY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    U2DSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    U2DSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSINV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EASINV        KKSINV
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        HDSEQ#
     C*
     C*****************************************************************
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   READ      PU201                                  98
     C     *IN98         DOWEQ     *OFF
     C*
     C     U2PRFL        IFEQ      'Q'
     C                   EXSR      WRTDEP
     C                   ENDIF
     C*
     C                   READ      PU201                                  98
     C                   ENDDO
     C*
     C                   EXSR      PGMEND
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDE1 - Write FDDEP record
     C*================================================================
     C     WRTDE1        BEGSR
     C*
     C                   CLEAR                   PDE01
     C                   Z-ADD     EACO#         DECO#
     C                   Z-ADD     EASUB#        DESUB#
     C                   Z-ADD     EAFND         DEFND
     C                   Z-ADD     EAFYR         DEFYR
     C                   Z-ADD     EACASE        DECASE
     C                   MOVEL     EASS#         DESS#
     C                   Z-ADD     EAACDT        DEACDT
     C                   Z-ADD     EAEOB#        DEEOB#
     C                   Z-ADD     U2DSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C                   MOVEL(P)  XXEOB         DEEOB
     C                   MOVEL(P)  XXEODA        DENOT1
     C                   MOVEL(P)  XXEODB        DENOT2
     C     1             SUBST     DENOT2:1      XXPOS1            1
     C     XXPOS1        IFEQ      *BLANK
     C     69            SUBST(P)  DENOT2:2      DENOT2
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  EAIUSR        DEUSER
     C                   MOVEL(P)  EAIPGM        DEPGM
     C                   Z-ADD     EAITIM        DETIME
     C                   Z-ADD     EAIDT         DEDT
     C                   MOVEL(P)  DEUSER        DEIUSR
     C                   MOVEL(P)  DEPGM         DEIPGM
     C                   MOVE      'Q'           DEIPGM
     C                   Z-ADD     DEITIM        DEITIM
     C                   Z-ADD     DEIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EARCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDEP - Write FDDEP record
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   Z-ADD     777           ##CO#
     C                   MOVEL(P)  U2TRID        KKSINV
     C     EAKY          CHAIN     PEA01                              99
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C* This code checks for EOB Codes/Notes in the expanded fields
     C*
     C                   SETOFF                                       71
     C     U2EOB1        IFNE      *BLANKS
     C     U2EOD1        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB1        XXEOB             3
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD1:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD1:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD1:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB2        IFNE      *BLANKS
     C     U2EOD2        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB2        XXEOB
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD2:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD2:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD2:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB3        IFNE      *BLANKS
     C     U2EOD3        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB3        XXEOB
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD3:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD3:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD3:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C     U2EOB4        IFNE      *BLANKS
     C     U2EOD4        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB4        XXEOB
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD4:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD4:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD4:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C                   ENDSR
