     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F7T95R - HUNTS FOR CASES THAT NEED TO BE ASSIGNED FYR
     H*****************************************************************
     H** REVISIONS:
     H*
     H*****************************************************************
     FWMCML1    IF   E           K DISK
     FFMCMLT    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     IPCM01
     I              CMOSCL                      C1OSCL
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FFYR            3
     C                   PARM                    @@TFYR            3
     C*
     C     CMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKWCF#
     C*
     C     CMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCASE
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMFYR         HLDFYR
     C     *LIKE         DEFINE    CMFYR         MCFYR
     C     *LIKE         DEFINE    CMFYR         XXFFYR
     C     *LIKE         DEFINE    CMFYR         XXTFYR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C                   SETOFF                                       1020
     C     Q1PRMS        IFGT      *ZEROS
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@FFYR        XXFFYR
     C                   MOVE      @@TFYR        XXTFYR
     C     CMKY2         CHAIN     PCM02                              99
     C     *IN99         DOWEQ     *OFF
     C                   SELECT
     C     CMFYR         WHENEQ    XXFFYR
     C                   SETON                                        10
     C     CMFYR         WHENEQ    XXTFYR
     C                   SETON                                        20
     C                   ENDSL
     C     CMKY2         READE     PCM02                                  99
     C                   ENDDO
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVEL     ' '           ##FLAG
     C     *IN20         IFEQ      *OFF
     C                   MOVEL     'X'           ##FLAG
     C                   ENDIF
     C*
     C                   CALL      'F7T96R'
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FND             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@FFYR            3
     C                   PARM                    @@TFYR            3
     C                   PARM                    ##FLAG            1
     C                   ENDIF
     C                   SETOFF                                       1020
     C*
     C                   ELSE
     C                   Z-ADD     57            KKCO#
     C                   Z-ADD     58            KKWCF#
     C     CMKY1         CHAIN     PCM01                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     57            KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      CMFYR         HLDFYR
     C                   MOVE      *ZEROS        MCFYR
     C                   SETOFF                                       102030
     C     CMKY2         CHAIN     PCM02                              99
     C     *IN99         DOWEQ     *OFF
     C                   SELECT
     C     CMFYR         WHENNE    HLDFYR
     C                   SETON                                        10
     C                   MOVE      CMFYR         MCFYR
     C     CMFYR         WHENEQ    HLDFYR
     C                   SETON                                        20
     C                   ENDSL
     C     CMKY2         READE     PCM02                                  99
     C                   ENDDO
     C*
     C                   Z-ADD     CMFND         KKSUB#
     C     CMKY2         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        30
     C                   ENDIF
     C*
     C                   MOVEL     ' '           ##FLAG
     C     *IN10         IFEQ      *ON
     C                   MOVE      MCFYR         ##FFYR
     C     *IN20         IFEQ      *OFF
     C                   MOVEL     'X'           ##FLAG
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'X'           ##FLAG
     C                   MOVE      HLDFYR        ##FFYR
     C                   ENDIF
     C     *IN30         IFEQ      *ON
     C                   MOVEL     ' '           ##FLAG
     C                   ENDIF
     C*
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMSUB#        ##SUB#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      HLDFYR        ##TFYR
     C*
     C                   CALL      'F7S96R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##SUB#            5
     C                   PARM                    ##FND             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##FFYR            3
     C                   PARM                    ##TFYR            3
     C                   PARM                    ##FLAG            1
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO
     C                   ENDIF
     C*
     C                   SETON                                        LR
