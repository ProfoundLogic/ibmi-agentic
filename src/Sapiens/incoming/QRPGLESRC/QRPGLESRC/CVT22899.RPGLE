     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO CORRECT CLAIM FIELDS IN WDELP
     H*****************************************************************
     H* Description:  This pgm prints a list of employers with        *
     H*   discrepancies in the claim total fields.                    *
     H*                                                               *
     H*****************************************************************
     FWMCML     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20

     C**************************************************************************
     C*
     C*  Key list for the Claim file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Key list for the Pmt Detail file
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQUPD             1
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMFYR         SVFYR
     C     *LIKE         DEFINE    ELOUTS        XXOUTS
     C     *LIKE         DEFINE    ELTCLP        XXTCLP
     C     *LIKE         DEFINE    ELT#CL        XXT#CL
     C     *LIKE         DEFINE    ELTCLC        XXTCLC
     C                   MOVE      'N'           XXPRT             1
     C                   TIME                    DSDTTM
     C*
     C                   MOVE      QQCO#         XXCO#
     C  N60COKY          CHAIN     PCO01                              60
     C                   EXCEPT    HEAD
     C*-------------------------------------------------------------------------
     C*  Start of main loop
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C                   Z-ADD     CMFND         SVFND
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         SVDIV
     C                   Z-ADD     CMFYR         SVFYR
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     CMFND         IFNE      SVFND
     C     CMEMP#        ORNE      SVEMP#
     C     CMDIV         ORNE      SVDIV
     C     CMFYR         ORNE      SVFYR
     C                   EXSR      SRBRK
     C                   ENDIF
     C*
     C*  Add claim totals to employer totals
     C*
     C                   XFOOT     RR            XXRR              9 2
     C                   XFOOT     AR            XXAR              9 2
     C                   ADD       XXRR          XXOUTS
     C                   ADD       XXAR          XXTCLP
     C                   ADD       1             XXT#CL
     C     CMSTAT        IFEQ      'C'
     C                   ADD       1             XXTCLC
     C                   END
     C*
     C     CMKY          READE     PCM01                                  60
     C                   ENDDO
     C*
     C                   EXSR      SRBRK
     C                   SETON                                        LR
     C*-------------------------------------------------------------------------
     C     SRBRK         BEGSR
     C*
     C     ELKY          CHAIN     PEL01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     ELOUTS        IFNE      XXOUTS
     C     ELTCLP        ORNE      XXTCLP
     C     ELT#CL        ORNE      XXT#CL
     C     ELTCLC        ORNE      XXTCLC
     C                   MOVE      'Y'           XXPRT
     C                   ENDIF
     C*
     C     XXPRT         IFEQ      'Y'
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    DET
     C*
     C     QQUPD         IFEQ      'Y'
     C                   Z-ADD     XXOUTS        ELOUTS
     C                   Z-ADD     XXTCLP        ELTCLP
     C                   Z-ADD     XXT#CL        ELT#CL
     C                   Z-ADD     XXTCLC        ELTCLC
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMFND         SVFND
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         SVDIV
     C                   Z-ADD     CMFYR         SVFYR
     C                   MOVE      'N'           XXPRT
     C                   Z-ADD     0             XXOUTS
     C                   Z-ADD     0             XXTCLP
     C                   Z-ADD     0             XXT#CL
     C                   Z-ADD     0             XXTCLC
     C*
     C                   ENDSR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           78 'CHECK WDELP CLAIM FIELDS'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            3 'Grp'
     O                                           14 'Employer#'
     O                                           21 'Div'
     O                                           30 'Pol YR'
     O                                           48 'Outstanding Rsv'
     O                                           66 'Total Paid'
     O                                           81 'Total Claims'
     O                                           97 'Closed Claims'
     O          E            DET            1
     O                       SVFND         4      3
     O                       SVEMP#        4     14
     O                       SVDIV         4     21
     O                       SVFYR         4     28
     O                       ELOUTS        J     49
     O                       ELTCLP        J     67
     O                       ELT#CL        4     77
     O                       ELTCLC        4     93
     O                                          107 '*WDELP'
     O          E            DET            1
     O                       XXOUTS        J     49
     O                       XXTCLP        J     67
     O                       XXT#CL        4     77
     O                       XXTCLC        4     93
     O                                          107 '*WMCMP'
