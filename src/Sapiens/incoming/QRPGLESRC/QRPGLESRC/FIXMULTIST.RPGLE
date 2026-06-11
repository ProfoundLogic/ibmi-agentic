     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIXWDCRP - CORRECT BANK ACCOUNT NUMBER
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS EMPLOYERS WITH MULISTATE LOCATIONS  **
     H**   CALLED FROM:                                              **
     H**     Command Line                                            **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H**   PROGRAMMER: C:09                                          **
     H**   CREATED:   02/11/99                                       **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     FWDELL5    IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMRSL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D* ---------------------------------------------------------------
     D* EXTERNAL SYSTEM INFORMATION DATA STRUCTURE
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ---------------------------------------------------------------
     D* SYSTEM DATE AND TIME
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     IPEL01
     I              ELCO#                       E1CO#
     I              ELFND                       E1FND
     I              ELFYR                       E1FYR
     I              ELEMP#                      E1EMP#
     I              ELDIV                       E1DIV
     I              ELRTCD                      E1RTCD
     IPEL02
     I              ELCO#                       E2CO#
     I              ELFND                       E2FND
     I              ELFYR                       E2FYR
     I              ELEMP#                      E2EMP#
     I              ELDIV                       E2DIV
     I              ELRTCD                      E2RTCD

     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    E2CO#
     C                   KFLD                    E2RTCD
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    E1CO#
     C                   KFLD                    E1FND
     C                   KFLD                    E1EMP#
     C*
     C*
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    RSJRST        XXSVST
     C*
     C*          GET TIME AND PRINT HEADING
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C                   EXCEPT    HDR1
     C                   Z-ADD     *ZEROS        XXCNT             5 0
     C                   Z-ADD     *ZEROS        XXCNT2            5 0
     C*
     C*          Read all records from WDELP
     C*          ------------------------------------------------------
     C                   READ      PEL01                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C     ELKY          CHAIN     PEL02                              60
     C     *IN60         IFEQ      '0'
     C     RSKY          CHAIN     PRS01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     RSJRST        XXSVST
     C                   ELSE
     C                   MOVE      *BLANKS       XXSVST
     C                   ENDIF
     C                   MOVE      'N'           XXOTHR            1
     C     ELKY          READE     PEL02                                  60
     C     *IN60         DOWEQ     '0'
     C     XXOTHR        ANDEQ     'N'
     C     RSKY          CHAIN     PRS01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     RSJRST        XXST
     C                   ELSE
     C                   MOVE      *BLANKS       XXST
     C                   ENDIF
     C     XXST          IFNE      XXSVST
     C     XXST          ANDNE     *BLANKS
     C     XXSVST        ANDNE     *BLANKS
     C                   MOVE      'Y'           XXOTHR
     C                   END
     C     ELKY          READE     PEL02                                  60
     C                   ENDDO
     C*
     C     XXOTHR        IFEQ      'Y'
     C                   ADD       1             XXCNT
     C     ELKY          CHAIN     PEL02                              60
     C     *IN60         DOWEQ     '0'
     C     RSKY          CHAIN     PRS01                              70
     C     *IN70         IFEQ      '0'
     C                   MOVEL     RSJRST        XXST
     C                   ELSE
     C                   MOVE      *BLANKS       XXST
     C                   ENDIF
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HDR1
     C                   MOVE      '0'           *INOA
     C                   END
     C                   ADD       1             XXCNT2
     C                   EXCEPT    DTL1
     C     ELKY          READE     PEL02                                  60
     C                   ENDDO
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HDR1
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DTL2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ELKY          SETGT     PEL01
     C                   READ      PEL01                                  50
     C                   ENDDO
     C*          End of Program
     C*          ------------------------------------------------------
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HDR1
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TTL
     C                   EXCEPT    TTL2
     C                   SETON                                        LR
      /EJECT
     O*------------------------------------------------------
     O* PRINTER AUDIT FILE
     O*------------------------------------------------------
     O*
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                                           41 'LIST MULTISTATE'
     O                                           50 'POLICIES'
     O                       UDATE         Y     80
     O*
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       PAGE          Z     80
     O                                           76 'PAGE'
     O          E            HDR1           1
     O                                           10 'CO#'
     O                                           20 'FND'
     O                                           30 'FYR'
     O                                           45 'EMPLOYER'
     O                                           55 'DIV'
     O                                           65 'ST'
     O          E            HDR1           1
     O                                           10 '---'
     O                                           20 '---'
     O                                           30 '---'
     O                                           45 '--------'
     O                                           55 '---'
     O                                           65 '--'
     O          E            DTL1           1
     O                       E2CO#         4     10
     O                       E2FND         4     20
     O                       E2FYR         4     30
     O                       E2EMP#        4     45
     O                       E2DIV         4     55
     O                       XXST                65
     O          E            DTL2           3
     O          E            TTL            2
     O                                           20 '# OF POLICIES:'
     O                       XXCNT         4     30
     O          E            TTL2           2
     O                                           20 '# OF LOCATIONS:'
     O                       XXCNT2        4     30
