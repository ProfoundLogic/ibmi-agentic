     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F9718RFL
     H*    DESCRITION: GENERATES A REPORT OF THE DUPLICATE CONTROL
     H*                NUMBERS IN FEPPFL, FEHPFL, FEDPFL AND FEXPFL
     H*
     H*    CALLED FROM: F9718CFL - FL EDI-CHECK DUPLICATES - PHYSICIAN
     H*                 F9719CFL - FL EDI-CHECK DUPLICATES - HOSPITAL
     H*                 F9720CFL - FL EDI-CHECK DUPLICATES - DENTAL
     H*                 F9721CFL - FL EDI-CHECK DUPLICATES - PHARMACY
     H*    CALLS TO   : NONE
     H*
     H*****************************************************************
     H* CREATED ON 06/03/2004 BY C:62 FOR PROJECT FL046
     H*****************************************************************
     H* REVISIONS:
     H*
     H*****************************************************************
     FFEPLXFL   IF   E           K DISK
     F                                     RENAME(FEPPX01:FAT01)
     FFEHLXFL   IF   E           K DISK
     F                                     RENAME(FEHPX01:FAT02)
     FFEDLXFL   IF   E           K DISK
     F                                     RENAME(FEDPX01:FAT03)
     FFEXLXFL   IF   E           K DISK
     F                                     RENAME(FEXPX01:FAT04)
     F*
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)

     C*
     C*===============================================================*
     C*  PARMs                                                        *
     C*===============================================================*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@PROC            1
     C*
     C*===============================================================*
     C*  KFLDs                                                        *
     C*===============================================================*
     C     FATKY         KLIST
     C                   KFLD                    KKCNTL           13
     C*===============================================================*
     C*  *LIKE DEFN Statements                                        *
     C*===============================================================*
     C     *LIKE         DEFINE    CNTL#         H1CNTL
     C     *LIKE         DEFINE    CNTL#         H2CNTL
     C*===============================================================*
     C*  MAIN LOGIC                                                   *
     C*===============================================================*
     C*
     C*  Initialize fields and write headings
     C*
     C                   MOVE      *LOVAL        KKCNTL
     C                   Z-ADD     0             COUNT             4 0
     C                   EXCEPT    HEADS
     C*
     C                   SELECT
     C*
     C     @@PROC        WHENEQ    'P'
     C     FATKY         SETLL     FAT01
     C                   READ      FAT01                                  99
     C                   MOVE      CNTL#         H1CNTL
     C                   READ      FAT01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     CNTL#         H2CNTL
     C     H1CNTL        IFEQ      H2CNTL
     C*
     C* Write duplicate control number to output file
     C*
     C                   MOVEL     H1CNTL        CNTL             13
     C                   EXCEPT    DETAIL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ADD       1             COUNT
     C                   ELSE                                                   H1CNTL=H2CNTL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ENDIF                                                  H1CNTL=H2CNTL
     C*
     C                   READ      FAT01                                  99
     C                   ENDDO                                                  *IN99=*ON
     C*
     C     @@PROC        WHENEQ    'H'
     C     FATKY         SETLL     FAT02
     C                   READ      FAT02                                  99
     C*
     C                   MOVE      CNTL#         H1CNTL
     C*
     C                   READ      FAT02                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     CNTL#         H2CNTL
     C     H1CNTL        IFEQ      H2CNTL
     C*
     C* Write duplicate control number to output file
     C*
     C                   MOVEL     H1CNTL        CNTL             13
     C                   EXCEPT    DETAIL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ADD       1             COUNT
     C                   ELSE                                                   H1CNTL=H2CNTL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ENDIF                                                  H1CNTL=H2CNTL
     C*
     C                   READ      FAT02                                  99
     C*
     C                   ENDDO                                                  *IN99=*ON
     C*
     C     @@PROC        WHENEQ    'D'
     C     FATKY         SETLL     FAT03
     C                   READ      FAT03                                  99
     C*
     C                   MOVE      CNTL#         H1CNTL
     C*
     C                   READ      FAT03                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     CNTL#         H2CNTL
     C     H1CNTL        IFEQ      H2CNTL
     C*
     C* Write duplicate control number to output file
     C*
     C                   MOVEL     H1CNTL        CNTL             13
     C                   EXCEPT    DETAIL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ADD       1             COUNT
     C                   ELSE                                                   H1CNTL=H2CNTL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ENDIF                                                  H1CNTL=H2CNTL
     C*
     C                   READ      FAT03                                  99
     C*
     C                   ENDDO                                                  *IN99=*ON
     C*
     C     @@PROC        WHENEQ    'X'
     C     FATKY         SETLL     FAT04
     C                   READ      FAT04                                  99
     C*
     C                   MOVE      CNTL#         H1CNTL
     C*
     C                   READ      FAT04                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   MOVEL     CNTL#         H2CNTL
     C     H1CNTL        IFEQ      H2CNTL
     C*
     C* Write duplicate control number to output file
     C*
     C                   MOVEL     H1CNTL        CNTL             13
     C                   EXCEPT    DETAIL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ADD       1             COUNT
     C                   ELSE                                                   H1CNTL=H2CNTL
     C                   MOVEL     H2CNTL        H1CNTL
     C                   ENDIF                                                  H1CNTL=H2CNTL
     C*
     C                   READ      FAT04                                  99
     C*
     C                   ENDDO                                                  *IN99=*ON
     C                   ENDSL
     C*
     C*  Write Summary
     C*
     C                   EXCEPT    FOOT
     C*
     C*  END PGM
     C*
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            HEADS          1 06
     O                                           17 'DUPLICATE CONTROL'
     O          E            HEADS          2
     O                                           12 'NUMBERS'
     O          E            HEADS          2
     O                                           12 'CONTROL #'
     O          E            DETAIL         1
     O                       CNTL                14
     O          E            FOOT        2
     O                                            9 'COUNT = '
     O                       COUNT               13
