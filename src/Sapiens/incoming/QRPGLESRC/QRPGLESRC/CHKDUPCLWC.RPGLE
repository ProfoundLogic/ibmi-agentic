     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKDUPCLM - PRINTS DUPLICATE CLAIMS IN FMCMP
    H*********************************************************************
    H* REVISIONS:                                                        *
    H*                                                                   *
     H*  02/03/15 C:300 PROJECT: SY246                                    *
     H*                 Masking of Social Security numbers to show        *
     H*                 as XXXXX9999 at all places                        *
    H*                                                                   *
     H*  08/29/14 C:300 PROJECT: SY246                                    *
     H*                 Masking of Social Security numbers to show        *
     H*                 as XXXXX9999                                      *
    H*                                                                   *
     H*  07/18/14 C:300 PROJECT: SY246                                    *
     H*                 Include Client# in both reports                   *
     H*                                                                   *
    H*********************************************************************
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

      *** Begin add ***  07/18/14  ***************************************************
     D xxCLT           s              3

     D RtvCltid        PR                  EXTPGM('RTVCLTID')
     D xxCLT                          3
      *** End add *****  07/18/14  ***************************************************
     C*
     C* LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    CMFND         SVFND
     C     *LIKE         DEFINE    CMFYR         SVFYR
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMSS#         SVSS#
     C     *LIKE         DEFINE    CMACDT        SVACDT
     C     *LIKE         DEFINE    CMIUSR        SVIUSR
     C     *LIKE         DEFINE    CMIPGM        SVIPGM
     C     *LIKE         DEFINE    CMIDT         SVIDT
     C     *LIKE         DEFINE    CMUSER        SVUSER
     C     *LIKE         DEFINE    CMPGM         SVPGM
     C     *LIKE         DEFINE    CMDT          SVDT
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   TIME                    DSDTTM
      *** Begin add ***  07/18/14  ***************************************************
      /Free
       RtvCltid(xxCLT);
      /End-Free
      *** End add *****  07/18/14  ***************************************************
     C*
     C* PRINT THE REPORT HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C* READ THE WCMS CLAIM MASTER FILE
     C*
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
      *** Begin add ***  02/03/15  ***************************************************
      /Free
       If CMSS# <> *Blanks;
          %subst(CMSS#:1:5) = 'XXXXX';
       Endif;
      /End-Free
      *** End add *****  02/03/15  ***************************************************
     C*
     C* CHECK FOR A DUPLICATE CLAIM
     C*
     C     CMFND         IFEQ      SVFND
     C     CMFYR         ANDEQ     SVFYR
     C     CMCASE        ANDEQ     SVCASE
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             COUNT
     C                   MOVE      '1'           *IN20
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C* LOAD THE SAVE FIELDS
     C*
     C                   Z-ADD     CMFND         SVFND
     C                   Z-ADD     CMFYR         SVFYR
     C                   Z-ADD     CMCASE        SVCASE
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         SVDIV
     C                   MOVE      CMSS#         SVSS#
      *** Begin add ***  08/29/14  ***************************************************
      *** Begin del ***  02/03/15  ***************************************************
      //  /Free
      //   %subst(SVSS#:1:5) = 'XXXXX';
      //  /End-Free
      *** End del *****  02/03/15  ***************************************************
      *** End add *****  08/29/14  ***************************************************
     C                   Z-ADD     CMACDT        SVACDT
     C                   MOVE      CMIUSR        SVIUSR
     C                   MOVE      CMIPGM        SVIPGM
     C                   Z-ADD     CMIDT         SVIDT
     C                   MOVE      CMUSER        SVUSER
     C                   MOVE      CMPGM         SVPGM
     C                   Z-ADD     CMDT          SVDT
     C*
     C                   READ      PCM01                                  50
     C                   ENDDO
     C*
     C* PRINT MESSAGE IF NO RECORDS FOR REPORT
     C*
     C     COUNT         IFEQ      0
     C                   EXCEPT    MSG
     C                   END                                                    END COUNT EQ 0
     C*
     C* PRINT THE TOTAL LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    TOTL
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           67 'THIS REPORT PRINTS DUPLI'
     O                                           88 'CATE CLAIMS IN WMCMP.'
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD           2
     O                                           10 'CHKDUPCLWC'
      *** Begin add ***  07/18/14  ***************************************************
     O                                           67 'CLTID -'
     O                       xxCLT               71
      *** End add *****  07/18/14  ***************************************************
     O                                          127 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                            4 'CO#'
     O                                            9 'FND'
     O                                           14 'FYR'
     O                                           22 'CASE'
     O                                           33 'EMPLOYER#'
     O                                           40 'DIV'
     O                                           51 'SOC SEC #'
     O                                           62 'ACC DATE'
     O                                           73 'INIT USER'
     O                                           84 'INIT PGM'
     O                                           97 'INIT DATE'
     O                                          110 'MAINT USER'
     O                                          121 'MAINT PGM'
     O                                          132 'MAINT DT'
     O          E   N20      DET         1  1
     O                       CMCO#         4      4
     O                       SVFND         4      9
     O                       SVFYR         4     14
     O                       SVCASE        4     22
     O                       SVEMP#        4     33
     O                       SVDIV         4     40
     O                       SVSS#               51
     O                       SVACDT              62 '   /  /  '
     O                       SVIUSR              74
     O                       SVIPGM              86
     O                       SVIDT               97 '   /  /  '
     O                       SVUSER             110
     O                       SVPGM              122
     O                       SVDT               132 '   /  /  '
     O          E            DET            1
     O                       CMCO#         4      4
     O                       CMFND         4      9
     O                       CMFYR         4     14
     O                       CMCASE        4     22
     O                       CMEMP#        4     33
     O                       CMDIV         4     40
     O                       CMSS#               51
     O                       CMACDT              62 '   /  /  '
     O                       CMIUSR              74
     O                       CMIPGM              86
     O                       CMIDT               97 '   /  /  '
     O                       CMUSER             110
     O                       CMPGM              122
     O                       CMDT               132 '   /  /  '
     O          E            TOTL        1  2
     O                                           27 'TOTAL NUMBER OF RECORDS:'
     O                       COUNT         B     35
     O          E            MSG            2
     O                                           27 'NO DUPLICATE CLAIMS EXIS'
     O                                           28 'T'
