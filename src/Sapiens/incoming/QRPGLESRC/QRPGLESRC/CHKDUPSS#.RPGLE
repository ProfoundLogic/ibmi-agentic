     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKDUPSS# - PRINTS DUPLICATE SS# IN WMCDP
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/09/20  C:295 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*****************************************************************
     h*                                                               *
     FWMCDL     IF   E           K DISK
     FQSYSPRT   O    F  140        PRINTER OFLIND(*INOA)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************
     C*
     C* LIKE DEFINE SECTION
     C*
     C     *LIKE         DEFINE    CDSS#         SVSS#
     C     *LIKE         DEFINE    CDLSN         SVLSN
     C     *LIKE         DEFINE    CDFSN         SVFSN
     C     *LIKE         DEFINE    CDIUSR        SVIUSR
     C     *LIKE         DEFINE    CDIPGM        SVIPGM
     C     *LIKE         DEFINE    CDIDT         SVIDT
     C     *LIKE         DEFINE    CDUSER        SVUSER
     C     *LIKE         DEFINE    CDPGM         SVPGM
     C     *LIKE         DEFINE    CDDT          SVDT
     C*
     C                   Z-ADD     *ZEROS        COUNT             5 0
     C                   TIME                    DSDTTM
     C                   CLEAR                   SVSS#
     C*
     C* PRINT THE REPORT HEADING
     C*
     C                   EXCEPT    HEAD
     C*
     C     *IN50         DOUEQ     *ON
     C*
     C* READ THE CLAIMANT DEMOGRAPHICS FILE
     C*
     C                   READ      PCD01                                  50
     C*
     C* CHECK FOR A DUPLICATE CLAIM
     C*
     C     CDSS#         IFEQ      SVSS#
     C*
     C*** Beg Add ********* 06/09/20 ******************************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CDSS#'       QQNAME           25
     C     CDSS#         PARM      CDSS#         QQVAL           100
     C                   MOVEL(P)  CDSS#         SVSS#
     C*** End Add ********* 06/09/20 ******************************************
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             COUNT
     C                   MOVE      *ON           *IN20
     C*
     C                   ELSE
     C*
     C   20              EXCEPT    DET1
     C                   MOVE      *OFF          *IN20
     C*
     C                   ENDIF                                                  CDSS#=SVSS#
     C*
     C*  LOAD THE SAVE FIELDS
     C*
     C*** Beg Add ********* 06/09/20 ******************************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'CDSS#'       QQNAME           25
     C     CDSS#         PARM      CDSS#         QQVAL           100
     C
     C*** End Add ********* 06/09/20 ******************************************
     C
     C                   MOVEL(P)  CDSS#         SVSS#
     C                   MOVEL(P)  CDLSN         SVLSN
     C                   MOVEL(P)  CDFSN         SVFSN
     C                   MOVEL(P)  CDIUSR        SVIUSR
     C                   MOVEL(P)  CDIPGM        SVIPGM
     C                   Z-ADD     CDIDT         SVIDT
     C                   MOVEL(P)  CDUSER        SVUSER
     C                   MOVEL(P)  CDPGM         SVPGM
     C                   Z-ADD     CDDT          SVDT
     C*
     C                   ENDDO
     C*
     C* PRINT MESSAGE IF NO RECORDS FOR REPORT
     C*
     C     COUNT         IFEQ      *ZEROS
     C                   EXCEPT    MSG
     C                   ENDIF                                                  COUNT=*ZEROS
     C*
     C* PRINT THE TOTAL LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    TOTL
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*
     OQSYSPRT   E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                                           71 'THIS REPORT PRINTS DUPLI'
     O                                         +000 'CATE SS# IN WMCDP.'
     O                       DSDATE             140 '  /  /    '
     O          E            HEAD           2
     O                                           10 'CHKDUPSS# '
     O                                          135 'PAGE:'
     O                       PAGE          Z    140
     O          E            HEAD           1
     O                                           10 'SOC SEC #'
     O                                           22 'LAST NAME'
     O                                           65 'FIRST NAME'
     O                                           76 'USER'
     O                                           91 'PROGRAM'
     O                                          105 'DATE MNT'
     O                                          116 'INIT USER'
     O                                          126 'INIT PGM'
     O                                          140 'INIT DATE'
     O          E   N20      DET         1  1
     O                       SVSS#                9
     O                       SVLSN               53
     O                       SVFSN               67
     O                       SVUSER              82
     O                       SVPGM               94
     O                       SVDT               105 '   /  /  '
     O                       SVIUSR             117
     O                       SVIPGM             129
     O                       SVIDT              140 '   /  /  '
     O          E            DET            1
     O                       CDSS#                9
     O                       CDLSN               53
     O                       CDFSN               67
     O                       CDUSER              82
     O                       CDPGM               94
     O                       CDDT               105 '   /  /  '
     O                       CDIUSR             117
     O                       CDIPGM             129
     O                       CDIDT              140 '   /  /  '
     O          E            DET1           1
     O          E            TOTL        1  2
     O                                           27 'TOTAL NUMBER OF RECORDS:'
     O                       COUNT         B     35
     O          E            MSG            2
     O                                           28 'NO DUPLICATE CLAIMS EXIS'
     O                                         +000 'T'
