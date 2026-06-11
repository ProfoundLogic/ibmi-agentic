     Hcopyright('(c) 2020 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO FIX CLAIMANT NAME IN FMCMP, FAEOP, FALTP
     H***** Beg Add *** 06/09/20 ************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*                                                               *
     H* Description:  This pgm prints a list of claims which have a   *
     H*   discrepancy between the claimant name in FMCMP, FAEOP, FALTP*
     H*   compared to FMCDP.                                          *
     H*
     H* PROJ: SUPP # 20590, 20871,  20774
     H* DATE: 02/07/00     C:03
     H*                                                               *
     H*****************************************************************
     H*****************************************************************
     H** REVISIONS:
     H*                                                               *
     H* 06/09/20 C:295 Project: SY420                                 *
     H*                -Mask SSN                                      *
     H*                                                               *
     H*****************************************************************
     F*
     F* MC CLAIMANT DEMOGRAPHICS (SS#)
     FFMCDL     IF   E           K DISK
     F*
     F* MC CLAIMANT MASTER (CO#/SUB#/FND/FYR/CASE)
     FFMCML1    UF   E           K DISK
     F*
     F* MC EOB MASTER (CO#/SUB#/FND/FYR/CASE/EOB#)
     FFAEOLG    UF   E           K DISK
     F*
     F* MC LTR MASTER (CO#/SUB#/FND/FYR/CASE/LTR#)
     FFALTL1    UF   E           K DISK
     F*
     F* COMPANY MASTER (CO#)
     FSMCOL     IF   E           K DISK
     F*
     F* CORRECTION REPORT
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     D*
     D* DEFINE DATA STRUCTURES
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C**************************************************************************
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Key list for the Company file
     C*
     C     COKY          KLIST
     C                   KFLD                    CMCO#
     C*
     C*  Key list for the Claimant Demographics file
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C* ENTRY PARMS
     C     *ENTRY        PLIST
     C                   PARM                    QQMODE            1
     C*
     C* DEFINE WORK FIELDS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C                   MOVE      *BLANKS       SVNAME           40
     C                   MOVE      *BLANKS       XXNAME           40
     C                   Z-ADD     0             CMCNT             7 0
     C                   Z-ADD     0             EOCNT             7 0
     C                   Z-ADD     0             LTCNT             7 0
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C*
     C*  Start FMCMP main loop
     C*
     C                   READ      PCM01                                  60
     C*
     C  N60COKY          CHAIN     PCO01                              61
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADCM
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     CMLSN         IFNE      CDLSN
     C     CMFSN         ORNE      CDFSN
     C     CMMID         ORNE      CDMID
     C*
     C     CMLSN         CAT(P)    ',':0         SVNAME
     C                   CAT       CMFSN:1       SVNAME
     C                   CAT       CMMID:1       SVNAME
     C     CDLSN         CAT(P)    ',':0         XXNAME
     C                   CAT       CDFSN:1       XXNAME
     C                   CAT       CDMID:1       XXNAME
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADCM
     C                   MOVE      '0'           *INOF
     C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CMSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DETCM
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CMSS# = UnMaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ADD       1             CMCNT
     C*
     C     QQMODE        IFEQ      'U'
     C                   MOVEL     CDLSN         CMLSN
     C                   MOVEL     CDFSN         CMFSN
     C                   MOVEL     CDMID         CMMID
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     DSTIME        CMTIME
     C                   Z-ADD     XXDT          CMDT
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PCM01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    TOTCM
     C*
     C*  Start FAEOP main loop
     C*
     C                   READ      PEO01                                  60
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADEO
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* GET THE CASE SO WE ENSURE WE HAVE THE CORRECT SS#
     C                   Z-ADD     EOCO#         XXCO#
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   Z-ADD     EOFND         XXFND
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOCASE        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              63
     C     *IN63         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     EOLSN         IFNE      CDLSN
     C     EOFSN         ORNE      CDFSN
     C     EOMID         ORNE      CDMID
     C*
     C     EOLSN         CAT(P)    ',':0         SVNAME
     C                   CAT       EOFSN:1       SVNAME
     C                   CAT       EOMID:1       SVNAME
     C     CDLSN         CAT(P)    ',':0         XXNAME
     C                   CAT       CDFSN:1       XXNAME
     C                   CAT       CDMID:1       XXNAME
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADEO
     C                   MOVE      '0'           *INOF
     C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CMSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DETEO
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CMSS# = UnMaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ADD       1             EOCNT
     C*
     C     QQMODE        IFEQ      'U'
     C                   MOVEL     CDLSN         EOLSN
     C                   MOVEL     CDFSN         EOFSN
     C                   MOVEL     CDMID         EOMID
     C                   MOVEL     CDSS#         EOSS#
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   Z-ADD     XXDT          EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDIF                                                  DIFF NAME END
     C*
     C                   ENDIF                                                  CDKY END
     C*
     C                   ENDIF                                                  CMKY END
     C*
     C                   READ      PEO01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    TOTEO
     C*
     C*  Start FALTP main loop
     C*
     C                   READ      PLT01                                  60
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADLT
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* GET THE CASE SO WE ENSURE WE HAVE THE CORRECT SS#
     C                   Z-ADD     LTCO#         XXCO#
     C                   Z-ADD     LTSUB#        XXSUB#
     C                   Z-ADD     LTFND         XXFND
     C                   Z-ADD     LTFYR         XXFYR
     C                   Z-ADD     LTCASE        XXCASE
     C*
     C     CMKY          CHAIN     PCM01                              63
     C     *IN63         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     LTLNM         IFNE      CDLSN
     C     LTFNM         ORNE      CDFSN
     C*
     C     LTLNM         CAT(P)    ',':0         SVNAME
     C                   CAT       LTFNM:1       SVNAME
     C     CDLSN         CAT(P)    ',':0         XXNAME
     C                   CAT       CDFSN:1       XXNAME
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADLT
     C                   MOVE      '0'           *INOF
     C                   END
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CMSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   EXCEPT    DETLT
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
        CMSS# = UnMaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ADD       1             LTCNT
     C*
     C     QQMODE        IFEQ      'U'
     C                   MOVEL     CDLSN         LTLNM
     C                   MOVEL     CDFSN         LTFNM
     C                   MOVEL     CDSS#         LTSS#
     C                   MOVEL     Q1PGM         LTPGM
     C                   Z-ADD     DSTIME        LTTIME
     C                   Z-ADD     XXDT          LTDT
     C                   UPDATE    PLT01
     C                   ENDIF
     C*
     C                   ENDIF                                                  DIFF NAME END
     C*
     C                   ENDIF                                                  CDKY END
     C*
     C                   ENDIF                                                  CMKY END
     C*
     C                   READ      PLT01                                  60
     C                   ENDDO
     C*
     C                   EXCEPT    TOTLT
     C*
     C                   SETON                                        LR
     C**************************************************************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O*
     O* FMCMP UPDATES
     O          E            HEADCM         2
     O                                            7 'SUB#'
     O                                           11 'FND'
     O                                           15 'FYR'
     O                                           23 'CASE'
     O                                           39 'SS#'
     O                                           60 'Previous name'
     O                                          101 'Correct name'
     O          E            HEADCM      1  1
     O                                           10 'FMCMP'
     O          E            DETCM          1
     O                       CMSUB#        4      7
     O                       CMFND         4     11
     O                       CMFYR         4     15
     O                       CMCASE        4     23
     O                       CMSS#               45
     O                       SVNAME              87
     O                       XXNAME             129
     O          E            TOTCM       1  1
     O                                           12 'FMCMP CNT:'
     O                       CMCNT         4     20
     O*
     O* FAEOP UPDATES
     O          E            HEADEO         2
     O                                            7 'SUB#'
     O                                           11 'FND'
     O                                           15 'FYR'
     O                                           23 'CASE'
     O                                           34 'EOB#'
     O                                           39 'SS#'
     O                                           60 'Previous name'
     O                                          101 'Correct name'
     O          E            HEADEO      1  1
     O                                           10 'FAEOP'
     O          E            DETEO          1
     O                       EOSUB#        4      7
     O                       EOFND         4     11
     O                       EOFYR         4     15
     O                       EOCASE        4     23
     O                       EOEOB#        4     34
     O                       CMSS#               45
     O                       SVNAME              87
     O                       XXNAME             129
     O          E            TOTEO       1  1
     O                                           12 'FAEOP CNT:'
     O                       EOCNT         4     20
     O*
     O* FALTP UPDATES
     O          E            HEADLT         2
     O                                            7 'SUB#'
     O                                           11 'FND'
     O                                           15 'FYR'
     O                                           23 'CASE'
     O                                           34 'LTR#'
     O                                           39 'SS#'
     O                                           60 'Previous name'
     O                                          101 'Correct name'
     O          E            HEADLT      1  1
     O                                           10 'FALTP'
     O          E            DETLT          1
     O                       LTSUB#        4      7
     O                       LTFND         4     11
     O                       LTFYR         4     15
     O                       LTCASE        4     23
     O                       LTLTR#        4     34
     O                       CMSS#               45
     O                       SVNAME              87
     O                       XXNAME             129
     O          E            TOTLT       1  1
     O                                           12 'FALTP CNT:'
     O                       LTCNT         4     20
     O          E            TOTLT       1  1
     O                                           12 'FAEOP CNT:'
     O                       EOCNT         4     20
     O          E            TOTLT       1  1
     O                                           12 'FMCMP CNT:'
     O                       CMCNT         4     20
