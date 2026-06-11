     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA744R - Update detail diagnosis pointers
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA744R                                            *
     H*    CREATED: 02/20/07  C:188                                   *
     H*    DESCRIPTION:  Detail diagnosis pointer update based on     *
     H*                  billtype. Corrects condition when header     *
     H*                  diagnosis codes are deleted and do not       *
     H*                  update detail pointers.                      *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  1/7/14  C:079 Project: MC329 Phase II                        *
     H*                Add ICD10 logic                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*    *IN99                GENERAL CHAINING INDICATOR            *
     H*****************************************************************
     F*
     FFAEOP     IF   E             DISK
     FFAEHL     IF   E           K DISK    USROPN
     FFAEDL     IF   E           K DISK    USROPN
     F*
     FFDDHL     UF   E           K DISK    USROPN
     FFDDDL     UF   E           K DISK    USROPN

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0           1
     C                   PARM                    @@RTCD            1             2
     C*
     C     TMKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOEOB#
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
     C* GET FAEOP RECORD USING RRN USING ENTRY PARM
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      SRPHY
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      SRDEN
     C                   ENDSL
     C                   ENDIF
     C*
     C                   SETON                                        LR
     C                   RETURN
     C*
     C*****************************************************************
     C* SR - PROCESS PHYSICIAN TYPE
     C*****************************************************************
     C     SRPHY         BEGSR
     C*
     C                   MOVE      '0'           CHGFLG            1
     C*
     C                   OPEN      FAEHL
     C     TMKY          CHAIN     PEH01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   OPEN      FDDHL
     C     TMKY          CHAIN     PDH01                              99
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
      *** Beg Add *** 02/07/14 ****************************************
      /free
       If EHicdind <> 'Y';
      /end-free
      *** End Add *** 02/07/14 ****************************************
     C*
     C     EHDIA1        IFEQ      *BLANKS
     C     DHDXR1        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DHDXR1
     C                   MOVE      '1'           CHGFLG
     C                   ENDIF
     C*
     C     EHDIA2        IFEQ      *BLANKS
     C     DHDXR2        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DHDXR2
     C                   MOVE      '1'           CHGFLG
     C                   ENDIF
     C*
     C     EHDIA3        IFEQ      *BLANKS
     C     DHDXR3        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DHDXR3
     C                   MOVE      '1'           CHGFLG
     C                   ENDIF
     C*
     C     EHDIA4        IFEQ      *BLANK
     C     DHDXR4        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DHDXR4
     C                   MOVE      '1'           CHGFLG
     C                   ENDIF
      *** Beg Add *** 02/07/14 ****************************************
      /free
       Else;
          If EHicd1001 = *blanks and DHdxr1 <> *blanks;
            eval dhdxr1 = *blanks;
            eval chgflg = '1';
          Endif;
          If EHicd1002 = *blanks and DHdxr2 <> *blanks;
            eval dhdxr2 = *blanks;
            eval chgflg = '1';
          Endif;
          If EHicd1003 = *blanks and DHdxr3 <> *blanks;
            eval dhdxr3 = *blanks;
            eval chgflg = '1';
          Endif;
          If EHicd1004 = *blanks and DHdxr4 <> *blanks;
            eval dhdxr4 = *blanks;
            eval chgflg = '1';
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/07/14 ****************************************
     C*
     C     CHGFLG        IFEQ      '1'
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDH01
     C                   ENDIF
     C*
     C     TMKY          READE     PDH01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C* SR - PROCESS DENTAL TYPE
     C*****************************************************************
     C     SRDEN         BEGSR
     C*
     C                   MOVE      '0'           CHGFLG            1
     C*
     C                   OPEN      FAEDL
     C     TMKY          CHAIN     PED01                              99
     C*
     C     *IN99         IFEQ      '0'
     C                   OPEN      FDDDL
     C  N99TMKY          CHAIN     PDD01                              99
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C*
      *** Beg Add *** 02/07/14 ****************************************
      /free
       If EDicdind <> 'Y';
      /end-free
      *** End Add *** 02/07/14 ****************************************
     C     EDDIA1        IFEQ      *BLANKS
     C     DDDXR1        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DDDXR1
     C                   MOVE      '1'           CHGFLG            1
     C                   ENDIF
     C*
     C     EDDIA2        IFEQ      *BLANKS
     C     DDDXR2        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DDDXR2
     C                   MOVE      '1'           CHGFLG            1
     C                   ENDIF
     C*
     C     EDDIA3        IFEQ      *BLANKS
     C     DDDXR3        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DDDXR3
     C                   MOVE      '1'           CHGFLG            1
     C                   ENDIF
     C*
     C     EDDIA4        IFEQ      *BLANK
     C     DDDXR4        ANDNE     *BLANKS
     C                   MOVE      *BLANKS       DDDXR4
     C                   MOVE      '1'           CHGFLG            1
     C                   ENDIF
      *** Beg Add *** 02/07/14 ****************************************
      /free
       Else;
          If EDicd1001 = *blanks and DDdxr1 <> *blanks;
            eval DDdxr1 = *blanks;
            eval chgflg = '1';
          Endif;
          If EDicd1002 = *blanks and DDdxr2 <> *blanks;
            eval DDdxr2 = *blanks;
            eval chgflg = '1';
          Endif;
          If EDicd1003 = *blanks and DDdxr3 <> *blanks;
            eval DDdxr3 = *blanks;
            eval chgflg = '1';
          Endif;
          If EDicd1004 = *blanks and DDdxr4 <> *blanks;
            eval DDdxr4 = *blanks;
            eval chgflg = '1';
          Endif;
       Endif;
      /end-free
      *** END Add *** 02/07/14 ****************************************
     C*
     C     CHGFLG        IFEQ      '1'
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1PGM         DHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   UPDATE    PDD01
     C                   ENDIF
     C*
     C     TMKY          READE     PDD01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
