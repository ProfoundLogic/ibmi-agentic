     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIX41844 - UPDATE EOB FILES FOR CASE TRANSFER
     H*****************************************************************
     H* DESCRIPTION: UPDATE EOB FILES FOR CASE TRANSFER
     H*
     H* 07/19/16 C:79 SUPPORT: 41844/136
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H* INDICATORS:
     H*
     H*****************************************************************
     H*
     F*
     FFACTP     IF   E             DISK
     F                                     infds(dbfds1)
     FFAEOL     IF   E           K DISK
     F                                     infds(dbfds2)
     FFMCML1    IF   E           K DISK
     F                                     infds(dbfds3)
     F*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     D dbfds1          ds
     D dbrr01                397    400b 0
     D dbfds2          ds
     D dbrr02                397    400b 0
     D dbfds3          ds
     D dbrr03                397    400b 0
     D*================================================================
     D ##rrn1          s              9  0
     D ##rrn2          s              9  0
     D ##rrn3          s              9  0
     D ##FLAG          s              1
     D ##UPDATE        s              1
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@update          1
     C*
     C     GENKY         KLIST                                                  FROM CASE
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    CTSTAT        ##TD
     C     *LIKE         DEFINE    CTKEY         ##KEY
     C*
     C*================================================================
     C* MAIN - Main program processing
     C*================================================================
     C                   EVAL      ##update = @@update
     C*
     C                   READ      PCT01                                  99
     C*
     C                   DOW       *IN99 = *OFF
     C*
     C                   IF        CTFFYR <> 0 AND CTFCAS <> 0
     C*
     C                   EVAL      ##RRN1 = DBRR01
     C*
     C                   EVAL      KKCO# = CTFCO#
     C                   EVAL      KKSUB# = CTFSUB
     C                   EVAL      KKFND = CTFFND
     C                   EVAL      KKFYR = CTFFYR
     C                   EVAL      KKCASE = CTFCAS
     C*
     C     GENKY         CHAIN     PEO01                              99
     C*
     C                   IF        *IN99 = *OFF
     C*
     C                   EVAL      ##TD = CTSTAT
     C                   EVAL      ##KEY = CTKEY
     C                   EVAL      ##KEY = CTKEY
     C                   EVAL      ##RRN2 = DBRR02
     C                   EVAL      ##RRN3 = 0
     C*
     C                   IF        (CTFFYR = CTTFYR AND CTFCAS = CTTCAS)
     C                             OR (CTTFYR = 0 AND CTTCAS = 0)
     C                   EVAL      ##FLAG = 'X'
     C*
     C                   CALL      'FIXCXFER'
     C                   PARM                    ##RRN1
     C                   PARM                    ##TD
     C                   PARM                    ##KEY
     C                   PARM                    ##FLAG
     C                   PARM                    ##RRN3
     C                   PARM                    ##RRN2
     C                   PARM                    ##update
     C*
     C* FROM CASE AND TO CASE ARE DIFFERENT
     C*
     C                   ELSE
     C                   IF        (CTFFYR <> CTTFYR OR CTFCAS = CTTCAS)
     C                              AND CTTFYR <> 0 AND CTTCAS <> 0
     C*
     C                   EVAL      ##FLAG = 'Y'
     C                   EVAL      KKCO# = CTTCO#
     C                   EVAL      KKSUB# = CTTSUB
     C                   EVAL      KKFND = CTTFND
     C                   EVAL      KKFYR = CTTFYR
     C                   EVAL      KKCASE = CTTCAS
     C*
     C     GENKY         CHAIN     PCM01                              99
     C*
     C                   IF        *IN99 = *OFF
     C*
     C                   EVAL      ##TD = CTSTAT
     C                   EVAL      ##KEY = CTKEY
     C                   EVAL      ##KEY = CTKEY
     C                   EVAL      ##RRN2 = DBRR02
     C                   EVAL      ##RRN3 = DBRR03
     C*
     C                   CALL      'FIXCXFER'
     C                   PARM                    ##RRN1
     C                   PARM                    ##TD
     C                   PARM                    ##KEY
     C                   PARM                    ##FLAG
     C                   PARM                    ##RRN3
     C                   PARM                    ##RRN2
     C                   PARM                    ##update
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   READ      PCT01                                  99
     C*
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
