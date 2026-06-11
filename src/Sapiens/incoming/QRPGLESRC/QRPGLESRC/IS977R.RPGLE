     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     FIS977F    CF   E             WORKSTN USROPN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FIMDOL1    IF   E           K DISK

     D MS              S             49    DIM(1) CTDATA PERRCD(1)
     D XXDCID          DS
     D  QTIM                   1      8
     D  TPERD                  9      9
     D  TSFX                  10     12
     D SPATH           DS
     D  SPFSYS                 1      5
     D  SVOLPT                 6     80
     D SPTHLN          DS
     D  SPTHL                  1      4B 0
     DtPath            s             80
     DtVolPt           s             80
     D TPTHLN          DS
     D  TPTHL                  1      4B 0
     D CPYINF          DS
     D  TYPOFC                 1      1
     D  CPYFIL                 2      6
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D RETCD           DS
     D  RCLEN                  1      4B 0
     D  RTCD                   5      8B 0
     D  EXID                   9     15
     D  CONDTN                 9     15
     D  RSV                   16     16
     D  EXDTA                 17    272
     D  MSG                   17    272
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     DRtvDocRoot       pr                  extPgm('RTVDOCROOT')
     D inRoot                        10

     DstrRoot          s             10

     C     PCPYSF        PLIST
     C                   PARM                    SPATH            80
     C                   PARM                    SPTHL
     C                   PARM                    CPYINF
     C                   PARM                    TPATH            80
     C                   PARM                    TPTHL
     C                   PARM                    RETCD
     C     PRMSG         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMDESC          256
     C                   PARM                    QMRTN             1
     C     PLBRK         PLIST
     C                   PARM                    QMID              7
     C                   PARM                    QMDTA           256
     C                   PARM                    QMRTN             1
     C     DOKY          KLIST
     C                   KFLD                    QQDCID
     C     *ENTRY        PLIST
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQADOC           12
     C                   PARM                    QQAFLD           80
      /Free
       RtvDocRoot(strRoot);
      /End-free
     C                   EXSR      DBGMOD
     C                   MOVEL     QQRTCD        INTER             1
     C     INTER         COMP      '*'                                    71
     C                   MOVEL     *BLANKS       QQRTCD
     C                   TESTN                   QQSQ#                  67
     C     QQSQ#         IFNE      *BLANKS
     C                   MOVE      QQSQ#         XXSEQ#            5 0
     C                   ELSE
     C                   MOVE      *ZEROS        XXSEQ#
     C                   ENDIF
     C                   MOVE      *BLANKS       MSGDSC          256
     C     *IN71         IFEQ      *ON
     C     OP977         ANDEQ     *BLANKS
     C                   OPEN      IS977F
     C                   MOVE      '1'           OP977             1
     C                   ENDIF
     C     DOKY          CHAIN     PDO01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVEL     MS(1)         SCMSG
     C                   MOVE      '1'           *IN10
     C   71              EXFMT     WDW01
     C                   MOVE      '1'           QQRTCD                         NO DL RCD FOUND
     C                   GOTO      TERM
     C                   END
     C     *IN71         IFEQ      *ON
     C                   MOVEL     'WDW01   '    Q2RECF
     C   60
     CANN60              READ      WDW01                                  60
     C                   WRITE     WDW01
     C                   ENDIF
     C                   EXSR      OPTRTV
     C     TERM          TAG
     C                   MOVE      '1'           *INLR






     C     OPTRTV        BEGSR

     C                   MOVEL     '/QOPT'       SPFSYS                         FROM OPTICAL
      /Free
       //tPath = '/QDLS' + %trim(tVolPt);
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
      /End-free
     C                   TIME                    DSDTTM
     C                   CALL      'SRGETTIM'
     C                   PARM      *BLANKS       QTIM
     C     QTIM          IFEQ      *BLANKS
     C                   MOVEL     '00000000'    QTIM
     C                   ENDIF
     C                   MOVEL     '.'           TPERD
     C                   Z-ADD     *ZERO         G
     C     '.'           SCAN      QQDCID:1      G                 2 0    43
     C     *IN43         IFEQ      *ON                                          FOUND
     C                   ADD       1             G
     C     3             SUBST     QQDCID:G      TSFX                   44
     C     *IN44         IFEQ      *ON                                          ERROR
     C                   MOVEL     'TXT'         TSFX
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'TXT'         TSFX
     C                   ENDIF
     **
     C                   MOVE      Q1JOBN        JOBNBR            6
     C                   CALL      'SA030R'
     C                   PARM      *BLANKS       ##PRFX            3
     C                   PARM      *BLANKS       ##PFLR           80
     C     ##PFLR        IFNE      *BLANKS
     C     '/'           CAT       'OPTTEMP'     OPTDIR            8
     C     '/'           CAT(P)    ##PFLR:0      TVOLPT
     C                   CAT       OPTDIR:0      TVOLPT
     C                   ELSE
     C     '/'           CAT(P)    'OPTTEMP'     TVOLPT
     C                   ENDIF
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    JOBNBR:0      TVOLPT
      /Free
       //tPath = '/QDLS' + %trim(tVolPt);
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
      /End-free
     C                   CALL      'IS977C1'
     C                   PARM                    TPATH
     C                   CAT(P)    '/':0         TVOLPT
     C                   CAT(P)    XXDCID:0      TVOLPT
      /Free
       //tPath = '/QDLS' + %trim(tVolPt);
       tPath = '/' +%trim(strRoot) + %trim(tVolPt);
      /End-free
     C     Q1PRMS        IFLE      5
     C                   MOVEL     QQDCID        ORGDOC           12
     C                   MOVE      '.ART'        ORGDOC
     C                   MOVEL     XXDCID        NEWDOC           12
     C                   MOVE      '.ART'        NEWDOC
     C                   CALL      'IS977C'
     C                   PARM                    ORGDOC
     C                   PARM                    QQFOLD
     C                   PARM                    Q1JOBN
     C                   PARM                    NEWDOC
     C                   PARM                    CXRTCD            1
     C                   ELSE
     C                   MOVEL     QQDCID        QQADOC
     C                   MOVE      '.ART'        QQADOC
     C                   MOVEL(P)  QQFOLD        QQAFLD
     C     'OPTTEMP'     CAT(P)    '/':0         QQFOLD
     C                   CAT       JOBNBR:0      QQFOLD
     C                   ENDIF
     C     '/'           CAT(P)    DOVOL1:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DOOFLD:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DOOSFL:0      SVOLPT
     C                   CAT(P)    '/':0         SVOLPT
     C                   CAT(P)    DOODCI:0      SVOLPT
     C                   Z-ADD     80            SPTHL                          PTH LEN
     C                   Z-ADD     80            TPTHL                          PTH LEN
     C                   MOVEL     '1'           TYPOFC                         TYPE OF COPY
     C                   Z-ADD     272           RCLEN                          LEN OF ERROR CD
     C                   CALL      'QHFCPYSF'    PCPYSF                         CPY DOC TO DASD
     C     RTCD          IFNE      0
     C     CONDTN        ANDNE     'CPF8A82'
     C                   MOVE      '1'           ERR               1
     C                   MOVEL     *BLANKS       QQDCID
     C                   MOVEL     *BLANKS       QQFOLD
     C                   MOVE      '3'           QQRTCD
     C     CONDTN        IFEQ      'CPF1F61'
     C                   MOVE      '1'           QQRTCD
     C                   END
     C     CONDTN        IFEQ      'CPF1F48'
     C                   MOVE      '2'           QQRTCD
     C                   END
     C                   MOVEL     CONDTN        QMID
     C                   MOVEL     MSG           QMDTA
     C                   MOVEL     *BLANKS       QMDESC
     C                   MOVEL     *BLANKS       QMRTN
     C                   CALL      'RTVMSGC'     PRMSG
     C     QMRTN         IFEQ      *BLANKS
     C                   MOVE      QMDESC        MSGDSC
     C                   ELSE
     C                   MOVE      *BLANKS       MSGDSC
     C                   END
     C     CONDTN        IFEQ      'CPF1F71'                                          F SYS
     C                   CALL      'BRKMSGC'     PLBRK
     C     QMRTN         IFEQ      '*'
     C                   MOVE      '*'           QQRTCD
     C                   GOTO      TERM
     C                   END
     C                   END
     C                   ELSE
     C                   MOVEL     XXDCID        QQDCID
     C                   MOVEL     *BLANKS       QQRTCD
     C                   END
     C     ENDOR         ENDSR






     C     DBGMOD        BEGSR
     C     *DTAARA       DEFINE                  VRTOPT            1
     C                   IN        *DTAARA                              83      ERROR IF 83
     C     *IN83         IFEQ      *OFF
     C     VRTOPT        IFEQ      'Y'
     C     Q1PRMS        IFLE      5
     C                   CALL      'VRT977R'                            99
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD            1
     C                   ELSE
     C                   CALL      'VRT977R'                            99
     C                   PARM                    QQKY             40
     C                   PARM                    QQSQ#             5
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFOLD           80
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQADOC           12
     C                   PARM                    QQAFLD           80
     C                   ENDIF
     C     *IN99         CABEQ     *ON           ENDDBG
     C                   GOTO      TERM
     C                   ELSE
     C                   GOTO      ENDDBG
     C                   ENDIF
     C                   ELSE
     C                   GOTO      ENDDBG
     C                   ENDIF
     C     ENDDBG        ENDSR
**
Error - No Document Location can be established
