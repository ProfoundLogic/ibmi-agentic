     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE F7062R - WC TO FS CASE TRANSFER RECORD UPDATES
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  03/23/93 C:20  1.ADDED SS#/ACDT TO WRITE OF FACTP FILE       *
     H*****************************************************************
     H** INDICATOR USAGE:                                             *
     H*****************************************************************
     FSMUPL     IF   E           K DISK
     FFACTP     O    E           K DISK
     F                                     RENAME(PCT01:PCT02)
     FFXCTP     UF   E           K DISK
     FFMFNL1    IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE USER PARM FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   XREF TO WCMS FUND# TO GET WCMC SUB#/FUND
     C*
     C     FNKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    WCFND
     C*
     C*   KEYLIST FOR TRANSFER WORKFILE
     C*
     C     CTKEY1        KLIST
     C                   KFLD                    XFCO#
     C                   KFLD                    XFFND
     C                   KFLD                    XFFYR
     C                   KFLD                    XFCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C*
     C                   MOVE      QQCO#         XFCO#
     C                   MOVE      QQFND         XFFND
     C                   MOVE      QQFYR         XFFYR
     C                   MOVE      QQCASE        XFCASE
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    CTTCO#        XXCO#                          3.0N
     C     *LIKE         DEFINE    CTTCO#        XFCO#                          3.0N
     C     *LIKE         DEFINE    CTTFND        WCFND                          3.0N
     C     *LIKE         DEFINE    CTTFND        XFFND                          3.0N
     C     *LIKE         DEFINE    CTTFYR        XFFYR                          3.0N
     C     *LIKE         DEFINE    CTTCAS        XFCASE                         7.0N
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              51
     C*
     C     XFFND         IFEQ      *ZEROS
     C     XFFYR         ANDEQ     *ZEROS
     C     XFCASE        ANDEQ     *ZEROS
     C*
     C                   READ      PCT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C* GET FROM CASE SUBSCRIBER NUMBER AND FUND FROM WC XREF FUND
     C*
     C                   Z-ADD     CTFCO#        XXCO#
     C                   Z-ADD     CTFCO#        CTFCO#
     C                   Z-ADD     CTFFND        WCFND
     C     FNKEY1        CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     FNSUB#        CTFSUB
     C                   Z-ADD     FNFND         CTFFND
     C                   ELSE                                                   *IN50 = 1
     C                   Z-ADD     UPSUB#        CTFSUB
     C                   Z-ADD     UPFND         CTFFND
     C                   END                                                    END *IN50
     C*
     C* MOVE IN THE REST OF FROM CASE VALUES FROM FXCTP
     C*
     C                   Z-ADD     CTFFYR        CTFFYR
     C                   Z-ADD     CTFCAS        CTFCAS
     C                   MOVE      CTFSS#        CTFSS#                         ADD 03/23/93
     C                   Z-ADD     CTFACD        CTFACD                         ADD 03/23/93
     C*
     C* GET TO CASE SUBSCRIBER NUMBER AND FUND FROM WC XREF FUND
     C*
     C                   Z-ADD     CTTCO#        XXCO#
     C                   Z-ADD     CTTCO#        CTTCO#
     C                   Z-ADD     CTTFND        WCFND
     C     FNKEY1        CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     FNSUB#        CTTSUB
     C                   Z-ADD     FNFND         CTTFND
     C                   ELSE                                                   *IN50 = 1
     C                   Z-ADD     UPSUB#        CTTSUB
     C                   Z-ADD     UPFND         CTTFND
     C                   END                                                    END *IN50
     C*
     C* MOVE IN THE REST OF TO CASE VALUES FROM FXCTP
     C*
     C                   Z-ADD     CTTFYR        CTTFYR
     C                   Z-ADD     CTTCAS        CTTCAS
     C                   MOVE      CTTSS#        CTTSS#                         ADD 03/23/93
     C                   Z-ADD     CTTACD        CTTACD                         ADD 03/23/93
     C*
     C* MOVE IN THE REST OF FXCTP FILEDS
     C*
     C                   MOVE      CTSTAT        CTSTAT
     C                   MOVE      CTUSER        CTUSER
     C                   MOVE      CTPGM         CTPGM
     C                   Z-ADD     CTTIME        CTTIME
     C                   Z-ADD     CTDT          CTDT
     C                   MOVEL     CTIUSR        CTIUSR
     C                   Z-ADD     CTIDT         CTIDT
     C*
     C* WRITE RECORDS TO CASE TRANSFER FILE(FACTP)
     C*
     C                   WRITE     PCT02
     C*
     C* DELETE THE PREVIOUSLY WRITTEN TRANFER RECORD
     C*
     C                   DELETE    PCT01
     C*
     C* GET THE NEXT RECORD FROM FXCTP
     C*
     C                   READ      PCT01                                  60
     C                   END
     C*
     C                   ELSE                                                   ELSE <> 0
     C*
     C     CTKEY1        CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* GET FROM CASE SUBSCRIBER NUMBER AND FUND FROM WC XREF FUND
     C*
     C                   Z-ADD     XFCO#         XXCO#
     C                   Z-ADD     CTFCO#        CTFCO#
     C                   Z-ADD     XFFND         WCFND
     C     FNKEY1        CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     FNSUB#        CTFSUB
     C                   Z-ADD     FNFND         CTFFND
     C                   ELSE                                                   *IN50 = 1
     C                   Z-ADD     UPSUB#        CTFSUB
     C                   Z-ADD     UPFND         CTFFND
     C                   END                                                    END *IN50
     C*
     C* MOVE IN THE REST OF FROM CASE VALUES FROM FXCTP
     C*
     C                   Z-ADD     CTFFYR        CTFFYR
     C                   Z-ADD     CTFCAS        CTFCAS
     C                   MOVE      CTFSS#        CTFSS#                         ADD 03/23/93
     C                   Z-ADD     CTFACD        CTFACD                         ADD 03/23/93
     C*
     C* GET TO CASE SUBSCRIBER NUMBER AND FUND FROM WC XREF FUND
     C*
     C                   Z-ADD     CTTCO#        XXCO#
     C                   Z-ADD     CTTCO#        CTTCO#
     C                   Z-ADD     CTTFND        WCFND
     C     FNKEY1        CHAIN     PFN01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     FNSUB#        CTTSUB
     C                   Z-ADD     FNFND         CTTFND
     C                   ELSE                                                   *IN50 = 1
     C                   Z-ADD     UPSUB#        CTTSUB
     C                   Z-ADD     UPFND         CTTFND
     C                   END                                                    END *IN50
     C*
     C* MOVE IN THE REST OF TO CASE VALUES FROM FXCTP
     C*
     C                   Z-ADD     CTTFYR        CTTFYR
     C                   Z-ADD     CTTCAS        CTTCAS
     C                   MOVE      CTTSS#        CTTSS#                         ADD 03/23/93
     C                   Z-ADD     CTTACD        CTTACD                         ADD 03/23/93
     C*
     C* MOVE IN THE REST OF FXCTP FILEDS
     C*
     C                   MOVE      CTSTAT        CTSTAT
     C                   MOVE      CTUSER        CTUSER
     C                   MOVE      CTPGM         CTPGM
     C                   Z-ADD     CTTIME        CTTIME
     C                   Z-ADD     CTDT          CTDT
     C                   MOVEL     CTIUSR        CTIUSR
     C                   Z-ADD     CTIDT         CTIDT
     C*
     C* WRITE RECORDS TO CASE TRANSFER FILE(FACTP)
     C*
     C                   WRITE     PCT02
     C*
     C* DELETE THE PREVIOUSLY WRITTEN TRANFER RECORD
     C*
     C                   DELETE    PCT01
     C*
     C                   END                                                    END *IN60
     C*
     C                   END                                                    END XFFND
     C*
     C* END THE PROGRAM
     C*
     C                   MOVE      '1'           *INLR
