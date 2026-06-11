    HCOPYRIGHT('(C) Copyright Fiserv, Inc., 2007')
    H*********************************************************************
    H/TITLE EDI Research w/ROES
    H*********************************************************************
    H*  CALLED FROM: W8079C1                                             *
    H*                                                                   *
    H*  CALLS TO:
    H*                                                                   *
    H*  CREATED: 12/21/07      C:184     PROJECT: 135WKI                 *
    H*********************************************************************
    H*  REVISIONS:                                                       *
    H*                                                                   *
    H*********************************************************************
     FWXXGP     IF   E             DISK
     FWXLGP     O    E             DISK
      *
     D*
     D  XGREC        E DS                  EXTNAME(WXXGP)
     D   XGDATA      E
     D   ACLC                        25    OVERLAY(XGDATA:1)
     D   TSET                         3    OVERLAY(XGDATA:26)
     D   MTC                          2    OVERLAY(XGDATA:29)
     D   JST                          2    OVERLAY(XGDATA:31)
     D   STS                          2    OVERLAY(XGDATA:33)
     D   MTCD                         8    OVERLAY(XGDATA:35)
     D   DUE                          8    OVERLAY(XGDATA:41)
     D   SENT                         8    OVERLAY(XGDATA:52)
     D   ARCV                         8    OVERLAY(XGDATA:58)
     D   ACL#                        25    OVERLAY(XGDATA:64)
     C*** MAIN PROCESSING STARTS *******************************************
     C*
     C                   READ      WXXGP
     C*
     C                   DOW       NOT %EOF(WXXGP)
     C*
     C                   EVAL      LXACLC = ACLC
     C*
     C                   EVAL      LXTSET = TSET
     C                   EVAL      LXMTC  = MTC
     C                   EVAL      LXJST  = JST
     C                   EVAL      LXSTS  = STS
     C                   EVAL      LXACL# = ACL#
     C*
     C                   IF        MTCD <> *blanks
     C                   MOVEL     MTCD          LXMTCD
     C                   ELSE
     C                   EVAL      LXMTCD = *ZEROS
     C                   ENDIF
     C*
     C                   IF        DUE <> *blanks
     C                   MOVEL     DUE           LXDUE
     C                   ELSE
     C                   EVAL      LXDUE = *ZEROS
     C                   ENDIF
     C*
     C                   IF        SENT <> *blanks
     C                   MOVEL     SENT          LXSENT
     C                   ELSE
     C                   EVAL      LXSENT = *ZEROS
     C                   ENDIF
     C*
     C                   IF        ARCV <> *blanks
     C                   MOVEL     ARCV          LXARCV
     C                   ELSE
     C                   EVAL      LXARCV = *ZEROS
     C                   ENDIF
     C*
     C                   WRITE     PLX01
     C*
     C                   READ      WXXGP
     C*
     C                   ENDDO
     C*
     C                   EVAL      *INLR = *ON
