     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXMINA - UPDATE NEW MINIMUM PREMIUM FIELDS IN WDELP.
     H*****************************************************************
     H*  DESCRIPTION:  UPDATE THE NEW FIELDS ELEMNA AND ELAMNA TO     *
     H*                HOLD THE VALUE OF MINIMUM PREMIUM.             *
     H*                                                               *
     H*  PROJECT:  SY048   DATE: 08/21/03  PGMR: 65                   *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDELL1    UF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMRSL     IF   E           K DISK
     FWTLML1    IF   E           K DISK
     FWMEPL1    IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWDCXL2    IF   E           K DISK
     FWTCRL     IF   E           K DISK

     D PDAT            S              7  0 DIM(4) ASCEND                        PAYROLL DATES
     D RDAT            S              7  0 DIM(4) ASCEND                        RATING DATES
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    XXEFDT            7 0
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS            5 0
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  LIMITS FILE KEY
     C*
     C     LMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY            10
     C                   KFLD                    XXEFDT
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    SVDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C* CX REASON FILE
     C*
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    CXCDAT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    PRRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* READ EACH EMPLOYER
     C*
     C                   READ      PEM01                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMCO#         XXCO#
     C                   Z-ADD     EMFND         XXFND
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         SVDIV
     C*
     C* READ EACH FYR ON EMPLR
     C*
     C     ELKY          CHAIN     PEL01                              51
     C     *IN51         DOWEQ     *OFF
     C*
     C                   Z-ADD     ELFYR         XXFYR
     C                   MOVE      *BLANK        XXDONE            1
     C*
     C* CHECK IF POLICY YEAR HAS INVOKED MINIMUM ALREADY
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     XXDONE        ANDEQ     *BLANK
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XXDIV
     C     ELKY1         CHAIN(N)  PEL02                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EMDIV         XSDIV             5 0
     C*
     C     ELEMNP        IFEQ      'Y'
     C     ELFAF         ANDNE     'A'
     C     ELAMNP        OREQ      'Y'
     C     ELFAF         ANDEQ     'A'
     C*
     C     ELFAF         IFNE      'A'
     C     ELANPR        SUB       ELETER        XXHIGH           15 4
     C                   ELSE
     C     ELAAPR        SUB       ELATER        XXHIGH
     C                   ENDIF
     C                   MOVE      'Y'           XXDONE
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C     XXDONE        IFEQ      *BLANK
     C     EMKY          READE     PEM01                                  60
     C                   ENDIF                                                  XXBLANK IFEQ
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXDONE        IFEQ      *BLANK
     C*
     C* FIND THE HIGHEST MINIMUM PREMIUM OVER ALL THE DIVISIONS.
     C*
     C                   Z-ADD     0             XXHIGH
     C                   Z-ADD     0             XLMIN1           11 2
     C                   Z-ADD     0             XLMIN2           11 2
     C                   Z-ADD     0             XXWVMN           11 2
     C*
     C     ELKY2         CHAIN(N)  PEL02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C*
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       ELWMIN        XXWVMN
     C                   ENDIF
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C*
     C* FIND HIGHEST INCR LIMIT MINIMUM WHILE WE ARE CHECKING.
     C*
     C     ELILCD        IFNE      *BLANKS
     C     ELPFDT        ANDNE     ELPTDT
     C     ELINL2        ORNE      *BLANKS
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C     ELILCD        IFNE      *BLANKS
     C                   MOVEL(P)  ELILCD        XXKEY
     C*
     C     LMKEY1        SETGT     PLM01
     C                   READP     PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     LMCO#
     C     XXFND         ANDEQ     LMFND
     C     XXST          ANDEQ     LMST
     C     XXKEY         ANDEQ     LMKEY
     C     XXEFDT        ANDGE     LMEFDT
     C*
     C     LMMIN         IFGT      XLMIN1
     C                   Z-ADD     LMMIN         XLMIN1
     C                   ENDIF                                                  LMMIN IFGT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELILCD IFNE
     C*
     C     ELINL2        IFNE      *BLANKS
     C                   MOVEL(P)  ELINL2        XXKEY
     C*
     C     LMKEY1        SETGT     PLM01
     C                   READP     PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     LMCO#
     C     XXFND         ANDEQ     LMFND
     C     XXST          ANDEQ     LMST
     C     XXKEY         ANDEQ     LMKEY
     C     XXEFDT        ANDGE     LMEFDT
     C*
     C     LMMIN         IFGT      XLMIN2
     C                   Z-ADD     LMMIN         XLMIN2
     C                   ENDIF                                                  LMMIN IFGT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELINL2 IFNE
     C*
     C                   ENDIF                                                  ELILCD IFNE
     C*
     C                   MOVE      '0'           *IN77
     C                   MOVEA     '11'          *IN(78)
     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     9999999       RDAT
     C                   Z-ADD     ELPFDT        PDAT(1)
     C                   Z-ADD     ELPFDT        RDAT(1)
     C                   Z-ADD     0             XXANDT
     C                   Z-ADD     2             X                 3 0
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C     ELANDT        IFNE      0
     C     ELANDT        ANDNE     ELPFDT
     C*
     C                   MOVE      ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C*
     C     XXANDT        IFGT      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C     XXANDT        IFGE      ELPTDT
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C                   MOVE      *OFF          *IN78
     C                   ENDIF
     C*
     C     *IN78         IFEQ      '0'
     C     ELEMD1        ANDEQ     ELANDT
     C     ELEMD2        ANDEQ     XXANDT
     C     ELEXM1        ANDNE     ELEXM2
     C                   MOVE      '0'           *IN79
     C                   ENDIF                                                  *IN78 IFEQ
     C*
     C     *IN78         IFEQ      '0'
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                    *IN78='0'
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C                   ADD       1             X
     C*
     C* CHECK IF ANY POLICY PERIODS NEED TO BE SPLIT FOR 4/1/3 RATES.
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        4             W                 2 0
     C     W             ADD       1             Z                 2 0
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFGE      1030101
     C     RDAT(W)       ANDLE     1030331
     C     PDAT(Z)       ANDGT     1030401
     C*
     C                   Z-ADD     1030401       PDAT(X)
     C                   Z-ADD     1030401       RDAT(X)
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* SORT DATE ARRAYS INTO SEQUENCE.
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C*
     C                   DO        4             X
     C*
     C     PDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         PDAT(X)
     C                   ENDIF
     C     RDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         RDAT(X)
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  READ EMPLOYER PAYROLL RECORDS
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C*
     C*  GET CORRECT DATE FROM RDAT ARRAY TO PULL RATE FROM RATE FILE
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   Z-ADD     RDAT(X)       XXEFDT
     C                   ELSE
     C                   Z-ADD     1             X
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   END
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C*  GET PAYROLL CLASS FILE
     C*
     C     PRKEY         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     PRMNPR        IFGT      XXHIGH
     C     EPAAPR        ANDNE     0
     C     ELFAF         ANDEQ     'A'
     C     PRMNPR        ORGT      XXHIGH
     C     XXST          ANDEQ     'NJ'
     C     PRMNPR        ORGT      XXHIGH
     C     ELFAF         ANDNE     'A'
     C                   Z-ADD     PRMNPR        XXHIGH
     C                   ENDIF                                                  PRMNPR IFGT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELKY2         READE(N)  PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* SET EMPLR DETAIL BACK TO STATE RPT DIVISION
     C*
     C                   Z-ADD     XSDIV         XXDIV
     C     ELKY1         CHAIN(N)  PEL02                              60
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C*
     C* IF AUDIT AND NO CLASS CODES DEVELOP EXPOSURE, CHECK 8810 MIN.
     C*
     C     XXHIGH        IFEQ      0
     C     ELFAF         ANDEQ     'A'
     C                   Z-ADD     8810          XXCLAS
     C*
     C     ELKY2         CHAIN(N)  PEL02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELPFDT        XXEFDT
     C                   MOVE(P)   ELRTCD        XXRTCD
     C     PRKEY         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRMNPR        ANDGT     XXHIGH
     C                   Z-ADD     PRMNPR        XXHIGH
     C                   ENDIF
     C*
     C     ELKY2         READE(N)  PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXHIGH IFEQ
     C*
     C* ADD INCR LIMITS AND WAIVER SURCHARGE MIN TO MIN PREM.
     C*
     C                   ADD       XXWVMN        XXHIGH
     C     XLMIN2        IFGT      XLMIN1
     C                   ADD       XLMIN2        XXHIGH
     C                   ELSE
     C                   ADD       XLMIN1        XXHIGH
     C                   ENDIF
     C*
     C* ADJUST MINIMUM PREMIUM FOR AUDIT RELATED ITEMS
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C* SET UP CX INFO
     C*
     C                   Z-ADD     1             XXX               1 0
     C                   MOVE      *BLANK        SVCNCL            1
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCNCL
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXX
     C                   ENDDO
     C*
     C* SAVE NCCI CANCEL CODE IF THE POLICY IS CANCELLED.
     C*
     C                   MOVE      *BLANK        XXCXCD            2
     C     SVCNCL        IFEQ      'Y'
     C*
     C     ELKY4         SETGT     PCX01
     C     ELKY4         READPE    PCX01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CXRSN         XXKEY
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRNCCI        XXCXCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C*
     C* PRORATE MINIMUM PREMIUM ONLY IF CANCELLED
     C*
     C     SVCNCL        IFEQ      'Y'
     C     ELPROR        ANDNE     0
     C     XXCXCD        ANDNE     '03'
     C     XXCXCD        ANDNE     '08'
     C*
     C     ELSRPR        IFEQ      'P'
     C     ELPFDT        ORLT      1000101
     C     XXST          OREQ      'MI'
     C                   MULT      ELPROR        XXHIGH
     C                   ENDIF                                                  ELSRPR IFEQ
     C                   END
     C*
     C* IF A FLAT CANCELLATION, PRORATE MIN PREMIUM DOWN TO ZERO.
     C*
     C     ELPFDT        IFEQ      ELPTDT
     C                   Z-ADD     0             XXHIGH
     C                   ENDIF                                                  ELPFDT IFEQ
     C*
     C     SVCNCL        IFNE      'Y'
     C     ELCOFL        ANDEQ     'Y'
     C                   MULT      ELPROR        XXHIGH
     C                   ENDIF                                                  SVCNCL IFNE
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD(H)  XXHIGH        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXHIGH
     C*
     C                   ELSE
     C*
     C     ELCOFL        IFEQ      'Y'
     C     XXHIGH        MULT(H)   ELPROR        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXHIGH
     C                   ENDIF                                                  SVCNCL IFNE
     C*
     C                   ENDIF                                                  ELFAF IFEQ
     C*
     C                   ENDIF                                                  XXDONE IFEQ
     C*
     C* UPDATE EACH EMPLR DETAIL RECORD WITH MIN PREM FOR THIS FYR.
     C*
     C     ELKY2         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELFAF         IFNE      'A'
     C                   Z-ADD     ELEMNA        XXSAVE           11 2
     C                   Z-ADD     XXHIGH        ELEMNA
     C                   ELSE
     C                   Z-ADD     ELAMNA        XXSAVE           11 2
     C                   Z-ADD     XXHIGH        ELAMNA
     C                   ENDIF
     C*
     C     XXHIGH        IFNE      XXSAVE
     C     ELFAF         ANDNE     *BLANK
     C                   MOVEL     Q1PGM         ELPGM
     C                   MOVEL     Q1USER        ELUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL02
     C                   ENDIF                                                  XXHIGH IFNE
     C*
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* READ NEXT FYR ON POLICY
     C*
     C     ELKY          READE     PEL01                                  51
     C                   ENDDO
     C*
     C* JUMP TO NEXT EMPLR
     C*
     C     EMKY          SETGT     PEM01
     C                   READ      PEM01                                  50
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
     C*
