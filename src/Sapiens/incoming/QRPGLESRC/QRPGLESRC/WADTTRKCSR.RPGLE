     H*copyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H* StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H* To process the subroutines in this copybook, it is assumed    *
     H*  that the Employer Master & Detail records have already been  *
     H*  retrieved.                                                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/04/11 C:136 Support: WA045 75953                           *
     H*               -Use SREFFD to pull rating date                 *
     H*                                                               *
     H* 02/11/10 C:191 Project: 25WRC                                 *
     H*               -Convert to RPGLE                               *
     H*          C:33 -Remove comments                                *
     H*                                                               *
     H* 09/04/08 C:62  PROJECT WC1611                                 *
     H*                USE ELNWRW                                     *
     H*                                                               *
     H* 11/30/07  C:33  Project: WC1196                               *
     H*                -If no History record was found but the        *
     H*                  Business Category has been assigned in Emp   *
     H*                  Detail, use it                               *
     H*                                                               *
     H* 09/08/07  C:33  Project: WC1196                               *
     H*                -Use new Employer Detail flags to determine    *
     H*                  Audit Co.                                    *
     H*                                                               *
     H* 10/02/06  C:33  Project: WC1196                               *
     H*                -Created.                                      *
     H*                -Add subroutine (ASGNAC) for assigning the     *
     H*                  Audit Company                                *
     H*                -Add subroutine (ASGNAT) for assigning the     *
     H*                  Audit Type                                   *
     H*                -Add subroutine (ASGNBC) for assigning the     *
     H*                  Business Category                            *
     H*****************************************************************
     C*
     C     *LIKE         DEFINE    *IN60         SVIN60
     C     *LIKE         DEFINE    *IN60         GOOD
     C*
     C*===============================================================
     C* Accumulate Premium values
     C*===============================================================
     C     ACCUPR        BEGSR
     C*
     C     *LIKE         DEFINE    ELAAPR        X9PREM
     C*
     C                   SELECT
     C     ELFAF         WHENEQ    'A'
     C                   ADD       ELAAPR        X9PREM
     C                   OTHER
     C                   ADD       ELANPR        X9PREM
     C                   ENDSL
     C*
     C                   ENDSR                                                  ACCUPR
     C*
     C*===============================================================
     C* Assign Audit Company
     C*===============================================================
     C     ASGNAC        BEGSR
     C*
     C     *LIKE         DEFINE    AAST          X9ST
     C     *LIKE         DEFINE    AACNTY        XXCNTY
     C     *LIKE         DEFINE    AAPRML        XXPRMA
     C     *LIKE         DEFINE    AAACO         XXACO
     C     *LIKE         DEFINE    AAST          K9ST
     C     *LIKE         DEFINE    AACNTY        K9CNTY
     C     *LIKE         DEFINE    AAPRML        K9PRMA
     C     *LIKE         DEFINE    AATYPA        K9TYPA
     C*
     C     AAKYT         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXTYPA
     C                   KFLD                    X9ST
     C                   KFLD                    X9CNTY
     C*
     C     AAKY1T        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXTYPA
     C                   KFLD                    X9ST
     C                   KFLD                    X9CNTY
     C                   KFLD                    XXPRMA
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C                   EXSR      GTRPDV
     C                   EXSR      GETST
     C                   EXSR      RTVXHP
     C*
     C                   MOVE      *BLANKS       XXACO
     C                   MOVE      XHTYPA        XXTYPA
     C*
     C                   EXSR      RTVPRM
     C*
     C                   MOVE      XHTYPA        K9TYPA
     C                   MOVE      X9ST          K9ST
     C                   MOVE      X9CNTY        K9CNTY
     C*
     C                   MOVE      *OFF          GOOD
     C*
     C     GOOD          DOWEQ     *OFF
     C*
     C     AAKY1T        SETGT     PAA01T
     C     AAKYT         READPE    PAA01T                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     XXPRMA        IFLE      AAPRMH
     C     AASTAT        ANDEQ     'A'
     C                   MOVE      *ON           GOOD
     C                   LEAVE
     C                   ENDIF
     C*
     C     AAKYT         READPE    PAA01T                                 60
     C*
     C                   ENDDO
     C*
     C     GOOD          IFEQ      *OFF
     C                   SELECT
     C     X9CNTY        WHENNE    *BLANKS
     C                   MOVE      *BLANKS       X9CNTY
     C     X9ST          WHENNE    *BLANKS
     C                   MOVE      *BLANKS       X9ST
     C     XXTYPA        WHENNE    *BLANKS
     C                   MOVE      *BLANKS       XXTYPA
     C                   OTHER
     C                   LEAVE
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   SELECT
     C     GOOD          WHENEQ    *OFF
     C                   ADD       1             I9
     C                   MOVEL     MSGAA         MSG(I9)
     C     K9TYPA        CAT       K9ST:1        INF(I9)
     C                   CAT       K9CNTY:1      INF(I9)
     C     GOOD          WHENEQ    *ON
     C                   MOVE      AAACO         XXACO
     C                   ENDSL
     C*
     C     XXACO         IFNE      *BLANKS
     C                   EXSR      WRTXHP
     C                   ENDIF
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR                                                  ASGNAC
     C*
     C*===============================================================
     C* Assign Audit Type
     C*===============================================================
     C     ASGNAT        BEGSR
     C*
     C     *LIKE         DEFINE    SSNRBS        XXNRFL
     C     *LIKE         DEFINE    SSBCTG        XXBCTG
     C     *LIKE         DEFINE    SSTYPA        XXTYPA
     C     *LIKE         DEFINE    SSNRBS        K9NRFL
     C     *LIKE         DEFINE    SSBCTG        K9BCTG
     C*
     C     SSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXBCTG
     C                   KFLD                    XXNRFL
     C*
     C     SSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXBCTG
     C                   KFLD                    XXNRFL
     C                   KFLD                    XXPRMA
     C*
     C                   MOVE      *IN60         SVIN60
     C                   Z-ADD     *ZERO         I9                3 0
     C                   CLEAR                   MSG
     C                   CLEAR                   INF
     C*
     C                   EXSR      GTRPDV
     C                   EXSR      RTVXHP
     C                   EXSR      RTVPRM
     C*
     C                   MOVE      *BLANKS       XXTYPA
     C                   MOVE      ELNWRW        XXNRFL
     C                   MOVE      XHBCTG        XXBCTG
     C*
     C                   MOVE      ELNWRW        K9NRFL
     C                   MOVE      XHBCTG        K9BCTG
     C*
     C                   MOVE      *OFF          GOOD
     C*
     C     GOOD          DOWEQ     *OFF
     C*
     C     SSKY1         SETGT     PSS01
     C     SSKY          READPE    PSS01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     XXPRMA        IFLE      SSPRMH
     C     SSSTAT        ANDEQ     'A'
     C                   MOVE      *ON           GOOD
     C                   LEAVE
     C                   ENDIF
     C*
     C     SSKY          READPE    PSS01                                  60
     C*
     C                   ENDDO
     C*
     C     GOOD          IFEQ      *OFF
     C                   SELECT
     C     XXBCTG        WHENNE    *BLANKS
     C                   MOVE      *BLANKS       XXBCTG
     C     XXNRFL        WHENNE    *BLANKS
     C                   MOVE      *BLANKS       XXNRFL
     C                   OTHER
     C                   LEAVE
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   SELECT
     C     GOOD          WHENEQ    *OFF
     C                   ADD       1             I9
     C                   MOVEL     MSGSS         MSG(I9)
     C     K9NRFL        CAT       K9BCTG:1      INF(I9)
     C     GOOD          WHENEQ    *ON
     C                   MOVE      SSTYPA        XXTYPA
     C     XHTYPA        WHENNE    *BLANKS
     C                   MOVE      XHTYPA        XXTYPA
     C                   ENDSL
     C*
     C     XXTYPA        IFNE      *BLANKS
     C                   EXSR      WRTXHP
     C                   ENDIF
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR                                                  ASGNAT
     C*
     C*===============================================================
     C* Assign Business Category
     C*===============================================================
     C     ASGNBC        BEGSR
     C*
     C     *LIKE         DEFINE    B3GOVN        XXGOVN
     C     *LIKE         DEFINE    B3BCTG        X3BCTG
     C*
     C     RRKY99        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    X9ST
      *** beg chg ***** 03/04/11 *******************************************
     C***********        KFLD                    XXEFDT
     C                   KFLD                    X9EFDT
      *** end chg ***** 03/04/11 *******************************************
     C*
     C     RRKY91        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    X9ST
     C*
     C     B3KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    RRBAFL
     C                   KFLD                    XXGOVN
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C                   CLEAR                   XXBCTG
     C*
     C                   EXSR      GTRPDV
     C                   EXSR      GETST
     C*
     C     RRKY99        SETGT     PRR01
     C     RRKY91        READPE    PRR01                                  60
     C*
     C                   EXSR      RTVXHP
     C*
     C                   SELECT
     C     RRBAFL        WHENEQ    'G'
     C                   MOVEL     X9GOVC        XXGOVN
     C     RRBAFL        WHENEQ    'S'
     C                   MOVEL     X9SIC         XXGOVN
     C     RRBAFL        WHENEQ    'N'
     C                   MOVEL     X9NAIC        XXGOVN
     C                   OTHER
     C                   MOVE      *BLANKS       XXGOVN
     C                   ENDSL
     C*
     C                   CLEAR                   B3BCTG
     C     B3KY          CHAIN     PB300                              60
     C     *IN60         DOWEQ     *OFF
     C     B3STAT        ANDNE     'A'
     C                   CLEAR                   B3BCTG
     C     B3KY          READE     PB300                                  60
     C                   ENDDO
     C                   MOVE      B3BCTG        X3BCTG
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C     X3BCTG        IFNE      *BLANKS
     C                   EXSR      WRTXHP
     C                   ENDIF
     C*
     C                   ENDSR                                                  ASGNBC
     C*
     C*===============================================================
     C* Get the Jursisdiction State
     C*===============================================================
     C     GETST         BEGSR
     C*
     C     RSKY99        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    X9RTCD
     C*
     C     *LIKE         DEFINE    ELRTCD        X9RTCD
     C*
     C                   EXSR      GTRPDV
     C*
     C                   CLEAR                   RSJRST
     C     RSKY99        CHAIN     PRS01                              60
     C                   MOVE      RSJRST        X9ST
     C*
     C     *IN60         IFEQ      *ON
     C                   ADD       1             I9
     C                   MOVEL     MSGRS         MSG(I9)
     C                   MOVEL     X9RTCD        INF(I9)
     C                   ENDIF
     C*
     C                   ENDSR                                                  GETST
     C*
     C*===============================================================
     C* Get the State Report Division
     C*===============================================================
     C     GTRPDV        BEGSR
     C*
     C     *LIKE         DEFINE    EMDIV         X9RPDV
     C     *LIKE         DEFINE    EMCNTY        X9CNTY
     C     *LIKE         DEFINE    ELGOVC        X9GOVC
     C     *LIKE         DEFINE    EMSIC         X9SIC
     C     *LIKE         DEFINE    EMNAIC        X9NAIC
      *** beg add ***** 03/04/11 *******************************************
     C     *LIKE         DEFINE    ELPFDT        X9EFDT
      *** end add ***** 03/04/11 *******************************************
     C*
     C     EMKY91        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C*
     C     EMKY92        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    SVDIV9
     C*
     C     ELKY93        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    X9RPDV
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C                   Z-ADD     ELDIV         SVDIV9
     C                   Z-ADD     ELDIV         X9RPDV
     C*
     C                   SELECT
     C     ELAAFL        WHENNE    'Y'
     C* Get State Reporting Division info
     C*
     C     EMKY91        CHAIN     WMEML                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY91        READE     WMEML                                  60
     C                   ENDDO
     C*
     C                   SELECT
     C     EMSTRP        WHENEQ    'Y'
     C                   Z-ADD     EMDIV         X9RPDV
     C                   OTHER
     C                   Z-ADD     SVDIV9        X9RPDV
     C     EMKY92        CHAIN     WMEML                              60
     C                   ENDSL
     C*
     C     ELKY93        CHAIN     WDELL1                             60
     C*
     C                   ENDSL
     C*
     C                   MOVE      EMCNTY        X9CNTY
     C                   MOVE      ELRTCD        X9RTCD
     C                   MOVE      ELGOVC        X9GOVC
     C                   MOVE      EMSIC         X9SIC
     C                   MOVE      EMNAIC        X9NAIC
      *** beg add ***** 03/04/11 *******************************************
     C                   move(p)   ELCO#         #9co#             3
     C                   move(p)   ELFND         #9fnd             3
     C                   move(p)   ELFYR         #9fyr             3
     C                   move(p)   ELEMP#        #9emp#            9
     C                   move(p)   ELDIV         #9div             5
     C                   move(p)   ELPFDT        #9date            7
     C                   move(p)   *BLANKS       #9type            1
     C                   move(p)   *blanks       #9clas            5
      *
     C                   call      'SREFFD'
     C                   parm                    #9co#
     C                   parm                    #9fnd
     C                   parm                    #9fyr
     C                   parm                    #9emp#
     C                   parm                    #9div
     C                   parm                    #9date
     C                   parm                    #9type
     C                   parm                    #9clas
      *
     C                   move      #9date        x9efdt
      *** end add ***** 03/04/11 *******************************************
     C*
     C     ELKY92        CHAIN     WDELL1                             60
     C     EMKY92        CHAIN     WMEML                              60
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR                                                  GTRPDV
     C*
     C*===============================================================
     C* Retrieve Employer Detail History record
     C*===============================================================
     C     RTVEHP        BEGSR
     C*
     C     EHKY14        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    X9RPDV
     C                   KFLD                    ELFYR
     C*
     C                   EXSR      GTRPDV
     C*
     C                   MOVE      *BLANKS       EHNWRW
     C*
     C     EHKY14        SETGT     PEH14
     C     EHKY14        READPE    PEH14                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   ADD       1             I9
     C                   MOVEL     MSGEH         MSG(I9)
     C                   ENDIF
     C*
     C                   ENDSR                                                  RTVEHP
     C*
     C*===============================================================
     C* Retrieve Audit Co/Business Category Assignment History record
     C*===============================================================
     C     RTVXHP        BEGSR
     C*
     C     *LIKE         DEFINE    XHTRDT        X7DT
     C     *LIKE         DEFINE    XHTRDT        SYSDT7
     C*
     C     XHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    X7DT
     C*
     C     XHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C                   CLEAR                   XHTRDT
     C                   Z-ADD     *HIVAL        X7DT
     C*
     C     XHKY          SETGT     PXH01
     C     XHKY1         READPE    PXH01                                  60
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT7
     C*
     C                   SELECT
     C     *IN60         WHENEQ    *ON
     C                   CLEAR                   XHRRN
     C                   MOVEL     ELBCTG        XHBCTG
     C     XHTRDT        WHENNE    SYSDT7
     C                   CLEAR                   XHRRN
     C                   MOVE      XHUSFL        RRBAFL
     C                   MOVE      XHBCTG        X3BCTG
     C                   MOVE      XHACO         XXACO
     C                   MOVE      XHTYPA        XXTYPA
     C     *IN60         WHENEQ    *OFF
     C                   ENDSL
     C*
     C                   ENDSR                                                  RTVXHP
     C*
     C*===============================================================
     C* Retrieve Premium value
     C*===============================================================
     C     RTVPRM        BEGSR
     C*
     C     *LIKE         DEFINE    ELDIV         SVDIV9
     C*
     C     ELKY91        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C*
     C     ELKY92        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    SVDIV9
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C                   Z-ADD     ELDIV         SVDIV9
     C                   CLEAR                   X9PREM
     C*
     C                   SELECT
     C     ELAPFL        WHENEQ    'Y'
     C* Loop thru all divisions (WDELL1)
     C     ELKY91        CHAIN     WDELL1                             60
     C     *IN60         DOWEQ     *OFF
     C                   EXSR      ACCUPR
     C     ELKY91        READE     WDELL1                                 60
     C                   ENDDO
     C                   OTHER
     C                   EXSR      ACCUPR
     C                   ENDSL
     C*
     C                   Z-ADD     X9PREM        XXPRMA
     C*
     C     ELKY92        CHAIN     WDELL1                             60
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR                                                  RTVPRM
     C*
     C*===============================================================
     C* Write and Audit Co/Business Category Assignment History record
     C*===============================================================
     C     WRTXHP        BEGSR
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C     XHRRN         CHAIN     PXH0P                              60
     C*
     C                   Z-ADD     ELCO#         XHCO#
     C                   Z-ADD     ELFND         XHFND
     C                   Z-ADD     ELEMP#        XHEMP#
     C                   Z-ADD     ELDIV         XHDIV
     C                   Z-ADD     ELFYR         XHFYR
     C                   Z-ADD     SYSDT7        XHTRDT
     C                   MOVE      XXNRFL        XHNRFL
     C                   Z-ADD     XXPRMA        XHPREM
     C*
     C     X3BCTG        IFNE      *BLANKS
     C                   MOVE      RRBAFL        XHUSFL
     C                   MOVE      EMNAIC        XHNAIC
     C                   MOVE      EMSIC         XHSIC
     C                   MOVE      ELGOVC        XHGOVN
     C                   MOVE      X3BCTG        XHBCTG
     C                   MOVEL     XHBCTG        ELBCTG
     C                   ENDIF
     C*
     C     XXACO         IFNE      *BLANKS
     C                   MOVE      XXACO         XHACO
     C                   MOVE      X9ST          XHST
     C                   MOVE      XXCNTY        XHCNTY
     C                   ENDIF
     C*
     C     XXTYPA        IFNE      *BLANKS
     C                   MOVE      XXTYPA        XHTYPA
     C                   ENDIF
     C*
     C                   MOVEL     Q1PGM         XHPGM
     C                   MOVEL     Q1USER        XHUSER
     C                   Z-ADD     DSTIME        XHTIME
     C                   Z-ADD     SYSDT7        XHDT
     C*
     C                   SELECT
     C     *IN60         WHENEQ    *OFF
     C                   UPDATE    PXH0P
     C                   OTHER
     C                   MOVEL     XHPGM         XHIPGM
     C                   MOVEL     XHUSER        XHIUSR
     C                   Z-ADD     XHTIME        XHITIM
     C                   Z-ADD     XHDT          XHIDT
     C                   WRITE     PXH0P
     C                   ENDSL
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR
