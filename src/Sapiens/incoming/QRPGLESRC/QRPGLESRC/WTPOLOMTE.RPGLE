     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H/TITLE WTPOLOMTE  -  Owner/Officer endorsement processing
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO)
     H BNDDIR('WTBNDDIR')

      *===============================================================*
      *  Created   : 05/23/12                                         *
      *  Programmer: C:191                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 12/13/19 C:318 Support: 85221
      *               -Corrected dart remark for addition of owner
      *
      * 11/09/15 C:302 Phaseware: 38149                               *
      *                Include officers/owners on mid-term endorsement*
      *                only if their expiration date is greater than  *
      *                or equal to the effective date of the current  *
      *                policy year.                                   *
      *                                                               *
      * 05/07/15 C:267 Phaseware: 31902 CSI                           *
      *                Include officers/owners on mid-term endorsement*
      *                only if their expiration date is greater to the*
      *                effective date of the current policy year.     *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwtmyp     if   e             disk    rename(pmy01:pmy0p)
     fwmenl3    if a e           k disk    rename(pen01:pen03)
     fwmdxl     if   e           k disk    rename(pdx01:pdx00)
     fwmool     if   e           k disk    rename(poo01:poo00)
     FWDF2L     IF   E           K DISK    rename(pf201:pf200)

      /copy sprDate
      /copy wprTable
      /copy wprEmplr
      /copy wprGroup
      /copy wprAgent
     D psds          esds                  extname(szq1p)

     D sysdt7          s              7  0 inz
      ** BEFORE arrays
     D LN              S             30    DIM(500)                             Last name
     D FN              S             15    DIM(500)                             First name
     D NM              S             45    DIM(500)                             Both names
     D PC              S              5  2 DIM(500)                             %
     D IE              S              1    DIM(500)                             Include/Exclude
     D SQ              S              5  0 DIM(500)                             Sequence#
     D ST              S              1    DIM(500)                             Status
     D TL              S             30    DIM(500)                             Title
     D ED              S              7  0 DIM(500)                             Effective date
     D EX              S              7  0 DIM(500)                             Expiration date
     D XXBL            S             50    DIM(5)                               Before line

      ** AFTER arrays
     D LN2             S             30    DIM(500)                             Last name
     D FN2             S             15    DIM(500)                             First name
     D NM2             S             45    DIM(500)                             Both names
     D PC2             S              5  2 DIM(500)                             %
     D IE2             S              1    DIM(500)                             Include/Exclude
     D SQ2             S              5  0 DIM(500)                             Sequence#
     D ST2             S              1    DIM(500)                             Status
     D TL2             S             30    DIM(500)                             Title
     D ED2             S              7  0 DIM(500)                             Effective date
     D EX2             S              7  0 DIM(500)                             Expiration date
     D XXAL            S             50    DIM(5)                               After line

     D NEW             C                   CONST('** NEW **')

      ** Effective Dates
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0

      ** Cancellation Dates
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Cancel dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

      ** Midterm Information data structure
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      ** MM/DD/YYYY breakdown
     D                 DS
     D  XXDT8                  1      8
     D  XXMN                   1      2
     D  XXDY                   3      4
     D  XXYR                   5      8

      *=========================================================================
      *  PLIST definitions
      *=========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@EMRRN           9 0          PEM01 RRN
     C                   PARM                    @@ELRRN           9 0          PEL01 RRN
     C                   PARM                    @@MYRRN           9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0          ENDORSE DATE
     C                   PARM                    @@TYPE            1            Type of call
     C                   PARM                    @@ACX             1            Add/Chg/Delete

      *================================================================
      *  KLIST definitions
      *================================================================
      ** Owner/Officer file
     C     OOKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV

      ** Endors/DARTS Control Definition file
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND

      ** Endorsement Request workfile
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#

      *=========================================================================
      *  MAIN
      *=========================================================================
     C     *DTAARA       DEFINE    *LDA          SVLDA

     C                   MOVE      '/'           XXS               1
     C     @@EMrrn       Chain(n)  pem0p
     C     @@ELrrn       CHAIN(n)  pel0p                              99
     C     @@MYrrn       CHAIN(n)  pmy0p                              99

     C     DXKY          CHAIN     PF200                              60
     C     DXKY          CHAIN     PDX00                              60

     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PDX00
     C                   ENDIF

     C                   Select
     C                   When      @@Type = 'S'
     C                   Exsr      StartProcess
     C                   When      @@Type = 'E'
     C                   Exsr      EndProcess
     C                   SETON                                        LR
     C                   EndSl
     C                   Return
      **********************************************************
      * Start Processing
      **********************************************************
     C     StartProcess  BegSr

     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA

     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS

      ** Check to see if called as part of an Addition of Division
      ** Process if not an Addition of Division
      ** Also, do not write when an entity is added
     C     MYPGM1        IFNE      'W0051R19'
     C     MYPGM1        ANDNE     'W0051R15'

     C                   MOVE      'M'           XXMID             1

     C                   Z-ADD     1             X                 3 0
     C                   MOVEA     *BLANKS       LN
     C                   MOVEA     *BLANKS       FN
     C                   Z-ADD     *ZEROS        PC
     C                   MOVEA     *BLANKS       IE
     C                   Z-ADD     *ZEROS        SQ
     C                   MOVEA     *BLANKS       ST
     C                   MOVEA     *BLANKS       TL
     C                   Z-ADD     *ZEROS        ED
     C                   Z-ADD     *ZEROS        EX

     C     OOKY          CHAIN(n)  POO00                              60
     C     *IN60         DOWEQ     *OFF
        // *** Beg Add *** 05/07/15 **************************************
        // *** Beg Chg * 11/09/15
      *    OOEXDT        IFGT      ELPFDT
     C     OOEXDT        IFGE      ELPFDT
        // *** End Chg * 11/09/15
     C     OOEXDT        OREQ      *ZEROS
        // *** End Add *** 05/07/15 **************************************

     C     OOELID        IFEQ      *BLANKS
     C                   MOVEL     OOOLSN        LN(X)
     C                   MOVEL     OOOFSN        FN(X)
     C     FN(X)         CAT       LN(X):0       NM(X)
     C                   Z-ADD     OOOFPC        PC(X)
     C                   MOVEL     OOEXFL        IE(X)
     C                   Z-ADD     OOSEQ#        SQ(X)
     C                   MOVEL     OOTITL        TL(X)
     C                   Z-ADD     OOEFDT        ED(X)
     C                   Z-ADD     OOEXDT        EX(X)
     C                   ADD       1             X
     C                   ENDIF                                                  OOELID=' '
        // *** Beg Add *** 05/07/15 **************************************
     C                   ENDIF
        // *** End Add *** 05/07/15 **************************************
     C     OOKY          READE(n)  POO00                                  60
     C                   ENDDO

     C                   ENDIF                                                  W0051R19
     C                   ENDIF
     C                   EndSr
      **********************************************************
      * End   Processing
      **********************************************************
     C     EndProcess    BegSr

     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS

      ** Check to see if this is called as part of an Addition of Division
      ** Only write a WMENP record if not from Addition of Division
      ** Also, do not write when an entity is added
     C     MYPGM1        IFNE      'W0051R19'
     C     MYPGM1        ANDNE     'W0051R15'

     C                   Z-ADD     1             X
     C                   MOVEA     *BLANKS       LN2
     C                   MOVEA     *BLANKS       FN2
     C                   MOVEA     *ZEROS        PC2
     C                   MOVEA     *BLANKS       IE2
     C                   MOVEA     *ZEROS        SQ2
     C                   MOVEA     *BLANKS       ST2
     C                   MOVEA     *BLANKS       TL2
     C                   Z-ADD     *ZEROS        ED2
     C                   Z-ADD     *ZEROS        EX2

      ** Load the after snapshot
     C     OOKY          CHAIN(n)  POO00                              60
     C     *IN60         DOWEQ     *OFF
        // *** Beg Add *** 05/07/15 **************************************
        // *** Beg Chg * 11/09/15
      *    OOEXDT        IFGT      ELPFDT
     C     OOEXDT        IFGE      ELPFDT
        // *** End Chg * 11/09/15
     C     OOEXDT        OREQ      *ZEROS
        // *** End Add *** 05/07/15 **************************************

     C     OOELID        IFEQ      *BLANKS
     C                   MOVEL     OOOLSN        LN2(X)
     C                   MOVEL     OOOFSN        FN2(X)
     C     FN2(X)        CAT       LN2(X):0      NM2(X)
     C                   Z-ADD     OOOFPC        PC2(X)
     C                   MOVEL     OOEXFL        IE2(X)
     C                   Z-ADD     OOSEQ#        SQ2(X)
     C                   MOVEL     OOTITL        TL2(X)
     C                   Z-ADD     OOEFDT        ED2(X)
     C                   Z-ADD     OOEXDT        EX2(X)
     C                   ADD       1             X

     C                   ENDIF                                                  OOELID=' '
        // *** Beg Add *** 05/07/15 **************************************
     C                   ENDIF
        // *** End Add *** 05/07/15 **************************************
     C     OOKY          READE(n)  poo00                                  60
     C                   ENDDO

      ** Setup for next request number
     C     ENKY          SETGT     pen03
     C     ENKY          READPE    pen03                                  60
     C   60              Z-ADD     *ZERO         ENREQ#

      ** Load WMENP record
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#

     C                   EVAL      ENEEDT = dtCYMD(LDEEDT)

     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1

     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2

     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2

     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1

     C                   MOVE      *BLANKS       ENNCFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENFIFL
     C                   MOVE      *BLANKS       ENCNFL
     C                   MOVE      *BLANKS       ENATFL
     C                   MOVE      *BLANKS       ENC3FL
     C                   MOVE      *BLANKS       ENTBFL
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENNOBF

     C                   MOVEL     DXOONC        ENRMCD

       // *** Beg Add *** 12/13/19 **************************************
      /free
        if  @@ACX='A' ;
          ENRMCD =DXOOAD;
        endif;
      /end-free
       // *** End Add *** 12/13/19 **************************************
     C                   Eval      ENfnnm = GetGrpName(EMfnd)
     C                   Eval      sysDt7 = DateTo7(SystemDate())
     C                   Eval      ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:
     C                              ELfyr:sysdt7:ENagnm)
     C                   Eval      ENcar1 = F2car#
     C                   MOVEL     Q1USER        ENRUSR

     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3

     C                   Eval      ENdt = DateTo7(SystemDate())
     C                   Eval      ENtime = TimeTo6(SystemTime())
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   Z-ADD     ELPFDT        ENPFD1

     C                   Z-ADD     1             X
     C                   MOVE      *OFF          XXDONE            1
     C     X             DOWLT     500
     C     XXDONE        ANDEQ     *OFF

     C                   MOVE      *OFF          XXWRT

     C                   SELECT
      ** Addition of Owner/Officer
     C     @@ACX         WHENEQ    'A'

     C                   Z-ADD     1             Y                 3 0

     C                   MOVE      *ON           XXWRT             1
     C                   MOVE      'A'           ENNOFL

      ** Build BEFORE line
     C                   MOVEL     *BLANKS       XXBL

      ** Build AFTER line
     C                   MOVEL     *BLANKS       XXAL
     C                   Z-ADD     1             Z
     C                   MOVEL(P)  LN2(X)        XXWK25           25
     C     FN2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN2(X):1      XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         XXAL(Z)
     C                   ENDIF
     C                   MOVEL     PC2(X)        XXWK03            3
     C                   MOVE      PC2(X)        XXWK02            2
     C     PC2(X)        IFNE      *ZERO
     C     XXAL(Z)       ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      XXAL(Z)
     C                   CAT       '.':0         XXAL(Z)
     C                   CAT       XXWK02:0      XXAL(Z)
     C                   ENDIF
     C     IE2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       IE2(X):1      XXAL(Z)
     C                   ENDIF
     C     TL2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       TL2(X):1      XXAL(Z)
     C                   ENDIF

     C                   ADD       1             Z
     C     ED2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   EVAL      XXDT8 = %Editc(dtMdyy(ED2(X)):'X')
     C                   CAT       XXMN:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C     EX2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXAL(Z)
     C                   EVAL      XXDT8 = %Editc(dtMdyy(EX2(X)):'X')
     C                   CAT       XXMN:1        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C                   Z-ADD     1             Y                 3 0
     C     NM2(X)        LOOKUP    NM(Y)                                  61
     C     *IN61         IFEQ      *OFF
     C                   CAT       NEW:5         XXAL(Z)
     C                   ENDIF

      ** Deletion of Owner/Officer
     C     @@ACX         WHENEQ    'X'
     C                   Z-ADD     1             Y
     C     NM(X)         LOOKUP    NM2(Y)                                 61
     C     *IN61         IFEQ      *OFF
     C                   MOVE      *ON           XXWRT             1
     C                   MOVE      'D'           ENNOFL

      ** Build BEFORE line
     C                   MOVEL     *BLANKS       XXBL

      ** Build AFTER line
     C                   MOVEL     *BLANKS       XXAL
     C                   Z-ADD     1             Z
     C                   MOVEL(P)  LN(X)         XXWK25           25
     C     FN(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN(X):1       XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         XXAL(Z)
     C                   ENDIF
     C                   MOVEL     PC(X)         XXWK03            3
     C                   MOVE      PC(X)         XXWK02            2
     C     PC(X)         IFNE      *ZERO
     C     XXAL(Z)       ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      XXAL(Z)
     C                   CAT       '.':0         XXAL(Z)
     C                   CAT       XXWK02:0      XXAL(Z)
     C                   ENDIF
     C     IE(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       IE(X):1       XXAL(Z)
     C                   ENDIF
     C     TL(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       TL(X):1       XXAL(Z)
     C                   ENDIF
     C                   ADD       1             Z
     C     ED(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   EVAL      XXDT8 = %Editc(dtMdyy(ED(X)):'X')
     C                   CAT       XXMN:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C     EX(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXAL(Z)
     C                   EVAL      XXDT8 = %Editc(dtMdyy(EX(X)):'X')
     C                   CAT       XXMN:1        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF

     C                   ENDIF                                                  *IN61 = *OFF

      ** Change of Owner/Officer
     C     @@ACX         WHENEQ    'C'
     C     FN(X)         IFNE      FN2(X)
     C     LN(X)         ORNE      LN2(X)
     C     PC(X)         ORNE      PC2(X)
     C     IE(X)         ORNE      IE2(X)
     C     TL(X)         ORNE      TL2(X)
     C     ED(X)         ORNE      ED2(X)
     C     EX(X)         ORNE      EX2(X)

     C                   MOVE      *ON           XXWRT             1
     C                   MOVE      'C'           ENNOFL

      ** Build BEFORE line
     C                   MOVEL     *BLANKS       XXBL
     C                   Z-ADD     1             Z                 3 0
     C                   MOVEL(P)  LN(X)         XXWK25           25
     C     FN(X)         IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN(X):1       XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT       ',':0         XXBL(Z)
     C                   ENDIF
     C                   MOVEL     PC(X)         XXWK03            3
     C                   MOVE      PC(X)         XXWK02            2
     C     PC(X)         IFNE      *ZERO
     C     XXBL(Z)       ORNE      *BLANK
     C                   CAT       XXWK03:1      XXBL(Z)
     C                   CAT       '.':0         XXBL(Z)
     C                   CAT       XXWK02:0      XXBL(Z)
     C                   ENDIF
     C     IE(X)         IFNE      *BLANK
     C                   CAT       ',':0         XXBL(Z)
     C                   CAT       IE(X):1       XXBL(Z)
     C                   ENDIF
     C     TL(X)         IFNE      *BLANK
     C                   CAT       ',':0         XXBL(Z)
     C                   CAT       TL(X):1       XXBL(Z)
     C                   ENDIF

     C                   ADD       1             Z
     C     ED(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   EVAL      XXDT8 = %Editc(dtMdyy(ED(X)):'X')
     C                   CAT       XXMN:0        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXDY:0        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXYR:0        XXBL(Z)
     C                   ENDIF
     C     EX(X)         IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXBL(Z)
     C                   EVAL      XXDT8 = %Editc(dtMdyy(EX(X)):'X')
     C                   CAT       XXMN:1        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXDY:0        XXBL(Z)
     C                   CAT       XXS:0         XXBL(Z)
     C                   CAT       XXYR:0        XXBL(Z)
     C                   ENDIF

      ** Build AFTER line
     C                   MOVEL     *BLANKS       XXAL
     C                   Z-ADD     1             Z
     C                   MOVEL(P)  LN2(X)        XXWK25           25
     C     FN2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXWK25           25
     C                   CAT(P)    FN2(X):1      XXWK25
     C                   ENDIF
     C     XXWK25        IFNE      *BLANK
     C     XXWK25        CAT(P)    ',':0         XXAL(Z)
     C                   ENDIF
     C                   MOVEL     PC2(X)        XXWK03            3
     C                   MOVE      PC2(X)        XXWK02            2
     C     PC2(X)        IFNE      *ZERO
     C     XXAL(Z)       ORNE      *BLANK
     C                   CAT(P)    XXWK03:1      XXAL(Z)
     C                   CAT       '.':0         XXAL(Z)
     C                   CAT       XXWK02:0      XXAL(Z)
     C                   ENDIF
     C     IE2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       IE2(X):1      XXAL(Z)
     C                   ENDIF
     C     TL2(X)        IFNE      *BLANK
     C                   CAT(P)    ',':0         XXAL(Z)
     C                   CAT       TL2(X):1      XXAL(Z)
     C                   ENDIF

     C                   ADD       1             Z
     C     ED2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   EVAL      XXDT8 = %Editc(dtMdyy(ED2(X)):'X')
     C                   CAT       XXMN:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF
     C     EX2(X)        IFNE      *ZEROS
     C                   CLEAR                   XXDT8
     C                   CAT       ' - ':0       XXAL(Z)
     C                   EVAL      XXDT8 = %Editc(dtMdyy(EX2(X)):'X')
     C                   CAT       XXMN:1        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXDY:0        XXAL(Z)
     C                   CAT       XXS:0         XXAL(Z)
     C                   CAT       XXYR:0        XXAL(Z)
     C                   ENDIF

     C                   ENDIF                                                  FN,X != FN2,X

     C                   ENDSL

     C     XXWRT         IFEQ      *ON
     C                   Z-ADD     1             Z
     C     Z             DOWLT     3
     C     XXBL(Z)       IFNE      *BLANKS
     C     XXAL(Z)       ORNE      *BLANKS
     C                   MOVEL     XXBL(Z)       ENBLIN
     C                   MOVEL     XXAL(Z)       ENALIN
     C                   WRITE     pen03
     C                   ADD       1             ENTIME
     C                   ADD       1             ENSEQ#
     C                   ADD       1             Z
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO

     C                   ELSE

     C     NM(X)         IFEQ      *BLANKS
     C     NM2(X)        ANDEQ     *BLANKS
     C                   MOVE      *ON           XXDONE
     C                   ENDIF

     C                   ENDIF

     C                   ADD       1             X
     C                   ENDDO                                                  DOW<=500

     C                   ENDIF                                                  ##EXST *OFF

     C                   ENDIF                                                  LDENDA<>' '
     C                   EndSr
