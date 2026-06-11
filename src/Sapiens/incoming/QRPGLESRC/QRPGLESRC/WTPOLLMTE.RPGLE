     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WTPOLLEND  -  Location/Entity endorsement processing
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 05/23/12                                         *
    *  Programmer: C:191                                            *
    *  Project   : WC2116                                           *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
    *  10/21/21 C:65  Project: Phw#98859 (Amwins)                   *
    *                 Increase array sizes from 500 to 9999 to      *
    *                 accomodate policies with more than 500 addl   *
    *                 locations.                                    *
    *                                                               *
      *  09/13/13 C:283 Support  WA133 83281                          *
      *                 Change the Endorsement Process for Entity/Loc *
      *                 add to use '+' in the Alstat. "A" should only *
      *                 be used to designate 'Active'.                *
      *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwtmyp     if   e             disk    rename(pmy01:pmy0p)
     fwmenl3    if a e           k disk    rename(pen01:pen03)
     FWDF2L     IF   E           K DISK    rename(pf201:pf200)
     fwmdxl     if   e           k disk    rename(pdx01:pdx00)
     fwmall     uf   e           k disk    rename(pal01:pal00)

      /copy sprDate
      /copy wprTable
      /copy wprEmplr
      /copy wprGroup
      /copy wprAgent
     D psds          esds                  extname(szq1p)
     D*
     D sysdt7          s              7  0 inz
     D*** begin chg ***  10/21/21  *******************************
     D*** NM              S             50    DIM(500)
     D*** N2              S             50    DIM(500)
     D*** NM2             S             50    DIM(500)
     D*** N22             S             50    DIM(500)
     D*** FN              S             50    DIM(500)
     D*** F2              S             50    DIM(500)
     D*** FN2             S             50    DIM(500)
     D*** F22             S             50    DIM(500)
     D*** P1              S             40    DIM(500)
     D*** P2              S             40    DIM(500)
     D*** P3              S             40    DIM(500)
     D*** PC              S             25    DIM(500)
     D*** PS              S              2    DIM(500)
     D*** PZ              S              9  0 DIM(500)
     D*** CN              S             25    DIM(500)
     D*** M1              S             40    DIM(500)
     D*** M2              S             40    DIM(500)
     D*** M3              S             40    DIM(500)
     D*** MC              S             25    DIM(500)
     D*** MS              S              2    DIM(500)
     D*** MZ              S              9  0 DIM(500)
     D*** FE              S              9  0 DIM(500)
     D*** TB              S             20    DIM(500)
     D*** ID              S             10    DIM(500)
     D*** DD              S              7  0 DIM(500)
     D*** DR              S             30    DIM(500)
     D*** NA              S              1    DIM(500)
     D*** MA              S              1    DIM(500)
     D*** PA              S              1    DIM(500)
     D*** CA              S              1    DIM(500)
     D*** FI              S              1    DIM(500)
     D*** P12             S             40    DIM(500)
     D*** P22             S             40    DIM(500)
     D*** P32             S             40    DIM(500)
     D*** PC2             S             25    DIM(500)
     D*** PS2             S              2    DIM(500)
     D*** PZ2             S              9  0 DIM(500)
     D*** CN2             S             25    DIM(500)
     D*** M12             S             40    DIM(500)
     D*** M22             S             40    DIM(500)
     D*** M32             S             40    DIM(500)
     D*** MC2             S             25    DIM(500)
     D*** MS2             S              2    DIM(500)
     D*** MZ2             S              9  0 DIM(500)
     D*** FE2             S              9  0 DIM(500)
     D*** TB2             S             20    DIM(500)
     D*** ID2             S             10    DIM(500)
     D*** DD2             S              7  0 DIM(500)
     D*** DR2             S             30    DIM(500)
     D*** ST2             S              1    DIM(500)
     D*** TP2             S              1    DIM(500)
     D NM              S             50    DIM(9999)
     D N2              S             50    DIM(9999)
     D NM2             S             50    DIM(9999)
     D N22             S             50    DIM(9999)
     D FN              S             50    DIM(9999)
     D F2              S             50    DIM(9999)
     D FN2             S             50    DIM(9999)
     D F22             S             50    DIM(9999)
     D P1              S             40    DIM(9999)
     D P2              S             40    DIM(9999)
     D P3              S             40    DIM(9999)
     D PC              S             25    DIM(9999)
     D PS              S              2    DIM(9999)
     D PZ              S              9  0 DIM(9999)
     D CN              S             25    DIM(9999)
     D M1              S             40    DIM(9999)
     D M2              S             40    DIM(9999)
     D M3              S             40    DIM(9999)
     D MC              S             25    DIM(9999)
     D MS              S              2    DIM(9999)
     D MZ              S              9  0 DIM(9999)
     D FE              S              9  0 DIM(9999)
     D TB              S             20    DIM(9999)
     D ID              S             10    DIM(9999)
     D DD              S              7  0 DIM(9999)
     D DR              S             30    DIM(9999)
     D NA              S              1    DIM(9999)
     D MA              S              1    DIM(9999)
     D PA              S              1    DIM(9999)
     D CA              S              1    DIM(9999)
     D FI              S              1    DIM(9999)
     D P12             S             40    DIM(9999)
     D P22             S             40    DIM(9999)
     D P32             S             40    DIM(9999)
     D PC2             S             25    DIM(9999)
     D PS2             S              2    DIM(9999)
     D PZ2             S              9  0 DIM(9999)
     D CN2             S             25    DIM(9999)
     D M12             S             40    DIM(9999)
     D M22             S             40    DIM(9999)
     D M32             S             40    DIM(9999)
     D MC2             S             25    DIM(9999)
     D MS2             S              2    DIM(9999)
     D MZ2             S              9  0 DIM(9999)
     D FE2             S              9  0 DIM(9999)
     D TB2             S             20    DIM(9999)
     D ID2             S             10    DIM(9999)
     D DD2             S              7  0 DIM(9999)
     D DR2             S             30    DIM(9999)
     D ST2             S              1    DIM(9999)
     D TP2             S              1    DIM(9999)
     D*** end chg *****  10/21/21  *******************************

     D XXDLT           S              1    INZ
     D xxTbid          s              4    inz
     D xxKey           s             10    inz

     D XXDES1          C                   CONST('Federal ID #:')
     D XXDES2          C                   CONST('Ent/Loc ID #:')

     D                 DS
     D  XXZIPX                 1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
     C*=========================================================================
     C*  PLIST definitions
     C*=========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@EMRRN           9 0          PEM01 RRN
     C                   PARM                    @@ELRRN           9 0          PEL01 RRN
     C                   PARM                    @@MYRRN           9 0          PMY01 RRN
     C                   PARM                    @@ENDT            8 0          ENDORSE DATE
     C                   PARM                    @@TYPE            1            Type of call
     C                   PARM                    @@DLT             1            Delete flag
     C*
     C*=========================================================================
     C*  KLIST definitions
     C*=========================================================================
     C     ALKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#

     C*  ENDORS/DARTS CONTROL DEFINITION
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*=========================================================================
     C*  MAIN
     C*=========================================================================
     C     *DTAARA       DEFINE    *LDA          SVLDA

     C                   MOVE      @@DLT         XXDLT
     C     @@EMrrn       Chain(n)  pem0p
     C     @@ELrrn       CHAIN(n)  pel0p                              99
     C     @@MYrrn       CHAIN(n)  pmy0p                              99
     C*
     C     DXKY          CHAIN     PF200                              60
     C     DXKY          CHAIN     PDX00                              60
     C*
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
     C**********************************************************
     C* Start Processing
     C**********************************************************
     C     StartProcess  BegSr
     C*
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   Z-ADD     1             X
     C                   MOVEA     *BLANKS       NM
     C                   MOVEA     *BLANKS       N2
     C                   MOVEA     *BLANKS       FN
     C                   MOVEA     *BLANKS       F2
     C                   MOVEA     *BLANKS       P1
     C                   MOVEA     *BLANKS       P2
     C                   MOVEA     *BLANKS       P3
     C                   MOVEA     *BLANKS       PC
     C                   MOVEA     *BLANKS       PS
     C                   MOVEA     *ZEROS        PZ
     C                   CLEAR                   CN
     C                   MOVEA     *BLANKS       M1
     C                   MOVEA     *BLANKS       M2
     C                   MOVEA     *BLANKS       M3
     C                   MOVEA     *BLANKS       MC
     C                   MOVEA     *BLANKS       MS
     C                   MOVEA     *ZEROS        MZ
     C                   MOVEA     *ZEROS        FE
     C                   MOVEA     *BLANKS       TB
     C                   MOVEA     *BLANKS       ID
     C                   MOVEA     *ZEROS        DD
     C                   MOVEA     *BLANKS       DR
     C     ALKY          CHAIN(N)  PAL00                              60
     C     *IN60         DOWEQ     '0'
      *
     C                   MOVEL(P)  ALNAME        NM(X)
     C                   MOVEL(P)  ALNAM2        XXNM10           10
     C                   MOVE      XXNM10        NM(X)
     C                   MOVE      ALNAM2        N2(X)
      *
     C                   MOVEL(P)  ALFDNM        FN(X)
     C                   MOVEL(P)  ALFDN2        XXNM10           10
     C                   MOVE      XXNM10        FN(X)
     C                   MOVE      ALFDN2        F2(X)
      *
     C                   MOVEL     ALPAD1        P1(X)
     C                   MOVEL     ALPAD2        P2(X)
     C                   MOVEL     ALPCTY        PC(X)
     C                   MOVEL     ALPST         PS(X)
     C                   Z-ADD     ALPZIP        PZ(X)
     C                   EXSR      GTCNTY
     C                   MOVEL(P)  XXCNTY        CN(X)
     C                   MOVEL     ALPZIP        XXZIPX
     C     ALPCTY        IFNE      *BLANKS
     C     ALPST         ORNE      *BLANKS
     C     ALPCTY        CAT(P)    ',':0         P3(X)
     C                   CAT       ALPST:1       P3(X)
     C                   CAT       XXZIP5:1      P3(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         P3(X)
     C                   CAT       XXZIP4:0      P3(X)
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     ALMAD1        M1(X)
     C                   MOVEL     ALMAD2        M2(X)
     C                   MOVEL     ALMCTY        MC(X)
     C                   MOVEL     ALMST         MS(X)
     C                   Z-ADD     ALMZIP        MZ(X)
     C                   MOVEL     ALMZIP        XXZIPX
     C     ALMCTY        IFNE      *BLANKS
     C     ALMST         ORNE      *BLANKS
     C     ALMCTY        CAT(P)    ',':0         M3(X)
     C                   CAT       ALMST:1       M3(X)
     C                   CAT       XXZIP5:1      M3(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         M3(X)
     C                   CAT       XXZIP4:0      M3(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     ALFEIN        FE(X)
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  ALTPOB        XXKEY
     C                   EVAL      TB(X)=GetGenCodeDs(ALco#:XXTBID:xxKey)
     C                   MOVEL     ALELID        ID(X)
     C                   Z-ADD     ALDLDT        DD(X)
     C                   MOVEL     ALDLRS        DR(X)
     C                   ADD       1             X
     C     ALKY          READE(N)  PAL00                                  60
     C                   END
     C                   END
     C                   EndSr
     C**********************************************************
     C* End   Processing
     C**********************************************************
     C     EndProcess    BegSr
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C*
     C                   MOVEA     *BLANKS       NM2
     C                   MOVEA     *BLANKS       FN2
     C                   MOVEA     *BLANKS       N22
     C                   MOVEA     *BLANKS       F22
     C                   MOVEA     *BLANKS       P12
     C                   MOVEA     *BLANKS       P22
     C                   MOVEA     *BLANKS       P32
     C                   MOVEA     *BLANKS       PC2
     C                   MOVEA     *BLANKS       PS2
     C                   MOVEA     *ZEROS        PZ2
     C                   CLEAR                   CN2
     C                   MOVEA     *BLANKS       M12
     C                   MOVEA     *BLANKS       M22
     C                   MOVEA     *BLANKS       M32
     C                   MOVEA     *BLANKS       MC2
     C                   MOVEA     *BLANKS       MS2
     C                   MOVEA     *ZEROS        MZ2
     C                   MOVEA     *ZEROS        FE2
     C                   MOVEA     *BLANKS       TB2
     C                   MOVEA     *BLANKS       ID2
     C                   MOVEA     *BLANKS       NA
     C                   MOVEA     *BLANKS       MA
     C                   MOVEA     *BLANKS       PA
     C                   MOVEA     *ZEROS        DD2
     C                   MOVEA     *BLANKS       DR2
     C                   MOVEA     *BLANKS       ST2
     C*** begin chg ***  10/21/21  *******************************
     C******             Z-ADD     1             X                 3 0
     C******             Z-ADD     0             K                 3 0
     C                   Z-ADD     1             X                 5 0
     C                   Z-ADD     0             K                 5 0
     C*** end chg *****  10/21/21  *******************************
     C     ALKY          CHAIN(N)  PAL00                              60
     C     *IN60         DOWEQ     '0'
      *
     C                   MOVEL(P)  ALNAME        NM2(X)
     C                   MOVEL(P)  ALNAM2        XXNM10           10
     C                   MOVE      XXNM10        NM2(X)
     C                   MOVE      ALNAM2        N22(X)
      *
     C                   MOVEL(P)  ALFDNM        FN2(X)
     C                   MOVEL(P)  ALFDN2        XXNM10           10
     C                   MOVE      XXNM10        FN2(X)
     C                   MOVE      ALFDN2        F22(X)
      *
     C                   MOVEL     ALPAD1        P12(X)
     C                   MOVEL     ALPAD2        P22(X)
     C                   MOVEL     ALPCTY        PC2(X)
     C                   MOVEL     ALPST         PS2(X)
     C                   Z-ADD     ALPZIP        PZ2(X)
     C                   EXSR      GTCNTY
     C                   MOVEL(P)  XXCNTY        CN2(X)
     C                   MOVEL     ALPZIP        XXZIPX
     C     ALPCTY        IFNE      *BLANKS
     C     ALPST         ORNE      *BLANKS
     C     ALPCTY        CAT(P)    ',':0         P32(X)
     C                   CAT       ALPST:1       P32(X)
     C                   CAT       XXZIP5:1      P32(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         P32(X)
     C                   CAT       XXZIP4:0      P32(X)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     ALMAD1        M12(X)
     C                   MOVEL     ALMAD2        M22(X)
     C                   MOVEL     ALMCTY        MC2(X)
     C                   MOVEL     ALMST         MS2(X)
     C                   Z-ADD     ALMZIP        MZ2(X)
     C                   MOVEL     ALMZIP        XXZIPX
     C     ALMCTY        IFNE      *BLANKS
     C     ALMST         ORNE      *BLANKS
     C     ALMCTY        CAT(P)    ',':0         M32(X)
     C                   CAT       ALMST:1       M32(X)
     C                   CAT       XXZIP5:1      M32(X)
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         M32(X)
     C                   CAT       XXZIP4:0      M32(X)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     ALFEIN        FE2(X)
     C                   MOVEL(P)  'BUSI'        XXTBID
     C                   MOVEL(P)  ALTPOB        XXKEY
     C                   EVAL      TB2(X)=GetGenCodeDs(ALco#:XXTBID:xxKey)
     C                   MOVEL     ALELID        ID2(X)
     C                   Z-ADD     ALDLDT        DD2(X)
     C                   MOVEL     ALDLRS        DR2(X)
      *
     C     NM(X)         IFNE      NM2(X)
     C     N2(X)         ORNE      N22(X)
     C     FN(X)         ORNE      FN2(X)
     C     F2(X)         ORNE      F22(X)
     C                   MOVE      'Y'           NA(X)
     C                   END
     C     M1(X)         IFNE      M12(X)
     C     M2(X)         ORNE      M22(X)
     C     M3(X)         ORNE      M32(X)
     C                   MOVE      'Y'           MA(X)
     C                   END
     C     P1(X)         IFNE      P12(X)
     C     P2(X)         ORNE      P22(X)
     C     P3(X)         ORNE      P32(X)
     C                   MOVE      'Y'           PA(X)
     C                   END
     C     CN(X)         IFNE      CN2(X)
     C                   MOVE      'Y'           CA(X)
     C                   ENDIF
     C     FE(X)         IFNE      FE2(X)
     C                   MOVE      'Y'           FI(X)
     C                   END
     C                   MOVE      ALSTAT        ST2(X)
     C                   MOVE      ALELTP        TP2(X)
     C     ST2(X)        IFNE      ' '
     C                   ADD       1             K
     C                   ENDIF
     C                   ADD       1             X
     C     ALKY          READE(N)  PAL00                                  60
     C                   END
     C*
     C                   Z-ADD     1             X
     C     NM2(X)        DOWNE     *BLANKS
     C*** begin chg ***  10/21/21  *******************************
     C**** X             ANDLT     500
     C**** N22(X)        ORNE      *BLANKS
     C**** X             ANDLT     500
     C     X             ANDLT     9999
     C     N22(X)        ORNE      *BLANKS
     C     X             ANDLT     9999
     C*** end chg *****  10/21/21  *******************************
     C*
     C     ST2(X)        IFEQ      'C'
     C*** Begin Chg ** 09/13/13 ****************************
     C***************     ST2(X)        OREQ      'A'
     C     ST2(X)        OREQ      '+'
     C*** End   Chg ** 09/13/13 ****************************
     C     ST2(X)        OREQ      'X'
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C     NA(X)         IFEQ      'Y'
     C     MA(X)         OREQ      'Y'
     C     PA(X)         OREQ      'Y'
     C     CA(X)         OREQ      'Y'
     C     FI(X)         OREQ      'Y'
     C     XXDLT         OREQ      'Y'
     C*
     C*  SETUP FOR NEXT REQUEST NUMBER
     C*
     C     ENKY          SETGT     PEN03
     C     ENKY          READPE    PEN03                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C*
     C                   EVAL      ENEEDT = dtCYMD(LDEEDT)
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
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
     C*
     C                   MOVE      *BLANKS       ENAELF
     C                   MOVE      *BLANKS       ENCELF
     C                   MOVE      *BLANKS       ENDELF
     C                   MOVE      *BLANKS       ENRMCD
     C*** Begin Chg ** 09/13/13 ****************************
     C*****************     ST2(X)        IFEQ      'A'
     C     ST2(X)        IFEQ      '+'
     C*** End   Chg ** 09/13/13 ****************************
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENAELF
     C                   MOVEL     DXENAD        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENAELF
     C                   MOVEL     DXLCAD        ENRMCD
     C                   END
     C                   ELSE
     C     ST2(X)        IFEQ      'C'
     C     NA(X)         IFEQ      'Y'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXENNC        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXLCNC        ENRMCD
     C                   END                                                    TP2,X='E'
     C                   END                                                    NA,X='Y'
     C     CA(X)         IFEQ      'Y'
     C                   MOVE      'Y'           ENCELF
     C     MA(X)         IFNE      'Y'
     C     PA(X)         ANDNE     'Y'
     C                   SELECT
     C     P12(X)        WHENNE    *BLANKS
     C     TP2(X)        ANDEQ     'E'
     C                   MOVEL     DXENPA        ENRMCD
     C     P12(X)        WHENNE    *BLANKS
     C     TP2(X)        ANDNE     'E'
     C                   MOVEL     DXLCPA        ENRMCD
     C     TP2(X)        WHENEQ    'E'
     C                   MOVEL     DXENMA        ENRMCD
     C                   OTHER
     C                   MOVEL     DXLCMA        ENRMCD
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C     MA(X)         IFEQ      'Y'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXENMA        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXLCMA        ENRMCD
     C                   END                                                    TP2,X='E'
     C                   END                                                    NA,X='Y'
     C     PA(X)         IFEQ      'Y'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXENPA        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXLCPA        ENRMCD
     C                   END                                                    TP2,X='E'
     C                   END                                                    NA,X='Y'
     C     FI(X)         IFEQ      'Y'
     C                   MOVE      'Y'           ENCELF
     C                   MOVEL     DXMISC        ENRMCD
     C                   END                                                    FE,X='Y'
     C                   ELSE
     C     ST2(X)        IFEQ      'X'
     C     TP2(X)        IFEQ      'E'
     C                   MOVE      'Y'           ENDELF
     C                   MOVEL     DXENDL        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENDELF
     C                   MOVEL     DXLCDL        ENRMCD
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   Eval      ENfnnm = GetGrpName(EMfnd)
     C                   Eval      sysDt7 = DateTo7(SystemDate())
     C                   Eval      ENagt# = GetEmpAgt(ELco#:ELfnd:ELemp#:ELdiv:
     C                              ELfyr:sysdt7:ENagnm)
     C                   Eval      ENcar1 = F2car#
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C                   MOVE      ID2(X)        ENELID
     C*
     C*  LOAD BEFORE FIELDS
     C*
     C                   Z-ADD     FE(X)         ENOFI1
     C                   MOVEL     NM(X)         ENOEN1
     C                   MOVEL     FN(X)         ENOFN1
     C                   MOVEL     M1(X)         ENOM11
     C                   MOVEL     M2(X)         ENOM21
     C                   MOVEL     MC(X)         ENOMC1
     C                   MOVEL     MS(X)         ENOMS1
     C                   Z-ADD     MZ(X)         ENOMZ1
     C                   MOVEL     P1(X)         ENOP11
     C                   MOVEL     P2(X)         ENOP21
     C                   MOVEL     PC(X)         ENOPC1
     C                   MOVEL     PS(X)         ENOPS1
     C                   Z-ADD     PZ(X)         ENOPZ1
     C                   MOVEL     CN(X)         ENOCN1
     C                   MOVEL     *BLANKS       ENOC11
     C                   MOVEL     *BLANKS       ENOC21
     C                   MOVEL     *BLANKS       ENOC31
     C*
     C*  LOAD AFTER FIELDS
     C*
     C                   Z-ADD     FE2(X)        ENOFI2
     C                   MOVEL     NM2(X)        ENOEN2
     C                   MOVEL     FN2(X)        ENOFN2
     C                   MOVEL     M12(X)        ENOM12
     C                   MOVEL     M22(X)        ENOM22
     C                   MOVEL     MC2(X)        ENOMC2
     C                   MOVEL     MS2(X)        ENOMS2
     C                   Z-ADD     MZ2(X)        ENOMZ2
     C                   MOVEL     P12(X)        ENOP12
     C                   MOVEL     P22(X)        ENOP22
     C                   MOVEL     PC2(X)        ENOPC2
     C                   MOVEL     PS2(X)        ENOPS2
     C                   Z-ADD     PZ2(X)        ENOPZ2
     C                   MOVEL     CN2(X)        ENOCN2
     C                   MOVEL     *BLANKS       ENOC12
     C                   MOVEL     *BLANKS       ENOC22
     C                   MOVEL     *BLANKS       ENOC32
     C*
     C                   Eval      ENdt = DateTo7(SystemDate())
     C                   Eval      ENtime = TimeTo6(SystemTime())
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   MOVE      TP2(X)        ENELTP
     C*
     C*** Begin Chg ** 09/13/13 ****************************
     C****************     ST2(X)        IFEQ      'A'
     C     ST2(X)        IFEQ      '+'
     C*** End   Chg ** 09/13/13 ****************************
     C     ST2(X)        OREQ      'X'
     C                   Z-ADD     0             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  NM2(X)        ENALIN
     C                   WRITE     PEN03
      *
     C     N22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  N22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C*
     C     FN2(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  FN2(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
      *
     C     F22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  F22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C*
     C                   SELECT
     C     P12(X)        WHENNE    *BLANKS
     C     P22(X)        ORNE      *BLANKS
     C     P32(X)        ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  P12(X)        ENALIN
     C                   WRITE     PEN03
     C*
     C     P22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  P22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  P32(X)        ENALIN
     C                   WRITE     PEN03
     C*
     C     M12(X)        WHENNE    *BLANKS
     C     M22(X)        ORNE      *BLANKS
     C     M32(X)        ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  M12(X)        ENALIN
     C                   WRITE     PEN03
     C*
     C     M22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  M22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  M32(X)        ENALIN
     C                   WRITE     PEN03
     C*
     C                   ENDSL
     C*
     C     CN2(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  CN2(X)        ENALIN
     C                   WRITE     PEN03
     C                   ENDIF
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVE      FE2(X)        XXWK9A            9
     C     XXDES1        CAT(P)    XXWK9A:1      ENALIN
     C                   WRITE     PEN03
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  TB2(X)        ENALIN
     C                   WRITE     PEN03
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C     XXDES2        CAT(P)    ID2(X)        ENALIN
     C                   WRITE     PEN03
     C*
     C     ST2(X)        IFEQ      'X'
     C                   ADD       1             ENSEQ#
     C                   MOVE      *BLANKS       ENBLIN
     C                   MOVEL(P)  DR2(X)        ENALIN
     C                   WRITE     PEN03
     C                   END                                                    ST2,X='X'
     C*
     C                   END                                                    ST2,X='A,X'
     C*
     C     ST2(X)        IFEQ      'C'
     C*
     C                   Z-ADD     0             ENSEQ#
     C                   MOVEL(P)  NM(X)         ENBLIN
     C                   MOVEL(P)  NM2(X)        ENALIN
     C                   WRITE     PEN03
     C     N2(X)         IFNE      *BLANKS
     C     N22(X)        ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  N2(X)         ENBLIN
     C                   MOVEL(P)  N22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C     FN2(X)        IFNE      *BLANKS
     C     FN(X)         ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  FN(X)         ENBLIN
     C                   MOVEL(P)  FN2(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C     F22(X)        IFNE      *BLANKS
     C     F2(X)         ORNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  F2(X)         ENBLIN
     C                   MOVEL(P)  F22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C*
     C     P1(X)         IFNE      P12(X)
     C     P2(X)         ORNE      P22(X)
     C     P3(X)         ORNE      P32(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  P1(X)         ENBLIN
     C                   MOVEL(P)  P12(X)        ENALIN
     C                   WRITE     PEN03
     C     P22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  P2(X)         ENBLIN
     C                   MOVEL(P)  P22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  P3(X)         ENBLIN
     C                   MOVEL(P)  P32(X)        ENALIN
     C                   WRITE     PEN03
     C                   END                                                    PHYS ADR CHG
     C*
     C     M1(X)         IFNE      M12(X)
     C     M2(X)         ORNE      M22(X)
     C     M3(X)         ORNE      M32(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  M1(X)         ENBLIN
     C                   MOVEL(P)  M12(X)        ENALIN
     C                   WRITE     PEN03
     C     M22(X)        IFNE      *BLANKS
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  M2(X)         ENBLIN
     C                   MOVEL(P)  M22(X)        ENALIN
     C                   WRITE     PEN03
     C                   END
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  M3(X)         ENBLIN
     C                   MOVEL(P)  M32(X)        ENALIN
     C                   WRITE     PEN03
     C                   END                                                    MAIL ADR CHG
     C*
     C     CN(X)         IFNE      CN2(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL     CN(X)         ENBLIN
     C                   MOVEL(P)  CN2(X)        ENALIN
     C                   WRITE     PEN03
     C                   ENDIF
     C*
     C     FE(X)         IFNE      FE2(X)
     C                   ADD       1             ENSEQ#
     C                   MOVE      FE(X)         XXWK9A            9
     C     XXDES1        CAT(P)    XXWK9A:1      ENBLIN
     C                   MOVE      FE2(X)        XXWK9A            9
     C     XXDES1        CAT(P)    XXWK9A:1      ENALIN
     C                   WRITE     PEN03
     C                   END                                                    FED ID CHG
     C*
     C     TB(X)         IFNE      TB2(X)
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  TB(X)         ENBLIN
     C                   MOVEL(P)  TB2(X)        ENALIN
     C                   WRITE     PEN03
     C                   END                                                    TPOB CHG
     C*
     C                   END                                                    ST2,X='C'
     C*
     C                   ADD       1             ENSEQ#
     C                   MOVEL(P)  *BLANKS       ENBLIN
     C                   MOVEL(P)  *BLANKS       ENALIN
     C                   WRITE     PEN03
     C*
     C                   ENDIF
     C                   END                                                    ST2,X=A,C,D
     C                   ADD       1             X
     C                   ENDDO                                                  DOW<=500
     C*
     C     ALKY          CHAIN     PAL00                              60
     C     *IN60         DOWEQ     '0'
     C*** Begin Chg ** 09/13/13 ****************************
     C************                   MOVE      *BLANKS       ALSTAT
     C************                   UPDATE    PAL00
     C                   IF        ALSTAT = '+' Or ALSTAT = 'C' Or
     C                             ALSTAT = 'X'
     C                   MOVE      'A'           ALSTAT
     C                   UPDATE    PAL00
     C                   ENDIF
     C*** End   Chg ** 09/13/13 ****************************
     C     ALKY          READE     PAL00                                  60
     C                   ENDDO                                                  DOW *IN60
     C*
     C                   END                                                    LDENDA<>' '
     C                   EndSr
     C*================================================================
     C* GTCNTY - Get the county information
     C*================================================================
     C     GTCNTY        BEGSR
     C*
     C     ALPST         IFNE      *BLANKS
     C     ALPST         CAT(P)    ALCNTY        XXKEY
     C                   ELSE
     C     ALMST         CAT(P)    ALCNTY        XXKEY
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXCNTY           35
     C                   EVAL      XXCNTY=CountyCdDs(EMco#:xxKey)
     C*
     C                   ENDSR
