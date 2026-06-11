     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE  W9208R - J. D. EDWARDS A/R INTERFACE PROGAM
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*           THIS IS A SPECIAL VERSION OF W9208R THAT WILL       *
     H*    DOWNLOAD ALL TRANSACTIONS FROM THE DATE PASSED IN AND      *
     H*    FORWARD.                                                   *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   COMMAND                                                     *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    10/07/10            PROJECT: SUPP#74362           *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*   50  GENERAL CHAINING                                        *
     H*   60  PRIMARY READ ON A/R TRANSACTION FILE                    *
     H*   LR  LAST RECORD INDICATOR                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* SPECIAL FILE USAGE:                                           *
     H*                                                               *
     H*    WMEAL  - QUERIED FOR RECORDS WITH POSTING OR TRANSACTION   *
     H*              DATE = TO PARM DATE.                             *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  10/07/10  C:65   PROJECT: SUPPORT#74362                      *
     H*                   ONLY PROCESS GROUP 1 AND 'B' TYPES.         *
     H*                                                               *
     H* PLEASE ADD TO TOP                                             *
     H*****************************************************************
      *
      *
     FWMEAL     IF   E           K DISK
     FWMA#L     IF   E           K DISK
     F                                     RENAME(PEA01:PA#01)
     FWMJEL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FF5503B11  O    E             DISK
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *  ORDER ID
      *
     D                 DS
     D  XXBORI                 1     50
     D  EAFND                  1      3  0
     D  EAEMP#                 4     12  0
     D  EADIV                 13     17  0
     D  EAFYR                 18     20  0
     D  EAARCD                21     22
     D  XXPODT                23     30  0
     D  XXDT                  31     38  0
     D  EATIME                39     44  0
      *  DATE FIELD WITH SEPARATORS
      *
     D                 DS
     D  XXDT10                 1     10
     D  XXDTMM                 1      2
     D  XXSEP1                 3      3
     D  XXDTDD                 4      5
     D  XXSEP2                 6      6
     D  XXDTYY                 7     10
      *
      *  DATE FIELD
      *
     D                 DS
     D  XXDT8                  1      8
     D  XXDTM8                 1      2
     D  XXDTD8                 3      4
     D  XXDTY8                 5      8
     D                 DS
     D  X1DATE                 1      7  0
     D  XXDTY                  1      3  0
     D  XXDTM                  4      5  0
     D  XXDTD                  6      7  0
      *
      ****************************************************************
      *    LIKE DEFINES
      ****************************************************************
      *

     C     *LIKE         DEFINE    EACO#         XXCO#
     C     *LIKE         DEFINE    EAFND         XXFND
     C     *LIKE         DEFINE    EAFYR         XXFYR
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EATRDT        XXEFDT
     C     *LIKE         DEFINE    EATRDT        XXDATE
      *
      ****************************************************************
      *    KEY LISTING
      ****************************************************************
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     F1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELFYR
      *
     C     F1KY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
     C     BSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
      *
     C     TTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ****************************************************************
      *    ENTRY PARMS
      ****************************************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQRNDT            8
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ****************************************************************
      *    SET UP AND INITIALIZE
      ****************************************************************
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQRNDT        XXTMP1            8 0
     C                   Z-ADD     XXTMP1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
      *
     C                   MOVE      'N'           XXTRAN            1            TRANSFER REC.
     C                   MOVE      'Y'           #FSTIM            1            FIRST TIME FLAG
     C                   Z-ADD     000           XXFND
     C                   MOVE      ' '           XXHLDF            1
      *
      * CHAIN TO JDE CONTROL FILE
     C     XXCO#         CHAIN     PJE01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     JESTCD        Q1BSTS
     C                   MOVEL     JEITYP        Q1BINT
     C                   ENDIF                                                  *IN50='0'
      *
      ****************************************************************
      *    START READING A/R TRANSACTION FILE
      ****************************************************************
      *
     C* Process WMEAP and when done process Holding file (WMA#P)
     C     XXHLDF        IFEQ      ' '
     C                   READ      PEA01                                  60
     C   60              MOVE      'Y'           XXHLDF
     C                   ENDIF                                                  XXHLDF=' '
     C     XXHLDF        IFEQ      'Y'
     C                   READ      PA#01                                  60
     C                   ENDIF                                                  XXHLDF='Y'
      *
     C     *IN60         DOWEQ     '0'
      *
      *  CHAIN TO EMPLOYER DETAIL FILE TO GET BILLING TYPE
      *
     C* Get Fund record whenever fund changes
     C     EACO#         IFNE      XXCO#
     C     EAFND         ORNE      XXFND
     C                   Z-ADD     EACO#         XXCO#
     C                   Z-ADD     EAFND         XXFND
     C     FNKY          CHAIN     WMFNL                              65
     C  N65              MOVE      FNTEST        XXTEST            1
     C                   ENDIF                                                  EAFND NE XFND
      *
     C                   Z-ADD     EAFYR         XXFYR
     C                   Z-ADD     EAEMP#        XXEMP#
     C                   Z-ADD     EADIV         XXDIV
     C     ELKY1         CHAIN     PEL01                              50
     C     *IN50         IFEQ      '0'
      *
      * DETERMINE IF THE RECORD SHOULD BE TRANSFERRED. IF THE CONDITION
      *   FOR TRANSFER ARE MET THEN XXTRAN WILL EQUAL 'Y'
      *
     C                   MOVE      'N'           XXTRAN
      *
     C* Only process billing A/R records (EAARTY = 'B', 'A', 'D', 'F', or 'H')
     C     EAARTY        IFEQ      'B'
     C*** BEGIN DEL ***  10/07/10  *********************************************
     C********   EAARTY    OREQ 'A'
     C********   EAARTY    OREQ 'D'
     C********   EAARTY    OREQ 'F'
     C********   EAARTY    OREQ 'H'
     C*** END DEL *****  10/07/10  *********************************************
     C                   MOVE      'Y'           XXTRAN
     C*
     C* Only check to include the record will be the transaction date EATRDT equ
     C* and the maintenance date EADT equal to the current date for WMA#P
     C*** BEGIN CHG ***  10/07/10  *********************************************
     C********   XXHLDF    IFEQ ' '
     C********   EATRDT    ANDEQXXDATE
     C********   XXHLDF    OREQ 'Y'
     C********   EADT      ANDEQXXDATE
     C     XXHLDF        IFEQ      ' '
     C     EATRDT        ANDGE     XXDATE
     C********   XXHLDF    OREQ 'Y'
     C********   EADT      ANDGEXXDATE
     C*** END CHG *****  10/07/10  *********************************************
     C                   MOVE      'Y'           XXTRAN
     C                   ELSE
     C                   MOVE      'N'           XXTRAN
     C                   ENDIF
      *
     C* Negate amount for holding file processing
     C     XXHLDF        IFEQ      'Y'
     C                   MULT      -1            EAAMT
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *  IF TRANSFER FLAG = 'Y' THEN EXCUTE SUBROUTINE TO
      *   WRITE RECORD TO INTERFACE FILE
      *
      * Check for FNTEST = 'Y' instead of EAFND = 999
      *
     C     XXTRAN        IFEQ      'Y'
     C     XXTEST        ANDNE     'Y'
     C*** BEGIN ADD ***  10/07/10  *********************************************
     C     EAFND         ANDEQ     1
     C*** END ADD *****  10/07/10  *********************************************
     C                   EXSR      SRINZ
     C                   EXSR      SRWRIT
     C                   END
      *
     C                   END                                                    ELKY1 = 0
      *
      *   READ NEXT RECORD
      *
     C* Process WMEAP and when done process Holding file (WMA#P)
     C     XXHLDF        IFEQ      ' '
     C                   READ      PEA01                                  60
     C   60              MOVE      'Y'           XXHLDF
     C                   ENDIF                                                  XXHLDF=' '
     C     XXHLDF        IFEQ      'Y'
     C                   READ      PA#01                                  60
     C                   ENDIF                                                  XXHLDF='Y'
     C                   END                                                    *IN60 = 0
      *
     C                   MOVE      '1'           *INLR
      *
      ************************************************************************
      *  SRINZ - INITIALIZE INTERFACE FILE FIELDS                            *
      ************************************************************************
      *
     C     SRINZ         BEGSR
      *
      *
     C                   MOVE      *BLANKS       Q1@TY             2
     C                   MOVE      *BLANKS       Q1BSTS
     C                   MOVE      *BLANKS       Q1BINT
     C                   MOVE      *BLANKS       Q1BPTY
     C                   MOVE      *ZEROS        Q1AN8
     C                   MOVE      *ZEROS        Q1BORI
     C                   Z-ADD     1             Q1LIN
     C                   MOVE      *BLANKS       Q1DCT
     C                   MOVE      *ZEROS        Q1DOC
     C                   MOVE      '001'         Q1SFX
     C                   MOVE      *BLANKS       Q1CO
     C                   MOVE      *BLANKS       Q1MCU
     C                   MOVE      *BLANKS       Q1MCU2
     C                   MOVE      *BLANKS       Q1OBJ
     C                   MOVE      *BLANKS       Q1SUB
     C                   MOVE      *BLANKS       Q1SBL
     C                   MOVE      *BLANKS       Q1SBLT
     C                   MOVE      *ZEROS        Q1AA
     C                   MOVE      *ZEROS        Q1AG
     C                   MOVE      *BLANKS       Q1EXA
     C                   MOVE      *BLANKS       Q1EXR
     C                   MOVE      *BLANKS       Q1PTC
     C                   MOVE      *ZEROS        Q1GLDT
     C                   MOVE      *ZEROS        Q1DTEN
     C                   MOVE      *ZEROS        Q1CKAM
     C                   MOVE      *BLANKS       Q1DMTJ
     C                   MOVE      *BLANKS       Q1CKNU
     C                   MOVE      *ZEROS        Q1PAAP
     C                   MOVE      *ZEROS        Q1AAP
     C                   MOVE      *BLANKS       Q1GLBA
     C                   MOVE      *BLANKS       Q1GLC
     C                   MOVE      *ZEROS        Q1DI
     C                   MOVE      *ZEROS        Q1DDJ
     C                   MOVEL     'AR'          Q1JDMD
     C                   MOVE      *BLANKS       Q1LNTY
     C                   MOVE      *ZEROS        Q1UORG
     C                   MOVE      *BLANKS       Q1LITM
     C                   MOVE      *BLANKS       Q1DSC1
     C                   MOVE      *BLANKS       Q1DSC2
     C                   MOVE      *BLANKS       Q1PMCT
     C* DATE SEPARATOR
     C                   MOVE      '/'           XXSEP1
     C                   MOVE      '/'           XXSEP2
      *
     C                   ENDSR
      *
      ************************************************************************
      *  SRWRIT- WRITE RECORD TO INTERFACE FILE                              *
      ************************************************************************
      *
     C     SRWRIT        BEGSR
      *
      * CHAIN TO JDE CONTROL FILE
     C     XXCO#         CHAIN     PJE01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     JESTCD        Q1BSTS
     C                   MOVEL     JEITYP        Q1BINT
     C                   ENDIF                                                  *IN50='0'
      *
     C                   SELECT
     C     FNPFFL        WHENEQ    'C'
     C                   MOVEL     'WCASO '      Q1BPTY
     C     ELAMAG        WHENNE    0
     C                   MOVEL     'WCAD  '      Q1BPTY
     C                   OTHER
     C                   MOVEL     'WCPOOL'      Q1BPTY
     C                   ENDSL
      *
     C                   MOVE      EANINV        Q1EXA
      *
      *  CHAIN TO EMPLOYER MASTER FILE TO GET DISTRICT
      *
     C     EMKY          CHAIN     PEM01                              50
     C                   Z-ADD     0             Q1AN8
     C     *IN50         IFEQ      '0'
     C     6             SUBST     EMSEMP:2      XX6A              6
     C                   MOVE      XX6A          Q1AN8
     C                   END
      *
     C* BUILD ORDERID - CONCATENATION OF EAFND, EAEMP#, EADIV, EAFYR, EAARCD
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT
     C*
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   MOVEL     XXBORI        Q1BORI
     C* LOAD GL DATE WITH SEPARATOR
     C                   MOVE      XXPODT        XXDT8
     C                   EXSR      SRLDDT
     C                   MOVE      XXDT10        Q1GLDT
     C                   MOVE      Q1GLDT        Q1DI
     C                   MOVE      Q1GLDT        Q1DDJ
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXWKDT            8 0
     C*
     C                   MOVE      XXWKDT        XXDT8
     C                   EXSR      SRLDDT
     C                   MOVE      XXDT10        Q1DTEN
      *
      *  CHAIN TO A/R TABLE FILE
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TTKY          CHAIN     PTT01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVE      TT1@TY        Q1@TY
     C                   MOVE      TT1DCT        Q1DCT
     C                   MOVE      TT1OBJ        Q1OBJ
     C                   MOVE      TT1GLC        Q1GLC
     C                   END
      *
      * DETERMINE CURRENT FUND YEAR
     C     XXDTM         IFGE      9
     C     XXDTM         ANDLE     12
     C                   Z-ADD     XXDTY         XNFYR             3 0
     C                   ELSE
     C     XXDTY         SUB       1             XNFYR
     C                   ENDIF
      *
      *  LOAD COST CENTER: IF RECORD TYPE NE TO 'D' THEN COMBINE
      *    '5' + EAFYR, ELSE FIND CURRENT FUND YEAR AND COMBINE
      *    WITH '5'.
      *
     C                   MOVE      *ZEROS        XXGLCC            4 0
     C                   MOVEL     '5'           Q1MCU
     C     TT1OBJ        IFGE      '4000'
     C     TT1OBJ        ANDLE     '8999'
     C                   Z-ADD     XNFYR         XXGLCC
     C                   MOVEL     '5'           XXGLCC
     C                   MOVEL     XXGLCC        Q1MCU
     C                   ELSE
     C     TT1OBJ        IFEQ      '1230'
     C     TT1OBJ        OREQ      '1201'
     C     TT1OBJ        OREQ      '2185'
     C     TT1OBJ        OREQ      '1212'
     C                   MOVEL     '4'           Q1MCU
     C                   ENDIF                                                  =1230 OR 1201
     C                   ENDIF                                                  >=4000/<=8999
 A   C     Q1@TY         IFEQ      '02'
     C                   MOVEL     '5'           Q1MCU2                         DEFAULT
     C     TT1GLC        IFGE      '4000'
     C     TT1GLC        ANDLE     '8999'
     C                   Z-ADD     XNFYR         XXGLCC
     C                   MOVEL     '5'           XXGLCC
     C                   MOVEL     XXGLCC        Q1MCU2
 A   C                   ELSE
 A   C     TT1GLC        IFEQ      '1230'
 A   C     TT1GLC        OREQ      '1201'
     C                   MOVEL     '4'           Q1MCU2
 A   C                   ENDIF                                                  =1230 OR 1201
     C                   ENDIF                                                  >=4000 & <=8999
 A   C                   ENDIF                                                  Q1@TY = 2
     C                   MOVEL     EADESC        Q1EXR
      *
      *  ADD A/R AMOUNT
      *
     C                   MOVE      EAAMT         Q1AA
      *
     C                   MOVE      Q1AA          Q1AG
      *
      *  LOAD INV# IF NOT ZERO.
      *
      *
      *
      *  WRITE RECORD TO INTERFACE FILE
      *
     C                   WRITE     I5503B
      *
     C                   ENDSR
      *
      ************************************************************************
      *  SRLDDT- LOAD 8,0 DATE TO 10A DATE WITH SEPARATOR                    *
      ************************************************************************
     C     SRLDDT        BEGSR
     C                   MOVE      XXDTM8        XXDTMM
     C                   MOVE      XXDTD8        XXDTDD
     C                   MOVE      XXDTY8        XXDTYY
     C                   ENDSR
