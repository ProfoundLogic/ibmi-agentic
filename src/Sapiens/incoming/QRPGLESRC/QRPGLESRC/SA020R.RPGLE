     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE S0020R - CHANGE NUMERIC FIELDS TO ALPHA
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL CHANGE NUMERIC FIELDS TO     *
     H*                ALPHA WITH ZERO SUPPRESSION AND EDIT MASKS.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 04/29/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:
     H*                                                               *
     H*  02/10/99  C:33  PROJECT: WC577    SEQ#: WA100/16244          *
     H*                  RESET *IN51                                  *
     H*                                                               *
     H*  11/11/98  C:33  PROJECT: 65WCU                               *
     H*                  PAD SSN FOR RECURRING SS# ENTRIES.           *
     H*                                                               *
     H*  08/05/98  C:87  PROJECT: WC591                               *
     H*                  PROGRAM WILL NOW FORMAT NUMERIC FIELDS TO    *
     H*                  CHARACTER ACCORDING TO EDIT CODES 1,2,3,4,A, *
     H*                  B,C,D,J,K,L,M,N,O,P, AND Q.                  *
     H*                                                               *
     H*  05/14/98  C:33  SEQ#: WA091/11509                            *
     H*                  ACCOUNT FOR NEGATIVE NUMBERS.                *
     H*                                                               *
     H*  04/08/98  C:87  PROJECT: WC630                               *
     H*                  'DATE6' FORMATS 6 DIGIT DATES FOR OUTPUT     *
     H*                  WITH SEPARATORS XX/XX/XX.                    *
     H*                                                               *
     H*  02/13/98  C:33  PROJECT: WC577                               *
     H*                  'XXX,XXX' CALCULATES FOR TOO MANY SPACES WHEN*
     H*                  LENGTH IS DIVISIBLE BY 3.                    *
     H*
     H* 12/18/97 C:53  PROJ: MC145
     H*                NAME CHANGE WA003R TO SA020R
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  50 - PUT NEGATIVE DESIGNATION AFTER  NUMBER (FOR EDITCODE)   *
     H*  51 - DO NOT ZERO SUPRESS IF OUTPUT = 0 (FOR EDITCODE)        *
     H*                                                               *
     H*****************************************************************

     D*
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)
     D*
     D EDTCDS          C                   CONST('1234ABCDJKLMNOPQ')
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C***  BEGIN ADD  ***  08/05/98  **********************************
     C                   PARM                    QQDEC             3 0
     C                   PARM                    QQTHOU            1
     C*
     C*  NOTE: Pass edit code left justified in qqmask and the number
     C*        of decimals in qqdec.  If not sure of edit code you can
     C*        leave qqmask blank and pass #decimals and '1' or '0' to
     C*        QQTHOU to indicate if you want thousands notation.
     C*  You can still pass only first two parms if desired and not
     C*  using 'EDITCODE' processing.
     C***  END   ADD  ***  08/05/98  **********************************
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C***  BEGIN ADD  ***  02/10/99  *********************************
     C                   MOVE      *OFF          *IN51
     C***  END   ADD  ***  02/10/99  *********************************
     C***  BEGIN ADD  ***  08/05/98  **********************************
     C                   Z-ADD     *ZERO         XXDEC
     C                   MOVE      *BLANKS       XXTHOU
     C                   MOVE      *BLANKS       XXNEG             2
     C     Q1PRMS        IFGT      2
     C                   Z-ADD     QQDEC         XXDEC             3 0
     C     EDTCDS        CHECK     QQMASK        Z
     C     Z             IFNE      1
     C                   MOVEL     QQMASK        CODE              1
     C                   EXSR      SREDIT
     C                   ELSE
     C     Q1PRMS        IFGT      3
     C                   MOVE      QQTHOU        XXTHOU            1
     C                   ENDIF                                                  Q1PRMS>3
     C                   ENDIF                                                  Z IFNE 1
     C                   MOVEL     'EDITCODE'    @@MASK           10
     C                   ENDIF                                                  Q1PRMS>2
     C***  END   ADD  ***  08/05/98  **********************************
     C*
     C                   CLEAR                   NM1
     C                   CLEAR                   NM2
     C*
     C     QQMASK        IFNE      'SSN'
     C*
     C     '0'           CHECK     QQNUM         XXZERO            3 0
     C     ' '           CHECKR    QQNUM         XXLEN             3 0
     C     XXZERO        SUB       1             XXBLNK            3 0
     C                   MOVEA     QQNUM         NM1
     C*
     C                   DO        XXBLNK        X                 3 0
     C                   MOVE      ' '           NM1(X)
     C                   ENDDO                                                  X->XXBLNK
     C*
     C                   MOVE      *OFF          NEG
     C                   Z-ADD     XXLEN         X
     C*
     C     XXLEN         IFNE      *ZERO
     C     NM1(X)        ANDNE     *BLANK
     C*
     C     NM1(X)        IFLT      '0'
     C     NM1(X)        ORGT      '9'
     C*
     C***  BEGIN CHG  ***  05/14/98  *********************************
     C**********           SUB  1         XXLEN
     C                   SELECT
     C     NM1(X)        WHENEQ    '}'
     C                   MOVEA     '0'           NM1(X)
     C     NM1(X)        WHENEQ    'J'
     C                   MOVEA     '1'           NM1(X)
     C     NM1(X)        WHENEQ    'K'
     C                   MOVEA     '2'           NM1(X)
     C     NM1(X)        WHENEQ    'L'
     C                   MOVEA     '3'           NM1(X)
     C     NM1(X)        WHENEQ    'M'
     C                   MOVEA     '4'           NM1(X)
     C     NM1(X)        WHENEQ    'N'
     C                   MOVEA     '5'           NM1(X)
     C     NM1(X)        WHENEQ    'O'
     C                   MOVEA     '6'           NM1(X)
     C     NM1(X)        WHENEQ    'P'
     C                   MOVEA     '7'           NM1(X)
     C     NM1(X)        WHENEQ    'Q'
     C                   MOVEA     '8'           NM1(X)
     C     NM1(X)        WHENEQ    'R'
     C                   MOVEA     '9'           NM1(X)
     C                   ENDSL
     C***  END   CHG  ***  05/14/98  *********************************
     C*
     C                   MOVE      *ON           NEG               1
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXLEN<>*ZERO
     C                   ENDIF                                                  QQMASK<>SSN
     C*
     C                   SELECT
     C*** THOUSANDS
     C*
     C     QQMASK        WHENEQ    'XXX,XXX'
     C*
     C                   Z-ADD     XXLEN         W                 3 0
     C     XXLEN         DIV       3             Y                 3 0
     C***  BEGIN ADD  ***  02/13/98  *********************************
     C                   MVR                     XXREM             3 0
     C*
     C     XXREM         IFEQ      *ZERO
     C                   SUB       1             Y
     C                   ENDIF                                                  XXREM=*ZERO
     C***  END   ADD  ***  02/13/98  *********************************
     C*
     C                   ADD       Y             XXLEN
     C                   Z-ADD     XXLEN         Z                 3 0
     C*
     C                   DO        XXLEN         X
     C*
     C     NM1(W)        IFNE      *BLANK
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C                   MOVEA     ','           NM2(Z)
     C                   ELSE
     C                   MOVE      NM1(W)        NM2(Z)
     C                   SUB       1             W
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->XXLEN
     C*
     C*** THOUSANDS W/DECIMALS
     C*
     C     QQMASK        WHENEQ    'X,XXX.XX'
     C*
     C                   Z-ADD     XXLEN         W
     C     XXLEN         DIV       3             Y
     C                   ADD       Y             XXLEN
     C                   ADD       1             XXLEN
     C                   Z-ADD     XXLEN         Z
     C*
     C                   DO        XXLEN         X
     C*
     C     X             IFEQ      1
     C*
     C                   MOVEL     *BLANK        NM2(Z)
     C*
     C                   ELSE
     C*
     C     NM1(W)        IFNE      *BLANK
     C     X             ORLE      4
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C*
     C     X             IFEQ      4
     C                   MOVEA     '.'           NM2(Z)
     C                   ELSE
     C                   MOVEA     ','           NM2(Z)
     C                   ENDIF                                                  Z=4
     C*
     C                   ELSE
     C*
     C     NM1(W)        IFEQ      *BLANK
     C                   MOVE      '0'           NM2(Z)
     C                   ELSE
     C                   MOVE      NM1(W)        NM2(Z)
     C                   ENDIF                                                  NM1,W=*BLANK
     C*
     C                   SUB       1             W
     C*
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   ENDIF                                                  X=1
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->XXLEN
     C*** DATE SEPARATORS - 8 DIGIT DATE
     C*
     C     QQMASK        WHENEQ    'DATE'
     C*
     C                   Z-ADD     1             W
     C*
     C     NM1(8)        IFNE      *BLANK
     C*
     C                   DO        10            X
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           NM2(X)
     C                   ELSE
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C                   ENDIF                                                  X=3/6
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDIF                                                  NM1,10<>*BLANK
     C*
     C***  BEGIN ADD  ***  04/08/98  *********************************
     C*** DATE SEPARATORS - 6 DIGIT DATE
     C*
     C     QQMASK        WHENEQ    'DATE6'
     C*
     C                   Z-ADD     1             W
     C*
     C     NM1(6)        IFNE      *BLANK
     C*
     C                   DO        8             X
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           NM2(X)
     C                   ELSE
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C                   ENDIF                                                  X=3/6
     C*
     C                   ENDDO                                                  X->8
     C*
     C                   ENDIF                                                  NM1,6<>*BLANK
     C***  END   ADD  ***  04/08/98  *********************************
     C*** DATE ??
     C*
     C     QQMASK        WHENEQ    'RPMY'
     C*
     C                   Z-ADD     1             W
     C*
     C     NM1(6)        IFNE      *BLANK
     C*
     C                   DO        7             X
     C*
     C     X             IFEQ      3
     C                   MOVEL     '/'           NM2(X)
     C                   ELSE
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C                   ENDIF                                                  X=3/6
     C*
     C                   ENDDO                                                  X->10
     C*
     C                   ENDIF                                                  NM1,10<>*BLANK
     C*** SSN
     C*
     C     QQMASK        WHENEQ    'SSN'
     C*
     C* SEPERATE SS#
     C                   MOVEL     QQNUM         XXSS#             9
     C                   MOVE      *ZERO         SS1               3
     C                   MOVE      *ZERO         SSX               6
     C                   MOVE      *ZERO         SS2               2
     C                   MOVE      *ZERO         SS3               4
     C                   MOVEL     XXSS#         SS1
     C                   MOVE      XXSS#         SSX
     C                   MOVEL     SSX           SS2
     C                   MOVE      SSX           SS3
     C* CREATE SS# STRING
     C***  BEGIN CHG  ***  11/11/98  *********************************
     C********** SS1       CAT  '-':0     XASS#  11
     C     SS1           CAT(P)    '-':0         XASS#            11
     C***  END   CHG  ***  11/11/98  *********************************
     C                   CAT       SS2:0         XASS#
     C                   CAT       '-':0         XASS#
     C                   CAT       SS3:0         XASS#
     C                   MOVEA     XASS#         NM2
     C*
     C***  BEGIN ADD  ***  08/05/98  **********************************
     C*  EDIT MASK
     C     @@MASK        WHENEQ    'EDITCODE'
     C*
     C*     DECIMALS
     C                   Z-ADD     XXLEN         XXINT
     C                   Z-ADD     1             W
     C     XXDEC         IFNE      0
     C     XXLEN         SUB       XXDEC         XXINT             3 0
     C*
     C     XXINT         IFLE      0
     C                   MOVE      '.'           NM2(W)
     C                   ADD       1             W
     C     XXINT         IFLT      0                                                      ->need to
     C                   Z-SUB     XXINT         #ZEROS            3 0                    add zeros
     C                   DO        #ZEROS                                                 after
     C                   MOVE      '0'           NM2(W)                                   decimal
     C                   ADD       1             W                                        point
     C                   ADD       1             XXLEN                                    point
     C                   ENDDO
     C                   ENDIF                                                            XXINT<0
     C                   ENDIF                                                            XXINT<=0
     C*
     C     1             DO        XXLEN         X
     C                   MOVE      NM1(X)        NM2(W)
     C     X             IFEQ      XXINT
     C                   ADD       1             W
     C                   MOVE      '.'           NM2(W)
     C                   ENDIF                                                            X=XXINT
     C                   ADD       1             W
     C                   ENDDO                                                            1-XXLEN
     C                   ADD       1             XXLEN
     C                   ELSE                                                               XXDEC<>0
     C                   MOVEA     NM1           NM2
     C                   ENDIF                                                            XXDEC<>0
     C*
     C*     THOUSANDS
     C     XXTHOU        IFEQ      *ON
     C                   MOVEA     NM2           NM1
     C                   CLEAR                   NM2
     C*
     C                   Z-ADD     XXINT         W
     C     XXINT         IFGT      0
     C     XXINT         DIV       3             Y
     C                   MVR                     XXREM
     C                   ELSE
     C                   Z-ADD     1             XXREM
     C                   Z-ADD     0             Y
     C                   ENDIF
     C*
     C     XXREM         IFEQ      *ZERO
     C                   SUB       1             Y
     C                   ENDIF                                                  XXREM=*ZERO
     C******************** Z-ADD0         #COMS   30
     C                   Z-ADD     Y             #COMS             3 0
     C*
     C     XXINT         ADD       Y             X2INT             3 0
     C                   Z-ADD     X2INT         Z                 3 0
     C*
     C                   DO        X2INT         X
     C*
     C     NM1(W)        IFNE      *BLANK
     C*
     C     X             DIV       4             Y
     C                   MVR                     Y
     C*
     C     Y             IFEQ      *ZERO
     C                   MOVEA     ','           NM2(Z)
     C******************** ADD  1         #COMS
     C                   ELSE
     C                   MOVE      NM1(W)        NM2(Z)
     C                   SUB       1             W
     C                   ENDIF                                                  Y=*ZERO
     C*
     C                   ENDIF                                                  NM1,W<>*BLANK
     C*
     C                   SUB       1             Z
     C*
     C                   ENDDO                                                  X->X2INT
     C*
     C     XXINT         IFGT      0
     C                   Z-ADD     XXINT         W
     C                   ELSE
     C                   Z-ADD     1             W
     C                   MOVEA     NM2           NM1
     C                   CLEAR                   NM2
     C                   ENDIF
     C     W             DO        XXLEN         X
     C     X             ADD       #COMS         Z
     C                   MOVE      NM1(X)        NM2(Z)
     C                   ENDDO                                                  XXINT->XXLEN
     C*
     C     #COMS         IFGT      0
     C                   ADD       #COMS         XXLEN
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXTHOU=*ON
     C*
     C*     ZEROS
     C     *IN51         IFEQ      *ON
     C                   Z-ADD     XXLEN         W
     C     XXDEC         IFGT      0
     C*
     C     W             DOWGT     0
     C     NM2(W)        ANDEQ     ' '
     C                   MOVE      '0'           NM2(W)
     C                   SUB       1             W
     C                   ENDDO
     C                   ELSE
     C     NM2(W)        IFEQ      ' '
     C                   MOVE      '0'           NM2(W)
     C                   ENDIF                                                  NM2,W=' '
     C*
     C                   ENDIF                                                  XXDEC>0
     C                   ENDIF                                                  *IN51=*ON
     C*
     C***  END   ADD  ***  08/05/98  **********************************
     C**
     C*
     C                   OTHER
     C*
     C                   MOVEA     NM1           NM2
     C*
     C                   ENDSL
     C*
     C**
     C*
     C     NEG           IFEQ      *ON
     C***  BEGIN CHG  ***  08/05/98  *********************************
     C     @@MASK        IFEQ      'EDITCODE'
     C     *IN50         IFEQ      *OFF
     C                   MOVEA     NM2           NM1
     C                   MOVEA     NM1           QQNUM
     C     ' '           CHECK     QQNUM         Z
     C                   MOVE      '-'           NM2(Z)
     C     Z             DO        XXLEN         W
     C     W             ADD       1             X
     C                   MOVEA     NM1(W)        NM2(X)
     C                   ENDDO                                                            W->XXLEN
     C                   ELSE
     C     XXLEN         ADD       1             W
     C                   MOVEA     XXNEG         NM2(W)
     C                   ENDIF                                                  *IN50=*ON
     C                   ELSE
     C***  END   CHG  ***  08/05/98  *********************************
     C***  BEGIN CHG  ***  05/14/98  *********************************
     C********** XXLEN     ADD  1         X
     C                   Z-ADD     XXLEN         X
     C***  END   CHG  ***  05/14/98  *********************************
     C                   MOVEA     '-'           NM2(X)
     C***  BEGIN CHG  ***  08/05/98  *********************************
     C                   ENDIF                                                  @@MASK='EDITMAS
     C***  END   CHG  ***  08/05/98  *********************************
     C                   ENDIF                                                  NEG=*ON
     C*
     C                   MOVEL     *BLANKS       QQNUM
     C                   MOVEA     NM2           QQNUM
     C*
     C***  BEGIN CHG  ***  08/05/98  *********************************
     C                   MOVE      *BLANKS       @@MASK
     C***  END   CHG  ***  08/05/98  *********************************
     C*
     C                   RETURN
     C*
     C***  BEGIN CHG  ***  08/05/98  *********************************
     C*================================================================
     C*  SREDIT - SUBROUTINE TO ASSIGN VALUES IF AN EDIT CODE WAS PASSED
     C*================================================================
     C     SREDIT        BEGSR
     C*
     C                   SELECT
     C*
     C     CODE          WHENEQ    '1'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      '  '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    '2'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      '  '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    '3'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      '  '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    '4'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      '  '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    'A'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      'CR'          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    'B'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      'CR'          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    'C'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      'CR'          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    'D'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      'CR'          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    'J'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      '- '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    'K'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      '- '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    'L'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      '- '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    'M'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      '- '          XXNEG
     C                   MOVE      *ON           *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    'N'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      ' -'          XXNEG
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    'O'
     C                   MOVE      *ON           XXTHOU
     C                   MOVE      ' -'          XXNEG
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C     CODE          WHENEQ    'P'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      ' -'          XXNEG
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *ON           *IN51
     C*
     C     CODE          WHENEQ    'Q'
     C                   MOVE      *OFF          XXTHOU
     C                   MOVE      ' -'          XXNEG
     C                   MOVE      *OFF          *IN50
     C                   MOVE      *OFF          *IN51
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SREDIT
     C***  END   CHG  ***  08/05/98  *********************************
