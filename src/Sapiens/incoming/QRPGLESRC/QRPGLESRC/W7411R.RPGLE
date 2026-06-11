     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7411R - CERTIFICATE OF SELF-INSURANCE - ADDITIONAL INSURED
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM PRODUCES A LIST OF ANY ADDITIONAL INSURED        *
     H* THAT IS PRODUCED ALONG WITH THE ORIGINAL CERTIFICATE OF       *
     H* INSURANCE.  THE FORM WILL ONLY PRINT IF THERE IS AT LEAST ONE *
     H* ADDTIONAL INSURED AND ALL ADDTIONAL INSURED DO NOT HAVE THE   *
     H* SAME ADDRESS.  ALSO, ANY ADDITIONAL INSURED WITH THE SAME   . *
     H* ADDRESS AS THE ORIGINAL INSURED WILL NOT BE SHOWN.            *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   EMPLOYER MASTER MAINTENANCE (CMD 17)                        *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: KENNETH KENNEDY                                   *
     H* CREATED: 01/16/91, BASE ENHANCEMENT 8/10/93   PROJECT WC257   *
     H*                                                               *
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   40  EQUALS ZERO IF ALL ADDITIONAL INSURED HAVE THE SAME     *
     H*       ADDRESS AS THE ORIGINAL INSURED                         *
     H*   60  GENERAL CHAIN INDICATOR                                 *
     H*   70  USED TO ONLY PRINT PERC SYMBOL '%' IF A PERCENTAGE      *
     H*       EXISTS                                                  *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/13/09 C:190 PROJECT: 114WTU                                *
     H*                Print additional Entity/Location name field    *
     H*                (ALNAM2) when printing Other Workplace info    *
     H*                on the Certificate of Self-Insurance.          *
     H*                                                               *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                PRINT EXTENDED EMPLR NAME FIELDS.              *
     H*                                                               *
     H* 01/24/01 C:61  SUPPORT WA113 26019                            *
     H*                Print employer mailing address if physical     *
     H*                address not entered.                           *
     H*                                                               *
     H* 01/16/01 C:61 PROJECT: WC996                                  *
     H*               LIST ALL ADDITIONAL NAMED INSUREDS THAT WERE    *
     H*               KEYED UNDER OPT 12 OF 'WORK WITH EMPLOYERS'.    *
     H*                                                               *
     H* 01/08/99 C:61 SUPPORT WA101  14431                            *
     H*               Correct - Owner name not printing if first      *
     H*                         name not entered.                     *
     H*                                                               *
     H* 08/10/98 C:65 PROJECT: WC577                                  *
     H*               ALLOW FOR THE PRINTING OF UNLIMITED OWNERS.     *
     H*                                                               *
     H* 08/07/98 C:65 PROJECT: WC806                                  *
     H*               FEIN WAS PRINTING OVER THE ENTITY/LOCATION NAME *
     H*                                                               *
     H* 06/23/98 C:77 PROJECT: WC806                                  *
     H*               Change to print Entity/Location information on  *
     H*               state certificate of insurance.                 *
     H*                                                               *
     H* 03/09/98 C:65 PROJECT: WC570                                  *
     H*               CHANGE TO USE NEW OWNERS/OFFICERS FILE.         *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWMOOL     IF   E           K DISK
     F*** BEGIN ADD *** 06/23/98 **************************************
     FWMALL     IF   E           K DISK
     F*** END ADD ***** 06/23/98 **************************************
      ***** BEG ADD ******* 01/16/01 **********************************
      *
      * Additional Named Insured
     FWMANL     IF   E           K DISK
      *
      * Employer Policy Year
     FWDELL     IF   E           K DISK
      ***** END ADD ******* 01/16/01 **********************************
      *
     F*** BEGIN CHG ***  03/07/05  ************************************
     F******QSYSPRT1O   F      80     OF     PRINTER
     FQSYSPRT1  O    F   96        PRINTER OFLIND(*INOF)
     F*** END CHG *****  03/07/05  ************************************

     D*** BEGIN DEL ***  08/10/98  ************************************
     D*****               OL          7 30  A            LAST NAME
     D*****               OF          7 15  A            FIRST NAME
     D*****               OP          7  5 2A            PERCENTAGE
     D*****               ORL         7 68  A
     D*****               ORP         7  5 2A
     D*** END DEL *****  08/10/98  ************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  EMPZIP                 1      9  0
     D  XXZPP1                 1      5  0
     D  XXZPP2                 6      9  0
     D*** BEGIN DEL ***  08/10/98  ************************************
     D*****       DS
     D*****                                   1 210 OL
     D*****                                   1  30 OOOLN1
     D*****                                  31  60 OOOLN2
     D*****                                  61  90 OOOLN3
     D*****                                  91 120 OOOLN4
     D*****                                 121 150 OOOLN5
     D*****                                 151 180 OOOLN6
     D*****                                 181 210 OOOLN7
     D*****       DS
     D*****                                   1 105 OF
     D*****                                   1  15 OOOFN1
     D*****                                  16  30 OOOFN2
     D*****                                  31  45 OOOFN3
     D*****                                  46  60 OOOFN4
     D*****                                  61  75 OOOFN5
     D*****                                  76  90 OOOFN6
     D*****                                  91 105 OOOFN7
     D*****       DS
     D*****                                   1  35 OP
     D*****                                   1   52OOOFP1
     D*****                                   6  102OOOFP2
     D*****                                  11  152OOOFP3
     D*****                                  16  202OOOFP4
     D*****                                  21  252OOOFP5
     D*****                                  26  302OOOFP6
     D*****                                  31  352OOOFP7
     D*** END DEL *****  08/10/98  ************************************
     D                 DS
     D*** BEGIN CHG ***  03/07/05  ************************************
     D*********                               1  68 XXLINE
     D*********                               1  40 XXPAR1
     D*** BEGIN ADD *** 06/23/98 **************************************
     D*********                              30  38 XXPA1A
     D*** END ADD ***** 06/23/98 **************************************
     D*********                              42  68 XXPAR2
     D  XXLINE                 1     78
     D  XXPAR1                 1     50
     D  XXPA11                41     50
     D  XXPA1A                40     48
     D  XXPAR2                52     78
     D*** END CHG *****  03/07/05  ************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*** BEGIN ADD *** 06/23/98 **************************************
     C*
     C*  KEY LIST FOR ENTITY/LOCATION FILE
     C*
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** END ADD ***** 06/23/98 **************************************
     C*
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     OOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXELID
      ***** BEG ADD ******* 01/16/01 **********************************
      *
      *  Additional Named Insured
      *
     C     ANKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  Employer Detail
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      ***** END ADD ******* 01/16/01 **********************************
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C*** BEGIN ADD *** 06/23/98 **************************************
     C     *LIKE         DEFINE    EMEMP#        XXPEMP
     C*** END ADD ***** 06/23/98 **************************************
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    OOELID        XXELID
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVEL     *BLANKS       XXELID
     C*** BEGIN DEL ***  08/10/98  ************************************
     C*****                MOVEA*BLANKS   ORL
     C*****                MOVEA*ZEROS    ORP
     C*****                MOVEA*BLANKS   OL
     C*****                MOVEA*BLANKS   OF
     C*****                MOVEA*ZEROS    OP
     C*** END DEL *****  08/10/98  ************************************
     C*
     C*** BEGIN ADD *** 06/23/98 **************************************
     C                   Z-ADD     0             XXPEMP
     C*** END ADD ***** 06/23/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN ADD *** 06/23/98 **************************************
     C*  BEGIN READING EMPLOYER MASTER FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      EMDIV         XXDIV
     C                   MOVEL     EMPAD1        XXPAD1
      *
      *  PRINT ONLY ONE CERTIFICATE PER EMPLOYER
      *
     C     EMEMP#        IFNE      XXPEMP
     C                   Z-ADD     *ZEROS        PAGE
     C                   Z-ADD     EMEMP#        XXPEMP
     C                   ENDIF
     C*** END ADD ***** 06/23/98 **************************************
     C*
     C*   ONLY PRINT THIS CERTIFICATE IF THERE ARE ADDITIONAL DIVISIONS
     C*
     C*** BEGIN CHG *** 06/23/98 **************************************
     C**********
     C********** EMKEY1    CHAINPEM01                60
     C********** *IN60     DOWEQ'0'
     C**********
     C********** EMDIV     IFNE XXDIV
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** EMKEY1    READEPEM01                    60
     C**********           END
     C**********
     C********** *IN60     CABEQ'1'       TERM
     C**********
     C********** STRT      TAG
     C*
     C                   MOVE      *OFF          *IN40
     C*
     C*** BEGIN ADD ***  08/10/98  ************************************
     C*
     C* PRINT CERTIFICATE IF THERE ARE ANY OWNERS SINCE THEY ARE NO
     C* NO LONGER PRINTING ON THE ORIGINAL CERTIFICATE ITSELF.
     C*
     C     OOKY          CHAIN     POO01                              60
     C     *IN50         IFEQ      *OFF
     C                   MOVE      *ON           *IN40
     C                   ENDIF                                                  *IN50 IFEQ
     C*** END ADD *****  08/10/98  ************************************
     C*
     C     EMKEY1        CHAIN     PEM01                              60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C*
     C     *IN60         DOWEQ     *OFF
     C     *IN40         ANDEQ     *OFF
     C*
     C     EMPAD1        IFNE      XXPAD1
     C*
     C                   MOVE      *ON           *IN40
     C*
     C                   ENDIF                                                  EMPAD1 IFNE
     C*
     C     *IN40         IFEQ      *OFF
     C     EMKEY1        READE     PEM01                                  60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C                   ENDIF                                                  *IN40 IFEQ
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*
     C*  If No additional Insured found, check Entity/Location File.
     C*
     C     ALKY          CHAIN     PAL01                              62
     C*
     C     *IN62         DOWEQ     *OFF
     C     *IN40         ANDEQ     *OFF
     C*
     C*  !!! PRINT REGARDLESS OF WHETHER ADDRESS IS DIFFERENT!!!
     C                   MOVE      *ON           *IN40
     C*
     C     *IN40         IFEQ      *OFF
     C     ALKY          READE     PAL01                                  62
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN62 DOWEQ
     C*
     C     *IN40         IFEQ      *ON
     C*** END CHG ***** 06/23/98 **************************************
     C*
     C                   EXCEPT    HEAD1
     C*** BEGIN ADD ***  08/10/98  ************************************
     C                   EXCEPT    HEAD2
     C*** END ADD *****  08/10/98  ************************************
     C*
     C*   GET NAME AND ADDRESS OF EMPLOYER AS PRINTED ON
     C*   ORIGINAL CERTIFICATE OF INSURANCE
     C*
     C     EMKEY         CHAIN     PEM01                              60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     EMPCTY        XXLAST           56
     C                   MOVEL     EMPST         XXSTZP           13
     C                   MOVE      EMPZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C                   MOVEL     EMPAD1        XXPAD1           40
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     EMPZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END                                                    XXZIP4
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C     OOKY          SETLL     POO01
     C                   MOVE      *OFF          *IN50
     C*
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****      1         DO   7         X       30
     C                   Z-ADD     0             X                 3 0
     C     *IN50         DOWEQ     *OFF
     C                   ADD       1             X
     C*** END CHG *****  08/10/98  ************************************
     C*
     C  N50OOKY          READE     POO01                                  50
     C     *IN50         IFEQ      *ON
     C                   MOVE      *BLANKS       OOOFSN
     C                   MOVE      *BLANKS       OOOLSN
     C                   Z-ADD     0             OOOFPC
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  08/10/98  ************************************
     C     *INOF         IFEQ      *ON
     C*
     C                   EXCEPT    FOOT
     C                   EXCEPT    HEAD1
     C*
     C     *IN50         IFEQ      *OFF
     C                   EXCEPT    HEAD2
     C                   ENDIF                                                  *IN50
     C*
     C                   MOVE      *OFF          *INOF
     C*
     C                   ENDIF                                                  *INOF IFEQ
     C*
     C     *IN50         IFEQ      *ON
     C*** BEGIN CHG ***  03/07/05  ************************************
     C*********  X         ANDLT5
     C     X             ANDLT     6
     C*** END CHG *****  03/07/05  ************************************
     C                   MOVE      *OFF          *IN50
     C                   ENDIF                                                  *IN50 IFEQ
     C*** END ADD *****  08/10/98  ************************************
     C*
     C                   MOVE      *BLANKS       XXLINE
     C*
     C*** BEGIN CHG ***  03/07/05  ************************************
     C*********  X         IFEQ 1
     C*********            MOVELEMNAME    XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 2
     C*********            MOVELEMPAD1    XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 3
     C*********  EMPAD2    ANDEQ*BLANKS
     C*********            MOVELXXCSZ     XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 3
     C*********  EMPAD2    ANDNE*BLANKS
     C*********            MOVELEMPAD2    XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 4
     C*********  EMPAD2    ANDNE*BLANKS
     C*********            MOVELXXCSZ     XXPAR1
     C*********            END                             X = 4
     C*********            END                             X = 3
     C*********            END                             X = 3 B
     C*********            END                             X = 2
     C*********            END                             X = 1
     C                   SELECT
     C     X             WHENEQ    1
     C                   MOVEL     EMNAME        XXPAR1
     C                   MOVEL     EMNAM2        XXPA11
     C     X             WHENEQ    2
     C                   MOVE      EMNAM2        XXPAR1
     C     X             WHENEQ    3
     C                   MOVEL     EMPAD1        XXPAR1
     C     X             WHENEQ    4
     C     EMPAD2        ANDEQ     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C     X             WHENEQ    4
     C     EMPAD2        ANDNE     *BLANKS
     C                   MOVEL     EMPAD2        XXPAR1
     C     X             WHENEQ    5
     C     EMPAD2        ANDNE     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C                   ENDSL
     C*** END CHG *****  03/07/05  ************************************
     C*
      ***** BEG DEL ******* 01/08/99 **********************************
     C********** OOOFSN    IFNE *BLANKS
     C********** OOOLSN    ANDNE*BLANKS
     C**********
     C**********           MOVE *BLANKS   XXLAST 56
     C**********           MOVE *BLANKS   XXFRST 15
     C**********           MOVELOOOLSN    XXLAST
     C**********           MOVELOOOFSN    XXFRST
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           MOVELXXLAST    XXPAR2
     C**********
     C**********           END                             OF,OL
      ***** END DEL ******* 01/08/99 **********************************
      ***** BEG ADD ******* 01/08/99 **********************************
      *
     C     OOOFSN        IFNE      *BLANKS
     C     OOOLSN        ORNE      *BLANKS
      *
     C                   MOVEL(P)  OOOLSN        XXPAR2
      *
     C     XXPAR2        IFEQ      *BLANKS
     C                   MOVEL(P)  OOOFSN        XXPAR2
     C                   ELSE
     C     OOOFSN        IFNE      *BLANKS
     C                   CAT(P)    ',':0         XXPAR2
     C                   CAT(P)    OOOFSN:1      XXPAR2
     C                   ENDIF                                                  OOOFSN
     C                   ENDIF                                                  XXPAR2
      *
     C                   END                                                    OF,OL
      *
      ***** END ADD ******* 01/08/99 **********************************
     C*
     C*** BEGIN DEL ***  08/10/98  ************************************
     C*****                Z-ADDX         L       30
     C*** END DEL *****  08/10/98  ************************************
     C     XXLINE        IFNE      *BLANKS
     C     OOOFPC        ORNE      *ZEROS
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****                MOVELXXLINE    ORL,L
     C*****                Z-ADDOOOFPC    ORP,L
     C*****      *ZEROS    COMP ORP,L                7070
     C     *ZEROS        COMP      OOOFPC                             7070
     C*** END CHG *****  08/10/98  ************************************
     C                   EXCEPT    DET1
     C                   END                                                    XXLINE
     C*
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****                END                             DO X 7
     C                   ENDDO                                                  *IN50 DOWEQ
     C*** END CHG *****  08/10/98  ************************************
     C*
     C                   END                                                    IFEQ *IN60
     C*
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****                EXCPTHEAD2
     C                   EXCEPT    HEAD3
     C*** END CHG *****  08/10/98  ************************************
      ***** BEG ADD ******* 01/16/01 **********************************
      *
      *  List Additional Named Insured
      *
     C     ELKY          SETGT     PEL01
     C     ELKY          READPE    PEL01                                  61
      *
     C     *IN61         IFEQ      *OFF
     C     ANKY          SETLL     PAN01
     C     ANKY          READE     PAN01                                  61
      *
     C     *IN61         DOWEQ     *OFF
      *
     C     ANDLDT        IFEQ      *ZERO
     C     ANDLDT        ORGT      ELPFDT
      *
     C     ANSTAT        IFEQ      'A'
      *
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    FOOT
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF IFEQ
      *
     C                   EXCEPT    ADDNAM
     C                   ENDIF                                                  ANSTAT
      *
     C                   ENDIF                                                  ANDLDT
      *
     C     ANKY          READE     PAN01                                  61
     C                   ENDDO
      *
     C                   ENDIF                                                  ELKY
      *
      ***** END ADD ******* 01/16/01 **********************************
      *
     C*
     C     EMKEY1        CHAIN     PEM01                              60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C     *IN60         DOWEQ     '0'
     C*
     C     EMDIV         IFNE      XXDIV
     C*
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     EMPCTY        XXLAST           56
     C                   MOVEL     EMPST         XXSTZP           13
     C                   MOVE      EMPZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     EMPZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END                                                    XXZIP4
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C*
     C     OOKY          SETLL     POO01
     C                   MOVE      *OFF          *IN50
     C*
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****      1         DO   7         X       30
     C                   Z-ADD     0             X                 3 0
     C     *IN50         DOWEQ     *OFF
     C                   ADD       1             X
     C*** END CHG *****  08/10/98  ************************************
     C*
     C  N50OOKY          READE     POO01                                  50
     C     *IN50         IFEQ      *ON
     C                   MOVE      *BLANKS       OOOFSN
     C                   MOVE      *BLANKS       OOOLSN
     C                   Z-ADD     0             OOOFPC
     C                   ENDIF
     C*** BEGIN ADD ***  08/10/98  ************************************
     C     *INOF         IFEQ      *ON
     C*
     C                   EXCEPT    FOOT
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      *OFF          *INOF
     C*
     C                   ENDIF                                                  *INOF IFEQ
     C*
     C     *IN50         IFEQ      *ON
     C*** BEGIN CHG ***  03/07/05  ************************************
     C*********  X         ANDLT5
     C     X             ANDLT     6
     C*** END CHG *****  03/07/05  ************************************
     C                   MOVE      *OFF          *IN50
     C                   ENDIF                                                  *IN50 IFEQ
     C*** END ADD *****  08/10/98  ************************************
     C*
     C                   MOVE      *BLANKS       XXLINE
     C*
     C*** BEGIN CHG ***  03/07/05  ************************************
     C*********  X         IFEQ 1
     C*********            MOVELEMNAME    XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 2
     C*********            MOVELEMPAD1    XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 3
     C*********  EMPAD2    ANDEQ*BLANKS
     C*********            MOVELXXCSZ     XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 3
     C*********  EMPAD2    ANDNE*BLANKS
     C*********            MOVELEMPAD2    XXPAR1
     C*********            ELSE
     C*********  X         IFEQ 4
     C*********  EMPAD2    ANDNE*BLANKS
     C*********            MOVELXXCSZ     XXPAR1
     C*********            END                             X = 4
     C*********            END                             X = 3
     C*********            END                             X = 3 B
     C*********            END                             X = 2
     C*********            END                             X = 1
     C                   SELECT
     C     X             WHENEQ    1
     C                   MOVEL     EMNAME        XXPAR1
     C                   MOVEL     EMNAM2        XXPA11
     C     X             WHENEQ    2
     C                   MOVE      EMNAM2        XXPAR1
     C     X             WHENEQ    3
     C                   MOVEL     EMPAD1        XXPAR1
     C     X             WHENEQ    4
     C     EMPAD2        ANDEQ     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C     X             WHENEQ    4
     C     EMPAD2        ANDNE     *BLANKS
     C                   MOVEL     EMPAD2        XXPAR1
     C     X             WHENEQ    5
     C     EMPAD2        ANDNE     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C                   ENDSL
     C*** END CHG *****  03/07/05  ************************************
     C*
      ***** BEG DEL ******* 01/08/99 **********************************
     C********** OOOFSN    IFNE *BLANKS
     C********** OOOLSN    ANDNE*BLANKS
     C**********
     C**********           MOVE *BLANKS   XXLAST 56
     C**********           MOVE *BLANKS   XXFRST 15
     C**********           MOVELOOOLSN    XXLAST
     C**********           MOVELOOOFSN    XXFRST
     C**********           MOVE '1'       XXTERM  1
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           MOVELXXLAST    XXPAR2
     C**********
     C**********           END                             OF,OL
      ***** END DEL ******* 01/08/99 **********************************
      ***** BEG ADD ******* 01/08/99 **********************************
      *
     C     OOOFSN        IFNE      *BLANKS
     C     OOOLSN        ORNE      *BLANKS
      *
     C                   MOVEL(P)  OOOLSN        XXPAR2
      *
     C     XXPAR2        IFEQ      *BLANKS
     C                   MOVEL(P)  OOOFSN        XXPAR2
     C                   ELSE
     C     OOOFSN        IFNE      *BLANKS
     C                   CAT(P)    ',':0         XXPAR2
     C                   CAT(P)    OOOFSN:1      XXPAR2
     C                   ENDIF                                                  OOOFSN
     C                   ENDIF                                                  XXPAR2
      *
     C                   END                                                    OF,OL
      *
      ***** END ADD ******* 01/08/99 **********************************
     C*
     C     XXLINE        IFNE      *BLANKS
     C     OOOFPC        ORNE      *ZEROS
     C*
     C     *ZEROS        COMP      OOOFPC                             7070
     C                   EXCEPT    DET2
     C*
     C                   END                                                    XXLINE
     C*
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****                END                             DO X 7
     C                   ENDDO                                                  *IN50 IFEQ
     C*** END CHG *****  08/10/98  ************************************
     C*
     C                   EXCEPT    SPACE
     C*
     C*** BEGIN CHG ***  08/10/98  ************************************
     C*****      *INOF     IFEQ '1'
     C*****                EXCPTFOOT
     C*****                EXCPTHEAD1
     C*****      1         DO   7         L
     C*****      ORL,L     IFNE *BLANKS
     C*****      ORP,L     ORNE *ZEROS
     C*****      *ZEROS    COMP ORP,L                7070
     C*****                EXCPTDET1
     C*****                END
     C*****                END
     C*****                EXCPTHEAD2
     C*****                Z-ADD0         X
     C*****                MOVE '0'       *INOF
     C*****                END                             *INOF
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    FOOT
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF IFEQ
     C*** END CHG *****  08/10/98  ************************************
     C*
     C                   END                                                    XXDIV <> EMDIV
     C*
     C     EMKEY1        READE     PEM01                                  60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C*
     C                   END                                                    DOWEQ *IN60
     C*
     C*** BEGIN ADD ***  08/07/98  ************************************
     C                   MOVE      *BLANKS       XXLINE
     C                   MOVE      *OFF          *IN70
     C*** END ADD *****  08/07/98  ************************************
     C*** BEGIN ADD *** 06/23/98 **************************************
     C*
     C*  PROCESS ENTITY/LOCATION INFO AND PRINT
     C*
     C     ALKY          CHAIN     PAL01                              62
     C     *IN62         DOWEQ     *OFF
     C*
     C*** BEGIN ADD ***  08/10/98  ************************************
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    FOOT
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD3
     C                   Z-ADD     0             X
     C                   MOVE      '0'           *INOF
     C                   END                                                    *INOF
     C*** END ADD *****  08/10/98  ************************************
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
      ***** BEG ADD ******* 01/16/01 **********************************
      *
     C     ALPAD1        IFNE      *BLANKS
      ***** END ADD ******* 01/16/01 **********************************
     C                   MOVEL     ALPCTY        XXLAST
     C                   MOVEL     ALPST         XXSTZP
     C                   MOVE      ALPZIP        XXSTZP
      ***** BEG ADD ******* 01/16/01 **********************************
      *
     C                   ELSE
      *
     C                   MOVEL     ALMCTY        XXLAST
     C                   MOVEL     ALMST         XXSTZP
     C                   MOVE      ALMZIP        XXSTZP
     C                   ENDIF                                                  ALPAD1
      *
      ***** END ADD ******* 01/16/01 **********************************
     C                   MOVE      XXSTZP        XXZIP4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5
      ***** BEG ADD ******* 01/16/01 **********************************
      *
     C     ALPAD1        IFNE      *BLANKS
      ***** END ADD ******* 01/16/01 **********************************
     C                   MOVEL     ALPZIP        XXST10
      ***** BEG ADD ******* 01/16/01 **********************************
      *
     C                   ELSE
     C                   MOVEL     ALMZIP        XXST10
     C                   ENDIF                                                  ALPAD1
      *
      ***** END ADD ******* 01/16/01 **********************************
     C                   MOVE      XX5           XXST10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF                                                  XXZIP4
     C*
     C                   MOVEL     XXSTZP        XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ
     C*
     C     1             DO        7             X
     C*
     C                   MOVE      *BLANKS       XXLINE
     C*
     C     X             IFEQ      1
     C                   MOVEL     ALNAME        XXPAR1
     C*** BEGIN ADD ***  04/13/09  ************************************
     C                   MOVEL     ALNAM2        XXPA11
     C*** END   ADD ***  04/13/09  ************************************
     C*** BEGIN CHG ***  08/07/98  ************************************
     C*****                MOVE ALFEIN    XXPA1A
     C                   MOVE      ALFEIN        XXPAR2
     C*** END CHG *****  08/07/98  ************************************
     C                   ELSE
     C*** BEGIN CHG ***  08/07/98  ************************************
     C*****                MOVE *BLANKS   XXPA1A
     C                   MOVE      *BLANKS       XXPAR2
     C*** END CHG *****  08/07/98  ************************************
      ***** BEG ADD ******* 01/16/01 **********************************
      ***** BEG ADD ******* 04/13/09 **********************************
     C     X             IFEQ      2
     C                   MOVE      ALNAM2        XXPAR1
      ***** END ADD ******* 04/13/09 **********************************
      *
     C     ALPAD1        IFNE      *BLANKS
      ***** END ADD ******* 01/16/01 **********************************
      ***** BEG CHG ******* 04/13/09 **********************************
     C*********  X         IFEQ 2
     C     X             IFEQ      4
      ***** END CHG ******* 04/13/09 **********************************
     C                   MOVEL     ALPAD1        XXPAR1
     C                   ELSE
      ***** BEG CHG ******* 04/13/09 **********************************
     C*********  X         IFEQ 3
     C     X             IFEQ      5
      ***** END CHG ******* 04/13/09 **********************************
     C     ALPAD2        ANDEQ     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C                   ELSE
      ***** BEG CHG ******* 04/13/09 **********************************
     C*********  X         IFEQ 3
     C     X             IFEQ      5
      ***** END CHG ******* 04/13/09 **********************************
     C     ALPAD2        ANDNE     *BLANKS
     C                   MOVEL     ALPAD2        XXPAR1
     C                   ELSE
     C     X             IFEQ      4
     C     ALPAD2        ANDNE     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
      ***** BEG ADD ******* 04/13/09 **********************************
     C                   ENDIF                                                  X = 5
      ***** END ADD ******* 04/13/09 **********************************
     C                   ENDIF                                                  X = 4
     C                   ENDIF                                                  X = 3
     C                   ENDIF                                                  X = 3 B
     C                   ENDIF                                                  X = 2
      ***** BEG ADD ******* 01/16/01 **********************************
      *
     C                   ELSE
      *
     C     X             IFEQ      2
     C                   MOVEL     ALMAD1        XXPAR1
     C                   ELSE
     C     X             IFEQ      3
     C     ALMAD2        ANDEQ     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C                   ELSE
     C     X             IFEQ      3
     C     ALMAD2        ANDNE     *BLANKS
     C                   MOVEL     ALMAD2        XXPAR1
     C                   ELSE
     C     X             IFEQ      4
     C     ALMAD2        ANDNE     *BLANKS
     C                   MOVEL     XXCSZ         XXPAR1
     C                   ENDIF                                                  X = 4
     C                   ENDIF                                                  X = 3
     C                   ENDIF                                                  X = 3 B
     C                   ENDIF                                                  X = 2
     C                   ENDIF                                                  ALPAD1
      *
      ***** END ADD ******* 01/16/01 **********************************
     C                   ENDIF                                                  X = 1
      *
     C********** AF,X      IFNE *BLANKS
     C********** AL,X      ANDNE*BLANKS
     C**********
     C**********           MOVE *BLANKS   XXLAST
     C**********           MOVE *BLANKS   XXFRST
     C**********           MOVELAL,X      XXLAST
     C**********           MOVELAF,X      XXFRST
     C**********           MOVE '1'       XXTERM
     C**********           CALL 'SRNAME'
     C**********           PARM           XXLAST
     C**********           PARM           XXFRST
     C**********           PARM           XXTERM
     C**********           MOVELXXLAST    XXPAR2
     C**********
     C**********           END                             OF,OL
     C*
     C     XXLINE        IFNE      *BLANKS
     C                   EXCEPT    DET2
     C                   ENDIF                                                  XXLINE
     C*
     C                   END                                                    DO X 7
     C*
     C                   EXCEPT    SPACE
     C*
     C*** BEGIN DEL ***  08/10/98  ************************************
     C*****      *INOF     IFEQ '1'
     C*****                EXCPTFOOT
     C*****                EXCPTHEAD1
     C*****      1         DO   7         L
     C*****      ORL,L     IFNE *BLANKS
     C*****      ORP,L     ORNE *ZEROS
     C*****      *ZEROS    COMP ORP,L                7070
     C*****                EXCPTDET1
     C*****                END
     C*****                END
     C*****                EXCPTHEAD2
     C*****                Z-ADD0         X
     C*****                MOVE '0'       *INOF
     C*****                END                             *INOF
     C*** END DEL *****  08/10/98  ************************************
     C*
     C     ALKY          READE     PAL01                                  62
     C*
     C                   ENDDO                                                  *IN62 DOWEQ
     C*** END ADD ***** 06/23/98 **************************************
     C*
     C                   EXCEPT    FOOT
     C*
     C*** BEGIN ADD *** 06/23/98 **************************************
     C                   ENDIF                                                  EMEMP# IFNE
     C*
     C     EMKEY         READE     PEM01                                  60
      ***** BEG ADD ******* 01/24/01 **********************************
     C                   EXSR      GETADR
      ***** END ADD ******* 01/24/01 **********************************
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*** END ADD ***** 06/23/98 **************************************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/24/01 **********************************
      *
      *
      *
      *=============================================================
      * GETADR - Use employer mailing address if no physical address
      *=============================================================
     C     GETADR        BEGSR
      *
     C     EMPAD1        IFEQ      *BLANKS
     C                   MOVEL(P)  EMMAD1        EMPAD1
     C                   MOVEL(P)  EMMAD2        EMPAD2
     C                   MOVEL(P)  EMMCTY        EMPCTY
     C                   MOVEL(P)  EMMST         EMPST
     C                   Z-ADD     EMMZIP        EMPZIP
     C                   ENDIF                                                  EMPAD1
      *
     C                   ENDSR                                                  GETADR
      *
      *
      *
      ***** END ADD ******* 01/24/01 **********************************
     C*
     OQSYSPRT1  E            HEAD1          0 06
     O                                           26 'CERTIFICATE OF INSURANCE'
     O          E            HEAD1          1
     O                                           26 'CERTIFICATE OF INSURANCE'
     O          E            HEAD1          0
     O                                           18 'ADDITIONAL NAMED'
     O                                           35 ' FLORIDA INSURED/'
     O                                           55 'ADDITIONAL ADDRESSES'
     O          E            HEAD1          2
     O                                           18 'ADDITIONAL NAMED'
     O                                           35 ' FLORIDA INSURED/'
     O                                           55 'ADDITIONAL ADDRESSES'
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****   E  0             HEAD1
     O          E            HEAD2          0
     O*** END CHG *****  08/10/98  ************************************
     O                                           22 'NAME AND ADDRESS AS '
     O                                           38 'STATED ON POLICY'
     O*** BEGIN CHG ***  03/07/05  ************************************
     O*********                          55 'LEGAL OWNERS'
     O                                           65 'LEGAL OWNERS'
     O*** END CHG *****  03/07/05  ************************************
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****   E  2             HEAD1
     O          E            HEAD2          2
     O*** END CHG *****  08/10/98  ************************************
     O                                           22 '____________________'
     O                                           38 '________________'
     O*** BEGIN CHG ***  03/07/05  ************************************
     O*********                          55 '____________'
     O                                           65 '____________'
     O*** END CHG *****  03/07/05  ************************************
     O          E            DET1           1
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****                    ORL,L     70
     O*****                    ORP,L 4   76
     O*** BEGIN CHG ***  03/07/05  ************************************
     O*********                XXLINE    70
     O*********                OOOFPC4   76
     O*** END CHG *****  08/10/98  ************************************
     O*********        70                78 '%'
     O                       XXLINE              80
     O                       OOOFPC        4     86
     O               70                          88 '%'
     O*** END CHG *****  03/07/05  ************************************
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****   E 10             HEAD2
     O          E            HEAD3       1  0
     O*** END CHG *****  08/10/98  ************************************
     O                                           20 'ADDITIONAL INSURED'
     O*** BEGIN DEL *** 06/23/98 **************************************
     O***********************************55 'LEGAL OWNERS'
     O*** END DEL ***** 06/23/98 **************************************
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****   E  2             HEAD2
     O          E            HEAD3          2
     O*** END CHG *****  08/10/98  ************************************
     O                                           20 '__________________'
     O*** BEGIN DEL *** 06/23/98 **************************************
     O***********************************55 '____________'
     O*** END DEL ***** 06/23/98 **************************************
     O          E            DET2           1
     O*** BEGIN CHG ***  03/07/05  ************************************
     O*********                XXLINE    70
     O*** BEGIN CHG ***  08/10/98  ************************************
     O*****                    OP,X  4   76
     O*********                OOOFPC4   76
     O*** END CHG *****  08/10/98  ************************************
     O*********        70                78 '%'
     O                       XXLINE              80
     O                       OOOFPC        4     86
     O               70                          88 '%'
     O*** END CHG *****  03/07/05  ************************************
     O          E            SPACE          1
     O          E            FOOT             63
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
      ***** BEG ADD ******* 01/16/01 **********************************
      *
     O          E            ADDNAM         2
     O                       ANNAME              42
      ***** END ADD ******* 01/16/01 **********************************
      *
