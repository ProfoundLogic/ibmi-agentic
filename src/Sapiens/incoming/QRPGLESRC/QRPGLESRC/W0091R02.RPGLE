     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:
     H*                                                               *
     H* 08/19/13 C:65  PROJECT: 140WAU                                *
     H*                ADD IN CODING TO DISPLAY THE NEW TYPES FOR     *
     H*                DEPOSIT AND TAX ON DEPOSIT.                    *
     H*                                                               *
     H* 04/20/11 C:136 SUPPORT: WA132 76619                           *
     H*                STRAIGHTEN OUT HEADINGS                        *
     H*                                                               *
     H* 12/11/08 C:65  PROJECT: WC1769                                *
     H*                ADD IN CODING TO DISPLAY THE 6TH AND 7TH       *
     H*                ASSESSMENTS. STATUS VALUES TO INDICATE A/R     *
     H*                TYPE HAS BEEN CHANGED.  SUMMARIZED ASSESSMENTS *
     H*                ARE NOW LISTED WITH A STATUS OF D THRU J.      *
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Charge assessments on the same basis as premium*
     H*                installments                                   *
     H*                                                               *
     H* 11/30/04 C:65  PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*
     H* 04/25/03 C:65  PROJECT: WC1116
     H*                ADD CODE FOR DISPLAYING TWICE A MONTH BILLINGS.
     H*
     H* 04/21/03 C:65  PROJECT: WC1115
     H*                ADD CODE FOR DISPLAYING WEEKLY BILLINGS.
     H*
     H*****************************************************************
     FW0091F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FWAEAP     UF A E             DISK
     F                                     RENAME(PEA01:DBRCDF)
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Revised Billing Adju-
     D                                     stments')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *** BEG ADD ***** 04/20/11 ************************************
     D PRHD            C                   CONST('Premium Amount')
     D TXHD            C                   CONST('Tax Amount')
     D A1HD            C                   CONST('1st Assessment')
     D A2HD            C                   CONST('2nd Assessment')
     D A3HD            C                   CONST('3rd Assessment')
     D A4HD            C                   CONST('4th Assessment')
     D A5HD            C                   CONST('5th Assessment')
     D A6HD            C                   CONST('6th Assessment')
     D A7HD            C                   CONST('7th Assessment')
      *** END ADD ***** 04/20/11 ************************************
     D                 DS
     D  DA                     1    096  0
     D                                     DIM(12)
     D  SCDAT1               001    008  0
     D  SCDAT2               009    016  0
     D  SCDAT3               017    024  0
     D  SCDAT4               025    032  0
     D  SCDAT5               033    040  0
     D  SCDAT6               041    048  0
     D  SCDAT7               049    056  0
     D  SCDAT8               057    064  0
     D  SCDAT9               065    072  0
     D  SCDA10               073    080  0
     D  SCDA11               081    088  0
     D  SCDA12               089    096  0
     D                 DS
     D  PA                     1    108  2
     D                                     DIM(12)
     D  SCPAY1                 1      9  2
     D  SCPAY2                10     18  2
     D  SCPAY3                19     27  2
     D  SCPAY4                28     36  2
     D  SCPAY5                37     45  2
     D  SCPAY6                46     54  2
     D  SCPAY7                55     63  2
     D  SCPAY8                64     72  2
     D  SCPAY9                73     81  2
     D  SCPA10                82     90  2
     D  SCPA11                91     99  2
     D  SCPA12               100    108  2
     D                 DS
     D  PO                     1    084  0
     D                                     DIM(12)
     D  EAPO01               001    007  0
     D  EAPO02               008    014  0
     D  EAPO03               015    021  0
     D  EAPO04               022    028  0
     D  EAPO05               029    035  0
     D  EAPO06               036    042  0
     D  EAPO07               043    049  0
     D  EAPO08               050    056  0
     D  EAPO09               057    063  0
     D  EAPO10               064    070  0
     D  EAPO11               071    077  0
     D  EAPO12               078    084  0
     D                 DS
     D  AM                     1    108  2
     D                                     DIM(12)
     D  EAAM01                 1      9  2
     D  EAAM02                10     18  2
     D  EAAM03                19     27  2
     D  EAAM04                28     36  2
     D  EAAM05                37     45  2
     D  EAAM06                46     54  2
     D  EAAM07                55     63  2
     D  EAAM08                64     72  2
     D  EAAM09                73     81  2
     D  EAAM10                82     90  2
     D  EAAM11                91     99  2
     D  EAAM12               100    108  2

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  GROUP MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
      *
      *  EMPLOYER MASTER FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  EDIT ENTERED DATES
     C*  DATE AND AMOUNT FIELD ARE MUTUALLY INCLUSIVE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD            7 0
     C                   Z-ADD     XXCURD        XXCURR            7 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     1             DO        12            Y                 3 0
     C     DA(Y)         IFNE      *ZERO
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C     SRERR         IFEQ      '1'
     C     Y             ADD       19            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     DA(Y)         IFNE      0
     C     PA(Y)         ANDEQ     0
     C     DA(Y)         OREQ      0
     C     PA(Y)         ANDNE     0
     C     Y             ADD       37            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C     DA(Y)         IFNE      *ZEROS
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWK60            7 0
     C*
     C     XXWK60        IFLT      XXCURR
     C     Y             ADD       67            X
     C                   MOVE      '1'           *IN(X)
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  TOTAL MUST MATCH CALCULATED ADJUSTMENT
     C*
     C                   XFOOT     PA            XXTEST           11 2
     C*
     C     XXTEST        IFNE      SCTOT
     C*** BEGIN ADD ***  04/21/03  ************************************
     C     *IN62         ANDEQ     *OFF
     C*** END ADD *****  04/21/03  ************************************
     C                   MOVE      '1'           *IN32
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C                   END
     C*
     C     STRT          TAG
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*===============================================================
     C* KFDO - SUBROUTINE FOR F0F
     C*===============================================================
     C*
     C*  F06=A/R INQUIRY
     C*
     C     KFDO          BEGSR
     C*
     C                   MOVE      EAFYR         QQFYR             3
     C                   MOVE      EAEMP#        QQEMP#            9
     C                   MOVE      EADIV         QQDIV             5
     C                   MOVE      EAFND         QQFND             3
     C                   MOVE      EACO#         QQCO#             3
     C                   MOVE      *BLANKS       QQDATE            8
     C                   MOVE      *BLANKS       QQAM              9
     C                   MOVE      *BLANKS       QQCHK#            7
     C                   MOVE      '2'           XXCALL            1
     C                   MOVE      'R'           QQRTC             1
     C                   CALL      'W0612R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    XXCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQDATE
     C                   PARM                    QQAM
     C                   PARM                    QQCHK#
     C                   PARM                    QQRTC             1
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C                   Z-ADD     0             SCTOT
     C     1             DO        12            X                 2 0
     C                   Z-ADD     AM(X)         PA(X)
     C                   Z-ADD     PO(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C                   ADD       AM(X)         SCTOT
     C                   ENDDO
      *** BEG DEL ***** 04/20/11 *************************************
     C***********EASTAT    IFEQ 'B'
     C***********EASTAT    OREQ 'C'
     C***********'Adj '    CAT  'Amounts' SCHEAD
     C***********          ELSE
     C***********'Tax '    CAT  'Amounts' SCHEAD
     C***********          ENDIF
      *** END DEL ***** 04/20/11 *************************************
     ******BEG ADD 05/15/07*******************************************
      *** BEG CHG ***** 04/20/11 *************************************
     C***********          SELEC
     C***********EASTAT    WHEQ '1'
     C***********          MOVEL'1ST ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '2'
     C***********          MOVEL'2ND ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '3'
     C***********          MOVEL'3RD ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '4'
     C***********          MOVEL'4TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '5'
     C***********          MOVEL'5TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C****BEGIN*CHG ***  12/11/08  ********************************
     C***********EASTAT    WHEQ '6'
     C***********          MOVEL'1ST ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '7'
     C***********          MOVEL'2ND ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '8'
     C***********          MOVEL'3RD ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '9'
     C***********          MOVEL'4TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '0'
     C***********          MOVEL'5TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '6'
     C***********          MOVEL'6TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ '7'
     C***********          MOVEL'7TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'D'
     C***********          MOVEL'1ST ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'E'
     C***********          MOVEL'2ND ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'F'
     C***********          MOVEL'3RD ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'G'
     C***********          MOVEL'4TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'H'
     C***********          MOVEL'5TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'J'
     C***********          MOVEL'6TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
     C***********EASTAT    WHEQ 'K'
     C***********          MOVEL'7TH ASSE'SCHEAD    P
     C***********          CAT  'SSMENT':1SCHEAD
      *
     C                   SELECT
     C     EASTAT        WHENEQ    'B'
     C     EASTAT        OREQ      'C'
     C                   MOVEL(P)  PRHD          SCHEAD
     C     EASTAT        WHENEQ    'U'
     C     EASTAT        OREQ      'T'
     C                   MOVEL(P)  TXHD          SCHEAD
     C*** BEGIN ADD ***  08/19/13  **********************************
     C     EASTAT        WHENEQ    'W'
     C     EASTAT        OREQ      'X'
     C                   MOVEL(P)  'Deposit'     SCHEAD
     C     EASTAT        WHENEQ    'Y'
     C     EASTAT        OREQ      'Z'
     C                   MOVEL(P)  'Deposit Tax' SCHEAD
     C*** END ADD *****  08/19/13  **********************************
     C     EASTAT        WHENEQ    '1'
     C                   MOVEL(P)  A1HD          SCHEAD
     C     EASTAT        WHENEQ    '2'
     C                   MOVEL(P)  A2HD          SCHEAD
     C     EASTAT        WHENEQ    '3'
     C                   MOVEL(P)  A3HD          SCHEAD
     C     EASTAT        WHENEQ    '4'
     C                   MOVEL(P)  A4HD          SCHEAD
     C     EASTAT        WHENEQ    '5'
     C                   MOVEL(P)  A5HD          SCHEAD
     C     EASTAT        WHENEQ    '6'
     C                   MOVEL(P)  A6HD          SCHEAD
     C     EASTAT        WHENEQ    '7'
     C                   MOVEL(P)  A7HD          SCHEAD
     C     EASTAT        WHENEQ    'D'
     C                   MOVEL(P)  A1HD          SCHEAD
     C     EASTAT        WHENEQ    'E'
     C                   MOVEL(P)  A2HD          SCHEAD
     C     EASTAT        WHENEQ    'F'
     C                   MOVEL(P)  A3HD          SCHEAD
     C     EASTAT        WHENEQ    'G'
     C                   MOVEL(P)  A4HD          SCHEAD
     C     EASTAT        WHENEQ    'H'
     C                   MOVEL(P)  A5HD          SCHEAD
     C     EASTAT        WHENEQ    'J'
     C                   MOVEL(P)  A6HD          SCHEAD
     C     EASTAT        WHENEQ    'K'
     C                   MOVEL(P)  A7HD          SCHEAD
      *** END CHG ***** 04/20/11 *************************************
     C*** END CHG *****  12/11/08  ********************************
     C                   ENDSL
     ******END ADD 05/15/07*******************************************
     C*
     C                   Z-ADD     EAFND         SCFND
     C                   Z-ADD     EAFYR         SCYR
     C     FNKY          CHAIN     PFN01                              60
      *
     C                   MOVEL     FNNAME        SCNAME
      *
     C     EMKY          CHAIN     PEM01                              60
      *
      *  COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#            9 0
     C                   PARM      EMDIV         XXDIV             5 0
     C                   PARM      EMNAME        XXNAME           40
     C                   PARM                    XXEDN            80
      *
     C                   MOVEL     XXEDN         SCEDN
      *
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVEL     ELBSCD        SCBSCD
     C*** BEGIN ADD ***  04/21/03  ************************************
     C                   MOVE      *OFF          *IN62
     C                   MOVE      *OFF          *IN63
     C*** BEGIN ADD ***  04/25/03  ************************************
     C                   MOVE      *OFF          *IN64
     C     EAEDIT        IFEQ      'T'
     C                   MOVE      *ON           *IN64
     C                   ENDIF
     C*** END ADD *****  04/25/03  ************************************
     C     EATRDT        IFLT      55
     C                   MOVE      *ON           *IN62
     C                   Z-ADD     EATRDT        SCINST
     C     EATRDT        IFEQ      0
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     C     AM(2)         MULT      EATRDT        SCTOT
     C                   ADD       AM(1)         SCTOT
     C                   ENDIF                                                  EATRDT IFLT
     C*** END ADD *****  04/21/03  ************************************
     C*** BEGIN ADD ***  11/30/04  ************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXACDT            8 0
     C*
     C                   Z-ADD     ELCO#         @@CO#
     C                   Z-ADD     ELFND         @@FND
     C                   Z-ADD     ELEMP#        @@EMP#
     C                   Z-ADD     ELDIV         @@DIV
     C                   Z-ADD     ELFYR         @@FYR
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@AGT#            5 0
     C                   PARM                    @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
     C*
     C     @@PLAN        IFEQ      'A'
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     EXAOID        XXDAYS            5 0
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      XXDAYS        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          XXACDT            8 0
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C*** END ADD *****  11/30/04  ************************************
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C     1             DO        12            X                 2 0
     C                   Z-ADD     PA(X)         AM(X)
     C                   Z-ADD     DA(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PO(X)
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKF         CASEQ     *ON           KFDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WAEAP
     C                   ENDSR
