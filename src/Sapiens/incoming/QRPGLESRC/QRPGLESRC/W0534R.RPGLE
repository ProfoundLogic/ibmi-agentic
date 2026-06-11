     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* PROGRAM:  W0534R                                             **
     H* DESCRIPTION:  PROVIDER/MODIFIER SEARCH W/ NAME/ADDRESS       **
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                               *
     H* 06/28/05 C:62  Support: WC129/47568                           *
     H*                Name field not cleared when filled.            *
     H*                                                               *
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                              **
     H*  05/26/99 C:118  PROJECT:  WC577                             **
     H*                  MODIFIED 'ROLL UP' AND 'ROLL DOWN', PROGRAM **
     H*                  NAME, TO MEET V3 STDS.                      **
     H*                                                              **
     H*  9/15/89  LJD - ADDED PROVIDER TYPE, BECAUSE IT WAS A PARM   **
     H*  PROJECT: A9C    BUT WASN'T BEING LOADED WITH ANYTHING       **
     H*                  NOTE: IT IS NOT A SCREEN FIELD              **
     H* 12/05/90  LJD - ADDED SPECIALTY CODE TO DISPLAY ON SCREEN    **
     H*               - REDUCED SIZE OF NAME/ADDRESS FIELD FROM 60   **
     H* 04/16/91  DVM - ADDED CODE TO POSITION CURSOR AT THE PROVIDER**
     H*                 AND MODIFIER PASSED TO THE PROVIDER/MOD      **
     H*                 SEARCH (F0534R) WHICH IS CALLED WHEN MORE    **
     H*                 THAN 1 (ONE) MODIFIER EXIST FOR A PROVIDER.  **
     H* 01/06/93  C:20- ON ROLL UP AND DOWN CHGD TO USE CORRECT KEY  **
     H*                 LIST PDKEY1 INSTEAD OF PDKEY                 **
     H*                                                              **
     H* 02/12/93 C:15  PROJECT: 40MAA                                **
     H*                COPIED THIS PROGRAM FROM F0534R TO BE USED FOR**
     H*                WORK COMP APPLICATIONS.                       **
     H*****************************************************************
     H** INDICATORS:                                                 **
     H*****************************************************************
     FW0534F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMPDL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************

     D FL              S              1    DIM(14)                                POSITIONAL FLAG
     D RR              S              9    DIM(2) CTDATA PERRCD(1)
     D KEYF            S              9  0 DIM(2)
     D KEYM            S              3  0 DIM(2)
     D*
     D* ARRAYS USED FOR COMBINE SUBROUTINE
     D*
     D*
     D*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     D* ARRAY USED FOR COMBINEING PROVIDER AND MODIFIER FOR LOOK UP
     D*  OPERATION.
     D PRM             S             12  0 DIM(14)
     D***** END ADD *** 04/16/91 ************* DVM *******************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  FE                     1    126  0
     D                                     DIM(14)                                PROVIDER FEIN
     D  SCFE01                 1      9  0
     D  SCFE02                10     18  0
     D  SCFE03                19     27  0
     D  SCFE04                28     36  0
     D  SCFE05                37     45  0
     D  SCFE06                46     54  0
     D  SCFE07                55     63  0
     D  SCFE08                64     72  0
     D  SCFE09                73     81  0
     D  SCFE10                82     90  0
     D  SCFE11                91     99  0
     D  SCFE12               100    108  0
     D  SCFE13               109    117  0
     D  SCFE14               118    126  0
     D                 DS
     D  MO                     1     42  0
     D                                     DIM(14)                                PROVIDER MODIFIER
     D  SCMO01                 1      3  0
     D  SCMO02                 4      6  0
     D  SCMO03                 7      9  0
     D  SCMO04                10     12  0
     D  SCMO05                13     15  0
     D  SCMO06                16     18  0
     D  SCMO07                19     21  0
     D  SCMO08                22     24  0
     D  SCMO09                25     27  0
     D  SCMO10                28     30  0
     D  SCMO11                31     33  0
     D  SCMO12                34     36  0
     D  SCMO13                37     39  0
     D  SCMO14                40     42  0
     D                 DS
     D  DE                     1    770
     D                                     DIM(14)                                NAME/ADDRESS
     D  SCDE01                 1     55
     D  SCDE02                56    110
     D  SCDE03               111    165
     D  SCDE04               166    220
     D  SCDE05               221    275
     D  SCDE06               276    330
     D  SCDE07               331    385
     D  SCDE08               386    440
     D  SCDE09               441    495
     D  SCDE10               496    550
     D  SCDE11               551    605
     D  SCDE12               606    660
     D  SCDE13               661    715
     D  SCDE14               716    770
     D*** BEGIN ADD *** 12/05/90 *************************************
     D                 DS
     D  SP                     1     42
     D                                     DIM(14)                                SPECIALTY CODE
     D  SCSP01                 1      3
     D  SCSP02                 4      6
     D  SCSP03                 7      9
     D  SCSP04                10     12
     D  SCSP05                13     15
     D  SCSP06                16     18
     D  SCSP07                19     21
     D  SCSP08                22     24
     D  SCSP09                25     27
     D  SCSP10                28     30
     D  SCSP11                31     33
     D  SCSP12                34     36
     D  SCSP13                37     39
     D  SCSP14                40     42
     D*** END ADD ***** 12/05/90 *************************************
     D*** BEGIN ADD *** 09/15/89 *************************************
     D                 DS
     D  PT                     1     14
     D                                     DIM(14)                                PROVIDER TYPE
     D  SCPT01                 1      1
     D  SCPT02                 2      2
     D  SCPT03                 3      3
     D  SCPT04                 4      4
     D  SCPT05                 5      5
     D  SCPT06                 6      6
     D  SCPT07                 7      7
     D  SCPT08                 8      8
     D  SCPT09                 9      9
     D  SCPT10                10     10
     D  SCPT11                11     11
     D  SCPT12                12     12
     D  SCPT13                13     13
     D  SCPT14                14     14
     D*** END ADD ***** 09/15/89 *************************************
     D*
     D* DATA STRUCTURES FOR COMBINE SUBROUTINE
     D*
     D                 DS
     D  SNAME                  1     65
     D  SN                     1     65
     D                                     DIM(65)
     D                 DS
     D  SADDR                  1     40
     D  SA                     1     40
     D                                     DIM(40)
     D                 DS
     D  SCITY                  1     15
     D  SC                     1     15
     D                                     DIM(15)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D***** BEGIN ADD ***** 05/26/99 **********************************
     D OPTNM           C                   CONST('Modifier Search -
     D                                     Within Provider')

     I***** END ADD ******* 05/26/99 **********************************

     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN            9 0
     C*
     C     PDKEY1        KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQFEIN            9 0
     C                   PARM                    QQMOD             3 0
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQRTCD            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C                   TIME                    DSDTTM
     C***** BEGIN DEL **** 05/26/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C**********           MOVE QQPRDS    SCPRDS
     C***** END DEL ****** 05/26/99 ***********************************
     C                   MOVEL(P)  OPTNM         SCPRDS
     C*
     C                   Z-ADD     0             XXMOD             3 0
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   MOVEL     QQFEIN        XXPRM            12 0
     C                   MOVE      QQMOD         XXPRM
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   LOAD THE INITIAL SCREEN FROM SELECTED POSITION
     C*
     C                   Z-ADD     QQFEIN        XXFEIN
     C                   Z-ADD     *ZERO         SCMOD
     C                   GOTO      REPOS
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     XXUP          IFEQ      '1'
     C                   MOVE      *BLANKS       SCUP
     C                   ELSE
     C                   MOVEL     RR(1)         SCUP
     C                   END
     C     XXDOWN        IFEQ      '1'
     C                   MOVE      *BLANKS       SCDOWN
     C                   ELSE
     C***** BEGIN CHG **** 05/26/99 ***********************************
     C**********           MOVELRR,2      SCDOWN
     C                   MOVEL     RR(2)         SCUP
     C***** END CHG ****** 05/26/99 ***********************************
     C                   END
     C                   EXFMT     SCR01
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      'A'           QQRTCD
     C                   Z-ADD     *ZERO         QQFEIN
     C                   Z-ADD     *ZERO         QQMOD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR01T
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR01T
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     KEYF(2)       XXFEIN
     C                   Z-ADD     KEYM(2)       XXMOD
     C********** PDKEY     CHAINPPD01                60    DEL 01/06/93
     C     PDKEY1        CHAIN     PPD01                              60        ADD 01/06/93
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     KEYF(1)       XXFEIN
     C                   Z-ADD     KEYM(1)       XXMOD
     C********** PDKEY     CHAINPPD01                60    DEL 01/06/93
     C     PDKEY1        CHAIN     PPD01                              60        ADD 01/06/93
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C*  REPOSITION FILE AT REQUESTED RECORD
     C*
     C     SCMOD         IFNE      *ZERO
     C     REPOS         TAG
     C                   MOVE      *BLANK        KEYF
     C                   MOVE      *BLANK        KEYM
     C                   MOVE      *BLANK        XXUP
     C                   MOVE      *BLANK        XXDOWN
     C                   Z-ADD     SCMOD         XXMOD
     C                   MOVE      *BLANK        SCMOD
     C     PDKEY1        SETLL     PPD01
     C     PDKEY         READE     PPD01                                  60
     C                   EXSR      LOAD
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   Z-ADD     0             X                 3 0
     C                   Z-ADD     1             Z                 3 0
     C     XXPRM         LOOKUP    PRM(Z)                                 98
     C     *IN98         IFEQ      '1'
     C     Z             ADD       29            X
     C                   MOVE      '1'           *IN(X)
     C                   END
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK CURSOR POSTION
     C*
     C     Q2CPOS        DIV       256           LIN               2 0
     C     LIN           SUB       7             I1                3 0
     C*
     C*  VALID CURSOR POSITION - RETURN WITH SELECTION
     C*
     C     I1            IFGE      1
     C     I1            ANDLE     14
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     FE(I1)        QQFEIN
     C                   Z-ADD     MO(I1)        QQMOD
     C                   MOVE      PT(I1)        QQTYPE
     C                   GOTO      TERM
     C                   END
     C*
     C*  INVALID CURSOR POSITION - RETURN WITH NO SELECTION
     C*
     C                   MOVE      '*'           QQRTCD
     C                   Z-ADD     *ZERO         QQFEIN
     C                   Z-ADD     *ZERO         QQMOD
     C                   GOTO      TERM
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*  CLEAR SCREEN ARRAYS
     C*
     C     CLR           BEGSR
     C                   MOVE      *BLANKS       FE
     C                   MOVE      *BLANKS       MO
     C                   MOVE      *BLANKS       DE
     C                   MOVE      *BLANKS       SP                             12/05/90 ADD
     C                   MOVE      *BLANKS       PT                             9/15/89 ADD
     C                   Z-ADD     *ZERO         I1
     C*
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   MOVEA     '0000000'     *IN(30)
     C                   MOVEA     '0000000'     *IN(37)
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C                   ENDSR
     C*
     C*  FILL SCREEN ARRAYS
     C*
     C     FILL          BEGSR
     C                   MOVE      PDFEIN        FE(I1)
     C                   MOVE      PDMOD         MO(I1)
     C     PDFSN         IFEQ      *BLANKS
     c*** begin chg *** 06/28/05 **************************************
     C**********           MOVELPDNAME    XXLAST 56
     C                   MOVEL(P)  PDNAME        XXLAST           56
     c*** end   chg *** 06/28/05 **************************************
     C                   ELSE
     c*** begin chg *** 06/28/05 **************************************
     C**********           MOVELPDNAME    XXLAST 56
     C                   MOVEL(P)  PDNAME        XXLAST           56
     c*** end   chg *** 06/28/05 **************************************
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVE      *BLANKS       XXNAME
     c*** begin chg *** 06/28/05 **************************************
     C**********           MOVELXXLAST    XXNAME 65
     C                   MOVEL(P)  XXLAST        XXNAME           65
     c*** end   chg *** 06/28/05 **************************************
     C                   MOVEL     PDADR1        XXADDR           40
     C                   MOVEL     PDCTY         XXCITY           15
     C                   MOVE      '1'           XXTERM            1
     C                   EXSR      COMB
     c*** begin chg *** 06/28/05 **************************************
     C**********           MOVELXXNAME    DE,I1
     C                   MOVEL(P)  XXNAME        DE(I1)
     c*** end   chg *** 06/28/05 **************************************
     C                   MOVE      PDSPCD        SP(I1)                         12/05/90 ADD
     C                   MOVE      PDTYPE        PT(I1)                         9/15/89 ADD
     C*
     C*** BEGIN ADD *** 04/16/91 ************* DVM *******************
     C                   MOVEL     PDFEIN        PROMOD           12 0
     C                   MOVE      PDMOD         PROMOD
     C                   MOVE      PROMOD        PRM(I1)
     C***** END ADD *** 04/16/91 ************* DVM *******************
     C*
     C                   ENDSR
     C*
     C*  LOAD FORWARD SUBROUTINE
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     PDKEY         READE     PPD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     FE(1)         KEYF(1)
     C                   Z-ADD     FE(14)        KEYF(2)
     C                   Z-ADD     MO(1)         KEYM(1)
     C                   Z-ADD     MO(14)        KEYM(2)
     C                   ENDSR
     C*
     C*  LOAD BACKWARD SUBROUTINE
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     15            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PPD01                                  60
     C     PDFEIN        IFNE      XXFEIN
     C                   MOVE      '1'           *IN60
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     FE(1)         KEYF(1)
     C                   Z-ADD     FE(14)        KEYF(2)
     C                   Z-ADD     MO(1)         KEYM(1)
     C                   Z-ADD     MO(14)        KEYM(2)
     C                   ENDSR
     C*
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     QQFEIN        XXFEIN
     C     PDKEY         SETLL     PPD01
     C     PDKEY         READE     PPD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     PDKEY         READE     PPD01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      14                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C*
     C*  THIS SUB-PROGRAM ACCEPTS A NAME, ADDRESS AND CITY AND
     C*  COMBINES THEM LEFTMOST INTO A 65 LENGTH NAME
     C*
     C     COMB          BEGSR
     C*
     C                   MOVE      XXNAME        SNAME
     C                   MOVE      XXADDR        SADDR
     C                   MOVE      XXCITY        SCITY
     C*
     C                   MOVE      '0'           *IN61
     C                   Z-ADD     65            S1                5 0
     C     *IN61         DOWEQ     '0'
     C     S1            ANDGT     *ZERO
     C     *BLANK        COMP      SN(S1)                             6161
     C     *IN61         IFEQ      '0'
     C                   SUB       1             S1
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN62
     C                   Z-ADD     40            S2                5 0
     C     *IN62         DOWEQ     '0'
     C     S2            ANDGT     *ZERO
     C     *BLANK        COMP      SA(S2)                             6262
     C     *IN62         IFEQ      '0'
     C                   SUB       1             S2
     C                   END
     C                   END
     C*
     C                   MOVE      '0'           *IN63
     C                   Z-ADD     15            S3                5 0
     C     *IN63         DOWEQ     '0'
     C     S3            ANDGT     *ZERO
     C     *BLANK        COMP      SC(S3)                             6363
     C     *IN63         IFEQ      '0'
     C                   SUB       1             S3
     C                   END
     C                   END
     C*
     C*
     C                   Z-ADD     1             SE                5 0
     C                   Z-ADD     1             SF                5 0
     C*
     C     S1            ADD       1             SX                5 0
     C*
     C     S1            IFGT      *ZERO
     C     S2            ANDGT     *ZERO
     C     S1            ORGT      *ZERO
     C     S3            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SE            ANDLE     S2
     C                   MOVE      SA(SE)        SN(SX)
     C                   ADD       1             SE
     C                   ADD       1             SX
     C                   END
     C*
     C     S2            IFGT      *ZERO
     C     S3            ANDGT     *ZERO
     C     SX            CABGT     65            DONE3
     C                   MOVE      ','           SN(SX)
     C                   ADD       2             SX
     C     SX            CABGT     65            DONE3
     C                   END
     C*
     C     SX            DOWLE     65
     C     SF            ANDLE     S3
     C                   MOVE      SC(SF)        SN(SX)
     C                   ADD       1             SF
     C                   ADD       1             SX
     C                   END
     C*
     C     DONE3         TAG
     C*
     C                   MOVE      SNAME         XXNAME
     C*
     C                   ENDSR
**
  More...
   Bottom
