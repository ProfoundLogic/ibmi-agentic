     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W2155R - WORK WITH BILLING SCHEDULE CODES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL BILLING*
     H*                SCHEDULE CODES.  EACH CODE CAN BE ACCESSED FOR *
     H*                UPDATE, DISPLAY OR SELECTION DEPENDING UPON THE*
     H*                CALLING PROGRAM.                               *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    W2155R02 - BILLING SCHEDULE CODE INFORMATION (OPT 2/5)     *
     H*                                                               *
     H*  CREATED: 05/13/96  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 12/30/09 C:232 PROJECT: WC1838                                *
     H*                ADDED PLAN TYPE IN THE "WORK WITH BILLING      *
     H*                SCHEDULE CODES" SCREEN. ALSO ADDED PLAN TYPE   *
     H*                IN SUBSET SCREEN.                              *
     H*                                                               *
     H* 08/13/07 C:136 SUPPORT: WA132 59462                           *
     H*                CORRECT BILLING CODE SEARCH. IT CALCULATES     *
     H*                THE WRONG START DAY IF THE BEGINNING DATE      *
     H*                IS THE FIRST DAY OF THE MONTH                  *
     H*                                                               *
     H* 10/19/06 C:136 SUPPORT: WA071 55436                           *
     H*                CORRECT SEARCH FUNCTION                        *
     H*                                                               *
     H* 03/24/06 C:168 PROJECT: 129WNU (CLIENT SPECIFIC)              *
     H*          C:136 Added code to also list billing schedules which*
     H*                start in the 15th, W0051R02 will now calculate *
     H*                and pass the number of installments for the 15th
     H*                based on the policy effective dates            *
     H*                                                               *
     H* 03/20/06 C:136 SUPPORT: WA099 39357                           *
     H*                FIX BSCD SEARCH SO THAT ONLY 'ELIGIBLE'        *
     H*                BSCD'S WILL BE DISPLAYED.                      *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  21 - Number Order Search                                     *
     H*  30 - Called as a search                                      *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  93 - Used to display CPF6A7C; Option invalid                 *
     H*  94 - Used to display CPF5204; Function Key invalid           *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW2155F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PFRCDF)
     FWTABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBS01:DBRCDF)
      *** BEG ADD ***** 03/20/06 *************************************
     FWMBXL1    IF   E           K DISK
     FWMBXL     IF   E           K DISK
     F                                     RENAME(PBX01:PBX00)
      *** BEG ADD ***** 12/30/09 *************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      *** END ADD ***** 12/30/09 *************************************
      *** END ADD ***** 03/20/06 *************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
      *** BEG ADD ***** 03/20/06 *************************************
      *** END ADD ***** 03/20/06 *************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Billing Schedule -
     D                                     Codes')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W2155R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(2)
     D INFOMX          C                   CONST(1)
     D**** BEG ADD ***** 12/30/09 *************************************
     D OPT#1           C                   CONST('Large Deductible')
     D OPT#2           C                   CONST('Paid Loss Retro')
     D OPT#3           C                   CONST('Incurred Loss Retro')
     D**** END ADD ***** 12/30/09 *************************************
     D*** BEG ADD ***** 03/20/06 *************************************
     D                 DS
     D  XXMMDD                 1      4  0
     D  XXMM                   1      2  0
     D  XXDD                   3      4  0
     D                 DS
     D  BS                     1    108  8
     D                                     DIM(12)
     D  BSAB01                 1      9  8
     D  BSAB02                10     18  8
     D  BSAB03                19     27  8
     D  BSAB04                28     36  8
     D  BSAB05                37     45  8
     D  BSAB06                46     54  8
     D  BSAB07                55     63  8
     D  BSAB08                64     72  8
     D  BSAB09                73     81  8
     D  BSAB10                82     90  8
     D  BSAB11                91     99  8
     D  BSAB12               100    108  8
     D*** END ADD ***** 03/20/06 *************************************
     D*** BEG ADD ***** 03/24/06 *************************************
      *
      * 15M - holds the number (in months) a billing on the 15th will
      * occur based on the effective dates for policy.
      * 15D - holds the start day for the billing (1 - 30)
      *
     D                 DS
     D  XX15DD                 1      4  0
     D  XX15M                  1      2  0
     D  XX15D                  3      4  0

     I*** END ADD ***** 03/24/06 *************************************

     C*
     C     *LIKE         DEFINE    BSKEY         KKKEY
      *** BEG ADD ***** 03/20/06 *************************************
     C     *LIKE         DEFINE    BSKEY         KKBSCD
     C     *LIKE         DEFINE    BXEFDT        SVEFDT
     C     *LIKE         DEFINE    BXBSCD        SVBSCD
      *** END ADD ***** 03/20/06 *************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     C*
      *** BEG ADD ***** 03/20/06 *************************************
     C     BXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    BSKEY
      *
     C     BXKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    BXTYPE
      *** END ADD ***** 03/20/06 *************************************
     C*
      *** BEG ADD ***** 03/24/06 *************************************
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKBSCD
      *** END ADD ***** 03/24/06 *************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C**** BEG ADD ***** 12/30/09 *************************************
     C  N90SSSPGM        IFNE      ' '
     C     SSSPGM        ANDNE     'L'
     C     SSSPGM        ANDNE     'P'
     C     SSSPGM        ANDNE     'I'
     C                   SETON                                        3690
     C                   ENDIF
     C**** END ADD ***** 12/30/09 *************************************
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C     'WC'          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL43        IFEQ      'Y'
     C     WCFL11        OREQ      'Y'
     C                   SETON                                        35
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKKEY
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL     SCPST1        KKKEY
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCKEY         KKKEY
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     BSKEY         SCKEY
     C*
     C                   MOVEL     BSSTAT        SCSTAT
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C                   MOVEL     *BLANKS       SCLPGM
     C*
     C     BSLPGM        IFEQ      'L'
     C                   MOVEL     OPT#1         SCLPGM
     C                   ENDIF
     C     BSLPGM        IFEQ      'P'
     C                   MOVEL     OPT#2         SCLPGM
     C                   ENDIF
     C     BSLPGM        IFEQ      'I'
     C                   MOVEL     OPT#3         SCLPGM
     C                   ENDIF
     C     BSLPGM        IFEQ      ' '
     C                   MOVEL     *BLANKS       SCLPGM
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C                   SELECT
B01  C     VIEW#         WHENEQ    1
     C                   MOVEL     BSKEY         XXKEY             3
     C                   MOVEL(P)  XXKEY         SCDATA
     C                   MOVE      BSDESC        SCDATA
E01  C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
      *** BEG ADD ***** 03/24/06 *************************************
     C                   Z-ADD     *ZEROS        XX15DD
     C                   Z-ADD     *ZEROS        XXMMDD
     C                   Z-ADD     *ZEROS        XXINCT
      *** BEG ADD ***** 03/24/06 *************************************
      *** BEG ADD ***** 03/20/06 *************************************
     C*
     C* GET NUMBER OF MONTHLY INSTALLMENTS AND EFDT FOR WMBXP
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     @@RRN         KKFND             3 0
     C                   MOVE      @@RRN         XXMMDD            4 0
     C                   Z-ADD     XXMM          XXINCT            2 0
      *** BEG CHG ***** 10/19/06 *************************************
     C*********  12        SUB  XXMM      XXMM
     C     13            SUB       XXMM          XXMM
      *** END CHG ***** 10/19/06 *************************************
      *** BEG DEL ***** 08/13/07 *************************************
     C*********  XXDD      IFEQ 1
     C*********            ADD  1         XXMM
     C*********            ENDIF
      *** END DEL ***** 08/13/07 *************************************
     C                   ENDIF
      *** END ADD ***** 03/20/06 *************************************
      **** BEGIN  ADD 03/24/06 ****************************************
      * position 4-5 holds the number of months for the 15th
      *
     C                   MOVE      @@RRN         XXTEMP            6 0
     C                   MOVEL     XXTEMP        XX15M
     C                   Z-ADD     XX15M         XX15CT            2 0
     C     12            SUB       XX15M         XX15M
     C     XX15M         IFEQ      *ZEROS
     C                   Z-ADD     1             XX15M
     C                   ENDIF
     C                   Z-ADD     XXDD          XX15D
      **** END    ADD 03/24/06 ****************************************
     C*
      **** BEGIN  ADD 03/24/06 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
      *** END ADD ***** 03/24/06 *************************************
      *
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C                   MOVEL     *BLANK        SCSTAT
     C**** BEG ADD ***** 12/30/09 *************************************
     C                   MOVEL     *BLANKS       SCLPGM
     C**** END ADD ***** 12/30/09 *************************************
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     BSSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C**** BEG ADD ***** 12/30/09 *************************************
     C     SSSPGM        IFNE      *BLANKS
     C     SSSPGM        ANDNE     BSLPGM
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C**** END ADD ***** 12/30/09 *************************************
      *** BEG ADD ***** 03/20/06 ************************************
     C     @@CALL        IFEQ      '2'
     C                   Z-ADD     12            X                 2 0
     C                   DO        12
     C     BS(X)         IFNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C                   Z-ADD     X             XXHIBS            2 0
     C                   CLEAR                   BXTYPE
     C                   CLEAR                   BXEFDT
      *
      *****  BEGIN  ADD 03/24/06 **************************************
      * Only execute if we passed a fund.
      *
     C     KKFND         IFNE      *ZEROS
      *****  END    ADD 03/24/06 **************************************
      *
     C     BXKY          CHAIN     PBX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BXBSCD        SVBSCD
     C                   Z-ADD     BXEFDT        SVEFDT
     C                   SELECT
     C     BXTYPE        WHENEQ    'A'
     C     BXTYPE        OREQ      'W'
     C     BXTYPE        OREQ      'T'
      *
      **** BEGIN  ADD  03/24/06 ***************************************
     C     XXCLT#        WHENEQ    '129'
     C     BSON15        ANDEQ     'Y'
     C     BXEFDT        ANDGT     XX15DD
     C                   MOVE      *OFF          MEETSS
      *
     C     XXCLT#        WHENEQ    '129'
     C     BSON15        ANDEQ     'Y'
     C     BXEFDT        ANDLT     XX15DD
     C     BXEFDT        ANDNE     *ZERO
     C     XXHIBS        ANDLE     XX15CT
     C     BXKY1         SETLL     PBX00
     C     BXKY1         READE     PBX00                                  60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  BXBSCD        KKBSCD
     C     BSKY          CHAIN     DBRCDF                             60
     C     BSON15        IFEQ      'Y'
     C     BXBSCD        IFNE      SVBSCD
     C     BXEFDT        ANDGT     SVEFDT
     C     BXEFDT        ANDLE     XX15DD
     C                   MOVE      *OFF          MEETSS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     BXKY1         READE     PBX00                                  60
     C                   ENDDO                                                  *IN60
     C                   MOVEL(P)  SVBSCD        KKBSCD
     C     BSKY          CHAIN     DBRCDF                             60
      *** END ADD **** 03/24/06 ***************************************
      *
     C     BXEFDT        WHENGT    XXMMDD
     C                   MOVE      *OFF          MEETSS
     C     BXEFDT        WHENLT    XXMMDD
     C     BXEFDT        ANDNE     *ZERO
     C     XXHIBS        ANDLE     XXINCT
     C     BXKY1         SETLL     PBX00
     C     BXKY1         READE     PBX00                                  60
     C     *IN60         DOWEQ     *OFF
      *** BEG ADD ***** 03/24/06 **************************************
     C                   MOVEL(P)  BXBSCD        KKBSCD
     C     BSKY          CHAIN     DBRCDF                             60
     C     BSON15        IFNE      'Y'
      *** END ADD ***** 03/24/06 **************************************
     C     BXBSCD        IFNE      SVBSCD
     C     BXEFDT        ANDGT     SVEFDT
     C     BXEFDT        ANDLE     XXMMDD
     C                   MOVE      *OFF          MEETSS
     C                   LEAVE
     C                   ENDIF
      *** BEG ADD ***** 03/24/06 **************************************
     C                   ENDIF
      *** END ADD ***** 03/24/06 **************************************
     C     BXKY1         READE     PBX00                                  60
     C                   ENDDO                                                  *IN60
     C                   MOVEL     SVBSCD        KKBSCD
     C     BSKY          CHAIN     DBRCDF                             60
     C                   ENDSL
     C                   ENDIF
      *
     C                   SELECT
     C     XXHIBS        WHENGT    XXINCT
      *** BEG ADD ***** 03/24/06 **************************************
C    C     BSON15        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C     XXHIBS        WHENGT    XX15CT
C    C     BSON15        ANDEQ     'Y'
      *** END ADD ***** 03/24/06 **************************************
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
      *
      *** BEGIN ADD  03/24/06 ***************************************
     C                   ENDIF
      *** END   ADD  03/24/06 ***************************************
      *
     C                   ENDIF                                                  @@CALL
      *** END ADD ***** 03/20/06 ************************************
     C*
     C                   ENDSR                                                  SUBSET
** Headers - HD
Code Description                      Plan type                Sts
Description                     Code  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1077=Auxiliary              NNN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
