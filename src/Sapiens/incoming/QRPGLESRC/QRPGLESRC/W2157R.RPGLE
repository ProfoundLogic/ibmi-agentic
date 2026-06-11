     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W2157R - WORK WITH BILLING SCHEDULE CODE AUXILIARY FILE
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL AUX    *
     H*                CODES BY NUMBER.  EACH CODE CAN BE ACCESSED    *
     H*                FOR UPDATE, DISPLAY OR SELECTION DEPENDING UPON*
     H*                THE CALLING PROGRAM.                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH GROUPS                                  *
     H*  CALLS TO:                                                    *
     H*    W2157R02 - BILLING SCHEDULE CODE AUXILIARY INFO (OPT 2/5)  *
     H*                                                               *
     H*  CREATED: 05/13/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*  REVISIONS:                                                   *
     H* 04/10/23  C:300  PROJECT: WC2196  Support:107629              *
     H*                  Make it pass-through for Billing Auxiliary   *
     H*                  search screen which called from Policy Detail*
     F*                                                               *
     F* 01/24/14  C:279  Redmine: SY266/3853                          *
     F*                  Changed to make LR indicator on before end   *
     F*                  of program.                                  *
     H*                                                               *
     F* 02/08/13  C:190  PROJECT: 140WAU                              *
     F*                  ADDED DEPOSIT % IN THE BILLING SCHEDULE      *
     F*                  CODES AUXILIARY SCREEN.                      *
     H*                                                               *
     H*  12/30/08 C:232 PROJECT: WC1838                               *
     H*                 ADDED PLAN TYPE IN THE "BILLING SCHEDULE      *
     H*                 CODES AUXILIARY" SCREEN.                      *
     H*                                                               *
     H*  03/28/06 C:168 PROJECT: 129WNU                               *
     H*           C:136 Recondition the record selection to allow the *
     H*                 select of billing schedules for the 1st and   *
     H*                 the 15th                                      *
     H*                                                               *
     H*  05/28/03 C:132 PROJECT: WC577                                *
     H*                 ADD 'DELETE' OPTION.                          *
     H*                                                               *
     H*  04/11/03 C:136 PROJECT: WC1116                               *
     H*                 ALLOW 'T - TWICE A MONTH' AS A VALID BILLING  *
     H*                 AUX TYPE IF ACH PROCESSING IS ENABLED.        *
     H*                                                               *
     H*  02/18/03 C:136 PROJECT: WC1115                               *
     H*                 ALLOW 'W - WEEKLY' AS A VALID BILLING AUX TYPE*
     H*                 IS VENDOR PROCESSING IS ENABLED.              *
     H*                                                               *
     H*  03/05/98  C:33  PROJECT: WC577                               *
     H*                  BE SURE THAT CORRECT DATE IS BEING USED.     *
     H*                                                               *
     H*  01/28/98  C:33  PROJECT: WC577                               *
     H*                  CORRECT PAGING.                              *
     H*                                                               *
     H*  12/02/97  C:33  PROJECT: WC577                               *
     H*                  ONLY DISPLAY SELECTED RECORD THAT CORRESPOND *
     H*                  TO THE PASSED IN POLICY EFFECTIVE DATE.      *
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
     FW2157F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMBXP     IF   E             DISK
     F                                     RENAME(PBX01:PFRCDF)
     FWMBXL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBX01:DBRCDF)
     FWMBXL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PBX01:DBRCDF2)
     FWMFNP     IF   E             DISK
     FWTABL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D                 DS
     D  XXPFDT                 1      8  0
     D  XXMMDD                 1      4  0
     D  XXINCT                 5      6  0
     D  XXIN15                 7      8  0
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
     D                 DS
     D  XX15DD                 1      4  0
     D  XX15M                  1      2  0
     D  XX15D                  3      4  0
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Billing Schedule -
     D                                     Codes Auxiliary')
     D TYPNM           C                   CONST('Select Installment -
     D                                     Plan')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W2157R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(1)
     D TYPE1           C                   CONST('W - Weekly')
     D TYPE2           C                   CONST('T - Twice a month')
     D OPT#1           C                   CONST('Large Deductible')
     D OPT#2           C                   CONST('Paid Loss Retro')
     D OPT#3           C                   CONST('Incurred Loss Retro')

     C*
     C     *LIKE         DEFINE    BSKEY         KKKEY
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    SCDBRR        XXRRN
     C     *LIKE         DEFINE    BXBSCD        KKBSCD
     C     *LIKE         DEFINE    SSBSTY        XXBSTY
     C     *LIKE         DEFINE    BXEFDT        XXEFDT
     C     *LIKE         DEFINE    XXMMDD        TTMMDD
     C     *LIKE         DEFINE    BXTYPE        KKTYPE
     C     *LIKE         DEFINE    BXEFDT        KKEFDT
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
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
     C                   KFLD                    SCFND
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXBSTY
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXBSTY
     C                   KFLD                    XXMMDD
     C                   KFLD                    KKKEY
     C*
     C     KEY2A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXBSTY
     C                   KFLD                    XXMMDD
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    KKKEY
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKEFDT
     C*
     C*  BILLING SCHEDULE CODE TABLE FILE
     C*
     C     BSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXKEY
      *
     C     BXKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXBSTY
     C*
     C     KEY3A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXBSTY
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKBSCD
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@BSTY            1
     C                   PARM                    @@PFDT            8
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSBSTY        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
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
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C     ERRCK2        BEGSR
     C                   SELECT
     C     *IN41         WHENEQ    *OFF
     C     *IN42         ANDEQ     *OFF
     C     SSBSTY        ANDNE     'M'
     C     SSBSTY        ANDNE     'Q'
     C     SSBSTY        ANDNE     'A'
     C     SSBSTY        ANDNE     'O'
     C                   SETON                                        71  90
     C     *IN41         WHENEQ    *ON
     C     *IN42         ANDEQ     *OFF
     C     SSBSTY        ANDNE     'M'
     C     SSBSTY        ANDNE     'Q'
     C     SSBSTY        ANDNE     'A'
     C     SSBSTY        ANDNE     'O'
     C     SSBSTY        ANDNE     'W'
     C                   SETON                                        72  90
     C     *IN42         WHENEQ    *ON
     C     SSBSTY        ANDNE     'M'
     C     SSBSTY        ANDNE     'Q'
     C     SSBSTY        ANDNE     'A'
     C     SSBSTY        ANDNE     'O'
     C     SSBSTY        ANDNE     'W'
     C     SSBSTY        ANDNE     'T'
     C                   SETON                                        73  90
     C                   ENDSL
     C                   ENDSR                                                  ERRCK2
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     XXBSTY        SSBSTY
     C                   ENDIF
     C*
     C                   ENDSR                                                  INZSST
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
     C                   MOVEL     *LOVAL        KKTYPE
     C                   Z-ADD     *LOVAL        KKEFDT
     C                   ENDSR                                                  KEYINZ
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKKEY
     C   22              MOVEL     SCPST2        KKKEY
     C                   MOVEL     *LOVAL        KKTYPE
     C                   Z-ADD     *LOVAL        KKEFDT
     C                   ENDSR                                                  KEYSET
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCKEY         KKKEY
     C                   MOVEL     SCTYPE        KKTYPE
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   ENDSR                                                  KEYSFL
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     BXBSCD        SCKEY
     C                   MOVEL     BXBSCD        SCBSCD
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVEL     BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C  N60              MOVEL     BSDESC        SCDESC
     C*
     C                   Z-ADD     BXEFDT        SCEFDT
     C                   MOVEL     BXTYPE        SCTYPE
     C                   MOVEL     *BLANKS       SCPLAN
      *
     C     BSLPGM        IFEQ      'L'
     C                   MOVEL     OPT#1         SCPLAN
     C                   ENDIF
     C     BSLPGM        IFEQ      'P'
     C                   MOVEL     OPT#2         SCPLAN
     C                   ENDIF
     C     BSLPGM        IFEQ      'I'
     C                   MOVEL     OPT#3         SCPLAN
     C                   ENDIF
     C     BSLPGM        IFEQ      ' '
     C                   MOVEL     *BLANKS       SCPLAN
     C                   ENDIF
      ********Beg Add************** 02/08/13 *************************
      /free
       //* For Estimating Plan
       If BSsefl = 'E';
          Select;
          When BSesdp <> 0;
               scDeposit = BSesdp;
          Other;
               scDeposit = BSesd2;
          Endsl;
       Endif;

       //* For Self Reporting Plan
       If BSsefl = 'S';
          Select;
          When BSab01 <> 0;
               scDeposit = BSab01 * 100;
          Other;
               scDeposit = BSab02 * 100;
          Endsl;
       Endif;
      /end-free
      ********End Add************** 02/08/13 *************************
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
      /free
       //****** beg add ****** 01/24/14 ******************************
       *inlr = *on;
       //****** end add ****** 01/24/14 ******************************
      /end-free
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
      *
      **** begin add **** 04/10/23 ******************************************
      * Pass-through this program, when called for a Search screen
     C                   IF        @@CALL = '2'
     C                   MOVE      @@RRN2        @@RRNCH
     C                   Z-ADD     *ZEROS        XXPFDT
     C                   MOVE      @@PFDT        XXPFDT
      *
     C                   CALL      'WTGRPPOLCG'
     C                   PARM                    @@RRNCH           9
     C                   PARM      'D'           @@MODE            1
     C                   PARM      'BILAUX'      @@NEXT           10
     C                   PARM      @@BSTY        @@TABIND          5
     C                   PARM      XXMMDD        @@MMDD            4 0
     C                   PARM      *zeros        @@BARRN           9 0
      *
     C                   MOVE      @@BARRN       @@RRN
     C                   RETURN
     C                   ENDIF                                                  PGMSTR
      **** end add **** 04/10/23 ********************************************
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
      *
      *
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL23        IFEQ      'Y'
     C                   SETON                                        41
     C                   MOVEL     TYPE1         SSTPD1
     C                   MOVEL     TYPE1         SCTPD1
     C                   ELSE
     C                   SETOFF                                       41
     C                   ENDIF                                                  WCFL23 IFEQ
     C*
     C     WCFL24        IFEQ      'Y'
     C                   SETON                                        42
     C                   MOVEL     TYPE1         SSTPD1
     C                   MOVEL     TYPE1         SCTPD1
     C                   MOVEL     TYPE2         SSTPD2
     C                   MOVEL     TYPE2         SCTPD2
     C                   ELSE
     C                   SETOFF                                       42
     C                   ENDIF                                                  WCFL24 IFEQ
     C*
     C*  GET GROUP INFORMATION
     C*
     C     @@RRN2        CHAIN     PFN01                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C     @@PFDT        IFEQ      *BLANK
     C*
     C                   MOVE      *ON           *IN22
     C                   MOVE      *OFF          *IN21
     C                   Z-ADD     2             VIEW#
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN21
     C                   MOVEL     @@PFDT        KKFND             3 0
     C                   Z-ADD     *ZEROS        XXPFDT
     C                   MOVE      @@PFDT        XXPFDT
     C                   MOVEL     @@BSTY        XXBSTY
     C*
     C     @@BSTY        IFEQ      *BLANK
     C*
     C                   MOVEL(P)  TYPNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDT
     C*
     C                   EXFMT     SCRTYP
     C*
     C     *INKL         IFEQ      *ON
     C                   RETURN
     C                   ENDIF                                                  *INKL=*ON
     C*
     C                   MOVE      SCBSTY        XXBSTY
     C                   MOVE      SCBSTY        SSBSTY
     C*
     C                   ENDIF                                                  @@BSTY=*BLANK
     C*
     C     KEY2A         SETGT     DBRCDF
     C     KEY2          READPE    DBRCDF                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSSTAT        ANDEQ     'A'
     C                   LEAVE
     C                   ENDIF                                                  *IN60=*OFF/BSSTAT=A
     C*
     C     KEY2          READPE    DBRCDF                                 60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C  N60              MOVE      BXEFDT        XXMMDD
     C*
      *
      *
      *
     C*  IF CALLED AS A SEARCH WITH A SPECIFIED TYPE AUTOMATICALLY
     C*  RETURN THE VALUE IF ONLY ONE EXISTS FOR THAT TYPE
     C*
     C   30@@BSTY        IFNE      *BLANK
     C*
     C                   Z-ADD     *ZERO         XXCNT             2 0
     C     KEY2A         CHAIN     DBRCDF                             60
     C     *IN60         DOWEQ     *OFF
     C     XXCNT         ANDLE     1
     C                   MOVEL(P)  BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C  N60BSSTAT        IFEQ      'A'
     C                   ADD       1             XXCNT
     C                   Z-ADD     DBRRN         XXRRN
     C                   ENDIF                                                  BSSTAT=A
     C     KEY2A         READE     DBRCDF                                 60
     C                   ENDDO                                                  *IN60=*OFF/XXCNT<=1
     C*
     C     XXCNT         IFEQ      1
     C                   Z-ADD     XXRRN         @@RRN
     C                   RETURN
     C                   ENDIF                                                  XXCNT=1
     C*
     C                   ENDIF                                                  @@BSTY<>*BLANK
     C*
     C                   ENDIF                                                  @@PFDT=*BLANK
     C*
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY2A         READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY2A         READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
      *
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C     SSBSTY        IFNE      *BLANKS
     C                   MOVEL     SSBSTY        XXBSTY
     C                   ENDIF                                                  SSBSTY<>*BLANKS
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C   22KEY1B         SETGT     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C     SSBSTY        IFNE      *BLANKS
     C                   MOVEL     SSBSTY        XXBSTY
     C                   ENDIF                                                  SSBSTY<>*BLANKS
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C   22KEY1B         SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDESC
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
     C                   MOVE      SCTYPE        @@BSTY
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
     C*  CHECK BILLING AUX TYPE AGAINST SUBSET TYPE
     C*
     C     SSBSTY        IFNE      *BLANK
     C     SSBSTY        ANDNE     BXTYPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  BXTYPE<>SSBXTY<>*BLA
     C*
     C*
     C*  BILLING SCHEDULE CODE MUST BE ACTIVE
     C*
     C                   MOVEL     BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      BSON15        SVON15            1
     C                   MOVEL(P)  BSKEY         SVBSCD           10
     C                   Z-ADD     BXEFDT        SVEFDT            7 0
     C                   ELSE
     C                   MOVE      'X'           SVON15
     C                   MOVE      'X'           BSON15
     C                   MOVEL     *BLANKS       SVBSCD
     C                   Z-ADD     *ZEROS        SVEFDT
     C                   ENDIF
     C*
     C  N60BSSTAT        IFNE      'A'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  BSSTAT<>A
     C*
      *
      * Finds the number of installments for this billing schedule
      *
     C                   EXSR      FHIBS                                         Find XXHIBS
      *
      * Only execute if we passed a fund.
      *
     C     KKFND         IFNE      *ZEROS
      *
     C                   SELECT
     C     BXTYPE        WHENEQ    'A'
     C     BXTYPE        OREQ      'W'
     C     BXTYPE        OREQ      'T'
      *----------------------------------------------------------------
      * Logic for billing on the 15th
      *----------------------------------------------------------------
     C     XXCLT#        WHENEQ    '129'
     C     BSON15        ANDEQ     'Y'
     C     BXEFDT        ANDGT     XXMMDD
     C                   MOVE      *OFF          MEETSS
      *
     C     XXCLT#        WHENEQ    '129'
     C     BSON15        ANDEQ     'Y'
     C     BXEFDT        ANDLT     XXMMDD
     C     BXEFDT        ANDNE     *ZERO
     C     XXHIBS        ANDLE     XXIN15
      *
     C     BXKY1         SETLL     DBRCDF
     C     BXKY1         READE     DBRCDF                                 60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C     BSON15        IFEQ      'Y'
     C                   EXSR      FHIBS                                         Find XXHIBS
     C     BXBSCD        IFNE      SVBSCD
     C     BXEFDT        ANDGT     SVEFDT
     C     BXEFDT        ANDLE     XXMMDD
     C     XXHIBS        ANDLE     XXIN15
     C                   MOVE      *OFF          MEETSS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     BXKY1         READE     DBRCDF                                 60
     C                   ENDDO                                                  *IN60
     C                   Z-ADD     SVEFDT        KKEFDT
     C                   MOVEL(P)  SVBSCD        KKBSCD
     C     KEY3A         CHAIN     DBRCDF                             60
      *----------------------------------------------------------------
      * General logic for OTHER billing schedule (01/01)
      *----------------------------------------------------------------
     C     BXEFDT        WHENGT    XXMMDD
     C     BSON15        ANDEQ     *BLANK
     C                   MOVE      *OFF          MEETSS
      *
     C     BXEFDT        WHENLT    XXMMDD
     C     BSON15        ANDEQ     *BLANK
     C     BXEFDT        ANDNE     *ZERO
     C     XXHIBS        ANDLE     XXINCT
      *
     C     BXKY1         SETLL     DBRCDF
     C     BXKY1         READE     DBRCDF                                 60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  BXBSCD        XXKEY
     C     BSKY          CHAIN     PBS01                              60
     C     BSON15        IFEQ      *BLANK
     C                   EXSR      FHIBS                                         Find XXHIBS
     C     BXBSCD        IFNE      SVBSCD
     C     BXEFDT        ANDGT     SVEFDT
     C     BXEFDT        ANDLE     XXMMDD
     C     XXHIBS        ANDLE     XXINCT
     C                   MOVE      *OFF          MEETSS
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C     BXKY1         READE     DBRCDF                                 60
     C                   ENDDO                                                  *IN60
     C                   Z-ADD     SVEFDT        KKEFDT
     C                   MOVEL(P)  SVBSCD        KKBSCD
     C     KEY3A         CHAIN     DBRCDF                             60
      *
     C     XXHIBS        WHENGT    XXINCT
     C     BSON15        ANDEQ     *BLANK
     C                   MOVE      *OFF          MEETSS
      *
     C     XXHIBS        WHENGT    XXIN15
     C     BSON15        ANDEQ     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
     C*===============================================================
     C* FHIBS  - FINDS THE NUMBER OF INSTALLMENTS FOR CURRENT CODE
     C*===============================================================
     C*
     C     FHIBS         BEGSR
      *
     C                   Z-ADD     12            X                 2 0
     C                   DO        12
     C     BS(X)         IFNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     X             XXHIBS            2 0
     C*
     C                   ENDSR                                                  SUBSET
** Headers - HD
Code  Description                      month/day  Type           Type
Code  Description                      month/day  Type
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
217F17=Subset               YYY
