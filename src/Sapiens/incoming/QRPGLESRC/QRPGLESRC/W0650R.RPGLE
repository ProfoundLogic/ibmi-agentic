     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0650R - PAYROLL CLASS EXTENSION MAINTENANCE
     H*****************************************************************
     H* DESCRIPTION:  THIS PROGRAM DEFINES EACH CLASS EXT DESCRIPTIONS*
     H*                                                               *
     H* CALLED FROM:  W0630R - WORK WITH PAYROLL CLASS CODES          *
     H* CALLS TO:     W0650R02 - EXT DESCRIPTION DATA SCREEN          *
     H* PROGRAMMER:   C:65   08/11/98        PROJECT: WC843           *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/09/18 C:TXO Project: WC2583                                *
     H*                Add Expiration date to the subfile grid        *
     H*                                                               *
     H* 01/12/18 C:267 Project: WC2583                                *
     H*                Add Effective date to the subfile grid         *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109AU}                                        *
     H*                Add new opt 90=Expanded Description            *
     H*                                                               *
     H* 08/29/03 C:61  SUPPORT WA099 38724                            *
     H*                Change to use SUBSET routine to only process   *
     H*                rating state passed in or records with blank   *
     H*                rating state.                                  *
     H*                                                               *
     H* 08/23/02 C:61  SUPPORT WC945                                  *
     H*                Only display for rating state and where state  *
     H*                value is blank.                                *
     H*                                                               *
     H* 11/16/99 C:90 PROJECT: WC945                                  *
     H*               WHEN CALLED FROM A SEARCH, WILL ONLY DISPLAY    *
     H*               VALID EXTENSIONS FOR THE RATING STATE AND WHERE *
     H*               THE STATE VALUE IS BLANK.                       *
     H*                                                               *
     H* 09/17/99 C:90 PROJECT: WC945                                  *
     H*               ADDED STATE VALUE TO SCREEN.                    *
     H*                                                               *
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
     H*  30 - Called as a search                                      *
     H*  31 - Called as a search w/YR                                 *
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
     FW0650F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      **** BEGIN CHG **** 09/17/99 ************************************
     F******WMPXL   IF  E           K        DISK
     FWMPXL1    IF   E           K DISK
      **** END   CHG **** 09/17/99 ************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPX01:DBRCDF)
     FWMPXP     IF   E             DISK
     F                                     RENAME(PPX01:PFRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              COLUMN HEADINGS
     D OF              S              1    DIM(26) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(26) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Payroll Class Descri-
     D                                     ption Extension')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0650R02')
     D***  Begin Add  ***  06/24/05  **************************************
     D EXTPGM          C                   CONST('W0650R90')
     D***  End   Add  ***  06/24/05  **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
      **** BEGIN ADD **** 01/12/18 ************************************
     D WKEFDT          S              8  0
      **** END   ADD **** 01/12/18 ************************************

      **** BEGIN ADD **** 02/09/18 ************************************
     D WKEXDT          S              8  0 INZ
      **** END   ADD **** 02/09/18 ************************************

     C*
     C     *LIKE         DEFINE    PXCO#         XXCO#
     C     *LIKE         DEFINE    PXCLAS        XXCLAS
     C     *LIKE         DEFINE    PXEXT         XXEXT
      **** BEGIN ADD **** 09/17/99 ************************************
     C     *LIKE         DEFINE    PXST          XXST
      **** END   ADD **** 09/17/99 ************************************
      **** BEGIN ADD **** 01/12/18 ************************************
     C     *LIKE         DEFINE    PXEFDT        XXEFDT
      **** END   ADD **** 01/12/18 ************************************
      **** BEGIN ADD **** 02/09/18 ************************************
     C     *LIKE         DEFINE    PXEXDT        XXEXDT
      **** END   ADD **** 02/09/18 ************************************
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    XXCO#
     C                   PARM                    XXCLAS
      ***** BEG ADD ******* 08/29/03 **********************************
     C                   PARM      @@ST          ##ST              2
      ***** END ADD ******* 08/29/03 **********************************
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C***  End   Add  ***  06/24/05  **************************************
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
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEXT
      **** BEGIN ADD **** 09/17/99 ************************************
     C                   KFLD                    XXST
      **** END   ADD **** 09/17/99 ************************************
      **** BEGIN ADD **** 01/12/18 ************************************
     C                   KFLD                    XXEFDT
      **** END   ADD **** 01/12/18 ************************************
     C*
     C     KEY1C         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@CLAS            5 0
      **** BEGIN ADD **** 11/16/99 ************************************
     C                   PARM                    @@ST              2
      **** END   ADD **** 11/16/99 ************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
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
     C* CVTDT2 - SUBROUTINE TO CONVERT DATE FROM MDY TO CYMD
     C*===============================================================
     C*
     C     CVTDT2        BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT2
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
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        XXEXT
      **** BEGIN ADD **** 11/16/99 ************************************
      ***** BEG CHG ******* 08/29/03 **********************************
     C*********            MOVE *BLANK    XXST
     C                   MOVE      *LOVAL        XXST
      ***** END CHG ******* 08/29/03 **********************************
      **** END   ADD **** 11/16/99 ************************************
     C                   ENDSR                                                  KEYINZ
     C*
     C*
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        XXEXT
      **** BEGIN ADD **** 09/17/99 ************************************
     C                   MOVE      SCPST2        XXST
      **** END   ADD **** 09/17/99 ************************************
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C                   MOVE      SCEXT         XXEXT
      **** BEGIN ADD **** 09/17/99 ************************************
     C                   MOVE      SCST          XXST
      **** END   ADD **** 09/17/99 ************************************
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      PXEXT         SCEXT
     C                   MOVE      PXDESC        SCDESC
     C                   MOVE      PXSTAT        SCSTAT
     C                   MOVE      *BLANKS       SCDEL
      **** BEGIN ADD **** 09/17/99 ************************************
     C                   MOVE      PXST          SCST
      **** END   ADD **** 09/17/99 ************************************
      **** BEGIN ADD **** 01/12/18 ************************************
     C                   MOVE      PXEFDT        XXEFDT
     C                   IF        PXEFDT   <>   0
     C                   Z-ADD     PXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      /FREE
         WKEFDT = %DEC(%SUBST(SRDTO:1:8):8:0);
      /END-FREE
     C     *USA          MOVE      WKEFDT        SCEFDT
     C                   ENDIF
      **** END   ADD **** 01/12/18 ************************************
      **** BEGIN ADD **** 02/09/18 ************************************
     C                   EVAL      WKEXDT  =  01010001
     C     *USA          MOVE      WKEXDT        SCEXDT
     C                   IF        PXEXDT   <>   0
     C                   Z-ADD     PXEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      /FREE
         WKEXDT = %DEC(%SUBST(SRDTO:1:8):8:0);
      /END-FREE
     C     *USA          MOVE      WKEXDT        SCEXDT
     C                   ENDIF
      **** END   ADD **** 02/09/18 ************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=ACTIVATE/DEACTIVATE
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C***  Begin Add  ***  06/24/05  **************************************
     C*===============================================================
     C* OPT90 - SUBROUTINE FOR OPTION 90
     C*===============================================================
     C*
     C* OPTION 90=EXPANDED DESCRIPTION
     C*
     C     OPT90         BEGSR
     C*
     C                   MOVEL     'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      EXTPGM        ACDPL1                 99
     C*
     C                   ENDSR                                                  OPT90
     C***  End   Add  ***  06/24/05  **************************************
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
     C***  Begin Add  ***  06/24/05  **************************************
     C     SCSEL         CASEQ     '90'          OPT90
     C***  End   Add  ***  06/24/05  **************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     @@CALL        COMP      '3'                                    31    SEARCH W/YR
     C*
     C   31              MOVEL     '2'           @@CALL
     C*
     C                   MOVE      @@CLAS        XXCLAS
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@CLAS        SCCLAS
     C                   MOVE      *BLANKS       SCPST1
     C                   Z-ADD     0             @@RRN
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1C         READE     DBRCDF                                 95
      *
      ***** BEG DEL ******* 08/29/03 **********************************
      *****BEGIN ADD **** 11/16/99 ************************************
     C*********
     C***21*30*  *IN60     DOUEQ*ON
     C*********  PXST      IFEQ *BLANK
     C*********  PXST      OREQ @@ST
     C*********            MOVE *ON       *IN60
     C*********            ELSE
     C*********  KEY1C     READEDBRCDF                   60
      ******BEG ADD ******* 08/23/02 **********************************
     C***60****            SETON                     95
      ******END ADD ******* 08/23/02 **********************************
     C*********            ENDIF
     C*********            ENDDO
      *****END*  ADD **** 11/16/99 ************************************
      ***** END DEL ******* 08/29/03 **********************************
     C*
     C                   ENDSR                                                  READ
     C*
     C*
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1C         READPE    DBRCDF                                 95
      *
      ***** BEG DEL ******* 08/29/03 **********************************
      *****BEGIN ADD **** 11/16/99 ************************************
     C*********
     C***21*30*  *IN60     DOUEQ*ON
     C*********  PXST      IFEQ *BLANK
     C*********  PXST      OREQ @@ST
     C*********            MOVE *ON       *IN60
     C*********            ELSE
     C*********  KEY1C     REDPEDBRCDF                   60
      ******BEG ADD ******* 08/23/02 **********************************
     C***60****            SETON                     95
      ******END ADD ******* 08/23/02 **********************************
     C*********            ENDIF
     C*********            ENDDO
      *****END*  ADD **** 11/16/99 ************************************
      ***** END DEL ******* 08/29/03 **********************************
     C*
     C                   ENDSR                                                  READP
     C*
     C*
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
      **** BEGIN ADD **** 02/09/18 ************************************
     C                   EXSR      MVDBSC
      **** END   ADD **** 02/09/18 ************************************
     C                   MOVEL     DELETD        SCDEL
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
      ***** BEG ADD ******* 08/29/03 **********************************
      *
      * IF called as SEARCH then only display records with same state
      * OR blank state.
      *
     C     PXST          IFNE      *BLANKS
     C     PXST          ANDNE     @@ST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  PXST
      *
      ***** END ADD ******* 08/29/03 **********************************
     C                   ENDSR                                                  SUBSET
     C*
** Column Headings
Opt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
19090=Exp Desc              YYN                            06/24/00
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
