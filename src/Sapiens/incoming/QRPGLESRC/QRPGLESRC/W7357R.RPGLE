     Hcopyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7357R - WORK WITH EXPOSURE RECORDS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: MENU                                            *
     H*                                                               *
     H*  CALLS TO: W7357R02                                           *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 05/19/98  PROGRAMMER: C:87  PROJECT: WC591          *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 12/23/24 C:344 Support: PHW118190                             *
     H*                Expand Manual Rate field to hold 8.3 zoned.    *
     H*                                                               *
     H* 10/31/11 C:65  Project: WC1776 (REMOVED)                      *
     H*                WC1776 CHANGE REMOVED, WAS NOT NEEDED AFTER    *
     H*                REDESIGN.                                      *
     H*                                                               *
     H* 03/17/11 C:189 Project: WC1776 PRISS 44                       *
     H*                The exposure should be displayed after         *
     H*                dividing by 10                                 *
     H*                                                               *
     H* 12/31/07 C:15  Project: 114WJG                                *
     H*                Add code to help process corrections.          *
     H*                                                               *
     H* 08/12/05 C:15  Support: WCxxxx                                *
     H*                Add code for correction processing.            *
     H*                                                               *
     H* 04/16/02 C:15  Support: WC113/31613                           *
     H*                Added SEEXPO to key list to prevent paging     *
     H*                issues due to change in WDSEL.                 *
     H*                                                               *
     H*  07/30/00  C:87  PROJECT: WC591                               *
     H*                  Added state code to processing               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW7357F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDSEP     IF   E             DISK
     F                                     RENAME(PSE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDSEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSE01:DBRCDF)
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     FWMSRP     IF   E             DISK
     F*** Begin add *** 08/12/05 **************************************
     FWMSRP2L   UF A E           K DISK
     F                                     RENAME(PSR01:PSR02)
     FWDSEP2L   IF   E           K DISK
     F                                     RENAME(PSE01:PSE02)
     FWDSEP2L1  IF   E           K DISK
     F                                     RENAME(PSE01:PSE03)
     FWDSEP2L2  IF A E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PSE01:PSE04)
     FWMSQL     IF   E           K DISK
     FWMSQP2    O    E             DISK
     F                                     RENAME(PSQ01:PSQ02)
     F*** End add ***** 08/12/05 **************************************
      /COPY SCOPYWW01D
     D*
     D*** Beg chg *** 12/23/24 *** PHW118190 ************************
     D*** HD              S             78    DIM(1) CTDATA PERRCD(1)           Column headings
     D HD              S             79    DIM(1) CTDATA PERRCD(1)              Column headings
     D*** End chg *** 12/23/24 *** PHW118190 ************************
     D*** Begin chg *** 08/12/05 **************************************
     D******************* OF      1  10  1   OFD    35   Options, Function
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
     D*** End chg ***** 08/12/05 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Exposure Records')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W7357R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D DELETD          C                   CONST('*Deleted')
     D*
     D*  Data structures for SCDATA
     D                 DS
     D*** Beg chg *** 12/23/24 *** PHW118190 ************************
     D****XXDATA                 1     75
     D  XXDATA                 1     76
     D*** End chg *** 12/23/24 *** PHW118190 ************************
     D  XXCLAS                 1      5
     D  XXEXP#                 8      9
     D  XXEXMD                12     16
     D  XXMDDT                19     28
     D  XXRTDT                30     39
     D*** Beg chg *** 12/23/24 *** PHW118190 ************************
     D****XMNRT                41     49
     D    XXMNRT              41     50
     D*** Begin chg *** 03/17/11 **************************************
     D*************                          51  63 XXEXPA
     D*************                          65  75 XXPRMA
     D****XXEXPA                50     64
     D  XXEXPA                51     65
     D****XXPRMA                65     75
     D  XXPRMA                66     76
     D*** End   chg *** 03/17/11 **************************************
     D*** End chg *** 12/23/24 *** PHW118190 ************************
     D*
     D*** Begin add *** 08/12/05 **************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     I*** End add ***** 08/12/05 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     CHGPL2        PLIST
     C                   PARM                    XXNUM
     C                   PARM                    EDTMSK
     C                   PARM                    DECIML            3 0
     C                   PARM                    XXTHOU            1
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** End add ***** 08/12/05 **************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKMDDT
     C                   KFLD                    KKRTDT
     C                   KFLD                    KKMNRT
     C*** Begin add *** 04/16/02 **************************************
     C                   KFLD                    KKEXPO
     C*** End add ***** 04/16/02 **************************************
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCORR
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKMDDT
     C                   KFLD                    KKRTDT
     C                   KFLD                    KKMNRT
     C                   KFLD                    KKEXPO
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCORR
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C                   KFLD                    KKSTCD
     C                   KFLD                    KKCORR
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKLINK
     C                   KFLD                    KKUPDT
     C*** End add ***** 08/12/05 **************************************
     C*
     C     SEKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   KFLD                    KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*
     C*  Darts/admin control defn key
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Fund master file key
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Employer detail file key
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SECO#         KKCO#
     C     *LIKE         DEFINE    SEFND         KKFND
     C     *LIKE         DEFINE    SERPMY        KKRPMY
     C     *LIKE         DEFINE    SEEMP#        KKEMP#
     C     *LIKE         DEFINE    SEFYR         KKFYR
     C     *LIKE         DEFINE    SECLAS        KKCLAS
     C     *LIKE         DEFINE    SEMDDT        KKMDDT
     C     *LIKE         DEFINE    SERTDT        KKRTDT
     C     *LIKE         DEFINE    SEMNRT        KKMNRT
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C     *LIKE         DEFINE    SESTCD        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*** Begin add *** 04/16/02 **************************************
     C     *LIKE         DEFINE    SEEXPO        KKEXPO
     C*** End add ***** 04/16/02 **************************************
     C*** Begin add *** 08/12/05 **************************************
     C     *LIKE         DEFINE    SECORR        KKCORR
     C     *LIKE         DEFINE    SELINK        KKLINK
     C     *LIKE         DEFINE    SEUPDT        KKUPDT
     C*** End add ***** 08/12/05 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C*
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C     @@RRN         CHAIN     PSR01                              60
     C*
     C                   MOVE      SRCO#         KKCO#
     C                   MOVE      SRFND         KKFND
     C                   MOVE      SRRPMY        KKRPMY
     C                   MOVE      SREMP#        KKEMP#
     C                   MOVE      SRFYR         KKFYR
     C***  BEGIN ADD  ***  07/30/00  **********************************
     C                   MOVE      SRSTCD        KKSTCD
     C***  END   ADD  ***  07/30/00  **********************************
     C*
     C                   MOVE      *LOVAL        KKCLAS
     C                   MOVE      *LOVAL        KKMDDT
     C                   MOVE      *LOVAL        KKRTDT
     C                   MOVE      *LOVAL        KKMNRT
     C*** Begin add *** 04/16/02 **************************************
     C                   MOVE      *LOVAL        KKEXPO
     C*** End add ***** 04/16/02 **************************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C*
     C                   Z-ADD     *LOVAL        KKDIV
     C     EMKY          SETLL     PEM01
     C                   READ      PEM01                                  60
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  EMEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C*
     C     XXNUM         CAT       EMNAME:1      SCEMPL
     C*
     C                   MOVE      SRFYR         SCFYR
     C*
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE(P)   SCPST1        KKCLAS
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCCLAS        KKCLAS
     C                   MOVE      SCMDDT        KKMDDT
     C                   MOVE      SCRTDT        KKRTDT
     C                   MOVE      SCMNRT        KKMNRT
     C*** Begin add *** 04/16/02 **************************************
     C                   MOVE      SCEXPO        KKEXPO
     C*** End add ***** 04/16/02 **************************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*** Begin add *** 08/12/05 **************************************
     C*================================================================
     C* KGDO - Process F7 (add an exposure correction record)
     C*================================================================
     C     KGDO          BEGSR
     C*
     C                   EXSR      WRTHDR
     C                   EXSR      WRTEX1
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7357R22'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C*
     C                   ENDSR                                                  OPT6
     C*** End add ***** 08/12/05 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      SECLAS        SCCLAS
     C                   MOVE      SEMDDT        SCMDDT
     C                   MOVE      SERTDT        SCRTDT
     C                   MOVE      SEMNRT        SCMNRT
     C*** Begin add *** 04/16/02 **************************************
     C                   MOVE      SEEXPO        SCEXPO
     C*** End add ***** 04/16/02 **************************************
     C*
     C*  Edit stuff for output
     C*
     C*** Begin chg *** 08/12/05 **************************************
     C***************      MOVE SEEXP#    XXEXP#
     C                   MOVE      SEEXPO        XXEXP#
     C*** End chg ***** 08/12/05 **************************************
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  SECLAS        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCLAS
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C                   Z-ADD     SEMDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXMDDT
     C*
     C                   Z-ADD     SERTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRTDT
     C*
     C*       Use Edit Code 'N' to format the following numbers
     C                   MOVEL(P)  'N'           EDTMSK
     C*
     C                   Z-ADD     4             DECIML
     C                   MOVEL(P)  SEEXMD        XXNUM
     C                   CALL      CHGNUM        CHGPL2
     C                   MOVEL     XXNUM         XXEXMD
     C*
     C                   MOVEL(P)  SEMNRT        XXNUM
     C                   Z-ADD     3             DECIML
     C                   CALL      CHGNUM        CHGPL2
     C                   MOVEL     XXNUM         XXMNRT
     C*
     C*** Begin chg *** 03/17/11 **************************************
     C*************        MOVELSEEXPA    XXNUM     P
     C*************        Z-ADD0         DECIML
     C                   MOVEL(P)  SEEXPA        XXNUM#           11 1
     C*** BEGIN DEL ***  10/31/11  ************************************
     C*********            DIV  10        XXNUM#
     C*** END DEL *****  10/31/11  ************************************
     C                   MOVEL(P)  XXNUM#        XXNUM
     C                   Z-ADD     1             DECIML
     C*** End   chg *** 03/17/11 **************************************
     C*
     C                   CALL      CHGNUM        CHGPL2
     C     SEEXPA        IFLT      0
     C                   SUBST     XXNUM:2       XXEXPA
     C                   ELSE
     C                   MOVEL     XXNUM         XXEXPA
     C                   ENDIF
     C*
     C*** Begin Add *** 03/17/11 **************************************
     C                   Z-ADD     0             DECIML
     C*** End   Add *** 03/17/11 **************************************
     C                   MOVEL(P)  SEPRMA        XXNUM
     C                   CALL      CHGNUM        CHGPL2
     C     SEPRMA        IFLT      0
     C                   SUBST     XXNUM:2       XXPRMA
     C                   ELSE
     C                   MOVEL     XXNUM         XXPRMA
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXDATA        SCDATA
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*** Begin add *** 12/31/07 **************************************
     C*================================================================
     C* OPT6 - Process option 6 (correction addition)
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   SETON                                            31
     C                   EXSR      WRTHDR
     C                   EXSR      WRTEXP
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7357R22'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C*
     C                   SETOFF                                           31
     C*
     C                   ENDSR                                                  OPT6
     C*** End add ***** 12/31/07 **************************************
     C*** Begin add *** 08/12/05 **************************************
     C*================================================================
     C* OPT7 - Process option 7 (corrections)
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   EXSR      WRTHDR
     C                   EXSR      WRTEXP
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C*
     C                   CALL      'W7357R22'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN2        ##RRN
     C*
     C                   ENDSR                                                  OPT7
     C*** End add ***** 08/12/05 **************************************
     C*** Begin add *** 12/31/07 **************************************
     C*================================================================
     C* OPT8 - Process option 8 (correction deletion)
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   SETON                                            32
     C                   EXSR      WRTHDR
     C                   EXSR      WRTEXP
     C                   SETOFF                                           32
     C*
     C                   ENDSR                                                  OPT8
     C*** End add ***** 12/31/07 **************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** Begin add *** 08/12/05 **************************************
     C     *INKG         CASEQ     *ON           KGDO
     C                   ENDCS
     C*** End add ***** 08/12/05 **************************************
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*** Begin add *** 12/31/07 **************************************
     C     SCSEL         CASEQ     '6'           OPT6
     C     SCSEL         CASEQ     '8'           OPT8
     C*** End add ***** 12/31/07 **************************************
     C*** Begin add *** 08/12/05 **************************************
     C     SCSEL         CASEQ     '7'           OPT7
     C                   ENDCS
     C*** End add ***** 08/12/05 **************************************
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21SEKY          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21SEKY          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
     C*** Begin add *** 08/12/05 **************************************
     C*================================================================
     C* WRTEXP - This subroutine creates the exposure correction
     C*   record.
     C*================================================================
     C     WRTEXP        BEGSR
     C*
     C*  Determine if a correction record already exists for the
     C*  selected exposure record.
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C                   MOVE      SECLAS        KKCLAS
     C                   MOVE      SEMDDT        KKMDDT
     C                   MOVE      SERTDT        KKRTDT
     C                   MOVE      SEMNRT        KKMNRT
     C                   MOVE      SEEXPO        KKEXPO
     C                   MOVE      SVCORR        KKCORR
     C     KEY2          CHAIN     PSE02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      SELINK        KKLINK
     C                   MOVE      'R'           KKUPDT
     C     KEY4          CHAIN     PSE04                              99
     C                   ENDIF
     C*
     C*  If the correction record does not exist, write a "previous"
     C*  record and a "revised" record.
     C*
     C     *IN99         IFEQ      *ON
     C*
     C*  Retrieve a unique link #, then retreive the original exposure
     C*  record.
     C*
     C     KEY3          SETGT     PSE03
     C     KEY3          READPE    PSE03                                  99
     C  N99SELINK        ADD       1             SVLINK            3 0
     C   99              Z-ADD     1             SVLINK
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*
     C                   MOVE      'P'           SEUPDT
     C                   MOVE      SVCORR        SECORR
     C                   MOVE      SVLINK        SELINK
     C                   MOVE      Q1USER        SEUSER
     C                   MOVE      Q1PGM         SEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEDT
     C                   MOVE      SEUSER        SEIUSR
     C                   MOVE      SEDT          SEIDT
     C*** Begin chg *** 12/31/07 **************************************
     C***************      WRITEPSE04
     C  N31              WRITE     PSE04
     C*** End chg ***** 12/31/07 **************************************
     C*
     C                   MOVE      'R'           SEUPDT
     C*** Begin chg *** 12/31/07 **************************************
     C***************      WRITEPSE04
     C  N32              WRITE     PSE04
     C*** End chg ***** 12/31/07 **************************************
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEX1 - This subroutine creates the exposure correction
     C*   record.
     C*================================================================
     C     WRTEX1        BEGSR
     C*
     C*  Retrieve a unique link #, then write a new "revised" exposure
     C*  record.
     C*
     C     KEY3          SETGT     PSE03
     C     KEY3          READPE    PSE03                                  99
     C  N99SELINK        ADD       1             SVLINK            3 0
     C   99              Z-ADD     1             SVLINK
     C*
     C     SEKY          CHAIN     DBRCDF                             95
     C*
     C                   MOVE      'R'           SEUPDT
     C                   MOVE      SVCORR        SECORR
     C                   MOVE      SVLINK        SELINK
     C                   MOVE      Q1USER        SEUSER
     C                   MOVE      Q1PGM         SEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEDT
     C                   MOVE      SEUSER        SEIUSR
     C                   MOVE      SEDT          SEIDT
     C                   WRITE     PSE04
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHDR - This subroutine creates the header and unit total
     C*   correction records.
     C*================================================================
     C     WRTHDR        BEGSR
     C*
     C*  Determine if a header correction record already exists.
     C*  If not, create it.
     C*
     C                   Z-ADD     0             SVCORR            1 0
     C     SEKY          CHAIN     PSR02                              99
     C     *IN99         DOWEQ     *OFF
     C     SRSTAT        ANDEQ     'P'
     C     SEKY          READE     PSR02                                  99
     C                   ENDDO
     C                   MOVE      SRCORR        SVCORR
     C*
     C*  Load the correction fields.
     C*
     C     *IN99         IFEQ      *ON
     C*
     C     @@RRN         CHAIN     PSR01                              60
     C                   ADD       1             SVCORR
     C                   MOVE      SVCORR        SRCORR
     C                   MOVE      'E'           SRCTYP
     C                   MOVE      Q1USER        SRUSER
     C                   MOVE      Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SRTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVE      SRUSER        SRIUSR
     C                   MOVE      SRDT          SRIDT
     C                   WRITE     PSR02
     C*
     C     SEKY          CHAIN     PSQ01                              60
     C                   MOVE      SVCORR        SQCORR
     C                   MOVE      Q1USER        SQUSER
     C                   MOVE      Q1PGM         SQPGM
     C                   Z-ADD     SRTIME        SQTIME
     C                   Z-ADD     SRDT          SQDT
     C                   MOVE      SQUSER        SQIUSR
     C                   MOVE      SQDT          SQIDT
     C                   WRITE     PSQ02
     C*
     C                   ELSE
     C*
     C     SRCTYP        IFNE      'E'
     C                   MOVE      'M'           SRCTYP
     C                   ENDIF
     C                   UPDATE    PSR02
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C*** End add ***** 08/12/05 **************************************
      //**** Beg Chg **** 12/23/24 **** PHW118190 *********************
      //**** Manual Rate was increased by 1 char. Shifted Manual Rate
      //**** and subsequent columns 1 char to the right.
      //**** End Chg **** 12/23/24 **** PHW118190 *********************
** Column headings
Opt Cls  Code ExpMod   Mod Date  Rate Date  Manual Rt       Exposure    Premium
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Correction addition    YNN
1077=Correction             YNN
1088=Correction deletion    YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Add correction        YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
