     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H Bnddir('STBNDDIR')
     H/define CONVERT
     H*
     H/TITLE F0032R - PROVIDER Master Maintenance - Search Program
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM DESCRIPTION:                                       *
     H*    This program displays PROVIDER records.  The user has the  *
     H*    ability to execute the following functions:                *
     H*                                                               *
     H*      1) Re-position at a specific record.                     *
     H*      2) F11 to toggle between:                                *
     H*         PROVIDER Name order and FEIN order        -or-        *
     H*         Subset PROVIDER Name order and FEIN order with        *
     H*         PDDBA1 equal 'Y'.                                     *
     H*      4) INQUIRY Mode:                                         *
     H*           Option '5' to Display                               *
     H*         SELECT Mode:                                          *
     H*           Option '1' to Select                                *
     H*           Option '5' to Display                               *
     H*         UPDATE Mode:                                          *
     H*           Option '2' to Update                                *
     H*           Option '5' to Display                               *
     H*           Option '20' to Activate/Inactivate record           *
     H*      5) F17 to enter Subset fields to limit Display records   *
     H*                                                               *
     H*  Called From:                                                 *
     H*    Menu                                                       *
     H*    Select programs                                            *
     H*                                                               *
     H*  Calls To:                                                    *
     H*    F0032R01                                                   *
     H*    QCLSCAN                                                    *
     H*    SA001R                                                     *
     H*    SRCENTER                                                   *
     H*    SRNAME                                                     *
     H*    F0040R                                                     *
     H*    F0041R                                                     *
     H*    SY601R                                                     *
     H*    F2493R                                                     *
     H*    F2335R                                                     *
     H*    N0037R    Work with Network Contracts                      *
     H*                                                               *
     H*    PROGRAMMER: C:61                                           *
     H*    CREATED:   12/19/97      PROJECT: MC159                    *
     H*               Copy from F0031R and Rewrite                    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H* 04/12/18 C:180 Project: 114MJB                                *
     H*                Add Provider info to Action Panel              *
     H*                                                               *
     H* 08/20/14 C:212 Project: 114MOJ
     H*                Add field on screen to display NPI#
     H*
     H* 11/15/12 C:287 Project: MC367
     H*                Remove options/functions for GS access to Bill Entry
     H*                                                               *
     H* 12/13/11 C:191 PROJECT: LA025                                 *
     H*                Add processing for LA Parish sales tax flag    *
     H*                                                               *
     H* 09/29/10 C:257 PROJECT: 133MJN                                *
     H*                ADD option 10 for W/W Provider sales tax       *
     H*
     H* 12/21/09 C:62  Support: SEQ71115
     H*                Don't zero fein and mod if fein/mod passed in.
     H*                                                               *
     H* 11/02/09 C:232 PROJECT: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*
     H* 03/31/09 C:79  Support: SEQ67643
     H*                1. Blank pad loading of position to fields
     H*                2. Zero FEIN and Mod on position to
     H*                3. Translate lower to upper on subset name
     H*
     H* 08/30/07 C:47  Project: SEQ59490
     H*                ADD XXFEIN AND XXMOD TO KEY1 KEY LIST.
     H*                                                               *
     H* 03/09/05 C:15  Project: WC1269                                *
     H*                Chgd pgm to display the out of country address *
     H*                line instead of the C/S/Z, if applicable.      *
     H*
     H*  10/08/02 C:62  Project: 114MEP
     H*                 SUBSET PAR/NON-PAR IF QQTYPE = 'N' OR 'P'
     H*
     H*  01/10/02 C:62  Project: WV004
     H*                 PASS IN ADJUDICATION STATE PARM TO PRE-SUBSET
     H*                 TO THAT ADJUDICATION STATE.
     H*
     H*  10/08/01 C:62  Project: MCMNT 29205  BASE ONLY
     H*                 MODIFY SCREEN HEADERS TO ACCOMODATE 37A
     H*                 ADDRESS FIELDS.
     H*
     H*  09/07/01 C:62  Project: MC044 28283
     H*                 IGNORE DBA WHEN LOADING NAME/ADDRESS.
     H*
     H*  04/12/01 C:62  Project: MC245
     H*                 POPULATE PHYSICAL ADDRESS.
     H*
     H*  03/23/01 C:62  Project: MCMNT  BASE ONLY
     H*                 LIFT LOGIC THAT PROTECTS PROVIDER TYPE SUBSET.
     H*
     H*  10/12/00 C:62  Project: MCMNT
     H*                 ADDED SECOND PART OF ZIP CODES(LAST 4 CHAR).
     H*                 DID THIS BY CAT.
     H*
     H*  08/31/00 C:62  PROJECT: MC237
     H*                 ADD ADDITIONAL VIEW FOR DBA/ADDRESS.
     H*
     H*  08/15/00 C:62  PROJECT: MC215
     H*                 REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  06/20/00 C:62 PROJECT:MCMNT
     H*                -GET RID OF FIRST NAME TO SCREEN NAME WHEN
     H*                 SUBSET TO DBA = 'Y'.
     H*                -FIX PAGING WHEN SUBSET TO DBA = 'Y'.
     H*
     H*  05/05/00 C:62 PROJECT:FSMNT
     H*                CHANGE OPTNM.
     H*
     H*  04/17/00 C:62 PROJECT: MC010 22593
     H*                CHANGE INIT OF KEY FIELDS.
     H*
     H*  04/06/00 C:62 PROJECT: MC225
     H*                ADD PARM TO CONTROL THE INITIAL VIEW.
     H*
     H*  04/06/00 C:62 PROJECT: MC225
     H*                ADD PARM TO CONTROL THE INITIAL VIEW.
     H*
     H*  01/31/00 C:62 PROJECT: MC048 21648
     H*                IF MOD IS SENT INTO PROGRAM THEN POSITION TO
     H*                THAT MODIFIER.
     H*
     H*  11/04/99 C:62 PROJECT: MC025 20469
     H*                CHANGE ASSIGNMENT OF SCNAME
     H*
     H*  11/03/99 C:62 PROJECT: MC025 20469
     H*                FIX THE PAGING
     H*
     H*  07/09/99 C:62 PROJECT: MC209
     H*                SUBSET ON PARTICIPATING
     H*
     H*  03/30/99 C:33 PROJECT: WC577                                 *
     H*          C:118 RESET VALID OPTION FLAG AFTER READS TO BE SURE *
     H*                CORRECT RRN'S ARE LOADED.                      *
     H*
     H*  03/25/99 C:56  Project: MC152
     H*                 Load the mailing name and address based on
     H*                 DBA flag (FMPDP/PDDBA1)
     H*
     H*  11/02/98 C:62  Project: MC145
     H*                 1) ADDED MODE PARM TO F0040R AND F0041R.
     H*                 2) ADD LOGICAL FMPDL4-7 FOR SUBSET ON PAR/NON.
     H*
     H*  09/28/98 C:56  Project: MC152
     H*                 Add entry parm QQ1PRV to indicate a subset
     H*                 only on QQFEIN and QQMOD.
     H*
     H*  07/14/98 C:61  Project: 98MAY
     H*               - Add entry parm, indicate if subset (and restrict
     H*                 access) to Provder Types '1' and '4' only.
     H*
     H*  06/05/98 C:61 PROJECT SUPPORT
     H*                Correct API name used to get Company Info
     H*
     H*  01/15/98 C:31  Project: 60MBS
     H*                 Anthem Network
     H*                 1. Add link to N0037R - Network Contracts
     H*                 2. Change opt 7 to opt 8
     H*                 3. Combine opt 8 and 9 into opt 7
     H*                 4. Add F6 (Add) if stand alone
     H*                 5. Remove F13
     H*
     H*   PLEASE ADD TO TOP
     H*****************************************************************
     H*
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*       09 - Inquiry                                            *
     H*       19 - Call from other pgm - SELECT mode                  *
     H*      N20 21 - Name Logical file access                        *
     H*       20N21 - FEIN Logical file access                        *
     H*       20 21 - BWC  Logical file access                        *
     H*       25 - Subset Window - Status Flag invalid (A/I)          *
     H*       22 - Subset Window - DBA Flag invalid (Y/N)             *
     H*       23 - Subset Window - Provider Type invalid (1 - 6)      *
     H*       24 - Subset Window - Provider Type invalid (1 - 6)      *
     H*       40 - LOKUP result indicator                             *
     H*       60 - General usage                                      *
     H*       61 - Load screen with Servicing providers only          *
     H*       62 - Subset Window - Invalid Par/Non/blank              *
     H*       63 - Subset Window - Position cursor to Specialty Code  *
     H*       75 - Option 8 err check                                 *
     H*       76 - Option 7 err check                                 *
     H*       77 - Option 7 err check if SCFEIN = *zeros              *
     H*       97 - SCSDBA = 'Y', use WMPDL68 for file access          *
     H*       98 - WCMS is not linked.  WCMC is Standalone            *
     H*      N98 - WCMS is linked.      WCMC is not Standalone.       *
     H*       99 - Subfile access                                     *
     H*                                                               *
     H*   SCR03                                                       *
     H*       78 - Cur/pos SCR03/SCSTY1 Provider type 1               *
     H*       79 - Cur/pos SCR03/SCSTY2 Provider type 2               *
     H*       80 - Cur/pos SCR03/SCSPAR Specialty Code                *
     H*       81 - Cur/pos SCR03/SCSPST Physical State                *
     H*       82 - Cur/pos SCR03/SCSPAR Zip code                      *
     H*                                                               *
     H*****************************************************************
     H*
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*    FILES:                                                     *
     H*       ______ (_____): ___________________ (___/___)           *
     H*****************************************************************
     H*
      *
      * Sub File Display Screen
     FF0032F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      * PROVIDER Demographics   Key=NAME/FSN/FEIN/MOD
     FFMPDL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
      *
      * PROVIDER Demographics   Key=FEIN/MOD
     FFMPDL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPD01:PPD02)
      *
      * PROVIDER Demographics Physical
     FFMPDP     IF   E             DISK
     F                                     RENAME(PPD01:PFRCDF)
      *
      * PROVIDER Demographics   Key=ADR1/FEIN/MOD
     FFMPDL2    IF   E           K DISK
     F*** BEGIN CHG *** 08/31/00 **************************************
     F**********  PPD01                             KRENAMEPPD04
     F**********                                    KINFDS DBFDS4
     F                                     RENAME(PPD01:PPD03)
     F                                     INFDS(DBFDS3)
     F*** END   CHG *** 08/31/00 **************************************
      *
      * PROVIDER Demographics   Key=BWC ID/FEIN/MOD
     FFMPDL360  IF   E           K DISK
     F*** BEGIN CHG *** 08/31/00 **************************************
     F**********  PPD01                             KRENAMEPPD03
     F**********                                    KINFDS DBFDS3
     F                                     RENAME(PPD01:PPD04)
     F                                     INFDS(DBFDS4)
     F*** END   CHG *** 08/31/00 **************************************
     F*** BEGIN ADD *** 11/02/98 **************************************
     FFMPDL4    IF   E           K DISK
     F                                     RENAME(PPD01:PPD05)
     F                                     INFDS(DBFDSA)
     FFMPDL5    IF   E           K DISK
     F                                     RENAME(PPD01:PPD06)
     F                                     INFDS(DBFDSB)
     FFMPDL6    IF   E           K DISK
     F                                     RENAME(PPD01:PPD07)
     F                                     INFDS(DBFDSC)
     FFMPDL7    IF   E           K DISK
     F                                     RENAME(PPD01:PPD08)
     F                                     INFDS(DBFDSD)
     F*** END   ADD *** 11/02/98 **************************************
     F*** BEGIN ADD *** 02/19/99 **************************************
     F*** BEGIN CHG *** 07/09/99 **************************************
     F*FMPDL8**IF  E           K        DISK
     F**********  PPD01                             KRENAMEPPD09
     F**********                                    KINFDS DBFDSE
     F*FMPDL9**IF  E           K        DISK
     F**********  PPD01                             KRENAMEPPD10
     F**********                                    KINFDS DBFDSF
     F*FMPDL10*IF  E           K        DISK
     F**********  PPD01                             KRENAMEPPD11
     F**********                                    KINFDS DBFDSG
     FFMPDL11   IF   E           K DISK
     F                                     RENAME(PPD01:PPD09)
     F                                     INFDS(DBFDSE)
     FFMPDL12   IF   E           K DISK
     F                                     RENAME(PPD01:PPD10)
     F                                     INFDS(DBFDSF)
     FFMPDL13   IF   E           K DISK
     F                                     RENAME(PPD01:PPD11)
     F                                     INFDS(DBFDSG)
     F*** END   CHG *** 07/09/99 **************************************
     F*** BEGIN ADD *** 08/31/00 **************************************
     FFMPDL14   IF   E           K DISK
     F                                     RENAME(PPD01:PPD12)
     F                                     INFDS(DBFDSH)
     FFMPDL15   IF   E           K DISK
     F                                     RENAME(PPD01:PPD13)
     F                                     INFDS(DBFDSI)
     FFMPDL16   IF   E           K DISK
     F                                     RENAME(PPD01:PPD14)
     F                                     INFDS(DBFDSJ)
     FFMPDL17   IF   E           K DISK
     F                                     RENAME(PPD01:PPD15)
     F                                     INFDS(DBFDSK)
     F*** END   ADD *** 08/31/00 **************************************
      *
      * PROVIDER Auxiliary      Key=FEIN/MOD
     FFMP1L     IF   E           K DISK
      *
      *  Generic Table file
     FFTGTL     IF   E           K DISK    USROPN
      *
      *  Application Fee Schedule Base Control File
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SFBPF   IF  E                    DISK
     F*** END   DEL *** 08/15/00 **************************************
      *
      *  State Code Table File
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PST02)
     FFMSIL     IF   E           K DISK
     F*** Beg Add *** 04/12/18 ***************************************
     FFACLML8   if   e           k disk    USROPN
     FFAHDRL    if   e           k DISK    USROPN
     FFAEOP     if   e             DISK    USROPN
     F*** End Add *** 04/12/18 ***************************************
      *
      /COPY SCOPYWW01D
      *** Beg Add ****** 04/12/18 **************************
      /Copy sPrModule
      *** End Add ****** 04/12/18 **************************
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column Headings
     D*** BEGIN ADD *** 09/29/10 **************************************
     D*                   OF      1  21  1   OFD    35   Options, Function
     D OF              S              1    DIM(22) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(22) ALT(OF)
     D*** END   ADD *** 09/29/10 **************************************
     D TYP             S              3  0 DIM(100)                             Service Prov.
     d*** Beg Add *** 04/12/18 ****************************************
     D xxZip           S                   LIKE(EOzip)
     D xxRRN           S              9P 0 INZ
     D QQRRN           S              9P 0
     d*** End Add *** 04/12/18 ****************************************
      *
     D***PY SCOPYWW01E
      *
     D***PY SCOPYWW01I
     D*** BEGIN ADD *** 11/02/98 **************************************
     D*
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D*** END   ADD *** 11/02/98 **************************************
     D*** BEGIN ADD *** 11/02/98 **************************************
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D*** END   ADD *** 11/02/98 **************************************
     D*** BEGIN ADD *** 08/31/00 **************************************
     D DBFDSH          DS
     D  DBRRNH               397    400B 0
     D DBFDSI          DS
     D  DBRRNI               397    400B 0
     D DBFDSJ          DS
     D  DBRRNJ               397    400B 0
     D DBFDSK          DS
     D  DBRRNK               397    400B 0
     D*** END   ADD *** 08/31/00 **************************************
     D                 DS
     D  DATA                   1     35
     D  FM                    28     28
     D  FI                    29     29
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  TY                     1      2
     D                                     DIM(2)                               Provider Type
     D  SCSTY1                 1      1
     D  SCSTY2                 2      2
      *
     D                 DS
     D  FEINMD                 1     14
      *
     D BLKBWC          C                   CONST('Not Available')
     D PGMCO#          C                   CONST('FA007R    ')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D*** BEG   ADD *** 04/12/18 **************************************
     D MOD837p         C                   CONST('837 ASC X12 Prof')
     D MOD837i         C                   CONST('837 ASC X12 Inst')
     D*** END   ADD *** 04/12/18 **************************************
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'Provider Master fil- C         OPTNM
     D**********    'e'
     D OPTNM           C                   CONST('Providers')
     D*** END   CHG *** 05/05/00 **************************************
      ***** BEG DEL ******* 06/05/98 **********************************
     D*********     'WA001R'              C         COMAPI
     D*********     'W0006R'              C         USPAPI
      ***** END DEL ******* 06/05/98 **********************************
      ***** BEG ADD ******* 06/05/98 **********************************
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
      ***** END ADD ******* 06/05/98 **********************************
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('F0032R01')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(4)
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D ADDFKY          C                   CONST('06F6=Add            -
     D                                            YNN')
      *

     C     *LIKE         DEFINE    PDNAME        XXNAME
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    PDFSN         XXFSN
     C     *LIKE         DEFINE    PDADR1        U1PDSC
     C     *LIKE         DEFINE    PDADR1        U2PDSC
     C     *LIKE         DEFINE    PDBWC#        XXBWC#
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C*** BEG ADD *** 09/28/98 ****************************************
     C     *LIKE         DEFINE    PDFEIN        HLFEIN
     C     *LIKE         DEFINE    PDMOD         HLMOD
     C*** END ADD *** 09/28/98 ****************************************
     C*** BEGIN ADD *** 06/20/00 **************************************
     C     *LIKE         DEFINE    PDFEIN        X1FEIN
     C     *LIKE         DEFINE    PDMOD         X1MOD
     C*** END   ADD *** 06/20/00 **************************************
     C*** BEGIN ADD *** 08/31/00 **************************************
     C     *LIKE         DEFINE    PDADR1        XXADR1
     C*** END   ADD *** 08/31/00 **************************************
      *
      *  F0032R1
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  STATE PARM LIST
      *
     C     STSPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
      *
     C     KEY1          KLIST
     C                   KFLD                    XXNAME
     C                   KFLD                    XXFSN
     C*** BEGIN ADD *** 08/30/07 **************************************
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*** END   ADD *** 08/30/07 **************************************
      *
     C     KEY2          KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C     KEY4          KLIST
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           KFLD           XXNAME
     C                   KFLD                    XXADR1
     C*** END   CHG *** 06/20/00 **************************************
     C*** BEGIN ADD *** 06/20/00 **************************************
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*** END   ADD *** 06/20/00 **************************************
      *
     C     KEY5          KLIST
     C                   KFLD                    XXBWC#
     C*** BEGIN ADD *** 11/02/98 **************************************
     C*
     C* VIEW 1A AND SCSPAR NE BLANKS.
     C*
     C     KEY6          KLIST
     C                   KFLD                    SCSPAR
     C                   KFLD                    XXNAME
     C                   KFLD                    XXFSN
     C*
     C* VIEW 2 AND SCSPAR NE BLANKS.
     C*
     C     KEY7          KLIST
     C                   KFLD                    SCSPAR
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C* VIEW 1B AND SCSPAR NE BLANKS.
     C*
     C     KEY8          KLIST
     C                   KFLD                    SCSPAR
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           KFLD           XXNAME
     C                   KFLD                    XXADR1
     C*** END   CHG *** 06/20/00 **************************************
     C*
     C* VIEW 3 AND SCSPAR NE BLANKS.
     C*
     C     KEY9          KLIST
     C                   KFLD                    SCSPAR
     C                   KFLD                    XXBWC#
     C*
     C*** END   ADD *** 11/02/98 **************************************
     C*** BEGIN ADD *** 02/19/99 **************************************
     C*
     C* VIEW 1A AND SSFEIN NE ZEROS.
     C*
     C     KEY10         KLIST
     C                   KFLD                    SSFEIN
     C*** BEGIN DEL *** 07/09/99 **************************************
     C**********           KFLD           SSMOD
     C*** END   DEL *** 07/09/99 **************************************
     C                   KFLD                    XXNAME
     C                   KFLD                    XXFSN
     C*** BEGIN ADD *** 11/03/99 **************************************
     C                   KFLD                    XXMOD
     C*** END   ADD *** 11/03/99 **************************************
     C*
     C* VIEW 2 AND SSFEIN NE ZEROS.
     C*
     C     KEY11         KLIST
     C                   KFLD                    SSFEIN
     C*** BEGIN DEL *** 07/09/99 **************************************
     C**********           KFLD           SSMOD
     C*** END   DEL *** 07/09/99 **************************************
     C*
     C* VIEW 1B AND SSFEIN NE ZEROS.
     C*
     C     KEY12         KLIST
     C                   KFLD                    SSFEIN
     C*** BEGIN DEL *** 07/09/99 **************************************
     C**********           KFLD           SSMOD
     C*** END   DEL *** 07/09/99 **************************************
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           KFLD           XXNAME
     C                   KFLD                    XXADR1
     C*** END   CHG *** 06/20/00 **************************************
     C*** BEGIN ADD *** 11/03/99 **************************************
     C                   KFLD                    XXMOD
     C*** END   ADD *** 11/03/99 **************************************
     C*
     C* VIEW 3 AND SSFEIN NE ZEROS.
     C*
     C     KEY13         KLIST
     C                   KFLD                    SSFEIN
     C*** BEGIN DEL *** 07/09/99 **************************************
     C**********           KFLD           SSMOD
     C*** END   DEL *** 07/09/99 **************************************
     C                   KFLD                    XXBWC#
     C*** BEGIN ADD *** 11/03/99 **************************************
     C                   KFLD                    XXMOD
     C*** END   ADD *** 11/03/99 **************************************
     C*** BEGIN ADD *** 11/03/99 **************************************
     C*
     C* VIEW 3 AND SSFEIN NE ZEROS.
     C*
     C     KEY14         KLIST
     C                   KFLD                    SSFEIN
     C                   KFLD                    XXMOD
     C*** END   ADD *** 11/03/99 **************************************
     C*
     C*** END   ADD *** 02/19/99 **************************************
      *
     C     KEYP1         KLIST
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           KFLD           XXFEIN
     C**********           KFLD           XXMOD
     C                   KFLD                    X1FEIN
     C                   KFLD                    X1MOD
     C*** END   CHG *** 06/20/00 **************************************
     C*** BEGIN ADD *** 03/23/01 **************************************
     C*
     C     P1KY          KLIST
     C                   KFLD                    PDFEIN
     C                   KFLD                    PDMOD
     C*** END   ADD *** 03/23/01 **************************************
      *
     C     SIKY          KLIST
     C                   KFLD                    KKP1ST
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKSPCD
      *
      *          GENERIC TABLE FILE
      *          ------------------------------------------------------
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXID
      *
     C     *LIKE         DEFINE    GTTBID        XXID
     C     *LIKE         DEFINE    SIST          KKP1ST
     C     *LIKE         DEFINE    SIPRTY        KKTYPE
     C     *LIKE         DEFINE    SISPCD        KKSPCD
      *
      *
      *          M A I N     L O G I C
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQTYPE            1
      ***** BEG ADD ******* 07/14/98 **********************************
     C                   PARM                    @@1N4             1            type 1 & 4 only
      ***** END ADD ******* 07/14/98 **********************************
      *
     C*** BEG ADD *** 09/28/98 ****************************************
     C                   PARM                    QQ1PRV            1            Y=subset on fein/mod
     C*** END ADD *** 09/28/98 ****************************************
     C*** BEGIN ADD *** 04/06/00 **************************************
     C                   PARM                    QQPRIV            1 0          VIEW
     C*** END   ADD *** 04/06/00 **************************************
     C*** BEGIN ADD *** 01/10/02 **************************************
     C                   PARM                    QQPST             2            ADJUDICATION ST
     C*** END   ADD *** 01/10/02 **************************************
     C*** BEGIN ADD *** 04/12/18 **************************************
     C                   PARM                    QQRRN                          ADJUDICATION ST
     C*** END   ADD *** 04/12/18 **************************************
     C                   SELECT
     C     @@CALL        WHENEQ    '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
     C     @@CALL        WHENEQ    '2'
     C                   MOVE      *ON           *IN19                          CALL FROM PGM
     C                   MOVE      'A'           SCSSTA                         SUBSET STATUM
     C                   ENDSL
      *
     C*** BEG ADD *** 11/02/09 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C     CLTID         IFEQ      '139'
     C                   MOVE      '1'           *IN69
     C                   ELSE
     C                   MOVE      '0'           *IN69
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
      *
     C     *LIKE         DEFINE    SCPSTB        SVPSTB
     C     *LIKE         DEFINE    SCPSTC        SVPSTC
      *
     C/COPY SCOPYWW01C
      *
     C*** BEGIN ADD *** 11/02/98 **************************************
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C*
     C     SSFEIN        WHENNE    *ZEROS
     C   21              Z-ADD     DBRRNE        dbrrn
     C   22              Z-ADD     DBRRN2        dbrrn2
     C   23              Z-ADD     DBRRNF        dbrrn3
     C   24              Z-ADD     DBRRNG        dbrrn4
     C     SCSDBA        WHENEQ    'Y'
     C   21              Z-ADD     DBRRNH        dbrrn
     C   22              Z-ADD     DBRRNI        dbrrn2
     C   23              Z-ADD     DBRRNJ        dbrrn3
     C   24              Z-ADD     DBRRNK        dbrrn4
     C     SCSPAR        WHENNE    *BLANKS
     C   21              Z-ADD     DBRRNA        dbrrn
     C   22              Z-ADD     DBRRNB        dbrrn2
     C   23              Z-ADD     DBRRNC        dbrrn3
     C   24              Z-ADD     DBRRND        dbrrn4
     C                   OTHER
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END   ADD *** 11/02/98 **************************************
      **********************************************************************
      *  CHKSSM
      **********************************************************************
     C     CHKSSM        BEGSR
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSNAM        ORNE      *BLANK
     C     SCSDBA        ORNE      *BLANK
     C     SCSTY1        ORNE      *BLANK
     C     SCSTY2        ORNE      *BLANK
     C     SCSADR        ORNE      *BLANK
     C     SCSPAR        ORNE      *BLANK
     C     SCSSPC        ORNE      *BLANK
     C     SCSCTY        ORNE      *BLANK
     C     SCSPST        ORNE      *BLANK
     C     SCSZIP        ORNE      *BLANK
     C     SCSPHN        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
      *
     C                   ENDSR
      *
      **********************************************************************
      *  ERRCK1 - Edit OPT screen field
      **********************************************************************
     C     ERRCK1        BEGSR
     C***  BEGIN ADD  ***  11/02/98  *********************************
     C                   MOVE      'Y'           VLDOPT            1
     C***  END   ADD  ***  11/02/98  *********************************
      *
     C                   SELECT
      *
     C     *IN19         WHENEQ    *OFF                                         NOT SELECT MODE
     C     SCSEL         IFNE      '2'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '7'
     C     SCSEL         ANDNE     '8'
     C     SCSEL         ANDNE     '9'
     C     SCSEL         ANDNE     '14'
     C     SCSEL         ANDNE     '20'
     C                   SETON                                        899005
     C                   ENDIF
     C*** BEGIN ADD *** 09/29/10 **************************************
     C*** BEGIN CHG ****** 12/13/11 ************************************
     C*********  CLTID     IFEQ '133'
     C     ##LAST        IFEQ      'Y'
     C*** END   CHG ****** 12/13/11 ************************************
     C     SCSEL         ANDEQ     '10'
     C                   SETOFF                                       899005
     C                   ENDIF
     C*** BEGIN ADD *** 09/29/10 **************************************
     C                   SELECT
     C*** BEGIN CHG *** 01/15/98 **************************************
     C*******    SCSEL     WHEQ '7'
     C********** SCSEL     WHEQ '8'
     C*** END CHG *** 01/15/98 ****************************************
     C********** SCPARN    ANDNE'P'
     C**********           MOVE '1'       *IN90
     C**********           MOVE '1'       *IN75
      *
     C*** BEGIN CHG *** 01/15/98 **************************************
     C*******    SCSEL     WHEQ '8'
     C     SCSEL         WHENEQ    '7'
     C     SCTYPE        ANDNE     '1'
     C     SCTYPE        ANDNE     '4'
     C*** END CHG *** 01/15/98 ****************************************
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN76
      *
     C*** BEGIN DEL *** 01/15/98 **************************************
     C********   SCSEL     WHEQ '9'
     C********   SCTYPE    ANDNE'4'
     C********             MOVE '1'       *IN90
     C********             MOVE '1'       *IN77
     C*** END DEL *** 01/15/98 ****************************************
      *
     C                   ENDSL
      *
     C                   OTHER                                                  *IN19 *ON
     C     SCSEL         IFNE      '1'
     C     SCSEL         ANDNE     '5'
     C     SCSEL         ANDNE     '8'
     C     SCSEL         ANDNE     '9'
     C     SCSEL         ANDNE     '14'
     C                   SETON                                        899005
     C                   ENDIF
     C*** BEGIN ADD *** 09/29/10 **************************************
     C*** BEGIN CHG ****** 12/13/11 ************************************
     C********   CLTID     IFEQ '133'
     C     ##LAST        IFEQ      'Y'
     C*** END   CHG ****** 12/13/11 ************************************
     C     SCSEL         ANDEQ     '10'
     C                   SETOFF                                       899005
     C                   ENDIF
     C*** BEGIN ADD *** 09/29/10 **************************************
     C                   ENDSL
     C*** BEGIN ADD *** 08/06/98 **************************************
     C     SCSEL         IFEQ      '8'
     C     SCPARN        ANDEQ     'N'
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF
     C*** END   ADD *** 08/06/98 **************************************
      *
     C                   ENDSR
      *
      **********************************************************************
      *  ERRCK2 - Edit SCR03 Fields
      **********************************************************************
     C     ERRCK2        BEGSR
      *
      *
      *  Process F10 search
      *
     C     *INKJ         CASEQ     '1'           KJDO
     C                   ENDCS
      *
      *
      *
      *  Edit Search Status
      *  Search Status value must be 'A', 'I', or Blank
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSSTA        IFNE      'A'
     C     SCSSTA        ANDNE     'I'
     C*** BEG ADD *** 11/02/09 ****************************************
     C     CLTID         IFNE      '139'
     C     CLTID         OREQ      '139'
     C     SCSSTA        ANDNE     'P'
     C     CLTID         IFNE      '139'
     C*** END ADD *** 11/02/09 ****************************************
     C                   SETON                                        9071
     C*** BEG ADD *** 11/02/09 ****************************************
     C                   ELSE
     C                   SETON                                        9075
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 11/02/09 ****************************************
     C                   ENDIF
     C                   ENDIF
      *
      *  Edit Search DBA Sort flag.
      *  Search DBA Sort flag must be 'N', 'Y', or Blank.
      *
     C                   MOVE      *OFF          *IN97
      *
     C     SCSDBA        IFNE      *BLANK
     C     SCSDBA        IFNE      'Y'
     C     SCSDBA        ANDNE     'N'
     C                   SETON                                        9072
     C*** BEGIN DEL *** 08/31/00 **************************************
     C**********           ELSE
     C********** SCSDBA    IFEQ 'Y'
     C**********           MOVE *ON       *IN97
     C**********           MOVE *ON       *IN21
     C**********           MOVE *OFF      *IN22
     C**********           MOVE *OFF      *IN23
     C**********           ENDIF
     C*** END   DEL *** 08/31/00 **************************************
     C                   ENDIF
     C                   ENDIF
      *
      *  Edit Search Provider Types
      *
     C                   Z-ADD     1             I                 2 0          Type Index
      *
     C                   DO        2             I
     C     TY(I)         IFNE      *BLANK
     C     TY(I)         IFNE      '1'
     C     TY(I)         ANDNE     '4'
      *
      *  Search Provider Type must be '1' or '4'.
      *
     C     I             ADD       72            N                 2 0          Indicator Index
     C                   MOVE      *ON           *IN(N)
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
      *
      *  Edit subset Par/Non/Both
      *
     C     SCSPAR        IFNE      *BLANK
     C     SCSPAR        IFNE      'P'
     C     SCSPAR        ANDNE     'N'
     C                   SETON                                        9062
     C                   ENDIF
     C                   ENDIF
      *
      *
     C                   ENDSR
      *
      **********************************************************************
      *  INZSST
      **********************************************************************
     C     INZSST        BEGSR
     C*** BEGIN ADD *** 02/19/99 **************************************
     C*
     C* IF CALLING PROGRAM HAS SENT SEVEN PARMS AND QQ1PRV IS 'Y'
     C* AND QQFEIN IS NOT BLANKS, THEN SUBESET ON QQFEIN/QQMOD
     C*
     C     Q1PRMS        IFGE      7
     C     QQ1PRV        ANDEQ     'Y'
     C     QQFEIN        ANDNE     *BLANKS
     C*
     C                   Z-ADD     0             SSFEIN
     C                   Z-ADD     0             SSMOD
     C                   MOVE      QQFEIN        SSFEIN
     C                   MOVE      QQMOD         SSMOD
     C*
     C                   ENDIF                                                  Q1PRMS GE 7
     C*** END   ADD *** 02/19/99 **************************************
     C*** BEGIN ADD *** 07/09/99 **************************************
     C*
     C     QQFEIN        IFNE      *BLANKS
     C*
     C                   Z-ADD     *ZEROS        SSFEIN
     C                   MOVE      QQFEIN        SSFEIN
     C                   MOVEL     *BLANKS       QQFEIN
     C*
     C                   ENDIF
     C*
     C     QQTYPE        IFEQ      'P'
     C*** BEGIN ADD *** 10/08/02 **************************************
     C     QQTYPE        OREQ      'N'
     C*** END   ADD *** 10/08/02 **************************************
     C*
     C                   MOVEL     QQTYPE        SCSPAR
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 07/09/99 **************************************
     C*** BEGIN ADD *** 01/10/02 **************************************
     C                   SETOFF                                       16
     C     Q1PRMS        IFGT      8
     C     QQPST         ANDNE     *BLANKS
     C                   MOVEL     QQPST         SCSPST
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 01/10/02 **************************************
     C                   ENDSR
      *
      **********************************************************************
      *  KEYINZ - Initialize Both File Key Fields to Low Values
      **********************************************************************
     C     KEYINZ        BEGSR
     C*** BEGIN ADD *** 01/31/00 **************************************
     C     QQMOD         IFEQ      *BLANKS
     C*** END   ADD *** 01/31/00 **************************************
     C                   MOVE      *LOVAL        XXNAME
     C                   MOVE      *LOVAL        XXFSN
     C                   MOVE      *LOVAL        XXFEIN
     C                   MOVE      *LOVAL        XXMOD
     C*** BEGIN ADD *** 08/31/00 **************************************
     C                   MOVE      *LOVAL        XXADR1
     C*** END   ADD *** 08/31/00 **************************************
     C                   MOVE      *LOVAL        XXBWC#
     C*** BEGIN ADD *** 01/31/00 **************************************
     C                   ELSE
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C*
     C     KEY2          CHAIN     PPD02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     QQTYPE        IFEQ      'P'
     C     PDPARN        ANDEQ     'P'
     C     PDSTAT        ANDEQ     'A'
     C*** BEGIN ADD *** 10/08/02 **************************************
     C     QQTYPE        OREQ      'N'
     C     PDPARN        ANDEQ     'N'
     C     PDSTAT        ANDEQ     'A'
     C*** END   ADD *** 10/08/02 **************************************
     C     QQTYPE        OREQ      ' '
     C     PDSTAT        ANDEQ     'A'
     C*
     C                   MOVEL(P)  PDNAME        XXNAME
     C                   MOVEL(P)  PDFSN         XXFSN
     C*** BEGIN ADD *** 08/31/00 **************************************
     C                   MOVEL(P)  PDADR1        XXADR1
     C*** END   ADD *** 08/31/00 **************************************
     C                   MOVEL(P)  PDBWC#        XXBWC#
     C*
     C                   ELSE
     C*
     C                   MOVE      *LOVAL        XXNAME
     C                   MOVE      *LOVAL        XXFSN
     C                   MOVE      *LOVAL        XXFEIN
     C                   MOVE      *LOVAL        XXMOD
     C*** BEGIN ADD *** 08/31/00 **************************************
     C                   MOVE      *LOVAL        XXADR1
     C*** END   ADD *** 08/31/00 **************************************
     C                   MOVE      *LOVAL        XXBWC#
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       XXNAME
     C                   MOVEL(P)  *BLANKS       XXFSN
     C*** BEGIN ADD *** 08/31/00 **************************************
     C                   MOVEL     *LOVAL        XXADR1
     C*** END   ADD *** 08/31/00 **************************************
     C                   MOVEL(P)  *BLANKS       XXBWC#
     C*** BEGIN ADD *** 04/17/00 **************************************
     C                   MOVE      *LOVAL        XXFEIN
     C                   MOVE      *LOVAL        XXMOD
     C*** END   ADD *** 04/17/00 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/31/00 **************************************
     C                   ENDSR
      *
      **********************************************************************
      *  KEYSET - Assign File Key Field based on *IN20
      **********************************************************************
     C     KEYSET        BEGSR
      *
     C*** BEGIN ADD *** 03/31/09 **************************************
     C*** BEGIN ADD *** 12/21/09 **************************************
     C     Q1PRMS        IFLT      7
     C     QQ1PRV        ORNE      'Y'
     C     QQFEIN        OREQ      *BLANKS
     C*** END   ADD *** 12/21/09 **************************************
     C                   Z-ADD     0             XXFEIN
     C                   Z-ADD     0             XXMOD
     C*** BEGIN ADD *** 12/21/09 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/21/09 **************************************
     C*** END   ADD *** 03/31/09 **************************************
     C   21              MOVEL     SCPSTB        SVPSTB
     C*** BEG CHG *** 03/31/09 ****************************************
     C** 21                MOVELSCPST1    XXNAME
     C   21              MOVEL(P)  SCPST1        XXNAME
     C*** END CHG *** 03/31/09 ****************************************
     C   21              MOVEL     SVPSTB        XXFSN
      *
     C   22              MOVE      SCPSTC        SVPSTC
     C   22              MOVE      SCPST2        XXFEIN
     C   22              MOVE      SVPSTC        XXMOD
      *
     C*** BEGIN ADD *** 08/31/00 **************************************
     C   23              MOVE(P)   SCPST3        XXADR1
     C*** END   ADD *** 08/31/00 **************************************
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***23*****           MOVE SCPST3    XXBWC#
     C   24              MOVE      SCPST4        XXBWC#
     C*** END   CHG *** 08/31/00 **************************************
      *
     C                   ENDSR
      *
      **********************************************************************
      *  KEYSFL - Assign Sub File Fields to File Key Fields
      **********************************************************************
     C     KEYSFL        BEGSR
     C   21              MOVEL     SCNAME        XXNAME
     C   21              MOVEL     SCFSN         XXFSN
     C   21              MOVEL     SCMOD         XXMOD
     C*** BEGIN ADD *** 06/20/00 **************************************
     C   21              Z-ADD     SCFEIN        XXFEIN
     C*** END   ADD *** 06/20/00 **************************************
      *
     C   22              MOVEL     SCFEIN        XXFEIN
     C   22              MOVEL     SCMOD         XXMOD
      *
     C*** BEGIN ADD *** 08/31/00 **************************************
     C   23              MOVEL     SCADR1        XXADR1
     C   23              MOVEL     SCMOD         XXMOD
     C*** END   ADD *** 08/31/00 **************************************
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***23*****           MOVELSCBWC#    XXBWC#
     C***23*****           MOVELSCMOD     XXMOD
     C   24              MOVEL     SCBWC#        XXBWC#
     C   24              MOVEL     SCMOD         XXMOD
     C*** END   CHG *** 08/31/00 **************************************
      *
     C                   ENDSR
      *
      **********************************************************************
      *  MVDBSC - Move File Fields to Screen Fields
      **********************************************************************
     C     MVDBSC        BEGSR
     C*
     C     *IN18         IFEQ      *OFF
     C*
     C                   MOVE      '     '       SCHDL1
     C*
     C                   ENDIF
      *
     C*** BEGIN DEL *** 11/02/98 **************************************
     C***97*****           Z-ADDDBRRN4    SCDBRR
     C*** END   DEL *** 11/02/98 **************************************
     C*** BEGIN ADD *** 11/02/98 **************************************
     C*
     C     VLDOPT        IFEQ      'N'
     C*
     C***** BEGIN DEL **** 03/30/99 ***********************************
     C**********           ELSE
     C**********
     C**********           MOVE 'N'       VLDOPT
     C***** END DEL ****** 03/30/99 ***********************************
     C                   ENDIF                                                  VLDOPT<>Y
     C***  END   ADD  ***  11/02/98  *********************************
      *
     C                   MOVEL     PDBWC#        SCBWC#
     C                   MOVEL     PDBWC#        XXBWC#
      *
      *  Access the Provide Auxiliary file
      *
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           Z-ADDPDFEIN    XXFEIN
     C**********           Z-ADDPDMOD     XXMOD
     C                   Z-ADD     PDFEIN        X1FEIN
     C                   Z-ADD     PDMOD         X1MOD
     C*** END   CHG *** 06/20/00 **************************************
      *
     C     KEYP1         CHAIN     PP101                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        SCP1ST
     C                   MOVE      'N'           SCPARN
     C                   MOVE      *BLANK        SCPNM
     C                   MOVE      *BLANK        SCPCSZ
      *
     C                   ELSE
      *
     C                   MOVE      P1ST          SCP1ST
     C   18              MOVEL     PDPARN        SCPARN
     C  N18              MOVEL     *BLANKS       SCPARN
      *
      *  Assign Physical Name
      *
     C     P1FSN         IFEQ      *BLANKS
     C                   MOVEL     P1NAME        SCPNM
     C                   ELSE
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     P1NAME        QQLAST           56
     C                   MOVEL     P1FSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCPNM
     C                   ENDIF
      *
      *  Assign Physical Address
      *
     C*** BEGIN ADD *** 04/12/01 **************************************
     C                   MOVEL(P)  P1ADR1        SCPAD1
     C                   MOVEL(P)  P1ADR2        SCPAD2
     C*** END   ADD *** 04/12/01 **************************************
     C                   MOVE      *BLANKS       SCPCSZ
     C*** BEGIN DEL *** 04/12/01 **************************************
     C********** P1ADR1    IFNE *BLANKS
     C*** END   DEL *** 04/12/01 **************************************
      *
     C*** Begin add *** 03/09/05 **************************************
     C     P1ADR3        IFEQ      *BLANKS
     C*** End add ***** 03/09/05 **************************************
     C                   MOVEL     P1ZIP         XXZIP1            5
     C*** BEGIN ADD *** 10/12/00 **************************************
     C                   MOVE      P1ZIP         XXZIP2            4
     C*** END   ADD *** 10/12/00 **************************************
     C*** BEGIN DEL *** 04/12/01 **************************************
     C********** P1ADR1    CAT  ',':0     SCPCSZ
     C*** END   DEL *** 04/12/01 **************************************
     C*** BEGIN CHG *** 04/12/01 **************************************
     C********** SCPCSZ    CAT  P1CTY:1   SCPCSZ
     C     SCPCSZ        CAT       P1CTY:0       SCPCSZ
     C*** END   CHG *** 04/12/01 **************************************
     C     SCPCSZ        CAT       ',':0         SCPCSZ
     C     SCPCSZ        CAT       P1PST:1       SCPCSZ
     C*** BEGIN DEL *** 04/12/01 **************************************
     C********** SCPCSZ    CAT  ',':0     SCPCSZ
     C*** END   DEL *** 04/12/01 **************************************
     C*** BEGIN CHG *** 04/12/01 **************************************
     C********** SCPCSZ    CAT  XXZIP1:1  SCPCSZ
     C     SCPCSZ        CAT       XXZIP1:2      SCPCSZ
     C*** END   CHG *** 04/12/01 **************************************
     C*** BEGIN ADD *** 10/12/00 **************************************
     C                   CAT       '-':0         SCPCSZ
     C                   CAT       XXZIP2:0      SCPCSZ
     C*** END   ADD *** 10/12/00 **************************************
      *
     C*** Begin add *** 03/09/05 **************************************
     C                   ELSE
     C                   MOVEL     P1ADR3        SCPCSZ
     C                   ENDIF
     C*** End add ***** 03/09/05 **************************************
     C*** BEGIN DEL *** 04/12/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 04/12/01 **************************************
      *
     C                   ENDIF
      *
      *  Assign Mailing Name
      *
     C*** BEGIN ADD *** 09/07/01 **************************************
     C     VIEW#         IFNE      3
     C     VIEW#         OREQ      3
     C     PDDBA1        ANDNE     'Y'
     C*** END   ADD *** 09/07/01 **************************************
     C     PDFSN         IFEQ      *BLANKS
     C*** BEGIN DEL *** 09/07/01 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C********** PDDBA1    IFEQ 'Y'
     C**********           MOVELPDADR1    SCMNM     P
     C**********           ELSE
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 09/07/01 **************************************
     C                   MOVEL     PDNAME        SCMNM
     C*** BEGIN DEL *** 09/07/01 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C**********           ENDIF                           PDDBA1 = 'Y'
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 09/07/01 **************************************
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C*** BEGIN DEL *** 09/07/01 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C********** PDDBA1    IFEQ 'Y'
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           MOVELPDADR1    QQLAST
     C**********           MOVELPDADR1    SCMNM     P
     C*** END   CHG *** 06/20/00 **************************************
     C**********           ELSE
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 09/07/01 **************************************
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           MOVELPDNAME    QQLAST 56
     C                   MOVEL(P)  PDNAME        QQLAST           56
     C*** END   CHG *** 06/20/00 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C*** BEGIN DEL *** 06/20/00 **************************************
     C**********           ENDIF                           PDDBA1 = 'Y'
     C*** END   DEL *** 06/20/00 **************************************
     C*** END ADD *** 03/25/99 ****************************************
     C                   MOVEL     PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCMNM
     C*** BEGIN DEL *** 09/07/01 **************************************
     C*** BEGIN ADD *** 06/20/00 **************************************
     C**********           ENDIF                           PDDBA1 = 'Y'
     C*** END   ADD *** 06/20/00 **************************************
     C*** END   DEL *** 09/07/01 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 09/07/01 **************************************
     C                   ELSE
     C                   MOVEL(P)  PDADR1        SCMNM
     C                   ENDIF
     C*** END   ADD *** 09/07/01 **************************************
      *
      *  Assign Mailing Address (factored by DBA flag)
      *
     C*** BEGIN DEL *** 09/07/01 **************************************
     C********** PDDBA1    IFEQ 'Y'
     C**********           MOVELPDADR2    XXADR  40
     C**********           ELSE
     C*** END   DEL *** 09/07/01 **************************************
     C                   MOVEL     PDADR1        XXADR            40
     C*** BEGIN DEL *** 09/07/01 **************************************
     C**********           ENDIF
     C*** END   DEL *** 09/07/01 **************************************
      *
     C*** BEGIN DEL *** 04/12/01 **************************************
     C**********
     C**********           MOVE *BLANKS   SCMCSZ
     C********** XXADR     IFNE *BLANKS
     C**********           MOVELPDZIP     XXZIP1  5
     C*** BEGIN ADD *** 10/12/00 **************************************
     C**********           MOVE P1ZIP     XXZIP2  4
     C*** END   ADD *** 10/12/00 **************************************
     C********** XXADR     CAT  ',':0     SCMCSZ
     C********** SCMCSZ    CAT  PDCTY:1   SCMCSZ
     C********** SCMCSZ    CAT  ',':0     SCMCSZ
     C********** SCMCSZ    CAT  PDST:1    SCMCSZ
     C********** SCMCSZ    CAT  ',':0     SCMCSZ
     C********** SCMCSZ    CAT  XXZIP1:1  SCMCSZ
     C*** BEGIN ADD *** 10/12/00 **************************************
     C**********           CAT  '-':0     SCMCSZ
     C**********           CAT  XXZIP2:0  SCMCSZ
     C*** END   ADD *** 10/12/00 **************************************
     C**********           ENDIF
      *
      *  If the Physical Name or Address equals the Mailing Name or Address,
      *  Display 'SAME' instead of repeating the information.
      *
     C********** SCPNM     IFEQ SCMNM
     C**********           MOVEL'SAME'    SCPNM     P
     C**********           ENDIF
      *
     C********** SCPCSZ    IFEQ SCMCSZ
     C**********           MOVEL'SAME'    SCPCSZ    P
     C**********           ENDIF
     C*** END   DEL *** 04/12/01 **************************************
      *
      *  Assign remaining subfile fields
      *
     C                   MOVE      PDTYPE        SCTYPE
     C                   MOVEL     PDSPCD        SCSPCD
     C                   MOVEL     PDSTAT        SCSTAT
     C*** BEG ADD *** 03/25/99 ****************************************
     C*** BEGIN DEL *** 11/05/99 **************************************
     C********** PDDBA1    IFEQ 'Y'
     C*** END   DEL *** 11/05/99 **************************************
     C*** BEGIN CHG *** 11/05/99 **************************************
     C**********           MOVELPDADR1    SCNAME
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           MOVELPDNAME    SCNAME
     C*** BEGIN DEL *** 08/31/00 **************************************
     C********** *IN97     IFEQ *OFF
     C*** END   DEL *** 08/31/00 **************************************
     C                   MOVEL(P)  PDNAME        SCNAME
     C*** BEGIN DEL *** 08/31/00 **************************************
     C**********           ELSE
     C*** END   DEL *** 08/31/00 **************************************
     C*** BEGIN CHG *** 08/31/00 **************************************
     C**********           MOVELPDADR1    SCNAME    P
     C**********           ENDIF
     C                   MOVEL(P)  PDADR1        SCADR1
     C*** END   CHG *** 08/31/00 **************************************
     C*** END   CHG *** 06/20/00 **************************************
     C*** END   CHG *** 11/05/99 **************************************
     C*** BEGIN DEL *** 09/07/01 **************************************
     C*** BEGIN ADD *** 11/05/99 **************************************
     C********** PDDBA1    IFEQ 'Y'
     C*** END   ADD *** 11/05/99 **************************************
     C**********           MOVELPDADR2    SCMAD1
     C**********           MOVE *BLANKS   SCMAD2
     C**********           ELSE
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 09/07/01 **************************************
     C                   MOVEL     PDADR1        SCMAD1
     C                   MOVEL     PDADR2        SCMAD2
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           MOVELPDNAME    SCNAME
     C                   MOVEL(P)  PDNAME        SCNAME
     C*** END   CHG *** 06/20/00 **************************************
     C*** BEGIN DEL *** 09/07/01 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C**********           ENDIF                           PDDBA1 = 'Y'
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 09/07/01 **************************************
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           MOVELPDFSN     SCFSN
     C                   MOVEL(P)  PDFSN         SCFSN
     C*** END   CHG *** 06/20/00 **************************************
     C                   MOVEL     PDFEIN        SCFEIN
     C                   MOVEL     PDMOD         SCMOD
      *
      *  Build City, State, and Zip into 1 field
      *
     C*** BEGIN CHG *** 04/12/01 **************************************
     C**********           MOVE *BLANKS   SCCSZ
     C**********           MOVELPDZIP     XXZIP1  5        MAKE ALPHA ZIP
     C*** BEGIN ADD *** 10/12/00 **************************************
     C**********           MOVE PDZIP     XXZIP2  4        MAKE ALPHA ZIP
     C*** END   ADD *** 10/12/00 **************************************
      *
     C********** PDCTY     IFNE *BLANKS
     C********** SCCSZ     CAT  PDCTY:0   SCCSZ            CITY
     C**********           ENDIF
      *
     C********** PDST      IFNE *BLANKS
     C********** SCCSZ     CAT  ',':0     SCCSZ
     C********** SCCSZ     CAT  PDST:1    SCCSZ            STATE
     C**********           ENDIF
      *
     C********** XXZIP1    IFNE *ZERO
     C********** SCCSZ     CAT  XXZIP1:2  SCCSZ            ZIP CODE
     C*** BEGIN ADD *** 10/12/00 **************************************
     C**********           CAT  '-':0     SCCSZ
     C**********           CAT  XXZIP2:0  SCCSZ
     C*** END   ADD *** 10/12/00 **************************************
     C**********           ENDIF
     C                   MOVE      *BLANKS       SCMCSZ
     C                   MOVEL     PDZIP         XXZIP1            5            MAKE ALPHA ZIP
     C                   MOVE      PDZIP         XXZIP2            4            MAKE ALPHA ZIP
      *
     C*** Begin add *** 03/09/05 **************************************
     C     PDADR3        IFEQ      *BLANKS
     C*** End add ***** 03/09/05 **************************************
      *
     C     PDCTY         IFNE      *BLANKS
     C     SCMCSZ        CAT       PDCTY:0       SCMCSZ                         CITY
     C                   ENDIF
      *
     C     PDST          IFNE      *BLANKS
     C     SCMCSZ        CAT       ',':0         SCMCSZ
     C     SCMCSZ        CAT       PDST:1        SCMCSZ                         STATE
     C                   ENDIF
      *
     C     XXZIP1        IFNE      *ZERO
     C     SCMCSZ        CAT       XXZIP1:2      SCMCSZ                         ZIP CODE
     C                   CAT       '-':0         SCMCSZ
     C                   CAT       XXZIP2:0      SCMCSZ
     C                   ENDIF
     C*** END   CHG *** 04/12/01 **************************************
      *
     C*** Begin add *** 03/09/05 **************************************
     C                   ELSE
     C                   MOVEL     PDADR3        SCMCSZ
     C                   ENDIF
     C*** End add ***** 03/09/05 **************************************
      *
      *  Format FEIN and Mod
      *
     C                   MOVE      *BLANKS       FEINMD
     C                   MOVEL     PDFEIN        FEINMD
     C                   MOVE      PDMOD         FEINMD
      *
      *  Format Name
      *
     C     PDFSN         IFEQ      *BLANKS
     C*** BEGIN DEL *** 08/31/00 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C********** PDDBA1    IFEQ 'Y'
     C**********           MOVELPDADR1    XXNM      P
     C**********           ELSE
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 08/31/00 **************************************
     C                   MOVEL(P)  PDNAME        XXNM             37
     C*** BEGIN DEL *** 08/31/00 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C**********           ENDIF                           PDDBA1 = Y
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 08/31/00 **************************************
     C*
     C                   ELSE
     C*
     C*** BEGIN DEL *** 08/31/00 **************************************
     C*** BEG ADD *** 03/25/99 ****************************************
     C********** PDDBA1    IFEQ 'Y'
     C*** BEGIN CHG *** 06/20/00 **************************************
     C**********           MOVELPDADR1    QQLAST    P
     C**********           MOVELPDADR1    XXNM      P
     C*** END   CHG *** 06/20/00 **************************************
     C**********           ELSE
     C*** END ADD *** 03/25/99 ****************************************
     C*** END   DEL *** 08/31/00 **************************************
     C                   MOVEL(P)  PDNAME        QQLAST           56
     C*** BEG ADD *** 03/25/99 ****************************************
     C*** BEGIN DEL *** 06/20/00 **************************************
     C**********           ENDIF                           PDDBA1 = 'Y'
     C*** END   DEL *** 06/20/00 **************************************
     C*** END ADD *** 03/25/99 ****************************************
     C                   MOVEL(P)  PDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNM
     C*** BEGIN DEL *** 08/31/00 **************************************
     C*** BEGIN ADD *** 06/20/00 **************************************
     C**********           ENDIF                           PDDBA1 = 'Y'
     C*** END   ADD *** 06/20/00 **************************************
     C*** END   DEL *** 08/31/00 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 08/31/00 **************************************
     C                   MOVEL(P)  PDADR1        XXAD1            37
     C*** END   ADD *** 08/31/00 **************************************
      *
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  XXNM          SCDATA
     C                   MOVE      FEINMD        SCDATA
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  FEINMD        SCDATA
     C     SCDATA        CAT       XXNM:2        SCDATA
     C*** BEGIN CHG *** 08/31/00 **************************************
     C     VIEW#         WHENEQ    3
     C                   MOVEL(P)  XXAD1         SCDATA
     C                   MOVE      FEINMD        SCDATA
     C*** END   CHG *** 08/31/00 **************************************
     C*** BEGIN CHG *** 08/31/00 **************************************
     C********** VIEW#     WHEQ 3
     C     VIEW#         WHENEQ    4
     C*** END   CHG *** 08/31/00 **************************************
     C     PDBWC#        IFEQ      *BLANK
     C                   MOVEL(P)  BLKBWC        SCDATA
     C                   ELSE
     C                   MOVEL(P)  PDBWC#        SCDATA
     C                   ENDIF
     C     SCDATA        CAT       XXNM:2        SCDATA
      *
     C                   ENDSL
     C*** BEGIN ADD *** 04/12/01 **************************************
     C*
     C*  If the Physical Name or Address equals the Mailing Name or Address,
     C*  Display 'SAME' instead of repeating the information.
     C*
     C     SCPNM         IFEQ      SCMNM
     C                   MOVEL(P)  'SAME'        SCPNM
     C                   ENDIF
     C*
     C     SCPAD1        IFEQ      SCMAD1
     C                   MOVEL(P)  'SAME'        SCPAD1
     C                   ENDIF
     C*
     C     SCPAD2        IFEQ      SCMAD2
     C     SCMAD2        ANDNE     *BLANKS
     C                   MOVEL(P)  'SAME'        SCPAD2
     C                   ENDIF
     C*
     C     SCPCSZ        IFEQ      SCMCSZ
     C                   MOVEL(P)  'SAME'        SCPCSZ
     C                   ENDIF
     C*** END   ADD *** 04/12/01 **************************************
     C***** Beg Add *** 08/20/14 **************************************
     C/free

       // load NPI# value
        scNPI# = P1N#02;

       // do not display NPI# if blank
        if P1N#02 = *blanks;
          dspNPI# = *off;
        else;
          dspNPI# = *on;
        endif;

      /end-free
     C***** End Add *** 08/20/14 **************************************
      *
      /Free
       // Beg Add *** 04/12/18 *****************************************
       scBPnam  = *blanks;                         //Clear 837 fields
       scBPadr1 = *blanks;
       scBPadr2 = *blanks;
       scBPadr3 = *blanks;
       scBPhdg  = *blanks;
       scPFhdg  = *blanks;
       VisBPhdg= *off;
       VisPFhdg   = *off;
       DisblBPhdg = *on;
       DisblPFhdg = *on;
       scPFadr1 = *blanks;
       scPFadr2 = *blanks;
       scPFadr3 = *blanks;
       scPFadr4 = *blanks;
       xxZip    = *Zeros;

       If Q1PRMS > 9 and QQRRN > *Zeros and
          isModEnabled(Mod837p) and isModEnabled(Mod837i);

          //Retrieve Providers info for Action Panel
          xxRRN = QQrrn;      //Get EOB
          chain xxRRN FAEOP;
          If %found(FAEOP);


             //Check if 837 Claim
             Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) FACLML8;
             If %found(FACLML8);
                Chain (CLMicntl:CLMgctl:CLMtscntl:CLMbhttid) FAHDRL;

               //Populate 837 fields

                If HDbpFnm = *blanks;           //Billing Provider Name
                   scBPnam = %Trim(HDbpNm);
                Else;
                   scBPnam = %Trim(HDbpFnm) + ' ' + %Trim(HDbpNm);
                Endif;

                If scBPnam > *blanks and HDbpSuff > *blanks;
                   scBPnam = %Trim(scBPnam) + ', ' + %Trim(HDbpSuff);
                Endif;


                scBPadr1 = HDbpAdr1;       //Billing Provider Address
                scBPadr2 = HDbpAdr2;
                scBPadr3 = *blanks;

                Monitor;
                xxZip = %Dec(HDbpPost:9:0);
                On-Error;
                xxZip = 0;
                Endmon;

                scBPhdg = 'Billing Provider Information';
                VisBPhdg= *on;
                DisblBPhdg = *off;

                If scBPadr2 <> *blanks;
                   If %Subst(%EditC(xxZip:'X'):6:4)='0000';
                      If HDbpCty > *blanks and HDbpST > *blanks;
                         scBPadr3=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                         %Subst(%EditC(xxZip:'X'):1:5);
                      Endif;
                   Else;
                      If HDbpCty > *blanks and HDbpST > *blanks;
                         scBPadr3=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                         %Subst(%EditC(xxZip:'X'):1:5) + '-' +
                         %Subst(%EditC(xxZip:'X'):6:4);
                      Endif;
                   EndIf;
                Else;
                   If %Subst(%EditC(xxZip:'X'):6:4)='0000';
                      If HDbpCty > *blanks and HDbpST > *blanks;
                         scBPadr2=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                         %Subst(%EditC(xxZip:'X'):1:5);
                      EndIf;
                   Else;
                      If HDbpCty > *blanks and HDbpST > *blanks;
                         scBPadr2=%Trim(HDbpCty) + ', ' + HDbpST + ' ' +
                         %Subst(%EditC(xxZip:'X'):1:5) + '-' +
                         %Subst(%EditC(xxZip:'X'):6:4);
                      EndIf;
                   EndIf;
                EndIf;


                Select;
                When EObtyp = 'H';                    // Pay to Provider Info
                   Chain (CLMicntl:CLMgctl:CLMtscntl:CLMbhttid) FAHDRL;
                   If %found(FAHDRL) and HDPPRadr1 > *blank;
                      scPFhdg = 'Pay to Provider Information';
                      VisPFhdg= *on;
                      DisblPFhdg = *off;
                      scPFadr1 = HDPPRadr1;
                      scPFadr2 = HDPPRadr2;
                      scPFadr3 = *blanks;
                      scPFadr4 = *blanks;

                      If scPFadr2 <> *blanks;
                         If %Subst(HDPPRpost:6:4)='0000' or
                            %Subst(HDPPRpost:6:4)= *blanks;
                            If HDPPRcty > *blanks and HDPPRst > *blanks;
                               scPFadr3=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                               %Subst(HDPPRpost:1:5);
                            Endif;
                         Else;
                            If HDPPRcty > *blanks and HDPPRst > *blanks;
                               scPFadr3=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                               %Subst(HDPPRpost:1:5) + '-' +
                               %Subst(HDPPRpost:6:4);
                            Endif;
                         EndIf;
                      Else;
                         If %Subst(HDPPRpost:6:4)='0000' or
                            %Subst(HDPPRpost:6:4)= *blanks;
                            If HDPPRcty > *blanks and HDPPRst > *blanks;
                               scPFadr2=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                               %Subst(HDPPRpost:1:5);
                            Endif;
                         Else;
                            If HDPPRcty > *blanks and HDPPRst > *blanks;
                               scPFadr2=%Trim(HDPPRcty) + ', ' + HDPPRst + ' ' +
                               %Subst(HDPPRpost:1:5) + '-' +
                               %Subst(HDPPRpost:6:4);
                            Endif;
                         EndIf;
                      EndIf;
                   EndIf;

                When EObtyp = 'N' and CLMSFnm > *blanks; // Service Facility Location
                   VisPFhdg = *on;
                   DisblPFhdg = *off;
                   scPFhdg = 'Service Facility Location';
                   scPFadr1 = CLMSFnm;
                   scPFadr2 = CLMSFadr1;
                   scPFadr3 = CLMSFadr2;
                   scPFadr4 = *blanks;

                   If scPFadr3 <> *blanks;
                      If %Subst(CLMSFpost:6:4)='0000' or
                         %Subst(CLMSFpost:6:4)= *blanks;
                         If CLMSFcty > *blanks and CLMSFst > *blanks;
                            scPFadr4=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                            %Subst(CLMSFpost:1:5);
                         Endif;
                      Else;
                         If CLMSFcty > *blanks and CLMSFst > *blanks;
                            scPFadr4=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                            %Subst(CLMSFpost:1:5) + '-' +
                            %Subst(CLMSFpost:6:4);
                         Endif;
                      EndIf;
                   Else;
                      If %Subst(CLMSFpost:6:4)='0000' or
                         %Subst(CLMSFpost:6:4)= *blanks;
                         If CLMSFcty > *blanks and CLMSFst > *blanks;
                            scPFadr3=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                            %Subst(CLMSFpost:1:5);
                         Endif;
                      Else;
                         If CLMSFcty > *blanks and CLMSFst > *blanks;
                            scPFadr3=%Trim(CLMSFcty) + ', ' + CLMSFst + ' ' +
                            %Subst(CLMSFpost:1:5) + '-' +
                            %Subst(CLMSFpost:6:4);
                         Endif;
                      EndIf;
                   EndIf;
                Endsl;

             Endif;
          Endif;
       Endif;
       // End Add *** 04/12/18 ****************************************
      /End-Free
     C                   ENDSR
      *
      *
     C*** BEGIN DEL *** 01/15/98 **************************************
      **********************************************************************
      ***OPT7*- Process Option ' 7' (Prof/Phys Info)
      **********************************************************************
     C********   OPT8      BEGSR
      ********
     C********   SCTYPE    IFEQ '1'
      ********
      ********
     C********             EXSR REFRSR
      ********
     C********             MOVE PDFEIN    QQFEIN  9
     C********             MOVE PDMOD     QQMOD   3
     C********             CALL 'F0040R'
     C********             PARM           SCPRDS
     C********             PARM           QQFEIN
     C********             PARM           QQMOD
     C********             PARM *BLANK    QQRTC
      ********
     C********             ENDIF
      ********
     C********             EXSR REFRSR
      ********
     C********             ENDSR
      ********
      ********
      **********************************************************************
      ***OPT9*- Process Option ' 9' (Fac/Hosp Info)
      **********************************************************************
     C********   OPT9      BEGSR
      ********
     C********   SCTYPE    IFEQ '4'
      ********
     C********             EXSR REFRSR
      ********
     C********             MOVE PDFEIN    QQFEIN  9
     C********             MOVE PDMOD     QQMOD   3
     C********             CALL 'F0041R'
     C********             PARM           SCPRDS
     C********             PARM           QQFEIN
     C********             PARM           QQMOD
     C********             PARM *BLANK    QQRTC
      ********
     C********             ENDIF
      ********
     C********             EXSR REFRSR
      ********
     C********             ENDSR
     C*** END DEL *** 01/15/98 ****************************************
     C*** BEGIN ADD *** 01/15/98 **************************************
      **********************************************************************
      *  OPT7 - Process Option ' 7' (Additional Detail Info)
      **********************************************************************
     C     OPT7          BEGSR
     C*
     C     SCFEIN        IFNE      *ZEROS
     C*** BEGIN ADD *** 11/02/98 **************************************
     C*
     C* SET THE MODE FOR F0040R AND F0041R.
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C                   MOVEL     'C'           QQMODE            1
     C*
     C                   ELSE
     C*
     C                   MOVEL     'D'           QQMODE
     C*
     C                   ENDIF
     C*** END   ADD *** 11/02/98 **************************************
     C*
     C     SCTYPE        IFEQ      '1'
     C                   EXSR      REFRSR
     C                   MOVE      PDFEIN        QQFEIN            9
     C                   MOVE      PDMOD         QQMOD             3
     C                   CALL      'F0040R'
     C                   PARM                    SCPRDS
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM      *BLANK        QQRTC
     C*** BEGIN ADD *** 11/02/98 **************************************
     C                   PARM                    QQMODE
     C*** END   ADD *** 11/02/98 **************************************
     C                   ENDIF
     C*
     C     SCTYPE        IFEQ      '4'
     C                   EXSR      REFRSR
     C                   MOVE      PDFEIN        QQFEIN            9
     C                   MOVE      PDMOD         QQMOD             3
     C                   CALL      'F0041R'
     C                   PARM                    SCPRDS
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM      *BLANK        QQRTC
     C*** BEGIN ADD *** 11/02/98 **************************************
     C                   PARM                    QQMODE
     C*** END   ADD *** 11/02/98 **************************************
     C                   ENDIF
      *
     C                   EXSR      REFRSR
     C*
     C                   ELSE
     C*
     C                   SETON                                        8390
     C*
     C                   ENDIF
      *
     C                   ENDSR
     C*** END ADD *** 01/15/98 ****************************************
      *
      **********************************************************************
      *  OPT8 - Process Option ' 8' (Network Info)
      **********************************************************************
     C*** BEGIN CHG *** 01/15/98 **************************************
     C********   OPT7      BEGSR
     C     OPT8          BEGSR
     C*** END CHG *** 01/15/98 ****************************************
      *
      *
     C*** BEGIN DEL *** 01/15/98 **************************************
     C********   PDPARN    IFEQ 'P'
     C********             ENDIF
     C*** END DEL *** 01/15/98 ****************************************
     C*** BEGIN ADD *** 01/15/98 **************************************
     C                   EXSR      REFRSR
      *
     C                   MOVE      PDFEIN        QQFEIN            9
     C                   MOVE      PDMOD         QQMOD             3
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C                   MOVEL     ' '           ##CALL
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 08/06/98 **************************************
     C*
     C********** SCPARN    IFEQ 'P'
     C*** END   DEL *** 08/06/98 **************************************
     C*
     C                   CALL      'N0037R'
     C                   PARM                    ##CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM      *BLANKS       QQNWCD            5
     C                   PARM      *BLANKS       QQCNT#           15
     C*
     C                   EXSR      REFRSR
     C*** END ADD *** 01/15/98 ****************************************
     C*** BEGIN DEL *** 08/06/98 **************************************
     C**********           ELSE
     C*
     C**********           SETON                     7790
     C*
     C**********           ENDIF
     C*** END   DEL *** 08/06/98 **************************************
     C*
     C                   ENDSR
      *
      *
      *
     C**********************************************************************
     C*  OPT9 - Process Option ' 9' (State Info)
     C**********************************************************************
     C     OPT9          BEGSR
     C*
     C*
     C* DETERMINE THE MODE THAT THE NEXT PROGRAMS ARE TO BE CALLED.
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C                   MOVEL     'M'           ##CALL
     C*
     C                   ELSE
     C*
     C                   MOVEL     'L'           ##CALL
     C*
     C                   ENDIF
     C*
     C* MOVE VALUES INTO KEY FIELDS TO CHAIN OUT TO FMSIP
     C*
     C                   MOVEL     SCP1ST        KKP1ST
     C                   MOVEL     SCTYPE        KKTYPE
     C*
     C                   SELECT
     C*
     C     SCTYPE        WHENEQ    '1'
     C                   MOVEL     SCSPCD        KKSPCD
     C*
     C     SCTYPE        WHENEQ    '4'
     C                   MOVEL     *BLANKS       KKSPCD
     C*
     C                   ENDSL
     C*
     C     SIKY          CHAIN     PSI01                              99
     C*
     C* IF CHAIN IS SUCCESSFUL, THEN F0042C SHOULD BE CALLED.
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   CALL      'F0042R'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FMP2P
     C                   PARM      SCDBRR        ##RRN2            9 0          FMPDP
     C*
     C                   ELSE
     C*
     C                   SETON                                        81
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 09/29/20 **************************************
     C**********************************************************************
     C*  OPT10- Process Option '10' (W/W PROVIDER SALES TAX)
     C**********************************************************************
     C     OPT10         BEGSR
     C*** BEGIN CHG ****** 12/13/11 ************************************
     C*********  CLTID     IFEQ '133'
     C     ##LAST        IFEQ      'Y'
     C*** END   CHG ****** 12/13/11 ************************************
     C                   CALL      'F2374R'
     C                   PARM      ' '           @@CALL
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*
     C*
     C                   ENDIF
     C                   ENDSR
     C*** END   ADD *** 09/29/20 **************************************
      **********************************************************************
      *  OPT14 - Process Option '14' (DARTS)
      **********************************************************************
     C     OPT14         BEGSR
     C*
     C                   MOVE      SCFEIN        #@FEIN
     C                   MOVE      SCMOD         #@MOD
     C*
     C                   CALL      'FA706R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCDK        #@WCDK            1
     C                   PARM                    #@FEIN            9
     C                   PARM                    #@MOD             3
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C**********************************************************************
      *  OPT20 - Process Option '20' (Activate / Inactivate)
      **********************************************************************
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   EXSR      REFRSR
      *
     C                   ENDSR
      *
      **********************************************************************
      *  OTHFKS - Process Other Function Keys
      **********************************************************************
     C     OTHFKS        BEGSR
      *
     C                   EXSR      ENTRDO
      *
     C                   ENDSR
      *
      *
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
      *
     C     STRING        BEGSR
      *
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
      *
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
      *
     C                   ENDSR                                                  STRING
      *
      *
      **********************************************************************
      *  KJDO - Process F10 Searches
      **********************************************************************
     C     KJDO          BEGSR
      *
      * SCR03 Subset - Physical State search
      *
     C     HFLD          IFEQ      'SCSPST'
      *
     C                   CALL      'SY601R'      STSPL
      *
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        8190
     C                   ELSE
     C     ##RRN         CHAIN     PST02                              99
     C                   MOVEL(P)  STKEY         SCSPST
     C                   SETON                                        8290
     C                   ENDIF
      *
     C                   ENDIF                                                  SCSPST
      *
      *
      * SCR03 Subset - Provider type search 1
      *
      ***** BEG CHG ******* 07/14/98 **********************************
     C*********  HFLD      IFEQ 'SCSTY1'
     C*** BEGIN CHG *** 03/23/01 **************************************
     C**N65***** HFLD      IFEQ 'SCSTY1'
     C     HFLD          IFEQ      'SCSTY1'
     C*** END   CHG *** 03/23/01 **************************************
      ***** END CHG ******* 07/14/98 **********************************
      *
     C                   MOVEL(P)  SCSTY1        XXKEY
     C                   MOVEL(P)  'PRTY'        XXTBID
     C                   MOVEL     OPTNM         QQPRDS
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      *BLANK        QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM      *BLANK        QQRTC             1
      *
     C     QQRTC         IFEQ      '*'
     C     XXKEY         ANDNE     *BLANKS
     C                   MOVEL(P)  XXKEY         SCSTY1
     C                   MOVE      '1'           *IN79
     C                   ELSE
     C                   MOVE      '1'           *IN78
     C                   ENDIF
      *
     C                   ENDIF                                                  SCSTY1
      *
      * SCR03 Subset - Provider type search 2
      *
      ***** BEG CHG ******* 07/14/98 **********************************
     C********** HFLD      IFEQ 'SCSTY2'
     C*** BEGIN CHG *** 03/23/01 **************************************
     C**N65***** HFLD      IFEQ 'SCSTY2'
     C     HFLD          IFEQ      'SCSTY2'
     C*** END   CHG *** 03/23/01 **************************************
      ***** END CHG ******* 07/14/98 **********************************
      *
     C                   MOVEL(P)  SCSTY2        XXKEY
     C                   MOVEL(P)  'PRTY'        XXTBID
     C                   MOVEL     OPTNM         QQPRDS
     C                   CALL      'F2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      *BLANK        QQCALL            1
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM      *BLANK        QQRTC             1
      *
     C     QQRTC         IFEQ      '*'
     C     XXKEY         ANDNE     *BLANKS
     C                   MOVEL(P)  XXKEY         SCSTY2
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C                   MOVE      '1'           *IN79
     C                   ENDIF
      *
     C                   ENDIF                                                  SCSTY2
      *
      * SCR03 Subset - Specialty code search
      *
     C     HFLD          IFEQ      'SCSSPC'
      *
     C                   MOVEL(P)  SCSSPC        SPKEY            10
     C                   CALL      'F2335R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    SPKEY
     C                   PARM                    QQRTC             1
      *
     C     QQRTC         IFEQ      '*'
     C     SPKEY         ANDNE     *BLANK
     C                   MOVEL(P)  SPKEY         SCSSPC
     C                   ELSE
     C                   MOVE      '1'           *IN63
     C                   END
      *
     C                   ENDIF                                                  SCSSPC
      *
     C                   MOVE      '1'           *IN90
      *
     C                   ENDSR
      *
      *
      **********************************************************************
      *  OTHOPT - Process Other Options.
      **********************************************************************
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '1'           OPT1
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '20'          OPT20
     C*** BEGIN ADD *** 09/29/10 **************************************
     C     SCSEL         CASEQ     '10'          OPT10
     C*** END   ADD *** 09/29/10 **************************************
     C                   ENDCS
     C                   ENDSR
      *
      **********************************************************************
      *  PGMEND
      **********************************************************************
     C     PGMEND        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C     QQRTCD        ANDNE     '*'
     C                   MOVE      *OFF          ALWF12            1            RCLRSC
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   ELSE
     C                   MOVE      'B'           QQRTCD
     C                   ENDIF
     C                   ENDIF
     C*
      /Free
       //*** Begin Add *** 04/12/18 *************************************
       //Open Files
        If %Open(FAEOP);
           Close FAEOP;
        Endif;

        If %Open(FACLML8);
           Close FACLML8;
        Endif;

        If %Open(FAHDRL);
           Close FAHDRL;
        Endif;
       //*** End   Add *** 04/12/18 *************************************
      /End-Free
     C                   SETON                                        LR
      *
     C                   ENDSR
      *
      **********************************************************************
      *  PGMSTR - Assign Initial field values
      **********************************************************************
     C     PGMSTR        BEGSR
     C*** BEGIN ADD *** 09/29/10 **************************************
     C*** BEGIN DEL ****** 12/13/11 ************************************
     C* This logic moved to after SA001R call
     C********** CLTID     IFNE '133'
     C**********           MOVE '1'       XXOPFN
     C**********           MOVE 'NNN'     XXMIS
     C**********           MOVE '10'      XXNMBR
     C**********           EXSR UPDOFA
     C**********           ENDIF
     C*** END   DEL ****** 12/13/11 ************************************
     C*** END   ADD *** 09/29/10 **************************************
      *
      *  Initialize Default Indicator values
      *
     C                   MOVE      *OFF          *IN09                          NOT INQUIRY
     C                   MOVE      *OFF          *IN19                          NOT CALL FROM PGM
      *
      *  Determine what function program is performing.
      *
     C                   SELECT
     C     @@CALL        WHENEQ    '1'
     C                   MOVE      *ON           *IN09                          INQUIRY
     C     @@CALL        WHENEQ    '2'
     C                   MOVE      *ON           *IN19                          CALL FROM PGM
     C                   MOVE      'A'           SCSSTA                         SUBSET ACTIVE STATUS
     C                   OTHER
      *                    "Work With" Function
     C                   ENDSL
      *
      *  WCMS / WCMC System Control flags assigned
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILLS
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANK        ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANK        ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANK        ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANK        ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANK        ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANK        ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANK        ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANK        ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANK        ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANK        ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANK        ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANK        ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANK        ##CLT#            3            Client Number
30   C                   PARM      *BLANK        ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANK        ##DCF             1            Data Center
32   C                   PARM      *BLANK        ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANK        ##DEIF            1            DE/Image Flag
34   C                   PARM      *BLANK        ##AURE            1            Auto-Reinstate
35   C                   PARM      *BLANK        ##CRED            1            Credit Module
36   C                   PARM      *BLANK        ##IDRT            1            Idarts Module
37   C                   PARM      *BLANK        ##CPRV            1            Create Provider
38   C                   PARM      *BLANK        ##EFT             1            EFT Flag
39   C                   PARM      *BLANK        ##GOVR            1            GO VERSION
40   C                   PARM      *BLANK        ##NCDC            1            NCCI data
41   C                   PARM      *BLANK        ##SPLT            1            SPLIT
42   C                   PARM      *BLANK        ##LAST            1            LA PARISH SALES TAX
     C*****
     C* Do not allow option 10 if LA parish sales tax flag is not set
     C     ##LAST        IFNE      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '10'          XXNMBR
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END   ADD ****** 12/13/11 ************************************
     C*** BEGIN ADD *** 04/06/00 **************************************
     C*
     C* ESTABLISH THE INITIAL VIEW
     C*
     C     Q1PRMS        IFGT      7
     C*
     C     QQPRIV        IFNE      *ZEROS
     C*
     C                   Z-ADD     QQPRIV        VIEW#
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 04/06/00 **************************************
      *
     C     QQFL01        IFEQ      'N'
     C                   MOVE      *ON           *IN98                          WCMC Standalone
      *
      *  ACTIVATE ADD MODE
      *
     C                   MOVE      OFD(14)       DATA
     C                   MOVE      'Y'           FM
     C                   MOVE      'N'           FI
     C                   MOVE      DATA          OFD(14)
      *
     C                   ENDIF
      *
      *  Check flag for display/not of option 7
      *
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           READ SFB01                    60
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL06    IFEQ 'Y'
     C     ##NTWK        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C                   MOVE      OFD(7)        DATA
     C                   MOVE      'Y'           FM
     C                   MOVE      'Y'           FI
     C                   MOVE      DATA          OFD(7)
     C                   ENDIF
      *
      *
     C*** BEGIN CHG *** 03/23/01 **************************************
     C**********           Z-ADD10        SF1PAG  50
     C                   Z-ADD     12            SF1PAG            5 0
     C*** END   CHG *** 03/23/01 **************************************
      *
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
      *
      *          Were 5 or more parameters passed?
      *             Indicator 61 indicates service providers only
      *          ------------------------------------------------------
     C                   SETOFF                                       61
     C     Q1PRMS        IFGE      5
      *
      *             Has Servicing provider search been requested?
      *             ---------------------------------------------------
     C     QQTYPE        IFEQ      'S'
      *
      *                Yes: Set on indicator 61
      *                     Read Provider type (PRTY) table
      *                ------------------------------------------------
     C                   OPEN      FTGTL
     C                   MOVEL(P)  'PRTY'        XXID
     C                   Z-ADD     1             I1                3 0
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         DOWEQ     *OFF
     C     I1            ANDLE     100
      *
      *                   If servicing provider: Save provider type
      *                   ---------------------------------------------
     C     GTFG01        IFEQ      'Y'
     C                   MOVEL     GTKEY         TYP(I1)
     C                   ADD       1             I1
     C                   ENDIF                                                  FG01 = Y
     C     GTKY          READE     PGT01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C                   SETON                                        61
     C                   CLOSE     FTGTL
     C                   ENDIF                                                  TYPE = S
     C                   ENDIF                                                  PARM => 5
     C*
     C* IF LINKED WITH WCMS REMOVE F6=ADD ELSE LOAD F6=ADD.
     C*
     C     QQFL01        IFEQ      'Y'
     C*
     C                   MOVEL     CLRFKY        OFD(14)
     C*
     C                   ELSE
     C*
     C                   MOVEL     ADDFKY        OFD(14)
     C*
     C                   ENDIF
     C*
     C     ##NTWK        IFEQ      'Y'
     C*
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C                   MOVEL     CLRFKY        OFD(8)
     C                   SETOFF                                       18
     C*
     C                   ENDIF
     C*
      *
      ***** BEG ADD ******* 07/14/98 **********************************
      *
      *  Set indicator to restrict access to records with
      *  provider types of either '1' or '4' only.
      *
     C     Q1PRMS        IFGT      5
     C     @@1N4         IFEQ      'Y'                                          1 AND 4 ONLY
     C     @@CALL        ANDEQ     '2'                                          SELECT MODE
     C                   MOVEL(P)  '1'           SCSTY1
     C                   MOVEL(P)  '4'           SCSTY2
     C                   SETON                                        65
     C                   ENDIF                                                  @@1N4
     C                   ENDIF
      *
      ***** END ADD ******* 07/14/98 **********************************
     C***** BEGIN DEL **** 03/30/99 ***********************************
     C*** BEGIN ADD *** 11/02/98 **************************************
     C**********           MOVEL'N'       VLDOPT
     C*** END   ADD *** 11/02/98 **************************************
     C***** END DEL ****** 03/30/99 ***********************************
      *
    C*** Beg Add *** MC367 *** 11/13/12 ******************************
      /Free
      /If Not Defined(WEB)
           XXOpfn = '1';  //These two fields remain the same for all options
           XXMIS = 'NNN'; //being conditioned here
       //Disable option 05
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable option 09
           XXNmbr = '09';
           Exsr UPDOFA;
       //Disable option 14
           XXNmbr = '14';
           Exsr UPDOFA;
      /EndIf
      /End-Free
      /Free
       //**** Begin 04/12/18 *****************************************
       //Open Files
        If isModEnabled(Mod837P) and isModEnabled(Mod837I) and
           Q1PRMS > 9 and QQRRN > *Zeros ;

           If not %Open(FAEOP);
              Open FAEOP;
           Endif;

           If not %Open(FACLML8);
              Open FACLML8;
           Endif;

           If not %Open(FAHDRL);
              Open FAHDRL;
           Endif;
        Endif;
       //**** End   04/12/18 *****************************************
      /End-Free
    C*** End Add *** MC367 *** 11/13/12 ********************************
     C                   ENDSR
      *
      **********************************************************************
      *  READ - Access PROVIDER Demographic file based on *IN20
      **********************************************************************
     C     READ          BEGSR
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C                   MOVE      'N'           VLDOPT
     C***** END ADD ****** 03/30/99 ***********************************
     C*
     C*** BEGIN CHG *** 11/02/98 **************************************
     C                   SELECT
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     SSFEIN        WHENNE    *ZEROS
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY11     READEPPD09                    95
     C   21KEY11         READE     PPD09                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C   22KEY11         READE     PPD02                                  95
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY11     READEPPD10                    95
     C***23***** KEY11     READEPPD11                    95
     C   23KEY11         READE     PPD10                                  95
     C   24KEY11         READE     PPD11                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C*** END   ADD *** 02/19/99 **************************************
     C*** BEGIN ADD *** 08/31/00 **************************************
     C     SCSDBA        WHENEQ    'Y'
     C   21              READ      PPD12                                  95
     C   22              READ      PPD13                                  95
     C   23              READ      PPD14                                  95
     C   24              READ      PPD15                                  95
     C*** END   ADD *** 08/31/00 **************************************
     C*
     C     SCSPAR        WHENNE    *BLANKS
      *
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97**           READ PPD05                    95
     C   21              READ      PPD05                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C   22              READ      PPD06                                  95
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97**           READ PPD07                    95
     C***23*****           READ PPD08                    95
     C   23              READ      PPD07                                  95
     C   24              READ      PPD08                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   OTHER
     C*
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97**           READ PPD01                    95
     C   21              READ      PPD01                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C   22              READ      PPD02                                  95
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97**           READ PPD04                    95
     C***23*****           READ PPD03                    95
     C   23              READ      PPD03                                  95
     C   24              READ      PPD04                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   ENDSL
     C*** END   CHG *** 11/02/98 **************************************
     C                   EXSR      ASSRRN
      *
     C                   ENDSR
      *
      **********************************************************************
      *  READP - Access PROVIDER Demographic file based on *IN20
      **********************************************************************
     C     READP         BEGSR
     C***** BEGIN ADD **** 03/30/99 ***********************************
     C                   MOVE      'N'           VLDOPT
     C***** END ADD ****** 03/30/99 ***********************************
     C*
     C*** BEGIN CHG *** 11/02/98 **************************************
     C                   SELECT
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     SSFEIN        WHENNE    *ZEROS
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY11     REDPEPPD09                    95
     C   21KEY11         READPE    PPD09                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C   22KEY11         READPE    PPD02                                  95
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY11     REDPEPPD10                    95
     C***23***** KEY11     REDPEPPD11                    95
     C   23KEY11         READPE    PPD10                                  95
     C   24KEY11         READPE    PPD11                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C*** BEGIN ADD *** 08/31/00 **************************************
     C     SCSDBA        WHENEQ    'Y'
     C   21              READP     PPD12                                  95
     C   22              READP     PPD13                                  95
     C   23              READP     PPD14                                  95
     C   24              READP     PPD15                                  95
     C*** END   ADD *** 08/31/00 **************************************
     C*
     C*** END   ADD *** 02/19/99 **************************************
     C*
     C     SCSPAR        WHENNE    *BLANKS
      *
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97**           READPPPD05                    95
     C   21              READP     PPD05                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C   22              READP     PPD06                                  95
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97**           READPPPD07                    95
     C***23*****           READPPPD08                    95
     C   23              READP     PPD07                                  95
     C   24              READP     PPD08                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   OTHER
     C*
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97**           READPPPD01                    95
     C   21              READP     PPD01                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C   22              READP     PPD02                                  95
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97**           READPPPD04                    95
     C***23*****           READPPPD03                    95
     C   23              READP     PPD03                                  95
     C   24              READP     PPD04                                  95
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   ENDSL
     C*** END   CHG *** 11/02/98 **************************************
     C                   EXSR      ASSRRN
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SETGT - Position PROVIDER Demographics for Read Access
      **********************************************************************
     C     SETGT         BEGSR
     C*
     C*** BEGIN CHG *** 11/02/98 **************************************
     C                   SELECT
     C*
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     SSFEIN        WHENNE    *ZEROS
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY10     SETGTPPD09                96
     C   21KEY10         SETGT     PPD09                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*** BEGIN ADD *** 11/03/99 **************************************
     C***22***** KEY11     SETGTPPD02                96
     C   22KEY14         SETGT     PPD02                              96
     C*** END   ADD *** 11/03/99 **************************************
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY12     SETGTPPD10                96
     C***23***** KEY13     SETGTPPD11                96
     C   23KEY12         SETGT     PPD10                              96
     C   24KEY13         SETGT     PPD11                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*** BEGIN ADD *** 08/31/00 **************************************
     C     SCSDBA        WHENEQ    'Y'
     C   21KEY1          SETGT     PPD12                              96
     C   22KEY2          SETGT     PPD13                              96
     C   23KEY4          SETGT     PPD14                              96
     C   24KEY5          SETGT     PPD15                              96
     C*** END   ADD *** 08/31/00 **************************************
     C*
     C*** END   ADD *** 02/19/99 **************************************
     C     SCSPAR        WHENNE    *BLANKS
     C*
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY6      SETGTPPD05                96
     C   21KEY6          SETGT     PPD05                              96
     C*** END   CHG *** 08/31/00 **************************************
     C   22KEY7          SETGT     PPD06                              96
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY8      SETGTPPD07                96
     C***23***** KEY9      SETGTPPD08                96
     C   23KEY8          SETGT     PPD07                              96
     C   24KEY9          SETGT     PPD08                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   OTHER
      *
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY1      SETGTPPD01                96
     C   21KEY1          SETGT     PPD01                              96
     C*** END   CHG *** 08/31/00 **************************************
     C   22KEY2          SETGT     PPD02                              96
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY4      SETGTPPD04                96
     C***23***** KEY5      SETGTPPD03                96
     C   23KEY4          SETGT     PPD03                              96
     C   24KEY5          SETGT     PPD04                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   ENDSL
     C*** END   CHG *** 11/02/98 **************************************
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SETLL - Position PROVIDER Demographics for Read Access
      **********************************************************************
     C     SETLL         BEGSR
     C*** BEGIN CHG *** 11/02/98 **************************************
     C                   SELECT
     C*
     C*** BEGIN ADD *** 02/19/99 **************************************
     C     SSFEIN        WHENNE    *ZEROS
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY10     SETLLPPD09                96
     C   21KEY10         SETLL     PPD09                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*** BEGIN ADD *** 11/03/99 **************************************
     C***22***** KEY11     SETLLPPD02                96
     C   22KEY14         SETLL     PPD02                              96
     C*** END   ADD *** 11/03/99 **************************************
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY12     SETLLPPD10                96
     C***23***** KEY13     SETLLPPD11                96
     C   23KEY12         SETLL     PPD10                              96
     C   24KEY13         SETLL     PPD11                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*** BEGIN ADD *** 08/31/00 **************************************
     C     SCSDBA        WHENEQ    'Y'
     C   21KEY1          SETLL     PPD12                              96
     C   22KEY2          SETLL     PPD13                              96
     C   23KEY4          SETLL     PPD14                              96
     C   24KEY5          SETLL     PPD15                              96
     C*** END   ADD *** 08/31/00 **************************************
     C*
     C*** END   ADD *** 02/19/99 **************************************
     C     SCSPAR        WHENNE    *BLANKS
     C*
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY6      SETLLPPD05                96
     C   21KEY6          SETLL     PPD05                              96
     C*** END   CHG *** 08/31/00 **************************************
     C   22KEY7          SETLL     PPD06                              96
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY8      SETLLPPD07                96
     C***23***** KEY9      SETLLPPD08                96
     C   23KEY8          SETLL     PPD07                              96
     C   24KEY9          SETLL     PPD08                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   OTHER
      *
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21N97** KEY1      SETLLPPD01                96
     C   21KEY1          SETLL     PPD01                              96
     C*** END   CHG *** 08/31/00 **************************************
     C   22KEY2          SETLL     PPD02                              96
     C*** BEGIN CHG *** 08/31/00 **************************************
     C***21*97** KEY4      SETLLPPD04                96
     C***23***** KEY5      SETLLPPD03                96
     C   23KEY4          SETLL     PPD03                              96
     C   24KEY5          SETLL     PPD04                              96
     C*** END   CHG *** 08/31/00 **************************************
     C*
     C                   ENDSL
     C*** END   CHG *** 11/02/98 **************************************
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SF1UPD - Assign Deleted message
      **********************************************************************
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCDATA
     C                   ENDSR
      *
      **********************************************************************
      *  SLCTRD - Used to move fields for selected records
      **********************************************************************
     C     SLCTRD        BEGSR
      *
      *  Return to Calling Program PARMS
      *
      *
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   MOVE      '*'           QQRTCD
      *
     C                   ENDSR
      *
      **********************************************************************
      *  SUBSET - Process record against Subset Criteria
      **********************************************************************
     C     SUBSET        BEGSR
      *
      *  Access the Provide Auxiliary file
      *
     C                   Z-ADD     PDFEIN        XXFEIN
     C                   Z-ADD     PDMOD         XXMOD
     C     KEYP1         CHAIN     PP101                              60
      *
      *
      * Check PROVIDER Status against Search Status
      *
     C     SCSSTA        IFNE      *BLANK
     C     SCSSTA        ANDNE     PDSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 03/23/01 **************************************
     C     P1KY          CHAIN     PP101                              99
     C*** END   ADD *** 03/23/01 **************************************
      *
      * Check PROVIDER Name against Search String
      *
B01  C     SCSNAM        IFNE      *BLANKS
     C*** BEGIN CHG *** 03/31/09 **************************************
     C**********           MOVELP1NAME    U1PDSC    P
     C**********           MOVELSCSNAM    U2PDSC    P
     C     LO:UP         XLATE(P)  P1NAME        U1PDSC
     C     LO:UP         XLATE(P)  SCSNAM        U2PDSC
     C*** END   CHG *** 03/31/09 **************************************
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SCSNAM <> BLANK
      *
      *          Address subset selected: Check address 1
      *          ------------------------------------------------------
     C     SCSADR        IFNE      *BLANKS
     C     LO:UP         XLATE     P1ADR1        U1PDSC
     C     LO:UP         XLATE     SCSADR        U2PDSC
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
      *
      *             Subset not in address 1:
      *             ---------------------------------------------------
     C     SCNRES        IFEQ      0
      *
      *                No address 2: set off switch
      *                ------------------------------------------------
     C     P1ADR2        IFEQ      *BLANKS
     C                   MOVE      *OFF          MEETSS
     C                   ELSE                                                   ADR 2 = blanks
      *
      *                If Address 2: Check it
      *                ------------------------------------------------
B01  C     SCSADR        IFNE      *BLANKS
     C                   MOVEL(P)  P1ADR2        U1PDSC
     C                   MOVEL(P)  SCSADR        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SCSADR <> BLANK
      *
     C                   ENDIF                                                  ADR 2 = blanks
     C                   ENDIF                                                  ADR 1 NRES=0
     C                   ENDIF                                                  SADR = blanks
      *
      *  Check city against subset string
      *
B01  C     SCSCTY        IFNE      *BLANKS
     C                   MOVEL(P)  P1CTY         U1PDSC
     C                   MOVEL(P)  SCSCTY        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SCSCTY <> BLANK
      *
      *  Check State code against subset criteria
      *
     C     SCSPST        IFNE      *BLANK
     C     SCSPST        ANDNE     P1PST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Zipcode contains subset number
      * ZZZZ
B01  C     SCSZIP        IFNE      *BLANKS
     C                   MOVEL(P)  P1ZIP         U1PDSC
     C                   MOVEL(P)  SCSZIP        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SCSZIP <> BLANK
      *
      *  Check Telephone contains subset number
      *
B01  C     SCSPHN        IFNE      *BLANKS
     C                   MOVEL(P)  P1PHN#        U1PDSC
     C                   MOVEL(P)  SCSPHN        U2PDSC
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SCSPHN <> BLANK
      *
      * Check DBA Flag against Search DBA Flag
      *
B01  C     SCSDBA        IFNE      *BLANKS
     C     SCSDBA        ANDNE     PDDBA1
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check PROVIDER Type against Search Types
      *
     C                   MOVEA     TY            XXTY              6
     C     XXTY          IFNE      *BLANK
     C                   Z-ADD     1             I
     C     PDTYPE        LOOKUP    TY(I)                                  40
     C     *IN40         IFEQ      *OFF
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** BEGIN ADD *** 03/23/01 **************************************
     C                   ELSE
     C     PDTYPE        IFNE      '1'
     C     PDTYPE        ANDNE     '4'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 03/23/01 **************************************
     C                   ENDIF
      *
      *          If servicing provider request: display only if
      *          provider type found in servicing provider array.
      *          ------------------------------------------------------
     C     *IN61         IFEQ      *ON
     C     MEETSS        ANDEQ     *ON
     C     PDBWCT        LOOKUP    TYP                                    60
     C     *IN60         IFEQ      *OFF
     C     PDBWCT        OREQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  *IN60 = Off
     C                   ENDIF                                                  *IN61 = On
      *
      *  Check Participation against subset criteria
      *
     C     SCSPAR        IFNE      *BLANK
     C     SCSPAR        ANDNE     PDPARN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Check Specialty Code against subset criteria
      *
     C     SCSSPC        IFNE      *BLANK
     C     SCSSPC        ANDNE     PDSPCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *
     C*** BEG ADD *** 09/28/98 ****************************************
     C*
     C* IF CALLING PROGRAM HAS SENT SEVEN PARMS AND QQ1PRV IS 'Y'
     C* AND QQFEIN IS NOT BLANKS, THEN SUBESET ON QQFEIN/QQMOD
     C*
     C     Q1PRMS        IFGE      7
     C     QQ1PRV        ANDEQ     'Y'
     C*** BEGIN CHG *** 12/21/09 **************************************
     C********** QQFEIN    ANDNE*BLANKS
     C     SSFEIN        ANDNE     *ZEROS
     C*** END   CHG *** 12/21/09 **************************************
     C*
     C                   Z-ADD     0             HLFEIN
     C                   Z-ADD     0             HLMOD
     C*** BEGIN CHG *** 12/21/09 **************************************
     C**********           MOVE QQFEIN    HLFEIN
     C**********           MOVE QQMOD     HLMOD
     C                   MOVE      SSFEIN        HLFEIN
     C                   MOVE      SSMOD         HLMOD
     C*** END   CHG *** 12/21/09 **************************************
     C     HLFEIN        IFNE      PDFEIN
     C     HLMOD         ORNE      PDMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  QQFEIN NE PDFEIN
     C*
     C                   ENDIF                                                  Q1PRMS GE 7
     C*** END ADD *** 09/28/98 ****************************************
     C                   ENDSR
      *
      ****************************************************************
      *  MCINTR - WCMC Interface                                     *
      ****************************************************************
     C     MCINTR        BEGSR
      *
      *  WCMS / WCMC Installed flags determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C                   ENDSR
      *
** Headers - HD
Opt Name                                  FEIN#      Mod   Tp Spec St Sts P/N
Opt FEIN#      MOD  Name                                   Tp Spec St Sts P/N
Opt DBA/Address                           FEIN#      Mod   Tp Spec St Sts P/N
Opt BWC ID    Name                                         Tp Spec St Sts P/N
**  Description-------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   NNN
1044=Delete                 NNN
1055=Display                YYY
1066=Print                  NNN
1077=Additional Detail Info YYN
1088=Network Info           YYY
1099=State Info             YYY
11010=Provdr Parish Sale TaxYYY
11414=DARTS                 YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
210F10=Search               NNN
211F11=Change View          YYY
212F12=Cancel               NNY
213F13=Chg Usr Parm         NNN
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
