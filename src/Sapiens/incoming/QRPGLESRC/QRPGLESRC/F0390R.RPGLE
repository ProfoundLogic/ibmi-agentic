     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*** Beg Add ***** 11/22/16 **************************************
     H Bnddir('STBNDDIR')
     H*** End Add ***** 11/22/16 **************************************
     H*****************************************************************
     H*  F0390R - WORK WITH PROVIDER BILL DETAIL       C:62 12/05/97  *
     H*                                                               *
     H*  CALLED FROM: MANY PLACES                                     *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  05/28/25 C:342 Support: 119130
     H*                 1.Pass correct rrn to MtStRules program
     H*                 2.Return to Rule Selection popup screen when we hit cancel
     H*                 on State Rules Maintenace screen.
     H*                 3. Fix issue reported under 119339
     H*
     H*  10/26/23 C:342 Project: FS668
     H*                 State Rules maintenance C/S Standards
     H*
     H*  07/18/23 C:342 Project: NY060
     H*                 NY Compliance: CARC/RARC Update
     H*
     H*  06/09/20  C:212 Project : SY420
     H*                  Mask SSN
     H*
     H*  05/03/19 C:079 Project: MC248
     H*                 Use new CS program for Detail Charges Prompt
     H*
     H*  06/15/18  C:180  Support: 099/70463
     H*                   Detect Enter Key, fix *INKG returned ON when
     H*                   user presses ENTER.
     H*
     H*  06/06/18  C:079  Support: 099/71442
     H*                   Use the correct RRN for rules window
     H*
     H*  05/09/18  C:079  Support: 114/70740
     H*                   If DRG line is deleted then set a flag
     H*
     H*  02/12/18  C:079  Support: 114/64080
     H*                   Added FL EDI Recon Error for Denied lines
     H*
     H*  11/22/16  C:309  Project: 25MUJ
     H*                   Added Check box for "Bill Import Error" Flag
     H*
     H*  08/11/16  C:079  Support: 42889/099
     H*                   Adjust screen heading to line up with data
     H*
     H*  08/05/16  C:212  Project: FS498
     H*                   Add procesing to Display State Rule window
     H*
     H*  11/19/15 C:309   SUPPORT: 37884 (EOR missing information)
     H*
     H*  08/06/14  C:296  Project: 114MNV
     H*                   Add procesing for Procura/bill review
     H*
     H*  04/16/13  C:079  SUPPORT: MC114 82097
     H*                   Highlight lines correctly when there is a message
     H*
     H*  02/06/13  C:079  PROJECT: 114MER
     H*                   Do not Reposition to line 1 after removing
     H*                   messages
     H*
     H*  01/04/13  C:280  SUPPORT: 81313
     H*                   fix bug
     H*
     H*  12/06/12  C:280  SUPPORT: 81215
     H*                   CHANGE INDICATOR BEING USED
     H*
     H*  11/15/12 C:287 Project: MC367
     H*                 Removed function codes 14,16,19,20,21 for GS
     H*
     H*  07/05/12  C:280  PROJECT: 114MER
     H*                   CLAIMCHECK PROCESSING
     H*
     H*  03/29/12  C:251  SUPPORT: 79591 MC114
     H*                   Initialize indicator 61, for correct line highligts
     H*
     H*  12/13/11  C:191  PROJECT: LA025
     H*                   POPULATE LA PARISH SALES TAX
     H*
     H*  10/13/11  C:280  PROJECT 114MBE
     H*                   Add subroutine to check for claim payment limit
     H*                   PAYLMT
     H*
     H*  06/16/11  C:061  PROJECT 114MZP
     H*                   Add "F1=Correct Code" for clt114.
     H*
     H*  05/24/11  C:257  SUPPORT 76935
     H*                   ERRCK1 should execute SR OPT13 and SR OPT12
     H*                   everytime it reads 13 and 12,respectively ,
     H*                   on the Line item.
     H*
     H*  06/25/09  C:227  SUPPORT 68961
     H*                   Compilation only.
     H*
    H*  01/09/08  C:062  PROJECT: MC025 61431
    H*                   CORRECT LOADING OF HCPCS CODE.
     H*
    H*  04/19/07  C:182  PROJECT: FL067
    H*                   CHANGE MADE TO SHOW HCPCS CODE ON FINAL
    H*                   ADJUDICATION SCREEN.
    H*                   CHANGE MADE TO SHOW MODIFIER ON FINAL
    H*                   ADJUDICATION SCREEN.
     H*
     H*  04/11/07  C:188  PROJECT: FL061
     H*                   1. CONDITIONED CALL TO FA713R ONLY WHEN EOCPST <> 'Y'
     H*                   2. CHANGED 'F7=HEADER INFO' FLAGS TO 'YYN' SO
     H*                      CAN USE IN INQUIRY MODE ALSO.
     H*
     H*  03/05/07  C:188  PROJECT: 10MJC
     H*                   EDI Edits (P2P Import)
     H*
     H*  02/21/07  C:188  SUPPORT: 57052
     H*                   ADDED CALL TO FA744R TO CORRECT
     H*                   DIAGNOSIS INDICATOR EDIT ISSUE
    H*
     H*  06/27/03  C:79   PROJECT: FSMNT
     H*                   CHANGE OUT LOGICAL TO HAVE EOB# IN KEY
     H*
     H*  05/16/03  C:79   PROJECT: FS249
     H*                   ADD FUNCTION 20=PENDED ITEMS
     H*
     H*  01/29/03  C:079  SUPPORT 35674
     H*                   SELECT ONLY ACTIVE EOB CODES FOR DISPLAY
     H*
     H*  08/21/02  C:079  PROJECT 114MDC
     H*                   1. CREATE SUB TO DELETE FSMRA DATA
     H*
     H*  08/16/02  C:062  PROJECT 114MDC
     H*                   ADD CODEREVIEW PROCESSING. CHG F2 TO PROCESS
     H*
     H*  05/30/02  C:079  SUPPORT:MC071 32741
     H*                   EXCLUDE SOME AL PROVIDERS FROM FSMRA CALCS
     H*
     H*  05/15/02  C:079  SUPPORT:MC095 32421
     H*                   DONT INCLUDE HCPCS IN FSMRA CALCS FOR OKLAH.
     H*
     H*  02/16/02  C:079  SUPPORT:MC044 31044
     H*                   DISPLAY MOST RECENT EOMB CODE
     H*
     H********** Install SA001R with this revision *****************
     H*  01/16/02  C:079  PROJECT:114MAO
     H*                   ADD OPTION 13=DISPLAY CORRECT CODING RULE
     H*
     H*  01/11/02  C:079  SUPPORT:30707
     H*                   WHEN WRITING FSMRA RECORD GET DESC FROM
     H*                   PROCEDURE CODE FILE.
     H*
     H*  01/09/02  C:079  SUPPORT:30602
     H*                   USE DSEQ# WHEN DELETING FSMRA EOB CODES
     H*
     H*  01/07/02  C:079  SUPPORT:MC095 30641
     H*                   WHEN ONLY CHANGING FSMRA LINE ITEM DON'T
     H*                   CALL THE ADJUDICATION API OR DELETE FSMRA
     H*
     H*  12/13/01  C:079  PROJECT:95MAX
     H*                   WHEN ONLY DELETING FSMRA LINE ITEM DON'T
     H*                   CALL THE CRTDIF SUBROUTINE FOR FSMRA
     H*
     H*  09/19/01  C:079  PROJECT:MC246
     H*                   DHUNTS CANNOT = 0 SO. IF IT IS THE SET = 1
     H*
     H*  08/23/01 C:079 SUPPORT:MC071 28611
     H*                 IF ALABAMA HOSP BILL CALCULATE FSMRA
     H*                 DIFFERENTLY USING CADIFF FROM ADJUDICATION
     H*
     H*  05/24/01 C:062 PROJECT:FS209
     H*                 SWAP OUT LOGICALS WITH CPT4 WITH THOSE WITH
     H*                 CODE.  SWAP OUT SCCPT4 WITH SCCODE.
     H*
     H*  05/24/01 C:062 PROJECT:FSMNT #192  BASE ONLY
     H*                 ADD PARM TO CALL OF F9951R, TO LET IT KNOW
     H*                 THAT IT IS NOT BEING CALLED FROM THE MENU.
     H*
     H*  05/16/01 C:079 PROJECT:91MEL
     H*                 1. Remove FSMRA logic for Georgia per state
     H*
     H*  02/19/01 C:062 PROJECT:FS223
     H*                 ADD NEW OPTION TO SEE FS RULE INFORMATION.
     H*
     H*  12/29/00 C:117 PROJECT: FS209
     H*                 CHANGE F16=HISTORY TO USE NEW PGMS
     H*
     H*  06/07/00 C:117 PROJECT:MCMNT
     H*                 ADDED F8=HEADER EOB/NOTES
     H*
     H*  05/09/00 C:079 Support: MC048 Seq#: 22827
     H*                 Added subroutine to update/add FDDAP records
     H*                 if the amounts dont match FDCAP amounts.
     H*
     H*  03/24/00 C:117 Support: MC048 Seq#: 22211
     H*                 Deleted update of FRDT & TODT on a recon for
     H*                 the FSMRA record.  Instead of using today's
     H*                 date for the FDAT & TDAT when writing the
     H*                 FSMRA record, use EOFRDT & EOTDAT.
     H*
     H*  02/29/00 C:79  Project: FSMNT
     H*                 Expand HL Fields used for FSMRA calc to 11.2
     H*
     H*  02/17/00 C:79  Support: MC048/21896
     H*                 Temporarily remove TX Hospital from FSMRA calc
     H*                 Use correct fields for PCA06
     H*                 No need for chain when writing FDCAP
     H*
     H*  02/15/00 C:62/79
     H*                 Support: MC044/21561
     H*                 Reposition to same page after adjudication
     H*
     H*  12/07/99 C:117 Project: FS144
     H*                 Add FSMRA logic to PBills
     H*
     H*  11/12/99 C:56  Project: MC152
     H*                 Delete/Change efficiency issue.
     H*
     H*  07/23/99 C:31  Project: NW010
     H*                 WC Summary File
     H*                 1. Add Opt7=Network Info
     H*
     H*  07/19/99 C:062 Project: MC208                                *
     H*                 ADD MMI DATE TO HEADER.                       *
     H*                                                               *
     H*  07/07/99 C:117 Project: MC152                                *
     H*                 Added revisions for Dental Bill Processing    *
     H*                                                               *
     H*  04/02/99 C:56 Support: MC025 #17457                          *
     H*                Change logicals for view#1 and view#2 to       *
     H*                resolve paging issue.                          *
     H*                                                               *
     H*  03/25/99 C:56 MC152                                          *
     H*                1. ADD CO#/SUB#/FND AS PARMS TO CALL OF        *
     H*                   HISTORY PROGRAM - F0575R                    *
     H*                                                               *
     H* 02/23/99 C:56 PROJECT: MC152                                  *
     H*               Remove the deletion of the EOB Codes/Notes and  *
     H*               Error Messages from SF1UPD subrouting.  This    *
     H*               will be done in the appropriate Detail program  *
     H*                                                               *
     H* 02/17/99 C:56 PROJECT: MC152                                  *
     H*               LOAD DESCRIPTION FOR CORRESPONDING DETAIL FILE  *
     H*                                                               *
     H* 01/18/99 C:79 PROJ:25MKO                                      *
     H*               1.  Add logic to load CPT4, DESC, and PMOD for  *
     H*                   "V" types of bills                          *
     H*
     H*  01/11/99 C:31  Project: MC152
     H*                 V4: Bill Entry re-write
     H*                 1. Add option 21
     H*
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0390F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFDCAP     IF   E             DISK
     F                                     RENAME(PCA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFDCAL10   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCA01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     F*** BEG CHG *** 04/02/99 ****************************************
     F**********FDCAL11 IF  E           K        DISK
     FFDCAL16   IF   E           K DISK
     F*** END CHG *** 04/02/99 ****************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCA01:DBRCDF2)
     F*
     F*  Tertiary logical file definition (for displaying items)
     F*** BEG CHG *** 04/02/99 ****************************************
     F**********FDCAL12 IF  E           K        DISK
     F*** BEGIN CHG *** 05/24/01 **************************************
     F**********FDCAL17 IF  E           K        DISK
     FFDCALY    IF   E           K DISK
     F*** END   CHG *** 05/24/01 **************************************
     F*** END CHG *** 04/02/99 ****************************************
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCA01:DBRCDF3)
     FFDCALL    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     F*** BEGIN CHG *** 08/16/02 **************************************
     F**********FAEOP   IF  E                    DISK
     FFAEOP     UF   E             DISK
     F*** END   CHG *** 08/16/02 **************************************
     FFMBEL     IF   E           K DISK
     FFMCXL     IF   E           K DISK
     F*** BEG DEL *** 02/17/99 ****************************************
     F*****FMPRL   IF  E           K        DISK
     F*****FMSVL1  IF  E           K        DISK
     F*** END DEL *** 02/17/99 ****************************************
     FFMCML1    IF   E           K DISK
     FFDDHL     IF   E           K DISK
     F                                     INFDS(DBFDSB)
     FFDDUL     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FFDDPL     IF   E           K DISK
     F                                     INFDS(DBFDSF)
     F*** BEG ADD *** 07/07/99 ****************************************
     FFDDDL     IF   E           K DISK
     F                                     INFDS(DBFDSG)
     F*** END ADD *** 07/07/99 ****************************************
     F*** BEG CHG *** 07/05/12 ****************************************
     F***FDMGL1    IF   E           K DISK
     FFDMGL1    UF   E           K DISK
     F*** END CHG *** 07/05/12 ****************************************
     F*** BEG DEL *** 02/23/99 ****************************************
     F**********FDMGL2  UF  E           K        DISK
     F**********  PMG01                             KRENAMEPMG02
     F*** END DEL *** 02/23/99 ****************************************
     FUMPCL11   IF   E           K DISK
     F                                     INFDS(DBFDSD)
     FFDDEL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFDCAL13   UF A E           K DISK
     F                                     RENAME(PCA01:PCA03)
     f*** Beg Add *** 06/06/18 ****************************************
     f                                     INFDS(DBFDSH)
     f*** End Add *** 06/06/18 ****************************************
     F*** BEG ADD *** 11/12/99 ****************************************
     FFDCAL12   IF   E           K DISK
     F                                     RENAME(PCA01:PCA04)
     FFDDAL2    UF A E           K DISK
     FFDDAL1    IF   E           K DISK
     F                                     RENAME(PDA01:PDA02)
     F*** END ADD *** 11/12/99 ****************************************
     F*** BEG ADD *** 12/07/99 ****************************************
     FFDDHL3    UF A E           K DISK
     F                                     RENAME(PDH01:PDH02)
     F*** BEG CHG *** 06/27/03 ****************************************
     F*****FDDUL1  UF  E           K        DISK                      A
     FFDDUL3    UF A E           K DISK
     F*** END CHG *** 06/27/03 ****************************************
     F                                     RENAME(PDU01:PDU02)
     F*** BEG CHG *** 06/27/03 ****************************************
     F*****FDDDL1  UF  E           K        DISK                      A
     FFDDDL2    UF A E           K DISK
     F*** END CHG *** 06/27/03 ****************************************
     F                                     RENAME(PDD01:PDD02)
     FFDDEL3    UF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDCALF    UF A E           K DISK
     F                                     RENAME(PCA01:PCA05)
     FFDCAL14   IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFDCAL19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA07)
     F*** END ADD *** 12/07/99 ****************************************
     F*** BEGIN ADD *** 02/19/01 **************************************
     FFDFRL0    IF   E           K DISK
     F                                     INFDS(DBFDSZ)
     F*** END   ADD *** 02/19/01 **************************************
     F*** BEG ADD *** 01/11/02 ****************************************
     FFMPRL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK
     F*** END DEL *** 01/11/02 ****************************************
     F*** BEG ADD *** 05/30/02 ****************************************
     FFMP2LAL   IF   E           K DISK
     F*** END ADD *** 05/30/02 ****************************************
     F*** BEG ADD *** 03/05/07 ****************************************
     FFDELL1    IF   E           K DISK    USROPN
     F*** END ADD *** 03/05/07 ****************************************
      ***** BEG ADD ******* 06/16/11 **********************************
      *
      *  WCMC MED BILL AUTH AUX
     FFAAUL1    IF   E           K DISK
      ***** END ADD ******* 06/16/11 **********************************
      ***** BEG ADD ******* 07/05/12 **********************************
     FFTABL     IF   E           K DISK
      ***** END ADD ******* 07/05/12 **********************************
      ***** Beg Add ******* 08/06/14 **********************************
     F* WCMC Case Lockout File
     FFXLOP     UF   E           K DISK
      ***** End Add ******* 08/06/14 **********************************
      ***** Beg Add ******* 08/05/16 **********************************
     FFARULL1   IF   E           K DISK
      ***** End Add ******* 08/05/16 **********************************
      /COPY SCOPYWW01D

     D*** Beg Add ****** 11/22/16 **************************
      /Copy sPrModule
     D*** End Add ****** 11/22/16 **************************
     D*** Beg Add ****** 02/12/18 **************************
      /Copy sPrGenPop
     D*** End Add ****** 02/12/18 **************************

     D*
     C**** Beg Chg *** 07/18/23 **************************************
     D**** HD              S             78    DIM(3) CTDATA PERRCD(1)              Column headings
     D HD              S             80    DIM(3) CTDATA PERRCD(1)              Column headings
     C**** End Chg *** 07/18/23 **************************************
     D*** BEGIN CHG *** 07/23/99 **************************************
     D********            OF      1  22  1   OFD    35   Options, Function
     D*** BEG CHG *** 01/16/02 ****************************************
     D******************* OF      1  24  1   OFD    35   Options, Function
     D*** BEG CHG *** 05/16/03 ****************************************
     D******************* OF      1  25  1   OFD    35   Options, Function
     D*** BEG CHG *** 03/05/07 ****************************************
     D******************* OF      1  26  1   OFD    35   Options, Function
     D OF              S              1    DIM(28) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(28) ALT(OF)
     D*** BEG CHG *** 03/05/07 ****************************************
     D*** END CHG *** 05/16/03 ****************************************
     D*** END CHG *** 07/23/99 ****************************************
     D QCA             S             56    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D MSG             S             65    DIM(1) CTDATA PERRCD(1)
     D*** END   ADD *** 05/24/01 **************************************
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSD          DS
     D  DBRRND               397    400B 0
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D*** BEG ADD *** 07/07/99 ****************************************
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D*** END ADD *** 07/07/99 ****************************************
     D*** BEGIN ADD *** 02/19/01 **************************************
     D DBFDSZ          DS
     D  DBRRNZ               397    400B 0
     D*** END   ADD *** 02/19/01 **************************************
     d*** Beg Add *** 06/06/18 **************************************
     d dbfdsh          DS
     d  dbrrnh               397    400B 0
     d*** End Add *** 06/06/18 **************************************
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D                 DS
     D  ##NUM                  1     80
     D  XXNUM                  6      9
     D*
     D                 DS
     D  XADATA                 1     40
     D  XALIN#                 1      4
     D  XAFDAT                 6     15
     D  XACPT4                17     28
     D  XAPMOD                30     31
     D  XADESC                33     40
     D*
     D                 DS
     D  XBDATA                 1     40
     D  XBFDAT                 1     10
     D  XBLIN#                12     15
     D  XBCPT4                17     28
     D  XBPMOD                30     31
     D  XBDESC                33     40
     D*
     D                 DS
     D  XCDATA                 1     40
     D  XCCPT4                 1     12
     D  XCPMOD                14     15
     D  XCDESC                17     24
     D  XCLIN#                26     29
     D  XCFDAT                31     40
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11
     D*** BEG ADD *** 11/12/99 ****************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END ADD *** 11/12/99 ****************************************
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill Detail')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D SLMSG1          C                   CONST('MC40081')
     D SLMSG2          C                   CONST('FSMESG')
     D CHGNUM          C                   CONST('SA020R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D DELETE          C                   CONST('** Delete  **')
     D MRAADJ          C                   CONST('MRA adjustment')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(6)
     D*================================================================
     D*  Program specific constants
     D*================================================================
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D CANCEL          C                   CONST('12F12=Cancel        -
     D                                            YYY')
     D*** BEGIN ADD *** 07/23/99 **************************************
     D NETWRK          C                   CONST('077=Network Info    -
     D                                            YYY')
     D*** END ADD *** 07/23/99 ****************************************
     D PCERTF          C                   CONST('18F18=View Pre-Cert -
     D                                            YYY')
     D*** BEG ADD *** 03/05/07 ****************************************
     D*
     D SETEDI          C                   CONST('01F1=Set EDI flag   -
     D                                            YYN')
     D*
     D WRKERR          C                   CONST('22F22=Work with Erro-
     D                                     rs     YYY')
     D*
     D*** END ADD *** 03/05/07 ****************************************
     D***** Beg Add *** 08/05/16 **************************************
     D*
     D ShowRuleWdw     pr                  ExtPgm('FTFSRULWDW')
     D rrnCA                          9
     D rrnFR                          9
     D kkmode                         1
     D kktabind                       5
     D***** Beg Add *** 05/28/25 ******************************
     D kkExit                         1
     D***** End Add *** 05/28/25 ******************************
     D*
     D***** End Add *** 08/05/16 **************************************
     D*** Beg Chg *** 08/06/14 ****************************************
     DReAssignQue      pr                  ExtPgm('IA001R')
     D EOidc                         12A
     D PROAU1                        10A   const
     D*
     DUpdBillHststs    pr                  ExtPgm('F0395R01')
     D ##kc                           1A
     D ##kl                           1A
     D ##mode                         1A
     D ##rrn                          9  0
     D ##rrn2                         9  0
     D ##BLST                         1A
     D ##StCd                         3A
     D ##ReAs                        60A
     D ##pgm                          8A
     D*
     DChkRightcoding   pr                  ExtPgm('FA785R')
     D ##rrn5                         9
     D ##rtcd                         1
     D*
     D ##kc1           s              1A
     D ##kl1           s              1A
     D ##mode1         s              1A
     D ##rrn1          s              9  0
     D ##rrn5          s              9    inz
     D ##rrn21         s              9  0
     D ##BLST1         s              1A
     D ##StCd1         s              3A
     D ##ReAs1         s             60A
     D ##pgm1          s              8A
     D*
     D*** End Chg *** 08/06/14 ****************************************
     D***** Beg Add *** 08/05/16 **************************************
     D xxcntrule       s              9  0 inz
     D kkrrn           s              9  0 inz
     D rrnFR#          s              9  0 inz
     D rrnFR           s              9    inz
     D rrnCA           s              9    inz
     D kkmode          s              1    inz
     D kktabind        s              5    inz
     D indwdwcall      s               n   inz(*off)
     D***** End Add *** 08/05/16 **************************************
      ***** BEG ADD ******* 06/16/11 **********************************
     D C114F1          C                   CONST('01F1=Correct code in-
     D                                     fo     YYN')
    D*** Beg Add ****** 05/03/19 **************************
     d ##PrDs          s             40a
     d*
     d DtlChgsList     pr                  ExtPgm('MTRPTDTCHP')
     d                                3  0
     d                                3  0
     d                                7  0
     d                                9  0
     d                                3  0
    D*** End Add ****** 05/03/19 **************************
     d*** Beg Add *** 10/26/23 ****************************************
      // calling parameter for MtStRules
     d @@rrn1          s              9
     d @@mode1         s              1
     d @@next1         s             10
     d @@tabind1       s              5
     d MtStRules       pr                  ExtPgm('MTSTRULES')
     d @@rrn1                         9
     d @@mode1                        1
     d @@next1                       10
     d @@tabind1                      5
     D*** End Add *** 10/26/23 ****************************************
     D***** Beg Add *** 05/28/25 ******************************
     D kkExit          s               n   inz(*off)
     D***** End Add *** 05/28/25 ******************************

     I*
     I***PY SCOPYWW01E
     I*
     IPCA03
     I              CACO#                       C3CO#
     I              CASUB#                      C3SUB#
     I              CAFND                       C3FND
     I              CAFYR                       C3FYR
     I              CACASE                      C3CASE
     I              CACSCD                      C3CSCD
     I              CAFDAT                      C3FDAT
     I              CATDAT                      C3TDAT
     I              CARDAT                      C3RDAT
     I              CAIPOP                      C3IPOP
     I              CAFEIN                      C3FEIN
     I              CAMOD                       C3MOD
     I              CAPRTY                      C3PRTY
     I              CATRCD                      C3TRCD
     I              CASVCD                      C3SVCD
     I              CAPMOD                      C3PMOD
     I              CACPT4                      C3CPT4
     I              CAICD9                      C3ICD9
     I              CAANMN                      C3ANMN
     I              CAUNTS                      C3UNTS
     I              CATOTC                      C3TOTC
     I              CATOTA                      C3TOTA
     I              CATOTD                      C3TOTD
     I              CAEOMB                      C3EOMB
     I              CATOTP                      C3TOTP
     I              CADSEQ                      C3DSEQ
     I              CADELF                      C3DELF
     I              CAFLAG                      C3FLAG
     I              CACHK#                      C3CHK#
     I              CAEOB#                      C3EOB#
     I              CAPART                      C3PART
     I              CAP1                        C3P1
     I              CASS#                       C3SS#
     I              CAACDT                      C3ACDT
     I              CASTAT                      C3STAT
     I              CAUSER                      C3USER
     I              CAPGM                       C3PGM
     I              CATIME                      C3TIME
     I              CADT                        C3DT
     I              CAIUSR                      C3IUSR
     I              CAIDT                       C3IDT
     I              CADXR                       C3DXR
     I              CADCS#                      C3DCS#
     I              CALTR#                      C3LTR#
     I              CARCFL                      C3RCFL
     I              CAOTHR                      C3OTHR
     I              CANDIS                      C3NDIS
     I              CARDTP                      C3RDTP
     I              CABRSV                      C3BRSV
     I              CABRPM                      C3BRPM
     I              CABRPC                      C3BRPC
     I              CABRAM                      C3BRAM
     I              CABRUN                      C3BRUN
     I              CABRTC                      C3BRTC
     I              CAZIPA                      C3ZIPA
     I              CAAST                       C3AST
     I              CADIFF                      C3DIFF
     I              CAIPGM                      C3IPGM
     I              CACHRG                      C3CHRG
     I              CAMRA                       C3MRA
     I              CAOTH2                      C3OTH2
     I              CAPAID                      C3PAID
     I              CARCN                       C3RCN
     I              CALIN#                      C3LIN#
     I              CARULE                      C3RULE
     I              CAOT1C                      C3OT1C
     I              CAOT2C                      C3OT3C
     I              CAMRAA                      C3MRAA
     I              CAITIM                      C3ITIM
     I*** BEG ADD *** 12/07/99 ****************************************
     I              CAADJ                       C3ADJ
     I              CABPRC                      C3BPRC
     I              CAMNAA                      C3MNAA
     I*** END ADD *** 12/07/99 ****************************************
     I*** BEGIN ADD *** 05/24/01 **************************************
     I              CACODE                      C3CODE
     I*** END   ADD *** 05/24/01 **************************************
     I*** BEG ADD *** 11/12/99 ****************************************
     IPCA04
     I              CACO#                       C4CO#
     I              CASUB#                      C4SUB#
     I              CAFND                       C4FND
     I              CAFYR                       C4FYR
     I              CACASE                      C4CASE
     I              CACSCD                      C4CSCD
     I              CAFDAT                      C4FDAT
     I              CATDAT                      C4TDAT
     I              CARDAT                      C4RDAT
     I              CAIPOP                      C4IPOP
     I              CAFEIN                      C4FEIN
     I              CAMOD                       C4MOD
     I              CAPRTY                      C4PRTY
     I              CATRCD                      C4TRCD
     I              CASVCD                      C4SVCD
     I              CAPMOD                      C4PMOD
     I              CACPT4                      C4CPT4
     I              CAICD9                      C4ICD9
     I              CAANMN                      C4ANMN
     I              CAUNTS                      C4UNTS
     I              CATOTC                      C4TOTC
     I              CATOTA                      C4TOTA
     I              CATOTD                      C4TOTD
     I              CAEOMB                      C4EOMB
     I              CATOTP                      C4TOTP
     I              CADSEQ                      C4DSEQ
     I              CADELF                      C4DELF
     I              CAFLAG                      C4FLAG
     I              CACHK#                      C4CHK#
     I              CAEOB#                      C4EOB#
     I              CAPART                      C4PART
     I              CAP1                        C4P1
     I              CASS#                       C4SS#
     I              CAACDT                      C4ACDT
     I              CASTAT                      C4STAT
     I              CAUSER                      C4USER
     I              CAPGM                       C4PGM
     I              CATIME                      C4TIME
     I              CADT                        C4DT
     I              CAIUSR                      C4IUSR
     I              CAIDT                       C4IDT
     I              CADXR                       C4DXR
     I              CADCS#                      C4DCS#
     I              CALTR#                      C4LTR#
     I              CARCFL                      C4RCFL
     I              CAOTHR                      C4OTHR
     I              CANDIS                      C4NDIS
     I              CARDTP                      C4RDTP
     I              CABRSV                      C4BRSV
     I              CABRPM                      C4BRPM
     I              CABRPC                      C4BRPC
     I              CABRAM                      C4BRAM
     I              CABRUN                      C4BRUN
     I              CABRTC                      C4BRTC
     I              CAZIPA                      C4ZIPA
     I              CAAST                       C4AST
     I              CADIFF                      C4DIFF
     I              CAIPGM                      C4IPGM
     I              CACHRG                      C4CHRG
     I              CAMRA                       C4MRA
     I              CAOTH2                      C4OTH2
     I              CAPAID                      C4PAID
     I              CARCN                       C4RCN
     I              CALIN#                      C4LIN#
     I              CARULE                      C4RULE
     I              CAOT1C                      C4OT1C
     I              CAOT2C                      C4OT3C
     I              CAMRAA                      C4MRAA
     I              CAITIM                      C4ITIM
     I*** END ADD *** 11/12/99 ****************************************
     I*** BEGIN ADD *** 05/24/01 **************************************
     I              CACODE                      C4CODE
     I*** END   ADD *** 05/24/01 **************************************
     I*** BEGIN ADD ****** 12/13/11 ************************************
     I              CATAX                       C4TAX
     I*** END   ADD ****** 12/13/11 ************************************
     I*** BEG ADD *** 12/07/99 ****************************************
     IPCA05
     I              CACO#                       C5CO#
     I              CASUB#                      C5SUB#
     I              CAFND                       C5FND
     I              CAFYR                       C5FYR
     I              CACASE                      C5CASE
     I              CACSCD                      C5CSCD
     I              CAFDAT                      C5FDAT
     I              CATDAT                      C5TDAT
     I              CARDAT                      C5RDAT
     I              CAIPOP                      C5IPOP
     I              CAFEIN                      C5FEIN
     I              CAMOD                       C5MOD
     I              CAPRTY                      C5PRTY
     I              CATRCD                      C5TRCD
     I              CASVCD                      C5SVCD
     I              CAPMOD                      C5PMOD
     I              CACPT4                      C5CPT4
     I              CAICD9                      C5ICD9
     I              CAANMN                      C5ANMN
     I              CAUNTS                      C5UNTS
     I              CATOTC                      C5TOTC
     I              CATOTA                      C5TOTA
     I              CATOTD                      C5TOTD
     I              CAEOMB                      C5EOMB
     I              CATOTP                      C5TOTP
     I              CADSEQ                      C5DSEQ
     I              CADELF                      C5DELF
     I              CAFLAG                      C5FLAG
     I              CACHK#                      C5CHK#
     I              CAEOB#                      C5EOB#
     I              CAPART                      C5PART
     I              CAP1                        C5P1
     I              CASS#                       C5SS#
     I              CAACDT                      C5ACDT
     I              CASTAT                      C5STAT
     I              CAUSER                      C5USER
     I              CAPGM                       C5PGM
     I              CATIME                      C5TIME
     I              CADT                        C5DT
     I              CAIUSR                      C5IUSR
     I              CAIDT                       C5IDT
     I              CADXR                       C5DXR
     I              CADCS#                      C5DCS#
     I              CALTR#                      C5LTR#
     I              CARCFL                      C5RCFL
     I              CAOTHR                      C5OTHR
     I              CANDIS                      C5NDIS
     I              CARDTP                      C5RDTP
     I              CABRSV                      C5BRSV
     I              CABRPM                      C5BRPM
     I              CABRPC                      C5BRPC
     I              CABRAM                      C5BRAM
     I              CABRUN                      C5BRUN
     I              CABRTC                      C5BRTC
     I              CAZIPA                      C5ZIPA
     I              CAAST                       C5AST
     I              CADIFF                      C5DIFF
     I              CAIPGM                      C5IPGM
     I              CACHRG                      C5CHRG
     I              CAMRA                       C5MRA
     I              CAOTH2                      C5OTH2
     I              CAPAID                      C5PAID
     I              CARCN                       C5RCN
     I              CALIN#                      C5LIN#
     I              CARULE                      C5RULE
     I              CAOT1C                      C5OT1C
     I              CAOT2C                      C5OT2C
     I              CAMRAA                      C5MRAA
     I              CAITIM                      C5ITIM
     I              CAADJ                       C5ADJ
     I              CABPRC                      C5BPRC
     I              CAMNAA                      C5MNAA
     I*** END ADD *** 12/07/99 ****************************************
     I*** BEGIN ADD *** 05/24/01 **************************************
     I              CACODE                      C5CODE
      ***** END ADD ******* 06/16/11 **********************************
      *
      *

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@MODE            1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          feaop
     C                   PARM                    @@PGM            10
     C                   PARM                    @@PGM2           10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBR2        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      SCDBRR        ##RRN3            9 0
     C                   PARM      *BLANKS       ##NWCD            5
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
      *
      ***** BEG ADD ******* 06/16/11 **********************************
      *
      *  WCMC MED BILL AUTH AUX
      *
     C     AUKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOSUB#
     C                   KFLD                    EOFND
     C                   KFLD                    EOFYR
     C                   KFLD                    EOCASE
     C                   KFLD                    EOIDOC
      *
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKFDAT
     C*
     C     KEY4          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*** BEGIN CHG *** 05/24/01 **************************************
     C**********           KFLD           KKCPT4
     C                   KFLD                    KKCODE
     C*** END   CHG *** 05/24/01 **************************************
     C*
     C*** BEG ADD *** 04/02/99 ****************************************
     C     KEY5          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKFDAT
     C                   KFLD                    KKLIN#
     C*
     C     KEY6          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*** BEGIN CHG *** 05/24/01 **************************************
     C**********           KFLD           KKCPT4
     C                   KFLD                    KKCODE
     C*** END   CHG *** 05/24/01 **************************************
     C                   KFLD                    KKLIN#
     C*** END ADD *** 04/02/99 ****************************************
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     DHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*** BEG ADD *** 12/13/01 ****************************************
     C     DHKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    HLDSEQ
     C*
     C     DDKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    HLDSEQ
     C*
     C     DUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    HLDSEQ
     C*** END ADD *** 12/13/01 ****************************************
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*** BEG ADD *** 07/07/99 ****************************************
     C     DDKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 07/07/99 ****************************************
     C*
     C     MGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     MGKY2         KLIST
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1CASE
     C                   KFLD                    K1EOB#
     C                   KFLD                    K1DSEQ
     C*
     C*** BEG DEL *** 02/17/99 ****************************************
     C********** PRKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           K1CPT4
     C**********
     C********** PRKY2     KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           K1CPT4
     C**********           KFLD           KKEFDT
     C**********
     C* Key list for Service Code File
     C**********
     C********** SVKY1     KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKCMSV
     C**********           KFLD           KKEFDT
     C**********
     C********** SVKY2     KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           KKCMSV
     C*** END DEL *** 02/17/99 ****************************************
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEGIN ADD *** 02/19/01 **************************************
     C*
     C* STATE RULES
     C*
     C     FRKY1         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKRULE
     C                   KFLD                    KKEFDT
     C*
     C     FRKY2         KLIST
     C                   KFLD                    KKST
     C                   KFLD                    KKRULE
     C*** END   ADD *** 02/19/01 **************************************
     C*
     C     PCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C* Key list for EOMB Detail EOB/Notes Trans file
     C*
     C     DEKY          KLIST
     C                   KFLD                    K1CO#
     C                   KFLD                    K1SUB#
     C                   KFLD                    K1FND
     C                   KFLD                    K1FYR
     C                   KFLD                    K1CASE
     C                   KFLD                    K1EOB#
     C                   KFLD                    K1DSEQ
     C*
     C     DEKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key List for Provider Bills Control File (FMBEP)
     C*
     C     BEKY          KLIST
     C                   KFLD                    EOAST
     C                   KFLD                    EOSYS
     C                   KFLD                    EOBTYP
     C                   KFLD                    EOSTYP
     C*** BEG ADD *** 11/12/99 ****************************************
     C*
     C* Key List for EOMB Detail Amount Transaction File
     C*
     C     DAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C     DAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C                   KFLD                    KKRCN
     C*
     C*** END ADD *** 11/12/99 ****************************************
     C*** BEG ADD *** 12/07/99 ****************************************
     C     DHKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKPRCD            5
     C*
     C     DUKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEG ADD *** 06/27/03 ****************************************
     C                   KFLD                    KKEOB#
     C*** END ADD *** 06/27/03 ****************************************
     C                   KFLD                    K1RVCD            3 0
     C                   KFLD                    KKRATE            5
     C*
     C     DDKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** BEG ADD *** 06/27/03 ****************************************
     C                   KFLD                    KKEOB#
     C*** END ADD *** 06/27/03 ****************************************
     C                   KFLD                    KKPRCD            5
     C*
     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY5         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CAKY6         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     CAKY7         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#
     C*
     C     CAKY8         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     CAKY9         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     DEKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*** BEG ADD *** 01/09/02 ****************************************
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 01/09/02 ****************************************
     C*
     C     DAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** END ADD *** 12/07/99 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Key list for Procedure code file
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRCD
     C*
     C* Key list for Service Code File
     C*
     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCMSV
     C*** END ADD *** 01/11/02 ****************************************
     C*** BEG ADD *** 05/30/02 ****************************************
     C*
     C* Key list for Provider Auxiliary file
     C*
     C     P2KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCMSV
     C                   KFLD                    KKFDAT
     C*
     C     P2KY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCMSV
     C*** END ADD *** 05/30/02 ****************************************
     C*** BEG ADD *** 07/05/12 ****************************************
     C     TBLKEY        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    CSCD             10
     C*** END ADD *** 07/05/12 ****************************************
     C*** Beg Add *** 08/06/14 ****************************************
     C*
     C* KEY LIST FOR WCMC CASE LOCKOUT FILE
     C*
     C     LOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** End Add *** 08/06/14 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CADCS#        KKDCS#
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C*** BEGIN CHG *** 05/24/01 **************************************
     C********** *LIKE     DEFN CACPT4    KKCPT4
     C     *LIKE         DEFINE    CACODE        KKCODE
     C*** END   CHG *** 05/24/01 **************************************
     C*** BEG DEL *** 02/17/99 ****************************************
     C********** *LIKE     DEFN PRCPT4    K1CPT4
     C********** *LIKE     DEFN PREFDT    KKEFDT
     C*** END DEL *** 02/17/99 ****************************************
     C     *LIKE         DEFINE    EOCASE        ##CASE
     C     *LIKE         DEFINE    EOSS#         ##SS#
     C     *LIKE         DEFINE    DHEOB#        KKEOB#
     C     *LIKE         DEFINE    DHDSEQ        KKDSEQ
     C     *LIKE         DEFINE    CAFEIN        K1FEIN
     C     *LIKE         DEFINE    CAMOD         K1MOD
     C     *LIKE         DEFINE    CXCPDF        ##CPDF
     C     *LIKE         DEFINE    CXCPPN        ##CPPN
     C     *LIKE         DEFINE    PCCERT        KKCERT
     C     *LIKE         DEFINE    PCEXT         KKEXT
     C     *LIKE         DEFINE    DHMOD1        XXPMOD
     C     *LIKE         DEFINE    DECO#         K1CO#
     C     *LIKE         DEFINE    DESUB#        K1SUB#
     C     *LIKE         DEFINE    DEFND         K1FND
     C     *LIKE         DEFINE    DEFYR         K1FYR
     C     *LIKE         DEFINE    DECASE        K1CASE
     C     *LIKE         DEFINE    DEEOB#        K1EOB#
     C     *LIKE         DEFINE    DEDSEQ        K1DSEQ
     C     *LIKE         DEFINE    DEDSEQ        HLDSEQ
     C     *LIKE         DEFINE    DEDSEQ        ##DSEQ
     C     *LIKE         DEFINE    CAPAID        XXTOTL
     C*** BEG ADD *** 11/12/99 ****************************************
     C     *LIKE         DEFINE    CACHRG        SMCHG
     C     *LIKE         DEFINE    CAMRA         SMMRA
     C     *LIKE         DEFINE    CANDIS        SMNDIS
     C     *LIKE         DEFINE    CAOTHR        SMOTHR
     C     *LIKE         DEFINE    CAOTH2        SMOTH2
     C     *LIKE         DEFINE    CAPAID        SMPAID
     C     *LIKE         DEFINE    CACHRG        DFCHRG
     C     *LIKE         DEFINE    CAMRA         DFMRA
     C     *LIKE         DEFINE    CANDIS        DFNDIS
     C     *LIKE         DEFINE    CAOTHR        DFOTHR
     C     *LIKE         DEFINE    CAOTH2        DFOTH2
     C     *LIKE         DEFINE    CAPAID        DFPAID
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C     *LIKE         DEFINE    CATAX         DFTAX
     C     *LIKE         DEFINE    CATAX         SMTAX
     C*** END   ADD ****** 12/13/11 ************************************
     C     *LIKE         DEFINE    CARCN         KKRCN
     C*** END ADD *** 11/12/99 ****************************************
     C*** BEG ADD *** 12/07/99 ****************************************
     C     *LIKE         DEFINE    CALIN#        X1LIN#
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAPAID        XXPAID
     C     *LIKE         DEFINE    CACHRG        X3CHRG
     C     *LIKE         DEFINE    CAMRAA        X3MRAA
     C     *LIKE         DEFINE    CADIFF        X3DIFF
     C*** END ADD *** 12/07/99 ****************************************
     C*** BEGIN ADD *** 02/15/00 **************************************
     C     *LIKE         DEFINE    SCLIN#        X@LIN#
     C*** BEGIN CHG *** 05/24/01 **************************************
     C********** *LIKE     DEFN SCCPT4    X@CPT4
     C     *LIKE         DEFINE    SCCODE        X@CODE
     C*** END   CHG *** 05/24/01 **************************************
     C     *LIKE         DEFINE    SCPST2        X@FDAT
     C*** END   ADD *** 02/15/00 **************************************
     C*** BEGIN ADD *** 02/19/01 **************************************
     C     *LIKE         DEFINE    FRST          KKST
     C     *LIKE         DEFINE    FRRULE        KKRULE
     C     *LIKE         DEFINE    FREFDT        KKEFDT
     C*** END   ADD *** 02/19/01 **************************************
     C*** BEG ADD *** 12/13/01 ****************************************
     C     *LIKE         DEFINE    CAFLAG        XXCRT
     C     *LIKE         DEFINE    DBRRNB        SVRRNB
     C     *LIKE         DEFINE    DBRRNE        SVRRNE
     C     *LIKE         DEFINE    DBRRNG        SVRRNG
     C*** END ADD *** 12/13/01 ****************************************
     C*** BEG ADD *** 01/04/13 ****************************************
     C     *LIKE         DEFINE    *IN01         ONCE
     C*** END ADD *** 01/04/13 ****************************************
     C*
     C* CONVERT @@CALL
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'A'
     C     @@MODE        OREQ      'C'
     C                   MOVEL     ' '           @@CALL            1
     C*
     C     @@MODE        WHENEQ    'D'
     C                   MOVEL     '1'           @@CALL
     C*
     C                   ENDSL
     C*
     C/COPY SCOPYWW01C
     C*** BEG ADD *** 05/09/00 ****************************************
     C*
     C*================================================================
     C* BALAMT - Create/Update FDDAP records if out of balance w/FDCAP
     C*================================================================
     C     BALAMT        BEGSR
     C*
     C* WRITE/UPDATE EOMB DETAIL AMOUNT TRANSACTION FILE
     C*
     C     CAKY          CHAIN(N)  PCA04                              99
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     0             SMCHG
     C                   Z-ADD     0             SMMRA
     C                   Z-ADD     0             SMNDIS
     C                   Z-ADD     0             SMOTHR
     C                   Z-ADD     0             SMOTH2
     C                   Z-ADD     0             SMPAID
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C                   Z-ADD     0             SMTAX
     C*** END   ADD ****** 12/13/11 ************************************
     C                   Z-ADD     C4DSEQ        KKDSEQ
     C     DAKY          CHAIN     PDA02                              98
     C     *IN98         DOWEQ     *OFF
     C                   ADD       DACHRG        SMCHG
     C                   ADD       DAMRA         SMMRA
     C                   ADD       DANDIS        SMNDIS
     C                   ADD       DAOTHR        SMOTHR
     C                   ADD       DAOTH2        SMOTH2
     C                   ADD       DAPAID        SMPAID
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C                   ADD       DATAX         SMTAX
     C*** END   ADD ****** 12/13/11 ************************************
     C     DAKY          READE     PDA02                                  98
     C                   ENDDO                                                  *IN98 = *OFF
     C*
     C* COMPARE THE DETAIL CHARGE AMOUNTS WITH SUM OF DETAIL AMOUNTS
     C* IF THERE IS A DIFFERENCE, THEN WRITE OR UPDATE DETAIL AMONT REC
     C*
     C     C4CHRG        SUB       SMCHG         DFCHRG
     C     C4MRA         SUB       SMMRA         DFMRA
     C     C4NDIS        SUB       SMNDIS        DFNDIS
     C     C4OTHR        SUB       SMOTHR        DFOTHR
     C     C4OTH2        SUB       SMOTH2        DFOTH2
     C     C4PAID        SUB       SMPAID        DFPAID
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C     C4TAX         SUB       SMTAX         DFTAX
     C*** END   ADD ****** 12/13/11 ************************************
     C*
     C     DFCHRG        IFNE      *ZEROS
     C     DFMRA         ORNE      *ZEROS
     C     DFNDIS        ORNE      *ZEROS
     C     DFOTHR        ORNE      *ZEROS
     C     DFOTH2        ORNE      *ZEROS
     C     DFPAID        ORNE      *ZEROS
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C     DFTAX         ORNE      *ZEROS
     C*** END   ADD ****** 12/13/11 ************************************
     C*
     C* UPDATE IF THERE IS A FDDAP FOR THE CURRENT RECON
     C* ELSE WRITE NEW FDDAP RECORD TO REFLECT CHANGES
     C*
     C     DAKY1         CHAIN     PDA01                              97
     C     *IN97         IFEQ      *OFF
     C     DFCHRG        ADD       DACHRG        DACHRG
     C     DFMRA         ADD       DAMRA         DAMRA
     C     DFNDIS        ADD       DANDIS        DANDIS
     C     DFOTHR        ADD       DAOTHR        DAOTHR
     C     DFOTH2        ADD       DAOTH2        DAOTH2
     C     DFPAID        ADD       DAPAID        DAPAID
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C     DFTAX         ADD       DATAX         DATAX
     C*** END   ADD ****** 12/13/11 ************************************
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1PGM         DAPGM
     C                   MOVEL(P)  Q1USER        DAUSER
     C                   Z-ADD     DSTIME        DATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DADT
     C                   UPDATE    PDA01
     C*
     C                   ELSE
     C*
     C                   CLEAR                   PDA01
     C                   Z-ADD     KKCO#         DACO#
     C                   Z-ADD     KKSUB#        DASUB#
     C                   Z-ADD     KKFND         DAFND
     C                   Z-ADD     KKFYR         DAFYR
     C                   Z-ADD     KKCASE        DACASE
     C                   MOVEL     EOSS#         DASS#
     C                   Z-ADD     EOACDT        DAACDT
     C                   Z-ADD     KKEOB#        DAEOB#
     C                   Z-ADD     KKDSEQ        DADSEQ
     C                   Z-ADD     KKRCN         DARCN#
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DATRDT
     C                   Z-ADD     DFCHRG        DACHRG
     C                   Z-ADD     DFMRA         DAMRA
     C                   Z-ADD     DFNDIS        DANDIS
     C                   Z-ADD     DFOTHR        DAOTHR
     C                   Z-ADD     DFOTH2        DAOTH2
     C                   Z-ADD     DFPAID        DAPAID
     C*** BEGIN ADD ****** 12/13/11 ************************************
     C                   Z-ADD     DFTAX         DATAX
     C*** END   ADD ****** 12/13/11 ************************************
     C                   MOVE      'A'           DASTAT
     C                   MOVEL(P)  Q1PGM         DAPGM
     C                   MOVEL(P)  Q1PGM         DAIPGM
     C                   MOVEL(P)  Q1USER        DAUSER
     C                   MOVEL(P)  Q1USER        DAIUSR
     C                   Z-ADD     DSTIME        DATIME
     C                   Z-ADD     DSTIME        DAITIM
     C                   MOVEL     SRDTO         DADT
     C                   MOVEL     SRDTO         DAIDT
     C                   WRITE     PDA01
     C                   ENDIF                                                  *IN97 = *OFF
     C*
     C                   ENDIF                                                  DFCHRG NE *ZEROS
     C     CAKY          READE     PCA04                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C                   ENDSR
     C*** END ADD *** 05/09/00 ****************************************
     C*
     C*================================================================
     C* CALCSM - Calculate the sum of the charges for bill
     C*================================================================
     C     CALCSM        BEGSR
     C*
     C* CALCULATE SUMMATION OF ALL CHARGES FOR BILL
     C*
     C                   Z-ADD     0             SCSUM
     C     CAKY          CHAIN(N)  PCA02                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       CACHRG        SCSUM
     C     CAKY          READE(N)  PCA02                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/07/99 ****************************************
     C*================================================================
     C* CRTDIF - Checks to see if a FSMRA rec needs to be created;
     C*          calls subroutine to create FSMRA rec
     C*================================================================
     C     CRTDIF        BEGSR
     C*
     C*** BEG CHG *** 02/29/00 ****************************************
     C******************** Z-ADD*ZEROS    HLDIFF  92
     C******************** Z-ADD*ZEROS    HLCHRG  92
     C******************** Z-ADD*ZEROS    HLPAID  92
     C******************** Z-ADD*ZEROS    HLTDIF  92
     C******************** Z-ADD*ZEROS    HLNEWP  92
     C                   Z-ADD     *ZEROS        HLDIFF           11 2
     C                   Z-ADD     *ZEROS        HLCHRG           11 2
     C                   Z-ADD     *ZEROS        HLPAID           11 2
     C                   Z-ADD     *ZEROS        HLTDIF           11 2
     C                   Z-ADD     *ZEROS        HLNEWP           11 2
     C*** END CHG *** 02/29/00 ****************************************
     C*
     C*** BEGIN CHG *** 08/16/02 **************************************
     C********** @@RRN     CHAINPEO01                99
     C     @@RRN         CHAIN(N)  PEO01                              99
     C*** END   CHG *** 08/16/02 **************************************
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C*
     C*** The following logic is for Medical/Physician bills
     C*
     C     EOBTYP        IFEQ      'N'
     C*
     C     CAKY8         CHAIN     PCA03                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C     C3CPT4        IFNE      'FSMRA'
     C*
     C*** BEG ADD *** 05/15/02 ****************************************
     C*
     C* EXCLUDE OKLAHOMA HCPCS CODES. HCPCS ARE PAID LESSER OF CHARGED
     C* OR HCFA ALLOWANCE.  PG 327 1.
     C*
     C     EOAST         IFEQ      'OK'
     C     C3CPT4        ANDGE     'A0000'
     C     C3CPT4        ANDLE     'Z9999'
     C*
     C                   UPDATE    PCA03
     C     CAKY8         READE     PCA03                                  99
     C*
     C                   ELSE
     C*** END ADD *** 05/15/02 ****************************************
     C                   Z-ADD     C3MRAA        X3MRAA
     C                   Z-ADD     C3CHRG        X3CHRG
     C     X3MRAA        SUB       X3CHRG        X3DIFF
     C*
     C     X3DIFF        IFGT      0
     C                   Z-ADD     X3DIFF        C3DIFF
     C                   ADD       C3DIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             C3DIFF
     C*
     C                   ENDIF                                                  C3DIFF=0
     C*
     C                   ADD       C3CHRG        HLCHRG
     C                   ADD       C3PAID        HLPAID
     C                   MOVE      Q1PGM         C3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3DT
     C                   Z-ADD     DSTIME        C3TIME
     C*
     C                   UPDATE    PCA03
     C     CAKY8         READE     PCA03                                  99
     C*** BEG ADD *** 05/15/02 ****************************************
     C*
     C                   ENDIF
     C*** END ADD *** 05/15/02 ****************************************
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*
     C     CAKY8         READE     PCA03                                  99
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     HLCHRG        SUB       HLPAID        HLTDIF
     C*
     C     HLDIFF        IFLE      HLTDIF
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     HLTDIF        HLNEWP
     C                   ENDIF                                                  DIFF<TDIF
     C*
     C     HLNEWP        IFGT      *ZERO
     C     HLNEWP        OREQ      *ZERO
     C     *IN55         ANDEQ     *ON
     C*
     C                   EXSR      WRTMED
     C*
     C                   ENDIF                                                  HLNEWP>0
     C*
     C                   ENDIF                                                  EOBTYP='N'
     C*
     C*** The following logic is for Dental bills
     C*
     C     EOBTYP        IFEQ      'D'
     C*
     C     CAKY8         CHAIN     PCA03                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C     C3CPT4        IFNE      'FSMRA'
     C*** BEG ADD *** 05/15/02 ****************************************
     C*
     C* EXCLUDE OKLAHOMA HCPCS CODES. HCPCS ARE PAID LESSER OF CHARGED
     C* OR HCFA ALLOWANCE.  PG 327 1.
     C*
     C     EOAST         IFEQ      'OK'
     C     C3CPT4        ANDGE     'A0000'
     C     C3CPT4        ANDLE     'Z9999'
     C*
     C                   UPDATE    PCA03
     C     CAKY8         READE     PCA03                                  99
     C*
     C                   ELSE
     C*** END ADD *** 05/15/02 ****************************************
     C                   Z-ADD     C3MRAA        X3MRAA
     C                   Z-ADD     C3CHRG        X3CHRG
     C     X3MRAA        SUB       X3CHRG        X3DIFF
     C*
     C     X3DIFF        IFGT      0
     C                   Z-ADD     X3DIFF        C3DIFF
     C                   ADD       C3DIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             C3DIFF
     C*
     C                   ENDIF                                                  C3DIFF=0
     C*
     C                   ADD       C3CHRG        HLCHRG
     C                   ADD       C3PAID        HLPAID
     C                   MOVE      Q1PGM         C3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3DT
     C                   Z-ADD     DSTIME        C3TIME
     C*
     C                   UPDATE    PCA03
     C     CAKY8         READE     PCA03                                  99
     C*** BEG ADD *** 05/15/02 ****************************************
     C*
     C                   ENDIF
     C*** END ADD *** 05/15/02 ****************************************
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*
     C     CAKY8         READE     PCA03                                  99
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     HLCHRG        SUB       HLPAID        HLTDIF
     C*
     C     HLDIFF        IFLE      HLTDIF
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     HLTDIF        HLNEWP
     C                   ENDIF                                                  DIFF<TDIF
     C*
     C     HLNEWP        IFGT      *ZERO
     C     HLNEWP        OREQ      *ZERO
     C     *IN55         ANDEQ     *ON
     C*
     C                   EXSR      WRTDEN
     C*
     C                   ENDIF                                                  HLNEWP>0
     C*
     C                   ENDIF                                                  EOBTYP='D'
     C*
     C*** The following logic is for Hospital bills
     C*
     C     EOBTYP        IFEQ      'H'
     C*
     C     CAKY8         CHAIN     PCA03                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*** BEG ADD *** 05/30/02 ****************************************
     C*
     C* Alabama Hospitals flagged with an '*' are excluded from FSMRA
     C*
     C     EOAST         IFEQ      'AL'
     C*
     C                   Z-ADD     C3DSEQ        KKDSEQ
     C                   MOVE      'N'           XXSKIP            1
     C*
     C     DUKY          CHAIN     PDU01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      DURVCD        KKRVCD
     C                   MOVE      DURATE        KKHCPC
     C                   MOVE      *ZEROS        KKUNTS
     C                   Z-ADD     C3FDAT        KKFDAT
     C*
     C                   ENDIF
     C*
     C     P2KY          SETGT     PP201
     C     P2KY1         READPE    PP201                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C     P2FL02        ANDEQ     '*'
     C*
     C                   MOVE      'Y'           XXSKIP
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDIF                                                  EOAST = AL
     C*
     C*** END ADD *** 05/30/02 ****************************************
     C     C3CPT4        IFNE      'FSMRA'
     C*** BEG ADD *** 05/30/02 ****************************************
     C     XXSKIP        IFNE      'Y'
     C*** END ADD *** 05/30/02 ****************************************
     C                   Z-ADD     C3MRAA        X3MRAA
     C                   Z-ADD     C3CHRG        X3CHRG
     C*
     C     X3MRAA        SUB       X3CHRG        X3DIFF
     C*
     C*** BEG ADD *** 08/23/01 ****************************************
     C     EOAST         IFEQ      'AL'
     C                   ADD       C3DIFF        HLDIFF
     C*
     C                   ELSE
     C*** END ADD *** 08/23/01 ****************************************
     C*
     C     X3DIFF        IFGT      0
     C                   Z-ADD     X3DIFF        C3DIFF
     C                   ADD       C3DIFF        HLDIFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             C3DIFF
     C                   ENDIF                                                  C3DIFF=0
     C*** BEG ADD *** 08/23/01 ****************************************
     C                   ENDIF                                                  EOAST = AL
     C*** END ADD *** 08/23/01 ****************************************
     C*
     C                   ADD       C3CHRG        HLCHRG
     C                   ADD       C3PAID        HLPAID
     C                   MOVE      Q1PGM         C3PGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3DT
     C                   Z-ADD     DSTIME        C3TIME
     C*
     C*** BEG ADD *** 05/30/02 ****************************************
     C                   ENDIF                                                  XXSKIP <> Y
     C*** END ADD *** 05/30/02 ****************************************
     C*
     C                   UPDATE    PCA03
     C*
     C     CAKY8         READE     PCA03                                  99
     C*
     C                   ELSE
     C*
     C                   SETON                                        55
     C*
     C     CAKY8         READE     PCA03                                  99
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN99=*OFF
     C*
     C     HLCHRG        SUB       HLPAID        HLTDIF
     C*
     C*** BEG ADD *** 08/23/01 ****************************************
     C     EOAST         IFEQ      'AL'
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*** END ADD *** 08/23/01 ****************************************
     C*
     C     HLDIFF        IFLE      HLTDIF
     C                   Z-ADD     HLDIFF        HLNEWP
     C*
     C                   ELSE
     C*
     C                   Z-ADD     HLTDIF        HLNEWP
     C                   ENDIF                                                  DIFF<TDIF
     C*
     C*** BEG ADD *** 08/23/01 ****************************************
     C                   ENDIF                                                  EOAST = AL
     C*** END ADD *** 08/23/01 ****************************************
     C*
     C     HLNEWP        IFGT      *ZERO
     C     HLNEWP        OREQ      *ZERO
     C     *IN55         ANDEQ     *ON
     C*
     C                   EXSR      WRTHOS
     C*
     C                   ENDIF                                                  HLNEWP>0
     C*
     C                   ENDIF                                                  EOBTYP='H'
     C*
     C                   Z-ADD     0             KKDSEQ
     C                   Z-ADD     0             KKLIN#
     C*
     C                   ENDSR
     C*** END ADD *** 12/07/99 ****************************************
     C*** BEG ADD *** 08/21/02 ****************************************
     C*================================================================
     C* DELFSM - Delete FSMRA record data
     C*================================================================
     C     DELFSM        BEGSR
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C                   MOVEL     'FSMRA'       KKRATE
     C                   Z-ADD     *ZEROS        K1RVCD
     C*
     C*** Physician
     C*
     C     EOBTYP        IFEQ      'N'
     C*
     C     DHKY2         CHAIN     PDH02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     DHEOB#        KKEOB#
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKY3         CHAIN     PCA03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EORCN         ANDEQ     CARCN
     C*
     C                   DELETE    PCA03
     C                   DELETE    PDH02
     C*
     C     DAKY1         CHAIN     PDA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PDA01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     DEKY2         CHAIN     PDE02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   DELETE    PDE02
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   EORCN=*ZERO
     C*
     C     DAKY1         CHAIN     PDA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PDA01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  EOBTYP=N
     C*
     C*** Hospital
     C*
     C     EOBTYP        IFEQ      'H'
     C*
     C     DUKY2         CHAIN     PDU02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     DUEOB#        KKEOB#
     C                   Z-ADD     DUDSEQ        KKDSEQ
     C*
     C     CAKY3         CHAIN     PCA03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EORCN         ANDEQ     CARCN
     C*
     C                   DELETE    PCA03
     C                   DELETE    PDU02
     C*
     C     DAKY1         CHAIN     PDA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PDA01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     DEKY2         CHAIN     PDE02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   DELETE    PDE02
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   EORCN=*ZERO
     C*
     C     DAKY1         CHAIN     PDA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PDA01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  EOBTYP=H
     C*
     C*** Dental
     C*
     C     EOBTYP        IFEQ      'D'
     C*
     C     DDKY2         CHAIN     PDD02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     DDEOB#        KKEOB#
     C                   Z-ADD     DDDSEQ        KKDSEQ
     C*
     C     CAKY3         CHAIN     PCA03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EORCN         ANDEQ     CARCN
     C*
     C                   DELETE    PCA03
     C                   DELETE    PDD02
     C*
     C     DAKY1         CHAIN     PDA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PDA01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C     DEKY2         CHAIN     PDE02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   DELETE    PDE02
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   EORCN=*ZERO
     C*
     C     DAKY1         CHAIN     PDA01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PDA01
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  EOBTYP=D
     C*
     C                   ENDSR
     C*** END ADD *** 08/21/02 ****************************************
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*** BEG ADD *** 12/13/01 ****************************************
     C                   MOVE      ' '           XXCRT
     C*** END ADD *** 12/13/01 ****************************************
     C*** Beg Add *** 02/12/18 ****************************************
     C*
     C* Prevent changing a FL EDI recon denied line item if previously
     C* sent to the state
     C*
     C     SCSEL         IFEQ      '2'
     C     EORCN         ANDGE     1
     C     SCAST         ANDEQ     'FL'
     C     EOTAPE        ANDNE     'Y'
     C     EOTAPE        ANDNE     'N'
     C     EOTAPE        ANDNE     'E'
     C     SCPAID        ANDEQ     0
      /Free
           PopOneBtn('Per FL MEIG: When paying or adjusting/paying a +
            previously denied line, it must be reported to the division as +
            a new bill with a new unique control number.':' ');
           *in90 = *on;
      /End-free
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSEL = '4'
     C*** End Add *** 02/12/18 ****************************************
     C*
     C* PREVENT THE DELETION OF A LINE ITEM IF IT'S RECON NUMBER
     C* DOES NOT MATCH THE CURRENT RECON
     C*
     C     SCSEL         IFEQ      '4'
     C     SCRCN         ANDNE     SCHRCN
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCSEL = '4'
     C*
     C* SAVE THE DETAIL SEQUENCE NUMBER IF DELETING. IT WILL BE NEEDED
     C* TO DELETE THE EOB CODES AND NOTES FOR THIS LINE ITEM
     C*
     C* IF CHANGING OR DELETING FSMRA RECORD DON'T RECREATE IT OR CALL
     C* ADJUDICATION.
     C     SCSEL         IFEQ      '4'
     C*** BEG ADD *** 01/07/02 ****************************************
     C     SCSEL         OREQ      '2'
     C*** END ADD *** 01/07/02 ****************************************
     C*
     C                   Z-ADD     SCDSEQ        HLDSEQ
     C*** BEG ADD *** 12/13/01 ****************************************
     C     EOBTYP        IFEQ      'N'
     C*
     C* Physician - Set flag if deleting FSMRA record
     C                   Z-ADD     DBRRNB        SVRRNB
     C*
     C     DHKY1         CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C     DHPRCD        ANDEQ     'FSMRA'
     C                   MOVE      'N'           XXCRT
     C                   ENDIF
     C*
     C                   Z-ADD     SVRRNB        DBRRNB
     C*
     C                   ELSE
     C     EOBTYP        IFEQ      'D'
     C*
     C* Dental - Set flag if deleting FSMRA record
     C                   Z-ADD     DBRRNG        SVRRNG
     C*
     C     DDKY1         CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C     DDPRCD        ANDEQ     'FSMRA'
     C                   MOVE      'N'           XXCRT
     C                   ENDIF
     C*
     C                   Z-ADD     SVRRNG        DBRRNG
     C*
     C                   ELSE
     C*
     C* Hospital - Set flag if deleting FSMRA record
     C                   Z-ADD     DBRRNE        SVRRNE
     C*
     C     DUKY1         CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C     DURATE        ANDEQ     'FSMRA'
     C                   MOVE      'N'           XXCRT
     C                   ENDIF
     C*
     C                   Z-ADD     SVRRNE        DBRRNE
       //*** Beg Add *** 05/09/18 ****************************************
      /Free
                         If scSel = '4' and *in99 = *off and DUrvCd = 100
                            and xxclt# = '114';
                            Chain @@rrn Peo01;
                            If %found(faeop);
                               Eval EOflg8 = 'D';
                               Update Peo01;
                            Endif;
                         Endif;
      /End-free
       //*** End Add *** 05/09/18 ****************************************
     C*
     C                   ENDIF                                                  EOBTYP IFEQ D
     C*
     C                   ENDIF                                                  EOBTYP IFEQ N
     C*
     C*** END ADD *** 12/13/01 ****************************************
     C                   ELSE
     C                   Z-ADD     0             HLDSEQ
     C                   ENDIF
     C*
     C*** BEG ADD *** 11/12/99 ****************************************
     C*
     C     @@MODE        IFEQ      'C'
     C*** BEG ADD *** 01/14/00 ****************************************
     C     @@MODE        OREQ      'A'
     C*** END ADD *** 01/14/00 ****************************************
     C*
     C* This code will force execution of SETLL after a 2 or 4 option
     C* Also XXSEL will be used for repositioning
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C*
     C                   MOVE      1             AUTORF
     C*
     C                   MOVEL     SCSEL         XXSEL             2
     C                   MOVEL     SCSEL         X1SEL             2
     C*
     C                   ENDIF                                                  SCSEL = 2
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C* This code is used for repositioning after adjudication. This
     C* stores the value of the fields for the last rcd changed/del
     C     SCSEL         IFEQ      '2'
     C*
     C   21              Z-ADD     SCLIN#        X@LIN#
     C*
     C   22              Z-ADD     SCFDAT        SRDTI
     C   22              CALL      'SRCVTDAT'    DTMDYY
     C   22              MOVEL     SRDTO         X@FDAT
     C   22              Z-ADD     SCLIN#        X@LIN#
     C*
     C*** BEGIN CHG *** 05/24/01 **************************************
     C***23*****           MOVE SCCPT4    X@CPT4    P
     C   23              MOVE(P)   SCCODE        X@CODE
     C*** END   CHG *** 05/24/01 **************************************
     C   23              Z-ADD     SCLIN#        X@LIN#
     C*
     C                   ENDIF                                                  SCSEL = 2
     C*** END   ADD *** 02/15/00 **************************************
     C*
     C                   ENDIF                                                  @@MODE = 'C'
     C*** END ADD *** 11/12/99 ****************************************
     C*** BEGIN ADD *** 02/19/01 **************************************
     C*
     C* Check to see if there is a rule, if not send message
     C*
     C     SCSEL         IFEQ      '12'
     C*
     C                   MOVEL(P)  EOAST         KKST
     C                   Z-ADD     SCRULE        KKRULE
     C                   Z-ADD     *HIVAL        KKEFDT
     C*
     C     FRKY1         SETGT     PFR01
     C     FRKY2         READPE    PFR01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8990
     C*** BEG ADD *** 05/24/11 ****************************************
     C                   ELSE
     C                   EXSR      OPT12
     C*** END ADD *** 05/24/11 ****************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 02/19/01 **************************************
     C*** BEG ADD *** 01/16/02 ****************************************
     C*
     C* Check to see if there is a correct coding rule, if not send msg
     C*
     C     SCSEL         IFEQ      '13'
     C*
     C                   Z-ADD     SCDSEQ        HLDSEQ
     C*
     C     DHKY1         CHAIN     PDH01                              99
     C*
     C                   CALL      'FA714R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      DBRRNB        ##RRN                          FDDHP
     C                   PARM      *ZEROS        ##RRNC            9 0
     C*
     C     ##RRNC        IFEQ      0
     C                   SETON                                        8990
     C*** BEG ADD *** 05/24/11 ****************************************
     C                   ELSE
     C                   EXSR      OPT13
     C*** END ADD *** 05/24/11 ****************************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 01/16/02 ****************************************
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCKM - BILL ERRORS.
     C*================================================================
     C     ERRCKM        BEGSR
     C*
     C                   Z-ADD     SCDSEQ        KKDSEQ
     C*
     C*** BEG ADD *** 10/13/11 ****************************************
     C*
     C     *IN61         IFNE      *ON
     C*
     C*** END ADD *** 10/13/11 ****************************************
     C*
     C*** BEG CHG *** 07/05/12 ****************************************
     C*
     C*
     C**** MGKY          CHAIN     PMG01                              99
     C     MGKY          CHAIN(N)  PMG01                              99
     C*
     C*** END CHG *** 07/05/12 ****************************************
     C*
     C*** BEG CHG *** 01/04/13 ****************************************
     C*
     C                   eval      once = *off
     C*
     C*** END CHG *** 01/04/13 ****************************************
     C*
     C*
     C*** BEG CHG *** 12/06/12 ****************************************
     C*
     C******IN99         IFEQ      *OFF
b01  C     *IN99         DOWEQ     *OFF
     C*
     C*** BEG CHG *** 01/04/13 ****************************************
     C*
b03  C                   IF        once = *OFF
     C                   eval      *in61 = *off
e03  C                   ENDIF
     C*
     C*** END CHG *** 01/04/13 ****************************************
     C*
     C                   MOVEA     MSG           SCMSG
     C*
     C*** BEG CHG *** 12/06/12 ****************************************
     C*
b02  C                   IF        MGMSG# <> 'CKNONCN'
     C*
     C*** END CHG *** 12/06/12 ****************************************
     C*
     C                   SETON                                        61
     C*** Beg Add *** 04/16/13 ****************************************
     C                   leave
     C*** End Add *** 04/16/13 ****************************************
     C*
     C*** BEG CHG *** 12/06/12 ****************************************
     C*
e02  C                   ENDIF
     C*
     C*** END CHG *** 12/06/12 ****************************************
     C*
x01  C*****              ELSE
     C*****              MOVE      *BLANKS       SCMSG
     C*****              SETOFF                                       61
e01  C*****              ENDIF
     C*
     C     MGKY          reade(N)  PMG01                                  99
     C*
     C                   enddo
     C*
     C*** END CHG *** 01/04/13 ****************************************
     C*
     C*** BEG ADD *** 10/13/11 ****************************************
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 10/13/11 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*** BEGIN CHG *** 08/16/02 **************************************
     C********** @@RRN     CHAINPEO01                99
     C     @@RRN         CHAIN(N)  PEO01                              99
     C*** END   CHG *** 08/16/02 **************************************
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C**********           MOVE EOEOB#    KKDCS#
     C                   MOVE      EOEOB#        KKEOB#
     C*** BEG ADD *** 11/12/99 ****************************************
     C                   Z-ADD     EORCN         KKRCN
     C*** END ADD *** 11/12/99 ****************************************
     C*
     C                   SETOFF                                       31
     C*
     C                   MOVE      *LOVAL        KKLIN#
     C                   MOVE      *LOVAL        KKFDAT
     C*** BEGIN CHG *** 05/24/01 **************************************
     C**********           MOVE *LOVAL    KKCPT4
     C                   MOVE      *LOVAL        KKCODE
     C*** END   CHG *** 05/24/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKLIN#
     C*
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTDAT'    DTCYMD
     C   22              MOVEL     SRDTO         KKFDAT
     C*
     C*** BEGIN CHG *** 05/24/01 **************************************
     C***23*****           MOVE SCPST3    KKCPT4    P
     C   23              MOVE(P)   SCPST3        KKCODE
     C*** END   CHG *** 05/24/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVE      SCLIN#        KKLIN#
     C   22              MOVE      SCFDAT        KKFDAT
     C*** BEG ADD *** 04/02/99 ****************************************
     C   22              MOVE      SCLIN#        KKLIN#
     C*** END ADD *** 04/02/99 ****************************************
     C*** BEGIN CHG *** 05/24/01 **************************************
     C***23*****           MOVE SCCPT4    KKCPT4
     C   23              MOVE      SCCODE        KKCODE
     C*** END   CHG *** 05/24/01 **************************************
     C*** BEG ADD *** 04/02/99 ****************************************
     C   23              MOVE      SCLIN#        KKLIN#
     C*** END ADD *** 04/02/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KBDO - F2=Complete
     C*================================================================
     C     KBDO          BEGSR
     C*
      /Free
       //***** Beg Chg ***** 08/06/14 ***************************************
       //-------------------------------------------------------------------
       // Check if Rightcoding in effect. If so :
       // 1) pend bill with code 'AIN',
       // 2) Send msg on screen  3) Call IA001R passing EOidoc and PROAU.
       // 3) Call IA001R (transfer queue) passing EOidoc and PROAU.
       //-------------------------------------------------------------------
       If CltId = '114';

        If EOpauf <> 'P' and EOpauf <> 'A';

          ##rrn5 = %editc(@@rrn:'X');
          ##rtcd = *blanks;

          //Verify if Rightcoding is in effect (bill would need audit then)
          ChkRightCoding(##rrn5:##rtcd);

            If ##rtcd = *blanks;

            Else;

              //Pend using AIN and set Audit flag
              Chain @@rrn Peo01;
                If %found(faeop);
                  EOblst = 'P';
                  EOpauf = 'A';
                  Update Peo01;
                Endif;

              Chain caky8 Pca03;
              If %found(FDCAL13);
                C3flag = 'P';
                Update Pca03;
              endif;

              ##kc1   = ' ';
              ##kl1   = ' ';
              ##mode1 = 'H';
              ##rrn1  = 0;
              ##rrn21 = @@rrn;                                             // faeop
              ##BLST1 = EOblst;
              ##StCd1 = 'AIN';
              ##ReAs1 = 'Sent to Audit Queue';
              ##pgm1  = ' ';

              //update History
              UpdBillHstSts(##kc1:##kl1:##mode1:##rrn1:##rrn21:##BLST1:##StCd1:
               ##ReAs1:##pgm1);

              //send to audit queue
              ReAssignQue(EOidoc:'PROAU');

              //--------------------------------------------------------------
              //Pop up window saying "Sent to Audit Queue".
              //--------------------------------------------------------------
              genmsg1 = 'Sent to Audit Queue';
              genmsg2 = *blanks;
              genmsgOK = *off;
              // display message
              Exfmt    scr04;
              // unlock and come out of bill
              chain loky plo01;
              if %found(fxlop);
                 delete plo01;
              endif;
              OutFlg = *on;
              leaveSr;

            Endif;
        Endif;
       Endif;
       //***** End Chg ***** 08/06/14 ***************************************
      /End-Free
     C     COUNT         IFEQ      *ZEROS
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C* DETERMINE IF ROOMM, ROOMS OR 00001 CODE IS NECESSARY ON
     C* AN INPATIENT BILL
     C*
     C     SCAST         IFEQ      'TX'
     C     EOBTYP        ANDEQ     'H'
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C     EUBLTY        IFGE      '111'
     C     EUBLTY        ANDLE     '115'
     C*** BEG CHG *** 12/07/99 ****************************************
     C***********CAKY2     CHAINPCA03               N99
     C     CAKY2         CHAIN(N)  PCA06                              99
     C     *IN99         DOWEQ     *OFF
     C*** BEG CHG *** 02/17/00 ****************************************
     C********** C3FDAT    IFLT 970801
     C     CAFDAT        IFLT      970801
     C*** END CHG *** 02/17/00 ****************************************
     C                   MOVE      'Y'           DATFL
     C                   LEAVE
     C                   ENDIF                                                  CLFDAT LT 970801
     C***********CAKY2     READEPCA03                    99
     C     CAKY2         READE(N)  PCA06                                  99
     C                   ENDDO                                                  *OFF = *IN99
     C*
     C     DATFL         IFEQ      'Y'
     C                   MOVE      'N'           IFLAG
     C***********CAKY2     CHAINPCA03               N99
     C     CAKY2         CHAIN(N)  PCA06                              99
     C     *IN99         DOWEQ     *OFF
     C*** BEG CHG *** 02/17/00 ****************************************
     C********** C3CPT4    IFEQ 'ROOMM'
     C********** C3CPT4    OREQ 'ROOMS'
     C********** C3CPT4    OREQ '00001'
     C     CACPT4        IFEQ      'ROOMM'
     C     CACPT4        OREQ      'ROOMS'
     C     CACPT4        OREQ      '00001'
     C*** END CHG *** 02/17/00 ****************************************
     C                   MOVE      'Y'           IFLAG
     C                   LEAVE
     C                   ENDIF
     C***********CAKY2     READEPCA03                    99
     C     CAKY2         READE(N)  PCA06                                  99
     C*** END CHG *** 12/07/99 ****************************************
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C     IFLAG         IFEQ      'N'
     C                   SETON                                        9036
     C                   GOTO      KBEND
     C                   ENDIF                                                  IFLAG = 'N'
     C                   ENDIF                                                  DATFL = 'Y'
     C*
     C                   ENDIF                                                  EUBLTY GE '111'
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCAST = 'TX'
     C*
     C* IF FIELD 'ERROR1' IS YES, THEN DISPLAY ERROR FOR NO EOB CODE
     C* ASSIGNED FOR LINE ITEM WITH CHARGE NOT EQUAL PAYMENT
     C*
     C**********           SETOF                     35
     C********** ERROR1    IFEQ 'Y'
     C********** CAKY2     CHAINPCA03                99
     C********** *IN99     DOWEQ*OFF
     C********** C3PAID    IFNE C3CHRG
     C**********           Z-ADD@@RRN     ##RRN1
     C**********           Z-ADDC3DSEQ    ##DSEQ
     C**********           CALL 'FA204R'
     C**********           PARM ' '       ##RTN1
     C**********           PARM           ##RRN1
     C**********           PARM           ##DSEQ
     C********** ##RTN1    IFEQ 'F'
     C**********           SETON                     9035
     C**********           GOTO KBEND
     C**********           ENDIF                           ##RTN1
     C**********           ENDIF                           C3PAID NE C3CHRG
     C********** CAKY2     READEPCA03                    99
     C**********           ENDDO                           *IN99 = *OFF
     C**********           ENDIF                           ERROR1 = Y
     C*** BEG ADD *** 05/09/00 ****************************************
     C* Call subroutine to create/update FDDAP if out of balance w/FDCAP
     C                   EXSR      BALAMT
     C*** END ADD *** 05/09/00 ****************************************
     C*
     C* CALCULATE SUMMATION OF ALL CHARGES FOR BILL
     C*
     C                   EXSR      CALCSM
     C*
     C     SCSUM         IFGT      SCTCHG
     C                   SETON                                        3033
     C                   SETOFF                                       31
     C                   GOTO      KBEND
     C                   ELSE
     C                   SETOFF                                       30
     C                   ENDIF
     C     SCSUM         IFLT      SCTCHG
     C                   SETON                                        32
     C                   SETOFF                                       31
     C                   GOTO      KBEND
     C                   ELSE
     C                   SETOFF                                       30
     C                   ENDIF
     C*
     C*
     C* CALL TO F0393R = ADJUDICATION/PEND SELECTION SCREEN
     C*
     C                   MOVE      @@MODE        ##MODE
     C                   Z-ADD     @@RRN         ##RRN                          FAEOP
     C                   MOVE      CXCPDF        ##CPDF
     C                   MOVE(P)   CXCPPN        ##CPPN
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'Y'           ##CANC            1
     C                   ELSE
     C                   MOVE      ' '           ##CANC
     C                   ENDIF
     C*
     C                   Z-ADD     SCTCHG        ##CHRG           11 2
     C*** BEGIN ADD *** 08/16/02 **************************************
     C*
     C* We need to get updated values of EOCRNE and EOCRDO
     C*
     C     @@RRN         CHAIN(N)  PEO01                              99
     C*
     C* If CodeReview installed, then prompt call CodeReview programs
     C*
     C     EOCRNE        IFEQ      'Y'
     C     ##CRVW        ANDEQ     'Y'
     C*
     C* Has Bill already gone thru CodeReview?
     C* user to decide if they want the bill to go thru CodeReview
     C* again.
     C*
     C                   MOVE      @@RRN         #1RRN
     C                   MOVEL     *BLANKS       ##KL
     C*
     C     EOCRDO        IFEQ      'Y'
     C*
     C* Prompt user to decide if they want the bill to go
     C* thru CodeReview again.
     C*
     C                   CALL      'F1390R'
     C     *INKL         PARM      *BLANKS       ##KL
     C                   PARM      *BLANKS       ##SBCR            1
     C*
     C* If user hits F12 then allow user to go back to Detail program
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     ##SBCR        IFEQ      'Y'
     C*
     C* Must update this flag to go thru CodeReview
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'N'           EOCRDO
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C* If user wants the bill to go thru CodeReview call
     C* CodeReview submission program.
     C*
     C*** BEG ADD *** 08/21/02 ****************************************
     C     XXCRT         IFEQ      ' '
     C*
     C                   EXSR      DELFSM
     C*
     C                   ENDIF
     C*** END ADD *** 08/21/02 ****************************************
     C*
     C                   CALL      'FA735C'
     C                   PARM                    #1RRN             9
     C                   PARM      XXCRT         #1CFMR            1
     C*
     C                   ELSE
     C*
     C* Must update this flag to go thru CodeReview
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'N'           EOCRNE
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C* If user does not want to go thru CodeReview again, then
     C* call the Complete/Pend program.
     C*
     C                   CALL      'F0393R'
     C                   PARM      ' '           ##KL
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN                          FAEOP
     C                   PARM                    ##CPDF
     C                   PARM                    ##CPPN
     C                   PARM                    ##CANC
     C                   PARM                    ##CHRG
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* Must update this flag to go thru CodeReview
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'N'           EOCRDO
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C* This bill has never gone thru CodeReview, so call
     C* CodeReview submission API
     C*
     C*** BEG ADD *** 08/21/02 ****************************************
     C     XXCRT         IFEQ      ' '
     C*
     C                   EXSR      DELFSM
     C*
     C                   ENDIF
     C*** END ADD *** 08/21/02 ****************************************
     C*
     C                   CALL      'FA735C'
     C                   PARM                    #1RRN             9
     C                   PARM      XXCRT         #1CFMR            1
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* CodeReview is not installed.
     C*
     C*** END   ADD *** 08/16/02 **************************************
     C                   CALL      'F0393R'
     C                   PARM      ' '           ##KL
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN                          FAEOP
     C                   PARM                    ##CPDF
     C                   PARM                    ##CPPN
     C                   PARM                    ##CANC
     C                   PARM                    ##CHRG
     C*** BEGIN ADD *** 08/16/02 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/16/02 **************************************
     C*
     C     ##KL          IFEQ      '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     *ON           OUTFLG
     C                   ENDIF
     C*
     C     KBEND         ENDSR
     C*
     C*================================================================
     C* KGDO - F7=HEADER INFO
     C*================================================================
     C     KGDO          BEGSR
     C     BEKY          CHAIN     PBE01                              99
     C     *IN99         IFEQ      *OFF
     C     BEHESM        ANDNE     *BLANK
     C                   Z-ADD     @@RRN         ##RRN
     C                   MOVEL(P)  BEHESM        PGMNAM           10
     C                   CALL      PGMNAM
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL
     C                   PARM      'D'           ##MODE            1
     C                   PARM                    ##RRN
     C                   ENDIF                                                  *IN99 = Off
     C                   SETON                                        90
     C     KGEND         ENDSR
     C*** BEG ADD *** 06/07/00 ****************************************
     C*================================================================
     C* KHDO - F8=Header EOB/Notes
     C*================================================================
     C     KHDO          BEGSR
     C*
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
     C                   CALL      'F0355R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ELSE
     C*
     C                   CALL      'F0355R'
     C                   PARM      '1'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD *** 06/07/00 ****************************************
     C*** BEG ADD *** 03/05/07 ****************************************
     C*================================================================
     C* KADO - F1=Set EDI flag
     C*================================================================
     C     KADO          BEGSR
      ***** BEG ADD ******* 06/16/11 **********************************
     C                   SELECT
      *
     C     CLTID         WHENEQ    '010'
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C* Display window, confirm with user
     C*
     C     @@MODE        IFEQ      'C'
     C     @@MODE        OREQ      'A'
     C                   EXFMT     SCR03
     C*
     C     *INKL         IFNE      *ON
     C*
     C* Update FAEOP
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'N'           EOTAPE
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C* Call FA740R to set EDI flag in provider bill header files
     C*
     C                   CALL      'FA740R'
     C                   PARM                    @@RRN
     C                   PARM                    ##RTCD
     C*
     C                   ENDIF
     C                   ENDIF
      ***** BEG ADD ******* 06/16/11 **********************************
      *
     C     CLTID         WHENEQ    '114'
     C     @@RRN         CHAIN(N)  PEO01                              99
     C     AUKY          CHAIN     PAU01                              60
     C                   MOVEL(P)  AUFRC1        SCFRC1
     C                   MOVEL(P)  AUFRC2        SCFRC2
     C                   MOVEL(P)  AUTOC1        SCTOC1
     C                   MOVEL(P)  AUTOC2        SCTOC2
     C                   MOVEL(P)  AUREAS        SCREAS
     C                   EXFMT     WDW114
      *
     C                   ENDSL
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C                   ENDSR
     C*
     C*** END ADD *** 03/05/07 ****************************************
      *
      *
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   ENDSR
     C*================================================================
     C* KQDO - F16=History
     C*================================================================
     C     KQDO          BEGSR
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*** BEG DEL *** 12/29/00 ****************************************
     C***********          CALL 'F0575R'
     C***********          PARM OPTNM     ##PRDS 40
     C***********          PARM *BLANKS   ##CALL  1
     C*****                PARM CMFYR     ##FYR
     C*****                PARM CMCASE    ##CASE
     C*****                PARM CMSS#     ##SS#
     C***********          PARM KKFYR     ##FYR
     C***********          PARM KKCASE    ##CASE
     C***********          PARM EOSS#     ##SS#
     C***********          PARM XXACDT    ##DATE  80
     C***********          PARM *BLANKS   ##RTCD  1
     C*** BEG ADD *** 03/25/99 ****************************************
     C***********          PARM           ##CO#
     C***********          PARM KKSUB#    ##SUB#
     C***********          PARM KKFND     ##FND
     C*** END ADD *** 03/25/99 ****************************************
     C*** END DEL *** 12/29/00 ****************************************
     C*** BEG ADD *** 12/29/00 ****************************************
     C*
     C*  Call new history pgm F0391R
     C*
     C                   CALL      'F0391R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C*** END ADD *** 12/29/00 ****************************************
     C*                    SETON                     90
     C*
     C                   ENDSR
     C*================================================================
     C* KSDO - F18=View Pre-Cert
     C*================================================================
     C     KSDO          BEGSR
     C*
     C* GET PRECERT RELLATIVE RECORD NUMBER
     C*
     C                   MOVEL     EOCERT        KKCERT
     C                   Z-ADD     EOEXT         KKEXT
     C     PCKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRND        ##RRN
     C*****                Z-ADD0         ##RRN
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      EOSUB#        ##SUB#
     C                   MOVE      EOFND         ##FND
     C                   MOVE      EOFYR         ##FYR
     C                   MOVE      EOCASE        ##CASE
     C                   MOVEL     EOCERT        ##CERT
     C                   MOVE      EOEXT         ##EXT
     C                   CALL      'U0060R01'
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      'D'           ##MODE
     C                   PARM                    ##RRN                          UMPCP
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    ##CERT           15
     C                   PARM                    ##EXT             3
     C                   PARM      ' '           ##OCST            1
     C                   PARM      'Y'           ##CONT            1
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KTDO - F19=View Prov info
     C*================================================================
     C     KTDO          BEGSR
     C*
     C* CALL WORK WITH PROVIDERS IN DISPLAY MODE
     C*
     C                   MOVEL(P)  EOFEIN        ##FEIN
     C                   MOVEL(P)  EOMOD         ##MOD
     C                   CALL      'F0032R'
     C                   PARM      '1'           ##CALL
     C                   PARM                    ##FEIN            9
     C                   PARM                    ##MOD             3
     C                   PARM      ' '           ##RTCD
     C                   PARM      ' '           ##TYPE            1
     C                   PARM      ' '           ##1N4             1
     C                   PARM      'Y'           ##1PRV            1
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/11/99 **************************************
     C*================================================================
     C* KVDO - Detail Charge List
     C*================================================================
     C     KVDO          BEGSR
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     CMACDT        QQACDT            7 0
     C                   MOVE      KKMOD         QQMOD             3
     C*** Beg Chg *** 05/03/19 *********************************************
     C*****************
     C*****************  CALL      'F9951R'
     C*****************  PARM      OPTNM         ##PRDS           40
     C*****************  PARM      '1'           ##CALL
     C*****************  PARM                    KKSUB#
     C*****************  PARM                    KKFND
     C*****************  PARM                    CMSS#
     C*****************  PARM                    QQACDT
     C*****************  PARM                    KKFEIN
     C*****************  PARM                    QQMOD
     C****BEGIN*ADD**** 05/24/01 **************************************
     C*****************  PARM      'Y'           QQMODE            1
     C*** END   ADD *** 05/24/01 **************************************
      /Free
                         Callp DtlChgsList(EOfnd:EOfyr:EOcase:EOfein:EOmod);
      /End-Free
     C*** End Chg *** 05/03/19 *********************************************
     C                   ENDSR
     C*** END ADD *** 01/11/99 ****************************************
     C*** BEG ADD *** 05/16/03 ****************************************
     C*================================================================
     C* KUDO - F20=Pended items
     C*================================================================
     C     KUDO          BEGSR
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*  Call new Pended items pgm F0398R
     C*
     C                   CALL      'F0398R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##CO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKEOB#
     C*
     C*
     C                   ENDSR
     C*** END ADD *** 05/16/03 ****************************************
     C*** BEG ADD *** 03/05/07 ****************************************
     C*================================================================
     C* KWDO - F22 = Call EDI error pgm
     C*================================================================
     C     KWDO          BEGSR
     C*
     C                   CALL      'F0400R'
     C                   PARM                    @@MODE
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          faeop
     C                   PARM                    @@PGM            10
     C                   PARM                    @@PGM2           10
     C*
     C                   ENDSR
     C*
     C*** END ADD *** 03/05/07 ****************************************
     C*================================================================
     C* CHRPMT- Highlight line if payment does not match charged amount
     C*================================================================
     C     CHRPMT        BEGSR
     C*
     C* THIS EDIT IS NECESSARY ONLY IF THE ADJUDICATON STATE IS TEXAS
     C* AND FA204R IS INSTALLED ON SYSTEM
     C*
     C     SCAST         IFEQ      'TX'
     C                   MOVE      *BLANKS       ##RTN             1
     C                   MOVEL(P)  'FA204R'      ##OBJ            10
     C                   MOVEL(P)  '*PGM'        ##TYP             7
     C                   CALL      'FA722C'
     C                   PARM                    ##RTN
     C                   PARM                    ##OBJ
     C                   PARM                    ##TYP
     C     ##RTN         IFNE      'F'
     C                   Z-ADD     @@RRN         ##RRN1            9 0
     C                   Z-ADD     SCDSEQ        ##DSEQ
     C                   CALL      'FA204R'
     C                   PARM      ' '           ##RTN1            1
     C                   PARM                    ##RRN1
     C                   PARM                    ##DSEQ
     C     ##RTN1        IFEQ      'F'
     C                   SETON                                        34
     C**********           MOVE 'Y'       ERROR1
     C                   ELSE
     C                   SETOFF                                       34
     C                   ENDIF                                                  ##RTN1 = 'F'
     C*
     C                   ENDIF                                                  ##RTN NE 'F'
     C                   ENDIF                                                  SCAST = 'TX'
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*****                SETOF                     34
     C*** Beg Add *** 03/29/12 ****************************************
     C                   SETOFF                                       61
     C*** End Add *** 03/29/12 ****************************************
     C                   MOVE      CALIN#        SCLIN#
     C                   MOVE      CAFDAT        SCFDAT
     C*** BEGIN CHG *** 05/24/01 **************************************
     C**********           MOVE CACPT4    SCCPT4
     C                   MOVE      CACODE        SCCODE
     C*** END   CHG *** 05/24/01 **************************************
     C                   MOVEL(P)  CAPMOD        SCPMOD
     C                   MOVEL(P)  CAPART        SCPART
     C                   MOVEL(P)  CAIPOP        SCIPOP
     C                   MOVEL(P)  CACSCD        SCCSCD
     C                   MOVE      CAMRA         SCMRA
     C                   MOVEL(P)  CAEOMB        SCEOMB
     C                   MOVE(P)   CADSEQ        SCDSEQ
     C                   MOVE      CADCS#        SCDCS#
     C*** BEGIN ADD *** 02/19/01 **************************************
     C                   Z-ADD     CARULE        SCRULE
     C*** END   ADD *** 02/19/01 **************************************
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDAT            8 0
     C*
     C                   MOVEL(P)  XXFDAT        ##NUM
     C                   MOVEL(P)  'DATE'        ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         X1FDAT           10
     C*
     C                   MOVEL(P)  CALIN#        ##NUM
     C                   MOVE(P)   *BLANKS       ##MASK
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVE      XXNUM         XXLIN#            4
     C*
     C* GET FIRST PROCECURE MODIFIER
     C*
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   MOVE      *BLANKS       XXPMOD
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
     C                   SELECT
     C     DHMOD1        WHENNE    *BLANKS
     C                   MOVEL     DHMOD1        XXPMOD
     C     DHMOD2        WHENNE    *BLANKS
     C                   MOVEL     DHMOD2        XXPMOD
     C     DHMOD3        WHENNE    *BLANKS
     C                   MOVEL     DHMOD3        XXPMOD
     C     DHMOD4        WHENNE    *BLANKS
     C                   MOVEL     DHMOD4        XXPMOD
     C                   ENDSL
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* GET MOST RECENTLY ENTERED ACTIVE EOMB CODE
     C*
     C*** BEG ADD *** 02/16/02 ****************************************
     C                   CLEAR                   SCEOB
     C*
     C     DEKY1         SETGT     PDE01
     C     DEKY1         READPE    PDE01                                  99
     C*** BEG ADD *** 01/29/03 ****************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C     DESTAT        ANDEQ     'A'
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL     DEEOB         SCEOB
     C                   MOVEL     DEEOBCD       SCEOB
     C**** End Chg *** 07/18/23 **************************************
     C                   LEAVE
     C                   ENDIF                                                  DEEOB IFNE
     C*
     C     DEKY1         READPE    PDE01                                  99
     C*
     C                   ENDDO
     C*** END ADD *** 01/29/03 ****************************************
     C*** BEG DEL *** 01/29/03 ****************************************
     C**********
     C********** *IN99     IFEQ *OFF
     C**********
     C********** DEEOB     IFNE *BLANKS
     C**********           MOVELDEEOB     SCEOB
     C**********           ENDIF                           DEEOB NE *BLANKS
     C**********
     C**********           ENDIF                           *IN99 = *OFF
     C*** END DEL *** 01/29/03 ****************************************
     C*** END ADD *** 02/16/02 ****************************************
     C*** BEG DEL *** 02/16/02 ****************************************
     C**********           CLEARSCEOB
     C********** DEKY1     CHAINPDE01               N99
     C********** *IN99     DOWEQ*OFF
     C********** DEEOB     IFNE *BLANKS
     C**********           MOVELDEEOB     SCEOB
     C**********           LEAVE
     C**********           ENDIF                           DEEOB NE *BLANKS
     C********** DEKY1     READEPDE01               N    99
     C**********           ENDDO                           *IN99 = *OFF
     C*** END DEL *** 02/16/02 ****************************************
     C*
     C* COUNT THE NUMBER OF EOB CODES FOR LINE ITEM, IF MORE THAN
     C* ONE THEN SET INDICATOR TO HI-LITE THE EOB CODE FIELD
     C*
     C                   MOVE      *ZEROS        COUNT2            1 0
     C     DEKY1         CHAIN(N)  PDE01                              99
     C     *IN99         DOWEQ     *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C*** BEG ADD *** 01/29/03 ****************************************
     C     DESTAT        ANDNE     'I'
     C*** END ADD *** 01/29/03 ****************************************
     C                   ADD       1             COUNT2
     C                   ENDIF                                                  DEEOB NE *BLANKS
     C     DEKY1         READE(N)  PDE01                                  99
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C     COUNT2        IFGT      1
     C                   SETON                                        51
     C                   ELSE
     C                   SETOFF                                       51
     C                   ENDIF                                                  COUNT GT 1
     C*
     C* GET CODE DESCRIPTION BASE ON TYPE OF BILL
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
      *** BEG ADD *** 01/18/99 ****************************************
     C     EOBTYP        OREQ      'V'
      *** END ADD *** 01/18/99 ****************************************
     C*** BEG DEL *** 02/17/99 ****************************************
     C**********           MOVELCACPT4    XXCPT4 12 P
     C**********           MOVELCACPT4    K1CPT4    P
     C**********           Z-ADDCAFDAT    KKEFDT
     C**********
     C********** PRKY2     SETGTPPR01
     C********** PRKY      REDPEPPR01                    99
     C**********
     C**N99*****           MOVELPRDSC1    X1DESC 13 P
     C***99*****           MOVE *BLANKS   X1DESC
     C*** END DEL *** 02/17/99 ****************************************
     C*
     C*** BEG ADD *** 02/17/99 ****************************************
     C     DHKY          CHAIN     PDH01                              99
     C     *IN99         IFEQ      *OFF
    C**** Beg Add *** 11/19/15 ***************************************
      /Free
           If DHPRCD = *Blanks and DHNDC <> *Blanks;
              XXCPT4 = DHNDC;
              X1DESC = DHNDCD;
           Else;
      /End-Free
    C**** End Add *** 11/19/15 ***************************************
     C                   MOVEL(P)  DHPRCD        XXCPT4           12
     C                   MOVEL(P)  DHPRDS        X1DESC           13
    C**** Beg Add *** 11/19/15 ***************************************
     C                   ENDIF
    C**** End Add *** 11/19/15 ***************************************
     C                   ENDIF
     C*** END ADD *** 02/17/99 ****************************************
     C*
     C     EOBTYP        WHENEQ    'H'
     C*** BEG DEL *** 02/17/99 ****************************************
     C**********           MOVELCACPT4    KKHCPC
     C**********           MOVE CASVCD    KKRVCD
     C**********           MOVE *ZEROS    KKUNTS
     C**********           Z-ADDCAFDAT    KKEFDT
     C**********           MOVELKKCMSV    XXCPT4    P
     C**********
     C********** SVKY1     SETGTPSV01
     C********** SVKY2     REDPEPSV01                    99
     C**N99*****           MOVELSVDESC    X1DESC    P
     C***99*****           MOVE *BLANKS   X1DESC
     C*** END DEL *** 02/17/99 ****************************************
     C*
     C*** BEG ADD *** 02/17/99 ****************************************
     C     DUKY          CHAIN     PDU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DURVCD        KKRVCD
     C                   MOVE      DURATE        KKHCPC
     C                   MOVE      *BLANKS       KKUNTS
     C                   MOVEL(P)  KKCMSV        XXCPT4
     C                   MOVEL(P)  DUDESC        X1DESC
    C*** BEG ADD *** 04/19/07 ****************************************
     C                   MOVEL     DUPMOD        XXPMOD
    C*** END ADD *** 04/19/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 02/17/99 ****************************************
     C*
     C     EOBTYP        WHENEQ    'X'
     C     DPKY          CHAIN     PDP01                              99
     C     *IN99         IFEQ      *OFF
    C*** BEG ADD *** 04/19/07 ****************************************
     C     DPRXPT        IFEQ      'D'
     C     DPRXPT        OREQ      *BLANK
    C*** BEG ADD *** 01/09/08 ****************************************
     C     DPHCPC        OREQ      *BLANKS
    C*** END ADD *** 01/09/08 ****************************************
    C*** END ADD *** 04/19/07 ****************************************
     C                   MOVEL(P)  DPNDC         XXCPT4
    C*** BEG ADD *** 04/19/07 ****************************************
     C                   ELSE
     C                   MOVEL(P)  DPHCPC        XXCPT4
     C                   ENDIF
    C*** END ADD *** 04/19/07 ****************************************
     C                   MOVEL(P)  DPDESC        X1DESC
     C                   ELSE
     C                   MOVE      *BLANKS       X1DESC
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C*** BEG ADD *** 07/07/99 ****************************************
     C*
     C     EOBTYP        WHENEQ    'D'
     C     DDKY          CHAIN     PDD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DDPRCD        XXCPT4
     C                   MOVEL(P)  DDPRDS        X1DESC
     C                   ELSE
     C                   MOVE      *BLANKS       X1DESC
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD *** 07/07/99 ****************************************
     C*
     C                   ENDSL

     C*** Beg Add ***** 11/22/16 ***************************
    C* Load Bill Import Error Check box

     C                   Eval      scBErr = CAberr
     C*** End Add ***** 11/22/16 ***************************

     C*
     C     *IN21         IFEQ      *ON
     C                   MOVEL     XXLIN#        XALIN#
     C                   MOVEL     X1FDAT        XAFDAT
     C                   MOVEL(P)  XXCPT4        XACPT4
     C                   MOVEL(P)  XXPMOD        XAPMOD
     C                   MOVEL(P)  X1DESC        XADESC
     C                   MOVEL(P)  XADATA        SCDATA
     C                   ENDIF                                                  *IN21 = *ON
     C*
     C     *IN22         IFEQ      *ON
     C                   MOVEL     X1FDAT        XBFDAT
     C                   MOVEL     XXLIN#        XBLIN#
     C                   MOVEL(P)  XXCPT4        XBCPT4
     C                   MOVEL(P)  XXPMOD        XBPMOD
     C                   MOVEL(P)  X1DESC        XBDESC
     C                   MOVEL(P)  XBDATA        SCDATA
     C                   ENDIF                                                  *IN22 = *ON
     C*
     C     *IN23         IFEQ      *ON
     C                   MOVEL     XXCPT4        XCCPT4
     C                   MOVEL(P)  XXPMOD        XCPMOD
     C                   MOVEL(P)  X1DESC        XCDESC
     C                   MOVEL     XXLIN#        XCLIN#
     C                   MOVEL     X1FDAT        XCFDAT
     C                   MOVEL(P)  XCDATA        SCDATA
     C                   ENDIF                                                  *IN23 = *ON
     C*
     C**N23*****           MOVELXXDAT1    SCDATA    P
     C**N23*****           MOVE XXDAT2    SCDATA
     C**********
     C***23*****           MOVELXXDAT2    SCDATA    P
     C***23*****           MOVE XXDAT1    SCDATA
     C*
     C                   Z-ADD     CACHRG        SCCHRG
     C                   Z-ADD     CAPAID        SCPAID
     C                   MOVEL(P)  CARCN         SCRCN
     C*
     C                   MOVE      CAMRA         SCMRA
     C                   MOVE      CANDIS        SCNDIS
     C                   MOVE      CAOTHR        SCOTHR
     C                   MOVE      CAOTH2        SCOTH2
     C*
     C* CALCULATE SUMMATION OF ALL CHARGES FOR BILL
     C*
     C                   EXSR      CALCSM
     C     SCSUM         IFGT      SCTCHG
     C     COUNT         ANDNE     *ZEROS
     C                   SETON                                        33
     C                   ELSE
     C                   SETOFF                                       33
     C                   ENDIF
     C*
     C                   Z-ADD     SCDSEQ        KKDSEQ
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'N'
      *** BEG ADD *** 01/18/99 ****************************************
     C     EOSYS         OREQ      'V'
     C     EOBTYP        ANDEQ     'V'
      *** END ADD *** 01/18/99 ****************************************
     C     DHKY          CHAIN     PDH01                              99
     C                   Z-ADD     DBRRNB        SCDBR2
     C                   ENDIF                                                  EOSYS = 'P'
     C*
     C     EOSYS         IFEQ      'H'
     C     EOBTYP        ANDEQ     'H'
     C     DUKY          CHAIN     PDU01                              99
     C                   Z-ADD     DBRRNE        SCDBR2
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'X'
     C     DPKY          CHAIN     PDP01                              99
     C                   Z-ADD     DBRRNF        SCDBR2
     C                   ENDIF                                                  EOSYS = 'H'
     C*
     C*** BEG ADD *** 07/07/99 ****************************************
     C     EOSYS         IFEQ      'P'
     C     EOBTYP        ANDEQ     'D'
     C     DDKY          CHAIN     PDD01                              99
     C                   Z-ADD     DBRRNG        SCDBR2
     C                   ENDIF                                                  EOSYS = 'H'
     C*** END ADD *** 07/07/99 ****************************************
     C*
     C* CALL SUBROUTINE TO DETERMINE IF THERE ARE ANY ADJUDICATON,
     C* NETWORK OR PRECERT ERRORS ON THE LINE TIME
     C*
     C                   EXSR      ERRCKM
     C*
     C*** BEG ADD *** 10/13/11 ****************************************
     C*
     C     CLTID         CASEQ     '114'         PAYLMT
     C                   ENDCS
     C*
     C*** END ADD *** 10/13/11 ****************************************
     C*
     C* IF PAYMENT AMOUNT IN NOT EQUAL THE CHARGE AMOUNT, CALL
     C* SUBROUTINE AND DO EDIT TO CHECK IF EOB CODE IS ASSIGNED
     C* EXCLUDING FSMRA
     C*
     C     SCPAID        IFNE      SCCHRG
     C*** BEG ADD *** 02/17/00 ****************************************
     C     CADELF        ANDNE     'X'
     C*** END ADD *** 02/17/00 ****************************************
     C                   EXSR      CHRPMT
     C                   ELSE
     C                   SETOFF                                       34
     C                   ENDIF                                                  SCPAID NE SCCHRG
     C*
     C* CALL SUBROUTINE TO XFOOT AMOUNTS, IF DOES NOT X-FOOT SET
     C* INDICATOR TO HI-LITE PAID/CHARGE AMOUNTS
     C*
     C                   EXSR      XFCALC
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - Messages
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   CALL      'F0675R'
     C                   PARM      ' '           ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      @@RRN         ##RRN2
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 07/23/99 **************************************
     C*
     C*================================================================
     C* OPT7 - Network Info
     C*================================================================
     C     OPT7          BEGSR
     C                   CALL      'N0582R'
     C                   PARM      OPTNM         ##PRDS
     C                   PARM      *BLANKS       ##CALL
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCDSEQ
     C                   PARM                    ##RTCD
     C                   PARM                    KKCO#
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C                   SETON                                        90
     C     KHEND         ENDSR
     C*** END ADD *** 07/23/99 ****************************************
     C*================================================================
     C* OPT8 - Work w/Amounts
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'F0397R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      @@RRN         ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C                   PARM      Q1PGM         @@1PGM            6
     C*
     C                   ENDSR
     C*================================================================
     C* OPT11  - EOB/Notes
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   CALL      'F0356R'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      SCDBRR        ##RRN3            9 0
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 02/19/01 **************************************
     C*================================================================
     C* OPT12  - Display State Rule
     C*================================================================
     C     OPT12         BEGSR
     C*
     C***** Beg Add *** 08/05/16 **************************************
     C/free
        //*** Beg Add *** 05/28/25 ******************************
        kkExit = *off;
        Dow kkExit = *off;
        //*** End Add *** 05/28/25 ******************************
        // see if there are more than one rule associated with this line
        xxCntRule = 0;
        Setll (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdseq) prul01;
        ReadE (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdseq) prul01;
        Dow not %eof(farull1);
          xxCntRule += 1;
          ReadE (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdseq) prul01;
        Enddo;

        rrnFR# = *zeros;
        indwdwcall = *off;
        If xxCntRule > 1;

          // show Rule selection window
          //*** Beg Chg *** 06/06/18 ***************************************
          //***rrnCA = %editC(dbrrn:'X');
          Chain(n) (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CArcn:CAdseq) pca03;
          If %Found (fdcal13);
             rrnCA = %editC(dbrrnh:'X');
          Else;
             rrnCA = *zeros;
          Endif;
          //*** End Chg *** 06/06/18 ***************************************
          rrnFR = %editC(rrnFR#:'X');
          kkmode = 'C';
          kktabind = '0';
          //*** Beg Chg *** 05/28/25 ******************************
          //*** callp ShowRuleWdw (rrnCA:rrnFR:kkmode:kktabind);
          callp ShowRuleWdw (rrnCA:rrnFR:kkmode:kktabind:kkExit);
          //*** End Chg *** 05/28/25 ******************************
          indwdwcall = *on;

        EndIf;

        Monitor;
          rrnFR# = %dec(rrnFR:9:0);
        On-Error;
          rrnFR# = *zeros;
        EndMon;

        // default to value selected earlier / rule applied
        kkrrn = DBRRNZ;

        // if user selected a rule from grid, display that
        If rrnFR# > 0;
          kkrrn = rrnFR#;
        EndIf;

        // if user did not select a rule from grid, dont display rule details
        If indwdwcall = *on and rrnFR# = 0;
        Else;

      /end-free
     C***** End Add *** 08/05/16 **************************************
     C*
     C***** Beg Chg *** 10/26/23 **************************************
     C*****              CALL      'F2400R01'
     C*****              PARM      '0'           ##KC              1
     C*****              PARM      '0'           ##KL              1
     C*****              PARM      'D'           ##MODE            1
     C***** Beg Chg *** 08/05/16 **************************************
     C*****              PARM      DBRRNZ        ##RRN             9 0
     C*****              PARM      kkrrn         ##RRN             9 0
     C***** End Chg *** 08/05/16 **************************************
     C*****              PARM      EOAST         ##ST              2
     C*****              PARM      ##CO#         ##CO#             3 0
     C*****              PARM      *ZEROS        ##RRN3            9 0
              //*** Beg Chg *** 05/28/25 ******************************
              //*** @@rrn1    = %char(dbrrnz);
              @@rrn1    = %char(kkrrn);
              //*** End Chg *** 05/28/25 ******************************
              @@mode1   = 'D';
              @@next1   = *blanks;
              @@TabInd1 = *blanks;
              MtStRules(@@rrn1:@@mode1:@@next1:@@TabInd1);
     C***** End Chg *** 10/26/23 **************************************
     C*
     C***** Beg Add *** 08/05/16 **************************************
      /free

        EndIf;

      /end-free
     C***** End Add *** 08/05/16 **************************************
        //*** Beg Add *** 05/28/25 ******************************
        If xxCntRule <= 1;
           kkExit = *on;
        Endif;
        Enddo;
        //*** End Add *** 05/28/25 ******************************
     C*
     C                   ENDSR
     C*** END   ADD *** 02/19/01 **************************************
     C*** BEG ADD *** 01/16/02 ****************************************
     C*================================================================
     C* OPT13  - Display Correct Coding Rule
     C*================================================================
     C     OPT13         BEGSR
     C*
     C* USE RELATIVE RECORD NUMBER FROM API FA714R CALLED IN ERRCK1.
     C* CALL PROGRAM BASED ON FLAG IN FDCAP (CACCDF)
     C*
     C* CHAIN OUT TO FDCAP TO FIND WHICH WORK WITH TO CALL BASE ON
     C* FDCAP(CACCDF)
     C*
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKY4         CHAIN(N)  PCA05                              99
     C*
     C* Convert date to pass in
     C*
     C                   SELECT
     C*
     C     CACCDF        WHENEQ    '1'
     C     CACCDF        OREQ      '2'
     C     CACCDF        OREQ      '5'
     C     CACCDF        OREQ      '6'
     C*
     C* COMPREHENSIVE AND COMPONENT CODE INFO (FMBNP)
     C*
     C                   CALL      'F2533R01'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      ##RRNC        ##RRN             9 0          FNBNP
     C                   PARM      @@RRN         ##RRN4            9 0          FAEOP
     C                   PARM      SCFDAT        ##FDAT            7 0
     C*
     C* MUTUALLY EXCLUSIVE CODES (FMMUP)
     C*
     C     CACCDF        WHENEQ    '3'
     C     CACCDF        OREQ      '4'
     C     CACCDF        OREQ      '7'
     C     CACCDF        OREQ      '8'
     C*
     C                   CALL      'F2534R01'
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      ##RRNC        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN4            9 0          FAEOP
     C                   PARM      SCFDAT        ##FDAT            7 0
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END ADD *** 01/16/02 ****************************************
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   SETOFF                                       5152
     C*** BEG ADD *** 03/05/07 ****************************************
      ***** BEG CHG ******* 06/16/11 **********************************
     C***41****  *INKA     CASEQ*ON       KADO             F1
     C     *INKA         CASEQ     *ON           KADO                           F1
      ***** END CHG ******* 06/16/11 **********************************
     C*** END ADD *** 03/05/07 ****************************************
     C     *INKB         CASEQ     *ON           KBDO                           F2
     C     *INKG         CASEQ     *ON           KGDO                           F7
     C*** BEG ADD *** 06/07/00 ****************************************
     C     *INKH         CASEQ     *ON           KHDO                           F8
     C*** END ADD *** 06/07/00 ****************************************
     C     *INKN         CASEQ     *ON           KNDO                           F14
     C     *INKQ         CASEQ     *ON           KQDO                           F16
     C     *INKS         CASEQ     *ON           KSDO                           F18
     C     *INKT         CASEQ     *ON           KTDO                           F19
     C*** BEG ADD *** 05/16/03 ****************************************
     C     *INKU         CASEQ     *ON           KUDO                           F20
     C*** END ADD *** 05/16/03 ****************************************
     C*** BEGIN ADD *** 01/11/99 **************************************
     C     *INKV         CASEQ     *ON           KVDO                           F21
     C*** END ADD *** 01/11/99 ****************************************
     C*** BEG DEL *** 05/16/03 ****************************************
     C********** *INKW     CASEQ*ON       KWDO             F22
     C*** END DEL *** 05/16/03 ****************************************
     C*** BEG ADD *** 03/05/07 ****************************************
     C   41*INKW         CASEQ     *ON           KWDO                           F22
     C*** END ADD *** 03/05/07 ****************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C*** BEGIN ADD *** 07/23/99 **************************************
     C     SCSEL         CASEQ     '7'           OPT7
     C*** END ADD *** 07/23/99 ****************************************
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '11'          OPT11
     C*** BEG DEL *** 05/24/11 ****************************************
     C*** BEGIN ADD *** 02/19/01 **************************************
     C********   SCSEL     CASEQ'12'      OPT12
     C*** END   ADD *** 02/19/01 **************************************
     C*** BEG ADD *** 01/16/02 ****************************************
     C********   SCSEL     CASEQ'13'      OPT13
     C*** END ADD *** 01/16/02 ****************************************
     C*** END DEL *** 05/24/11 ****************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** BEG ADD *** 02/21/07 ****************************************
     C*
     C* CALL PGM TO UPDATE DIAGNOSIS INDICATORS
     C*
     C                   CALL      'FA744R'
     C                   PARM                    @@RRN
     C                   PARM                    RTNCOD            1
     C*
     C*** END ADD *** 02/21/07 ****************************************
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   SETON                                        LR
     C                   MOVE      '1'           OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* SET PRGRAM VARIABLES
     C*
     C                   MOVE      'N'           IFLAG             1
     C                   MOVE      'N'           DATFL             1
     C*
     C                   SETOFF                                       33
     C                   Z-ADD     0             COUNT             1 0
     C     @@MODE        COMP      'A'                                    40
     C*
     C*** BEG ADD *** 02/15/00 ****************************************
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   MOVEL     '2'           XXMODE            1
     C*
     C                   ENDIF
     C*** END ADD *** 02/15/00 ****************************************
     C*
     C*** BEG ADD *** 03/05/07 ****************************************
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C*** Beg Add ***** 11/22/16 ***************************
     C                   Eval      isBErr  = *Off
     C                   If        IsModEnabled('Line Validation')
     C                   Eval      isBErr  = *On
     C                   EndIf
     C*** End Add ***** 11/22/16 ***************************

     C     XXCLT#        IFEQ      '010'
     C                   OPEN      FDELL1
     C                   MOVE      '1'           *IN41
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C* Check for unprocessed errors.
     C*
     C     EOEOB#        CHAIN     PEL01                              99
     C     *IN99         IFEQ      *OFF
     C                   CALL      'F0400R'
     C                   PARM                    @@MODE
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          faeop
     C                   PARM                    @@PGM            10
     C                   PARM                    @@PGM2           10
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      '0'           *IN41
     C                   ENDIF
     C*
     C*** END ADD *** 03/05/07 ****************************************
     C*
     C                   MOVEL(P)  @@PGM         ACDPGM           10
     C*
     C     @@MODE        IFEQ      'E'
     C                   MOVEL(P)  'F0353R'      ACDPGM
     C                   ENDIF
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL     QCA(2)        QQCMD
     C                   MOVEL     EOAST         QQST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   SETON                                        12
     C*
     C**********           Z-ADD6         SF1PAG  50
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** BEG ADD *** 01/16/02 ****************************************
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C                   PARM      *BLANK        ##OS              1            OUTOME/SAVINGS
     C                   PARM      *BLANK        ##ARLS            1            Auto Release Flag
     C                   PARM      *BLANK        ##CCOR            1            CORRECT CODING
     C*** END ADD *** 01/16/02 ****************************************
     C*** BEGIN ADD *** 08/16/02 **************************************
     C                   PARM      *BLANK        ##CRVW            1
     C                   PARM      *BLANK        ##PROM            1
     C                   PARM      *BLANK        ##MCRV            1
     C                   PARM      *BLANK        ##FREQ            1
     C                   PARM      *BLANK        ##CRAC            1
     C*** END   ADD *** 08/16/02 **************************************
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      EOCO#         #1CO#             3
     C                   MOVE      EOSUB#        #1SUB#            5
     C                   MOVE      EOFND         #1FND             3
     C                   MOVE      EOFYR         #1FYR             3
     C                   MOVE      EOCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCL1HD        PARM      *BLANKS       ##L1             78
     C     SCL2HD        PARM      *BLANKS       ##L2             78
     C*
     C                   CALL      'FA707R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      @@RRN         ##RRN             9 0
     C     SCL3HD        PARM      *BLANKS       ##L1             78
     C*****      SCL4HD    PARM *BLANKS   ##L2   78
     C*
     C                   MOVE      EOEOB#        SCEOB#
     C                   MOVEL(P)  EOAST         SCAST
     C                   MOVE      EOZIPA        SCZIPA
     C                   MOVEL(P)  EOSPCD        SCSPCD
     C                   Z-ADD     EOCHRG        SCTCHG
     C*
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNTWK
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  EONTWK
     C*
     C* CALCULATE SUMMATION OF ALL CHARGES FOR BILL
     C*
     C                   EXSR      CALCSM
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C                   MOVEL(P)  CXHMRA        SCHMRA
     C                   MOVE      ':'           SCHMRA
     C                   MOVEL(P)  CXHNET        SCHNET
     C                   MOVE      ':'           SCHNET
     C                   MOVEL(P)  CXHOT1        SCHOT1
     C                   MOVE      ':'           SCHOT1
     C                   MOVEL(P)  CXHOT2        SCHOT2
     C                   MOVE      ':'           SCHOT2
     C*
     C                   Z-ADD     EORCN         SCHRCN
     C*** BEGIN DEL *** 07/23/99 **************************************
     C********
     C**IF*BILL HAS A PRR-CERT, THEN DISPLAY F18=VIEW PRE-CERT
     C********
     C********   EOCERT    IFNE *BLANKS
     C********             MOVELPCERTF    OFD,18
     C********             ELSE
     C********             MOVELCLRFKY    OFD,18
     C********             ENDIF                           EOCERT NE *BLANKS
     C*** END DEL *** 07/23/99 ****************************************
     C*** BEGIN ADD *** 07/23/99 **************************************
     C*
     C*          Determine if optional functions are to display
     C*          ------------------------------------------------------
     C                   Z-ADD     1             IX                3 0
     C     OF(IX)        DOWNE     '3'
     C                   MOVEA     OFD(IX)       XXOFD            30
      ***** BEG ADD ******* 06/16/11 **********************************
     C                   MOVEL     OFD(IX)       DUMNUM
      *
     C     CLTID         IFEQ      '114'
     C     OF(IX)        ANDEQ     '2'
     C     DUMNUM        ANDEQ     '01'
     C                   MOVEL     C114F1        OFD(IX)
     C                   MOVEA     OFD(IX)       XXOFD            30
     C                   ENDIF
      ***** END ADD ******* 06/16/11 **********************************
     C*
     C*             If network: Display Opt7=Network Info
     C*             ---------------------------------------------------
     C     XXOFD         IFEQ      NETWRK
     C     ##NTWK        IFEQ      'Y'
     C     EONTWK        ANDNE     *BLANKS
     C                   MOVEL     NETWRK        OFD(IX)
     C                   ELSE
     C                   MOVEL     CLRFKY        OFD(IX)
     C                   ENDIF                                                  CERT <> blanks
     C                   ENDIF                                                  OFD = NETWRK
     C*
     C*             If bill has Pre-cert: Display F18=Pre-cert
     C*             ---------------------------------------------------
     C     XXOFD         IFEQ      PCERTF
     C     EOCERT        IFNE      *BLANKS
     C                   MOVEL     PCERTF        OFD(IX)
     C                   ELSE
     C                   MOVEL     CLRFKY        OFD(IX)
     C                   ENDIF                                                  CERT <> blanks
     C                   ENDIF                                                  OFD = PCERTF
     C*
     C* IF CORRECT CODING NOT INSTALLING DONT DISPLAY OPT 13
     C*
     C     IX            IFEQ      10
     C     ##CCOR        ANDEQ     *BLANK
     C*
     C                   MOVE      *BLANKS       OFD(10)
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 03/05/07 **************************************
     C*
     C*             Client 10: Set EDI Flag
     C*             ---------------------------------------------------
     C     XXOFD         IFEQ      SETEDI
     C     XXCLT#        IFEQ      '010'
     C                   MOVEL     SETEDI        OFD(IX)
     C                   ELSE
     C                   MOVEL     CLRFKY        OFD(IX)
     C                   ENDIF
     C                   ENDIF
     C*
     C*             Client 10: Work w/Errors
     C*             ---------------------------------------------------
     C     XXOFD         IFEQ      WRKERR
     C     XXCLT#        IFEQ      '010'
     C                   MOVEL     WRKERR        OFD(IX)
     C                   ELSE
     C                   MOVEL     CLRFKY        OFD(IX)
     C                   ENDIF
     C                   ENDIF
     C*
     C*** END ADD *** 03/05/07 ****************************************
     C*
     C                   ADD       1             IX
     C                   ENDDO                                                  OF,IX <> '3'
     C*** END ADD *** 07/23/99 ****************************************
     C*
     C* CLEAR WARNING MESSAGE FIELD
     C*
     C                   CLEAR                   SCMSG
     C**********           MOVE *BLANKS   ERROR1  1
     C*
     C*** BEGIN ADD *** 07/19/99 **************************************
     C     CMKY          CHAIN     PCM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     CMMMI         IFNE      *ZEROS
     C*
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*** END   ADD *** 07/19/99 **************************************
     C*
     C*** Beg Add *** MC367 *** 11/15/12 ******************************
      /Free
      /If Not Defined(WEB)
       //Disable function F14
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '14';
           Exsr UPDOFA;
       //Disable function F16
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '16';
           Exsr UPDOFA;
       //Disable function F19
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '19';
           Exsr UPDOFA;
       //Disable function F20
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '20';
           Exsr UPDOFA;
       //Disable function F21
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '21';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/15/12 ******************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C* Set to reposition to the top of the page if user is deleting
     C*
     C     @RCDS         IFEQ      1
     C*
     C   21              Z-ADD     SCLIN#        X@LIN#
     C*
     C   22              Z-ADD     SCFDAT        SRDTI
     C   22              CALL      'SRCVTDAT'    DTMDYY
     C   22              MOVEL     SRDTO         X@FDAT
     C   22              Z-ADD     SCLIN#        X@LIN#
     C*
     C*** BEGIN CHG *** 05/24/01 **************************************
     C***23*****           MOVE SCCPT4    X@CPT4    P
     C   23              MOVE(P)   SCCODE        X@CODE
     C*** END   CHG *** 05/24/01 **************************************
     C   23              Z-ADD     SCLIN#        X@LIN#
     C*
     C     X1SEL         IFEQ      '4'
     C                   MOVE      *BLANK        X1SEL
     C                   ENDIF                                                  X1SEL=4
     C*
     C                   ENDIF                                                  @RCDS=1
     C*
     C*** END   ADD *** 02/15/00 **************************************
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C   23KEY1          READE     DBRCDF3                                95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** BEGIN ADD *** 02/15/00 **************************************
     C* Set to reposition to the top of the page if user is deleting
     C     @RCDS         IFEQ      6
     C*
     C   21              Z-ADD     SCLIN#        X@LIN#
     C*
     C   22              Z-ADD     SCFDAT        SRDTI
     C   22              CALL      'SRCVTDAT'    DTMDYY
     C   22              MOVEL     SRDTO         X@FDAT
     C   22              Z-ADD     SCLIN#        X@LIN#
     C*
     C*** BEGIN CHG *** 05/24/01 **************************************
     C***23*****           MOVE SCCPT4    X@CPT4    P
     C   23              MOVE(P)   SCCODE        X@CODE
     C*** END   CHG *** 05/24/01 **************************************
     C   23              Z-ADD     SCLIN#        X@LIN#
     C*
     C     X1SEL         IFEQ      '4'
     C                   MOVE      *BLANK        X1SEL
     C                   ENDIF                                                  X1SEL=4
     C*
     C                   ENDIF                                                  @RCDS =1
     C*
     C*** END   ADD *** 02/15/00 **************************************
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C   23KEY1          READPE    DBRCDF3                                95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C*** BEG CHG *** 04/02/99 ****************************************
     C***22***** KEY3      SETGTDBRCDF2              96
     C***23***** KEY4      SETGTDBRCDF3              96
     C   22KEY5          SETGT     DBRCDF2                            96
     C   23KEY6          SETGT     DBRCDF3                            96
     C*** END CHG *** 04/02/99 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C*** BEGIN ADD *** 02/15/00 **************************************
     C* This code is used for repositioning after adjudication. This
     C* loads the position to field and key values then calls KEYSET
     C     XXSEL         IFEQ      '2'
     C     XXSEL         OREQ      '4'
     C     ##MODE        OREQ      'A'
     C*
     C*
     C   21              MOVE      X@LIN#        SCPST1
     C*
     C   22              MOVE      X@FDAT        SCPST2
     C   22              Z-ADD     X@LIN#        KKLIN#
     C*
     C*** BEGIN CHG *** 05/24/01 **************************************
     C***23*****           MOVE X@CPT4    SCPST3
     C   23              MOVE      X@CODE        SCPST3
     C*** END   CHG *** 05/24/01 **************************************
     C   23              Z-ADD     X@LIN#        KKLIN#
     C*
     C                   EXSR      KEYSET
     C*
     C                   ENDIF
     C*
     C   21KKLIN#        IFEQ      0
     C*
     C                   MOVE      SCPST1        KKLIN#
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 02/15/00 **************************************
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C*** BEG CHG *** 04/02/99 ****************************************
     C***22***** KEY3      SETLLDBRCDF2              96
     C***23***** KEY4      SETLLDBRCDF3              96
     C   22KEY5          SETLL     DBRCDF2                            96
     C   23KEY6          SETLL     DBRCDF3                            96
     C*** END CHG *** 04/02/99 ****************************************
     C*
     C*** BEG ADD *** 11/12/99 ****************************************
     C*
     C* Call Adj API if in Add or Change mode but not if F5 or F11 was
     C* pressed or page up/page down
     C********** @@MODE    IFEQ 'C'
     C********** *INKE     ANDEQ*OFF
     C********** *INKK     ANDEQ*OFF
     C********** *IN01     ANDEQ*OFF
     C********** *IN02     ANDEQ*OFF
     C**********
     C********** @@MODE    OREQ 'A'
     C********** *INKE     ANDEQ*OFF
     C********** *INKK     ANDEQ*OFF
     C********** *IN01     ANDEQ*OFF
     C********** *IN02     ANDEQ*OFF
     C*
     C     XXMODE        IFEQ      '2'
     C     ##MODE        OREQ      'A'
     C     XXSEL         OREQ      '2'
     C     XXSEL         OREQ      '4'
     C*
     C*** BEG ADD *** 07/05/12 ****************************************
     C*
     C                   IF        EOCRNE = 'Y'
     C                   EXSR      RMV_MSG_CDE
     C                   ENDIF
     C*
     C*** END ADD *** 07/05/12 ****************************************
     C*
     C*** BEG ADD *** 12/13/01 ****************************************
     C     ##MODE        IFEQ      'A'
     C                   MOVE      ' '           XXCRT
     C                   ENDIF
     C*** END ADD *** 12/13/01 ****************************************
     C*
     C                   MOVEL     *BLANKS       ##MODE
     C                   MOVEL     *BLANKS       XXMODE
     C                   MOVE      *BLANKS       XXSEL             2
     C*
     C     SFRRN1        IFEQ      *ZEROS
     C*** BEG ADD *** 12/07/99 ****************************************
     C*** This loop deletes FSMRA records in appropriate header file
     C*** and in FDCAP, FDDAP, and FDDEP unless doing a 4=Delete
     C*
     C*** BEG ADD *** 01/07/02 ****************************************
     C     XXCRT         IFEQ      ' '
     C*** BEG ADD *** 08/21/02 ****************************************
     C     EOCRDO        ANDNE     'Y'
     C*** END ADD *** 08/21/02 ****************************************
     C****END*ADD *** 01/07/02 ****************************************
     C*
     C*** BEG ADD *** 08/21/02 ****************************************
     C                   EXSR      DELFSM
     C*** END ADD *** 08/21/02 ****************************************
     C*
     C*** BEG DEL *** 08/21/02 ****************************************
     C*********
     C*********            MOVEL'FSMRA'   KKPRCD
     C*********            MOVEL'FSMRA'   KKRATE
     C****BEG*CHG *** 08/23/01 ****************************************
     C******************** MOVELKKRVCD    K1RVCD
     C*********            Z-ADD*ZEROS    K1RVCD
     C****END*CHG *** 08/23/01 ****************************************
     C****Physician
     C*********
     C*********  EOBTYP    IFEQ 'N'
     C*********
     C*********  DHKY2     CHAINPDH02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********
     C*********            Z-ADDDHEOB#    KKEOB#
     C*********            Z-ADDDHDSEQ    KKDSEQ
     C*********
     C*********  CAKY3     CHAINPCA03                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********  EORCN     ANDEQCARCN
     C*********
     C*********            DELETPCA03
     C*********            DELETPDH02
     C*********
     C*********  DAKY1     CHAINPDA01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            DELETPDA01
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********  DEKY2     CHAINPDE02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********
     C*********            DELETPDE02
     C*********
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********            ELSE                            EORCN=*ZERO
     C*********
     C*********  DAKY1     CHAINPDA01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            DELETPDA01
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********            ENDIF                           EORCN=0
     C*********
     C*********            ENDIF                           *IN60=OFF
     C*********
     C*********            ENDIF                           EOBTYP=N
     C****Hospital
     C*********
     C*********  EOBTYP    IFEQ 'H'
     C*********
     C*********  DUKY2     CHAINPDU02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********
     C*********            Z-ADDDUEOB#    KKEOB#
     C*********            Z-ADDDUDSEQ    KKDSEQ
     C*********
     C*********  CAKY3     CHAINPCA03                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********  EORCN     ANDEQCARCN
     C*********
     C*********            DELETPCA03
     C*********            DELETPDU02
     C*********
     C*********  DAKY1     CHAINPDA01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            DELETPDA01
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********  DEKY2     CHAINPDE02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********
     C*********            DELETPDE02
     C*********
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********            ELSE                            EORCN=*ZERO
     C*********
     C*********  DAKY1     CHAINPDA01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            DELETPDA01
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********            ENDIF                           EORCN=0
     C*********
     C*********            ENDIF                           *IN60=OFF
     C*********
     C*********            ENDIF                           EOBTYP=H
     C****Dental
     C*********
     C*********  EOBTYP    IFEQ 'D'
     C*********
     C*********  DDKY2     CHAINPDD02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********
     C*********            Z-ADDDDEOB#    KKEOB#
     C*********            Z-ADDDDDSEQ    KKDSEQ
     C*********
     C*********  CAKY3     CHAINPCA03                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********  EORCN     ANDEQCARCN
     C*********
     C*********            DELETPCA03
     C*********            DELETPDD02
     C*********
     C*********  DAKY1     CHAINPDA01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            DELETPDA01
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********  DEKY2     CHAINPDE02                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********
     C*********            DELETPDE02
     C*********
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********            ELSE                            EORCN=*ZERO
     C*********
     C*********  DAKY1     CHAINPDA01                60
     C*********
     C*********  *IN60     IFEQ *OFF
     C*********            DELETPDA01
     C*********            ENDIF                           *IN60=*OFF
     C*********
     C*********            ENDIF                           EORCN=0
     C*********
     C*********            ENDIF                           *IN60=OFF
     C*********
     C*********            ENDIF                           EOBTYP=D
     C*** END DEL *** 08/21/02 ****************************************
     C*
     C*** END ADD *** 12/07/99 ****************************************
     C*** BEG ADD *** 04/11/07 ****************************************
     C     EOCPST        IFNE      'Y'
     C*** END ADD *** 04/11/07 ****************************************
     C                   CALL      'FA713R'
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM                    @@RRN
     C*** BEG ADD *** 04/11/07 ****************************************
     C                   ENDIF
     C*** END ADD *** 04/11/07 ****************************************
     C*** BEG DEL *** 08/21/02 ****************************************
     C**********
     C****BEG*ADD *** 12/07/99 ****************************************
     C********** EOAST     IFEQ 'OK'
     C****BEG*ADD *** 12/13/01 ****************************************
     C********** XXCRT     ANDEQ' '
     C****END*ADD *** 12/13/01 ****************************************
     C********** EOAST     OREQ 'AL'
     C********** EOBTYP    ANDEQ'H'
     C****BEG*ADD *** 12/13/01 ****************************************
     C********** XXCRT     ANDEQ' '
     C****END*ADD *** 12/13/01 ****************************************
     C****BEG*DEL *** 05/16/01 ****************************************
     C********** EOAST     OREQ 'GA'
     C********** EOBTYP    ANDEQ'H'
     C*** END DEL *** 05/16/01 ****************************************
     C*** BEG DEL *** 02/17/00 ****************************************
     C********** EOAST     OREQ 'TX'
     C********** EOBTYP    ANDEQ'H'
     C*** END DEL *** 02/17/00 ****************************************
     C* Recreate new FS MRA vs. Payment Difference
     C******************** EXSR CRTDIF
     C******************** END
     C*** END DEL *** 08/21/02 ****************************************
     C*** END ADD *** 12/07/99 ****************************************
     C*
     C*** BEG ADD *** 01/07/02 ****************************************
     C                   ENDIF                                                  XXCRT=' '
     C*** END ADD *** 01/07/02 ****************************************
     C*
     C*** BEG DEL *** 05/09/00 ****************************************
     C**********
     C**WRITE/UPDATE EOMB DETAIL AMOUNT TRANSACTION FILE
     C**********
     C********** CAKY      CHAINPCA04               N99
     C********** *IN99     DOWEQ*OFF
     C**********           Z-ADD0         SMCHG
     C**********           Z-ADD0         SMMRA
     C**********           Z-ADD0         SMNDIS
     C**********           Z-ADD0         SMOTHR
     C**********           Z-ADD0         SMOTH2
     C**********           Z-ADD0         SMPAID
     C**********           Z-ADDC4DSEQ    KKDSEQ
     C********** DAKY      CHAINPDA02                98
     C********** *IN98     DOWEQ*OFF
     C**********           ADD  DACHRG    SMCHG
     C**********           ADD  DAMRA     SMMRA
     C**********           ADD  DANDIS    SMNDIS
     C**********           ADD  DAOTHR    SMOTHR
     C**********           ADD  DAOTH2    SMOTH2
     C**********           ADD  DAPAID    SMPAID
     C********** DAKY      READEPDA02                    98
     C**********           ENDDO                           *IN98 = *OFF
     C**********
     C**COMPARE*THE DETAIL CHARGE AMOUNTS WITH SUM OF DETAIL AMOUNTS
     C**IF*THERE IS A DIFFERENCE, THEN WRITE OR UPDATE DETAIL AMONT REC
     C**********
     C********** C4CHRG    SUB  SMCHG     DFCHRG
     C********** C4MRA     SUB  SMMRA     DFMRA
     C********** C4NDIS    SUB  SMNDIS    DFNDIS
     C********** C4OTHR    SUB  SMOTHR    DFOTHR
     C********** C4OTH2    SUB  SMOTH2    DFOTH2
     C********** C4PAID    SUB  SMPAID    DFPAID
     C**********
     C********** DFCHRG    IFNE *ZEROS
     C********** DFMRA     ORNE *ZEROS
     C********** DFNDIS    ORNE *ZEROS
     C********** DFOTHR    ORNE *ZEROS
     C********** DFOTH2    ORNE *ZEROS
     C********** DFPAID    ORNE *ZEROS
     C**********
     C**UPDATE*IF THERE IS A FDDAP FOR THE CURRENT RECON
     C**ELSE*WRITE NEW FDDAP RECORD TO REFLECT CHANGES
     C**********
     C********** DAKY1     CHAINPDA01                97
     C********** *IN97     IFEQ *OFF
     C****BEG*CHG *** 12/07/99 ****************************************
     C***********          Z-ADDC4CHRG    DACHRG
     C***********          Z-ADDC4MRA     DAMRA
     C***********          Z-ADDC4NDIS    DANDIS
     C***********          Z-ADDC4OTHR    DAOTHR
     C***********          Z-ADDC4OTH2    DAOTH2
     C***********          Z-ADDC4PAID    DAPAID
     C********** DFCHRG    ADD  DACHRG    DACHRG
     C********** DFMRA     ADD  DAMRA     DAMRA
     C********** DFNDIS    ADD  DANDIS    DANDIS
     C********** DFOTHR    ADD  DAOTHR    DAOTHR
     C********** DFOTH2    ADD  DAOTH2    DAOTH2
     C********** DFPAID    ADD  DAPAID    DAPAID
     C****END*CHG *** 12/07/99 ****************************************
     C**********           TIME           DSDTTM
     C**********           MOVELQ1PGM     DAPGM     P
     C**********           MOVELQ1USER    DAUSER    P
     C**********           Z-ADDDSTIME    DATIME
     C**********           Z-ADDDSDATE    SRDTI
     C**********           MOVE *BLANKS   SRDTO
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     DADT
     C**********           UPDATPDA01
     C**********
     C**********           ELSE
     C**********
     C**********           CLEARPDA01
     C**********           Z-ADDKKCO#     DACO#
     C**********           Z-ADDKKSUB#    DASUB#
     C**********           Z-ADDKKFND     DAFND
     C**********           Z-ADDKKFYR     DAFYR
     C**********           Z-ADDKKCASE    DACASE
     C**********           MOVELEOSS#     DASS#
     C**********           Z-ADDEOACDT    DAACDT
     C**********           Z-ADDKKEOB#    DAEOB#
     C**********           Z-ADDKKDSEQ    DADSEQ
     C**********           Z-ADDKKRCN     DARCN#
     C**********           TIME           DSDTTM
     C**********           Z-ADDDSDATE    SRDTI
     C**********           MOVE *BLANKS   SRDTO
     C**********           MOVE *BLANKS   SRERR
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     DATRDT
     C**********           Z-ADDDFCHRG    DACHRG
     C**********           Z-ADDDFMRA     DAMRA
     C**********           Z-ADDDFNDIS    DANDIS
     C**********           Z-ADDDFOTHR    DAOTHR
     C**********           Z-ADDDFOTH2    DAOTH2
     C**********           Z-ADDDFPAID    DAPAID
     C**********           MOVE 'A'       DASTAT
     C**********           MOVELQ1PGM     DAPGM     P
     C**********           MOVELQ1PGM     DAIPGM    P
     C**********           MOVELQ1USER    DAUSER    P
     C**********           MOVELQ1USER    DAIUSR    P
     C**********           Z-ADDDSTIME    DATIME
     C**********           Z-ADDDSTIME    DAITIM
     C**********           MOVELSRDTO     DADT
     C**********           MOVELSRDTO     DAIDT
     C**********           WRITEPDA01
     C**********           ENDIF                           *IN97 = *OFF
     C**********
     C**********           ENDIF                           DFCHRG NE *ZEROS
     C********** CAKY      READEPCA04                    99
     C**********           ENDDO                           *IN99 = *OFF
     C*** END DEL *** 05/09/00 ****************************************
     C*
     C*** BEG ADD *** 05/09/00 ****************************************
     C* Call subroutine to create/update FDDAP if out of balance w/FDCAP
     C                   EXSR      BALAMT
     C*** END ADD *** 05/09/00 ****************************************
     C                   EXSR      CALCSM
     C*
     C                   ENDIF                                                  SFRRN1 = *ZEROS
     C                   ENDIF                                                  @@MODE = C
     C*** END ADD *** 11/12/99 ****************************************
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*** BEG DEL *** 02/23/99 ****************************************
     C**********
     C**DELETE*ALL EOB CODES/NOTES FOR THIS LINE ITEM IN FDDEP
     C**********
     C**********           Z-ADDCACO#     K1CO#
     C**********           Z-ADDCASUB#    K1SUB#
     C**********           Z-ADDCAFND     K1FND
     C**********           Z-ADDCAFYR     K1FYR
     C**********           Z-ADDCACASE    K1CASE
     C**********           Z-ADDCAEOB#    K1EOB#
     C**********           Z-ADDHLDSEQ    K1DSEQ
     C**********
     C**********
     C**DELETE*EOB CODES/NOTES FOR THE LINE ITEM
     C**********
     C********** DEKY      CHAINPDE01                99
     C********** *IN99     DOWEQ*OFF
     C**********           DELETPDE01
     C********** DEKY      READEPDE01                    99
     C**********           ENDDO                           *IN99 = *OFF
     C**********
     C**DELETE*ANY ERRROR MESSAGES FOR THE LINE ITEM
     C**********
     C********** MGKY2     CHAINPMG02                99
     C********** *IN99     DOWEQ*OFF
     C**********           DELETPMG02
     C********** MGKY2     READEPMG02                    99
     C**********           ENDDO                           *IN99 = *OFF
     C*** END DEL *** 02/23/99 ****************************************
     C*
     C                   MOVEL(P)  DELETE        SCDATA
     C                   MOVE(P)   DELETE        SCDATA
     C*
     C*** BEG DEL *** 11/12/99 ****************************************
     C**********
     C**CALL*ADJUDICATION AFTER DELETING A LINE ITEM
     C**********
     C**********           Z-ADD@@RRN     ##RRN
     C**********           CALL 'FA713R'
     C**********           PARM ' '       ##KC
     C**********           PARM ' '       ##KL
     C**********           PARM           ##RRN
     C**********
     C**RECALCUALTE THE SUM OF THE TOTAL CHARGES
     C**********
     C*** END DEL *** 11/12/99 ****************************************
     C*
     C* MUST RE-ESTABLISH WHETHER THE ERROR WARNING MESSAGE SHOULD BE
     C* DISPLAYED.
     C*
     C                   CLEAR                   SCMSG
     C*** BEG CHG *** 12/07/99 ****************************************
     C***********CAKY2     CHAINPCA03               N99
     C     CAKY2         CHAIN(N)  PCA06                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C                   Z-ADD     CADSEQ        KKDSEQ
     C*
     C*** BEG CHG *** 07/05/12 ****************************************
     C*
     C**** MGKY          CHAIN     PMG01                              99
     C     MGKY          CHAIN(N)  PMG01                              99
     C*
     C*** END CHG *** 07/05/12 ****************************************
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEA     MSG           SCMSG
     C                   LEAVE
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C***********CAKY2     READEPCA03                    99
     C     CAKY2         READE(N)  PCA06                                  99
     C*** END CHG *** 12/07/99 ****************************************
     C                   ENDDO                                                  *IN99 = *OFF
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/07/99 ****************************************
     C*================================================================
     C* WRTDEN - Writes dental FSMRA record to FDCAP, FDDAP, & FDDDP
     C*================================================================
     C     WRTDEN        BEGSR
     C*
     C                   Z-ADD     0             XXPAID
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     CAKY4         SETGT     PCA05
     C     CAKY5         READPE(N) PCA05                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     C5DSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY7         SETGT     PCA07
     C     CAKY8         READPE(N) PCA07                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C*  If you are on a recon, it checks to see if there is all ready
     C*  an FSMRA record in FDCAP. If not, then it will write an FSMRA
     C*  record in FDCAP, otherwise, it will update the existing FSMRA
     C*  record.
     C*
     C     EORCN         IFGT      *ZERO
     C*
     C     DDKY2         CHAIN     PDD02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DDEOB#        KKEOB#
     C                   Z-ADD     DDDSEQ        KKDSEQ
     C*
     C     CAKY4         CHAIN     PCA05                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         C5CO#
     C                   Z-ADD     EOFND         C5FND
     C                   Z-ADD     EOFYR         C5FYR
     C                   Z-ADD     EOCASE        C5CASE
     C                   MOVEL     '2'           C5CSCD
     C*** BEG DEL *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5TDAT
     C*** BEG DEL *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5RDAT
     C                   Z-ADD     EOFEIN        C5FEIN
     C                   Z-ADD     EOMOD         C5MOD
     C                   MOVEL     'FSMRA'       C5CPT4
     C                   MOVEL     *BLANKS       C5FLAG
     C                   Z-ADD     EOEOB#        C5EOB#
     C                   MOVEL     EOSS#         C5SS#
     C                   Z-ADD     EOACDT        C5ACDT
     C                   MOVEL     'A'           C5STAT
     C                   MOVEL     Q1USER        C5USER
     C                   MOVEL     Q1PGM         C5PGM
     C                   Z-ADD     DSTIME        C5TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5DT
     C                   MOVEL     *BLANKS       C5DXR
     C                   MOVEL     'C'           C5RCFL
     C                   MOVEL     EOSUB#        C5SUB#
     C                   Z-ADD     *ZERO         C5CHRG
     C                   Z-ADD     *ZERO         C5OTHR
     C                   Z-ADD     *ZERO         C5OTH2
     C                   Z-ADD     *ZERO         C5NDIS
     C                   Z-ADD     HLNEWP        C5PAID
     C     C5CHRG        SUB       C5PAID        C5MRA
     C                   MOVEL     *BLANKS       C5RULE
     C                   MOVEL     *BLANKS       C5OT1C
     C                   MOVEL     *BLANKS       C5OT2C
     C                   Z-ADD     *ZEROS        C5MRAA
     C                   Z-ADD     *ZEROS        C5MNAA
     C                   MOVEL     *BLANKS       C5ADJ
     C                   MOVEL     *BLANKS       C5BPRC
     C                   MOVE      'X'           C5DELF
     C*
     C                   UPDATE    PCA05
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C                   Z-ADD     EOCO#         C5CO#
     C                   Z-ADD     EOFND         C5FND
     C                   Z-ADD     EOFYR         C5FYR
     C                   Z-ADD     EOCASE        C5CASE
     C                   MOVEL     '2'           C5CSCD
     C*** BEG CHG *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5TDAT
     C                   Z-ADD     EOFRDT        C5FDAT
     C                   Z-ADD     EOTODT        C5TDAT
     C*** BEG CHG *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5RDAT
     C                   Z-ADD     EOFEIN        C5FEIN
     C                   Z-ADD     EOMOD         C5MOD
     C                   MOVEL     'FSMRA'       C5CPT4
     C                   MOVEL     *BLANKS       C5FLAG
     C                   Z-ADD     EOEOB#        C5EOB#
     C                   MOVEL     EOSS#         C5SS#
     C                   Z-ADD     EOACDT        C5ACDT
     C                   MOVEL     'A'           C5STAT
     C                   MOVEL     Q1USER        C5USER
     C                   MOVEL     Q1PGM         C5PGM
     C                   Z-ADD     DSTIME        C5TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5DT
     C                   MOVEL     *BLANKS       C5DXR
     C                   MOVEL     'C'           C5RCFL
     C                   MOVEL     EOSUB#        C5SUB#
     C                   Z-ADD     *ZERO         C5OTHR
     C                   Z-ADD     *ZERO         C5OTH2
     C                   Z-ADD     *ZERO         C5NDIS
     C                   Z-ADD     HLNEWP        C5PAID
     C                   Z-ADD     *ZEROS        C5CHRG
     C     C5CHRG        SUB       C5PAID        C5MRA
     C                   MOVEL     EORCN         C5RCN
     C                   MOVEL     *BLANKS       C5RULE
     C                   MOVEL     *BLANKS       C5OT1C
     C                   MOVEL     *BLANKS       C5OT2C
     C                   Z-ADD     *ZEROS        C5MRAA
     C                   Z-ADD     *ZEROS        C5MNAA
     C                   MOVEL     *BLANKS       C5ADJ
     C                   MOVEL     *BLANKS       C5BPRC
     C                   Z-ADD     X1LIN#        C5LIN#
     C                   Z-ADD     XXDSEQ        C5DSEQ
     C                   MOVEL     Q1USER        C5IUSR
     C                   MOVEL     Q1PGM         C5IPGM
     C                   Z-ADD     DSTIME        C5ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5IDT
     C                   Z-ADD     *ZERO         C5DIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C5DELF
     C*
     C                   WRITE     PCA05
     C*  Update FDDDP
     C*
     C     DDKY2         CHAIN     PDD02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DDCO#
     C                   Z-ADD     EOSUB#        DDSUB#
     C                   Z-ADD     EOFND         DDFND
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   MOVEL     C5SS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   Z-ADD     XXDSEQ        DDDSEQ
     C                   Z-ADD     EOEOB#        DDEOB#
     C                   MOVEL     'FSMRA'       DDPRCD
     C*** BEG DEL *** 01/11/02 ****************************************
     C******************** MOVELMRAADJ    DDPRDS
     C*** END DEL *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DDPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DDPRDS
     C*
     C                   ENDIF
     C*** END ADD *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/09/02 ****************************************
     C                   MOVEL     '11'          DDPOS
     C*** END ADD *** 01/09/02 ****************************************
     C                   Z-ADD     *ZEROS        DDUNTS
     C                   MOVEL     *BLANKS       DDPART
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     Q1PGM         DDIPGM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSTIME        DDITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDIDT
     C                   MOVEL     'C'           DDRCFL
     C                   MOVEL     *BLANKS       DDBPRC
     C                   MOVEL     *BLANKS       DDBPRD
     C*
     C                   WRITE     PDD02
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN>0
     C*
     C*  If you are NOT on a recon, then an FSMRA record gets written
     C*  to FDCAP and FDDDP.
     C*
     C     EORCN         IFEQ      0
     C*
     C     CAKY8         CHAIN(N)  PCA03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         C3CO#
     C                   Z-ADD     EOFND         C3FND
     C                   Z-ADD     EOFYR         C3FYR
     C                   Z-ADD     EOCASE        C3CASE
     C                   MOVEL     '2'           C3CSCD
     C*** BEG CHG *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C3FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C3TDAT
     C                   Z-ADD     EOFRDT        C3FDAT
     C                   Z-ADD     EOTODT        C3TDAT
     C*** END CHG *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3RDAT
     C                   Z-ADD     EOFEIN        C3FEIN
     C                   Z-ADD     EOMOD         C3MOD
     C                   MOVEL     'FSMRA'       C3CPT4
     C                   MOVEL     *BLANKS       C3FLAG
     C                   Z-ADD     EOEOB#        C3EOB#
     C                   MOVEL     EOSS#         C3SS#
     C                   Z-ADD     EOACDT        C3ACDT
     C                   MOVEL     'A'           C3STAT
     C                   MOVEL     Q1USER        C3USER
     C                   MOVEL     Q1PGM         C3PGM
     C                   Z-ADD     DSTIME        C3TIME
     C                   MOVEL     Q1USER        C3IUSR
     C                   MOVEL     Q1PGM         C3IPGM
     C                   Z-ADD     DSTIME        C3ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3DT
     C                   MOVEL     SRDTO         C3IDT
     C                   MOVEL     *BLANKS       C3DXR
     C                   MOVEL     'C'           C3RCFL
     C                   MOVEL     EOSUB#        C3SUB#
     C                   Z-ADD     *ZERO         C3OTHR
     C                   Z-ADD     *ZERO         C3OTH2
     C                   Z-ADD     *ZERO         C3NDIS
     C                   Z-ADD     HLNEWP        C3PAID
     C                   Z-ADD     *ZEROS        C3CHRG
     C     C3CHRG        SUB       C3PAID        C3MRA
     C                   MOVEL     EORCN         C3RCN
     C                   MOVEL     *BLANKS       C3RULE
     C                   MOVEL     *BLANKS       C3OT1C
     C                   MOVEL     *BLANKS       C3OT3C
     C                   Z-ADD     *ZEROS        C3MRAA
     C                   Z-ADD     *ZEROS        C3MNAA
     C                   MOVEL     *BLANKS       C3ADJ
     C                   MOVEL     *BLANKS       C3BPRC
     C                   Z-ADD     X1LIN#        C3LIN#
     C                   Z-ADD     XXDSEQ        C3DSEQ
     C                   MOVEL     Q1USER        C3IUSR
     C                   MOVEL     Q1PGM         C3IPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3IDT
     C                   Z-ADD     *ZERO         C3DIFF
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C3DELF
     C*
     C                   WRITE     PCA03
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Update FDDDP
     C*
     C     DDKY2         CHAIN     PDD02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DDCO#
     C                   Z-ADD     EOSUB#        DDSUB#
     C                   Z-ADD     EOFND         DDFND
     C                   Z-ADD     EOFYR         DDFYR
     C                   Z-ADD     EOCASE        DDCASE
     C                   MOVEL     C3SS#         DDSS#
     C                   Z-ADD     EOACDT        DDACDT
     C                   Z-ADD     C3DSEQ        DDDSEQ
     C                   Z-ADD     EOEOB#        DDEOB#
     C                   MOVEL     'FSMRA'       DDPRCD
     C*** BEG DEL *** 01/11/02 ****************************************
     C******************** MOVELMRAADJ    DDPRDS
     C*** END DEL *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DDPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DDPRDS
     C*
     C                   ENDIF
     C*** END ADD *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/09/02 ****************************************
     C                   MOVEL     '11'          DDPOS
     C*** END ADD *** 01/09/02 ****************************************
     C                   Z-ADD     *ZEROS        DDUNTS
     C                   MOVEL     *BLANKS       DDPART
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL     Q1USER        DDUSER
     C                   MOVEL     Q1USER        DDIUSR
     C                   MOVEL     Q1PGM         DDPGM
     C                   MOVEL     Q1PGM         DDIPGM
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSTIME        DDITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDIDT
     C                   MOVEL     'C'           DDRCFL
     C                   MOVEL     *BLANKS       DDBPRC
     C                   MOVEL     *BLANKS       DDBPRD
     C*
     C                   WRITE     PDD02
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDSR
     C*================================================================
     C* WRTHOS - Writes hospital FSMRA record to FDCAP, FDDAP, & FDDUP
     C*================================================================
     C     WRTHOS        BEGSR
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     CAKY4         SETGT     PCA05
     C     CAKY5         READPE(N) PCA05                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     C5DSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY7         SETGT     PCA07
     C     CAKY8         READPE(N) PCA07                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C*  If you are on a recon, it checks to see if there is all ready
     C*  an FSMRA record in FDCAP. If not, then it will write an FSMRA
     C*  record in FDCAP, otherwise, it will update the existing FSMRA
     C*  record.
     C*
     C     EORCN         IFGT      *ZERO
     C*
     C     DUKY2         CHAIN     PDU02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DUEOB#        KKEOB#
     C                   Z-ADD     DUDSEQ        KKDSEQ
     C*
     C     CAKY4         CHAIN     PCA05                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         C5CO#
     C                   Z-ADD     EOFND         C5FND
     C                   Z-ADD     EOFYR         C5FYR
     C                   Z-ADD     EOCASE        C5CASE
     C                   MOVEL     '2'           C5CSCD
     C*** BEG DEL *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5TDAT
     C*** END DEL *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5RDAT
     C                   Z-ADD     EOFEIN        C5FEIN
     C                   Z-ADD     EOMOD         C5MOD
     C                   MOVEL     'FSMRA'       C5CPT4
     C                   MOVEL     *BLANKS       C5FLAG
     C                   Z-ADD     EOEOB#        C5EOB#
     C                   MOVEL     EOSS#         C5SS#
     C                   Z-ADD     EOACDT        C5ACDT
     C                   MOVEL     'A'           C5STAT
     C                   MOVEL     Q1USER        C5USER
     C                   MOVEL     Q1PGM         C5PGM
     C                   Z-ADD     DSTIME        C5TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5DT
     C                   MOVEL     *BLANKS       C5DXR
     C                   MOVEL     'S'           C5RCFL
     C                   MOVEL     EOSUB#        C5SUB#
     C                   Z-ADD     *ZERO         C5CHRG
     C                   Z-ADD     *ZERO         C5OTHR
     C                   Z-ADD     *ZERO         C5OTH2
     C                   Z-ADD     *ZERO         C5NDIS
     C                   Z-ADD     HLNEWP        C5PAID
     C     C5CHRG        SUB       C5PAID        C5MRA
     C                   MOVEL     *BLANKS       C5RULE
     C                   MOVEL     *BLANKS       C5OT1C
     C                   MOVEL     *BLANKS       C5OT2C
     C                   Z-ADD     *ZEROS        C5MRAA
     C                   Z-ADD     *ZEROS        C5MNAA
     C                   MOVEL     *BLANKS       C5ADJ
     C                   MOVEL     *BLANKS       C5BPRC
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C5DELF
     C*
     C                   UPDATE    PCA05
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C                   Z-ADD     EOCO#         C5CO#
     C                   Z-ADD     EOFND         C5FND
     C                   Z-ADD     EOFYR         C5FYR
     C                   Z-ADD     EOCASE        C5CASE
     C                   MOVEL     '2'           C5CSCD
     C*** BEG CHG *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5TDAT
     C                   Z-ADD     EOFRDT        C5FDAT
     C                   Z-ADD     EOTODT        C5TDAT
     C*** END CHG *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5RDAT
     C                   Z-ADD     EOFEIN        C5FEIN
     C                   Z-ADD     EOMOD         C5MOD
     C                   MOVEL     'FSMRA'       C5CPT4
     C                   MOVEL     *BLANKS       C5FLAG
     C                   Z-ADD     EOEOB#        C5EOB#
     C                   MOVEL     EOSS#         C5SS#
     C                   Z-ADD     EOACDT        C5ACDT
     C                   MOVEL     'A'           C5STAT
     C                   MOVEL     Q1USER        C5USER
     C                   MOVEL     Q1PGM         C5PGM
     C                   Z-ADD     DSTIME        C5TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5DT
     C                   MOVEL     *BLANKS       C5DXR
     C                   MOVEL     'S'           C5RCFL
     C                   MOVEL     EOSUB#        C5SUB#
     C                   Z-ADD     *ZERO         C5OTHR
     C                   Z-ADD     *ZERO         C5OTH2
     C                   Z-ADD     *ZERO         C5NDIS
     C                   Z-ADD     HLNEWP        C5PAID
     C                   Z-ADD     *ZEROS        C5CHRG
     C     C5CHRG        SUB       C5PAID        C5MRA
     C                   MOVEL     EORCN         C5RCN
     C                   MOVEL     *BLANKS       C5RULE
     C                   MOVEL     *BLANKS       C5OT1C
     C                   MOVEL     *BLANKS       C5OT2C
     C                   Z-ADD     *ZEROS        C5MRAA
     C                   Z-ADD     *ZEROS        C5MNAA
     C                   MOVEL     *BLANKS       C5ADJ
     C                   MOVEL     *BLANKS       C5BPRC
     C                   Z-ADD     X1LIN#        C5LIN#
     C                   Z-ADD     XXDSEQ        C5DSEQ
     C                   MOVEL     Q1USER        C5IUSR
     C                   MOVEL     Q1PGM         C5IPGM
     C                   Z-ADD     DSTIME        C5ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5IDT
     C                   Z-ADD     *ZERO         C5DIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C5DELF
     C*
     C                   WRITE     PCA05
     C*  Update FDDUP
     C*
     C     DUKY2         CHAIN     PDU02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL     C5SS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     C5DSEQ        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** MOVEL*BLANKS   DUPOS
     C                   MOVE      '11'          DUPOS
     C*** END CHG *** 01/09/02 ****************************************
     C                   Z-ADD     *ZEROS        DURVCD
     C                   MOVEL     'FSMRA'       DURATE
     C*** BEG DEL *** 01/11/02 ****************************************
     C******************** MOVELMRAADJ    DUDESC
     C*** END DEL *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Get description from serv code file or default to MRAADJ const.
     C*
     C                   CLEAR                   KKCMSV
     C                   MOVE      '000'         KKRVCD
     C                   MOVE      '000'         KKUNTS
     C                   MOVEL     'FSMRA'       KKHCPC
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY          READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        DUDESC
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DUDESC
     C*
     C                   ENDIF
     C*** END ADD *** 01/11/02 ****************************************
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** Z-ADD*ZEROS    DUUNTS
     C                   Z-ADD     1             DUUNTS
     C*** END CHG *** 01/09/02 ****************************************
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C                   MOVEL     *BLANKS       DUBRAT
     C                   MOVEL     *BLANKS       DUBPRC
     C                   MOVEL     *BLANKS       DUBPRD
     C*
     C                   WRITE     PDU02
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EORCN>0
     C*
     C*  If you are NOT on a recon, then an FSMRA record gets written
     C*  to FDCAP and FDDUP.
     C*
     C     EORCN         IFEQ      0
     C*
     C     CAKY8         CHAIN(N)  PCA03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         C3CO#
     C                   Z-ADD     EOFND         C3FND
     C                   Z-ADD     EOFYR         C3FYR
     C                   Z-ADD     EOCASE        C3CASE
     C                   MOVEL     '2'           C3CSCD
     C*** BEG CHG *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C3FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C3TDAT
     C                   Z-ADD     EOFRDT        C3FDAT
     C                   Z-ADD     EOTODT        C3TDAT
     C*** END CHG *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3RDAT
     C                   Z-ADD     EOFEIN        C3FEIN
     C                   Z-ADD     EOMOD         C3MOD
     C                   MOVEL     'FSMRA'       C3CPT4
     C                   MOVEL     *BLANKS       C3FLAG
     C                   Z-ADD     EOEOB#        C3EOB#
     C                   MOVEL     EOSS#         C3SS#
     C                   Z-ADD     EOACDT        C3ACDT
     C                   MOVEL     'A'           C3STAT
     C                   MOVEL     Q1USER        C3USER
     C                   MOVEL     Q1PGM         C3PGM
     C                   Z-ADD     DSTIME        C3TIME
     C                   MOVEL     Q1USER        C3IUSR
     C                   MOVEL     Q1PGM         C3IPGM
     C                   Z-ADD     DSTIME        C3ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3DT
     C                   MOVEL     SRDTO         C3IDT
     C                   MOVEL     *BLANKS       C3DXR
     C                   MOVEL     'S'           C3RCFL
     C                   MOVEL     EOSUB#        C3SUB#
     C                   Z-ADD     *ZERO         C3OTHR
     C                   Z-ADD     *ZERO         C3OTH2
     C                   Z-ADD     *ZERO         C3NDIS
     C                   Z-ADD     HLNEWP        C3PAID
     C                   Z-ADD     *ZEROS        C3CHRG
     C     C3CHRG        SUB       C3PAID        C3MRA
     C                   MOVEL     EORCN         C3RCN
     C                   MOVEL     *BLANKS       C3RULE
     C                   MOVEL     *BLANKS       C3OT1C
     C                   MOVEL     *BLANKS       C3OT3C
     C                   Z-ADD     *ZEROS        C3MRAA
     C                   Z-ADD     *ZEROS        C3MNAA
     C                   MOVEL     *BLANKS       C3ADJ
     C                   MOVEL     *BLANKS       C3BPRC
     C                   Z-ADD     X1LIN#        C3LIN#
     C                   Z-ADD     XXDSEQ        C3DSEQ
     C                   MOVEL     Q1USER        C3IUSR
     C                   MOVEL     Q1PGM         C3IPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3IDT
     C                   Z-ADD     *ZERO         C3DIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C3DELF
     C*
     C                   WRITE     PCA03
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Update FDDUP
     C*
     C     DUKY2         CHAIN     PDU02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DUCO#
     C                   Z-ADD     EOSUB#        DUSUB#
     C                   Z-ADD     EOFND         DUFND
     C                   Z-ADD     EOFYR         DUFYR
     C                   Z-ADD     EOCASE        DUCASE
     C                   MOVEL     C3SS#         DUSS#
     C                   Z-ADD     EOACDT        DUACDT
     C                   Z-ADD     C3DSEQ        DUDSEQ
     C                   Z-ADD     EOEOB#        DUEOB#
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** MOVEL*BLANKS   DUPOS
     C                   MOVE      '11'          DUPOS
     C*** END CHG *** 01/09/02 ****************************************
     C                   Z-ADD     *ZEROS        DURVCD
     C                   MOVEL     'FSMRA'       DURATE
     C*** BEG DEL *** 01/11/02 ****************************************
     C******************** MOVELMRAADJ    DUDESC
     C*** END DEL *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Get description from serv code file or default to MRAADJ const.
     C*
     C                   CLEAR                   KKCMSV
     C                   MOVE      '000'         KKRVCD
     C                   MOVE      '000'         KKUNTS
     C                   MOVEL     'FSMRA'       KKHCPC
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY          READPE    PSV01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SVDESC        DUDESC
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DUDESC
     C*
     C                   ENDIF
     C*** END ADD *** 01/11/02 ****************************************
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** Z-ADD*ZEROS    DUUNTS
     C                   Z-ADD     1             DUUNTS
     C*** END CHG *** 01/09/02 ****************************************
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL     Q1USER        DUUSER
     C                   MOVEL     Q1USER        DUIUSR
     C                   MOVEL     Q1PGM         DUPGM
     C                   MOVEL     Q1PGM         DUIPGM
     C                   Z-ADD     DSTIME        DUTIME
     C                   Z-ADD     DSTIME        DUITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUIDT
     C                   MOVEL     *BLANKS       DUBRAT
     C                   MOVEL     *BLANKS       DUBPRC
     C                   MOVEL     *BLANKS       DUBPRD
     C*
     C                   WRITE     PDU02
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDSR
     C*================================================================
     C* WRTMED - Writes medical FSMRA record to FDCAP, FDDAP, & FDDHP
     C*================================================================
     C     WRTMED        BEGSR
     C*
     C* Get the next available seq# & line#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *HIVAL        KKDSEQ
     C*
     C     CAKY4         SETGT     PCA05
     C     CAKY5         READPE(N) PCA05                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     C5DSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   MOVE      *HIVAL        KKLIN#
     C*
     C     CAKY7         SETGT     PCA07
     C     CAKY8         READPE(N) PCA07                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     CALIN#        ADD       1             X1LIN#
     C                   ELSE
     C                   Z-ADD     1             X1LIN#
     C                   ENDIF
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C*  If you are on a recon, it checks to see if there is all ready
     C*  an FSMRA record in FDCAP. If not, then it will write an FSMRA
     C*  record in FDCAP, otherwise, it will update the existing FSMRA
     C*  record.
     C*
     C     EORCN         IFGT      *ZERO
     C*
     C     DHKY2         CHAIN     PDH02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DHEOB#        KKEOB#
     C                   Z-ADD     DHDSEQ        KKDSEQ
     C*
     C     CAKY4         CHAIN     PCA05                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         C5CO#
     C                   Z-ADD     EOFND         C5FND
     C                   Z-ADD     EOFYR         C5FYR
     C                   Z-ADD     EOCASE        C5CASE
     C                   MOVEL     '2'           C5CSCD
     C*** BEG DEL *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5TDAT
     C*** END DEL *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5RDAT
     C                   Z-ADD     EOFEIN        C5FEIN
     C                   Z-ADD     EOMOD         C5MOD
     C                   MOVEL     'FSMRA'       C5CPT4
     C                   MOVEL     *BLANKS       C5FLAG
     C                   Z-ADD     EOEOB#        C5EOB#
     C                   MOVEL     EOSS#         C5SS#
     C                   Z-ADD     EOACDT        C5ACDT
     C                   MOVEL     'A'           C5STAT
     C                   MOVEL     Q1USER        C5USER
     C                   MOVEL     Q1PGM         C5PGM
     C                   Z-ADD     DSTIME        C5TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5DT
     C                   MOVEL     *BLANKS       C5DXR
     C                   MOVEL     'C'           C5RCFL
     C                   MOVEL     EOSUB#        C5SUB#
     C                   Z-ADD     *ZERO         C5OTHR
     C                   Z-ADD     *ZERO         C5CHRG
     C                   Z-ADD     *ZERO         C5OTH2
     C                   Z-ADD     *ZERO         C5NDIS
     C                   Z-ADD     HLNEWP        C5PAID
     C     C5CHRG        SUB       C5PAID        C5MRA
     C                   MOVEL     *BLANKS       C5RULE
     C                   MOVEL     *BLANKS       C5OT1C
     C                   MOVEL     *BLANKS       C5OT2C
     C                   Z-ADD     *ZEROS        C5MRAA
     C                   Z-ADD     *ZEROS        C5MNAA
     C                   MOVEL     *BLANKS       C5ADJ
     C                   MOVEL     *BLANKS       C5BPRC
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C5DELF
     C*
     C                   UPDATE    PCA05
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE                                                   *IN60=*OFF
     C*
     C*** BEG DEL *** 02/17/00 ****************************************
     C********** CAKY4     CHAINPCA05               N60
     C*
     C********** *IN60     IFEQ *OFF
     C*** END DEL *** 02/17/00 ****************************************
     C                   Z-ADD     EOCO#         C5CO#
     C                   Z-ADD     EOFND         C5FND
     C                   Z-ADD     EOFYR         C5FYR
     C                   Z-ADD     EOCASE        C5CASE
     C                   MOVEL     '2'           C5CSCD
     C*** BEG CHG *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C5TDAT
     C                   Z-ADD     EOFRDT        C5FDAT
     C                   Z-ADD     EOTODT        C5TDAT
     C*** END CHG *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5RDAT
     C                   Z-ADD     EOFEIN        C5FEIN
     C                   Z-ADD     EOMOD         C5MOD
     C                   MOVEL     'FSMRA'       C5CPT4
     C                   MOVEL     *BLANKS       C5FLAG
     C                   Z-ADD     EOEOB#        C5EOB#
     C                   MOVEL     EOSS#         C5SS#
     C                   Z-ADD     EOACDT        C5ACDT
     C                   MOVEL     'A'           C5STAT
     C                   MOVEL     Q1USER        C5USER
     C                   MOVEL     Q1PGM         C5PGM
     C                   Z-ADD     DSTIME        C5TIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5DT
     C                   MOVEL     *BLANKS       C5DXR
     C                   MOVEL     'C'           C5RCFL
     C                   MOVEL     EOSUB#        C5SUB#
     C                   Z-ADD     *ZERO         C5OTHR
     C                   Z-ADD     *ZERO         C5OTH2
     C                   Z-ADD     *ZERO         C5NDIS
     C                   Z-ADD     HLNEWP        C5PAID
     C                   Z-ADD     *ZEROS        C5CHRG
     C     C5CHRG        SUB       C5PAID        C5MRA
     C                   MOVEL     EORCN         C5RCN
     C                   MOVEL     *BLANKS       C5RULE
     C                   MOVEL     *BLANKS       C5OT1C
     C                   MOVEL     *BLANKS       C5OT2C
     C                   Z-ADD     *ZEROS        C5MRAA
     C                   Z-ADD     *ZEROS        C5MNAA
     C                   MOVEL     *BLANKS       C5ADJ
     C                   MOVEL     *BLANKS       C5BPRC
     C                   Z-ADD     X1LIN#        C5LIN#
     C                   Z-ADD     XXDSEQ        C5DSEQ
     C                   MOVEL     Q1USER        C5IUSR
     C                   MOVEL     Q1PGM         C5IPGM
     C                   Z-ADD     DSTIME        C5ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C5IDT
     C                   Z-ADD     *ZERO         C5DIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C5DELF
     C*
     C                   WRITE     PCA05
     C*
     C*  Write FDDHP
     C     DHKY2         CHAIN     PDH02                              60
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   Z-ADD     EOCO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   MOVEL     C5SS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   Z-ADD     XXDSEQ        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** MOVEL*BLANKS   DHPOS
     C                   MOVE      '11'          DHPOS
     C*** END CHG *** 01/09/02 ****************************************
     C                   MOVEL     *BLANKS       DHTOS
     C                   MOVEL     'FSMRA'       DHPRCD
     C*** BEG DEL *** 01/11/02 ****************************************
     C******************** MOVELMRAADJ    DHPRDS
     C*** END DEL *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DHPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DHPRDS
     C*
     C                   ENDIF
     C*** END ADD *** 01/11/02 ****************************************
     C                   MOVEL     *BLANKS       DHMOD1
     C                   MOVEL     *BLANKS       DHMOD2
     C                   MOVEL     *BLANKS       DHMOD3
     C                   MOVEL     *BLANKS       DHMOD4
     C*
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** MOVEL*BLANKS   DHDXR1
     C                   MOVEL     'X'           DHDXR1
     C*** END CHG *** 01/09/02 ****************************************
     C                   MOVEL     *BLANKS       DHDXR2
     C                   MOVEL     *BLANKS       DHDXR3
     C                   MOVEL     *BLANKS       DHDXR4
     C*** BEG CHG *** 09/19/01 ****************************************
     C******************** Z-ADD*ZEROS    DHUNTS
     C                   Z-ADD     1             DHUNTS
     C*** END CHG *** 09/19/01 ****************************************
     C                   MOVEL     *BLANKS       DHAREA
     C                   MOVEL     *BLANKS       DHPPLN
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHIDT
     C                   MOVEL     'C'           DHRCFL
     C                   MOVEL     *BLANKS       DHBPRC
     C                   MOVEL     *BLANKS       DHBPRD
     C*
     C                   WRITE     PDH02
     C*
     C                   ENDIF                                                  IN60=ON
     C*
     C*
     C*** BEG DEL *** 02/17/00 ****************************************
     C******************** ENDIF                           *IN60=*OFF
     C*** END DEL *** 02/17/00 ****************************************
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  EORCN>0
     C*
     C*  If you are NOT on a recon, then an FSMRA record gets written
     C*  to FDCAP and FDDHP.
     C*
     C     EORCN         IFEQ      0
     C*
     C     CAKY8         CHAIN(N)  PCA03                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EOCO#         C3CO#
     C                   Z-ADD     EOFND         C3FND
     C                   Z-ADD     EOFYR         C3FYR
     C                   Z-ADD     EOCASE        C3CASE
     C                   MOVEL     '2'           C3CSCD
     C*** BEG CHG *** 03/24/00 ****************************************
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C3FDAT
     C***********          Z-ADDDSDATE    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     C3TDAT
     C                   Z-ADD     EOFRDT        C3FDAT
     C                   Z-ADD     EOTODT        C3TDAT
     C*** BEG CHG *** 03/24/00 ****************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3RDAT
     C                   Z-ADD     EOFEIN        C3FEIN
     C                   Z-ADD     EOMOD         C3MOD
     C                   MOVEL     'FSMRA'       C3CPT4
     C                   MOVEL     *BLANKS       C3FLAG
     C                   Z-ADD     EOEOB#        C3EOB#
     C                   MOVEL     EOSS#         C3SS#
     C                   Z-ADD     EOACDT        C3ACDT
     C                   MOVEL     'A'           C3STAT
     C                   MOVEL     Q1USER        C3USER
     C                   MOVEL     Q1PGM         C3PGM
     C                   Z-ADD     DSTIME        C3TIME
     C                   MOVEL     Q1USER        C3IUSR
     C                   MOVEL     Q1PGM         C3IPGM
     C                   Z-ADD     DSTIME        C3ITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3DT
     C                   MOVEL     SRDTO         C3IDT
     C                   MOVEL     *BLANKS       C3DXR
     C                   MOVEL     'C'           C3RCFL
     C                   MOVEL     EOSUB#        C3SUB#
     C                   Z-ADD     *ZERO         C3OTHR
     C                   Z-ADD     *ZERO         C3OTH2
     C                   Z-ADD     *ZERO         C3NDIS
     C                   Z-ADD     HLNEWP        C3PAID
     C                   Z-ADD     *ZEROS        C3CHRG
     C     C3CHRG        SUB       C3PAID        C3MRA
     C                   MOVEL     EORCN         C3RCN
     C                   MOVEL     *BLANKS       C3RULE
     C                   MOVEL     *BLANKS       C3OT1C
     C                   MOVEL     *BLANKS       C3OT3C
     C                   Z-ADD     *ZEROS        C3MRAA
     C                   Z-ADD     *ZEROS        C3MNAA
     C                   MOVEL     *BLANKS       C3ADJ
     C                   MOVEL     *BLANKS       C3BPRC
     C                   Z-ADD     X1LIN#        C3LIN#
     C                   Z-ADD     XXDSEQ        C3DSEQ
     C                   MOVEL     Q1USER        C3IUSR
     C                   MOVEL     Q1PGM         C3IPGM
     C                   Z-ADD     DSTIME        CAITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C3IDT
     C                   Z-ADD     *ZERO         C3DIFF
     C*
     C* Flag FSMRA so it wont be checked for an EOMB code.
     C                   MOVE      'X'           C3DELF
     C*
     C                   WRITE     PCA03
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*  Update FDDHP
     C*
     C     DHKY2         CHAIN     PDH02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EOCO#         DHCO#
     C                   Z-ADD     EOSUB#        DHSUB#
     C                   Z-ADD     EOFND         DHFND
     C                   Z-ADD     EOFYR         DHFYR
     C                   Z-ADD     EOCASE        DHCASE
     C                   MOVEL     C3SS#         DHSS#
     C                   Z-ADD     EOACDT        DHACDT
     C                   Z-ADD     C3DSEQ        DHDSEQ
     C                   Z-ADD     EOEOB#        DHEOB#
     C*
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** MOVEL*BLANKS   DHPOS
     C                   MOVE      '11'          DHPOS
     C*** END CHG *** 01/09/02 ****************************************
     C                   MOVEL     *BLANKS       DHTOS
     C                   MOVEL     'FSMRA'       DHPRCD
     C*** BEG DEL *** 01/11/02 ****************************************
     C******************** MOVELMRAADJ    DHPRDS
     C*** END DEL *** 01/11/02 ****************************************
     C*** BEG ADD *** 01/11/02 ****************************************
     C*
     C* Get description from proc code file or default to MRAADJ const.
     C*
     C                   MOVEL     'FSMRA'       KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY          READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        DHPRDS
     C*
     C                   ELSE
     C                   MOVEL(P)  MRAADJ        DHPRDS
     C*
     C                   ENDIF
     C*** END ADD *** 01/11/02 ****************************************
     C                   MOVEL     *BLANKS       DHMOD1
     C                   MOVEL     *BLANKS       DHMOD2
     C                   MOVEL     *BLANKS       DHMOD3
     C                   MOVEL     *BLANKS       DHMOD4
     C*
     C*** BEG CHG *** 01/09/02 ****************************************
     C******************** MOVEL*BLANKS   DHDXR1
     C                   MOVEL     'X'           DHDXR1
     C*** END CHG *** 01/09/02 ****************************************
     C                   MOVEL     *BLANKS       DHDXR2
     C                   MOVEL     *BLANKS       DHDXR3
     C                   MOVEL     *BLANKS       DHDXR4
     C*** BEG CHG *** 09/19/01 ****************************************
     C******************** Z-ADD*ZEROS    DHUNTS
     C                   Z-ADD     1             DHUNTS
     C*** END CHG *** 09/19/01 ****************************************
     C                   MOVEL     *BLANKS       DHAREA
     C                   MOVEL     *BLANKS       DHPPLN
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL     Q1USER        DHUSER
     C                   MOVEL     Q1USER        DHIUSR
     C                   MOVEL     Q1PGM         DHPGM
     C                   MOVEL     Q1PGM         DHIPGM
     C                   Z-ADD     DSTIME        DHTIME
     C                   Z-ADD     DSTIME        DHITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DHIDT
     C                   MOVEL     'C'           DHRCFL
     C                   MOVEL     *BLANKS       DHBPRC
     C                   MOVEL     *BLANKS       DHBPRD
     C*
     C                   WRITE     PDH02
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EORCN=0
     C*
     C                   ENDSR
     C*** END ADD *** 12/07/99 ****************************************
     C*================================================================
     C* XFCALC - X-FOOT calculation
     C*================================================================
     C     XFCALC        BEGSR
     C*
     C     SCCHRG        SUB       SCMRA         XXTOTL
     C                   SUB       SCNDIS        XXTOTL
     C                   SUB       SCOTHR        XXTOTL
     C                   SUB       SCOTH2        XXTOTL
     C     XXTOTL        IFNE      SCPAID
     C                   SETON                                        52
     C                   ELSE
     C                   SETOFF                                       52
     C                   ENDIF                                                  XXTOTL NE SCPAID
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 10/13/11 ****************************************
     C*
     C*================================================================
     C* PAYLMT - DETERMINE IF PAYMENT LEVEL REACHED
     C*================================================================
     C*
     C     PAYLMT        BEGSR
     C*
     C* GET EOMB Detail EOB/Notes Transaction File
     C*
     C     DEKY1         SETLL     PDE01
     C     DEKY1         READE     PDE01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFEQ      '230'
     C     DEEOBCD       IFEQ      '230'
     C**** End Chg *** 07/18/23 **************************************
     C                   MOVE      *ON           *IN62
     C                   LEAVE
     C                   ELSE
     C                   MOVE      *OFF          *IN62
     C                   ENDIF
     C*
     C     DEKY1         READE     PDE01                                  99
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*** BEG ADD *** 07/05/12 ****************************************
     C*
     C*================================================================
     C* RMV_MSG_CDE - REMOVE MESSAGE CODES
     C*================================================================
     C*
     C     RMV_MSG_CDE   BEGSR
     C*
     C                   MOVE      *OFF          FOUND             1
     C*
     C     CAKY7         CHAIN     PCA07                              99
     C*
b01  C                   If        Not *IN99
     C*
b02  C                   If        CARCN = EORCN AND CAPAID = *ZEROS
     C*
     C                   movel     cacscd        cscd
     C*
     C     TBLKEY        CHAIN     PCS01                              98
     C*
b03  C                   If        Not *in98 and CSOVRD = 'Y'
     C*
     C     DEKY2         CHAIN(n)  pde02                              97
     C*
b4   C                   DOW       NOT *IN97
     C*
     C**** Beg Chg *** 07/18/23 **************************************
     C****               MOVEL     DEEOB         TESTD             1
     C                   MOVEL     DEEOBCD       TESTD             1
     C**** End Chg *** 07/18/23 **************************************
     C*
b05  C                   If        DEESRC = 'K' and TESTD = 'D'
     C*
     C                   EVAL      FOUND = *ON
     C*
     C                   LEAVE
     C*
e05  C                   ENDIF
     C*
     C     DEKY2         READE(n)  pde02                                  97
     C*
e04  C                   ENDDO
     C*
b04  C                   IF        FOUND = *ON
     C*
     C     MGKY          CHAIN     PMG01                              96
     C*
b5   C                   DOW       NOT *IN96
     C*
b06  C                   IF        MGSRC = 'A' OR MGSRC = 'N' OR MGSRC = 'P'
     C*
     C                   Delete    pmg01
     C*
e06  C                   ENDIF
     C*
     C     MGKY          READE     PMG01                                  96
     C*
e05  C                   ENDDO
     C*
     C     DEKY2         CHAIN     pde02                              95
     C*
b5   C                   DOW       NOT *IN95
     C*
b06  C                   IF        DEESRC = 'A' OR DEESRC = 'N' OR DEESRC = 'P'
     C*
     C                   Delete    PDE02
     C*
e06  C                   ENDIF
     C*
     C     DEKY2         READE     pde02                                  95
     C*
e05  C                   ENDDO
     C*
e04  C                   ENDIF
     C*
e03  C                   ENDIF
     C*
e02  C                   ENDIF
     C*
e01  C                   ENDIF
     C*** Beg Del *** 02/06/13 *********************************************
     C**************
     C**************     MOVE      *LOVAL        KKLIN#
     C**************
     C***21KEY2*****     SETGT     DBRCDF                             96
     C***22KEY5*****     SETGT     DBRCDF2                            96
     C***23KEY6*****     SETGT     DBRCDF3                            96
     C**************
     C*** End Del *** 02/06/13 *********************************************
     C                   ENDSR
     C*================================================================
     C*
     C*** END ADD *** 07/05/12 ****************************************
     C*
     O***PCA03   E                RCA03
** Column headings
Opt Lin#  Dt of Svc   Code       MD Desc          Charged       Paid EOB    Rcn
Opt  Dt of Svc Lin#   Code       MD Desc          Charged       Paid EOB    Rcn
Opt    Code      MD Desc      Lin# Dt of Svc      Charged       Paid EOB    Rcn
** Description--------------MISN
1022=Change                 YNN
1033=Copy                   NNN
1044=Delete                 YNN
1055=Display                YYY
1066=Messages               YYY
1077=Network Info           YYY        Added 07/23/99
1088=Work w/Amounts         YYY
11111=EOB/Notes             YYY
11212=Display State Rule    YYY
11313=Correct Coding Rule   YYY
201F1=Set EDI flag          YYN
202F2=Process               YNN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=Header Info           YYN
208F8=Header EOB/Notes      YYN
211F11=Change View          YYY
212F12=Cancel               NYY
214F14=DARTS                YYY
215F15=Fold/Truncate        YYY
216F16=History              YYY
217F17=Subset               NNN
218F18=View Pre-Cert        YYY
219F19=View Prov info       YYY
220F20=Pended items         YYY
221F21=Dtl Chrg List        YYY
222F22=Work with Errors     YYY
3
**  (QCMDEX)
DLTOVR FILE(FMPRL) LVL(*JOB)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  ) OVRSCOPE(*JOB)
** HI-LITE MESSAGE
THERE ARE MESSAGES ASSOCIATED WITH THE HIGHLIGHTED LINE ITEMS
