     HCopyright('(c) 2019 Sapiens International. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H*** Beg   Change *** 11/21/19 ******************************
     H* dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H*** End   Change *** 11/21/19 ******************************
     H/define CONVERT
     H*
     H* W0162R - Work with Cancellation Batch Detail
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/24/20 C:180 PROJECT WC2710                                 *
     H*                Display File changes only:                     *
     H*                   Change default text for the                 *
     H*                   inclusion/exclusion dropdown                *
     H*                                                               *
     H* 11/21/19 C:180 PROJECT 102WIC                                 *
     H*                Add state inclusion/exclusion dropdown.        *
     H*                                                               *
     H* 02/15/07 C:61  PROJECT 91WNZ                                  *
     H*                When Minimum Balance was used to select
     H*                employer divisions for cancellation, highlight *
     H*                employer divisions with outstanding balance    *
     H*                less than the minimum balance entered as       *
     H*                selection criteria.                            *
     H*                                                               *
     H* 03/07/05 C:136 PROJECT: WC1199 (REVAMP DUNNING II)            *
     H*                DISPLAY  'INCLUDE DEPOSIT' FLAG IN HEADER.     *
     H*                                                               *
     H* 10/27/03 C:33  Project: WC1132                                *
     H*                Revamp Dunning Process.                        *
     H*                                                               *
     H* 06/26/03 C:136 Support: WC1116                                *
     H*                Add code to look at the ACH status in WMDBP.   *
     H*                DBSTAT = 'A'                                   *
     H*                                                               *
     H* 05/02/03 C:61  PROJECT WC1108                                 *
     H*              - Change header from "Minimum balance" to        *
     H*                "% of Premium".                                *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116 (ACH PROCESSING)               *
     H*                Add column to designate if policy is ACH       *
     H*                processing.                                    *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115 (PAYROLL/PREM EXPORT/IMPORT)   *
     H*                Modify if vendor processing is enabled.        *
     H*                CCFREQ <> blanks                               *
     H*                                                               *
     H* 01/30/02 C:61  SUPPORT WA107 31022                            *
     H*                Auto-Refresh subfile display after delete.     *
     H*                                                               *
     H* 09/27/01 C:61  PROJECT WC757 WC577                            *
     H*                Correct subfile record display.                *
     H*                                                               *
     H* 12/02/99 C:90  PROJECT: WC912                                 *
     H*                ADD 'BALANCE TYPE' (CCEAFL) TO HEADER.         *
     H*                                                               *
     H* 10/07/99 C:87  PROJECT: WC894                                 *
     H*                In screen hdgs chg 'Balance date' to 'Billings *
     H*                through date' and put 'Payments through dt'    *
     H*                immediately below.  Adjust other headings in   *
     H*                order to accomodate.                           *
     H*                                                               *
     H* 09/29/99 C:118 PROJECT:  WC939                                *
     H*                Added column heading for premium finance. If   *
     H*                policy is premium financed a 'Y' will be       *
     H*                displayed in this column. Added Opt7=Finance   *
     H*                info for display of premium finance co details.*
     H*                                                               *
     H* 09/02/99 C:61  PROJECT WC577                                  *
     H*                Add option 60 to call A/R Inquiry.             *
     H*                                                               *
     H*  07/22/99 C:87  PROJECT: SUPP WA093 #18935                    *
     H*                 Added emp# and div key fields to resolve      *
     H*                 paging issue for multiple divs w/ same name.  *
     H*                                                               *
     H*  11/24/98 C:87  PROJECT: WC757                                *
     H*                 For each detail line displayed, check if canx *
     H*                 date already exists for employer. If so then  *
     H*                 display line in reverse image.                *
     H*                                                               *
     H*  11/10/98 C:87  PROJECT: WC852                                *
     H*                 Add a column to the first screen, after amount*
     H*                 due, to display the cancellation reason code. *
     H*                 If the code is blank, surround w/asterisks.   *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0162F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWXCNP     IF   E             DISK
     F                                     RENAME(PCN01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWXCNL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCN01:DBRCDF)
     FWXCNL4    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FWXCCP     IF   E             DISK
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWMBXL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     F***** BEGIN ADD **** 09/29/99 ***********************************
     F* Logical for Premium Finance Co.
     F*
     FWMPFL     IF   E           K DISK    INFDS(DBFDSF)
     F*
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDSG)
      *** BEG ADD ***** 04/11/03 **************************************
     FSWCPF     IF   E             DISK
     F***  Begin Del  ***  10/27/03  **************************************
     F**********WMDBL   IF  E           K        DISK
     F***  End   Del  ***  10/27/03  **************************************
      *** END ADD ***** 04/11/03 **************************************
     F***** END   ADD **** 09/29/99 ***********************************
      *** BEG ADD ***** 11/21/19 **************************************
     FSTSTL5    IF   E           K DISK    USROPN
      *** END ADD ***** 11/21/19 **************************************
      /COPY SCOPYWW01D
      *** BET ADD ***** 11/21/19 **************************************
     D/Copy sPrString
      *** END ADD ***** 11/21/19 **************************************
     D*
      **** BEG CHG ******** 04/11/03 **********************************
     D******************* HDA     1   2 78               Column headings line 1
     D HDA             S             78    DIM(3) CTDATA PERRCD(1)              Column headings line
      **** END CHG ******** 04/11/03 **********************************
      **** BEG CHG ******** 02/18/03 **********************************
     D******************* HD      1   6 78               Column headings line 2
      **** BEG CHG ******** 04/11/03 **********************************
     D******************* HD      1   8 78               Column headings line 2
     D HD              S             78    DIM(14) CTDATA PERRCD(1)             Column headings line
      **** END CHG ******** 04/11/03 **********************************
      **** END CHG ******** 02/18/03 **********************************
      ***** BEG CHG ******* 09/02/99 **********************************
     D****************    OF      1  12  1   OFD    35   Options, Function
     D***** BEGIN CHG **** 09/29/99 ***********************************
     D**********          OF      1  13  1   OFD    35   Options, Function
     D***  Begin Chg  ***  10/27/03  **************************************
      ******END*CHG ******* 09/02/99 **********************************
     D**********          OF      1  14  1   OFD    35   Options, Function
     D******END*  CHG **** 09/29/99 ***********************************
     D OF              S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(15) ALT(OF)
     D***  End   Chg  ***  10/27/03  **************************************
     D***  BEGIN ADD  ***  11/24/98  **********************************
     D***  END   ADD  ***  11/24/98  **********************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D***** BEGIN ADD **** 09/29/99 ***********************************
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D DBFDSG          DS
     D  DBRRNG               397    400B 0
     D***** END   ADD **** 09/29/99 ***********************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Cancellation Batch D-
     D                                     etail')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0162R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(2)
     D***  BEGIN CHG  ***  10/07/99  **********************************
     D************  9                     C         SF1PAG
     D SF1PAG          C                   CONST(8)
     D***  END   CHG  ***  10/07/99  **********************************
     D DELETE          C                   CONST('* Deleted')
     D***  Begin Add  ***  10/27/03  **************************************
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D***  End   Add  ***  10/27/03  **************************************
      *
     D***  BEGIN CHG  ***  11/10/98  **********************************
     D*********** DS **********************
     D**************************************  1  65 XXDATA
     D**************************************  1  25 XSENAM
     D**************************************  2  10 XSEMP#
     D************************************** 13  17 XSDIV
     D************************************** 29  31 XSFYR
     D************************************** 28  29 XSRSN
     D************************************** 32  47 XSCOM1
     D************************************** 34  46 XSAMTD
     D************************************** 37  43 XSRPMY
     D************************************** 50  65 XSCOM2
     D************************************** 52  52 XSTYPE
     D************************************** 59  59 XSADJ
     D************************************** 64  64 XSVIP
     D*
     D ASTRSK          C                   CONST('*  *')
     D*
     D***** BEGIN CHG **** 09/29/99 ***********************************
     D**********  DS
     D**********                              1  68 XXDATA
     D**********                              1  25 XSENAM
     D**********                              2  10 XSEMP#
     D**********                             13  17 XSDIV
     D**********                             28  30 XSFYR
     D**********                             28  29 XSRSN
     D**********                             32  47 XSCOM1
     D**********                             34  46 XSAMTD
     D**********                             37  43 XSRPMY
     D**********                             50  65 XSCOM2
     D**********                             49  50 XSCXCD
     D**********                             48  51 XSASTK
     D**********                             56  56 XSTYPE
     D**********                             63  63 XSADJ
     D**********                             68  68 XSVIP
     D***  END   CHG  ***  11/10/98  **********************************
     D                 DS
     D  XXDATA                 1     68
     D  XSENAM                 1     23
     D  XSEMP#                 2     10
     D  XSDIV                 13     17
     D  XSFYR                 26     28
     D***  Begin Chg  ***  10/27/03  **************************************
     D**********                             28  29 XSRSN
     D**********                             32  47 XSCOM1
     D  XSRSN                 26     27
     D  XSCNC#                31     33
     D  XSCOM1                36     51
     D***  End   Chg  ***  10/27/03  **************************************
     D  XSAMTD                32     44
     D  XSRPMY                37     43
      *** BEG ADD **** 02/18/03 **************************************
     D  XSWEDT                34     43
      *** END ADD **** 02/18/03 **************************************
     D***  Begin Chg  ***  10/27/03  **************************************
     D**********                             50  65 XSCOM2
     D  XSCOM2                53     68
     D***  End   Chg  ***  10/27/03  **************************************
     D  XSCXCD                47     48
     D  XSASTK                46     49
     D  XSTYPE                54     54
      *** BEG ADD **** 04/11/03 **************************************
     D  X2TYPE                52     52
     D  X2ACH                 57     57
     D  X2ADJ                 61     61
      *** end ADD **** 04/11/03 **************************************
     D  XSADJ                 60     60
     D  XSPFIN                64     65
     D  XSVIP                 68     68
     D***** END   CHG **** 09/29/99 ***********************************
     D***  BEGIN CHG  ***  11/24/98  **********************************
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Canx dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     I***  END   CHG  ***  11/24/98  **********************************
      *

     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKENAM
     C***  BEGIN ADD  ***  07/22/99  **********************************
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C***  END   ADD  ***  07/22/99  **********************************
      *
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKBAT#
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKBAT#
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
      *
      * KEY LIST FOR THE CANCELLATION NOTICE SELECTION CRITERIA WORKFILE
      *
     C     CCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBAT#
      *
      * KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      * KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      ***** BEG ADD ******* 09/27/01 **********************************
      *
     C     EMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      ***** END ADD ******* 09/27/01 **********************************
      *
      * KEY LIST FOR THE BILLING SCHEDULE CODE AUXILIARY FILE
      *
     C     BXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBSCD
     C*
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  GROUP KEY LIST
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  PREMIUM FINANCE CO KEY LISTS
     C*
     C     PFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFCOD
     C*
     C     PFKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFCOD
     C                   KFLD                    XXPFDT
     C***** END   ADD **** 09/29/99 ***********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CNCO#         KKCO#
     C     *LIKE         DEFINE    CNFND         KKFND
     C     *LIKE         DEFINE    CNBAT#        KKBAT#
     C     *LIKE         DEFINE    CNENAM        KKENAM
     C     *LIKE         DEFINE    CNDATE        KKDATE
     C     *LIKE         DEFINE    CNEMP#        KKEMP#
     C     *LIKE         DEFINE    CNDIV         KKDIV
     C     *LIKE         DEFINE    CCBAT#        XXBAT#
     C     *LIKE         DEFINE    BXBSCD        XXBSCD
     C     *LIKE         DEFINE    CNCO#         XXCO#
     C     *LIKE         DEFINE    CNFND         XXFND
     C     *LIKE         DEFINE    CNFYR         XXFYR
     C     *LIKE         DEFINE    CNEMP#        XXEMP#
     C     *LIKE         DEFINE    CNDIV         XXDIV
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C***** END   ADD **** 09/29/99 ***********************************
     C***  Begin Add  ***  10/27/03  **************************************
     C     *LIKE         DEFINE    CNENAM        U1PDSC
     C     *LIKE         DEFINE    CNENAM        U2PDSC
     C***  End   Add  ***  10/27/03  **************************************
      *
     C                   MOVE      *BLANKS       XXDIVA            5
     C                   MOVE      *BLANKS       XXEMPA            9
     C*================================================================
     C*  PLIST definitions
     C*================================================================
      *
      * ENTRY PARAMETER LIST
      *
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
      *
      * EDIT MASK API PARAMETER LIST
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     SSENAM        IFNE      *BLANKS
     C     SSRECD        ORNE      *BLANKS
     C     SSFYR         ORNE      *ZEROS
     C     SSAMTD        ORNE      *ZEROS
     C     SSCNC#        ORNE      *ZEROS
     C     SSFLAG        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C                   MOVEL     SCPFDT        XXPFDT
     C     PFKY2         SETGT     PPF01
     C     PFKY          READPE    PPF01                                  60
     C*
     C  N90SCSEL         IFEQ      '7'
     C     SCFCOD        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCSEL=7
     C***** END   ADD **** 09/29/99 ***********************************
      ***** BEG ADD ******* 01/30/02 **********************************
      *
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF                                                  SCSEL=7
      *
      ***** END ADD ******* 01/30/02 **********************************
     C                   ENDSR
      *
      *
      *
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C                   CLEAR                   SSENAM
     C                   CLEAR                   SSRECD
     C                   CLEAR                   SSFYR
     C                   CLEAR                   SSAMTD
     C                   CLEAR                   SSCNC#
     C                   CLEAR                   SSFLAG
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
      *
     C     @@RRN         CHAIN     PCC01                              60
      *
     C                   MOVE      CCCO#         KKCO#
     C                   MOVE      CCFND         KKFND
     C                   MOVE      CCBAT#        KKBAT#
     C                   MOVE      *BLANKS       KKENAM
     C                   Z-ADD     0             KKEMP#
     C                   Z-ADD     0             KKDIV
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
      *
     C                   MOVEL     SCPST1        KKENAM
     C                   MOVE      SCPST2        KKEMP#
     C                   MOVE      SCPSTA        KKDIV
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVEL     SCENAM        KKENAM
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
      *
      ***** BEG ADD ******* 09/27/01 **********************************
      *
      *  If employer already has canx date in emp master file then
      *  display info in reverse image
      *
     C     EMKY1         CHAIN     PEM01                              60
      *
      *  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
      *
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
      *
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
      *  Determine if cancellation date exists, set canx indicator
      *
     C     XI            IFEQ      *ZERO
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C     CX(XI)        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
      *
      ***** END ADD ******* 09/27/01 **********************************
      ***** BEG ADD ******* 02/15/07 **********************************
      *
     C     CLTID         IFEQ      '091'
     C     SCAMTD        ANDLT     SCMIN
     C     CCADPP        ANDNE     'P'
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CLTID = 091
      *
      ***** END ADD ******* 02/15/07 **********************************
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
      ***** BEG ADD ******* 02/15/07 **********************************
     C                   Z-ADD     CNAMTD        SCAMTD
      ***** END ADD ******* 02/15/07 **********************************
     C                   MOVE      CNENAM        SCENAM
     C                   MOVE      CNEMP#        SCEMP#
     C                   MOVE      CNDIV         SCDIV
     C                   MOVE      CNFYR         SCFYR
     C                   MOVE      CNSELC        SCSELC
     C                   MOVE      *BLANKS       XXDATA
      *
     C     VIEW#         IFEQ      1
      *
     C                   MOVEL     CNENAM        XSENAM
      *
     C                   ELSE
      *
     C                   MOVEL(P)  CNEMP#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
      *
     C                   CALL      CHGNUM        CHGPL
      *
     C                   MOVEL     XXNUM         XSEMP#
     C                   MOVEL(P)  CNDIV         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
      *
     C                   CALL      CHGNUM        CHGPL
      *
     C                   MOVEL     XXNUM         XSDIV
      *
     C                   ENDIF                                                  VIEW# IFEQ 1
      *
     C     INFO#         IFEQ      1
      *
     C                   MOVEL(P)  CNFYR         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
      *
     C                   CALL      CHGNUM        CHGPL
      *
     C                   MOVEL     XXNUM         XSFYR
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C     CNRECD        IFNE      *BLANK
     C                   MOVE      CNRECD        XSCXCD
     C                   ELSE
     C                   MOVE      ASTRSK        XSASTK
     C                   ENDIF
     C***  END   ADD  ***  11/10/98  **********************************
      *
     C     CNSRYM        IFEQ      0
     C*** BEG ADD **** 02/18/03 ***************************************
     C     CNWEDT        ANDEQ     0
     C*** END ADD **** 02/18/03 ***************************************
      *
     C                   MOVEL(P)  CNAMTD        XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XSAMTD
      *
     C                   ELSE
      *
     C*** BEG ADD **** 02/18/03 ***************************************
     C                   SELECT
     C     *IN41         WHENEQ    *OFF
     C*** END ADD **** 02/18/03 ***************************************
     C                   MOVE      CNSRYM        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'RPMY'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XSRPMY
     C*** BEG ADD **** 02/18/03 ***************************************
     C     *IN41         WHENEQ    *ON
     C                   MOVE      CNWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XSWEDT
     C                   ENDSL
     C*** END ADD **** 02/18/03 ***************************************
      *
     C                   ENDIF                                                  CNSRYM IFEQ
      *
      *  MARK ACCOUNTS WHICH ARE ON AN ANNUAL INSTALLMENT PLAN
      *
     C                   MOVE      CNCO#         XXCO#
     C                   MOVE      CNFND         XXFND
     C                   MOVE      CNFYR         XXFYR
     C                   MOVE      CNEMP#        XXEMP#
     C                   MOVE      CNDIV         XXDIV
     C                   MOVE      *BLANK        XSTYPE
      *** BEG ADD ***** 04/11/03 *************************************
     C                   MOVE      *BLANK        X2TYPE
      *** END ADD ***** 04/11/03 *************************************
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     DBRRN3        SCDBR2
     C                   MOVEL     ELBSCD        XXBSCD
     C*
      *
     C     BXKY          CHAIN     PBX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     BXTYPE        ANDEQ     'A'
      *
      *** BEG CHG ***** 04/11/03 *************************************
     C*******************  MOVE BXTYPE    XSTYPE           ANNUAL INSTALLMT PLAN
     C  N42              MOVE      BXTYPE        XSTYPE                         ANNUAL INSTALLMT PLA
     C   42              MOVE      BXTYPE        X2TYPE                         ANNUAL INSTALLMT PLA
      *** END CHG ***** 04/11/03 *************************************
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
      *** BEG CHG ***** 04/11/03 *************************************
     C*******************  MOVE CNADJ     XSADJ
     C     *IN42         IFEQ      *OFF
     C                   MOVE      CNADJ         XSADJ
     C                   ELSE
     C                   MOVE      CNADJ         X2ADJ
     C***  Begin Chg  ***  10/27/03  **************************************
     C********** EMKY      CHAINPDB01                60
      ****BEG*CHG ***** 06/26/03 *****************************************
     C**N60**************  MOVE 'Y'       X2ACH
     C***60**************  MOVE *BLANK    X2ACH
     C********** *IN60     IFEQ *OFF
     C********** DBSTAT    ANDEQ'A'
     C**********           MOVE 'Y'       X2ACH
     C**********           ELSE
     C**********           MOVE *BLANK    X2ACH
     C**********           ENDIF
      ****BEG*CHG ***** 06/26/03 *****************************************
     C                   MOVE      CNACH         X2ACH
     C***  End   Chg  ***  10/27/03  **************************************
     C                   ENDIF
      *** END CHG ***** 04/11/03 *************************************
     C                   MOVEL     CNVIP         XSVIP
      *
     C                   ELSE
      *
     C                   MOVE      CNRECD        XSRSN
     C                   MOVEL     CNCOM1        XSCOM1
     C                   MOVEL     CNCOM2        XSCOM2
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C                   MOVEL(P)  CNCNC#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XSCNC#
     C***  Begin Add  ***  10/27/03  **************************************
      *
     C                   ENDIF                                                  INFO# IFEQ
      *
     C***  Begin Chg  ***  10/27/03  **************************************
     C******BEGIN ADD **** 09/29/99 ***********************************
     C***Load*'Y' if policy is premium financed
     C**********
     C**********           MOVE CNCO#     XXCO#
     C**********           MOVE CNFND     XXFND
     C**********           MOVE CNFYR     XXFYR
     C**********           MOVE CNEMP#    XXEMP#
     C**********           MOVE CNDIV     XXDIV
     C********** ELKY      CHAINPEL01                60
     C**********
     C**********           MOVELELPFIN    SCFCOD
     C**********           MOVELELPFDT    XXPFDT
     C**********           MOVELELPFDT    SCPFDT
     C**********
     C********** SCFCOD    IFNE *BLANKS
     C**********
     C********** PFKY2     SETGTPPF01
     C**********
     C********** PFKY      REDPEPPF01                    60
     C********** *IN60     IFEQ *OFF
     C********** INFO#     ANDEQ1
     C**********           MOVE 'Y'       XSPFIN
     C**********           ENDIF                           *IN60=OFF
     C**********           ENDIF                           SCFCOD<>BLKS
     C******END*  ADD **** 09/29/99 ***********************************
     C     INFO#         IFEQ      1
     C                   MOVE      CNPFIN        XSPFIN
     C                   ENDIF
     C***  Begin Chg  ***  10/27/03  **************************************
     C*
     C                   MOVE      XXDATA        SCDATA
      *
      * Set up approriate screen headers based on VIEW#, INFO# & SCRPMY
      *
     C***** BEG CHG ****** 02/18/03 ***********************************
     C********** INFO#     IFEQ 1
     C  N41INFO#         IFEQ      1
     C***** END CHG ****** 02/18/03 ***********************************
      *
      *** BEG CHG ***** 04/11/03 **************************************
     C*******************  MOVE HDA,1     SCHD1A
     C  N42              MOVE      HDA(1)        SCHD1A
     C   42              MOVE      HDA(3)        SCHD1A
      *** END CHG ***** 04/11/03 **************************************
      *
     C     VIEW#         IFEQ      1
      *
     C     CCRPMY        IFEQ      0
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,1      SCHDL1
     C  N42              MOVE      HD(1)         SCHDL1
     C   42              MOVE      HD(9)         SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ELSE
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,2      SCHDL1
     C  N42              MOVE      HD(2)         SCHDL1
     C   42              MOVE      HD(10)        SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ENDIF                                                  CCRPMY IFEQ
      *
     C                   ELSE
      *
     C     CCRPMY        IFEQ      0
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,3      SCHDL1
     C  N42              MOVE      HD(3)         SCHDL1
     C   42              MOVE      HD(11)        SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ELSE
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,4      SCHDL1
     C  N42              MOVE      HD(4)         SCHDL1
     C   42              MOVE      HD(12)        SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ENDIF                                                  CCRPMY IFEQ
      *
     C                   ENDIF                                                  VIEW# IFEQ
      *
     C                   ELSE
      *
     C                   MOVE      HDA(2)        SCHD1A
      *
     C     VIEW#         IFEQ      1
     C                   MOVE      HD(5)         SCHDL1
     C                   ELSE
     C                   MOVE      HD(6)         SCHDL1
     C                   ENDIF                                                  VIEW# IFEQ
      *
     C                   ENDIF                                                  INFO# IFEQ
     C*
     C***** BEG ADD ****** 02/18/03 ***********************************
     C   41INFO#         IFEQ      1
     C***** END CHG ****** 02/18/03 ***********************************
      *
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HDA,1     SCHD1A
     C  N42              MOVE      HDA(1)        SCHD1A
     C   42              MOVE      HDA(3)        SCHD1A
     C*** END CHG **** 04/11/03 ****************************************
      *
     C     VIEW#         IFEQ      1
      *
     C     CCWEDT        IFEQ      0
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,1      SCHDL1
     C  N42              MOVE      HD(1)         SCHDL1
     C   42              MOVE      HD(9)         SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ELSE
     C*** BEG CHG **** 04/11/03 ****************************************
     C******************** MOVE HD,7      SCHDL1
     C  N42              MOVE      HD(7)         SCHDL1
     C   42              MOVE      HD(13)        SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ENDIF                                                  CCRPMY IFEQ
      *
     C                   ELSE
      *
     C     CCWEDT        IFEQ      0
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,3      SCHDL1
     C  N42              MOVE      HD(3)         SCHDL1
     C   42              MOVE      HD(11)        SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ELSE
     C*** BEG CHG **** 04/11/03 ****************************************
     C*******************  MOVE HD,8      SCHDL1
     C  N42              MOVE      HD(8)         SCHDL1
     C   42              MOVE      HD(14)        SCHDL1
     C*** END CHG **** 04/11/03 ****************************************
     C                   ENDIF                                                  CCRPMY IFEQ
      *
     C                   ENDIF                                                  VIEW# IFEQ
      *
     C                   ELSE
      *
     C                   MOVE      HDA(2)        SCHD1A
      *
     C     VIEW#         IFEQ      1
     C                   MOVE      HD(5)         SCHDL1
     C                   ELSE
     C                   MOVE      HD(6)         SCHDL1
     C                   ENDIF                                                  VIEW# IFEQ
      *
     C                   ENDIF                                                  INFO# IFEQ
     C***** END ADD ****** 02/18/03 ***********************************
     C*
     C***  BEGIN ADD  ***  11/24/98  **********************************
     C*  If employer already has canx date in emp master file then
     C*  display info in reverse image
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
     C*
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      ELPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date exists, set canx indicator
     C     XI            IFEQ      *ZERO
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C     CX(XI)        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN89
     C                   ELSE
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CX,XI=*ZEROS
     C                   ENDIF                                                  XI IFEQ *ZERO
     C***  END   ADD  ***  11/24/98  **********************************
      ***** BEG ADD ******* 02/15/07 **********************************
      *
     C     CLTID         IFEQ      '091'
     C     SCAMTD        ANDLT     SCMIN
     C     CCADPP        ANDNE     'P'
     C                   MOVE      *ON           *IN89
     C                   ENDIF                                                  CLTID = 091
      *
      ***** END ADD ******* 02/15/07 **********************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * OPT6 - SUBROUTINE FOR OPTION 6
      *===============================================================
      *
      * OPTION 6=EMPLOYER INFO
      *
     C     OPT6          BEGSR
      *
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'Z'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      0             ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM                    ##VIEW            1 0
     C                   PARM      DBRRNE        ##RRN3            9 0
      *
     C                   ENDSR                                                  OPT6
      *
     C***** BEGIN ADD **** 09/29/99 **********************************
     C*===============================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7
     C*===============================================================
     C*
     C*  OPTION 7 = FINANCE INFO
     C*
     C     OPT7          BEGSR
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   CALL      'W0045R02'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      'D'           @@MODE            1
     C                   PARM      DBRRNF        @@RRN             9 0          WMPFP
     C                   PARM      DBRRNG        @@RRN2            9 0          WMFNP
     C*
     C                   ENDSR
     C***** END   ADD **** 09/29/99 **********************************
      *===============================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *===============================================================
      *
      * OPTION 20=TOGGLE SELECT FOR CX FLAG
      *
     C     OPT20         BEGSR
      *
     C                   MOVE      'S'           ##MODE
      *
     C                   CALL      ACDPGM        ACDPL                  99
      *
     C                   ENDSR                                                  OPT20
      ***** BEG ADD ******* 09/02/99 **********************************
      *
      *
      *
      *===============================================================
      * OPT60 - A/R Inquiry
      *===============================================================
     C     OPT60         BEGSR
      * zzz
     C     SCDBRR        CHAIN     PFRCDF                             60
      *
     C                   MOVE      CNCO#         #1CO#
     C                   MOVE      CNFND         #1FND
     C                   MOVE      CNEMP#        #1EMP#
     C                   MOVE      CNDIV         #1DIV
     C                   MOVE      CNFYR         #1FYR
      *
     C                   CALL      'W0612R'                             99
     C     *INKC         PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM      '2'           #1MODE            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1EMP#            9
     C                   PARM                    #1DIV             5
     C                   PARM                    #1FYR             3
     C                   PARM      *ZEROS        #1DATE            8
     C                   PARM      *ZEROS        #1AM              9
     C                   PARM      *ZEROS        #1CHK#            7
     C                   PARM      *BLANK        #1RTC             1
      *
     C                   ENDSR                                                  OPT60
      ***** END ADD ******* 09/02/99 **********************************
      *
      *
      *
      *===============================================================
      * OPT62 - SUBROUTINE FOR OPTION 62
      *===============================================================
      *
      * OPTION 62=CANCELLATION HISTORY
      *
     C     OPT62         BEGSR
      *
     C                   CALL      'W0168R'
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      '1'           @@CALL            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      SCDBR2        @@RRN2            9 0
      *
     C                   ENDSR                                                  OPT62
      *
      *
      *
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
      *
     C                   MOVE      SCDBRR        ##RRN
      *
     C     SCSEL         CASEQ     '6'           OPT6
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C     SCSEL         CASEQ     '7'           OPT7
     C***** END   ADD **** 09/29/99 ***********************************
     C     SCSEL         CASEQ     '20'          OPT20
      ***** BEG ADD ******* 09/02/99 **********************************
     C     SCSEL         CASEQ     '60'          OPT60
      ***** END ADD ******* 09/02/99 **********************************
     C     SCSEL         CASEQ     '62'          OPT62
     C                   ENDCS                                                  SCSEL CASEQ
     C*
     C                   ENDSR
      *
      *
      *
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *** BEG ADD **** 02/18/03 ******************************************
     C                   SETON                                        LR
      *** END ADD **** 02/18/03 ******************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
      ***** BEG CHG ******* 05/02/03 **********************************
     C     CCADPP        COMP      'P'                                    43
      ***** END CHG ******* 05/02/03 **********************************
      *
     C     CCRPMY        COMP      0                                  4040
      *** BEG ADD **** 02/18/03 ******************************************
     C     CCFREQ        COMP      *BLANKS                            4141
     C   41CCWEDT        COMP      0                                  4040
      *
     C   41              SELECT
     C     CCFREQ        WHENEQ    'B'
     C                   MOVEL     'Both'        SCFREQ
     C     CCFREQ        WHENEQ    'W'
     C                   MOVEL     'Weekly'      SCFREQ
     C     CCFREQ        WHENEQ    'M'
     C                   MOVEL     'Monthly'     SCFREQ
     C                   ENDSL
      *** END ADD **** 02/18/03 ******************************************
      *
      *** BEG ADD **** 04/11/03 ******************************************
     C                   READ      SWCPF                                  60
     C  N60WCFL24        COMP      'Y'                                    42
      *** END ADD **** 04/11/03 ******************************************
      *
      * MOVE HEADER FIELDS TO SCREEN HERE SO THAT THEY ARE DISPLAYED
      * EVEN IF THERE ARE NO DETAIL RECORDS FOR THE BATCH
      *
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCNAME
     C                   MOVE      CCBAT#        SCBAT#
     C                   MOVE      CCETYP        SCETYP
      **** BEGIN ADD **** 12/02/99 ************************************
     C                   MOVE      CCEAFL        SCEAFL
      **** END   ADD **** 12/02/99 ************************************
      **** BEG ADD ***** 03/07/05 *************************************
     C                   MOVE      CCINDP        SCINDP
      **** END ADD ***** 03/07/05 *************************************
     C                   MOVE      CCRSN         SCRSN
     C                   MOVE      CCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C                   MOVE      CCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
      *
     C     CCRPMY        IFNE      0
      *** BEG ADD **** 02/18/03 ******************************************
     C     *IN41         ANDEQ     *OFF
      *** END ADD **** 02/18/03 ******************************************
      *
     C                   MOVE      CCRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         SCRPMY
      *
     C                   ELSE
      *
      ***** BEG CHG ******* 05/02/03 **********************************
     C*********            MOVE CCMIN     SCMIN
     C                   Z-ADD     CCMIN         SCMIN
      ***** END CHG ******* 05/02/03 **********************************
     C                   MOVE      CCBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBLDT
     C***  BEGIN ADD  ***  10/07/99  **********************************
     C                   MOVE      CCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C***  END   ADD  ***  10/07/99  **********************************
      *
     C                   ENDIF                                                  CCRPMY IFNE
      *
      *** BEG ADD **** 02/18/03 ******************************************
     C   41CCWEDT        IFNE      0
     C                   MOVE      CCWEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCWEDT
     C                   ENDIF                                                  CCRPMY IFNE
      *** END ADD **** 02/18/03 ******************************************
      *
      **** Begin Add ***  11/21/19 ************************************
      /Free
       LoadSTcodes();
      /End-Free
      **** End   Add ***  11/21/19 ************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
      *
     C   21KEY1A         READE     DBRCDF                                 95
     C   22KEY1A         READE     PCN01                                  95
      *
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
      *
     C   21KEY1A         READPE    DBRCDF                                 95
     C   22KEY1A         READPE    PCN01                                  95
      *
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
      *
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     PCN01                              96
      *
     C                   ENDSR                                                  BEGSR SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
      *
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     PCN01                              96
      *
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
      *
     C                   MOVEL(P)  DELETE        SCDATA
     C                   MOVE      *BLANKS       SCSELC
      *
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
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
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C***  Begin Add  ***  10/27/03  **************************************
     C     SSENAM        IFNE      *BLANKS
     C     LO:UP         XLATE(P)  CNENAM        U1PDSC
     C     LO:UP         XLATE(P)  SSENAM        U2PDSC
     C                   EXSR      STRING
     C                   ENDIF
     C*
     C     SSRECD        IFNE      *BLANKS
     C     SSRECD        ANDNE     CNRECD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFYR         IFNE      *ZEROS
     C     SSFYR         ANDNE     CNFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSAMTD        IFNE      *ZEROS
     C     SSAMTD        ANDGT     CNAMTD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCNC#        IFNE      *ZEROS
     C     SSCNC#        ANDNE     CNCNC#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   SELECT
     C     SSFLAG        WHENEQ    'A'
     C     CNAIP         ANDNE     'A'
     C                   MOVE      *OFF          MEETSS
     C     SSFLAG        WHENEQ    'C'
     C     CNACH         ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C     SSFLAG        WHENEQ    'N'
     C     CNADJ         ANDNE     'R'
     C                   MOVE      *OFF          MEETSS
     C     SSFLAG        WHENEQ    'P'
     C     CNPFIN        ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C     SSFLAG        WHENEQ    'V'
     C     CNVIP         ANDNE     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
     C***  End   Add  ***  10/27/03  **************************************
     C*
     C                   ENDSR
      **  Begin Add  ***  11/21/19  ***********************************
      *//===================================================================
      *// LoadSTCodes - Load State codes dropdown
      *//===================================================================
     p LoadSTCodes     b
     d LoadSTCodes     pi
     d strSTdesc       s             30
     D i               s              3  0 inz
     D kkSt            s             10    inz
     D StateListDS     DS           150
     D   xxStList                   150
     D   AryStList                    2    DIM(75) OVERLAY(xxStList)
      /Free

       If Not %Open(STSTL5);
          Open STSTL5;
       Endif;

       xxStList = ccStList;
       scSTsel = *Blanks;
       scSTval = *Blanks;
       strSTdesc = *Blanks;

       If ccStInEx <> *blanks;
          For i = 1 to 75;
             kkst = AryStList(i);
             If kkst <> *blanks;
                chain kkSt PST01;
                If %found(STSTL5);
                   strSTdesc = TitleCase(STname);
                   If scSTsel = *Blanks;
                      scSTsel = %trim(STkey)+'-'+%trim(strSTdesc);
                      scSTval = %trim(STkey);
                   else;
                      scSTsel=%trim(scSTsel)+','+%trim(STkey)+'-'+
                              %trim(strSTdesc);
                      scSTval = %trim(scSTval)+','+%trim(STkey);
                   endif;
                endif;
             endif;
          Endfor;

       Else;
          Setll *loval STSTL5;
          Read PST01;
          Dow not %eof(STSTL5);
             strSTdesc = TitleCase(STname);
             If scSTsel = *Blanks;
                scSTsel = %trim(STkey)+'-'+%trim(strSTdesc);
                scSTval = %trim(STkey);
             else;
                scSTsel=%trim(scSTsel)+','+%trim(STkey)+'-'+
                        %trim(strSTdesc);
                scSTval = %trim(scSTval)+','+%trim(STkey);
             endif;
             Read PST01;
          Enddo;
       Endif;

       scStCodIE = ccStInEx;

       If %Open(STSTL5);
          Close STSTL5;
       Endif;

      /End-Free

     p LoadSTCodes     e
      **  End  Add  ***  11/21/19  ***********************************
** Column headings line 1
     Sel                                              CX   Annual  Neg
     Sel                          CX   # of
     Sel                                              CX   Ann      Neg
** Column headings line 2
Opt  CX  Employer Name             Yr     Amount Due  Rsn    Pay   Adj  PF VIP
Opt  CX  Employer Name             Yr     Rpt Mth/Yr  Rsn    Pay   Adj  PF VIP
Opt  CX  Employer #   Division     Yr     Amount Due  Rsn    Pay   Adj  PF VIP
Opt  CX  Employer #   Division     Yr     Rpt Mth/Yr  Rsn    Pay   Adj  PF VIP
Opt  CX  Employer Name            Rsn   CX  Comment Line 1   Comment Line 2
Opt  CX  Employer #   Division    Rsn   CX  Comment Line 1   Comment Line 2
Opt  CX  Employer Name             Yr     Report date Rsn    Pay   Adj  PF VIP
Opt  CX  Employer #   Division     Yr     Report date Rsn    Pay   Adj  PF VIP
Opt  CX  Employer Name             Yr     Amount Due  Rsn  Pay  ACH Adj PF VIP
Opt  CX  Employer Name             Yr     Rpt Mth/Yr  Rsn  Pay  ACH Adj PF VIP
Opt  CX  Employer #   Division     Yr     Amount Due  Rsn  Pay  ACH Adj PF VIP
Opt  CX  Employer #   Division     Yr     Rpt Mth/Yr  Rsn  Pay  ACH Adj PF VIP
Opt  CX  Employer Name             Yr     Report date Rsn  Pay  ACH Adj PF VIP
Opt  CX  Employer #   Division     Yr     Report date Rsn  Pay  ACH Adj PF VIP
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1066=Employer info          YYY
1077=Finance info           YYY
12020=Chg CX select status  YNN
16060=A/R Inquiry           YYY
16262=CX history            YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
