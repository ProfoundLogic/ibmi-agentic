     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H bnddir('WTBNDDIR')
      ******************************************************************
      *  W2422R02 - Bank Account Master "Maintenance"                  *
      *                                                                *
      *  Description:                                                  *
      *                                                                *
      *    This program is used to maintain the Bank Account Master    *
      *    file.  The valid options are:                               *
      *        Change                                                  *
      *        Display                                                 *
      *        Add                                                     *
      *                                                                *
      *  Created:  08/18/97  C:30  Project:  WC659                     *
      *****************************************************************
      *  Revisions:                                                   *
      *                                                               *
      * 09/27/18 C:079 Support: 65/73564                              *
      *                Read WSRTL without locking when there is no    *
      *                update                                         *
      *                                                               *
      * 04/28/16 C:298 Project: 114WYH                                *
      *                Added Bank 2 functionality.                    *
      *                                                               *
      * 07/30/15 C:136 Project: WC2337                                *
      *                Remove status field. Make effective date       *
      *                required. Add edit on expiration date.         *
      *                                                               *
      * 05/06/15 C:062 Project: 114MRH                                *
      *                Add EFT Sequence #                             *
      *                                                               *
      * 04/07/15 C:298 Project: WC1991 : Priss 42                     *
      *                 Correction made in Display File to left       *
      *                 align Immediate Destination Field.            *
      *                                                               *
      * 02/13/15 C:298 Project: WC1991 : Priss 41                     *
      *                 Correction made to hold leading zeroes for    *
      *                 Immediate Destination Field.                  *
      *                                                               *
      * 05/20/13 C:189 Project: WC1991                                *
      *                 Flag controls for Combining EFT Transactions  *
      *                                                               *
      * 05/28/14 C:136 Project: 129WTG                                *
      *                Group EFT transactions together into the same  *
      *                file from different accounts if the accounts   *
      *                share the same EFT information. Modify edit    *
      *                for unique file name.                          *
      *                                                               *
      * 03/05/14 C:65  Project: 114WDE                                *
      *                Modfiy program to use new field "EFT Special   *
      *                processing", added on the screen.              *
      *                (Copied source from V1R1114SRC).               *
      *                                                               *
      * 04/15/13 C:279 Redmine: 2709                                  *
      *                Voucher checkbox should not give edit for 'Y'  *
      *                or 'N'.                                        *
      *                                                               *
      * 12/18/12 C:189 Project: 114WGD Bug 1856                       *
      *                Text box should be displayed instead of checkbox*
      *                for CSI as there can be three possible values  *
      *                                                               *
      * 01/19/12 C:255 Project: WC2112                                *
      *                Modfiy program to use new alpha fields for pro-*
      *                cessing Immediate Destination & Origin values  *
      *                while still maintaining existing numeric values*
      *                                                               *
      *  08/09/11 C:255 SUPPORT: WA114 77595                          *
      *                 Added code to populate voucher flag in the    *
      *                 Bank Account master file correctly.           *
      *                                                               *
      *  04/15/11 C:136 SUPPORT: WA025 76578                          *
      *                 ALLOW ZERO FOR CHECK DIGIT                    *
      *                                                               *
      *  05/21/10 C:255 PROJECT: WC1806(PRISS 50)                     *
      *                  ADDED VALIDATION AT BANK ACCOUNT MASTER      *
      *                  SCREEN 03, EFT FILE NAME MUST BE VALID AND   *
      *                  NO SPACES ALLOWED BETWEEN NAME.              *
      *                                                               *
      *  01/15/10 C:235 SUPPORT: 71339                                *
      *                  In Bank Account Master Screen 2 (SCR02)      *
      *                  SIGNATURE is not able to Validate from CHECK *
      *                  SIGNATURE FILE.
      *                                                               *
      *  07/27/09 C:136 PROJECT: 114WUK                               *
      *                 IMPLEMENT XML FUNCTIONALITY BY CHANGING       *
      *                 HOW THE VOUCHER FLAG WORKS                    *
      *                                                               *
      *  06/25/09 C:136 PROJECT: WC1806                               *
      *  06/29/09 C:191 PROJECT: WC1806                               *
      *                -ADD EFF AND EXP DATE                          *
      *                -ADD CHK INFO TO SCR02                         *
      *                -ADD EFT INFO TO SCR03                         *
      *                                                               *
      *  05/27/09 C:227 SUPPORT: WC090 68593                          *
      *                 VALIDATION CHECK FOR EFT FILENAME.            *
      *                                                               *
      *  04/02/09 C:136 SUPPORT: WA025 67192                          *
      *                 CORRECTED EDIT FOR EFT FILE NAME              *
      *                                                               *
      *  08/09/07 C:191 PROJECT: WC1453                               *
      *                 Validated file name for EFT download, if non  *
      *                 blank.                                        *
      *                                                               *
      * 07/18/07 C:136 PROJECT: WC1491 (IND DEP CASH)                 *
      *                ADD A/R CODES FOR RETURN DEPOSIT - REFUND AND  *
      *                VOID/STOP PAY                                  *
      *                                                               *
      * 06/20/07 C:53 114WMF  BACK TO BASE-114WED                     *
      * 04/29/04 C:53 Project: 114WHR                                 *       *
      *               Add Wachovia download flag                      **
      *                                                               *
      * 05/05/07 C: 58    Project: 139WHE ARP Pos Pay download
      *
      * 03/27/06 C:132 Project: WC025 - 47283                         *
      *                Create new logical over WMBIP, and use it to   *
      *                Update the Bank Name, Address fields in WMBIP  *
      *                if these fields are changed in the WSCKP bank  *
      *                master file. Will read all records in WMBIP    *
      *                with matching CO# and ACCT#, and update all    *
      *                with same CO# and ACCT#.                       *
      *                                                               *
      *          NOTE: Must send WMBIL9 logical with pgm when Instal  *
      *                at client if before any 2006 BASECD release.   *
      *                                                               *
      * 07/18/05 C:15  Project: 71WFX                                 *
      *                Include Positive Pay fields for client 71.     *
      *                                                               *
      * 06/09/05 C:58  Add Client number and download flag for
      *                Wachovia download (client 105) in file WSCKP
      *
      * 05/26/04 C:15  Support: WC128/42607                           *
      *                Chgd code to retrieve the CO# from WMUPP,      *
      *                instead of SMUPP.                              *
      *                                                                *
      *  06/17/03  C:142  PROJECT:  WC1033                             *
      *            C:136  ADDED RETURN PREM CHECK CODE, AND RETURN     *
      *                   PREMIUM STOP/PAY CODE                        *
      *                                                                *
      *  05/11/99  C:118  PROJECT:  WC?                                *
      *                   CHANGED BANK ADDRESS FROM REQUIRED TO        *
      *                   OPTIONAL.                                    *
      *                                                                *
      ******************************************************************
      *  Indicator Summary:                                            *
      *                                                                *
      *                                                                *
      *                                                                *
      ******************************************************************
      *
     FW2422F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *_________________________________________________________________
      *  Bank Account Master (for maintaining record)
     FWSCKP     UF A E             DISK
     F                                     RENAME(PCK01:DBRCDF)
      *_________________________________________________________________
      *
      *  Bank Account Master (for maintaining record)
     FWSCKL     IF   E           K DISK
      *_________________________________________________________________
      *
      *  User Parameters
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
      *_________________________________________________________________
      *
      ****** BEG ADD ***** 03/27/06 ***********************************
      *  Bank Account Information file (by Group)
     FWMBIL9    UF   E           K DISK
      ****** END ADD ***** 03/27/06 ***********************************
      *** BEG ADD ***** 06/05/09 **************************************
      *  WCMS Check Signature file
     FWMSGL1    IF   E           K DISK
      *** END ADD ***** 06/05/09 **************************************
      *
      *_________________________________________________________________
      *
      ****** BEGIN ADD *** 06/17/03 ***********************************
      *  TABLE
     FWTABL     IF   E           K DISK
      ****** END   ADD *** 06/17/03 ***********************************
     F*** BEG ADD ***** 06/25/09 ********************************
     FWTGTL     IF   E           K DISK
     F*** END ADD ***** 06/25/09 ********************************
      *_________________________________________________________________
      *** BEG ADD ***** 07/18/07 **************************************
      *  SYSTEM FILE
     FSWCPF     IF   E           K DISK
      *** beg add ***** 05/20/13 ***********************************************
      * Bank Routing file
     Fwsrtl     uf a e           k disk    rename(PRT01:PRT00)
      *** end add ***** 05/20/13 ***********************************************
      *** END ADD ***** 07/18/07 **************************************
      *_________________________________________________________________
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Bank Account Master')
     D COMAPI          C                   CONST('WA001R')
      *** BEG DEL ***** 06/25/09 **************************************
     D**********    1                     C         #OFSCR
      *** END DEL ***** 06/25/09 **************************************
     D*** BEG ADD *** 05/27/09 ****************************************
     D*
     D* Constants
     D*
     D*** BEG DEL *** 05/21/10 ****************************************
     D*             'abcdefghijklmnopqrs- C         LO
     D*             'tuvwxyz'
     D*             'ABCDEFGHIJKLMNOPQRS- C         HI
     D*             'TUVWXYZ'
     D*             'ABCDEFGHIJKLMNOPQRS- C         VALCHR
     D*             'TUVWXYZ@#$'
     D*** END DEL *** 05/21/10 ****************************************
     D*** END ADD *** 05/27/09 ****************************************
      *** BEG ADD ***** 06/25/09 **************************************
     D ER220A          C                   CONST('The use of this -
     D                                     bank account will -
     D                                     be restricted to')
     D ER220B          C                   CONST('credits only.')
     D ER225A          C                   CONST('The use of this -
     D                                     bank account will -
     D                                     be restricted to')
     D ER225B          C                   CONST('debits only.')
      *** END ADD ***** 06/25/09 **************************************
      *** beg add ***** 05/28/14 **************************************
     d/copy wPrGenPop
     d xxCancRsp       s               n
      *** end add ***** 05/28/14 **************************************
      *** beg add ***** 07/30/15 **************************************
     d/copy wPrBank
     d #2eftc          s              1
     d #2rtcd          s              1
     d #2fyr           s              3
     d #2case          s              7
     d #2data          s             75
     d #2coid          s             20
     d #2chkq          s             10
     d #2ftyp          s             10
     d #2hdcy          s              1
     d #2cpyq          s             10
     d #2efdt          s              7
     d #2Acct          s                   like(BIAcct)
     d #2Appl          s              2
     d #2Call          s              1
     d #2CkTp          s                   like(BICkTp)
     d #2Co#           s              3
     d #2Fnd           s              3
     d #2SrDt          s              8
      *** end add ***** 07/30/15 **************************************
      *================================================================
      *  KLIST definitions
      *================================================================

     C     CKKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
      ****** BEGIN ADD *** 06/17/2003 *********************************
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      ****** END   ADD *** 06/17/2003 *********************************
      *================================================================
      *  *LIKE definitions
      *================================================================
     C     *LIKE         DEFINE    CKCO#         XXCO#
     C     *LIKE         DEFINE    CKACCT        XXACCT
      *
      ****** BEG ADD ***** 03/27/06 ***********************************
     C     *LIKE         DEFINE    CKAI01        SVAI01
     C     *LIKE         DEFINE    CKAI02        SVAI02
     C     *LIKE         DEFINE    CKAI03        SVAI03
      ****** END ADD ***** 03/27/06 ***********************************
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C     *LIKE         DEFINE    CKONUS        SVONUS
     C     *LIKE         DEFINE    CKCKDG        SVCKDG
     C     *LIKE         DEFINE    CKBNK#        SVBNK#
     C     *LIKE         DEFINE    CKTRNS        SVTRNS
     C     *LIKE         DEFINE    CKAXOU        SVAXOU
     C     *LIKE         DEFINE    CKRTUP        SVRTUP
     C     *LIKE         DEFINE    CKRTLW        SVRTLW
     C*** END   ADD ****** 06/29/09 ************************************
      *** BEG ADD ***** 06/05/09 **************************************
     C     *LIKE         DEFINE    CKNAM1        KKNAME
     C     *LIKE         DEFINE    BINAM1        ##NAM1
     C     *LIKE         DEFINE    BINAM2        ##NAM2
     C     *LIKE         DEFINE    BI10A1        ##10A1
     C     *LIKE         DEFINE    BI10A2        ##10A2
     C     *LIKE         DEFINE    BICMD1        ##CMD1
     C     *LIKE         DEFINE    BICMD2        ##CMD2
      *** END ADD ***** 06/05/09 **************************************
      *================================================================
      *  PLIST definitions
      *================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
     C     BIKY          KLIST
     C                   KFLD                    CKCO#
     C                   KFLD                    CKACCT
      *** BEG ADD ***** 06/05/09 ************************************
     C     SGKEY         KLIST
      *** BEG CHG ***** 01/15/10 ************************************
     C*******              KFLD           CKCO#
     C                   KFLD                    XXCO#
      *** END CHG ***** 01/15/10 ************************************
     C                   KFLD                    KKNAME
      *** beg add ***** 05/20/13 ***********************************************
     C     keyWSRTL      klist
     C                   kfld                    xxco#
     C                   kfld                    xxrtky           10
      *** end add ***** 05/20/13 ***********************************************
      *** END ADD ***** 06/05/09 ************************************
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C/COPY SCOPYMT01C
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      **
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      *  If ADD mode
      *
     C     @@MODE        IFEQ      'A'
      *
      *  The Account Number cannot be BLANKS   (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C     SCACCT        ANDEQ     *BLANKS
     C                   SETON                                        9077
     C                   ENDIF
      *
      *  The Bank Account Number must NOT exist in Account Master
      *                                        (--Add-- Mode only)
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      UPCO#         XXCO#
     C                   MOVEL     SCACCT        XXACCT
     C     CKKY          CHAIN     PCK01                              60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9078
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *  Bank Name can not be blanks
      *
     C     *IN90         IFEQ      *OFF
     C     SCAI01        ANDEQ     *BLANK
     C                   SETON                                        9071
     C                   ENDIF
      ***** BEGIN DEL **** 05/11/99 ***********************************
      *
      *  Bank Address can not be blanks
      *
      *          *IN90     IFEQ *OFF
      *          SCAI02    ANDEQ*BLANK
      *                    SETON                     9072
      *                    ENDIF
      *
      *  Bank City, St Zip  can not be blanks
      *
      *          *IN90     IFEQ *OFF
      *          SCAI03    ANDEQ*BLANK
      *                    SETON                     9073
      *                    ENDIF
      ***** END DEL ****** 05/11/99 ***********************************
      *
      *  Next Available Check# can not be zero
      *
     C     *IN90         IFEQ      *OFF
     C     SCLCHK        ANDLE     *ZERO
     C                   SETON                                        9074
     C                   ENDIF
      *
      *  Voucher Flag must be Y or N all clients except 114.
      *  114 must be P, V, or X. P=Paper, V-Voucher, X=XML Transmission
      *
      *** BEG CHG ***** 07/27/09 **************************************
     C********** *IN90     IFEQ *OFF
     C********** SCVCHR    ANDNE'Y'
     C********** SCVCHR    ANDNE'N'
     C**********           SETON                     9075
     C**********           ENDIF
     C  N90              SELECT
      *** BEG CHG ***** 12/18/12 **************************************
     C     @@CLT#        WHENEQ    '114'
     C*****SCVCHR        IFNE      'P'
     C*****SCVCHR        ANDNE     'V'
     C*****SCVCHR        ANDNE     'X'
     C     SCVCHR#       IFNE      'P'
     C     SCVCHR#       ANDNE     'V'
     C     SCVCHR#       ANDNE     'X'
      *** END CHG ***** 12/18/12 **************************************
     C                   SETON                                        9075
     C                   ENDIF
     C                   OTHER
     C     SCVCHR        IFNE      'Y'
     C     SCVCHR        ANDNE     'N'
     C                   SETON                                        9075
     C                   ENDIF
     C                   ENDSL
      *** END CHG ***** 07/27/09 **************************************
      *
      *  Chk Rec Flag must be Y or N
      *
     C     *IN90         IFEQ      *OFF
     C     SCCRUP        ANDNE     'Y'
     C     SCCRUP        ANDNE     'N'
     C                   SETON                                        9076
     C                   ENDIF
      ****** BEGIN ADD *** 06/17/03 ***********************************
      *
      *  VALIDATE TRANSACTION CODES
      *
      *** BEG ADD ***** 07/18/07 **************************************
     C     *IN41         IFEQ      *ON
      *** END ADD ***** 07/18/07 **************************************
      *
     C     *IN90         IFEQ      *OFF
     C     SCRPCD        ANDNE     *BLANK
     C                   MOVEL     SCRPCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTTYPE        ORNE      'R'
      *** BEG ADD ***** 07/18/07 **************************************
     C     TTCTYP        ORNE      'P'
     C     TTSTAT        OREQ      'I'
      *** END ADD ***** 07/18/07 **************************************
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCRPVS        ANDNE     *BLANK
     C                   MOVEL     SCRPVS        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTTYPE        ORNE      'T'
      *** BEG ADD ***** 07/18/07 **************************************
     C     TTCTYP        ORNE      'P'
     C     TTSTAT        OREQ      'I'
      *** END ADD ***** 07/18/07 **************************************
     C                   MOVE      *ON           *IN80
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      ****** END   ADD *** 06/17/03 ***********************************
      *** BEG ADD ***** 07/18/07 **************************************
      *
      *  VALIDATE TRANSACTION CODES
      *
     C     *IN90         IFEQ      *OFF
     C     SCRDCD        ANDNE     *BLANK
     C                   MOVEL     SCRDCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTTYPE        ORNE      'R'
     C     TTCTYP        ORNE      'D'
     C     TTSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN90         IFEQ      *OFF
     C     SCRDVS        ANDNE     *BLANK
     C                   MOVEL     SCRDVS        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     TTTYPE        ORNE      'T'
     C     TTCTYP        ORNE      'D'
     C     TTSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN86
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN41 = *ON
      *** END ADD ***** 07/18/07 **************************************
     C*** Begin add *** 05/05/07 **************************************
     C*
     C*     Edit Pos Pay download flag.
     C*     Download flag must be Y or N
     C     *IN68         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C     SCWPP         IFNE      'Y'
     C***** Beg Chg ***** 04/28/16 ************************************
     C*****SCWPP         ANDNE     'N'
     C     SCWPP         ANDNE     'I'
     C***** End Chg ***** 04/28/16 ************************************
     C                   SETON                                        8490
     C                   ENDIF                                                  e.WPP<>'Y'...
     C                   ENDIF                                                  e.IN68=ON...
     C*** End add ***** 05/05/07 **************************************
     C*
     C*** BEG DEL ***** 06/25/09 **************************************
     C*** Beg Add ***** 08/09/07 **************************************
     C***********SCEFTF    IFNE *BLANKS
     C****BEG*ADD ***** 05/27/09 **************************************
     C***********          MOVELSCEFTF    XXEFTF  1
     C***********LO:HI     XLATEXXEFTF    XXEFTF
     C***********XXEFTF    SCAN VALCHR    SNPOS   20
     C***********SNPOS     IFEQ 0
     C***********          SETON                     8890
     C***********          ENDIF
     C************IN90     IFEQ *OFF
     C****END*ADD ***** 05/27/09 **************************************
     C************LOVAL    SETLLPCK01
     C***********          READ PCK01                    60
     C************IN60     DOWEQ*OFF
     C***********SCEFTF    IFEQ CKEFTF
     C****BEG*ADD ***** 04/02/09 **************************************
     C***********SCACCT    ANDNECKACCT
     C****END*ADD ***** 04/02/09 **************************************
     C***********          SETON                     8790
     C***********          LEAVE
     C***********          ENDIF
     C***********          READ PCK01                    60
     C***********          ENDDO
     C****BEG*ADD ***** 05/27/09 **************************************
     C***********          ENDIF                           *IN90=*OFF
     C****END*ADD ***** 05/27/09 **************************************
     C***********          ENDIF
     C*** End Add ***** 08/09/07 **************************************
     C*** END DEL ***** 06/25/09 **************************************
     C*** Begin dlt *** 07/18/05 **************************************
     C****** BEGIN ADD *** 06/09/05 ***********************************
     C*
     C*     Edit Wachovia Download data.
     C*****      *IN08     IFEQ *ON
     C*****      *IN90     IFEQ *OFF
     C*****      SCWPP     IFEQ 'Y'
     C*****      SCWBK#    ANDLE*ZERO
     C*****                MOVE *ON       *IN83
     C*****                MOVE *ON       *IN90
     C*****                ENDIF                           e.WPP=Y
     C*****                ENDIF                           e.IN90=OFF
     C*****                ENDIF                           e.IN08=ON
     C****** END ADD ***** 06/09/05 ***********************************
     C*** End dlt ***** 07/18/05 **************************************
     C*** BEG ADD ***** 06/25/09 **************************************
      *
      *  VALIDATE EFF DATE
      *
      *** beg add ***** 07/30/15 *****************************
      /free
       If scefdt = 0;
          *in88 = *on;
          *in90 = *on;
       Endif;
      /end-free
      *** end add ***** 07/30/15 *****************************
     C****** Beg Chg ***** 04/15/13 ***********************************
     C******IN90**       IFEQ      *OFF
     C*****SCEFDT*       ANDNE     *ZERO
     C     SCEFDT        IFNE      *ZERO
     C****** End Chg ***** 04/15/13 ***********************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN83
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C*
     C*  VALIDATE TO DATE
     C*
     C****** Beg Chg ***** 04/15/13 ***********************************
     C******IN90**       IFEQ      *OFF
     C*****SCEXDT*       ANDNE     *ZERO
     C     SCEXDT        IFNE      *ZERO
     C****** End Chg ***** 04/15/13 ***********************************
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C*
     C*  VALIDATE FROM DATE LESS THAN TO DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCEXDT        ANDNE     *ZERO
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C*
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT            7 0
     C*
     C     XXEFDT        IFGT      XXEXDT
     C                   MOVE      *ON           *IN89
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ***** 06/25/09 **************************************
     C*** beg add ***** 07/30/15 **************************************
      /free
       // Don't allow the policy to be expired unless another bank is
       //  active as of the expiration date for each group/appl/cktp.
       If not(*in90) and @@mode <> 'A';
          Setll (CKco#:CKacct) pbi01;
          Dou %eof;
          Reade(n) (CKco#:CKacct) pbi01;
             If %Eof(WMBIL9);
                Leave;
             Endif;
             If BIstat <> 'A';
                Iter;
             Endif;
             If BIstat = 'A' and BIefdt < xxExDt and BIacct = CKacct;
                #2co#  = %Editc(BIco#:'X');
                #2fnd  = %EditC(BIfnd:'X');
                #2srdt = %EditC(scExDt:'X');
                #2acct = GetBankAcc(#2co#: #2fnd: BIappl:
                 #2srdt: #2call: #2hdcy: #2chkq: #2ftyp: #2efdt:
                 #2cpyq: BIcktp: BIeftc: #2coid: #2rtcd);
                If #2acct = BIacct;
                    *in81 = *on;
                    *in90 = *on;
                    Leave;
                Endif;
             Endif;
          Enddo;
       Endif;
      /end-free
     C*** end add ***** 07/30/15 **************************************
     C*** Begin Add *** 05/06/15 ********************************************
     C*
     C* EFT Sequence # must be greater than equal to zero
     C*
     C                   If        *in90 = *Off And scEftSeq# < 0
     C                   SetOn                                        6190
     C                   EndIf
     C*** End   Add *** 05/06/15 ********************************************
      *
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C*** BEG ADD ***** 06/25/09 **************************************
      *
      *  Routing for MICR cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCTRNS        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  Transit Number cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCBNK#        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *** BEG DEL ***** 04/15/11 ************************************
      ***********
      ***Check*Digit cannot be BLANK
      ***********
     C************IN90     IFEQ *OFF
     C***********SCCKDG    ANDEQ*ZEROS
     C***********          MOVE *ON       *IN73
     C***********          MOVE *ON       *IN90
     C***********          ENDIF
      *** END DEL ***** 04/15/11 ************************************
      *
      *  ON-US cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCONUS        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN74
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  Routing Entry Upper cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCRTUP        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN75
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  Routing Entry Lower cannot be BLANK
      *
     C     *IN90         IFEQ      *OFF
     C     SCRTLW        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN76
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  Signature 1 must exist in Check Signature file
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAM1        ANDNE     *BLANKS
     C                   MOVEL     SCNAM1        KKNAME
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN77
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
      *
      *  Signature 2 must exist in Check Signature file
      *
     C     *IN90         IFEQ      *OFF
     C     SCNAM2        ANDNE     *BLANKS
     C                   MOVEL     SCNAM2        KKNAME
     C     SGKEY         CHAIN     PSG01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN78
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 06/25/09 **************************************
      *
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C*** BEG ADD ***** 06/25/09 **************************************
     C*** Beg Chg ***** 01/19/12 **************************************
      *
      *  IMMEDIATE DESTIONATION
      *
     C**N90***** SCIDST    IFLE *ZERO
     C********** SCIORG    ANDGT*ZERO
     C**********           SETON                     71  90
     C**********           ENDIF
      *
      *  IMMEDIATE ORIGIN
      *
     C**N90***** SCIORG    IFLE *ZERO
     C********** SCIDST    ANDGT*ZERO
     C**********           SETON                     72  90
     C**********           ENDIF
      *
      *  DESTINATION BANK NAME
      *
     C**N90***** SCBNAM    IFEQ *BLANK
     C********** SCIDST    ANDGT*ZERO
     C**********           SETON                     73  90
     C**********           ENDIF
      *
      *  DESTINATION BANK NAME
      *
     C**N90***** SCCNAM    IFEQ *BLANK
     C********** SCIDST    ANDGT*ZERO
     C**********           SETON                     74  90
     C**********           ENDIF
      *
      *  COMPANY ENTERED DESCRIPTION
      *
     C**N90***** SCCEDS    IFEQ *BLANK
     C********** SCIDST    ANDGT*ZERO
     C**********           SETON                     75  90
     C**********           ENDIF
      *
      *  ORIGINATING DFI
      *
     C**N90***** SCORDI    IFEQ *ZERO
     C********** SCIDST    ANDGT*ZERO
     C**********           SETON                     76  90
     C**********           ENDIF
      *
      *  SERVICE CLASS
      *
     C********** *IN90     IFEQ *OFF
     C********** SCIDST    ANDGT*ZERO
     C**********           MOVEL'SCLS'    KKTBID  4 P
     C**********           MOVELSCSVCL    KKKEY  10 P
     C********** GTKY      CHAINPGT01                60
     C********** *IN60     IFEQ *ON
     C********** *IN60     OREQ *OFF
     C********** GTSTAT    ANDNE'A'
     C**********           SETON                     77  90
     C**********           ENDIF
     C**********           ENDIF
      *
      *  IMMEDIATE DESTINATION
      *
     C  N90SCIDST        IFEQ      *ZEROS
     C     SCIDST        OREQ      *BLANKS
     C     SCIORG        IFNE      *ZEROS
     C     SCIORG        ANDNE     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF
     C                   ELSE
     C*Check validity of entered value
     C  N90SCIDST        IFNE      *BLANKS
     C     SCIDST        ANDNE     *ZEROS
     C                   TESTN                   SCIDST               434748
     C  N43
     CANN47
     CANN48              SETON                                        44  90
     C                   ENDIF
     C                   ENDIF
      *
      *  IMMEDIATE ORIGIN
      *
     C  N90SCIORG        IFEQ      *ZEROS
     C     SCIORG        OREQ      *BLANKS
     C     SCIDST        IFNE      *ZEROS
     C     SCIDST        ANDNE     *BLANKS
     C                   SETON                                        72  90
     C                   ENDIF
     C                   ELSE
     C*Check validity of entered value
     C  N90SCIORG        IFNE      *BLANKS
     C     SCIORG        ANDNE     *ZEROS
     C                   TESTN                   SCIORG               434748
     C  N43
     CANN47
     CANN48              SETON                                        46  90
     C                   ENDIF
     C                   ENDIF
      *
      *  DESTINATION BANK NAME
      *
     C  N90SCBNAM        IFEQ      *BLANK
     C     SCIDST        ANDNE     *ZEROS
     C     SCIDST        ANDNE     *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF
      *
      *  DESTINATION COMPANY NAME
      *
     C  N90SCCNAM        IFEQ      *BLANK
     C     SCIDST        ANDNE     *ZEROS
     C     SCIDST        ANDNE     *BLANKS
     C                   SETON                                        74  90
     C                   ENDIF
      *
      *  COMPANY ENTERED DESCRIPTION
      *
     C  N90SCCEDS        IFEQ      *BLANK
     C     SCIDST        ANDNE     *ZEROS
     C     SCIDST        ANDNE     *BLANKS
     C                   SETON                                        75  90
     C                   ENDIF
      *
      *  ORIGINATING DFI
      *
     C  N90SCORDI        IFEQ      *ZEROS
     C     SCIDST        ANDNE     *ZEROS
     C     SCIDST        ANDNE     *BLANKS
     C                   SETON                                        76  90
     C                   ENDIF
      *
      *  SERVICE CLASS
      *
     C     *IN90         IFEQ      *OFF
     C     SCIDST        ANDNE     *ZEROS
     C     SCIDST        ANDNE     *BLANKS
     C                   MOVEL(P)  'SCLS'        KKTBID            4
     C                   MOVEL(P)  SCSVCL        KKKEY            10
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     *IN60         OREQ      *OFF
     C     GTSTAT        ANDNE     'A'
     C                   SETON                                        77  90
     C                   ENDIF
     C                   ENDIF
     C*** End Chg ***** 01/19/12 **************************************
      *
     C  N90SVSVCL        IFEQ      200
     C     SCSVCL        ANDNE     200
     C     SCSVCL        ANDNE     *ZERO
     C                   CLEAR                   WDERR1
     C                   CLEAR                   WDERR2
     C                   SELECT
     C     SCSVCL        WHENEQ    220
     C                   MOVEL     ER220A        WDERR1
     C                   MOVEL     ER220B        WDERR2
     C     SCSVCL        WHENEQ    225
     C                   MOVEL     ER225A        WDERR1
     C                   MOVEL     ER225B        WDERR2
     C                   ENDSL
     C     *INKG         DOUEQ     *ON
     C     *INKL         OREQ      *ON
     C                   EXFMT     WDWERR
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
      *
      * CHECK IF EFT FILE NAME IS VALID AND UNIQUE
      *
     C  N90SCEFTF        IFNE      *BLANKS
      *** BEG CHG ***** 05/21/10 ************************************
     C*******              MOVELSCEFTF    XXEFTF  1
     C*******    LO:HI     XLATEXXEFTF    XXEFTF
     C*******    XXEFTF    SCAN VALCHR    SNPOS   20
     C*******    SNPOS     IFEQ 0
     C*******              SETON                     88  90
     C*******              ENDIF
     C*CALL PROGRAM TO CHECK THE VALIDITY OF NAME ENTERED AT SCREEN
     C                   CALL      'SRCKOBJN'
     C                   PARM                    SCEFTF
     C                   PARM                    @@FLG             1
     C*IF INVALID
     C     @@FLG         IFEQ      '1'
     C                   SETON                                        88  90
     C                   ENDIF
      *** END CHG ***** 05/21/10 ************************************
      *** beg chg ***** 05/28/14 ******************************
     C******IN90*        IFEQ      *OFF
     C******LOVAL        SETLL     PCK01
     C***********        READ      PCK01                                  60
     C******IN60*        DOWEQ     *OFF
     C*****SCEFTF        IFEQ      CKEFTF
     C*****SCACCT        ANDNE     CKACCT
     C***********        SETON                                        87  90
     C***********        LEAVE
     C***********        ENDIF
     C***********        READ      PCK01                                  60
     C***********        ENDDO
     C***********        ENDIF                                                  *IN90=*OFF
      /free
       If not(*in90);
          Setll xxCo# pck01;
          Reade xxCo# pck01;
          Dow not(%eof) and not *in90;

             If scEftf = CKeftf and scAcct <> CKacct;
                If scIdst = CKidsa;
                   xxCancRsp = PopTwoBtn('The EFT file name matches that of -
                    an existing "immediate Destion #". EFT transactions will -
                    be grouped together for sumission to the bank.':
                    'EFT File Name Message':'Cancel':'Submit');
                   Select;
                   When xxCancRsp = *Off;
                      Leave;
                   When xxCancRsp = *On;
                      *in90 = *On;
                   Endsl;
                Else;
                   PopOneBtn('The EFT file name must be unique -
                    based on the "Immediate Destionation #". Please enter -
                    either a different name or the name that exists for -
                    this "Immediate Destination #".' :
                    'EFT File Name Edit');
                   *in90 = *On;
                Endif;
             Endif;

             If scEftf <> CKeftf and scIdst = CKidsa and scAcct <> CKacct;
                xxCancRsp = PopTwoBtn('The EFT file name does not match -
                 the file name for an account already set for the same -
                 "Immediate Destination #". EFT transactions will -
                 not be combined for submission to the bank. Click Sumbit -
                 to continue with the file name entered.':
                 'EFT File Name Warning':'Cancel':'Submit');
                Select;
                When xxCancRsp = *Off;
                   Leave;
                When xxCancRsp = *On;
                   *in90 = *On;
                Endsl;
             Endif;
          Reade xxCo# pck01;
          Enddo;
       Endif;
      /end-free
      *** end chg ***** 05/28/14 ******************************
     C                   ELSE
     C     SCIDST        IFGT      *ZERO
     C                   SETON                                        89  90
     C                   ENDIF
     C                   ENDIF
      *
      * ACH INTERIM DAYS
      *
     C  N90SCACID        IFLT      *ZERO
     C                   SETON                                        78  90
     C                   ENDIF
      *
      * BANK LEAD TIME
      *
     C  N90SCBKLT        IFLT      *ZERO
     C                   SETON                                        79  90
     C                   ENDIF
      *
      * BYPASS PRENOTE PROCESSING
      *
     C  N90SCBYPN        IFNE      *BLANK
     C     SCBYPN        ANDNE     'Y'
     C                   SETON                                        80  90
     C                   ENDIF
     C*** END ADD ***** 06/25/09 **************************************
      *
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      CKACCT        SCACCT                         Bank Account
     C                   MOVE      CKAI01        SCAI01                         Bank Name
     C                   MOVE      CKAI02        SCAI02                         Bank Address
     C                   MOVE      CKAI03        SCAI03                         Bank City StZip
     C                   Z-ADD     CKLCHK        SCLCHK                         Next Avail Chk#
      *** BEG CHG ***** 07/27/09 **************************************
     C***********          MOVE CKVCHR    SCVCHR           Voucher flag
     C     @@CLT#        IFEQ      '114'
     C                   SELECT
      *** BEG CHG ***** 08/09/11 **************************************
     C********** CKVCHR    WHEQ 'Y'
     C**********           MOVE 'P'       SCVCHR
     C********** CKVCHR    WHEQ 'N'
     C**********           MOVE 'V'       SCVCHR
     C     CKVCHR        WHENEQ    'Y'
      *** BEG CHG ***** 12/18/12 **************************************
     C**********         MOVE      'V'           SCVCHR
     C                   MOVE      'V'           SCVCHR#
      *** END CHG ***** 12/18/12 **************************************
     C     CKVCHR        WHENEQ    'N'
      *** BEG CHG ***** 12/18/12 **************************************
     C**********         MOVE      'P'           SCVCHR
     C                   MOVE      'P'           SCVCHR#
      *** END CHG ***** 12/18/12 **************************************
      *** END CHG ***** 08/09/11 **************************************
     C                   OTHER
      *** BEG CHG ***** 12/18/12 **************************************
     C**********         MOVE      CKVCHR        SCVCHR
     C                   MOVE      CKVCHR        SCVCHR#
      *** END CHG ***** 12/18/12 **************************************
     C                   ENDSL
     C                   ELSE
     C****** Beg Add ******* 04/15/13 *********************************
     C                   IF        CKVCHR = ' '
     C                   EVAL      SCVCHR = 'N'
     C                   ELSE
     C****** End Add ******* 04/15/13 *********************************
     C                   MOVE      CKVCHR        SCVCHR
     C****** Beg Add ******* 04/15/13 *********************************
     C                   ENDIF
     C****** End Add ******* 04/15/13 *********************************
     C                   ENDIF
      *** END CHG ***** 07/27/09 **************************************
     C****** Beg Add ******* 04/15/13 *********************************
     C                   IF        CKCRUP = ' '
     C                   EVAL      SCCRUP = 'N'
     C                   ELSE
     C****** End Add ******* 04/15/13 *********************************
     C                   MOVE      CKCRUP        SCCRUP                         Chk Rec flag
     C****** Beg Add ******* 04/15/13 *********************************
     C                   ENDIF
     C****** End Add ******* 04/15/13 *********************************
      *** beg del ***** 07/30/15 **************************************
     C***********        MOVE      CKSTAT        SCSTAT                         STATUS
      *** end del ***** 07/30/15 **************************************
      *** BEG ADD ***** 06/17/03 **************************************
     C                   MOVE      CKRPCD        SCRPCD                         Retrn Prem Code
     C                   MOVE      CKRPVS        SCRPVS                         Retrn Prem Void/Stop
      *** END ADD ***** 06/17/03 **************************************
      *** BEG ADD ***** 07/18/07 **************************************
     C                   MOVE      CKRDCD        SCRDCD                         Retrn DEP Code
     C                   MOVE      CKRDVS        SCRDVS                         Retrn DEP Void/Stop
      *** END ADD ***** 07/18/03 **************************************
      *
      ****** BEG ADD ***** 03/27/06 ***********************************
      * Save for changes to Bank Info File
     C                   MOVE(P)   CKAI01        SVAI01                         Bank Name
     C                   MOVE(P)   CKAI02        SVAI02                         Bank Address
     C                   MOVE(P)   CKAI03        SVAI03                         Bank City StZip
      ****** END ADD ***** 03/27/06 ***********************************
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C                   MOVE(P)   CKONUS        SVONUS
     C                   MOVE(P)   CKCKDG        SVCKDG
     C                   MOVE(P)   CKBNK#        SVBNK#
     C                   MOVE(P)   CKTRNS        SVTRNS
     C                   MOVE(P)   CKAXOU        SVAXOU
     C                   MOVE(P)   CKRTUP        SVRTUP
     C                   MOVE(P)   CKRTLW        SVRTLW
     C*** END   ADD ****** 06/29/09 ************************************
     C*
     C****** BEGIN ADD *** 06/09/05 ***********************************
     C     *IN08         IFEQ      *ON
     C                   MOVE      CKWPP         SCWPP                          Positive pay
     C                   MOVE      CKWBK#        SCWBK#                         Wach bank #
     C                   ENDIF                                                  e. IN08 ON
     C****** END ADD ***** 06/09/05 ***********************************
     C*** Begin add *** 05/05/07 **************************************
     C     *IN68         IFEQ      *ON
     C                   MOVE      CKWPP         SCWPP                          Positive pay
     C                   ENDIF                                                  e. IN08 ON
     C     SCWPP         IFEQ      ' '
     C***** Beg Chg ***** 04/28/16 ************************************
     C**********         MOVE      'N'           SCWPP                           de fault
     C                   MOVE      ' '           SCWPP                           de fault
     C***** End Chg ***** 04/28/16 ************************************
     C                   ENDIF                                                  e.WPP=' '
     C*** End add ***** 05/05/07 **************************************
     C*** Beg Add ***** 08/09/07 **************************************
     C                   MOVEL     CKEFTF        SCEFTF                         EFT file name
     C*** End Add ***** 08/09/07 **************************************
     C*** BEG ADD ***** 06/25/09 **************************************
     C                   Z-ADD     CKEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     CKEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C                   MOVEL     CKTRNS        SCTRNS                         TRANSIT ROUTING #
     C                   MOVEL     CKBNK#        SCBNK#                         TRANSIT BANK #
     C                   MOVEL     CKCKDG        SCCKDG                         TRANSIT CHECK DIGIT
     C                   MOVEL     CKAXOU        SCAXOU                         AUX ON-US FLD UPTO
     C                   MOVEL     CKONUS        SCONUS                         ON-US FLD UP TO 19A
     C                   MOVEL     CKRTUP        SCRTUP                         ROUTING ENTRY UPPER
     C                   MOVEL     CKRTLW        SCRTLW                         ROUTING ENRTY LOWER
     C                   MOVEL     CKNAM1        SCNAM1                         SIG 1 STRING
     C                   MOVEL     CKNAM2        SCNAM2                         SIG 2 STRING
     C*** Beg Chg ***** 01/19/12 **************************************
     C*******              MOVE CKIDST    SCIDST           IMMED DEST
     C*******              MOVE CKIORG    SCIORG           IMMED ORIG
     C*Move Alpha fields if non zero/non blanks on screen else Numeric fields
     C     CKIDSA        IFNE      *BLANKS
     C     CKIDSA        ANDNE     *ZEROS
     C                   MOVE      CKIDSA        SCIDST                         IMMED DEST
     C                   ELSE
     C                   MOVE      CKIDST        SCIDST                         IMMED DEST
     C                   ENDIF
     C     CKIORA        IFNE      *BLANKS
     C     CKIORA        ANDNE     *ZEROS
     C                   MOVE      CKIORA        SCIORG                         IMMED DEST
     C                   ELSE
     C                   MOVE      CKIORG        SCIORG                         IMMED DEST
     C                   ENDIF
     C*** End Chg ***** 01/19/12 **************************************
     C                   MOVEL     CKBNAM        SCBNAM                         DEST BANK
     C                   MOVEL     CKCONM        SCCNAM                         COMP NAME
     C                   MOVEL     CKCEDS        SCCEDS                         CO ENT DESC
     C                   MOVEL     CKRFCD        SCRFCD                         REF CODE
     C                   MOVE      CKORDI        SCORDI                         ORIG DFI ID
     C     @@MODE        IFEQ      'A'
     C                   MOVE      '200'         SCSVCL                         SRV CLASS
     C                   MOVE      '200'         SVSVCL
     C                   ELSE
     C                   MOVE      CKSVCL        SCSVCL                         SRV CLASS
     C                   MOVE      CKSVCL        SVSVCL            3 0
     C                   ENDIF
     C                   MOVEL(P)  'SCLS'        KKTBID            4
     C                   MOVEL(P)  SCSVCL        KKKEY            10
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCSCLD
     C                   MOVE      CKACID        SCACID                         ACH INTERIM
     C                   MOVE      CKBKLT        SCBKLT                         BANK LEAD TIME
     C                   MOVE      CKBYPN        SCBYPN                         BYPASS PRENOTE
     C*** END ADD ***** 06/25/09 **************************************
      *** Begin Add *** 03/05/14 **************************************
     C                   MOVE      CKEFSP        SCEFSP                         EFT Special Process
      *** End Add ***** 03/05/14 **************************************
      *** beg add ***** 05/20/13 ***********************************************
      /free
       Clear RTspad;
       Clear RTcmwc;
       Clear RTcmwp;
       Clear RTcmag;

       If  CKidsa <> *blanks;
          xxrtky = CKidsa ;
       Else;
          xxrtky = %Editc(CKidst : 'X');
       Endif;

       If  xxrtky = *blanks;
          xxrtky = CKTRNS + CKBNK# + %Editc(CKckdg : 'X');
       Endif;

       //*** Beg Chg *** 09/27/18 ****************************************
       //**** Chain keyWSRTL PRT00;
       Chain(n) keyWSRTL PRT00;
       //*** End Chg *** 09/27/18 ****************************************
       if %found();
          SCSPAD = RTspad;
          SCCMWC = RTcmwc;
          SCCMWP = RTcmwp;
          SCCMAG = RTcmag;
       Endif;

       //*** Begin Add *** 05/06/15 ********************************************
       scEftSeq# = 0;
       scEftSeq# = CKeft#;
       //*** End   Add *** 05/06/15 ********************************************

      /end-free
      *** end add ***** 05/20/13 ***********************************************
     C*
      *** beg del ***** 07/30/15 ***********************************************
     C******IN40*        IFEQ      *ON
     C***********        MOVE      'A'           SCSTAT
     C***********        ENDIF
      *** end del ***** 07/30/15 ***********************************************
      *
     C                   ENDSR
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
      ****** BEG ADD ***** 03/27/06 ***********************************
      * If not an ADD, check for changes to screen fields for update
     C                   MOVE      *OFF          *IN45
     C  N40SCAI01        IFNE      SVAI01
     C     SCAI02        ORNE      SVAI02
     C     SCAI03        ORNE      SVAI03
     C*** BEGIN ADD ****** 06/29/09 ************************************
     C     SCONUS        ORNE      SVONUS
     C     SCCKDG        ORNE      SVCKDG
     C     SCBNK#        ORNE      SVBNK#
     C     SCTRNS        ORNE      SVTRNS
     C     SCAXOU        ORNE      SVAXOU
     C     SCRTUP        ORNE      SVRTUP
     C     SCRTLW        ORNE      SVRTLW
     C*** END   ADD ****** 06/29/09 ************************************
     C                   MOVE      *ON           *IN45
     C                   ENDIF
      ****** END ADD ***** 03/27/06 ***********************************
     C*
     C                   MOVE      SCACCT        CKACCT                         Bank Account
     C                   MOVE      SCAI01        CKAI01                         Bank Name
     C                   MOVE      SCAI02        CKAI02                         Bank Address
     C                   MOVE      SCAI03        CKAI03                         Bank City StZip
     C                   Z-ADD     SCLCHK        CKLCHK                         Next Avail Chk#
      *** BEG CHG ***** 07/27/09 **************************************
     C***********          MOVE SCVCHR    CKVCHR           Voucher flag
     C     @@CLT#        IFEQ      '114'
     C                   SELECT
      *** BEG CHG ***** 08/09/11 **************************************
     C********** SCVCHR    WHEQ 'P'
     C**********           MOVE 'Y'       CKVCHR
     C********** SCVCHR    WHEQ 'V'
     C**********           MOVE 'N'       CKVCHR
      *** BEG CHG ***** 12/18/12 **************************************
     C**** SCVCHR        WHENEQ    'P'
     C     SCVCHR#       WHENEQ    'P'
      *** END CHG ***** 12/18/12 **************************************
     C                   MOVE      'N'           CKVCHR
      *** BEG CHG ***** 12/18/12 **************************************
     C*****SCVCHR        WHENEQ    'V'
     C     SCVCHR#       WHENEQ    'V'
      *** END CHG ***** 12/18/12 **************************************
     C                   MOVE      'Y'           CKVCHR
      *** END CHG ***** 08/09/11 **************************************
     C                   OTHER
      *** BEG CHG ***** 12/18/12 **************************************
     C************       MOVE      SCVCHR        CKVCHR
     C                   MOVE      SCVCHR#       CKVCHR
      *** END CHG ***** 12/18/12 **************************************
     C                   ENDSL
     C                   ELSE
     C                   MOVE      SCVCHR        CKVCHR
     C                   ENDIF
      *** END CHG ***** 07/27/09 **************************************
     C                   MOVE      SCCRUP        CKCRUP                         Chk Rec flag
     C                   MOVE      Q1USER        CKUSER                         USER
     C                   MOVE      Q1PGM         CKPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CKTIME                         TIME MAINTAINED
      *** BEG ADD ***** 06/17/03 **************************************
     C                   MOVE      SCRPCD        CKRPCD                         Retrn Prem Code
     C                   MOVE      SCRPVS        CKRPVS                         Retrn Prem Void/Stop
      *** END ADD ***** 06/17/03 **************************************
      *** BEG ADD ***** 07/18/07 **************************************
     C                   MOVE      SCRDCD        CKRDCD                         Retrn DEP Code
     C                   MOVE      SCRDVS        CKRDVS                         Retrn DEP Void/Stop
      *** END ADD ***** 07/18/03 **************************************
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CKDT                           DATE MAINTAINED
     C*** Beg Add ***** 08/09/07 **************************************
     C                   MOVEL     SCEFTF        CKEFTF                         EFT file name
     C*** End Add ***** 08/09/07 **************************************
      *
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     UPCO#         CKCO#                          Company #
      *** beg del ***** 07/30/15 *******************************************************
     C***********        MOVE      SCSTAT        CKSTAT                         STATUS
      *** end del ***** 07/30/15 *******************************************************
     C                   ENDIF
     C*
     C****** BEGIN ADD *** 06/09/05 ***********************************
     C     *IN08         IFEQ      *ON
     C                   MOVE      SCWPP         CKWPP                          Positive payoid/Stop
     C                   MOVE      SCWBK#        CKWBK#                         Wach bank # oid/Stop
     C                   ENDIF                                                  e. IN08 ON
     C****** END ADD ***** 06/09/05 ***********************************
     C*** Begin add *** 05/05/07 **************************************
     C     *IN68         IFEQ      *ON
     C                   MOVE      SCWPP         CKWPP                          Positive payoid/Stop
     C                   ENDIF                                                  e. IN08 ON
     C*** End add ***** 05/05/07 **************************************
     C*** BEG ADD ***** 06/25/09 **************************************
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CKEFDT            7 0
     C                   Z-ADD     SCEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CKEXDT            7 0
     C                   MOVEL     SCTRNS        CKTRNS                         TRANSIT ROUTING #
     C                   MOVEL     SCBNK#        CKBNK#                         TRANSIT BANK #
     C                   MOVEL     SCCKDG        CKCKDG                         TRANSIT CHECK DIGIT
     C                   MOVEL     SCAXOU        CKAXOU                         AUX ON-US FLD UPTO
     C                   MOVEL     SCONUS        CKONUS                         ON-US FLD UP TO 19A
     C                   MOVEL     SCRTUP        CKRTUP                         ROUTING ENTRY UPPER
     C                   MOVEL     SCRTLW        CKRTLW                         ROUTING ENRTY LOWER
     C                   MOVEL     SCNAM1        CKNAM1                         SIG 1 STRING
     C                   MOVEL     SCNAM2        CKNAM2                         SIG 2 STRING
     C                   MOVE      SCIDST        CKIDST                         IMMED DEST
     C                   MOVE      SCIORG        CKIORG                         IMMED ORIG
     C*** Beg add ***** 01/19/12 **************************************
      *** Beg Chg ***** 05/20/13 **************************************
     C************       MOVE      SCIDST        CKIDSA                         IMMED DES ALPHA
      /Free
       //***** Beg Chg ***** 02/13/15 ************************************
       //************CKidsa = %Trim(%Editc(%Dec(scIDST : 10 : 0) : 'Z'));
       CKidsa = scIDST;
       //***** End Chg ***** 02/13/15 ************************************
      /End-Free
      *** End Chg ***** 05/20/13 **************************************
     C                   MOVE      SCIORG        CKIORA                         IMMED ORI ALPHA
     C*** End add ***** 01/19/12 **************************************
     C                   MOVEL     SCBNAM        CKBNAM                         DEST BANK
     C                   MOVEL     SCCNAM        CKCONM                         COMP NAME
     C                   MOVEL     SCCEDS        CKCEDS                         CO ENT DESC
     C                   MOVEL     SCRFCD        CKRFCD                         REF CODE
     C                   MOVE      SCORDI        CKORDI                         ORIG DFI ID
     C                   MOVE      SCSVCL        CKSVCL                         SRV CLASS
     C                   MOVE      SCACID        CKACID                         ACH INTERIM
     C                   MOVE      SCBKLT        CKBKLT                         BANK LEAD TIME
     C                   MOVE      SCBYPN        CKBYPN                         BYPASS PRENOTE
     C*** END ADD ***** 06/25/09 **************************************
      *** Begin Add *** 03/05/14 **************************************
     C                   MOVE      SCEFSP        CKEFSP                         EFT Special Process
      *** End Add ***** 03/05/14 **************************************
      *** beg add ***** 05/20/13 ***********************************************
      /free
          RTspad =   SCSPAD;
          RTcmwc =   SCCMWC;
          RTcmwp =   SCCMWP;
          RTcmag =   SCCMAG;
       //*** Begin Add *** 05/06/15 ********************************************
          CKeft# = scEftSeq#;
       //*** End   Add *** 05/06/15 ********************************************

      /end-free
      *** end add ***** 05/20/13 ***********************************************
      *
     C                   ENDSR
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *** BEG ADD ***** 06/17/03 **************************************
      *** BEG CHG ***** 06/29/05 **************************************
     C*********  *INKJ     CASEQ*ON       KJDO
     C     *INKJ         CASEQ     *ON           KJDO                     90
      *** END CHG ***** 06/29/05 **************************************
     C                   ENDCS
      *** END ADD ***** 06/17/03 **************************************
     C                   ENDSR
      *** BEG ADD ***** 06/17/03 **************************************
      *================================================================
      * KJDO - SEARCH A/R CODES
      *================================================================
     C     KJDO          BEGSR
      *
      *  AR SEARCH
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCRPCD'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY
      *
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCRPCD
      *** BEG CHG ***** 07/18/07 *************************************
     C***********          SETON                     82  90
     C***********          ELSE
     C***********          SETON                     81  90
     C                   SETON                                        32  90
     C                   ELSE
     C                   SETON                                        31  90
      *** BEG CHG ***** 07/18/07 *************************************
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCRPVS'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY
      *
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCRPVS
      *** BEG CHG ***** 07/18/07 *************************************
     C***********          SETON                         90
     C***********          ELSE
     C***********          SETON                     82  90
     C                   SETON                                        33  90
     C                   ELSE
     C                   SETON                                        32  90
      *** END CHG ***** 07/18/07 *************************************
     C                   ENDIF
      *
      *** BEG ADD ***** 07/18/07 **************************************
     C     HFLD          WHENEQ    'SCRDCD'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY
      *
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCRDCD
     C                   SETON                                        34  90
     C                   ELSE
     C                   SETON                                        33  90
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCRDVS'
     C                   MOVE      *BLANKS       QQKEY            10
     C                   CALL      'W2161R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY
      *
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCRDVS
      *** BEG CHG ***** 06/29/09 **************************************
     C*****************    SETON                     35  90
     C     *IN08         IFEQ      *ON
     C     *IN68         OREQ      *ON
     C                   SETON                                        35  90
     C                   ELSE
     C                   SETON                                        36  90
     C                   ENDIF                                                  *IN08, 68
      *** END CHG ***** 06/29/09 **************************************
     C                   ELSE
     C                   SETON                                        34  90
     C                   ENDIF
      *** END ADD ***** 07/18/07 **************************************
      *** BEG ADD ***** 06/25/09 **************************************
     C     HFLD          WHENEQ    'SCNAM1'                                     Sig 1 Search
      *
     C                   CALL      'W2424R'
     C                   PARM      '2'           ##MODE            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CO#
     C                   PARM      *BLANKS       ##NAM1
     C                   PARM      *BLANKS       ##10A1
     C                   PARM      *BLANKS       ##CMD1
      *
     C     ##NAM1        IFNE      *BLANKS
     C                   MOVE      *ON           *IN82
     C                   MOVEL     ##NAM1        SCNAM1
     C                   ELSE
     C                   MOVE      *ON           *IN81
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCNAM2'                                     Sig 2 Search
      *
     C                   CALL      'W2424R'
     C                   PARM      '2'           ##MODE            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CO#
     C                   PARM      *BLANKS       ##NAM2
     C                   PARM      *BLANKS       ##10A2
     C                   PARM      *BLANKS       ##CMD2
      *
     C     ##NAM2        IFNE      *BLANKS
     C                   MOVEL     ##NAM2        SCNAM2
     C                   ELSE
     C                   MOVE      *ON           *IN82
     C                   ENDIF
      *
     C     HFLD          WHENEQ    'SCSVCL'                                     SERVICE CLASS
     C                   MOVEL(P)  'SCLS'        QQTBID            4
     C                   CALL      'W2493R'
     C                   PARM      *BLANK        QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQTBID
     C                   PARM      *BLANK        QQKEY            10
     C                   PARM      *BLANK        QQRTCD            1
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  QQKEY         SCSVCL
     C                   MOVEL(P)  'SCLS'        KKTBID            4
     C                   MOVEL(P)  QQKEY         KKKEY            10
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCSCLD
     C                   MOVE(P)   *ON           *IN85
     C                   END
     C                   ELSE
     C                   MOVE(P)   *ON           *IN84
     C                   END
      *** END ADD ***** 06/25/09 **************************************
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KJDO
      *** END ADD ***** 06/17/03 **************************************
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C*
     C****** BEGIN ADD *** 06/09/05 ***********************************
     C*    Determine Client number
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
14   C                   PARM                    @@NPWC            1            NON-PDS WC  LS
15   C                   PARM                    @@OS              1            OUTOME/SAVINGS
16   C                   PARM                    @@ARLS            1            Auto Release Flag
17   C                   PARM                    @@CCOD            1            Correct Coding Flag
18   C                   PARM                    @@CRVW            1            Code Review Flag
19   C                   PARM                    @@PROM            1            ProMatch Flag
20   C                   PARM                    @@MCRV            1            Medicare CR Flag
21   C                   PARM                    @@FREQ            1            Frequency Flag
22   C                   PARM                    @@CRAC            1            CR Auto Complete
23   C                   PARM                    @@PREO            1            CR Auto Complete
24   C                   PARM                    @@PARN            1            Par/Non-Par/Both
25   C                   PARM                    @@MGRI            1            Multi Group Restr
26   C                   PARM                    @@KCIF            1            Keep Claimant info
27   C                   PARM                    @@CURS            1            PBILLS entry cursor
28   C                   PARM                    @@PCIB            1            Prov Check in Batch
29   C                   PARM                    @@CLT#            3            Client Number
30   C                   PARM                    @@UCPF            1            UCR Percentile
31   C                   PARM                    @@DCF             1            Data Center
32   C                   PARM                    @@AUTB            1            Auto Batch   Flag
33   C                   PARM                    @@DEIF            1            DE/Image Flag
     C                   PARM                    @@AURE            1            Auto-Reinstate
     C*
      *** BEG ADD ***** 06/25/09 **********************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#
      *** END ADD ***** 06/25/09 **********************************
     C*
     C*     If NCA -105- activate Wachovia Download fields.
     C     @@CLT#        IFEQ      '105'
     C*** Begin add *** 07/18/05 **************************************
     C     @@CLT#        OREQ      '071'
     C*** End add ***** 07/18/05 **************************************
      *** BEG ADD *** 04/29/04 ****************************
     C     @@CLT#        OREQ      '114'
      *** END ADD *** 04/29/04 ****************************
     C                   MOVE      *ON           *IN08
     C                   ENDIF                                                  e. CLT#=105
     C****** END ADD ***** 06/09/05 ***********************************
     C*** BEG ADD ***** 07/27/09 **************************************
     C     @@CLT#        IFEQ      '114'
     C                   MOVE      *ON           *IN37
     C                   ELSE
     C                   MOVE      *OFF          *IN37
     C                   ENDIF
     C*** END ADD ***** 07/27/09 **************************************
     C*** Begin add *** 05/05/07 **************************************
     C*     If 139- activate Pos Pay download fields.
     C     CLTID         IFEQ      '139'
     C                   MOVE      *ON           *IN68
     C                   ENDIF                                                  e. CLT#=139
     C*** End add ***** 05/05/07 **************************************
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
      *** beg del ***** 07/30/15 ******************************
     C***********        MOVE      'A'           SCSTAT                         STATUS
      *** end del ***** 07/30/15 ******************************
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF
      *
     C     Q1USER        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ELSE
     C                   MOVE      UPCO#         XXCO#
     C                   END
      *
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
      *
      *** BEG ADD ***** 07/18/07 **********************************
     C                   READ      SWCPF                                  60
     C     WCFL25        IFEQ      'Y'
     C                   MOVE      *ON           *IN41
     C                   ENDIF
      *** END ADD ***** 07/18/07 **********************************
      *** BEG ADD ******* 08/09/07 ********************************
      *** BEG DEL ***** 06/25/09 **********************************
     C***********WCFL35    IFEQ 'Y'
     C***********          MOVE *ON       *IN42
     C***********          ENDIF
      *** END DEL ***** 06/25/09 **********************************
      *** END ADD ******* 08/09/07 ********************************
      *** BEG ADD ***** 06/25/09 **********************************
     C                   SELECT
     C     WCFL17        WHENEQ    'Y'
     C     WCFL24        OREQ      'Y'
     C     WCFL35        OREQ      'Y'
     C                   Z-ADD     3             #OFSCR            1 0
     C                   OTHER
     C                   Z-ADD     2             #OFSCR
     C                   ENDSL
      *** END ADD ***** 06/25/09 **********************************
      *
     C                   ENDSR
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
      *
     C                   UPDATE    DBRCDF
      *
      ****** BEG ADD ***** 03/27/06 ***********************************
      *
     C     *IN45         IFEQ      *ON
     C     BIKY          CHAIN     PBI01                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  CKAI01        BIAI01
     C                   MOVEL(P)  CKAI02        BIAI02
     C                   MOVEL(P)  CKAI03        BIAI03
      *** BEG ADD ***** 06/25/09 **************************************
     C                   MOVEL(P)  CKONUS        BIONUS
     C                   MOVEL(P)  CKCKDG        BICKDG
     C                   MOVEL(P)  CKBNK#        BIBNK#
     C                   MOVEL(P)  CKTRNS        BITRNS
     C                   MOVEL(P)  CKAXOU        BIAXOU
     C                   MOVEL(P)  CKRTUP        BIRTUP
     C                   MOVEL(P)  CKRTLW        BIRTLW
      *** END ADD ***** 06/25/09 **************************************
     C                   UPDATE    PBI01
     C     BIKY          READE     PBI01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C                   ENDIF                                                  *IN45 = *ON
      *
      ****** END ADD ***** 03/27/06 ***********************************
      *
E01  C                   ENDIF
     C                   EXSR      UPDSRT
      *
     C                   ENDSR
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      WSCKP
     C                   EXSR      UPDSRT
     C                   ENDSR
      *================================================================
      * UPDSRT - Update WSRTP
      *================================================================
      /free
       BegSr UPDSRT;
       Clear PRT00;

       //*** Beg Chg ***** 05/20/13 ***********************************************
       //************** xxrtky = CKidst;
       xxrtky = CKidsa;
       //*** End Chg ***** 05/20/13 ***********************************************

       If  xxrtky = *blanks;
          xxrtky = CKTRNS + CKBNK# + %Editc(CKckdg : 'X');
       Endif;

       Chain keyWSRTL PRT00;

       RTco# =XXCO#;
       RTcmwc= SCCMWC;
       RTcmwp= SCCMWP;
       RTcmag= SCCMAG;
       RTspad= SCSPAD;
       RTuser= CKuser;
       RTpgm =CKpgm;
       RTdt =CKdt;
       RTtime= CKtime;
       If %found;
          Update PRT00;
       Else;
          RTco# =XXCO#;
          RTkey =XXRTKY;
          RTIusr= RTuser;
          RTIpgm= RTpgm;
          RTIdt =RTdt;
          RTitm= RTtime;

          Write  PRT00;
          FEOD   WSRTL;
       Endif;

       EndSr;
      /end-free
