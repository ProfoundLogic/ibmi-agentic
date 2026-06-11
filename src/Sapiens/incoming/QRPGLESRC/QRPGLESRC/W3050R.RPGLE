     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W3050R - COMMISSION CHECKS
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 04/30/19 C:062 Project: 152WAB
     H*                Apply Optacomp Mods.
     H*
     H* 09/11/13 C:53 PROJECT 147WCC
     H*               Change MICR positions for OPTACOMP groups.
     H*                                                               *
     H* 07/08/13 C:282 Project: 132WQN Priss #38                      *
     H*                Added the special character (!).               *
     H*                                                               *
     H* 05/20/13 C:190 Project: 132WQN                                *
     H*                Layout changes as per client.                  *
     H*                                                               *
     H* 03/04/13 C:282 Project: 132WQN                                *
     H*                Print Comm/Return Prem Chks/Lansing Ofc        *
     H*                                                               *
     H* 05/29/12 C:180 SUPPORT 79995                                  *
     H*                Increase size of payee info fields to          *
     H*                achieve consistency with reporting of
     H*                same in the positive pay file.
     H*
     H*                                                               *
     H* 09/16/08 C:53  SEQ#: 62654                                    *
     H*                QUALIFY CHANGES FOR ITEM # 62064               *
     H*                                                               *
     H* 07/01/08 C:33  Project: 46WFQ   WA046/64205                   *
     H*                Always print 1st signature line for clt046     *
     H*                                                               *
     H* 04/09/08 C:53  Project: 93WHC                                 *
     H*                Back to base with the following changes:       *
     H*                08/16/06 51434 - Lowered the micr line by 1/4".
     H*                06/23/05 58360 - Adjust bank name, date & check #
     H*
     H* 02/25/08 C:53  SEQ#: 62064
     H*                ALLIGN MICR LINE TO MATCH THE WC CHECKS.
     H*                                                               *
     H* 02/08/08 C:61  Project: 46WFQ                                 *
     H*                Always print 2nd signature line for clt046     *
     H*                                                               *
     H* 10/08/07 C:33  Project: 71WHI  WA071/59809                    *
     H*                Always print 2nd signature line for clt071     *
     H*                                                               *
     H* 06/20/07 C:53 114WMF  BACK TO BASE-114WED                     *
     H* 07/26/02 C:61  Project: 114WED                                *
     H*                Changed program to print 'Void after x days',  *
     H*                where x is BI50N3 (a value moved to a 3.0      *
     H*                numeric field for output purposes).  If the    *
     H*                number of days is zero, the verbiage will not  *
     H*                be printed on the checks.                      *
     H*                                                               *
     H* 04/27/07 C:15  Project: 71WHI                                 *
     H*                Chgd pgm to print 1st signature line for clt   *
     H*                71.                                            *
     H*                                                               *
     H* 02/26/07 C:132 Project: 90WEY
     H*                Add void msg, BACK TO BASE
     H*                *IN78 condition 1st signature line to print
     H*                                                               *
     H* 11/29/05 C:53  Project: 139WAD
     H*                Add void msg.
     H*                *IN78 condition 1st signature line to print
     H*
     H* 11/28/05 C:53  Project: 136WAH
     H*                Add void msg.
     H*                *IN78 condition 1st signature line to print
     H*                                                               *
     H* 09/07/05 C:53  Project: 129WLV/WC1340
     H*                ADD FLAG FOR USING THE CURRENT DATE
     H*
     H* 06/23/05 C:53  Project: WC
     H*                MOVE DATE AND CHECK # ON CHECK
     H*                                                               *
     H* 06/30/03 C:15 Project: WC1133                                 *
     H*               Moved the bank name and added the bank address. *
     H*                                                               *
     H*  10/12/00 C:15  Project: WC577                                *
     H*                -Added the fund name to the check stub.        *
     H*                -Fixed the condition which prints the second   *
     H*                 signature line.                               *
     H*                                                               *
     H*  12/14/98 C:15  Project:                                      *
     H*                 Changed the filecopies to pull paper from the *
     H*                 second drawer of the printer.                 *
     H*                                                               *
     H*  10/29/98 C:15  Project:                                      *
     H*                 Lowered the routing upper and lower numbers.  *
     H*                                                               *
     H* 05/20/98  C:33  SEQ#: WA095/11653                             *
     H*                 ADD GROUP # TO CHECK                          *
     H*                                                               *
      * 02/02/98  47
      *           PRINT GROUP NAME/ADDRESS IN UPPER LEFT CORNER.
      *
     H*  12/16/97 C:30 Project:  WC500                                *
     H*           If AGCCKM NE AGMOD, access WMAGP using AGCCKM Mod   *
     H*           value and assign to Check Name and Address.         *
     H*                                                               *
     H*  11/25/97 C:30 Project:  WC500                                *
     H*           Print Laser Checks with Bank MICR.                  *
     H*****************************************************************
     FWAR2L     IF   E           K DISK
     FWAC2L     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMFNL     IF   E           K DISK
      *______________________________________________________________________
      *  Bank Information file - CO#/APPL/GRP/APPL/ACCT#/EFDT(D)
     FWMBIL1    IF   E           K DISK
      *______________________________________________________________________
      *** BEG ADD **** 03/04/13 *************************************
     Fwxrpc     o    e             disk
      *** End ADD **** 03/04/13 *************************************
      *
     FCHECKS    O    F  145        PRINTER OFLIND(*INOF)
     FFILECOPY  O    F  145        PRINTER OFLIND(*INOA)
     F                                     INFDS(PIDS01)
      *

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D SIGMS1          S              4    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 1
     D SIGMS2          S             26    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 2
      *** BEG ADD **** 03/04/13 *************************************
     dcharAmt          s             10
     dintDate          s              8  0
     dCharDate8        s              8
     dintRedDay        s              5  0
     dcharErr          s              1
      *** End ADD **** 03/04/13 *************************************
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D*
     D                 DS
     D  XXTRAN                 1     11
     D  XXAMPR                 1      1
     D  BITRNS                 2      5
     D  BIBNK#                 6      9
     D  BICKDG                10     10
     D  XXAMP2                11     11
     D                 DS
     D  XXCNBR                 1     10
     D  XXSLSH                 1      1
     D  R2CHCK                 2      8  0
     D  XXSLS2                 9      9
     D  XXBLK1                10     10
     D                 DS
     D  VDY                    1      3
     D  VDY1                   1      1
     D  VDY2                   2      2
     D  VDY3                   3      3
      **** Header *****
      *** BEG CHG **** 05/20/13 *************************************
     d***** RPrmComHeader                283
      *** BEG CHG **** 07/08/13 *************************************
     d***** RPrmComHeader                291
     d RPrmComHeader                292
      *** END CHG **** 07/08/13 *************************************
      *** END CHG **** 05/20/13 *************************************
     d charHeadRw                     1    overlay(RPrmComHeader)
     d charChkProf                    3    overlay(RPrmComHeader:2)
     d charCol1                       7    overlay(RPrmComHeader:5)
     d charBankId                     3    overlay(RPrmComHeader:12)
     d charCol2                       3    overlay(RPrmComHeader:15)
     d charPaymentMd                  3    overlay(RPrmComHeader:18)
     d charCol3                       4    overlay(RPrmComHeader:21)
     d charCashAcctId                 3    overlay(RPrmComHeader:25)
     d charCol4                       2    overlay(RPrmComHeader:28)
     d charChkNumber                  9    overlay(RPrmComHeader:30)
     d charCol5                       6    overlay(RPrmComHeader:39)
     d charPaymentDt                  8    overlay(RPrmComHeader:45)
     d charCol6                       2    overlay(RPrmComHeader:53)
     d charVendorId                   9    overlay(RPrmComHeader:55)
     d charCol7                       6    overlay(RPrmComHeader:64)
     d charPayeeNm                   45    overlay(RPrmComHeader:70)
     d charCol8                      25    overlay(RPrmComHeader:115)
     d charPayeeNm2                  30    overlay(RPrmComHeader:140)
      *** BEG CHG **** 05/20/13 *************************************
     d***** charPayeeNm3                  30    overlay(RPrmComHeader:170)
     d***** charPayeeAddr                 28    overlay(RPrmComHeader:200)
     d***** charCol9                       2    overlay(RPrmComHeader:228)
     d charPayeeAddr                 50    overlay(RPrmComHeader:170)
     d charCol9                      10    overlay(RPrmComHeader:220)
      *** END CHG **** 05/20/13 *************************************
     d charPayeeCty                  15    overlay(RPrmComHeader:230)
     d charCol10                      2    overlay(RPrmComHeader:245)
     d charPayeeSt                    2    overlay(RPrmComHeader:247)
     d charCol11                      3    overlay(RPrmComHeader:249)
      *** BEG CHG **** 05/20/13 *************************************
     d****** charPayeeZip                   9    overlay(RPrmComHeader:252)
     d****** charCol12                      9    overlay(RPrmComHeader:261)
     d****** charCheckAmt                  14    overlay(RPrmComHeader:270)
     d charPayeeZip                   5    overlay(RPrmComHeader:252)
     d charCol12                     13    overlay(RPrmComHeader:257)
     d charCheckAmt                  14    overlay(RPrmComHeader:270)
     d charCol19                      8    overlay(RPrmComHeader:284)
      *** END CHG **** 05/20/13 *************************************
      *** BEG ADD **** 07/08/13 *************************************
     d charSpl1                       1    overlay(RPrmComHeader:292)
      *** END ADD **** 07/08/13 *************************************
      **** Detail *****
      *** BEG CHG **** 05/20/13 *************************************
     d******* RPrmComDetail                151
      *** BEG CHG **** 07/08/13 *************************************
     d***** RPrmComDetail                159
     d RPrmComDetail                160
      *** END CHG **** 07/08/13 *************************************
      *** END CHG **** 05/20/13 *************************************
     d charDtlLn                      1    overlay(RPrmComDetail)
     d charVoucNm                     9    overlay(RPrmComDetail:2)
     d charCol13                      4    overlay(RPrmComDetail:11)
     d charInvoiceNm                 15    overlay(RPrmComDetail:15)
     d charCol14                     14    overlay(RPrmComDetail:30)
     d charInvoiceDt                  8    overlay(RPrmComDetail:44)
     d charCol15                      3    overlay(RPrmComDetail:52)
     d charDesp                      40    overlay(RPrmComDetail:55)
     d charCol16                      7    overlay(RPrmComDetail:95)
     d charGrossAmt                  14    overlay(RPrmComDetail:102)
     d charCol17                     16    overlay(RPrmComDetail:116)
     d charDiscount                   4    overlay(RPrmComDetail:132)
     d charCol18                      2    overlay(RPrmComDetail:136)
     d charNetAmt                    14    overlay(RPrmComDetail:138)
      *** BEG ADD **** 05/20/13 *************************************
     d charCol20                      8    overlay(RPrmComDetail:152)
      *** END ADD **** 05/20/13 *************************************
      *** BEG ADD **** 07/08/13 *************************************
     d charSpl2                       1    overlay(RPrmComDetail:160)
      *** END ADD **** 07/08/13 *************************************
      *
      * Date in Char
     d charDate                       6
     d charMM                         2    Overlay(charDate)
     d charDD                         2    Overlay(charDate:3)
     d charYY                         2    Overlay(charDate:5)
      *
      * Date Data Structure
     d                 DS
     d intDate8                1      8  0
     d intMM                   1      2  0
     d intDD                   3      4  0
     d intYY                   5      8  0
      *
      * Month End Date
     dGetMonthEnd      pr                  ExtPgm('SRPAST8')
     d intDate2                       8  0
     d intRedDay                      5  0
     d charErr                        1
      *** End Add **** 03/04/13 *************************************
      *
     D XVMSG1          C                   CONST('Void after')
     D XVMSG2          C                   CONST('days')
      *
     D XCACA           C                   CONST('AGENT COMMISSION -
     D                                     ACCOUNT')
     D VOID90          C                   CONST('VOID AFTER 90 DAYS')
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PIDS01        E DS                  EXTNAME(SZQ4P)
      *

     C     BIKY1         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    R2FND
     C                   KFLD                    QQACCT
     C                   KFLD                    XXDT
      *
     C     BIKY2         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    R2FND
     C                   KFLD                    QQACCT
      *
      *  KEY LIST FOR REQUEST COMMISSION PAYMENT FILE
      *
     C     R2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  CHECK SCHEDULE FILE
      *
     C     C2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDT
     C                   KFLD                    XXFND
      *
      *  FUND MASTER
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE AGENT ASSIGNMENT MASTER FILE
      *
     C     AAKEY         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    R2FND
     C                   KFLD                    R2AGT#
      *
      *   KEY LIST FOR THE AGENT MASTER FILE
      *
     C     AGKEY         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      *
     C     AGKY2         KLIST
     C                   KFLD                    R2CO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    XXCCKM
      *
     C     *LIKE         DEFINE    AGCCKM        XXCCKM
     C     *LIKE         DEFINE    BIEFDT        XXDT
     C     *LIKE         DEFINE    BIEFDT        XXCYMD
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQACCT           20
     C                   PARM                    QQCURD            1
      *
      *  Assign 'AG' for "WCMS" Bank Info file
      *
     C                   MOVE      'AG'          XXAPPL
      *
      ****** PERFORM CVTHEX TO GET ATRN VALUE TO PASS TO THE PRINTER **
      *****                                                          **
     C                   MOVEL     '03011B'      CHR             256
     C                   MOVE      *BLANKS       HEX             128
     C                   MOVE      *BLANK        RTCODE            1
     C                   CALL      'CVTHEX'
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE
     C                   MOVEL     HEX           ESC               3
      *
     C                   MOVE      QQDATE        X1DATE            8 0
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C*
     C*** USE CURRENT DATE IF FLAGGED
     C*
     C     QQCURD        IFEQ      'Y'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        XXDATE
     C                   ENDIF
     C*
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      '/'           XXSLSH            1
     C                   MOVE      '/'           XXSLS2            1
     C                   MOVE      '&'           XXAMPR            1
     C                   MOVE      '&'           XXAMP2            1
     C                   MOVE      *BLANKS       XXBLK2            2
     C                   MOVE      *BLANKS       XXBLK1            1
      *
      *  GET THE ENTRY FROM THE CHECK PROCESSING FILE
      *
     C     C2KEY         CHAIN     PC201                              60
     C     *IN60         IFEQ      '0'
      *
     C                   MOVE      ' '           XXSTRT
      *
     C     R2KEY         CHAIN     WAR2L                              50
      *
      *  IF THE REPLACEMENT CHECK IS NOT ZERO, WE NEED TO START WITH
      *  A CERTAIN CHECK
      *
     C     *IN50         DOWEQ     '0'
      *
     C     C2RSTR        IFNE      *ZERO
     C     C2RSTR        IFNE      R2CHCK
     C     XXSTRT        IFEQ      ' '
     C                   GOTO      NCHK#
     C                   END
     C                   ELSE
     C                   MOVE      'Y'           XXSTRT            1
     C                   END
     C                   END
      *
      *  RETRIEVE NAME AND MAILING ADDRESS - USE EITHER SECRETARY OR
      *  AGENT NAME.
      *
      *
     C                   MOVE      *BLANKS       XXNAME           35
     C                   MOVE      *BLANKS       XXADDR           35
     C                   MOVE      *BLANKS       XXMCSZ           35
     C     AAKEY         CHAIN     PAA01                              51
     C     *IN51         IFEQ      '0'
      *
     C     AGKEY         CHAIN     PAG01                              51
      *
     C     *IN51         IFEQ      '0'
      *
      *
      *  If "Check Mailed to Modifier" is not equal to the "Agent Modifier",
      *  access the Agent Master file using the "Mailed to Modifier" value.
      *  Then proceed with processin.
      *
     C     AGMOD         IFNE      AGCCKM
     C                   Z-ADD     AGCCKM        XXCCKM
     C     AGKY2         CHAIN     PAG01                              51
      *
     C     *IN51         IFEQ      *ON
      *  Re-read original Agent master record
     C     AGKEY         CHAIN     PAG01                              51
     C                   ENDIF                                                  *IN51 *ON
     C                   ENDIF                                                  AGMOD NE AGCCKM
      *
      *
     C                   MOVEL     AGNAME        XXNAME
     C                   MOVEL     AGMAD1        XXADDR
      *
      *   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
      *
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGMST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
      *
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGMCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXMCSZ
      *
     C                   END
     C                   END
      *
      *  LOAD DOLLAR AMOUNT SPELLED OUT
      *
     C                   MOVE      R2AMT         QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVEL     QQAMT1        XXAMT1
     C                   MOVEL     QQAMT2        XXAMT2
      *
      ***REVERSE DATE TO PRINT
      *****
      *
      *  Access Fund Master to obtain Fund Short Name
      *
     C     FNKY          CHAIN     PFN01                              60
      *
      *** beg add *** 09/11/13 ****************************
     C     ##CLT#        IFEQ      '114'
     C     FNGRPASC      ANDEQ     'OPTACOMP'
     C*** Begin Add *** 04/30/19 **************************
     C     ##CLT#        OREQ      '152'
     C     FNGRPASC      ANDEQ     'OPTACOMP'
     C*** End   Add *** 04/30/19 **************************
     C                   MOVE      *on           *in20
     C                   ELSE
     C                   MOVE      *off          *in20
     C                   END
      *** end add *** 09/11/13 ****************************
      **** GET BANK INFORMATION TO PRINT ON CHECK ****
      *
     C                   Z-ADD     XXDT          XXCYMD                         Batch Date CYMD
      *
     C     BIKY1         SETLL     PBI01
     C     BIKY2         READE     PBI01                                  76
      *
     C     *IN76         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY2         READE     PBI01                                  76
     C                   ENDDO
      *
     C     *IN76         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C* Check data client data area
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C                   MOVE      *BLANKS       @@VOID           19
     C*
     C     ##CLT#        IFEQ      '093'
     C                   MOVE      *ON           *IN80
     C                   ELSE
     C                   MOVE      *OFF          *IN80
     C                   ENDIF
     C     ##CLT#        IFEQ      '146'
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      *OFF          *IN81
     C                   ENDIF
     C*
     C                   SELECT
     C     ##CLT#        WHENEQ    '136'
     C                   MOVE      *ON           *IN15                          COND XXX
     C                   MOVE      *BLANKS       @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '139'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '090'
     C                   MOVEL     VOID90        @@VOID
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '071'
     C                   MOVE      *ON           *IN78                          1ST UNDERLINE
     C*
     C     ##CLT#        WHENEQ    '046'
     C                   MOVE      *ON           *IN78                          1st Underline
     C*
     C                   OTHER
     C*
      *
      * This code initializes the 'Void after x days' display field
      * If the number of days (BI50N3) is *zero the message
      * field (XVOAFT) is blanked out
      *
     C                   MOVE      *BLANKS       XVOAFT
      *
     C     BI50N3        IFGT      *ZERO
     C                   MOVE      BI50N3        XVDAYS            3 0
     C                   MOVE      XVDAYS        VDY
     C                   MOVE      *BLANKS       XXVDAY            3
      *
     C     XVDAYS        IFLT      10
     C                   MOVE      *BLANK        VDY1
     C                   MOVE      *BLANK        VDY2
     C                   MOVEL     VDY3          XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFLT      100
     C                   MOVE      *BLANK        VDY1
     C                   MOVEL     VDY2          XXVDAY
     C     XXVDAY        CAT       VDY3:0        XXVDAY
      *
     C                   ELSE
      *
     C     XVDAYS        IFGT      99
     C                   MOVE      VDY           XXVDAY
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVEL     XVMSG1        XVOAFT           19
     C     XVOAFT        CAT       XXVDAY:1      XVOAFT
     C     XVOAFT        CAT       XVMSG2:1      XVOAFT
     C                   MOVEL     XVOAFT        @@VOID
     C                   MOVE      *OFF          *IN78
     C                   ENDIF
      *
     C*
     C                   MOVE      *OFF          *IN78
     C*
     C*
     C                   ENDSL
     C*
      *
      *  Assign MACRO to PCL5 control characters with Bank Information Macro ID
      *
     C                   MOVE      BIMACR        TEMP4             4
     C     '&f'          CAT       TEMP4         TEMP6             6
     C     TEMP6         CAT       'y3X'         MACRO             9
      *
     C     BIAXOU        IFEQ      *BLANK
     C                   MOVE      *BLANKS       XXAXOU
     C                   MOVE      XXCNBR        XXAXOU           20
     C                   ELSE
     C                   MOVE      BIAXOU        XXAXOU
     C                   END
      *
     C     BIBNK#        CAT(P)    '/'           XXBT#5            5
     C     XXBT#5        CAT(P)    BITRNS        XXBT#             9
      *
      *
      *
     C     BILOGO        IFEQ      'N'
     C                   MOVE      '1'           *IN73
     C                   ENDIF
      *
     C     BITRST        IFEQ      'N'
     C                   MOVEL(P)  XCACA         XXWCA            35
     C                   END
      *
      *  Determine if Second Signature Verbage should Print
      *
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN83
     C     BI$LMT        IFGT      0
     C     R2AMT         ANDGT     BI$LMT
     C                   MOVE      '1'           *IN85
     C                   MOVE      *ON           *IN83
     C                   END
     C*
     C                   SELECT
      *
     C     ##CLT#        WHENEQ    '071'
     C                   MOVE      *ON           *IN83
      *
     C     ##CLT#        WHENEQ    '046'
     C                   MOVE      *ON           *IN83
      *
     C                   ENDSL
      *
      * Check for First Signature entry
      *
     C     BI10A1        IFNE      *BLANK
     C                   MOVEL     BI10A1        CH1               7
     C                   MOVE      '1'           *IN86
     C                   END
      *
      * Check for Second Signature entry
      * If $$$ Limit entered and Check amount greater than limit,
      * then do NOT print signature
      *
     C                   MOVE      *OFF          *IN87
     C     BI10A2        IFNE      *BLANK
     C                   MOVEL     BI10A2        CH2               7
     C                   MOVE      '1'           *IN87
     C                   MOVE      *ON           *IN83
     C                   END
      *** Beg Add **** 03/04/13 *************************************
      /Free
       If ##CLT# = '132';

       // Populating Header Values.
          charHeadRw     = 'H';
          charChkProf    = 'CWC';
          charBankId     = *Blanks;
          charPaymentMd  = 'CCK';
          charCashAcctId = *Blanks;
          charVendorId   = %EditC(R2agt#:'X');
          charChkNumber  = '00' + %EditC(R2chck:'X');
          charAmt        = %EditC(R2amt:'3');
          charCheckAmt   = '    ' + charAmt;
          charDate       = %EditC(xxDATE:'X');
          charPaymentDt  = charMM + '/' + charDD + '/' + charYY;

          // Populate the Payee related information for Header
          charPayeeNm    = xxNAME;
          charPayeeAddr  = xxADDR;
          charPayeeCty   = AGmcty;
          charPayeeSt    = AGmst;
          charPayeeZip   = %EditC(AGmzip:'X');
       // *** BEG ADD **** 07/08/13 *************************************
          charSpl1      = '!';
       // *** END ADD **** 07/08/13 *************************************

       // Writing the Header Record in work file WXRPC.
          RPCDTA1 = RPrmComHeader;
          Write RPC01;

       // Populating Detail Values.
          charDtlLn     = 'D';
          charVoucNm    = *Blanks;
          charInvoiceNm = *Blanks;

       // Retriving the Month End Date
          intDate8 = x1Date;
          intDD = 01;
          If intMM = 12;
             intMM = 01;
             intYY = intYY + 1;
          Else;
             intMM = intMM + 1;
          Endif;
          intDate = intDate8;
          intRedDay = 1;
          GetMonthEnd(intDate:intRedDay:charErr);

       // BEG CHG **** 05/20/13 *************************************
       //**** charInvoiceDt = %EditC(intDate:'X');
          CharDate8 = %EditC(intDate:'X');
         charInvoiceDt = %subst(CharDate8:1:2) + '/' +
          %subst(CharDate8:3:2) +  '/' + %subst(CharDate8:7:2);
       // END CHG **** 05/20/13 *************************************
          charDesp      = 'COMMISSION PAYMENT';
          charGrossAmt  = '    ' + charAmt;
          charDiscount  = '0.00';
          charNetAmt    = '    ' + charAmt;
       // *** BEG ADD **** 07/08/13 *************************************
          charSpl2      = '!';
       // *** END ADD **** 07/08/13 *************************************
       // Writing the Detail Record in work file WXRPC.

          RPCDTA1 = RPrmComDetail;
          Write RPC01;

       Else;
      /End-Free
      *** End Add **** 03/04/13 *************************************
      *
      *  PRINT CHECK
      *
      *
     C  N77              EXCEPT    BANNER
     C  N77              EXCEPT    BANNEF
      *
     C                   EXCEPT    CHECK
     C                   EXCEPT    CHECKF
     C                   EXCEPT    DET
     C                   EXCEPT    DETF
      *
      *** Beg Add **** 03/04/13 *************************************
      /Free
       EndIf;
      /End-Free
      *** End Add **** 03/04/13 *************************************
     C     NCHK#         TAG
     C                   MOVE      '1'           *IN77
      *
     C     R2KEY         READE     WAR2L                                  50
      *
     C                   END
     C                   END
      *
     C     TERM          TAG
     C                   SETON                                        LR
      *
      * ================================================================
      *      O U T P U T
      * ================================================================
      *
     OCHECKS    E   N77      BANNER      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portriat Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O                                              '&l1H'
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O                                              'CHECKS FOR FUND: '
     O                       R2FND
      * ================================================================
     OCHECKS    E            CHECK       0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O                                              '&l1H'
     O                       ESC
     O                       MACRO
      * ******************************************************************
     O* ** MICR BANK INFO *****
     O*
     O*                                        MICR Font Change
     O          E            CHECK       0  0
     O                       ESC
     O                                              '(8M'
     O                       ESC
     O                                              '(s0p8h8v0s0b80T'
     O*** beg add *** 09/11/13 **************************
      * added condition N20 to the next 3 printed lines.
     O*** eng add *** 09/11/13 **************************
      **** MICR LINE BASE *****
     O          E   N80N81N20CHECK       0  0
     O                       ESC
     O                                              '*p20x980Y'
     O                       XXAXOU
     O*
     O          E   N80N81N20CHECK       0  0
     O                       ESC
     O                                              '*p770x980Y'
     O                       XXTRAN
     O*
     O          E   N80N81N20CHECK       0  0
     O                       ESC
     O                                              '*p1185x980Y'
     O                       BIONUS
      **** MICR LINE CLT093 *****
     O          E    80      CHECK       0  0
     O                       ESC
     O                                              '*p20x1055Y'
     O                       XXAXOU
     O*
     O          E    80      CHECK       0  0
     O                       ESC
     O                                              '*p770x1055Y'
     O                       XXTRAN
     O*
     O          E    80      CHECK       0  0
     O                       ESC
     O                                              '*p1185x1055Y'
     O                       BIONUS
      **** MICR LINE CLT146 *****
     O          E    81      CHECK       0  0
     O                       ESC
     O                                              '*p15x980Y'
     O                       XXAXOU
     O*
     O          E    81      CHECK       0  0
     O                       ESC
     O                                              '*p765x980Y'
     O                       XXTRAN
     O*
     O          E    81      CHECK       0  0
     O                       ESC
     O                                              '*p1180x980Y'
     O                       BIONUS
     O*
     O*** beg add *** 09/11/13 **************************
      **** MICR LINE OPTACOMP *****
     O          E    20      CHECK       0  0
     O                       ESC
     O                                              '*p30x1035Y'
     O                       XXAXOU
     O          E    20      CHECK       0  0
     O                       ESC
     O                                              '*p780x1035Y'
     O                       XXTRAN
     O          E    20      CHECK       0  0
     O                       ESC
     O                                              '*p1230x1035Y'
     O                       BIONUS
     O*** end add *** 09/11/13 **************************
     O          E            CHECK       0  0
      *                                                Font Change
     O**  Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O                                              '*p2250x120Y'
     O                       ESC
     O                                              '&d1D'
     O                       BIRTUP
     O                       ESC
     O                                              '&d@'
     O          E            CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O                                              '*p2250x155Y'
     O                       BIRTLW
     O          E            CHECK       0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1850x165Y'
     O                       R2CHCK
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
     O** ITALIC, NOT BOLD                   '(s0p12h10v1s0T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECK       0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x155Y'
     O                       XXDATE        Y
     O          E            CHECK       0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
     O          E            CHECK       0  0
      *                                                Font Change
     O**                                               8 PT.
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1710x225Y'
     O                       BIAI01
     O          E            CHECK       0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1710x260Y'
     O                       BIAI02
      *
     O          E            CHECK       0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1710x295Y'
     O                       BIAI03
     O* *******************************************************************
     O          E            CHECK       0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       R2AMT                  '  ,   , $0.  '
     O*
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       XXAMT1
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       XXAMT2
     O          E            CHECK       0  0
     O*   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p2020x500Y'
     O                       @@VOID
     O          E            CHECK       0  0
      *  Courier 10, using 12 point, 10 pitch  "Bitmapped" font
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p10h12v0s0b3T'
      *                                           CHECK NAME/ADDRESS
     O                       ESC
     O                                              '*p250x650Y'
     O                       XXNAME
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x700Y'
     O                       XXADDR
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x750Y'
     O                       XXMCSZ
      *
      *  UNDERLINE FOR FIRST SIGNATURE
      *
     O          E    78      CHECK       0  0
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *  Print underline for Second Signature in 2 situations:
      *     1) Second Signature is entered.
      *     2) Zero < $$$ Limit < Check Amount.
      *
     O          E    83      CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *********************************************************************
      *  Signatures and $$$ limit
      *
     O          E    85      CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *       E 00     85      CHECK
     O                       ESC
     O                                              '*p1400x605Y'
     O*       E 00     85      CHECK
     O                       SIGMS1
     O                       BI$LMT                 ' ,   ,   , $0'
     O                       SIGMS2
      *
     O          E    87      CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD2
     O                       CH2
      *
     O          E    86      CHECK       0  0
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD1
     O                       CH1
      *
      ***************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      ***************************************************************
     O          E            CHECK       0  0
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECK       0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
     O*
     O          E            DET            1 27
     O                                           16 'GROUP #'
     O                       R2FND         Z     22
     O                       FNNAME              84
     O          E            DET            1 28
     O                                           16 'AGENT #'
     O                       R2AGT#        Z     22
     O                                           46 'COMMISSION PAYMENT'
     O                       R2AMT         J     74
     **====================================================================*
     OFILECOPY  E   N77      BANNEF      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portriat Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O                                              '&l4H'
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O                                              'FILE COPY FOR FUND: '
     O                       R2FND
     OFILECOPY  E            CHECKF      0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O                                              '&l4H'
     O                       ESC
     O                       MACRO
     O          E            CHECKF      0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1850x165Y'
     O                       R2CHCK
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECKF      0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
     O** ITALIC, NOT BOLD                   '(s0p12h10v1s0T'
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECKF      0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECKF      0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECKF      0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x155Y'
     O                       XXDATE        Y
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O          E            CHECKF      0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
     O          E            CHECKF      0  0
      *                                                Font Change
     O**                                               8 PT.
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECKF      0  0
      *                                                BANK NAME
     O                       ESC
     O                                              '*p1710x225Y'
     O                       BIAI01
     O          E            CHECKF      0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1710x260Y'
     O                       BIAI02
      *
     O          E            CHECKF      0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1710x295Y'
     O                       BIAI03
     O*********************************************************************
     O          E            CHECKF      0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECKF      0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       R2AMT                  '  ,   , $0.  '
     O*
     O          E            CHECKF      0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       XXAMT1
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       XXAMT2
     O          E            CHECKF      0  0
      *                                           CHECK NAME/ADDRESS
     O                       ESC
     O                                              '*p250x650Y'
     O                       XXNAME
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x700Y'
     O                       XXADDR
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x750Y'
     O                       XXMCSZ
     O*
     O          E    85      CHECKF      0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*       E 00     85      CHECKF
     O                       ESC
     O                                              '*p1400x605Y'
     O*
     O          E            CHECKF      0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1600x700Y'
     O                                              'FILE COPY'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1500x830Y'
     O                                              'NON-NEGOTIABLE'
     O*
     O****************************************************************
      ***************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      ***************************************************************
     O          E            CHECKF      0  0
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECKF      0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
     O*
     O          E            DET            1 39
     O                                           16 'GROUP #'
     O                       R2FND         Z     22
     O                       FNNAME              84
     O          E            DETF           1 40
     O                                           16 'AGENT #'
     O                       R2AGT#        Z     22
     O                                           46 'COMMISSION PAYMENT'
     O                       R2AMT         J     74
** SIGNATURE MESSAGE PART ONE
OVER
** SIGNATURE MESSAGE PART TWO
 REQUIRES SECOND SIGNATURE
