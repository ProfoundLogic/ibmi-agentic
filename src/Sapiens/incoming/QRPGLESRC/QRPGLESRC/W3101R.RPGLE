     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H/TITLE W3101R - GENERATE AGENT ACCOUNT CURRENT BILLING TRANSACTIONS
     H option(*nodebugio:*srcstmt)
     H*********************************************************************
     H* PROGRAM DESCRIPTION:                                              *
     H*   THIS PROGRAM READS THE A/R FILE (WMEAP) AND WRITES THE NECESSARY*
     H*   RECORDS TO THE AGENCY BILLING DETAIL FILE (WDABP).  THE         *
     H*   COMMISSION RATE, COMMISSION AMOUNT, AND NET DUE ARE THEN        *
     H*   CALCULATED AND CAPTURED IN THE FILE.  EACH SELECTED A/R TRANS   *
     H*   WILL BE WRITTEN TO THE FILE, EVEN IF IT DOES NOT ISSUE COMM.    *
     H*   THE COMMISSION RATE, AMOUNT AND NET DUE WILL BE ZERO FOR THESE  *
     H*   TRANSACTIONS.  BALANCE INFORMATION IS RECALCULATED AND WRITTEN  *
     H*   TO THE ACCOUNT CURRENT SUMMARY FILE (WSASP).  RE-RUNNING THIS   *
     H*   PROCESS WILL DELETE 'ALL' CURRENT TRANSACTIONS FOR THE          *
     H*   CORRESPONING STATEMENT DATE (AND AGENT, IF ENTERED) IN WDABP.   *
     H*   MANUAL ENTRIES WILL NOT BE DELETED.                             *
     H*                                                                   *
     H*  CALLED FROM:                                                     *
     H*    W3101C                                                         *
     H*  CALLS TO:                                                        *
     H*    NONE                                                           *
     H*                                                                   *
     H*  CREATED: 04/29/99     C:33    PROJECT: 96WAD                     *
     H*  IN BASE: 07/19/99     C:33    PROJECT: WC899                     *
     H*********************************************************************
     H* REVISIONS:                                                        *
    H*                                                                   *
    H* 04/30/15 C:33  PHW33902                                           *
    H*                Delete records created by this process.            *
    H*                                                                   *
    H* 06/16/08 C:136 Support: WA132 63244                               *
    H*                Don't delete voids                                 *
    H*                                                                   *
    H* 08/22/06 C:09  Support: WA132 54328                               *
    H*                We should not be paying commission on either       *
     H*                REFUNDS or TRUE VOID transactions issued against   *
    H*                those refunds.                                     *
    H*                                                                   *
    H* 08/08/06 C:09  Support: WA132 53985                               *
    H*                In addition to Manual adjustments (ABMADJ), do     *
     H*                not delete Refund (Return Premium) transactions    *
    H*                from the Account Current Detail file (WDABP).      *
    H*                                                                   *
    H* 04/18/06 C:09  Support: WA132 52173                               *
    H*                Add code for 'T' type A/R Codes.                   *
     H*                                                                   *
     H* 11/05/04 C:33  Project: WC1033                                    *
     H*                Load ABFEIN & ABMOD.                               *
     H*                                                                   *
     H* 01/09/04 C:136 PROJECT: WC1115                                    *
     H*                Change program to exclude the policy if an active  *
     H*                vendor history record exists. Refer to previous    *
     H*                item dated 02/18/03.                               *
     H*                                                                   *
     H*  02/18/03 C:136 Project: WC1115 (Premium/Payroll Export/Import)   *
     H*                 Do not process if vendor number is assigned to    *
     H*                 policy.                                           *
     H*                                                                   *
     H*  04/02/01  C:132 SEQ#: WA107/26913                                *
     H*                  Add Other CatchAll to Rate Date Compares, For    *
     H*                  Short Policy Year & Renewal Date in Second year  *
     H*                                                                   *
     H*  08/10/00  C:33  SEQ#: WA109/23913                                *
     H*                  CHECK FOR INACTIVE RATES.                        *
     H*
     H*  07/28/00  C:58  PROJECT: WC993  Remove test dump routine.
     H*
     H*  06/22/00  C:58  PROJECT: WC993  Remove W3101C QRYSLT skips grp 999
     H*                  Change WMEAP to fund-sequenced file, get fund mas-
     H*                  ter, bypass if group FNTEST flag = 'Y.'
     H*                                                                   *
     H*  05/18/00  C:33  PROJECT: WC899                                   *
     H*                  Calculate ASBILL and ASPAID correctly.           *
     H*                                                                   *
     H*  05/17/00  C:33  PROJECT: WC899   WA107/22067                     *
     H*                  Do not load commission rate, amount, or net when *
     H*                  processing payment types (ie. P, I or Q).        *
     H*                                                                   *
     H*  03/13/00  C:33  SEQ#: WA107/21862                                *
     H*                  Read every record in the A/R file.               *
     H*                                                                   *
     H*  01/21/00  C:33  SEQ#: WA096/21515                                *
     H*                  Load ABNET even if no commission is paid.        *
     H*                                                                   *
     H*  12/15/99  C:33  PROJECT: WC899                                   *
     H*                  Use Voids and Refund differently.                *
     H*                                                                   *
     H*  08/31/99  C:33  PROJECT: WC899                                   *
     H*                  Add processing for secondary agent               *
     H*********************************************************************
     FWMFNL     IF   E           K DISK
     F***  BEGIN CHG  ***  06/22/99  **************************************
     F***********************ave any group 999 records in it due to a QRYSLT
     F***********************fo is true only at the client site * *)********
     F****************WMEAL19   IF   E           K DISK
     F*
     FWMEAL     IF   E           K DISK
     F***  END CHANGE ***  06/22/99  **************************************
     FWDELL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMARL     IF   E           K DISK
     FWMAAL5    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWTABL     IF   E           K DISK    ignore(PRS01)
     FWDABL     UF A E           K DISK
     FWSASL     UF A E           K DISK
     F*** BEGIN ADD ***  01/09/04  ************************************
     FWDVHL1    IF   E           K DISK
     F*** END   ADD ***  01/09/04  ************************************
     D                 DS
     D  EF                     1     35  0 INZ
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D X7STDT                         7  0 inz
     D  X7STDT_MM                     2  0 overlay(X7STDT:4)
     D  X7STDT_DD                     2  0 overlay(X7STDT:6)
     D PSDS          ESDS                  extname(SZQ1P)
     C***  Begin Add  ***  11/05/04  **************************************
     C/COPY wpragent
     C***  End   Add  ***  11/05/04  **************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     D DUE_DATE        s               D   datfmt(*USA)
     D CVT_DT_USA      s               D   datfmt(*USA)
     D SYS_DATE        s               D   datfmt(*USA)
     D SYS_TIME        s               T
     D SYS_TIME_N      s              6  0
     D QQCO#           s              3
     D QQFRDT          s              8
     D QQTODT          s              8
     D QQSTDT          s              6
     D QQAGT#          s              5
     D XXCO#           s                   like(EACO#)
     D X8FRDT          s              8  0
     D X8TODT          s              8  0
     D X6STDT          s              6  0
     D XXAGT#          s                   like(ABAGT#)
     D***  BEGIN ADD  ***  08/31/99  **************************************
     D XXAGT#_2        s                   like(ABAGT#)
     D***  BEGIN ADD  ***  08/31/99  **************************************
     D SVAGT#          s                   like(ABAGT#)
     D X7FRDT          s                   like(ABTRDT)
     D X7TODT          s                   like(ABTRDT)
     D X5STDT          s                   like(ABSTDT)
     D X7DT            s                   like(ABTRDT)
     D X7DATE          s                   like(ABTRDT)
     D XXFND           s                   like(ABFND)
     D XXEMP#          s                   like(ABEMP#)
     D XXDIV           s                   like(ABDIV)
     D XXFYR           s                   like(ABFYR)
     D XXPROD          s                   like(ABPROD)
     D***  BEGIN ADD  ***  08/31/99  **************************************
     D XXPROD_2        s                   like(ABPROD)
     D***  END   ADD  ***  08/31/99  **************************************
     D XXNET           s                   like(ABNET)
     D XXRATE          s                   like(ABRATE)
     D SVRATE          s                   like(ABRATE)
     D***  BEGIN ADD  ***  08/31/99  **************************************
     D SVRATE_2        s                   like(ABRATE)
     D holdrate        s                   like(ABRATE)
     D xxplan          s                   like(ARPLAN)
     D xxplan_2        s                   like(ARPLAN)
     D***  END   ADD  ***  08/31/99  **************************************
     D XXCOMM          s                   like(ABCOMM)
     D XXDUE           s                   like(ABDUE)
     D XXCHAP          s                   like(ABCHAP)
     D XXBILL          s                   like(ASBILL)
     D XXPAID          s                   like(ASPAID)
     D XXRT#           s                   like(AHRT#)
     D***  BEGIN ADD  ***  08/31/99  **************************************
     D XXRT#_2         s                   like(AHRT#)
     D***  END   ADD  ***  08/31/99  **************************************
     D XXKEY           s                   like(BSKEY)
     D XXSEFL          s                   like(BSSEFL)
     D EXC_COMM        s                   like(TTECFL)
     D RENEW_DATE      s                   like(ABTRDT)
     D***  BEGIN ADD  ***  06/22/00  **************************************
     D svFND           s                   like(EAFND)
     D***  END ADD ******  06/22/00  **************************************
     D VALID           s              1
     D***  BEGIN ADD  ***  08/31/99  **************************************
     D VALID_2         s              1
     D agent1or2       s              3  0
     D x               s              3  0
     D***  END   ADD  ***  08/31/99  **************************************
     D P               s              1  0
     D*** BEG ADD ****** 02/18/03  ****************************************
     D vndrTest        s              1
     D*** END ADD ****** 02/18/03  ****************************************
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     C*
     C/COPY SCOPYDTPL
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    X7FRDT
     C*
     C     EAKY1         klist
     C                   kfld                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C                   kfld                    EAFYR
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    ELRTCD
     C*
     C*  AGENT HISTORY MASTER FILE
     C*
     C     AHKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C                   kfld                    EAFYR
     C                   kfld                    X7DT
     C*
     C     AHKY1         klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C                   kfld                    EAFYR
     C*
     C*  KEY LIST FOR THE AGENT RATE FILE
     C*
     C     ARKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C                   kfld                    XXRT#
     C                   kfld                    X7FRDT
     C*
     C     ARKY1         klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C                   kfld                    XXRT#
     C*
     C     ARKYX         klist
     C                   kfld                    ARCO#
     C                   kfld                    ARFND
     C                   kfld                    ARRT#
     C                   kfld                    AREFDT
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    XXAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          klist
     C                   kfld                    AACO#
     C                   kfld                    AAFEIN
     C                   kfld                    AAMOD
     C*
     C*  AGENCY BILLING DETAIL FILE
     C*
     C     ABKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    X5STDT
     C*
     C***  BEGIN ADD  ***  06/22/00  **************************************
     C*  FUND MASTER FILE
     C*
     C     FNKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    EAFND
     C***  END ADD ******  06/22/00  **************************************
     C*
     C     ABKY1         klist
     C                   kfld                    XXCO#
     C                   kfld                    X5STDT
     C                   kfld                    SVAGT#
     C*
     C*  ACCOUNT CURRENT SUMMARY FILE
     C*
     C     ASKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    X5STDT
     C*
     C     ASKY1         klist
     C                   kfld                    XXCO#
     C                   kfld                    X5STDT
     C                   kfld                    ABAGT#
     C*
     C*  TABLE FILES
     C*
     C     TBKY          klist
     C                   kfld                    XXCO#
     C                   kfld                    XXKEY

      *** beg add ***** 01/09/04 ******************************************
     C     vhky          klist
     C                   kfld                    xxco#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C                   kfld                    EAFYR

     C     vhky1         klist
     C                   kfld                    xxco#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C                   kfld                    EAFYR
     C                   kfld                    x7todt
      *** end add ***** 01/09/04 ******************************************
     C*____________________________________________________________________
     C*
     C     *entry        plist
     C                   parm                    QQCO#
     C                   parm                    QQFRDT
     C                   parm                    QQTODT
     C                   parm                    QQSTDT
     C                   parm                    QQAGT#
     C*____________________________________________________________________
     C*
     C                   exsr      DLT_WDABP
     C*
     C***  BEGIN CHG  ***  03/13/00  **************************************
     C*****EAKY*         setll     PEA01
     C*****EAKY1         reade     PEA01                                  60
     C     EAKY1         chain     PEA01                              60
     C***  END   CHG  ***  03/13/00  **************************************
     C*
     C                   dow       *in60 = *off
     C***  BEGIN DEL  ***  03/13/00  **************************************
     C**********                    and EAPODT <= x7todt
     C***  END   DEL  ***  03/13/00  **************************************
     C***  BEGIN ADD  ***  06/22/00  **************************************
     C                   if        svFND <> EAFND
     C     FNKY          chain     PFN01                              67
     C                   if        *in67 = *ON
     C                   eval      FNTEST = 'Y'                                 BYPASS
     C                   endif                                                  VALID=*on
     C                   z-add     EAFND         svFND
     C                   endif                                                  VALID=*on

     C*** BEG ADD ****** 02/18/03  ****************************************
     C                   eval      vndrTest =  'N'
     C*** beg chg ****** 01/09/04  ****************************************
     C*****ELKY*******   chain     PEL01
     C****************   if        %found and ELVDR# <> *zero
     C****************   eval      vndrTest =  'Y'
     C****************   endif
      *
      * Get vendor history record prior to QQTODT, and skip processing if vendor history was
      * active during any part of the installment range (QQFRDT - QQTODT).
      *
     C     vhky1         setgt     pvh01
     C     vhky          readpe    pvh01
     C                   dow       not %eof

     C                   select
     C                   when      vhexdt = 0
     C                   eval      vndrTest = 'Y'
     C                   leave
     C                   when      vhexdt >  x7frdt and
     C                             vhexdt <> vhefdt
     C                   eval      vndrTest = 'Y'
     C                   leave
     C                   endsl

     C     vhky          readpe    pvh01
     C                   enddo
     C*** end chg ****** 01/09/04  ****************************************
     C*** END ADD ****** 02/18/03  ****************************************
     C*
     C*** BEG CHG ****** 02/18/03  ****************************************
     C****************** if        FNTEST <> 'Y'
     C                   if        FNTEST <> 'Y' and vndrTest <> 'Y'
     C*** END CHG ****** 02/18/03  ****************************************
     C*
     C***  END ADD ******  06/22/00  **************************************
     C*
     C                   exsr      CHK_TRANS
     C*
     C***  BEGIN DEL  ***  07/28/00  **************************************
     C********Force dump
     C**********         z-add     0             twix              1 0
     C**********         add       1             twcnt             1 0
     C**********         if        twcnt = 0
     C**********         div       twix          twox              1 0
     C**********         endif                                                  VALID=*on
     C***  END   DEL  ***  07/28/00  **************************************
     C*
     C                   if        VALID = *ON
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   eval      agent1or2 = 1
     C***  END   ADD  ***  08/31/99  **************************************
     C                   exsr      WRT_WDABP
     C                   endif                                                  VALID=*on
     C*
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   if        VALID_2 = *ON
     C                   eval      agent1or2 = 2
     C                   exsr      WRT_WDABP
     C                   endif                                                  VALID_2=*on
     C***  END   ADD  ***  08/31/99  **************************************
     C***  BEGIN ADD  ***  06/22/00  **************************************
     C                   endif                                                  FNTEST <> Y
     C***  END ADD ******  06/22/00  **************************************
     C*
     C     EAKY1         reade     PEA01                                  60
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   exsr      UPDT_WSASP
     C*
     C                   MOVE      *on           *inlr
     C*____________________________________________________________________
     C*
     C*====================================================================
     C* *inzsr - Initialize fields
     C*====================================================================
     C*
     C     *inzsr        BEGSR
     C*
     C                   move      QQCO#         XXCO#
     C                   move      QQFRDT        X8FRDT
     C                   move      QQTODT        X8TODT
     C                   move      QQSTDT        X6STDT
     C*
     C                   if        %parms >= 5
     C                   move      QQAGT#        XXAGT#
     C                   else
     C                   clear                   XXAGT#
     C                   endif                                                  %parms>=5
     C*
     C                   eval      SVAGT# = XXAGT#
     C*
     C                   test(D)                 X8FRDT                 60
     C   60              move      X8FRDT        CVT_DT_USA
     C     *cymd         move      CVT_DT_USA    X7FRDT
     C*
     C                   test(D)                 X8TODT                 60
     C   60              move      X8TODT        CVT_DT_USA
     C     *cymd         move      CVT_DT_USA    X7TODT
     C*
     C                   z-add     X6STDT        SRDTI
     C                   call      'SRCVTMY'     DTCYMD
     C                   movel     SRDTO         X5STDT
     C*
     C                   move      *date         CVT_DT_USA
     C     *cymd         move      CVT_DT_USA    X7DATE
     C*
     C                   move      *date         SYS_DATE
     C                   time                    SYS_TIME_N
     C     *hms          move      SYS_TIME_N    SYS_TIME
     C*
     C                   ENDSR                                                  *inzsr
     C*
     C*====================================================================
     C* CALC_DUE - Calculate the due date
     C*====================================================================
     C*
     C     CALC_DUE      BEGSR
     C*
     C                   movel     X5STDT        X7STDT
     C*
     C                   select
     C                   when      X7STDT_MM = 2
     C                   eval      X7STDT_DD = 28
     C                   when      X7STDT_MM = 4
     C                              or X7STDT_MM = 6
     C                              or X7STDT_MM = 9
     C                              or X7STDT_MM = 11
     C                   eval      X7STDT_DD = 30
     C                   other
     C                   eval      X7STDT_DD = 31
     C                   endsl
     C*
     C     *cymd         move      X7STDT        DUE_DATE
     C*
     C     AAKY          chain     PAA01                              60
     C*
     C                   dow       *in60 = *off
     C                              and AASTAT = 'I'
     C     AAKY          reade     PAA01                                  60
     C                   enddo
     C*
     C  N60AGKY          chain     PAG01                              60
     C*
     C                   if        *in60 = *off
     C                   adddur    AGACDU:*D     DUE_DATE
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR                                                  CALC_DUE
     C*
     C*====================================================================
     C* CALC_COMM - Calculate the comm rate, amount and net due
     C*====================================================================
     C*
     C     CALC_COMM     BEGSR
     C*
     C                   eval      XXCOMM = EAAMT * (SVRATE / 100)
     C                   eval      XXNET  = EAAMT - XXCOMM
     C                   eval      XXRATE = SVRATE
     C*
     C                   ENDSR                                                  CALC_COMM
     C*
     C*====================================================================
     C* CHK_TRANS - Determine if the transaction should be used
     C*====================================================================
     C*
     C     CHK_TRANS     BEGSR
     C*
     C                   eval      VALID = *off
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   eval      VALID_2 = *off
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C                   exsr      GET_BSCD
     C*
     C                   exsr      GET_AR_DT
     C*
     C                   exsr      GET_AGENT
     C*
     C                   exsr      GET_RATE
     C*
     C*  ONLY WRITE A RECORD IF:
     C*    - X7DT IS WITH IN THE SELECTED DATE RANGE
     C*    - THE AGENT HAS BEEN REQUESTED
     C*    - THE PLAN TYPE IS 'A'
     C*
     C                   if        X7DT >=  X7FRDT
     C                              and X7DT <= X7TODT
     C***  BEGIN DEL  ***  08/31/99  **************************************
     C**********                    and ARPLAN = 'A'
     C***  END   DEL  ***  08/31/99  **************************************
     C                              and RENEW_DATE <> *ZEROS
     C*
     C***  BEGIN CHG  ***  08/31/99  **************************************
     C**********         if        SVAGT# = *zeros
     C**********                    or XXAGT# = SVAGT#
     C                   if        (SVAGT# = *zeros or XXAGT# = SVAGT#)
     C                              and XXPLAN = 'A'
     C***  END   CHG  ***  08/31/99  **************************************
     C                   eval      VALID = *on
     C                   endif                                                  SVAGT#=*zeros
     C*
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   if        (SVAGT# = *zeros or XXAGT#_2 = SVAGT#)
     C                              and XXPLAN_2 = 'A'
     C                              and XXAGT#_2 <> *zeros
     C                   eval      VALID_2 = *on
     C                   endif                                                  SVAGT#=*zeros
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C                   endif                                                  XXTODT>=X7DT>=XXFRDT
     C*
     C                   ENDSR                                                  CALC_DUE
     C*
     C*====================================================================
     C* DLT_WDABP - Delete records in WDABP
     C*====================================================================
     C*
     C     DLT_WDABP     BEGSR
     C*
     C                   select
     C                   when      SVAGT# <> *ZEROS
     C     ABKY1         chain     PAB01                              60
     C                   other
     C     ABKY          chain     PAB01                              60
     C                   endsl
     C*
     C                   dow       *in60 = *off
     C*
     C***  Begin Add  ***  08/08/06  **************************************
     C*******************if        ABMADJ <> 'Y'
     C*** beg chg ***** 06/18/08 ******************************************
     C***********        if        ABMADJ <> 'Y' and ABARTY <> 'R'
     C***  Begin Chg  ***  04/30/15  **************************************
     C**********         if        ABMADJ <> 'Y' and ABARTY <> 'R' and
     C**********                   ABARTY <> 'T'
     C                   if        ABmAdj <> 'Y' and ABiPgm = q1Pgm
     C***  End   Chg  ***  04/30/15  **************************************
     C*** end chg ***** 06/18/08 ******************************************
     C***  End Add ******  08/08/06  **************************************
     C                   delete    PAB01
     C                   endif                                                  ABMADJ<>Y
     C*
     C                   select
     C                   when      SVAGT# <> *ZEROS
     C     ABKY1         reade     PAB01                                  60
     C                   other
     C     ABKY          reade     PAB01                                  60
     C                   endsl
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   ENDSR                                                  DLT_WDABP
     C*
     C*====================================================================
     C* GET_AGENT - Get the active agent as of the 'TO' date
     C*====================================================================
     C*
     C     GET_AGENT     BEGSR
     C*
     C                   clear                   XXAGT#
     C                   clear                   XXPROD
     C                   clear                   XXRT#
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   clear                   XXAGT#_2
     C                   clear                   XXPROD_2
     C                   clear                   XXRT#_2
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C     AHKY          setgt     PAH01
     C     AHKY1         readpe    PAH01                                  60
     C*
     C                   dow       *in60 = *off
     C                               and AHSTAT = 'I'
     C     AHKY1         readpe    PAH01                                  60
     C                   enddo                                                  *in60=*off
     C*
     C                   if        *in60 = *off
     C                   eval      XXAGT# = AHAGT#
     C                   eval      XXPROD = AHPROD
     C                   eval      XXRT#  = AHRT#
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   eval      XXAGT#_2 = AHSAGT
     C                   eval      XXPROD_2 = AHSPRD
     C                   eval      XXRT#_2  = AHSRT#
     C***  END   ADD  ***  08/31/99  **************************************
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR                                                  DLT_WDABP
     C*
     C*====================================================================
     C* GET_AR_DT - Get the date used for comparisons
     C*====================================================================
     C*
     C     GET_AR_DT     BEGSR
     C*
     C                   if        XXSEFL = 'S'
     C*
     C*  USE TRANSACTION DATE FOR SELF REPORTING ACCOUNTS, EXCEPT
     C*  IF RECORD IS A DEPOSIT (H) BECAUSE THEY MAY PUT THE DEPOSIT
     C*  ON A PAYMENT PLAN.   ALSO, USE PODT IF RECORD IS AN
     C*  ADJUSTMENT (A) BECAUSE THIS IS USED TO PUT AN AUDIT ON A
     C*  PAYMENT PLAN.  - ALL OTHER ADJUSTMENT WILL SHOW CORRECTLY
     C*  BECAUSE TRDT ALWAYS EQUALS PODT.
     C*
     C                   select
     C*____________________________________________________________________
     C*
     C                   when      EAARTY = 'A'
     C*
     C                   eval      X7DT = EAPODT
     C*____________________________________________________________________
     C*
     C                   when      EAPODT > EATRDT
     C                             and EAARTY <> 'P'
     C                             and EAARTY <> 'I'
     C                             and EAARTY <> 'Q'
     C                             and EAARTY <> 'R'
     C                             and EAARTY <> 'N'
     C***  Begin Add  ***  04/18/06  **************************************
     C                             and EAARTY <> 'T'
     C***  End Add ******  04/18/06  **************************************
     C                   eval      X7DT = EAPODT
     C*____________________________________________________________________
     C*
     C                   other
     C*
     C                   eval      X7DT = EATRDT
     C*____________________________________________________________________
     C*
     C                   endsl
     C*
     C                   else
     C*
     C*  USE POSTING DATE FOR ESTIMATING ACCOUNTS, EXCEPT WHEN THE
     C*  TRANSACTION DATE IS GREATER THAN POSTING DATE.  THIS WILL
     C*  OCCUR WHEN A SELF REPORTING ACCT CHANGES TO AN ESTIMATING
     C*  ACCT & HAD A BILL POSTED FOR THE PRIOR MONTH AS A S/R ACCT.
     C*  THIS CAN ALSO OCCUR IF THE KEY A LATE PAYMENT WITH PODT
     C*  FOR A PRIOR DATE.
     C*
     C                   select
     C*____________________________________________________________________
     C*
     C                   when      EATRDT > EAPODT
     C                             or EAARTY = 'P'
     C                             or EAARTY = 'I'
     C                             or EAARTY = 'Q'
     C                             or EAARTY = 'R'
     C                             or EAARTY = 'N'
     C***  Begin Add  ***  04/18/06  **************************************
     C                             or EAARTY = 'T'
     C***  End Add ******  04/18/06  **************************************
     C*
     C                   eval      X7DT = EATRDT
     C*____________________________________________________________________
     C*
     C                   other
     C*
     C                   eval      X7DT = EAPODT
     C*____________________________________________________________________
     C*
     C                   endsl
     C*
     C                   endif                                                  XXSEFL=S
     C*
     C                   ENDSR                                                  GET_AR_DT
     C*
     C*====================================================================
     C* GET_BSCD - Get the Billing Schedule Code
     C*====================================================================
     C*
     C     GET_BSCD      BEGSR
     C*
     C                   eval      XXEMP# = EAEMP#
     C                   eval      XXDIV  = EADIV
     C                   eval      XXSEFL = *BLANK
     C                   eval      RSJRST = *BLANK
     C*
     C     ELKY          chain     PEL01                              60
     C*
     C                   if        *in60 = *off
     C*
     C     RSKY          chain     PRS01                              60
     C*
     C                   movel(P)  ELBSCD        XXKEY
     C*
     C     TBKY          CHAIN     PBS01                              60
     C*
     C                   if        *in60 = *off
     C                   eval      XXSEFL = BSSEFL
     C                   else
     C                   eval      XXSEFL = 'E'
     C                   endif                                                  *in60=*off
     C*
     C                   else
     C*
     C                   eval      XXSEFL = 'E'
     C*
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR                                                  GET_AGENT
     C*
     C*====================================================================
     C* GET_CMM_FL - Determine if the record should be paid comm
     C*====================================================================
     C*
     C     GET_CMM_FL    BEGSR
     C*
     C                   clear                   EXC_COMM
     C                   clear                   XXCOMM
     C                   clear                   XXNET
     C                   clear                   XXRATE
     C*
     C                   movel(P)  EAARCD        XXKEY
     C*
     C     TBKY          chain     PTT01                              60
     C*
     C                   if        *in60 = *off
     C                   eval      EXC_COMM = TTECFL
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR                                                  GET_COMM
     C*
     C*====================================================================
     C* GET_RATE - Get the rate based on the rate # and new or renew
     C*====================================================================
     C*
     C     GET_RATE      BEGSR
     C*
     C                   exsr      GET_RNW_DT
     C*
     C***  BEGIN CHG  ***  08/31/99  **************************************
     C**********         clear                   ARPLAN
     C                   clear                   xxplan
     C                   clear                   xxplan_2
     C***  END   CHG  ***  08/31/99  **************************************
     C                   clear                   SVRATE
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   clear                   SVRATE_2
     C*
     C                   do        2             x
     C*
     C                   select
     C                   when      x = 2
     C                   eval      xxrt# = xxrt#_2
     C                   eval      holdrate = svrate
     C                   clear                   SVRATE
     C                   endsl
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C     ARKY          setgt     PAR01
     C     ARKY1         readpe    PAR01                                  60
     C*
     C***  BEGIN ADD  ***  08/10/00  **************************************
     C                   dow       not *in60 and ARSTAT = 'I'
     C     ARKY1         readpe    PAR01                                  60
     C                   enddo
     C***  END   ADD  ***  08/10/00  **************************************
     C*
     C                   if        *in60 = *off
     C*
     C                   select
     C                   when      RENEW_DATE >= ELPTDT
     C                   eval      SVRATE = ARNWRT                              New Business
     C                   when      RENEW_DATE <= ELPFDT
     C                   eval      SVRATE = ARRNRT                              New Business
     C                   when      EATRDT >= RENEW_DATE
     C                   eval      SVRATE = ARRNRT                              New Business
     C*
     C***  BEGIN ADD  ***  04/02/01  **************************************
     C                   other
     C                   eval      SVRATE = ARNWRT
     C***  END   ADD  ***  04/02/01  **************************************
     C                   endsl
     C*
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   else
     C*
     C                   clear                   ARPLAN
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C                   endif                                                  *in60=*off
     C*
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   select
     C                   when      x = 2
     C                   eval      svrate_2 = svrate
     C                   eval      svrate = holdrate
     C                   eval      xxplan_2 = ARPLAN
     C                   other
     C                   eval      xxplan = ARPLAN
     C                   endsl
     C*
     C                   enddo
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C                   ENDSR                                                  GET_RATE
     C*
     C*====================================================================
     C* GET_RNW_DT - Get the date used for determining renewals
     C*====================================================================
     C*
     C     GET_RNW_DT    BEGSR
     C*
     C                   clear                   RENEW_DATE
     C*
     C     EMKY          chain     PEM01                              60
     C*
     C                   if        *IN60 = *off
     C*
     C*  GET EMPLOYER'S MOST CURRENT EFFECTIVE DATE
     C*
     C     1             do        5             P
     C*
     C                   if        EF(P) <> *zeros
     C                   eval      RENEW_DATE = EF(P)
     C                   endif                                                  EF(P)<>*zeros
     C*
     C                   enddo                                                  P->5
     C*
     C                   if        RENEW_DATE <> *zeros
     C     *cymd         move      RENEW_DATE    CVT_DT_USA
     C                   adddur    1:*Y          CVT_DT_USA
     C     *cymd         move      CVT_DT_USA    RENEW_DATE
     C                   endif                                                  RENEW_DATE<>*zeros
     C*
     C                   endif                                                  *in60=*off
     C*
     C                   ENDSR                                                  GET_RNW_DT
     C*
     C*====================================================================
     C* UPDT_WSASP - Update records in WSASP
     C*====================================================================
     C*
     C     UPDT_WSASP    BEGSR
     C*
     C     ASKY          chain     PAS01                              60
     C*
     C                   dow       *in60 = *off
     C*
     C                   eval      xxagt# = ASAGT#
     C*
     C                   exsr      CALC_DUE
     C*
     C     *cymd         move      DUE_DATE      ASDUE
     C*
     C                   clear                   XXBILL
     C                   clear                   XXPAID
     C                   eval      SVAGT# = ASAGT#
     C*
     C     ABKY1         chain(N)  PAB01                              60
     C*
     C                   dow       *in60 = *off
     C*
     C                   select
     C                   when      ABARTY = 'A'
     C                              or ABARTY = 'B'
     C                              or ABARTY = 'D'
     C                              or ABARTY = 'F'
     C***  BEGIN CHG  ***  05/18/00  **************************************
     C**********                    or ABARTY = 'G'
     C                              or ABARTY = 'H'
     C***  END   CHG  ***  05/18/00  **************************************
     C***  BEGIN DEL  12/15/99  *******************************************
     C**********                    or ABARTY = 'R'
     C**********                    or ABARTY = 'V'
     C***  END   DEL  12/15/99  *******************************************
     C                              or ABARTY = 'N'
     C                   add       ABNET         XXBILL
     C                   when      ABARTY = 'P'
     C***  BEGIN ADD  ***  05/18/00  **************************************
     C                              or ABARTY = 'I'
     C                              or ABARTY = 'Q'
     C***  Begin Add  ***  04/18/06  **************************************
     C                              or ABARTY = 'T'
     C***  End Add ******  04/18/06  **************************************
     C***  END   ADD  ***  05/18/00  **************************************
     C                   add       ABAMT         XXPAID
     C***  BEGIN ADD  12/15/99  *******************************************
     C                   when      ABARTY = 'R'
     C***  Begin Del  ***  04/18/06  **************************************
     C******************************or ABARTY = 'V'
     C***  End Del ******  04/18/06  **************************************
     C                   sub       ABAMT         XXPAID
     C***  END   ADD  12/15/99  *******************************************
     C                   endsl
     C*
     C     ABKY1         reade(N)  PAB01                                  60
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      ASBILL = XXBILL
     C                   eval      ASPAID = XXPAID
     C                   eval      ASBAL  = ASBILL - ASPAID
     C*
     C                   adddur    1:*S          SYS_TIME
     C*
     C                   eval      ASPGM  = Q1PGM
     C                   eval      ASUSER = Q1USER
     C     *cymd         move      SYS_DATE      ASDT
     C                   move      SYS_TIME      ASTIME
     C*
     C                   update    PAS01
     C*
     C     ASKY          reade     PAS01                                  60
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   ENDSR                                                  UPDT_WSASP
     C*
     C*====================================================================
     C* WRT_WDABP - Write records to WDABP
     C*====================================================================
     C*
     C     WRT_WDABP     BEGSR
     C*
     C***  BEGIN ADD  ***  08/31/99  **************************************
     C                   select
     C                   when      agent1or2 = 2
     C                   eval      xxagt# = xxagt#_2
     C                   eval      xxprod = xxprod_2
     C                   eval      svrate = svrate_2
     C                   endsl
     C***  END   ADD  ***  08/31/99  **************************************
     C*
     C                   exsr      CALC_DUE
     C*
     C                   exsr      GET_CMM_FL
     C*
     C***  Begin Add  ***  08/22/06  **************************************
     C*******************if********EXC_COMM <> 'Y'
     C                   if        EXC_COMM <> 'Y' and EAARTY <> 'R'
     C                                   and EAARTY <> 'T'
     C***  End Add ******  08/22/06  **************************************
     C                   exsr      CALC_COMM
     C***  BEGIN ADD  ***  01/21/00  **************************************
     C                   else
     C                   eval      xxnet = EAAMT
     C***  END   ADD  ***  01/21/00  **************************************
     C                   endif                                                  EXC_COMM<>Y
     C*
     C                   clear                   PAB01
     C*
     C                   eval      ABCO#  = XXCO#
     C                   eval      ABAGT# = XXAGT#
     C                   eval      ABPROD = XXPROD
     C                   eval      ABSTDT = X5STDT
     C     *cymd         move      DUE_DATE      ABDUE
     C                   eval      ABFND  = EAFND
     C                   eval      ABEMP# = EAEMP#
     C                   eval      ABDIV  = EADIV
     C                   eval      ABFYR  = EAFYR
     C                   eval      ABARTY = EAARTY
     C                   eval      ABARCD = EAARCD
     C                   eval      ABDESC = EADESC
     C                   eval      ABPODT = EAPODT
     C                   eval      ABTRDT = EATRDT
     C                   eval      ABCHK# = EACHK#
     C                   eval      ABAMT  = EAAMT
     C***  Begin Add  ***  11/05/04  **************************************
     C                   eval      ABFEIN = GetAgtFein(ABCO#:ABFND:ABAGT#:
     C                                                 ABMOD)
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C***  BEGIN ADD  ***  05/17/00  *************************************
     C                   if        TTTYPE <> 'P' and
     C                             TTTYPE <> 'I' and
     C                             TTTYPE <> 'Q' and
     C***  Begin Add  ***  04/18/06  **************************************
     C                             TTTYPE <> 'T'
     C***  End Add ******  04/18/06  **************************************
     C***  END   ADD  ***  05/17/00  *************************************
     C                   eval      ABRATE = XXRATE
     C                   eval      ABCOMM = XXCOMM
     C                   eval      ABNET  = XXNET
     C***  BEGIN ADD  ***  05/17/00  *************************************
     C                   endif
     C***  END   ADD  ***  05/17/00  *************************************
     C*
     C                   eval      ABRST  = RSJRST
     C                   eval      ABCHAP = EMCHAP
     C*
     C                   adddur    1:*S          SYS_TIME
     C*
     C                   eval      ABPGM  = Q1PGM
     C                   eval      ABIPGM = Q1PGM
     C                   eval      ABUSER = Q1USER
     C                   eval      ABIUSR = Q1USER
     C     *cymd         move      SYS_DATE      ABDT
     C     *cymd         move      SYS_DATE      ABIDT
     C                   move      SYS_TIME      ABTIME
     C                   move      SYS_TIME      ABITIM
     C*
     C                   write     PAB01
     C*
     C                   exsr      WRT_WSASP
     C*
     C                   ENDSR                                                  WRT_WDABP
     C*
     C*====================================================================
     C* WRT_WSASP - Write records to WSASP, if they don't already exist
     C*====================================================================
     C*
     C     WRT_WSASP     BEGSR
     C*
     C     ASKY1         CHAIN(N)  PAS01                              60
     C*
     C                   if        *in60 = *on
     C*
     C                   clear                   PAS01
     C*
     C                   eval      ASCO#  = XXCO#
     C                   eval      ASAGT# = XXAGT#
     C                   eval      ASSTDT = X5STDT
     C*
     C                   adddur    1:*S          SYS_TIME
     C*
     C                   eval      ASPGM  = Q1PGM
     C                   eval      ASIPGM = Q1PGM
     C                   eval      ASUSER = Q1USER
     C                   eval      ASIUSR = Q1USER
     C     *cymd         move      SYS_DATE      ASDT
     C     *cymd         move      SYS_DATE      ASIDT
     C                   move      SYS_TIME      ASTIME
     C                   move      SYS_TIME      ASITIM
     C*
     C                   write     PAS01
     C*
     C                   endif                                                  *in60=*on
     C*
     C                   ENDSR                                                  WRT_WSASP
     C*
