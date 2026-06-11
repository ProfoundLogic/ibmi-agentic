     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8914R - WORK FILE BUILD - A/R FILE - WHARP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - A/R FILE                                   *
     H*                                                               *
     H* CREATED: 02/06/01  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/04/11  C:191  Project: WC1856 PRISS #10                    *
     H*                  Make processing backward compatible          *
     H*                                                               *
     H* 11/25/10  C:220  Project: WC1856                              *
     H*                  Implement WA726R API to retrieve policy      *
     H*                  start policy year of the group.              *
     H*                                                               *
     H* 07/25/06 C: 132   CI031                                       *
     H*           - Remove the code to insert Carrier Policy number   *
     H*             in WHARP/ARLONR field, use Grp-Emp#-Fyr as Policy *
     H*             number.                                           *
     H*                                                               *
     H*  01/03/06 C: 132 PROJECT: CI129 - 50603                       *
     H*             Change to create correct Policy number field      *
     H*                                                               *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 01/05/05  C:132 PROJECT: CI025                                *
     H*              - Change to include all billing and payment      *
     H*                transactions in the future, instead of using   *
     H*                input parm date as cutoff date. (Like 91DKC)   *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHARP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 10/09/03 C:132 PROJECT:  CI010                                *
     H*                - USE PARM DATE FOR ASOF DATE                  *
     H*                                                               *
     H* 04/03/03  C:132 PROJECT: WC924  SEQ#: 36720                   *
     H*              - Change to PUT EAARTY (Trans Type) of "N" into  *
     H*                Billing coding instead of Payment coding       *
     H*                                                               *
     H* 05/28/02  C:132 PROJECT: WC924                                *
     H*              - Change to use EATRDT (Trans date) in compare   *
     H*                to bypass processing if EAARTY is a Payment    *
     H*                type code.                                     *
     H*              - Use EAPODT (Posting date) in compare to bypass *
     H*                processing if EAARTY is a Billing type code    *
     H*                                                               *
     H*                                                               *
     H* 05/08/02  C:132 PROJECT: CI091 SEQ: 32340                     *
     H*               - Change position for default of Is A Direct    *
     H*                 Bill Policy.                                  *
     H*                                                               *
     H* 12/04/01  C:132 PROJECT WC924                                 *
     H*               - ADD AR TYPE FOR NSF CHECKS TO PAY TYPE CODE   *
     H*                                                               *
     H* 11/29/01  C:003 PROJECT WC924                                 *
     H*               - CORRECT AMT FOR REFUNDS SINCE USING WMEAP     *
     H*                 RATHER THAN WFARP
     H*                                                               *
     H* 11/13/01  C:132 PROJECT WC924                                 *
     H*               - Change: If WMEAP/EAPODT is Zero, use EATRDT   *
     H*                                                               *
     H* 10/12/01  C:132 PROJECT WC924                                 *
     H*               - Change: If WMEAP/EAPODT is a Greater than     *
     H*                 Input Parm Date, bypass it                    *
     H*                                                               *
     H* 10/09/01  C:132 PROJECT WC924                                 *
     H*               - Change: If WMEAP/EAARTY is a Payment type     *
     H*                 make it negative by MUlT by a -1              *
     H*                 FIX ARTRDT & ARPODT IN WHARP                  *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H*  09/24/01 C:132  Project:  WC924                              *
     H*           Add WMEAL for Input instead of WFARP file. Input    *
     H*           Fields also changed to use WMEAl fields             *
     H*
     H*     08/03/01  C:003  CHG MULT ON "REFUND"
     H*                      CLEAR UNIQUE KEY FIELD (A2KEY)
     H*
     H*     07/11/01  C:132  Add ARARCD & ARARTY to Unique Key
     H*               C:003  LOADED DEFAULT VALUE FOR DIRECT BILL
     H*
     H*     04/05/01  C:58   Add input ocde D = bill.           %
     H*
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      **** BEGIN CHG **** 09/24/01 *************************************
      * A/R Detail Report Workfile
     F****WFARP   IF  E           K        DISK
      *
      * Employer A/R File by CO#, FND, EMP#
     FWMEAL     IF   E           K DISK
      **** END CHG ****** 09/24/01 *************************************
      *
      * Agent History
     FWMAHL     IF   E           K DISK
      *
      * Agent Rate
     FWMARL     IF   E           K DISK
     F                                     RENAME(PAR01:PAR03)
      *
      * FUND
     FWMFNL     IF   E           K DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
      * Exclude Employers file
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     F**** Begin Add ****  07/22/05  **************************************
     F*******   Employer Carrier File
     F*******WMRPL   IF  E           K        DISK
     F**** End Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
      *
      * Output - Data Warehouse - WHARP
     FWHARP     IF A E             DISK
     F                                     RENAME(PAR01:PAR02)
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1YY                   5      8
      *
      **** BEGIN ADD **** 09/24/01 *************************************
     D                 DS
     D  XXKEY                  1     36
     D  EACO#                  1      3  0
     D  EAFND                  4      6  0
     D  EAEMP#                 7     15  0
     D  EADIV                 16     20  0
     D  EAFYR                 21     23  0
     D  EATRDT                24     30  0
     D  EATIME                31     36  0
      **** END ADD ****** 09/24/01 *************************************
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPAR02
     I              ARPOL#                      A2POL#
     I              ARDIV                       A2DIV
     I              ARTRTP                      A2TRTP
     I              ARTRDS                      A2TRDS
     I              ARAMT                       A2AMT
     I              ARTRDT                      A2TRDT
     I              ARCHK#                      A2CHK#
     I              ARNINV                      A2NINV
     I              ARFND                       A2FND
     I              ARDBIL                      A2DBIL
     I              ARPODT                      A2PODT
     I              ARKEY                       A2KEY
     I              ARAODT                      A2AODT
      *
     IPAR03
     I              ARCO#                       A3CO#
     I              ARFND                       A3FND
     I              ARRT#                       A3RT#
     I              AREFDT                      A3EFDT
     I              ARNWRT                      A3NWRT
     I              ARRNRT                      A3RNRT
     I              ARSTAT                      A3STAT
     I              ARUSER                      A3USER
     I              ARPGM                       A3PGM
     I              ARTIME                      A3TIME
     I              ARDT                        A3DT
     I              ARDESC                      A3DESC
     I              ARSBRT                      A3SBRT
     I              ARDDP                       A3DDP
     I              ARPLAN                      A3PLAN
     I              ARDBPP                      A3DBPP
      *
      ************************************************************
      *
      *  KLIST
      *

     C**** BEGIN CHG **** 09/24/01 *************************************
      *  Change all KFLDs ARxxxx to EAxxxx Fields
      *
      *  Fund
      *
     C     FNKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
      *
      *  Agent History
      *
     C     AHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C                   KFLD                    XXRNDT
      *
     C     AHKY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
      *
      *  Agent Rate
      *
     C     A3KY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    AHRT#
     C                   KFLD                    XXRNDT
      *
     C     A3KY1         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    AHRT#
     C**** END CHG ****** 09/24/01 *************************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C**** Begin Add ****  07/22/05  **************************************
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C********** RPKEY     KLIST
     C**********           KFLD           EACO#
     C**********           KFLD           EAFND
     C**********           KFLD           EAEMP#
     C**********           KFLD           EADIV
     C**********           KFLD           EAFYR
      ***** END DEL ***** 07/25/06 ****************************************
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
      *
      ************************************************************
      *
      *   *LIKE defines
      *
      *
      ************************************************************
      *
      *  PLIST definitions
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RNDT            8
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
      *
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
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DT1           PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DT2           PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      ************************************************************
      *  Begin
      ************************************************************
      *
      *  INITIALIZE
      *
     C                   TIME                    DSDTTM
      *
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Check if WA726R exists in environment
     C                   MOVEL(P)  'WA726R'      XXPGM
     C                   MOVEL     '*PGM'        XXTYP             7
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM            10
     C                   PARM                    XXTYP
     C                   PARM                    XXOLDF            1
     C* If XXOLDF = '1' then use old process
     C* If XXOLDF = '0' then use new API WA726R
     C*** END   ADD ****** 01/04/11 ************************************
     C***** BEG ADD ******* 11/25/10 **********************************
     C                   MOVEL     @@STYR        @@YRS             3
     C***** END ADD ******* 11/25/10 **********************************
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT            7 0
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
     C                   ENDIF                                                  SRERR
      *
      *  READ WORK FILE
      *
     C**** BEGIN CHG **** 09/24/01 *************************************
     C********             READ PAR01                    60
     C                   READ      PEA01                                  60
     C**** END CHG ****** 09/24/01 *************************************
     C     *IN60         DOWEQ     '0'
      *
     C***** BEG ADD ******* 11/25/10 **********************************
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Process using API only if exists in environment. Else assume old process
     C     XXOLDF        IFEQ      '0'
     C*** END   ADD ****** 01/04/11 ************************************
     C*
     C* Retrieve start policy year based on extarct start date.
     C*
     C                   MOVEL     *ZEROS        @@STYR
     C                   MOVEL     *BLANKS       XXSTYR
     C                   MOVEL     EACO#         X1CO#             3
     C                   MOVEL     EAFND         X1FND             3
     C*
     C                   CALL      'WA726R'
     C                   PARM                    X1CO#
     C                   PARM                    X1FND
     C                   PARM                    @@YRS
     C                   PARM                    XXSTYR            3
     C                   PARM                    @@RTCD            1
     C*
     C     @@RTCD        IFEQ      'Y'
     C*
     C                   MOVEL     XXSTYR        @@STYR            3 0
     C                   ELSE
     C*
     C                   MOVEL     000           @@STYR
     C                   ENDIF
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C                   ENDIF                                                  XXOLDF='0'
     C*** END   ADD ****** 01/04/11 ************************************
     C***** END ADD ******* 11/25/10 **********************************
     C*
      *   Selection: bypass all test groups, Excluded Groups
      *
     C**** BEGIN CHG **** 09/24/01 *************************************
     C*******    ARCO#     IFEQ *ZERO
     C     EACO#         IFEQ      *ZERO
     C***** BEG ADD ******* 03/10/04 **********************************
     C     EAFYR         ORLT      @@STYR
     C***** END ADD ******* 03/10/04 **********************************
     C**** END CHG ****** 09/24/01 *************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
     C     FNKY          CHAIN     PFN01                              60
      *
     C     FNTEST        IFEQ      'Y'
     C     *IN60         OREQ      *ON
     C**** BEGIN CHG **** 10/02/01 *************************************
     C     FNCICF        ORNE      'Y'
     C**** END CHG ****** 10/02/01 *************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *OFF
     C                   GOTO      NEXT
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
      *
      ***** BEG DEL ***** 01/05/05 *************************************
      * DELETE TO ALLOW FUTURE A/R DETAIL TO BE EXTRACTED
     C**** BEGIN ADD **** 10/12/01 *************************************
      ********** If Posting date is > Input run date and is a Billing type, then
      ********** bypass the record.
     C********** EAPODT    IFGT XXRNDT
     C**** BEGIN CHG **** 05/28/02 *************************************
     C********** EAARTY    IFEQ 'B'
     C********** EAARTY    OREQ 'A'
     C********** EAARTY    OREQ 'D'
     C********** EAARTY    OREQ 'F'
     C********** EAARTY    OREQ 'H'
     C**** BEGIN ADD **** 04/03/03 *************************************
     C********** EAARTY    OREQ 'N'
     C**** END ADD ****** 04/03/03 *************************************
     C**********           GOTO NEXT
     C**********           ENDIF
     C*************        GOTO NEXT
     C**** END CHG ****** 05/28/02 *************************************
     C**********           ENDIF
     C**** END ADD ****** 10/12/01 *************************************
      ***** END DEL ***** 01/05/05 *************************************
      *
      ***** BEG DEL ***** 01/05/05 *************************************
     C**** BEGIN ADD **** 05/28/02 *************************************
      ********* If Transaction date is > Input run date and is a Payment type,
      ********* then bypass the record.
     C*********  EATRDT    IFGT XXRNDT
     C*********  EAARTY    IFEQ 'P'
     C*********  EAARTY    OREQ 'Q'
     C*********  EAARTY    OREQ 'I'
     C**** BEGIN DEL **** 04/03/03 *************************************
     C*******    EAARTY    OREQ 'N'
     C**** END DEL ****** 04/03/03 *************************************
     C*********  EAARTY    OREQ 'R'
     C*********            GOTO NEXT
     C*********            ENDIF
     C*********            ENDIF
     C**** END CHG ****** 05/28/02 *************************************
      ***** END DEL ***** 01/05/05 *************************************
      *
      * WRITE TO OUTPUT
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C**** BEGIN CHG **** 09/24/01 *************************************
     C*******              READ PAR01                    60
     C                   READ      PEA01                                  60
     C**** END CHG ****** 09/24/01 *************************************
     C                   ENDDO
      *
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * SROUT - Write record to output file
      *================================================================
     C     SROUT         BEGSR
      *
     C  N60
     CAN 60              READ      PAR02                                  60
     C                   CLEAR                   PAR02
      *
      *  Policy Number
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C*****Begin Add **** 07/22/05 ****************************************
     C********** CLTID     IFEQ '135'
     C********** RPKEY     CHAINPRP01                62
     C********** *IN62     IFEQ *OFF
     C********** RPCAP#    ANDNE*BLANK
     C**********           MOVELRPCAP#    A2POL#    P
     C**********           ELSE
     C*****End*Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
      *
     C**** BEGIN CHG **** 09/24/01 *************************************
     C*********            MOVELARFND     A2POL#    P
     C*********            MOVELAREMP#    XXTMP   9 P
     C*********            CAT  XXTMP:0   A2POL#    P
     C*********            MOVELARFYR     XXTMP   9 P
     C                   MOVEL(P)  EAFND         A2POL#
     C                   MOVEL(P)  EAEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       A2POL#
     C                   MOVEL(P)  EAFYR         XXTMP             9
     C**** END CHG ****** 09/24/01 *************************************
     C                   CAT(P)    XXTMP:0       A2POL#
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C*****Begin Add ****  07/22/05  **************************************
     C**********           ENDIF                           e 62 OFF...&
      ******BEG*ADD ***** 01/03/06 ***************************************
     C**********           ELSE
     C**********           MOVELEAFND     A2POL#    P
     C**********           MOVELEAEMP#    XXTMP   9 P
     C**********           CAT  XXTMP:0   A2POL#    P
     C**********           MOVELEAFYR     XXTMP   9 P
     C**********           CAT  XXTMP:0   A2POL#    P
      ******END*ADD ***** 01/03/06 ***************************************
     C**********           ENDIF                           e CLTID=135
     C*****End*Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
      *
     C**** BEGIN CHG **** 09/24/01 *************************************
      * Changed all the Input fields from ARxxxx to EAxxxx
     C                   Z-ADD     EADIV         A2DIV
      *
     C                   Z-ADD     EAAMT         A2AMT
      *
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C**** BEGIN CHG **** 04/03/03 *************************************
     C     EAARTY        OREQ      'N'
     C**** END CHG ****** 04/03/03 *************************************
     C                   MOVE      'I'           A2TRTP
     C                   ENDIF
      *
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     C     EAARTY        OREQ      'I'
     C***  BEGIN ADD  ***  02/19/04  *********************************
     C     EAARTY        OREQ      'T'
     C***  END   ADD  ***  02/19/04  *********************************
     C*** BGN ADD 12/04/01 *******************************************
     C**** BEGIN DEL **** 04/03/03 *************************************
     C*********  EAARTY    OREQ 'N'                         NSF CHECKS
     C**** END DEL ****** 04/03/03 *************************************
     C*** END ADD 12/04/01 *******************************************
     C                   MOVE      'C'           A2TRTP
     C                   ENDIF
      *
     C     EAARTY        IFEQ      'R'                                          REFUNDS
     C                   MOVE      'C'           A2TRTP
     C************** 08/03/01 BGN CHG ********************************
     C******************** MULT -1        A2AMT
     C************** 08/03/01 END CHG ********************************
     C                   ENDIF
      *
     C**** BEGIN CHG *** 10/09/01 ************************************
      * IF IS A PAYMENT TYPE, MAKE IT NEGATIVE
     C     A2TRTP        IFEQ      'C'
     C                   MULT      -1            A2AMT
     C                   ENDIF
     C*** BGN ADD 11/29/01 *******************************************
     C     EAARTY        IFEQ      'R'
     C                   MULT      -1            A2AMT
     C                   ENDIF
     C*** END ADD 11/29/01 *******************************************
     C**** END CHG ***** 10/09/01 ************************************
      *
     C                   MOVEL     EADESC        A2TRDS
      *
     C**** BEGIN CHG *** 10/09/01 ************************************
     C*******              MOVELEATRDT    SRDTI
     C*******              CALL 'SRCVTDAT'DT1
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C**** END CHG ***** 10/09/01 ************************************
     C                   MOVEL     SRDTO         XXDATE            8 0
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2TRDT
      *
     C                   MOVEL     EACHK#        A2CHK#
     C                   MOVEL     EANINV        A2NINV
     C                   Z-ADD     EAFND         A2FND
      *
     C**** BEGIN CHG *** 10/09/01 ************************************
     C*******              MOVELEAPODT    SRDTI
     C*******              CALL 'SRCVTDAT'DT1
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C**** END CHG ***** 10/09/01 ************************************
     C                   MOVEL     SRDTO         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2PODT
     C**** BEGIN CHG *** 11/13/01 ************************************
     C     A2PODT        IFLE      *BLANKS
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2PODT
     C                   ENDIF
     C**** END CHG ***** 11/13/01 ************************************
      *
     C**** BEGIN CHG *** 10/09/01 ************************************
     C*******              MOVELDSDATE    SRDTI
     C*******              CALL 'SRCVTDAT'DT2
     C*******              MOVELSRDTO     XXDATE
      ***** BEG CHG **** 10/09/03 ********************************
      * Change the Parm Date into the AS OF Date for output record
     C                   MOVEL     @@RNDT        ZZRNDT            8 0
     C                   MOVEL     ZZRNDT        XXDATE
     C**************       MOVELDSDATE    XXDATE
      ***** END CHG **** 10/09/03 ********************************
      *
     C**** END CHG ***** 10/09/01 ************************************
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        A2AODT
      *
     C                   MOVEL(P)  XXKEY         A2KEY
     C**** BEGIN ADD **** 07/11/01 *************************************
     C                   CAT(P)    EAARCD:0      A2KEY
     C                   CAT(P)    EAARTY:0      A2KEY
     C**** END   ADD **** 07/11/01 *************************************
      *
      * End of Input Field Changes from ARxxxx to EAxxxx
     C**** END CHG ****** 09/24/01 *************************************
      *
     C************** 08/03/01 BGN CHG ********************************
     C                   MOVE      *BLANKS       A2KEY
     C************** 08/03/01 END CHG ********************************
      *
      * IS DIRECT BILL POLICY ?
      *
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY1         READPE    PAH01                                  60
     C                   ENDDO
      *
     C**** BEGIN ADD **** 05/08/02  **************************
      * Change to always load a "Y" even if no hit on Rate file
     C                   MOVE      'Y'           A2DBIL
     C**** END ADD ****** 05/08/02  **************************
      *
     C     *IN60         IFEQ      *OFF
      *
     C     A3KY          SETGT     PAR03
     C     A3KY1         READPE    PAR03                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     A3STAT        ANDEQ     'I'
     C     A3KY1         READPE    PAR03                                  60
     C                   ENDDO
      *
     C**** BGN ADD 07/11/01  **************************
     C**** BEGIN DEL **** 05/08/02  **************************
     C**********           MOVE 'Y'       A2DBIL
     C**** END DEL ****** 05/08/02  **************************
     C**** END ADD 07/11/01  **************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      A3PLAN        A2DBIL
     C**** BGN ADD 07/11/01  **************************
     C     A3PLAN        IFEQ      *BLANKS
     C                   MOVE      'Y'           A2DBIL
     C                   END
     C**** END ADD 07/11/01  **************************
     C                   ENDIF                                                  AGKY
      *
     C                   ENDIF                                                  AHKY
      *
      *  Write to output file
      *
     C                   WRITE     PAR02
     C                   ENDSR                                                  SROUT
      *
      *
      *
      *================================================================
      * GETDAT- Return date formatted mm/dd/yy
      *================================================================
     C     GETDAT        BEGSR
      *
     C**** BEGIN CHG *** 10/09/01 ************************************
     C*********            Z-ADDXXDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DT1
     C*********            MOVELSRDTO     X1DATE    P
     C                   MOVEL(P)  XXDATE        X1DATE
     C**** END CHG ***** 10/09/01 ************************************
      *
     C     SRERR         IFEQ      *OFF
      *
     C     X1MM          CAT(P)    '/':0         X2DATE           10
     C                   CAT(P)    X1DD:0        X2DATE
     C                   CAT(P)    '/':0         X2DATE
     C                   CAT(P)    X1YY:0        X2DATE
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       X2DATE
     C                   ENDIF                                                  SRERR
      *
     C                   ENDSR                                                  GETDAT
      *
      *
      *
