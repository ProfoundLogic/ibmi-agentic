     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
     H/TITLE W8902R - WORK FILE BUILD - POLICY AGENT FILE - WHPAP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - POLICY AGENT FILE                          *
     H*                                                               *
     H* CREATED: 05/16/00  C: 58                                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/19/14  C:180  Support: WA133 85652                         *
     H*                  Provide agent info for division with a year  *
     H*                  non-existant for the State reporting div     *
     H*                                                               *
     H*                                                               *
     H* 01/04/11  C:191  Project: WC1856 PRISS #10                    *
     H*                  Make processing backward compatible          *
     H*                                                               *
     H* 11/25/10  C:220  Project: WC1856                              *
     H*                  Implement WA726R API to retrieve policy      *
     H*                  start policy year of the group.              *
     H*                                                               *
     H* 10/27/10  C:246  Project: 139WNQ                              *
     H*                  BUILD Policy Agent File - WHPAP for Tertiary *
     H*                  Agent.                                       *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/15/10 C:61 PROJECT 139DMQ                                  *
     H*               Extract Secondary Agent information record.     *
     H*                                                               *
     H* 07/25/06 C: 132   CI031                                       *
     H*           - Remove the code to insert Carrier Policy number   *
     H*             in WHPAP/PAPONR field, use Grp-Emp#-Fyr as Policy *
     H*             number.                                           *
     H*                                                               *
     H* 09/09/05 C: 132   CI099 - 48925                               *
     H*             Change to create correct Policy number field      *
     H*                                                               *
     H* 08/12/05 C: 132 CI093 - 47984                                 *
     H*             Retrieve the Client ID and if is KRMS (093) then  *
     H*             bypass the check on WDEHP and always write record *
     H*                                                               *
     H*    07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          For: client 135;
     H* 11/15/04 C: 132 CI021                                         *
     H*             Add check on WDEHP file when a policy start date  *
     H*             in WDELL is in the future. If no WDEHP record     *
     H*             for future start dates, do not write the record.  *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHPAP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 04/13/04 C:132  CI099  Seq. 41986                             *
     H*             Add edit for zero agent number, print on error    *
     H*             list if have zero agent #, and bypass write of    *
     H*             WHPAP for these records.                          *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*
     H* 02/27/04 C:132 SUPPORT CI099 40909 Check for HEX characters   *
     H*                NOTE: THIS PROGRAM DOES NOT HAVE ANY ALPHA     *
     H*                FIELDS, SO NO CHANGES NEEDED IN SOURCE CODE    *
     H*                                                               *
     H* 10/09/03 C:132 PROJECT:  CI010                                *
     H*                - USE PARM DATE FOR ASOF DATE                  *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* 08/17/01  C:132 PROJECT WC924                                 *
     H*               - Bypass of Same Policy Years for Same CO#,
     H*                 Group#, Emp#. Also get Agent with most recent
     H*                 effective date for Emp#                       *
     H*                                                               *
     H* 08/15/01  C:132 PROJECT WC924                                 *
     H*               - Chg Bypass of Divisions that are not in the
     H*                 State Reporting Flag DIV, Report on all DIVs
     H*               - Also Check that last Active Agent is assigned *
     H*                 for the current Year Record                   *
     H*                                                               *
     H* 07/24/01  C:132 PROJECT: WC924
     H*                 Concatenate Agent FEIN, and Agent MODIFIER
     H*                 into PAAGT# and change nothing else.
     H*                                                               *
     H* 04/04/01  C:58 PROJECT: WC924
     H*                Concatenate Group to Agent number, and
     H*                change nothing else.
     H*                                                               *
     H* 03/21/01  C:61 PROJECT: WC924                                 *
     H*                 FEIN, Left align trailing blanks.             *
     H*                                                               *
     H* 09/05/00  C:58 PROJECT: WC924 Add prodcr loc ID 3S 0 (Mod)
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     F*
     F* Employer Detail
     F**** BEG CHG ***** 08/17/01 ************************************
     F****WDELL   IF  E           K        DISK
     FWDELL8    IF   E           K DISK
     F**** END CHG ***** 08/17/01 ************************************
     F*
     F* Employer Master
     FWMEML     IF   E           K DISK
     F*
      ***** BEG CHG ******* 01/15/10 **********************************
     F**** Agent History
     F*****WMAHL   IF  E           K        DISK
      *   Agent History
     FWMAHP     IF   E             DISK
      ***** END CHG ******* 01/15/10 **********************************
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     F*
     F* Agent Assignment
     FWMAAL1    IF   E           K DISK
     F*
     F* Agent Rate
     FWMARL     IF   E           K DISK
     F*
     F* Output - Policy agent file layout
     FWHPAP     O    E             DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
     F*
      ***** BEG ADD ***** 04/13/04 *************************************
     F* Output - Policy agent file layout
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F*
     FSMORL     IF   E           K DISK
      ***** END ADD ***** 04/13/04 *************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * Employer Detail History
     FWDEHL1    IF   E           K DISK
      ****** END ADD ***** 11/15/04 ************************************
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     F**** Begin Add ****  07/22/05  **************************************
     F******** Employer Carrier File
     F********WMRPL   IF  E           K        DISK
     F**** End Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
     F*

     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
     D*
     D* Effective and Cancel Date Arrays
     D*
     D*
     D*
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*    8 and 10 digit dates, numeric and character
     D                 DS
     D  DN                     1      8
     D                                     DIM(8)
     D  D8X                    1      8
     D  D8N                    1      8  0
     D                 DS
     D  DX                     1     10
     D                                     DIM(10)
     D  D10X                   1     10
     D  D10N                   1     10  0
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D**** Begin Add **** 08/19/14 *********************************************
     D  @WrtAgt        S               N   INZ(*OFF)
     D***  End Add ****** 08/19/14 *********************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C************************************************************
     C*  Begin
     C************************************************************
     C*
     C*  READ EMPLOYER DETAIL
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELCO#         IFEQ      *ZERO
     C**** BEGIN DLT *** 06/10/00 ************************************
     C********** ELFND     OREQ 999
     C**** END DELETE ** 06/10/00 ************************************
     C                   GOTO      NEXT
     C                   ENDIF
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*   Get group master.
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF
     C                   ENDIF
     C*
     C     FNTEST        IFEQ      'Y'
     C**** BEGIN ADD **** 10/02/01 *************************************
     C     FNCICF        ORNE      'Y'
     C**** END ADD ****** 10/02/01 *************************************
     C*
     C                   GOTO      NEXT
     C                   ENDIF
     C**** END ADD ***** 06/10/00 ************************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *OFF
     C                   GOTO      NEXT
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
      *
     C***** BEG ADD ******* 03/10/04 **********************************
     C***** BEG ADD ******* 11/25/10 **********************************
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Process using API only if exists in environment. Else assume old process
     C     XXOLDF        IFEQ      '0'
     C*** END   ADD ****** 01/04/11 ************************************
     C* Retrieve start policy year based on extarct start date.
     C*
     C                   MOVEL     *ZEROS        @@STYR
     C                   MOVEL     *BLANKS       XXSTYR
     C                   MOVEL     ELCO#         X1CO#             3
     C                   MOVEL     ELFND         X1FND             3
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
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C                   ENDIF
     C*** END   ADD ****** 01/04/11 ************************************
     C*
     C***** END ADD ******* 11/25/10 **********************************
     C*
     C     ELFYR         IFLT      @@STYR
     C                   GOTO      NEXT
     C                   ENDIF
     C***** END ADD ******* 03/10/04 **********************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * If Policy start date > run date, check Detail History to see if
      * has been billed (WDEHP), bypass if no recordin WDEHP.
     C     ELPFDT        IFGT      XXDATE
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *ON
      ****** BEG ADD ***** 08/12/05 ************************************
     C     CLTID         ANDNE     '093'
      ****** END ADD ***** 08/12/05 ************************************
     C                   GOTO      NEXT
     C                   ENDIF
     C                   ENDIF
      ****** END ADD ***** 11/15/04 ************************************
      *
     C**** BEGIN ADD *** 08/17/01 ************************************
     C*
     C*   Bypass if have same Policy Year for Employer
     C*
     C     ELCO#         IFEQ      SVCO#
     C     ELFYR         ANDEQ     SVFYR
     C     ELFND         ANDEQ     SVFND
     C     ELEMP#        ANDEQ     SVEMP#
     C                   GOTO      NEXT
     C                   ENDIF
     C**** END ADD ***** 08/17/01 ************************************
     C*
     C*   Get state reporting division
     C*
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     ELEMP#        ORNE      SVEMP#
     C                   EXSR      SREMP
     C                   ENDIF
     C*
     C*   Only process policy records for state reporting division
     C**** BEG DEL **** 08/15/01 ************************************
     C*   Remove code for only policy rcrds for state reporting division
     C*******    ELDIV     IFNE EMDIV
     C*******              GOTO NEXT
     C*******              ENDIF
     C**** END DEL **** 08/15/01 ************************************
     C*
     C*   Write output record
     C*
     C                   EXSR      SROUT
     C*
     C     NEXT          TAG
     C                   READ      PEL01                                  60
     C                   END
     C*
      ***** BEG ADD ***** 04/13/04 *************************************
     C     *IN69         IFEQ      *ON
     C                   EXCEPT    TOTL
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If XXORD = '1' or XXAGT# <> 0;
          xxPrt = '0';
          Chain ORKY POR01;
          If %found and *in69 ='0';
             Except Route;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   CLOSE     QSYSPRT
     C                   ENDIF
      ***** END ADD ***** 04/13/04 *************************************
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 01/15/10 **********************************
     C*
     C*================================================================
     C* SRAGT - Write Agent record to WHPAP
      *================================================================
     C     SRAGT         BEGSR
      *
     C     XXORD         IFEQ      '1'
     C     XXAGT#        ORNE      0
     C                   MOVEL(P)  XXPROD        PAPROD
      *
      *  Get Agent rate information
      *
     C                   EXSR      SRRATE
      *
      *  Get Agent FEIN
      *
     C  N60AAKY          CHAIN     PAA01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AAFEIN        PAAGT#
     C                   MOVEL(P)  AAMOD         XXMOD             3
     C     PAAGT#        CAT(P)    XXMOD:0       PAAGT#
     C                   MOVEL(P)  AAFEIN        PAFEIN
     C                   MOVE      AAMOD         PAMOD
      *
     C                   ELSE
      *
      * If no agent number, open output file (for first record) get co#
      * information, print headings and detail lines
      *
     C                   ADD       1             ERCNT             7 0
      *
     C     *IN69         IFEQ      *OFF
     C                   OPEN      QSYSPRT
     C                   EXSR      SRSETC
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN69
     C                   ENDIF
      *
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
      *
     C                   EXCEPT    DETL
     C                   GOTO      NXTRCD
     C                   ENDIF
     C*
     C                   MOVE(P)   XXORD         PAPRI
     C*
     C*      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
      *
      * Change the Parm Date into the AS OF Date for output record
     C                   MOVEL     @@DATE        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
     C                   ENDIF
      *
     C                   MOVE      D10X          PAAODT
      *
      *  Write to output file
      *
     C                   WRITE     PPA01
     C                   ENDIF
      *
     C     NXTRCD        TAG
     C                   ENDSR
      *
      ***** END ADD ******* 01/15/10 **********************************
     C*
     C*================================================================
     C* SREMP - Get employer record for state reporting division
     C*================================================================
     C     SREMP         BEGSR
     C*
     C*  Get employer record flagged as state reporting division
     C*     else get first employer record
     C*
     C*  If no employer records, process next employer policy year record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO
     C*
     C   60EMKY          CHAIN     PEM01                              60
     C   60              GOTO      NEXT
     C*
     C*  Get Employer Effective/Cancellation date
     C*
     C                   Z-ADD     5             X                 3 0
     C*
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
     C*
     C     10000         ADD       ED(X)         XXRNDT
     C*
     C                   Z-ADD     ELCO#         SVCO#
     C                   Z-ADD     ELFND         SVFND
     C                   Z-ADD     ELEMP#        SVEMP#
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SROUT - Write record to output file WHPAP
     C*================================================================
     C     SROUT         BEGSR
     C*
     C                   CLEAR                   PPA01
      *
      ***** BEG ADD ***** 04/13/04 *************************************
     C                   ADD       1             INCNT             7 0
      ***** END ADD ***** 04/13/04 *************************************
     C*
     C**** BEGIN ADD *** 08/17/01 ************************************
     C                   Z-ADD     ELFYR         SVFYR
     C**** END ADD ***** 08/17/01 ************************************
     C*
     C*  Policy Number
      *
      ****************
      ***** BEG DEL ***** 07/25/06 ****************************************
     C**** Begin Add ****  07/22/05  **************************************
     C***********CLTID     IFEQ '135'
     C***********RPKEY     CHAINPRP01                62
     C************IN62     IFEQ *OFF
     C***********RPCAP#    ANDNE*BLANK
     C****************     MOVELRPCAP#    PAPONR    P
     C****************     ELSE
     C**** End Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
     C*
     C                   MOVEL(P)  ELFND         PAPONR
     C                   MOVEL(P)  ELEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       PAPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       PAPONR
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C*****Begin*Add*****  07/22/05  **************************************
     C****************     ENDIF                           e 62 OFF...&
      ******BEG*ADD****** 09/09/05 ***************************************
     C****************     ELSE
     C****************     MOVELELFND     PAPONR    P
     C****************     MOVELELEMP#    XXTMP   9 P
     C****************     CAT  XXTMP:0   PAPONR    P
     C****************     MOVELELFYR     XXTMP   9 P
     C****************     CAT  XXTMP:0   PAPONR    P
      ******END*ADD****** 09/09/05 ***************************************
     C****************     ENDIF                           e CLTID=135
     C*****End*Add*******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
     C*
     C                   Z-ADD     ELCO#         PACO#
      *********
      ***** BEG DEL ******* 01/15/10 **********************************
     C*********
     C***Get*LAST active agent history record for current year
     C*********
     C*****BEGIN DEL *** 08/17/01 ************************************
     C*********  AHKY      SETGTPAH01
     C*********  AHKY      REDPEPAH01                    60
     C*********  *IN60     DOWEQ*OFF
     C*********  AHSTAT    ANDNE'A'
     C*********  AHKY      REDPEPAH01                    60
     C*********            ENDDO
     C*****END*DEL ***** 08/17/01 ************************************
     C*********
     C*****BEGIN ADD *** 08/17/01 ************************************
     C**Set*Key for CO#, FND, EMP#, and read until get = policy Yr
     C**Then*get Active Agent # with latest Eff Date.
     C*********            CLEARPAH01
     C*********            Z-ADD*ZEROS    #HEFDT
     C*********            Z-ADD*ZEROS    #AHRT#
     C*********            Z-ADD*ZEROS    #HAGT#
     C*********
     C*********  AHKY      SETLLPAH01
     C*********  AHKY      READEPAH01                    60
     C*********
     C*********  *IN60     DOWEQ*OFF
     C**Read*and save fields until get Highest Eff Date
     C*********  AHFYR     IFEQ ELFYR
      *********
     C*********  AHSTAT    IFEQ 'A'
     C*********  AHEFDT    ANDGT#HEFDT
     C*********            Z-ADDAHEFDT    #HEFDT
     C*********            Z-ADDAHRT#     #AHRT#
     C*********            Z-ADDAHAGT#    #HAGT#
     C*********            MOVELAHPROD    PAPROD    P
     C*********            ENDIF                           End AHSTAT=A
     C*********            ENDIF                           End AHFYR =
     C*********  AHKY      READEPAH01                    60
     C*********            ENDDO
     C*********
     C**If*have an Effective Date, Move Saved Fields Back
     C*********  #HEFDT    IFGT *ZEROS
     C*********  AHPROD    ANDGT*ZEROS
     C*********            Z-ADD#HEFDT    AHEFDT
     C*********            Z-ADD#AHRT#    AHRT#
     C*********            Z-ADD#HAGT#    AHAGT#
     C*********            MOVE *OFF      *IN60
     C*********            ENDIF
     C*********
     C*****END*ADD ***** 08/17/01 ************************************
     C*********
     C*********  *IN60     IFEQ *OFF
     C*****BEGIN CHG *** 04/04/01 ************************************
     C*********            MOVELAHAGT#    PAAGT#    P
     C*****BEGIN DEL *** 07/24/01 ************************************
     C*********            MOVELAHFND     PAAGT#    P
     C*********            MOVELAHAGT#    XXAGT#  5 P
     C*********  PAAGT#    CAT  XXAGT#:0  PAAGT#    P
     C*****BEG*DEL ***** 08/17/01 ************************************
     C*********            MOVELAHPROD    PAPROD    P
     C*****END*DEL ***** 08/17/01 ************************************
     C*****END*DEL ***** 07/24/01 ************************************
     C*****END*CHANGE ** 04/04/01 ************************************
     C*********
     C***Get*Agent rate information
     C*********
     C*********            EXSR SRRATE
     C*********
     C***Get*Agent FEIN
     C*********
     C**N60****  AAKY      CHAINPAA01                60
     C*********            ENDIF
     C*********
     C*********  *IN60     IFEQ *OFF
     C*****BEGIN ADD *** 07/24/01 ************************************
     C*********            MOVELAAFEIN    PAAGT#    P
     C*********            MOVELAAMOD     XXMOD   3 P
     C*********  PAAGT#    CAT  XXMOD:0   PAAGT#    P
     C*****END*ADD ***** 07/24/01 ************************************
      ******BEG CHG ******* 03/21/01 **********************************
     C*********            MOVE AAFEIN    PAFEIN
     C*********            MOVELAAFEIN    PAFEIN    P
      ******END CHG ******* 03/21/01 **********************************
     C****BEGIN ADD ***** 09/05/00 ************************************
     C*********            MOVE AAMOD     PAMOD
     C****END*ADD ******* 09/05/00 ************************************
      ******BEG ADD ******* 03/21/01 **********************************
      *********
     C*********            ELSE
      *********
      ******BEG ADD ***** 04/13/04 *************************************
      **If*no*agent number, open output file (for first record) get co#
      **information, print headings and detail lines
     C*********            ADD  1         ERCNT   70
     C*********  *IN69     IFEQ *OFF
     C*********            OPEN QSYSPRT
     C*********            EXSR SRSETC
     C*********            EXCPTROUTE
     C*********            EXCPTHEAD
     C*********            MOVE *ON       *IN69
     C*********            ENDIF
     C*********  *INOB     IFEQ *ON
     C*********            EXCPTHEAD
     C*********            ENDIF
     C*********            EXCPTDETL
      *********
     C*********            GOTO NXTRCD
      ******END ADD ***** 04/13/04 *************************************
      *********
      ******BEG DEL ***** 04/13/04 *************************************
     C*****BEGIN ADD *** 07/24/01 ************************************
      *********  IF DO NOT GET A WMAAL1 RECORD ZERO OUT FIELDS
     C*********            MOVE *ZEROS    WKA09   9
     C*********            MOVELWKA09     PAFEIN    P
     C*********            MOVE *ZEROS    WKA12  12
     C*********            MOVELWKA12     PAAGT#    P
     C*****END*ADD ***** 07/24/01 ************************************
     C*********            MOVE *ZERO     PAMOD
      ******END ADD ******* 03/21/01 **********************************
      ******END DEL ***** 04/13/04 *************************************
     C*********            ENDIF
     C*********
     C*********            MOVE 'Y'       PAPRI     P
     C*********
     C*******As of date
     C*********            MOVEL*BLANKS   D10X
     C*********            TIME           DSDTTM
      ******BEG CHG **** 10/09/03 ********************************
      **Change*the Parm Date into the AS OF Date for output record
     C*********            MOVEL@@DATE    ZZRNDT  80
     C*********            Z-ADDZZRNDT    SRDTI
     C*************        Z-ADDDSDATE    SRDTI
      ******END CHG **** 10/09/03 ********************************
     C*********            CALL 'SRCVTDAT'DTMDYY
     C*********  SRERR     IFEQ '0'
     C*********            MOVELSRDTO     D8N
     C*********            EXSR EDDAT
     C*********            ENDIF
     C*********            MOVE D10X      PAAODT
     C*********
     C***Write*to output file
     C*********
     C*********            WRITEPPA01
      *********
      ******BEG ADD ***** 04/13/04 *************************************
     C*********  NXTRCD    TAG
      ******END ADD ***** 04/13/04 *************************************
      ***** END DEL ******* 01/15/10 **********************************
      ***** BEG ADD ******* 01/15/10 **********************************
      /Free
       //****** Begin add ****** 08/19/14 ********************************* ****
         @WrtAgt = *OFF;
       //****** End   add ****** 08/19/14 ********************************* ****
      /End-free
      *
      *  Get LAST active agent history record for state reporting division
      *
     C                   CALL      'WA002R'
     C                   PARM      EMCO#         ##CO#             3 0
     C                   PARM      EMFND         ##FND             3 0
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C                   PARM      *ZEROS        ##AGT#            5 0
     C                   PARM      *BLANKS       ##ANAM           60
     C                   PARM      *ZEROS        ##PROD            5 0
     C                   PARM      *BLANKS       ##PNAM           60
     C                   PARM      *ZEROS        ##AREA            3 0
     C                   PARM      *ZEROS        ##PHON            7 0
     C                   PARM      '*'           ##PLAN            1
     C                   PARM      12312099      ##EFDT            8 0
     C                   PARM      *ZEROS        ##NWRT            5 2
     C                   PARM      *ZEROS        ##RNRT            5 2
     C                   PARM      *ZEROS        ##RT#             3 0
     C                   PARM      *BLANKS       ##PEML           60
     C                   PARM      *BLANKS       ##CAFL            1
     C                   PARM      *ZEROS        #AHRRN            9 0
     C*
     C     ##AGT#        IFNE      *ZEROS
     C     #AHRRN        ANDNE     *ZEROS
     C********* Begin add ****** 08/19/14 **************************************
     C                   MOVE      *ON           @WrtAgt
     C                   ELSE
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C                   PARM      *ZEROS        ##AGT#            5 0
     C                   PARM      *BLANKS       ##ANAM           60
     C                   PARM      *ZEROS        ##PROD            5 0
     C                   PARM      *BLANKS       ##PNAM           60
     C                   PARM      *ZEROS        ##AREA            3 0
     C                   PARM      *ZEROS        ##PHON            7 0
     C                   PARM      '*'           ##PLAN            1
     C                   PARM      12312099      ##EFDT            8 0
     C                   PARM      *ZEROS        ##NWRT            5 2
     C                   PARM      *ZEROS        ##RNRT            5 2
     C                   PARM      *ZEROS        ##RT#             3 0
     C                   PARM      *BLANKS       ##PEML           60
     C                   PARM      *BLANKS       ##CAFL            1
     C                   PARM      *ZEROS        #AHRRN            9 0
     C     ##AGT#        IFNE      *ZEROS
     C     #AHRRN        ANDNE     *ZEROS
     C                   MOVE      *ON           @WrtAgt
     C                   ENDIF
     C                   ENDIF
      **
     C                   IF        @WrtAgt
     C********* End add ******** 08/19/14 **************************************
     C     #AHRRN        CHAIN     PAH01                              60
      *
      * AGENT 1
      *
     C                   Z-ADD     AHAGT#        XXAGT#
     C                   Z-ADD     AHRT#         XXRT#
     C                   Z-ADD     AHPROD        XXPROD
     C                   MOVE      '1'           XXORD             1
     C                   EXSR      SRAGT
      *
      * AGENT 2
      *
     C                   Z-ADD     AHSAGT        XXAGT#
     C                   Z-ADD     AHSRT#        XXRT#
     C                   Z-ADD     AHSPRD        XXPROD
     C                   MOVE      '2'           XXORD
     C                   EXSR      SRAGT
      *
      ***** BEG ADD ******* 10/27/10 **********************************
      *
      * AGENT 3 Tertiary Agent
      *
     C                   Z-ADD     AH3AGT        XXAGT#
     C                   Z-ADD     AH3RT#        XXRT#
     C                   Z-ADD     AH3PRD        XXPROD
     C                   MOVE      '3'           XXORD
     C                   EXSR      SRAGT
      ***** END ADD ******* 10/27/10 **********************************
     C                   ENDIF
      *
      ***** END ADD ******* 01/15/10 **********************************
     C                   ENDSR
     C*
     C*================================================================
     C*      EDDAT      EDIT DATE  8 digit to MM/DD/YYYY
     C*================================================================
     C     EDDAT         BEGSR
     C*
     C                   Z-ADD     1             W                 2 0
     C                   DO        10            X
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           DX(X)
     C                   ELSE
     C                   MOVEL     DN(W)         DX(X)
     C                   ADD       1             W
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SRRATE - Get agent rate
     C*================================================================
     C     SRRATE        BEGSR
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  61
      *
      ***** BEG ADD ******* 01/15/10 **********************************
     C     *IN61         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY1         READPE    PAR01                                  61
     C                   ENDDO
      ***** END ADD ******* 01/15/10 **********************************
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     XXRNDT        WHENGE    ELPTDT
     C     ARNWRT        DIV       100           PARATE
     C*
     C     XXRNDT        WHENLE    ELPFDT
     C     ARRNRT        DIV       100           PARATE
     C*
     C     XXDATE        WHENGE    XXRNDT
     C     ARRNRT        DIV       100           PARATE
     C*
     C                   OTHER
     C     ARNWRT        DIV       100           PARATE
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*  Commission rate is zero if agent effective date in future
     C*
     C     XXDATE        IFLT      AHEFDT
     C                   Z-ADD     *ZERO         PARATE
     C                   ENDIF
     C*
     C                   ENDSR
     C*
      ***** BEG ADD ***** 04/13/04 *************************************
     C*
     C*================================================================
     C*      SRSETC - GET COMPANY INFORMATION FOR ROUTING PAGE
     C*================================================================
     C     SRSETC        BEGSR
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      @@DATE        JOBDTE            8 0
     C*
     C                   ENDSR
     C*
      ***** END ADD ***** 04/13/04 *************************************
     C*
     C*================================================================
     C*      *INZSR  Initialization
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C*  KLIST
     C*
     C*  Employer
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      ***** BEG DEL ******* 01/15/10 **********************************
     C*********
     C****Agent History
     C*********
     C*********  AHKY      KLIST
     C*********            KFLD           ELCO#
     C*********            KFLD           ELFND
     C*********            KFLD           ELEMP#
      ***** END DEL ******* 01/15/10 **********************************
     C***** BEG DEL **** 08/17/01 ******************************
     C*****                KFLD           ELDIV
     C*****                KFLD           ELFYR
     C***** END DEL **** 08/17/01 ******************************
     C*
     C*  Agent Assignment File
     C*
     C     AAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      ***** BEG CHG ******* 01/15/10 **********************************
     C*********            KFLD           AHAGT#
     C                   KFLD                    XXAGT#
      ***** END CHG ******* 01/15/10 **********************************
     C*
     C*   Agent History
     C*
     C     ARKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      ***** BEG CHG ******* 01/15/10 **********************************
     C*********            KFLD           AHRT#
     C                   KFLD                    XXRT#
      ***** END CHG ******* 01/15/10 **********************************
     C                   KFLD                    XXDATE
     C*
     C     ARKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      ***** BEG CHG ******* 01/15/10 **********************************
     C*********            KFLD           AHRT#
     C                   KFLD                    XXRT#
      ***** END CHG ******* 01/15/10 **********************************
     C*
      ****** BEG ADD ***** 11/15/04 ************************************
      *
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
      ****** END ADD ***** 11/15/04 ************************************
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C**** END ADD ***** 06/10/00 ************************************
     C************************************************************
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C**** Begin Add ****  07/22/05  **************************************
     C     RPKEY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
     C*
     C*   *LIKE defines
     C*
      ***** BEG ADD ******* 01/15/10 **********************************
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHRT#         XXRT#
     C     *LIKE         DEFINE    AHPROD        XXPROD
      ***** END ADD ******* 01/15/10 **********************************
     C     *LIKE         DEFINE    AREFDT        XXRNDT
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C**** BEGIN ADD *** 08/17/01 ************************************
     C     *LIKE         DEFINE    ELFYR         SVFYR
      ***** BEG DEL ******* 01/15/10 **********************************
     C*********  *LIKE     DEFN AHEFDT    #HEFDT
     C*********  *LIKE     DEFN AHAGT#    #HAGT#
     C*********  *LIKE     DEFN AHRT#     #AHRT#
      ***** END DEL ******* 01/15/10 **********************************
     C**** END ADD ***** 08/17/01 ************************************
     C*
     C************************************************************
     C*
     C*  PLIST definitions
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@DATE            8
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
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
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  INITIALIZE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     @@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C***** BEG ADD ******* 11/25/10 **********************************
     C                   MOVEL     @@STYR        @@YRS             3
     C***** END ADD ******* 11/25/10 **********************************
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     @@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ELSE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         SVCO#
     C                   Z-ADD     *ZERO         SVFND
     C                   Z-ADD     *ZERO         SVEMP#
     C**** BEGIN ADD *** 08/17/01 ************************************
     C                   Z-ADD     *ZERO         SVFYR
     C**** END ADD ***** 08/17/01 ************************************
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
     C*
     C                   ENDSR
      ** ============================================================ **
      *
      ***** BEG ADD ***** 04/13/04 *************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 58 'COPIES:'
     O**********             ORCOPY        Z     62
     O**********                                 72 'HOLD:'
     O**********             ORHOLD              77
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
     O                                           19 'JOB DATE  . :'
     O                       JOBDTE              30 '  /  /    '
     O          E            ROUTE          2
     O                                           19 'RUN TIME/DTE:'
     O                       DSDTTM              39 '  :  :  &  /  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               52 '.2004'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     OQSYSPRT   E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                                           45 'CICENTER - WHPAP FILE'
     O                       DSDATE             123 '  /  /    '
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           47 'ERROR LISTING-MISSING A'
     O                                           63 'GENT INFORMATION'
     O                       PAGE          Z    123
     O                                          118 'PAGE'
     O          E            HEAD           2
     O                                            4 'CO#'
     O                                            9 'GRP'
     O                                           20 'EMPLOYER#'
     O                                           25 'DIV'
     O                                           30 'YEAR'
     O                                           45 'INSURED NAME'
     O*
     O          E            DETL           1
     O                       ELCO#         3      4
     O                       ELFND         3      9
     O                       ELEMP#        3     20
     O                       ELDIV         3     25
     O                       ELFYR         3     30
     O                       EMNAME              73
     O*
     O          E            TOTL        1  2
     O                                           25 '***** TOTALS ***** '
     O                                           55 '******** NO AGENT NUMB'
     O                                           74 'ER RECORDS ********'
     O*
     O          E            TOTL           1
     O                                           22 'Total number of record'
     O                                           44 's read  . . . . . . .:'
     O                       INCNT         3     53
     O          E            TOTL           1
     O                                           22 'Total number-Records w'
     O                                           44 ' ith No Agent Number :'
     O                       ERCNT         3     53
      ***** END ADD ***** 04/13/04 *************************************
