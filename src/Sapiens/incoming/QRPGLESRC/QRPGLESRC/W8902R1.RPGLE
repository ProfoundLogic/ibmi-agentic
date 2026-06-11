     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8902R1 - WORK FILE BUILD - POLICY AGENT FILE - WHPAP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - POLICY AGENT FILE                          *
     H*                                                               *
     H* CREATED: 05/16/00  C: 58                                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
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
     H*                                                               *
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
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909 Check for HEX characters   *
     H*                NOTE: THIS PROGRAM DOES NOT HAVE ANY ALPHA     *
     H*                FIELDS, SO NO CHANGES NEEDED IN SOURCE CODE    *
     H*                                                               *
     H* 04/24/02  C:132 WCI091 SEQ: 32217                             *
     H*               - Change: Chg Extact date to be > Maint date    *
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
     H*                                                               *
     H* 08/15/01  C:132 PROJECT WC924                                 *
     H*               - Chg Bypass of Divisions that are not in the
     H*                 State Reporting Flag DIV, Report on all DIVs
     H*               - Also Check that last Active Agent is assigned *
     H*                 for the current Year Record                   *
     H*                                                               *
     H* 07/30/01  C:132 PROJECT: WC924                                *
     H*                 Change input Parm Date to Last Extract Date.  *
     H*                 Compare this input Date against WDELL maint   *
     H*                 Date and BYPASS if > Maint Date               *
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
     F* Employer Detail  Change from WDELL to WDELL8
     FWDELL8    IF   E           K DISK
     F*
     F* Employer Master
     FWMEML     IF   E           K DISK
     F*
     F* Agent History
     FWMAHL     IF   E           K DISK
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
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
     F*
     F* Routing Output File
     FSMORL     IF   E           K DISK
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * Employer Detail History
     FWDEHL1    IF   E           K DISK
      ****** END ADD ***** 11/15/04 ************************************
      ***** BEG DEL ***** 07/25/06 ****************************************
     F**** Begin Add ****  07/22/05  **************************************
     F******   Employer Carrier File
     F******WMRPL   IF  E           K        DISK
      ***** END DEL ***** 07/25/06 ****************************************
     F*
     F* Output - Policy agent file layout
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOB)
     F                                     USROPN
      ***** END ADD ***** 04/13/04 *************************************
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
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C************************************************************
     C*  Begin
     C************************************************************
     C* Retrieve Brand Name
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
     C*  READ EMPLOYER DETAIL
     C*
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELCO#         IFEQ      *ZERO
     C                   GOTO      NEXT
     C                   ENDIF
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
     C     FNCICF        ORNE      'Y'
     C*
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
     C*
     C***** BEG ADD ******* 03/10/04 **********************************
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
     C*   Bypass if have same Policy Year for Employer
     C*
     C     ELCO#         IFEQ      SVCO#
     C     ELFYR         ANDEQ     SVFYR
     C     ELFND         ANDEQ     SVFND
     C     ELEMP#        ANDEQ     SVEMP#
     C                   GOTO      NEXT
     C                   ENDIF
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
     C*   Only process policy records w/ELDT Greater than Input Parm Date
     C*
      **** BEG CHG ***** 04/24/02 **********************************************
     C*******    XXDATE    IFGT ELDT
     C     XXDATE        IFGE      ELDT
      **** END CHG ***** 04/24/02 **********************************************
     C                   GOTO      NEXT
     C                   ENDIF
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
       xxPrt = '0';
       Chain ORKY POR01;
       If %found and *in69 ='0';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   CLOSE     QSYSPRT
     C                   ENDIF
      ***** END ADD ***** 04/13/04 *************************************
     C*
     C                   MOVE      '1'           *INLR
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
     C*                                                     Add 8/17/01
     C                   Z-ADD     ELFYR         SVFYR
     C*
     C*  Policy Number
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C*****Begin*Add*****  07/22/05  **************************************
     C***********CLTID     IFEQ '135'
     C***********RPKEY     CHAINPRP01                62
     C************IN62     IFEQ *OFF
     C***********RPCAP#    ANDNE*BLANK
     C****************     MOVELRPCAP#    PAPONR    P
     C****************     ELSE
     C*****End*Add*******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
     C*
     C                   MOVEL(P)  ELFND         PAPONR
     C                   MOVEL(P)  ELEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       PAPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       PAPONR
      ****************
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
     C*
     C* Set Key for CO#, FND, EMP#, and read until get = policy Yr
     C* Then get Active Agent # with latest Eff Date.
     C                   CLEAR                   PAH01
     C                   Z-ADD     *ZEROS        #HEFDT
     C                   Z-ADD     *ZEROS        #AHRT#
     C                   Z-ADD     *ZEROS        #HAGT#
     C*
     C     AHKY          SETLL     PAH01
     C     AHKY          READE     PAH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C* Read and save fields until get Highest Eff Date
     C     AHFYR         IFEQ      ELFYR
     C     AHSTAT        IFEQ      'A'
     C     AHEFDT        ANDGT     #HEFDT
     C                   Z-ADD     AHEFDT        #HEFDT
     C                   Z-ADD     AHRT#         #AHRT#
     C                   Z-ADD     AHAGT#        #HAGT#
     C                   MOVEL(P)  AHPROD        PAPROD
     C                   ENDIF
     C                   ENDIF
     C     AHKY          READE     PAH01                                  60
     C                   ENDDO
     C*
     C* If have an Effective Date, Move Saved Fields Back
     C     #HEFDT        IFGT      *ZEROS
     C     AHPROD        ANDGT     *ZEROS
     C                   Z-ADD     #HEFDT        AHEFDT
     C                   Z-ADD     #AHRT#        AHRT#
     C                   Z-ADD     #HAGT#        AHAGT#
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C*
     C*  Get Agent rate information
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      SRRATE
     C*
     C*  Get Agent FEIN
     C*
     C  N60AAKY          CHAIN     PAA01                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  AAFEIN        PAAGT#
     C                   MOVEL(P)  AAMOD         XXMOD             3
     C     PAAGT#        CAT(P)    XXMOD:0       PAAGT#
     C                   MOVEL(P)  AAFEIN        PAFEIN
     C                   MOVE      AAMOD         PAMOD
      *
     C                   ELSE
      *
      ***** BEG ADD ***** 04/13/04 *************************************
      * If no agent number, open output file (for first record) get co#
      * information, print headings and detail lines
     C                   ADD       1             ERCNT             7 0
     C     *IN69         IFEQ      *OFF
     C                   OPEN      QSYSPRT
     C                   EXSR      SRSETC
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   EXCEPT    HEAD
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C                   EXCEPT    DETL
      *
     C                   GOTO      NXTRCD
      ***** END ADD ***** 04/13/04 *************************************
      *
      ***** BEG DEL ***** 04/13/04 *************************************
      ***********  IF DO NOT GET A WMAAL1 RECORD ZERO OUT FIELDS
     C***********          MOVE *ZEROS    WKA09   9
     C***********          MOVELWKA09     PAFEIN    P
     C***********          MOVE *ZEROS    WKA12  12
     C***********          MOVELWKA12     PAAGT#    P
     C***********          MOVE *ZERO     PAMOD
      ***** END DEL ***** 04/13/04 *************************************
     C                   ENDIF
     C*
     C                   MOVE(P)   'Y'           PAPRI
     C*
     C*      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
      *
      ***** BEG CHG ***** 03/25/04 ************************************
     C*************        Z-ADDDSDATE    SRDTI
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
      ***** END CHG ***** 03/25/04 ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** END ADD ***** 03/25/04 ************************************
     C                   ENDIF
     C                   MOVE      D10X          PAAODT
     C*
     C*  Write to output file
     C*
     C                   WRITE     PPA01
      *
      ***** BEG ADD ***** 04/13/04 *************************************
     C     NXTRCD        TAG
      ***** END ADD ***** 04/13/04 *************************************
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
     C*
     C     ARKY          SETGT     PAR01
     C     ARKY1         READPE    PAR01                                  61
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
     C*
     C*   Agent History
     C*
     C     AHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C* Use Partial Key for Agent History File
     C*********            KFLD           ELDIV
     C*********            KFLD           ELFYR
     C*
     C*  Agent Assignment File
     C*
     C     AAKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    AHAGT#
     C*
     C*   Agent History
     C*
     C     ARKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    AHRT#
     C                   KFLD                    XXDATE
     C*
     C     ARKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    AHRT#
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
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
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
     C************************************************************
     C*
     C*   *LIKE defines
     C*
     C     *LIKE         DEFINE    AREFDT        XXRNDT
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C*
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    AHEFDT        #HEFDT
     C     *LIKE         DEFINE    AHAGT#        #HAGT#
     C     *LIKE         DEFINE    AHRT#         #AHRT#
     C*
     C************************************************************
     C*  PLIST definitions
     C************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@DATE            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
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
     C                   Z-ADD     *ZERO         SVFYR
     C*
     C                   ENDSR
     C***** ============================================================ *****
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
     O****    E   60           ROUTE
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
     O                                         +  1 'INCREMENTAL EXTRACT'
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
