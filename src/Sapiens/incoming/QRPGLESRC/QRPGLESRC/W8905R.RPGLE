     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8905R - WORK FILE BUILD - WHPRP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - PDS DIVISION PREMIUM FILE - WHPRP          *
     H*                                                               *
     H* CREATED: 04/04/00  C:61                                       *
     H*****************************************************************
     H* Indicators
     H*        60  General chain, read
     H*        61  Chain carrier WMRPP
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/23/13  C:283  Support: OP099 83793                         *
     H*                  Correctly populate the exposure state value. *
     H*                                                               *
     H* 01/04/11  C:191  Project: WC1856 PRISS #10                    *
     H*                  Make processing backward compatible          *
     H*                                                               *
     H* 11/25/10  C:220  Project: WC1856                              *
     H*                  Implement WA726R API to retrieve policy      *
     H*                  start policy year of the group.              *
     H*                                                               *
     H* 08/12/09 C:33  Project: CI077                                 *
     H*                Expand name.                                   *
     H*                                                               *
     H* 02/13/09 C:61  SUPPORT WA071 67185                            *
     H*                Correct employer CX status determination.      *
     H*                                                               *
     H* 03/25/08 C:61  PROJECT CI041                                  *
     H*                Add processing to populate division status     *
     H*                fields.                                        *
     H*                                                               *
     H* 01/23/08 C:61  SUPPORT CI132 61860                            *
     H*                - Add mailing address fields.                  *
     H*                                                               *
     H* 07/25/06 C: 132   CI031                                       *
     H*           - Remove the code to insert Carrier Policy number   *
     H*             in WHPRP/PRPONR field, use Grp-Emp#-Fyr as Policy *
     H*             number.                                           *
     H*                                                               *
     H* 02/20/06 C: 132   135WEJ                                      *
     H*             Code to get the "Writen Premium" and the "Manual  *
     H*             premium" from file QXSQL if client is PPC (135)   *
     H*             and place AMTS in file WHPRP                      *
     H*                                                               *
     H*                                                               *
     H* 09/09/05 C: 132   CI099 - 48925                               *
     H*             Change to create correct Policy number field      *
     H*                                                               *
     H* 08/12/05 C: 132 CI093 - 47984                                 *
     H*             Retrieve the Client ID and if is KRMS (093) then  *
     H*             bypass the check on WDEHP and always write record *
     H*                                                               *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 06/30/05 C: 132  CI135 - 47867                                *
     H*             RECOMPILE for WMFNP change length-NO code chgs.   *
     H*                                                               *
     H* 11/15/04 C: 132 CI021                                         *
     H*             Add check on WDEHP file when a policy start date  *
     H*             in WDELL is in the future. If no WDEHP record     *
     H*             for future start dates, do not write the record.  *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHPRP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909     (ADDED 03/12/04)       *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 10/09/03 C:132 PROJECT:  CI010                                *
     H*                - USE PARM DATE FOR ASOF DATE-DONE ON 03/21/01 *
     H*                                                               *
     H* 05/30/02 C:132 PROJECT WC924                                  *
     H*                Add new fields - Pay Plan Type Code - PRBSCD   *
     H*                               - Pay Plan Type Desc - PRBDES   *
     H*                               - Rating Tier Code   - PRRTCD   *
     H*                               - Rating Tier Descptn- PRRDES   *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* 08/13/01 C:132 PROJECT WC924                                  *
     H*                Load PREST Exposure State Field                *
     H*                                                               *
     H* 03/21/01 C:61  PROJECT WC924                                  *
     H*                Output 'AS OF Date' in mm/dd/yyyy              *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*
     H* 05/15/00  C:58 PROJECT WC924
     H*                 ADD field: PRAODT - Data AS OF date.
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
      * Employer Detail
     FWDELL     IF   E           K DISK
      *
      * Employer Master
     FWMEML     IF   E           K DISK
      *
      * Employer Detail History
     FWDEHL1    IF   E           K DISK
      *
     F**** BEGIN ADD *** 06/10/00 ************************************
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F**** END ADD ***** 06/10/00 ************************************
     F**** BEGIN ADD *** 08/13/01 ************************************
     F*
     F* State Rating Code Master
     FWMRSL     IF   E           K DISK
     F**** END ADD ***** 08/13/01 ************************************
     F*
      * County
     FWTCNL1    IF   E           K DISK
      *
      * Output - PDS Division Premium
     FWHPRP     O    E             DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     F**** Begin Add ****  07/22/05  **************************************
     F****** Employer Carrier File
     F******WMRPL   IF  E           K        DISK
     F**** End Add ******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
     F*
      **** BEGIN ADD *** 05/30/02 ************************************
     F*
     F* Billing Schedule Code Table File
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
      **** END ADD ***** 05/30/02 ************************************
     F*
      ***** BEG ADD ***** 01/05/06 ***********************************
     FQXSQL     IF   E           K DISK
      ***** END ADD ***** 01/05/06 ***********************************
     F*
      *
      ***** BEG ADD ***** 02/27/04 ************************************

     D HX              S              1    DIM(50)                              HEX CNVT ARRY
      ***** END ADD ***** 02/27/04 ************************************
      ***** BEG ADD ******* 03/25/08 **********************************
      *
      * Effective and Cancel Date Arrays
      *
      *
     D                 DS
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      ***** END ADD ******* 03/25/08 **********************************
      *
     D                 DS
     D  X1DATE                 1      8
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1YY                   5      8
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      ************************************************************
      *  KLIST
      ************************************************************
      *
      *  Employer
      *

     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      ***** BEG ADD ******* 03/25/08 **********************************
      *
     C     EMKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      *
      * Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SVFYR
      *
     C     ELKY1         KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
      ***** END ADD ******* 03/25/08 **********************************
      *
      *  Employer Detail History
      *
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
      *  Table
      *
     C     CNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKKEY
      *
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C**** END ADD ***** 06/10/00 ************************************
     C**** BEGIN ADD *** 08/13/01 ************************************
     C*
     C* State Rating Code Master
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C**** END ADD ***** 08/13/01 ************************************
      *
      **** BEGIN ADD *** 05/30/02 ************************************
     C* Billing Schedule Code Table File
     C     BSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    BSKEY
      **** END ADD ***** 05/30/02 ************************************
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
      *
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
      *
      ************************************************************
      *   *LIKE defines
      ************************************************************
      *
      ***** BEG ADD ******* 03/25/08 **********************************
     C     *LIKE         DEFINE    EMCAD1        XXCXDT
     C     *LIKE         DEFINE    ELFYR         SVFYR
      ***** END ADD ******* 03/25/08 **********************************
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C     *LIKE         DEFINE    CNKEY         KKKEY
     C     *LIKE         DEFINE    ELPFDT        XXRNDT
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
      *
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
     C***** BEG ADD ***** 11/25/10 *********************************** *
     C                   MOVEL     @@STYR        @@YRS             3
     C***** END ADD ***** 11/25/10 *********************************** *
      *
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
      ***** BEG ADD ******* 03/21/01 **********************************
     C                   MOVEL     @@RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      ***** END ADD ******* 03/21/01 **********************************
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
      *
     C                   ENDIF                                                  SRERR
      *
      *
     C                   Z-ADD     *ZERO         SVCO#
     C                   Z-ADD     *ZERO         SVFND
     C                   Z-ADD     *ZERO         SVEMP#
     C                   Z-ADD     *ZERO         SVDIV
      *
      *  READ EMPLOYER DETAIL
      *
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
      *
     C***** BEG ADD ***** 11/25/10 **********************************
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C* Process using API only if exists in environment. Else assume old process
     C     XXOLDF        IFEQ      '0'
     C*** END   ADD ****** 01/04/11 ************************************
     C*
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
     C*
     C*** BEGIN ADD ****** 01/04/11 ************************************
     C                   ENDIF                                                  XXOLDF='0'
     C*** END   ADD ****** 01/04/11 ************************************
     C***** END ADD ***** 11/25/10 **********************************
     C*
     C     ELCO#         IFEQ      *ZERO
     C**** BEGIN DLT *** 06/10/00 ************************************
     C********** ELFND     OREQ 999
     C**** END DELETE ** 06/10/00 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ELFYR         ORLT      @@STYR
     C***** END ADD ******* 03/10/04 **********************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * If Policy start date > run date, check Detail History to see if
      * has been billed (WDEHP), bypass if no recordin WDEHP.
     C     ELPFDT        IFGT      XXRNDT
     C     EHKY          CHAIN     PEH01                              62
     C     *IN62         IFEQ      *ON
      ****** BEG ADD ***** 08/12/05 ************************************
     C     CLTID         ANDNE     '093'
      ****** END ADD ***** 08/12/05 ************************************
     C                   GOTO      NEXT
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                   ELPFDT<>XXDATE
      ****** END ADD ***** 11/15/04 ************************************
     C*
     C**** BEGIN ADD *** 06/10/00 ************************************
     C*
     C*   Get group master.
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   ENDIF                                                  .CO/FND <>
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
      *   Get employer master record
      *
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C     ELEMP#        ORNE      SVEMP#
     C     ELDIV         ORNE      SVDIV
     C                   EXSR      SREMP
     C**** BEGIN DEL *** 10/23/13 ************************************
     C**** BEGIN ADD *** 08/13/01 ************************************
     C****************   EXSR      SRRSP
     C**** END ADD ***** 08/13/01 ************************************
     C**** END   DEL *** 10/23/13 ************************************
      *
     C                   ENDIF
     C**** BEGIN ADD *** 10/23/13 ************************************
     C                   EXSR      SRRSP
     C**** END ADD ***** 10/23/13 ************************************
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C                   READ      PEL01                                  60
     C                   END
      *
      *
     C                   MOVE      '1'           *INLR
      *
      *
      *
      *================================================================
      * GETDAT- Return date formatted mm/dd/yy
      *================================================================
     C     GETDAT        BEGSR
      *
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         X1DATE
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
      *================================================================
      * SRPHN - Format phone number for output
      *================================================================
     C     SRPHN         BEGSR
      *
     C     XXAREA        IFNE      '000'
      *
     C                   MOVEL(P)  '('           XXPHON           20
     C                   CAT(P)    XXAREA:0      XXPHON
     C                   CAT(P)    ')':0         XXPHON
     C                   CAT(P)    XXPH1:1       XXPHON
     C                   CAT(P)    '-':0         XXPHON
     C                   CAT(P)    XXPH2:0       XXPHON
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       XXPHON
     C                   ENDIF
      *
     C                   ENDSR                                                  SRPHN
      *
      *
      *
      *================================================================
      * SREMP - Get employer record for state reporting division
      *================================================================
     C     SREMP         BEGSR
      *
      *  Get employer record
      *
      *  If no employer records, process next employer policy year record
      *
     C     EMKY          CHAIN     PEM01                              60
      *
     C   60              GOTO      NEXT
      *
     C                   Z-ADD     EMCO#         SVCO#
     C                   Z-ADD     EMFND         SVFND
     C                   Z-ADD     EMEMP#        SVEMP#
     C                   Z-ADD     EMDIV         SVDIV
      *
     C                   ENDSR                                                  SREMP
      *
      *================================================================
      * SRRSP - Get Exposure State Field from WMRSL
      *================================================================
     C     SRRSP         BEGSR
     C*
     C**** BEGIN ADD *** 08/13/01 ************************************
      **** BEGIN CHG *** 05/30/02 ************************************
     C**********           CLEARRSJRST
     C                   MOVE      *BLANKS       ##JRST            2
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL     RSJRST        ##JRST                         SAVE ST JURSTN
      **** END CHG ***** 05/30/02 ************************************
     C**** END ADD ***** 08/13/01 ************************************
      *
     C                   ENDSR                                                  SRDSP
      *
      *================================================================
      * SROUT - Write record to output file WHPRP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PPR01
      *
      *  Policy Number
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C*****Begin*Add*****  07/22/05  **************************************
     C***************
     C******Policy*number
     C***********CLTID     IFEQ '135'
     C***********RPKEY     CHAINPRP01                61
     C************IN61     IFEQ *OFF
     C***********RPCAP#    ANDNE*BLANK
     C***************      MOVELRPCAP#    PRPONR    P
     C***************      ELSE
     C*****End*Add*******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
      *
     C                   MOVEL(P)  ELFND         PRPONR
     C                   MOVEL(P)  ELEMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       PRPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       PRPONR
      *
      ***** BEG DEL ***** 07/25/06 ****************************************
     C*****Begin*Add*****  07/22/05  **************************************
     C***************      ENDIF                           e 61 OFF...&
      ******BEG*ADD****** 09/09/05 ***************************************
     C***************      ELSE
     C***************      MOVELELFND     PRPONR    P
     C***************      MOVELELEMP#    XXTMP   9 P
     C***************      CAT  XXTMP:0   PRPONR    P
     C***************      MOVELELFYR     XXTMP   9 P
     C***************      CAT  XXTMP:0   PRPONR    P
      ******END*ADD****** 09/09/05 ***************************************
     C***************      ENDIF                           e CLTID=135
     C*****End*Add*******  07/22/05  **************************************
      ***** END DEL ***** 07/25/06 ****************************************
      *
     C                   Z-ADD     EMCO#         PRCO#
     C                   Z-ADD     EMDIV         PRDIV
      *
      *  Exposure State
      *
      **** BEGIN ADD *** 05/30/02 ************************************
      *  Rating Tier Code & Description
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  ELRTCD        PRRTCD
     C                   MOVEL(P)  RSDESC        PRRDES
     C                   ENDIF
      *  Pay Plan Type Code
     C                   MOVEL     ELBSCD        PRBSCD
     C                   MOVEL(P)  ELBSCD        BSKEY
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  BSDESC        PRBDES
     C                   ENDIF
      **** END ADD ***** 05/30/02 ************************************
      *
     C**** BEGIN CHG *** 08/13/01 ************************************
     C**** BEG CHG ***** 05/30/02 ************************************
     C***********          MOVELRSJRST    PREST
     C                   MOVEL     ##JRST        PREST
      **** END CHG ***** 05/30/02 ************************************
     C     PREST         IFEQ      *BLANKS
     C**** END CHG ***** 08/13/01 ************************************
     C     EMPST         IFNE      *BLANKS
     C                   MOVEL(P)  EMPST         PREST
     C                   ELSE
     C                   MOVEL(P)  EMMST         PREST
     C                   ENDIF                                                  EMPST
     C**** BEGIN CHG *** 08/13/01 ************************************
     C                   ENDIF                                                  PREST
     C**** END CHG ***** 08/13/01 ************************************
      *
      *
     C     ELFAF         IFEQ      'A'
     C                   ADD       ELAAPR        PRTTPR
      *
     C                   ELSE
      *
     C                   ADD       ELANPR        PRTTPR
     C                   ENDIF                                                  ELFAF
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAME        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAME
      ***** END ADD ***** 02/27/04 ************************************
     C***  Begin Add  ***  08/12/09  **************************************
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAM2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAM2
     C***  End   Add  ***  08/12/09  **************************************
      *
     C***  Begin Chg  ***  08/12/09  **************************************
     C**********           MOVELEMNAME    PRNAME    P
     C     EMNAME        CAT(P)    EMNAM2        PRNAME
     C***  End   Chg  ***  08/12/09  **************************************
      *
     C**** BEGIN CHG *** 08/13/01 ************************************
     C*  Check P/M Address Code-If 'P' load Physical Address, ELSE Mailing Addr
      *
     C*********  EMPAD1    IFNE *BLANKS
      ***** BEG DEL ******* 01/23/08 **********************************
     C*********  EMMPFL    IFEQ 'P'
      ***** END DEL ******* 01/23/08 **********************************
     C**** END CHG ***** 08/13/01 ************************************
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMPAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMPAD1
      *
     C     EMPAD2        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMPAD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMPAD2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMPCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMPCTY
      ***** END ADD ***** 02/27/04 ************************************
     C                   MOVEL(P)  EMPAD1        PRAD1
     C                   MOVEL(P)  EMPAD2        PRAD2
     C                   MOVEL(P)  EMPCTY        PRCTY
     C                   MOVEL(P)  EMPST         PRST
     C                   MOVEL     EMPZIP        XXZIP5            5
     C                   MOVE      EMPZIP        XXZIP4            4
     C**** BEGIN DEL *** 08/13/01 ************************************
     C*********  XXZIP5    CAT  '-':0     PRZIP     P
     C*********            CAT  XXZIP4:0  PRZIP     P
     C**** END DEL ***** 08/13/01 ************************************
      ***** BEG ADD ******* 01/23/08 **********************************
     C     XXZIP5        CAT(P)    '-':0         PRZIP
     C                   CAT(P)    XXZIP4:0      PRZIP
      ***** END ADD ******* 01/23/08 **********************************
      ***** BEG DEL ******* 01/23/08 **********************************
      ******
     C******               ELSE
      ******
      ******BEG ADD ***** 02/27/04 ************************************
      **Clear array then Check for Invalid Hex characters
     C******               MOVEA*BLANKS   HX
     C******               MOVEAEMMAD1    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        EMMAD1
      ******
     C******     EMMAD2    IFGT *BLANKS
     C******               MOVEA*BLANKS   HX
     C******               MOVEAEMMAD2    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        EMMAD2
     C******               ENDIF
      ******
     C******               MOVEA*BLANKS   HX
     C******               MOVEAEMMCTY    HX
     C******               EXSR SRHEX
     C******               MOVEAHX        EMMCTY
      ******END ADD ***** 02/27/04 ************************************
      ******
     C******               MOVELEMMAD1    PRAD1     P
     C******               MOVELEMMAD2    PRAD2     P
     C******               MOVELEMMCTY    PRCTY     P
     C******               MOVELEMMST     PRST      P
     C******               MOVELEMMZIP    XXZIP5  5
     C******               MOVE EMMZIP    XXZIP4  4
     C*****BEGIN DEL *** 08/13/01 ************************************
     C*********  XXZIP5    CAT  '-':0     PRZIP     P
     C*********            CAT  XXZIP4:0  PRZIP     P
     C*****END DEL ***** 08/13/01 ************************************
     C******               ENDIF                           EMPAD1
      ******
     C*****BEGIN ADD *** 08/13/01 ************************************
     C******     XXZIP5    CAT  '-':0     PRZIP     P
     C******               CAT  XXZIP4:0  PRZIP     P
     C*****END ADD ***** 08/13/01 ************************************
      ***** END DEL ******* 01/23/08 **********************************
      ***** BEG ADD ******* 01/23/08 **********************************
      ******
      * Clear array then Check for Invalid Hex characters
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMMAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMMAD1
      *
     C     EMMAD2        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMMAD2        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMMAD2
     C                   ENDIF
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMMCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMMCTY
      *
     C                   MOVEL(P)  EMMAD1        PRMAD1
     C                   MOVEL(P)  EMMAD2        PRMAD2
     C                   MOVEL(P)  EMMCTY        PRMCTY
     C                   MOVEL(P)  EMMST         PRMST
     C                   MOVEL     EMMZIP        XXZIP5            5
     C                   MOVE      EMMZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         PRMZIP
     C                   CAT(P)    XXZIP4:0      PRMZIP
      ***
     C                   MOVE      EMMPFL        PRMPFL
      *
     C     PRAD1         IFEQ      *BLANK
     C                   MOVE      PRMAD1        PRAD1
     C                   MOVE      PRMAD2        PRAD2
     C                   MOVE      PRMCTY        PRCTY
     C                   MOVE      PRMST         PRST
     C                   MOVE      PRMZIP        PRZIP
     C                   ENDIF
      *
     C     PRMAD1        IFEQ      *BLANK
     C                   MOVE      PRAD1         PRMAD1
     C                   MOVE      PRAD2         PRMAD2
     C                   MOVE      PRCTY         PRMCTY
     C                   MOVE      PRST          PRMST
     C                   MOVE      PRZIP         PRMZIP
     C                   ENDIF
      *
      ***** END ADD ******* 01/23/08 **********************************
      *
      *  Get county code description
      *
     C     EMPST         IFNE      *BLANKS
     C     EMPST         CAT       EMCNTY:0      ZZKEY             4
     C                   ELSE
     C     EMMST         CAT       EMCNTY:0      ZZKEY
     C                   ENDIF                                                  EMPST
      *
     C                   MOVEL(P)  ZZKEY         KKKEY
     C     CNKY          CHAIN     PCN01                              60
     C  N60              MOVEL(P)  CNDESC        PRCNDS
      *
     C                   MOVE      EMFAXA        XXAREA            3
     C                   MOVEL     EMFAX#        XXPH1             3
     C                   MOVE      EMFAX#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        PRFAX#
      *
     C                   MOVE      EMAREA        XXAREA            3
     C                   MOVEL     EMPHN#        XXPH1             3
     C                   MOVE      EMPHN#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        PRPHN#
      *
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMNAOB        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMNAOB
      *
      * Clear array then Check for Invalid Hex characters
     C     EMIADR        IFGT      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     EMIADR        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            EMIADR
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  EMNAOB        PRNAOB
     C                   MOVEL(P)  EMFEIN        PRFEIN
     C                   MOVEL(P)  EMIADR        PRIADR
      *
     C                   Z-ADD     ELPFDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        PRPFDT
      *
     C                   Z-ADD     ELPTDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        PRPTDT
      *
      *  Get Earned premium including expense constant
     C                   MOVE      ELCO#         ##CO#             3
     C                   MOVE      ELFND         ##FND             3
     C                   MOVE      ELFYR         ##FYR             3
     C                   MOVE      ELEMP#        ##EMP#            9
     C                   MOVE      ELDIV         ##DIV             5
      *
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DATE
      *
     C                   CALL      'SRPRT1'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##DATE            8            COMES IN M/D/Y
     C                   PARM      *BLANKS       ##AMT            11            ERN PRM ALL
     C                   PARM      *BLANKS       ##AMT2           11            ERN PRM 1 MTH
     C                   PARM      *BLANK        ##TERM            1            SET LR FLAG.
      *
     C                   MOVE      ##AMT         XXEPWC           11 2
     C                   Z-ADD     XXEPWC        PREPWC
      *
      *  Get Earned premium excluding expense constant
      *
     C                   MOVE      ELCO#         ##CO#             3
     C                   MOVE      ELFND         ##FND             3
     C                   MOVE      ELFYR         ##FYR             3
     C                   MOVE      ELEMP#        ##EMP#            9
     C                   MOVE      ELDIV         ##DIV             5
      *
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DATE
      *
     C                   CALL      'SRPRTD'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM                    ##DATE            8            COMES IN M/D/Y
     C                   PARM      *BLANKS       ##AMT            11            ERN PRM ALL
     C                   PARM      *BLANKS       ##AMT2           11            ERN PRM 1 MTH
     C                   PARM      *BLANK        ##TERM            1            SET LR FLAG.
      *
     C                   MOVE      ##AMT         XXEPXC           11 2
     C                   Z-ADD     XXEPXC        PREPXC
      *
      ***** BEG ADD ***** 02/20/06 **************************************
      * Get the value for two fields from earned premium file if exists
      *
     C     CLTID         IFEQ      '135'
     C     RPKEY         CHAIN     PSQ01                              80
     C     *IN(80)       IFEQ      *OFF
     C     SQMAPR        ANDNE     *ZEROS
     C                   Z-ADD     SQWPRM        PRTTPR
     C                   Z-ADD     SQMAPR        PREPXC
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 02/20/06 **************************************
      *
      *  Get initial transaction date
      *
     C     EHKY          CHAIN     PEH01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     EHTRDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        PRTRDT
      *
     C                   ENDIF                                                  *IN60
      *
      **** BEGIN ADD *** 05/15/00 ************************************
      *      As of date
      ***** BEG DEL ******* 03/21/01 **********************************
     C*************        MOVEL##DATE    PRAODT    P
      ***** END DEL ******* 03/21/01 **********************************
      ***** BEG ADD ******* 03/21/01 **********************************
      *
     C                   Z-ADD     XXRNDT        XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        PRAODT
      *
      ***** END ADD ******* 03/21/01 **********************************
      ***** BEG ADD ******* 03/25/08 **********************************
      *
      *  Get division Status
      *
     C                   EXSR      SRDSTS
      *
      ***** END ADD ******* 03/25/08 **********************************
      **** END ADD ***** 05/15/00 ************************************
      *
      *  Write to output file
      *
     C                   WRITE     PPR01
     C                   ENDSR                                                  SROUT
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *         The Alpha field is loaded into the arrary w/MOVEA
      *         then Loop thru array & replace w/blank if out of
      *         is not a valid HEX Character. After exiting subr
      *         the array is moved back to the alpha field w/MOVEA
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        50            K                 3 0
      *
     C     HX(K)         IFLT      X'40'
     C     HX(K)         OREQ      X'41'
     C     HX(K)         ORGT      X'F9'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SRHEX
      *================================================================
      ***** END ADD ***** 02/27/04 ************************************
      ***** BEG ADD ******* 03/25/08 **********************************
      *
      *
      *================================================================
      * SRDSTS - Get status of Division
      *          'P'ending, 'I'nforce, 'D'eleted, 'C'ancelled,
      *          'E'xpired
      *================================================================
     C     SRDSTS        BEGSR
      *
     C                   MOVE      *BLANK        PRSTCD
     C                   MOVE      *BLANKS       PRCAD
      *
      *  Get status of current division
      *
     C                   EXSR      SRDIVS
     C                   MOVE(P)   XXSTCD        PRSTCD
     C                   MOVE(P)   XXCAD         PRCAD
      *
      *  If division is cancelled; But, state reporting division is not
      *  cancelled; then change division status to 'D'eleted.
      *
     C     PRSTCD        IFEQ      'C'
     C                   MOVE      ELFYR         SVFYR
     C     EMKY1         CHAIN     PEM01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C                   LEAVE
     C                   ENDIF
      *
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO                                                  EMKY1
      *
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDEQ     'Y'
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   EXSR      SRDIVS
      *
     C     XXCAD         IFNE      *BLANKS
     C     XXCAD         ANDNE     PRCAD
     C                   MOVE      'D'           PRSTCD
     C                   ENDIF                                                  XXSTCD
      *
     C                   ENDIF                                                  ELKY
      *
     C                   ENDIF                                                  EMSTRP = 'Y'
      *
      * Reset file pointers
      *
     C     ELKY1         CHAIN     PEL01                              60
     C     EMKY          CHAIN     PEM01                              60
     C                   ENDIF                                                  PRSTCD = 'C'
      *
     C                   ENDSR                                                  SRDSTS
      *
      *
      *================================================================
      * SRDIVS - Division CX date and Status select
      *================================================================
     C     SRDIVS        BEGSR
      *
     C                   MOVE(P)   *BLANK        XXSTCD            1            Status Code
     C                   MOVE(P)   *BLANKS       XXCAD            10            CX date
      *
      *  Get Employer Effective/Cancellation date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C                   Z-ADD     CD(X)         XXCXDT
      *
     C     XXCXDT        IFNE      *ZERO
     C                   Z-ADD     CD(X)         XXDATE
     C                   EXSR      GETDAT
     C                   MOVEL(P)  X2DATE        XXCAD
     C                   ENDIF                                                  XXCXDT
      *
      * Determine Division Status
      *
     C                   SELECT
      *    Pending
     C     XXRNDT        WHENLT    ELPFDT
      ****** BEG ADD ***** 02/13/09   *********************************
     C     XXCXDT        ANDEQ     0
     C     XXRNDT        ORLT      ELPFDT
     C     XXCXDT        ANDGT     XXRNDT
      ****** END ADD ***** 02/13/09   *********************************
     C                   MOVE      'P'           XXSTCD
     C                   MOVE      *BLANKS       XXCAD
      ****** BEG DEL ***** 02/13/09   *********************************
      *****Inforce
     C********** XXCXDT    WHEQ 0
     C********** XXRNDT    ANDGEELPFDT
     C********** XXRNDT    ANDLTELPTDT
     C**********           MOVE 'I'       XXSTCD
     C**********           MOVE *BLANKS   XXCAD
      ****** END DEL ***** 02/13/09   *********************************
      *    Cancelled
     C     XXCXDT        WHENNE    0
     C     XXCXDT        ANDLE     XXRNDT
      ****** BEG DEL ***** 02/13/09   *********************************
     C********** XXCXDT    ANDGTELPFDT
      ****** END DEL ***** 02/13/09   *********************************
     C     XXCXDT        ANDLE     ELPTDT
     C                   MOVE      'C'           XXSTCD
      *
      ****** BEG DEL ***** 02/13/09   *********************************
     C******     XXCXDT    WHNE 0
     C******     XXCXDT    ANDLEXXRNDT
     C******     XXCXDT    ANDEQELPFDT
     C******     XXCXDT    ANDEQELPTDT
     C******               MOVE 'C'       XXSTCD
      ******
     C******     XXCXDT    WHNE 0
     C******     XXCXDT    ANDLEXXRNDT
     C******     XXCXDT    ANDLTELPFDT
     C******               MOVE 'C'       XXSTCD
      *****Inforce
     C******     XXCXDT    WHNE 0
     C******     XXCXDT    ANDGTXXRNDT
     C******     XXCXDT    ANDLEELPTDT
     C******               MOVE 'I'       XXSTCD
      ******
     C******     XXRNDT    WHLE ELPTDT
     C******               MOVE 'I'       XXSTCD
     C******               MOVE *BLANKS   XXCAD
      ****** END DEL ***** 02/13/09   *********************************
      ****** BEG ADD ***** 02/13/09   *********************************
      *
      *    Inforce
     C     XXRNDT        WHENGE    ELPFDT
     C     XXRNDT        ANDLT     ELPTDT
     C                   MOVE      'I'           XXSTCD
     C                   MOVE      *BLANKS       XXCAD
      ****** END ADD ***** 02/13/09   *********************************
      *    Expired
     C                   OTHER
     C                   MOVE      'E'           XXSTCD
     C                   MOVE      *BLANKS       XXCAD
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  SRDIVS
      *
      ***** END ADD ******* 03/25/08 **********************************
