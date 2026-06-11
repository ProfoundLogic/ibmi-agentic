     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W7031R2 - CHECK REGISTER DEMAND REPORT
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRINTS THE CHECK REGISTER BY VARIOUS DATES **
     H**     SELECTION OR BUILDS A WORK FILE WFCRP AND MOVES THE     **
     H*      DATA TO THE LIBRARY 'PDSRPT' OR DOES BOTH PROCESSES.    **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7031C2                                                 **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:48                                          **
     H**   CREATED:   04/01/96      PROJECT: WC028                   **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/23/20 C:062 Project: WC2749                                *
     H*                Change check# heading to REF #                 *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  01/11/13 C:287 Project: SY266_QRY                            *
     H*                  Add DFTACTGRP and ACTGRP compile options     *
     H*                                                               *
     H* 08/08/11  C:53   PROJECT: 139WQK                              *
     H*                  Added retrieval of Source invoice #          *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                               *
     H*  01/03/07  C:53   Support: WC045/53227
     H*                   Add additionl links to WDRPP.
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Use PARQDT instead of PAPODT if PARQDT not zero*
     H*                                                               *
     H*  04/06/04  C:58   PROJECT: WC1148
     H*     Add Group/Non-group/Both selection parameter & logic.
     H*
     H* 05/22/03 C:15 Support: WC065/31676                            *
     H*               Change access to WDRPP.                         *
     H*                                                               *
     H*  02/14/02 C:15  Support: WC025/31250                          *
     H*                 Added code to prevent duplicate records from  *
     H*                 printing.                                     *
     H*                                                               *
     H*  06/26/01 C:15  Project:  WC577                               *
     H*                 Added code to prevent test groups from being  *
     H*                 processed if all groups are selected.         *
     H*                                                               *
     H*  05/16/01 C:15  Project:  Per JJ                              *
     H*                 Added code to load the dates of services in   *
     H*                 the workfile.                                 *
     H*                                                               *
     H*  05/19/00 C:15  Support: WC034/20089                          *
     H*                 Added the TRDT to key list for WDRPP.         *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                 IF AVAILABLE.                                 *
     H*                                                               *
     H*  02/16/00 C:61 PROJECT WC577                                  *
     H*                Restrict access upon a combination             *
     H*                of user parm flags and Group                   *
     H*                restrictions.                                  *
     H*                                                               *
     H* 09/23/99 C:61  PROJECT 82WCG                                  *
     H*                Write employer number, div, name to WFCRP.     *
     H*                                                               *
     H*  04/07/99 C:15 Support: WC090/17156                           *
     H*                Removed PATRDT from key list used to retrieve  *
     H*                records from WDRPP due to incorrect date being *
     H*                loaded into RPTRDT.                            *
     H*                                                               *
     H* 02/26/99  C:118  CHANGED 'FND' TO 'GRP'
     H*
     H* 04/28/98  C:33  SEQ#: WA078/11317                             *
     H*                 CHANGE PARMS FOR DATE CONVERSIONS (FROM       *
     H*                 MMDDYYYY TO MMDDYY)                           *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     08   -      BATCH MOD                                     *
     H*     09   -      DEMAND MOD                                    *
     H*     15   -      TABLES LOKUP                                  *
     H*     25   -      PRINTED ONLY                                  *
     H*     26   -      FILE ONLY                                     *
     H*     27   -      PRINTED AND TO FILE                           *
     H*     28   -      PRINT ROUTING PAGE ONLY                       *
     H*     30   -      PRINTS FUND YEAR TOTALS                       *
     H*     31   -      PRINTS FUND TOTALS                            *
     H*     32   -      PRINTS FUND YEAR HEADING, IF FUND CHGS BUT    *
     H*                 FUND YEAR DOES NOT                            *
     H*     33   -  34  ARRAYS ACCUMULATION                           *
     H*     35          USER RESTRICTION                              *
     H*     36          USER RESTRICTED FROM DATA                     *
     H*     50   -      LOAD PMT TO ARRAY                             *
     H*     60   -      GENERAL CHAINING                              *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H** FILE USAGE:                                                 **
     H**                                                             **
     H**    WFCRP  (PCR01):                                          **
     H**    SMCOL  (PCO01): CO#                                      **
     H**    SMORL  (POR01): ORPNAM,ORFIL#                            **
     H**    WMUPL  (PUP01): USER                                     **
     H**    WMUPL1 (PUP01): STAF                                     **
     H**    WMFNL  (PFN01): CO#/FND                                  **
     H**    WTABL  (PRT01): CO#/KEY                                  **
     H**    WDPAL9 (PPA01): CO#/TRDT/CHK#                            **
     H**    WDPAL7 (PPA02): CO#/FND/TRDT/CHK#/SEQ#                   **
     H**    WDRPL  (PRP01): CO#/FND/FYR/CASE/CHK#/TRDT               **
     H**    WMPDL  (PPD01): FEIN/MOD                                 **
     H**    WMCDL  (PCD01): CDSS#                                    **
     H**    WMCML1 (PCM01): CO#/FND/FYR/CASE                         **
     H**                                                             **
     H*****************************************************************
     F*
     F* Check Register Workfile
     FWFCRP     O  A E           K DISK
     F*
     F* Company File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameters File
     FWMUPL     IF   E           K DISK
     F*
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Output Routing File
     FSMORL     IF   E           K DISK
     F*
     F* Claims Checks Payment File
     F*** Begin dlt *** 06/26/01 **************************************
     F*****WDPAL9  IF  E           K        DISK
     F*** End dlt ***** 06/26/01 **************************************
     F*
     FWDPAL7    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     F*** BEG ADD *** 01/03/07 **************************************
      *  Payment Detail Logical  (CO#/FND/CHK#)
     FWDPAL4    IF   E           K DISK
     F                                     RENAME(PPA01:DBRCDF4)
     F*** END ADD *** 01/03/07 **************************************
     F*
     F* Payee Demographics File
     FWMPDL     IF   E           K DISK
     F*
     F* Claimant Demographics File
     FWMCDL     IF   E           K DISK
     F*
     F*** BEG DEL *** 01/03/07 **************************************
     F* Check View File
     F****WDRPL   IF  E           K        DISK
     F*** END DEL *** 01/03/07 **************************************
     F*** Begin add *** 05/22/03 **************************************
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     F*** End add ***** 05/22/03 **************************************
     F*
     F* Claim File
     FWMCML1    IF   E           K DISK
      ***** BEG ADD ******* 09/23/99 **********************************
      *
      * Employer master
     FWMEML     IF   E           K DISK
      ***** END ADD ******* 09/23/99 **********************************
     F*
     F* Table File
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D TYP             S              1    DIM(7)                               Payment Type
     D AMT             S             12  2 DIM(7)                               Pymt Amt FYR
     D AMTT            S             12  2 DIM(7)                               Pymt Amt LR
     D CNT             S              9  0 DIM(7)                               LR count
     D TABC            S              1    DIM(7) CTDATA PERRCD(1)              Pymt Typ Desc
     D TABD            S             20    DIM(7) ALT(TABC)
     D TCD             S              2    DIM(100)                             Payment Code
     D AMC             S             12  2 DIM(100)                             Pymt/Code Amt
     D COM2            S             14    DIM(100)                             Fnd array comb
     D COM3            S             19    DIM(200)                             FYR array comb
     D TABS            S              1    DIM(7) CTDATA PERRCD(1)              TYPE SEQ
     D TABQ            S              1  0 DIM(7) ALT(TABS)
     D FYTP            S              7    DIM(200)                             FYR Type
     D FAMT            S             12  2 DIM(200)                             FYR amount
     D*** END ADD *** 01/03/07 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XCOMB2                 1     14
     D  XXCOD2                 1      2
     D  XXAMT2                 3     14  2
     D                 DS
     D  XCOMB3                 1     19
     D  XXFYT3                 1      7
     D  XXAMT3                 8     19  2
     D                 DS
     D  XXFYTP                 1      7
     D  PAFND                  1      3  0
     D  PAFYR                  4      6  0
     D  PATYPE                 7      7
     D                 DS
     D  XXFYT2                 1      7
     D  X2FND                  1      3  0
     D  X2FYR                  4      6  0
     D  X2TYPE                 7      7
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*** NO AUTHORITY ***')

     I*** BEG ADD *** 01/03/07 **************************************
     IDBRCDF4
     I              PACO#                       P4CO#
     I              PAFND                       P4FND
     I              PAFYR                       P4FYR
     I              PACASE                      P4CASE
     I              PASEQ#                      P4SEQ#
     I              PACHK#                      P4CHK#
     I              PABASQ                      P4BASQ
     I              PATRDT                      P4TRDT
     I              PAPODT                      P4PODT
     I              PAFEIN                      P4FEIN
     I              PAMOD                       P4MOD
     I              PAAMT                       P4AMT
     I              PABR2                       P4BR2
     I              PABR7                       P4BR7
     I              PAFRDT                      P4FRDT
     I              PATODT                      P4TODT
     I              PAWKS                       P4WKS
     I              PADAYS                      P4DAYS
     I              PATRCD                      P4TRCD
     I              PATYPE                      P4TYPE
     I              PARECD                      P4RECD
     I              PARETP                      P4RETP
     I              PADESC                      P4DESC
     I              PAADST                      P4ADST
     I              PASTAT                      P4STAT
     I              PAUSER                      P4USER
     I              PAPGM                       P4PGM
     I              PATIME                      P4TIME
     I              PADT                        P4DT
     I              PABUL1                      P4BUL1
     I              PAPGTP                      P4PGTP
     I              PAEOB#                      P4EOB#
     I              PACKTP                      P4CKTP
     I              PAACCT                      P4ACCT
     I              PAOFF#                      P4OFF#
     I              PARCVD                      P4RCVD
     I              PADUE                       P4DUE
     I              PALTCD                      P4LTCD
     I              PADPCD                      P4DPCD
     I              PADPCU                      P4DPCU
     I              PARQST                      P4RQST
     I              PAFNAL                      P4FNAL
     I              PARCDT                      P4RCDT
     I              PARADJ                      P4RADJ
     I              PAEOBS                      P4EOBS
     I              PAXFYR                      P4XFYR
     I              PAXCAS                      P4XCAS
      *** END ADD ****** 05/25/07 **********************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQRCOV            1
     C*** BEGIN ADD ****** 04/06/04 ********************************************
     C                   PARM                    QQCKTY            1
     C*** END ADD ******** 04/06/04 ********************************************
     C                   PARM                    QQOTYP            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMOD             1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C***  BEGIN CHG  ***  04/28/98  *********************************
     C**********           PARM *BLANKS   SRFFMT
     C**********           PARM '*JOB'    SRTFMT
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C***  END   CHG  ***  04/28/98  *********************************
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    PACO#         XXCO#
     C     *LIKE         DEFINE    PAFND         XXFND
     C     *LIKE         DEFINE    PAFYR         XLFYR
     C     *LIKE         DEFINE    PAFYR         SVFYR
     C     *LIKE         DEFINE    PAFND         SVFND
     C     *LIKE         DEFINE    PCKEY         XXKEY
     C     *LIKE         DEFINE    PATRDT        XXSYDT
     C     *LIKE         DEFINE    PATRDT        XRFRDT
     C     *LIKE         DEFINE    PATRDT        XRTODT
     I*** BEG ADD *** 01/03/07 **************************************
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#                         ADD 01/03/07
     C     *LIKE         DEFINE    PASEQ#        XBSEQ#                         ADD 01/03/07
     I*** END ADD *** 01/03/07 **************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXCLEX            3
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR TABLE FILE - PAYMENT CODE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  PAYMENT DETAIL FILE  - WDPAL9
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XRFRDT
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  PAYMENT DETAIL FILE  - WDPAL7
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XRFRDT                         YYMMDD
     C*
     C     PAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C*  CLAIMANT DEMOGRAPHICS
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER
     C*
     C     CMKY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C*** BEG DEL *** 01/03/07 **************************************
     C*  REQUEST PAYMENT DETAIL FILE
     C*
     C*******    RPKY      KLIST
     C*******              KFLD           PACO#
     C*******              KFLD           PAFND
     C*******              KFLD           PAFYR
     C*******              KFLD           PACASE
     C*******              KFLD           PACHK#
     C*** BEGIN ADD *** 05/19/00 **************************************
     C*******              KFLD           PATRDT
     C*** END ADD ***** 05/19/00 **************************************
     C*** BEGIN DLT *** 04/07/99 ***********************************************
     C******************** KFLD           PATRDT
     C*** END DLT ***** 04/07/99 ***********************************************
     C*** Begin add *** 05/22/03 **************************************
     C*
     C*******    R2KY      KLIST
     C*******              KFLD           PACO#
     C*******              KFLD           PAFND
     C*******              KFLD           PAFYR
     C*******              KFLD           PACASE
     C*******              KFLD           PASEQ#
     C*** End add ***** 05/22/03 **************************************
     C*** END DEL *** 01/03/07 **************************************
     C*** BEG ADD *** 01/03/07 **************************************
     C*
     C     R2KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    XXSEQ#
     C*
     C     PAKY4         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PACHK#
     C*** End add ***** 01/03/07 **************************************
      ***** BEG ADD ******* 09/23/99 **********************************
      *
      *  Employer master
      *
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      ***** END ADD ******* 09/23/99 **********************************
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C     XXOTYP        IFEQ      'F'
     C                   MOVE      '1'           *IN28
     C                   ELSE
     C                   MOVE      '0'           *IN28
     C                   END
     C*
     C* * * * * * * * * *
     C*   MAIN LOOP     *
     C* * * * * * * * * *
     C*** Begin add *** 06/26/01 **************************************
     C                   MOVE      '1'           *INOF
     C*
     C     *IN78         IFEQ      *ON                                          All groups selected
     C     COKY          CHAIN     PFN01                              99
     C                   ELSE
     C     FNKY          CHAIN     PFN01                              99
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C     *IN78         IFEQ      *OFF
     C     FNTEST        ORNE      'Y'
     C                   MOVE      FNFND         XXFND
     C*** End add ***** 06/26/01 **************************************
     C*
     C*  READ THE PAYMENT RECORDS
     C*
     C*** Begin dlt *** 06/26/01 **************************************
     C********** XXFND     IFNE *ZERO
     C*** End dlt ***** 06/26/01 **************************************
     C     PAKY2         SETLL     PPA02
     C     PAKY3         READE     PPA02                                  51
     C*** Begin dlt *** 06/26/01 **************************************
     C**********           ELSE
     C********** PAKY      SETLLPPA01
     C********** PAKY1     READEPPA01                    51
     C**********           END
     C*** End dlt ***** 06/26/01 **************************************
     C*
     C* MAKE SURE THAT THE TRANSACTION DATE IS WITHIN THE RANGE
     C*
     C*** Begin add *** 02/14/02 **************************************
     C     *IN51         IFEQ      *OFF
     C*** End add ***** 02/14/02 **************************************
     C     PATRDT        IFGE      XRFRDT
     C     PATRDT        ANDLE     XRTODT
     C                   MOVE      '0'           *IN51
     C                   ELSE
     C                   MOVE      '1'           *IN51
     C                   END
     C*** Begin add *** 02/14/02 **************************************
     C                   END
     C*** End add ***** 02/14/02 **************************************
     C*
     C*** Begin dlt *** 06/26/01 **************************************
     C********** *IN51     IFEQ '1'
     C**N28*****           EXCPTHEAD
     C**N28*****           EXCPTERRMSG
     C**********           GOTO ENDPGM
     C**********           ELSE
     C**N28*****           EXCPTHEAD
     C**N28*****           EXCPTHEADD
     C**********           END
     C*** End dlt ***** 06/26/01 **************************************
     C*
     C     *IN51         DOWEQ     '0'
     C*
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2TRDT            6 0
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PAPODT        SRDTI
     C                   ENDIF
     C**************       Z-ADDPAPODT    SRDTI
      ***** END CHG ***** 12/15/06 ***************************************
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2PODT            6 0
     C*
     C*  INCLUDE RECOVERIES?
     C*
     C     QQRCOV        IFEQ      'N'
     C     PATYPE        ANDEQ     'R'
     C                   GOTO      RECOV
     C                   END
     C*
     C*** BEGIN ADD ****** 04/06/04 ********************************************
     C*  SELECT BY CHECK TYPE
     C*
     C     QQCKTY        IFEQ      'G'
     C     PAPGTP        ANDNE     'G'
     C                   GOTO      RECOV
     C                   END
     C*
     C     QQCKTY        IFEQ      'N'
     C     PAPGTP        ANDEQ     'G'
     C                   GOTO      RECOV
     C                   END
     C*** END ADD ******** 04/06/04 ********************************************
     C*
     C*  RETRIEVE CLAIMANT NAME
     C*
     C     CMKY          CHAIN     PCM01                              60
     C   60              MOVE      *BLANK        CMSS#
      ***** BEG ADD ******* 02/16/00 **********************************
      *
      * Check for Fund Restriction
      *
     C                   SETOFF                                       36
     C     UPKY          CHAIN     WMUPL                              60
      *
     C     ##RCDS        IFEQ      *ON
     C     UPXFND        ANDEQ     'Y'
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C     *IN36         PARM      *OFF          ##RTCD            1
     C                   PARM      *BLANK        ##AUTH            1
     C                   PARM      PAFND         ##FND             3 0
     C                   ENDIF                                                  UPXFND
      *
     C   36              GOTO      RECOV
      *
      ***** END ADD ******* 02/16/00 **********************************
      ***** BEG ADD ******* 09/23/99 **********************************
      *
      *  Get employer number, div, name for output file (WMCRP)
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     CMEMP#        CREMP#
     C                   Z-ADD     CMDIV         CRDIV
     C     EMKY          CHAIN     PEM01                              60
     C  N60              MOVEL(P)  EMNAME        CRENAM
     C   60              MOVEL(P)  *BLANKS       CRENAM
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         CREMP#
     C                   Z-ADD     *ZERO         CRDIV
     C                   MOVEL(P)  *BLANKS       CRENAM
     C                   ENDIF                                                  *IN60
      *
      ***** END ADD ******* 09/23/99 **********************************
     C*
     C     CMSS#         IFNE      *BLANK
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCNAM           30
     C*
     C                   ELSE
     C                   MOVE      *BLANK        XXCNAM
     C                   END                                                    IFEQ *IN60
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      PACO#         ##CO#
     C                   MOVE      PAFND         #@FND
     C                   MOVE      PAFYR         ##FYR
     C                   MOVE      PACASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *BLANKS       XXCNAM
     C                   MOVEL(P)  XXATTX        XXCNAM
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   ELSE
     C                   MOVE      *BLANK        XXCNAM
     C                   END                                                    IFNE BLANK
     C*
     C*  RETRIEVE PAYEE NAME
     C*
     C*** BEG DEL *** 01/03/07 **************************************
     C*** Begin add *** 05/22/03 **************************************
     C*******    R2KY      CHAINPRP02                60
     C*******    *IN60     IFEQ *ON
     C*** End add ***** 05/22/03 **************************************
     C*******    RPKY      CHAINPRP01                60
     C*** Begin add *** 05/22/03 **************************************
     C*******              ENDIF
     C*** End add ***** 05/22/03 **************************************
     C*** END DEL *** 01/03/07 **************************************
     C*** BEG ADD *** 01/03/07 **************************************
     C                   Z-ADD     PASEQ#        XXSEQ#
     C     R2KY          CHAIN     PRP02                              60
     C  N60RPCHK#        COMP      PACHK#                             6060
     C*
     C     *IN60         IFEQ      *ON
     C     PABASQ        ANDNE     *ZEROS
     C                   Z-ADD     PABASQ        XXSEQ#
     C     R2KY          CHAIN     PRP02                              60
     C  N60RPCHK#        COMP      PACHK#                             6060
     C                   ENDIF
      *
     C     *IN60         IFEQ      *ON
     C*
     C     PACHK#        IFNE      *ZEROS
     C     PABASQ        ANDNE     *ZEROS
     C*
     C*** Setup the sequence # to match
     C                   Z-ADD     PABASQ        XBSEQ#
     C*
     C*** Read payments for FND/CHK#
     C     PAKY4         CHAIN     DBRCDF4                            98
     C     *IN98         DOWEQ     *OFF
     C*
     C*** Match found
     C     XBSEQ#        IFEQ      P4SEQ#
     C                   Z-ADD     P4BASQ        XXSEQ#
     C*
     C*** Try for RP rcd.
     C     R2KY          CHAIN     PRP02                              60
     C  N60RPCHK#        COMP      PACHK#                             6060
     C*
     C*** Not found, reset and look for the next base seq#
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     P4BASQ        XBSEQ#
     C*
     C** If this is the original rcd and still not found,
     C** leave to prevent endless loop.
     C     XBSEQ#        IFEQ      *ZEROS
     C                   LEAVE
     C                   ENDIF
     C*
     C     PAKY4         READE     DBRCDF4                                98
     C                   ITER
     C*** Found RCD, leave
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C*
     C     PAKY4         READE     DBRCDF4                                98
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 01/03/07 **************************************
     C     *IN60         IFEQ      '0'
      **** BEGIN CHG **** 05/20/98 ************************************
     C******************** MOVE RPPNAM    XXNAME 34
     C                   MOVEL     RPPNAM        XXNAME           34
      **** END   CHG **** 05/20/98 ************************************
     C                   ELSE
     C*
     C     PAFEIN        IFNE      0
     C*
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C                   MOVEL     PDFSN         XXFRST           15
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        XXNAME
     C                   ELSE
     C                   MOVEL     XXCNAM        XXNAME
     C                   END
     C                   ELSE
     C                   MOVEL     XXCNAM        XXNAME
     C                   END
     C                   END
     C*
     C*  LOAD NEGATIVE AMOUNT FOR VOIDS, STOPS, AND RECOVERIES.
     C*
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C                   MULT      -1            PAAMT
     C                   END
     C*
     C*  GET RESERVE CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PARECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XXRDES           30
     C                   ELSE
     C                   MOVEL     RTDESC        XXRDES
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C  N28              EXCEPT    HEAD
     C  N28              EXCEPT    HEADD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C*   FIND PAYMENT CODE IN ARRAY AND ACCUMULATE AMOUNTS
     C*
     C                   Z-ADD     1             Y
     C     PATRCD        LOOKUP    TCD(Y)                                 34
     C     *IN34         IFEQ      '1'
     C                   ADD       PAAMT         AMC(Y)
     C                   END
     C*
     C                   Z-ADD     1             V                 1 0
     C     PATYPE        LOOKUP    TABS          TABQ                     33
     C     *IN33         IFEQ      '1'
     C                   MOVE      TABQ          V
     C                   MOVEA     PATYPE        TYP(V)
     C                   ADD       PAAMT         AMT(V)
     C                   ADD       PAAMT         AMTT(V)
     C                   ADD       1             CNT(V)
     C                   END
     C*
     C* CHECK ARRAY FOR TYPE AND ADD THE PAYMENTS
     C*
     C                   Z-ADD     1             X                 3 0
     C     XXFYTP        LOOKUP    FYTP(X)                                60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             Y
     C     *BLANKS       LOOKUP    FYTP(Y)                                65
     C*
     C     *IN65         IFEQ      '1'
     C                   MOVE      XXFYTP        FYTP(Y)
     C                   Z-ADD     PAAMT         FAMT(Y)
     C                   END
     C*
     C                   ELSE
     C                   ADD       PAAMT         FAMT(X)
     C                   END
     C*
     C* GET ADJUSTER NAME
     C*
     C                   MOVE      *BLANK        XXCLEX
     C                   MOVE      CMCLEX        XXCLEX
     C                   MOVE      *BLANK        CRADJ
     C     XXCLEX        CHAIN     PUP02                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSR         XXADJ            10
     C                   ELSE
     C                   MOVEL     CMCLEX        XXADJ            10
     C                   END
     C*
     C* GET TRANSACTION CODE DESCRIPTION
     C*
     C     PATRCD        IFNE      *BLANK
     C                   MOVEL     PATRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              50
     C                   MOVEL     PCDESC        XXTDES           20
     C                   END
     C*
     C*
     C* MOVE ALL FIELDS FOR THE WRITE IF OUTPUT TYPE = 'F' OR 'B'
     C*
     C     XXOTYP        IFEQ      'F'
     C     XXOTYP        OREQ      'B'
     C*
     C*** Beg add *** 08/08/11 **************************************
     C                   EXSR      SREOB
     C                   MOVEL(P)  @#SINV        CRSINV
     C*** End add *** 08/08/11 **************************************
     C*
     C                   MOVE      PACO#         CRCO#
     C                   Z-ADD     PATRDT        CRTRDT
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   Z-ADD     PARQDT        CRPODT
     C                   ELSE
     C                   Z-ADD     PAPODT        CRPODT
     C                   ENDIF
     C************         Z-ADDPAPODT    CRPODT
      ***** END CHG ***** 12/15/06 ***************************************
     C                   MOVE      PACHK#        CRCHK#
     C                   MOVE      PAAMT         CRAMT
     C                   MOVE      PATRCD        CRTRCD
     C                   MOVE      PATYPE        CRTYPE
     C                   MOVEL     XXTDES        CRTDES
     C                   MOVE      PARECD        CRRECD
     C                   MOVE      PARETP        CRRETP
     C                   MOVE      XXRDES        CRRDES
     C                   MOVEL     XXNAME        CRNAME
     C                   MOVE      PAFEIN        CRFEIN
     C                   MOVE      PAMOD         CRMOD
     C                   MOVE      PAFND         CRFND
     C                   MOVEL     XFNAME        CRFNAM
     C                   MOVE      PAFYR         CRFYR
     C                   MOVE      PACASE        CRCASE
     C                   MOVEL     XXCNAM        CRCNAM
     C                   MOVEL     XXADJ         CRADJ
     C                   MOVEL     Q1USER        CRUSER
     C                   MOVE      XXSYDT        CRDT
     C                   MOVE      DSTIME        CRTIME
     C                   MOVE      PAPGTP        CRPGTP
     C*** Begin add *** 05/16/01 **************************************
     C                   Z-ADD     PAFRDT        CRFRDT
     C                   Z-ADD     PATODT        CRTODT
     C*** End add ***** 05/16/01 **************************************
     C*
     C                   WRITE     PCR01
     C                   END                                                    IF= F / B
     C*
     C*** Begin add *** 06/26/01 **************************************
     C                   MOVE      '1'           *IN98
     C*** End add ***** 06/26/01 **************************************
     C*
     C  N28              EXCEPT    DETAIL
     C*
     C     RECOV         TAG
     C*
     C*** Begin dlt *** 06/26/01 **************************************
     C********** XXFND     IFNE *ZERO
     C*** End dlt ***** 06/26/01 **************************************
     C     PAKY3         READE     PPA02                                  51
     C*** Begin dlt *** 06/26/01 **************************************
     C**********           ELSE
     C********** PAKY1     READEPPA01                    51
     C**********           END
     C*** End dlt ***** 06/26/01 **************************************
     C*
     C* CHECK IF TRANSACTION DATE IS STILL WITH THE RANGE
     C*
     C     PATRDT        IFGT      XRTODT                                       YYMMDD
     C                   MOVE      '1'           *IN51
     C                   END
     C*
     C                   END                                                    DOWEQ '0'
     C*
     C*** Begin add *** 06/26/01 **************************************
     C                   ENDIF
     C*
     C     *IN78         IFEQ      *ON                                          All groups selected
     C     COKY          READE     PFN01                                  99
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*** End add ***** 06/26/01 **************************************
     C*****************************************************************
     C*
     C*** Begin add *** 06/26/01 **************************************
     C     *IN98         IFEQ      '0'
     C  N28              EXCEPT    HEAD
     C  N28              EXCEPT    ERRMSG
     C                   GOTO      ENDPGM
     C                   END
     C*** End add ***** 06/26/01 **************************************
     C*
     C* CHECK OUTPUT TYPE = 'P' OR 'B'
     C*
     C     XXOTYP        IFEQ      'P'                                          PRINTED
     C     XXOTYP        OREQ      'B'                                          PRINTED/FILE
     C*
     C* PRINT SUMMARY BY FND/FYR/PAYMENT TYPE
     C*
     C                   Z-ADD     *ZEROS        SVFND
     C                   Z-ADD     *ZEROS        SVFYR
     C*
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN31
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C* SORT THE ARRAY FOR FUND/FYR/PAYMENT TYPE
     C*
     C                   Z-ADD     1             Y
     C     Y             DOWLT     201
     C     FYTP(Y)       ANDNE     *BLANK
     C                   MOVEL     FYTP(Y)       XXFYT3
     C                   MOVE      FAMT(Y)       XXAMT3
     C                   MOVEA     XCOMB3        COM3(Y)
     C                   ADD       1             Y
     C                   END
     C*
     C                   SORTA     COM3
     C*
     C                   Z-ADD     1             X
     C*
     C     X             DOWLE     200
     C*
     C                   MOVE      COM3(X)       XCOMB3
     C*
     C     XXFYT3        IFNE      *BLANKS
     C                   MOVE      XXFYT3        XXFYT2
     C* * * * *
     C* FUND  *
     C* * * * *
     C     X2FND         IFNE      SVFND
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* PRINT PREVIOUS FUND YEAR AND FUND TOTALS
     C*
     C  N30              EXCEPT    TOTYT
     C                   MOVE      '1'           *IN30
     C                   MOVE      '1'           *IN32
     C  N31              EXCEPT    TOTF
     C                   MOVE      SVFND         S2FND             3 0
     C                   MOVE      X2FND         SVFND
     C                   MOVE      '0'           *IN31
     C  N31              Z-ADD     *ZEROS        AMTF             12 2
     C*
     C* PRINT NEW FUND HEADINGS
     C*
     C                   EXCEPT    HEADF
     C                   END                                                    IFNE SVFND
     C* * * * * * *
     C* FUND YEAR *
     C* * * * * * *
     C     X2FYR         IFNE      SVFYR
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* PRINT PREVIOUS FUND YEAR TOTALS
     C*
     C  N30              EXCEPT    TOTYT
     C  N30              Z-ADD     *ZEROS        AMTY             12 2
     C*
     C* PRINT NEW FUND YEAR HEADINGS
     C*
     C                   MOVE      X2FYR         SVFYR
     C                   EXCEPT    TOTY1
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN32
     C                   END
     C*
     C* IF FUND HAS CHANGED BUT FUND YEAR REMAINS THE SAME PRINT
     C* THE FUND YEAR INFO ONCE  -  S2FND = THE PREVIOUS FUND
     C*
     C     *IN32         IFEQ      '1'
     C     SVFND         IFNE      S2FND
     C     X2FYR         ANDEQ     SVFYR
     C                   EXCEPT    TOTY1
     C                   MOVE      '0'           *IN30
     C                   MOVE      '0'           *IN32
     C                   END
     C                   END
     C*
     C     X2TYPE        LOOKUP    TABC          TABD                     60
     C*
     C     *IN60         IFEQ      '1'
     C                   ADD       XXAMT3        AMTY             12 2          FUND YR AMT
     C                   ADD       XXAMT3        AMTF             12 2          FUND AMT
     C                   MOVEL     TABD          X2DESC           20
     C                   EXCEPT    TOTY
     C                   END                                                    IFNE SVFYR
     C*
     C                   END                                                    IFNE*BLANKS
     C*
     C                   ADD       1             X
     C*
     C                   END                                                    DOWLE 200
     C*
     C                   EXCEPT    TOTYT                                        PRT FYR TOT
     C                   EXCEPT    TOTF                                         PRT FND TOT
     C*
     C* PRINT TOTALS BY PAYMENT TRANSACTION CODE
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   XFOOT     AMC           AMCT             12 2
     C                   Z-ADD     1             Y
     C     Y             DOWLT     101
     C     TCD(Y)        ANDNE     *BLANK
     C                   MOVEL     TCD(Y)        XXCOD2
     C                   MOVE      AMC(Y)        XXAMT2
     C                   MOVEA     XCOMB2        COM2(Y)
     C                   ADD       1             Y
     C                   END
     C*
     C                   SORTA     COM2
     C*
     C                   Z-ADD     1             Y
     C     Y             DOWLT     101
     C                   MOVE      COM2(Y)       XCOMB2
     C     XXCOD2        IFNE      *BLANK
     C                   MOVEL     XXCOD2        XXKEY
     C     TBKY          CHAIN     PPC01                              50
     C                   MOVEL     PCDESC        XXDESC           30
     C*
     C     XXAMT2        IFNE      0
     C                   EXCEPT    TOTC
     C                   END
     C*
     C                   END
     C                   ADD       1             Y
     C                   END
     C*
     C                   EXCEPT    TOT2
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   XFOOT     AMTT          AMTG             12 2
     C                   XFOOT     CNT           CNTG              9 0
     C*
     C                   Z-ADD     1             X
     C     X             DOWLT     08
     C     TYP(X)        IFNE      *BLANK
     C     TYP(X)        LOOKUP    TABC          TABD                     15
     C                   EXCEPT    TOTG
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   EXCEPT    TOT3
     C*
     C                   END                                                    IF = P / B
     C*
     C     ENDPGM        TAG
      /Free
       // ***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     QQMOD         IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQFRDT        X1FRDT            8 0
     C*
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   MOVEL     QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   MOVE      QQOTYP        XXOTYP            1
     C*
     C     XXOTYP        IFEQ      'P'                                          PRINTED ONLY
     C                   MOVE      '1'           *IN25
     C                   END
     C*
     C     XXOTYP        IFEQ      'F'                                          FILE ONLY
     C                   MOVE      '1'           *IN26
     C                   END
     C*
     C     XXOTYP        IFEQ      'B'                                          PRINTED/FILE
     C                   MOVE      '1'           *IN27                             (BOTH)
     C                   END
     C*
     C     XXFND         COMP      *ZERO                                  78    ALL FNDS
     C     QQRCOV        COMP      'Y'                                    79    RECOVERIES
     C*
     C     UPKY          CHAIN     WMUPL                              60
      ***** BEG ADD ******* 02/16/00 **********************************
      *
     C     UPXFND        IFEQ      'Y'
      *
      * Check for existence of Fund/Emplr restriction records for this
      *   user.  SMAZP
      *
     C                   CALL      'SA008R'
     C                   PARM                    UPCO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
     C                   PARM      *OFF          ##RCDS            1
     C                   PARM      *BLANK        ##AUTH            1
      *
     C                   SETON                                        35
      *
     C                   ENDIF                                                  UPXFND
      *
      ***** END ADD ******* 02/16/00 **********************************
     C*
     C     COKY          CHAIN     SMCOL                              60
     C***** BEGIN DEL **** 04/10/00 ***********************************
     C***  BEGIN ADD  ***  04/28/98  *********************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C**********           MOVELCONAME    ##DATA    P
     C*
     C**********           CALL 'SRCENTER'             99
     C**********           PARM           ##DATA
     C**********           PARM 40        ##LENG
     C*
     C**********           MOVEL##DATA    CONAME    P
     C***  END   ADD  ***  04/28/98  *********************************
     C***** END   DEL **** 04/10/00 ***********************************
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSYDT
     C*
     C*  GET FUND DESCRIPTION
     C*
     C     *IN78         IFEQ      '0'
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           55
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME
     C                   END
     C                   END
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C***  BEGIN ADD  ***  04/28/98  *********************************
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C***  END   ADD  ***  04/28/98  *********************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ***********************************
     C******             EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ***********************************
     C                   END
     C***  BEGIN ADD  ***  04/28/98  *********************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL(P)  ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        ORTITL
     C***  END   ADD  ***  04/28/98  *********************************
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRFRDT
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XRTODT
     C*
     C* FILL THE ARRAY WITH THE PAYMENT CODES
     C*
     C                   Z-ADD     1             Y                 3 0
     C     *LOVAL        SETLL     PPC01
     C     COKY          CHAIN     PPC01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     PCKEY         TCD(Y)
     C                   ADD       1             Y
     C     COKY          READE     PPC01                                  60
     C                   END
     C*
     C*** Beg add *** 08/08/11 **************************************
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
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C*
     C*** End add *** 08/08/11 **************************************
     C*
     C                   ENDSR
     C*
     C*** Beg add *** 08/08/11 **************************************
     C     SREOB         BEGSR
     C*
     C     ##MCIF        IFEQ      'Y'
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA033R'
     C                   PARM      PACO#         @#CO#             3 0          Company #
     C                   PARM      PAFND         @#WCF#            3 0          WCMS Fund
     C                   PARM      PAFYR         @#FYR             3 0          Policy Year
     C                   PARM      PACASE        @#CASE            7 0          Claim #
     C                   PARM      PAEOB#        @#EOB#            9 0          EOMB #
     C                   PARM      *ZEROS        @#RCN             3 0          RECON #
     C                   PARM      PAFEIN        @#FEIN            9 0          RECON #
     C                   PARM      PAMOD         @#MOD             3 0          RECON #
     C                   PARM      PACHK#        @#CHK#            9 0          CHECK #
     C                   PARM      *ZEROS        @#OTHR            9 2          EOMB billing amt
     C                   PARM      *BLANKS       @#AST             2            ADJUDICATION ST
     C                   PARM      *ZEROS        @#INV#            9 0          INVOICE #
     C                   PARM      *ZEROS        @#NINV            9 0          NWS INV#
     C                   PARM      *ZEROS        @#SUB#            5 0          SUBSCRIBER
     C                   PARM      *ZEROS        @#FND             3 0          GROUP
     C                   PARM      *ZEROS        @#RRN             9 0          RRN OF FAEOP
     C                   PARM      *BLANKS       @#RC              1            Return Code
     C                   PARM      *BLANKS       @@ACCT           30            Pat Account#
     C                   PARM      *BLANKS       @@BTYP            1            Bill type
     C                   PARM      *BLANKS       @#SINV           35            Source Inv#
     C*
     C                   ELSE
     C                   CALL      'FA034R'
1    C                   PARM      PACO#         @#CO#             3 0          Company #
2    C                   PARM      PAFND         @#WCF#            3 0          WCMS Fund
3    C                   PARM      PAFYR         @#FYR             3 0          Policy Year
4    C                   PARM      PACASE        @#CASE            7 0          Claim #
5    C                   PARM      PAEOB#        @#EOB#            9 0          EOMB #
6    C                   PARM      PAFEIN        @#FEIN            9 0          FEIN #
7    C                   PARM      PAMOD         @#MOD             3 0          MOD #
8    C                   PARM      PACHK#        @#CHK#            9 0          CHECK #
9    C                   PARM      *ZEROS        @#OTHR            9 2          EOMB billing amt
10   C                   PARM      *BLANKS       @#AST             2            ADJUDICATION ST
11   C                   PARM      *ZEROS        @#INV#            9 0          INVOICE #
12   C                   PARM      *ZEROS        @#NINV            9 0          NWS INV#
13   C                   PARM      *ZEROS        @#SUB#            5 0          SUBSCRIBER
14   C                   PARM      *ZEROS        @#FND             3 0          GROUP
15   C                   PARM      *ZEROS        @#RRN             9 0          RRN OF FAEOP
16   C                   PARM      *BLANKS       @#RC              1            Return Code
17   C                   PARM      *BLANKS       @#SINV           35            Source Inv#
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** End add *** 08/08/11 **************************************
     C*****************************************************************
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 **************************************
     O*********                                   8 'XXXXXXXX'
     O*********                                  20 'XXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXX     XX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  34 'XX      XX'
     O*********                                  45 'XXXXXXXXXX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XXXX    XX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XXXX    XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                  10 'XX      XX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   9 'XXXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX XX   XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   8 'XXXXXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  71 'XX'
     O********* E            ROUTE          1
     O*********                                   4 'XXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX  XX  XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   5 'XXXXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX    XXXX'
     O********* E            ROUTE          1
     O*********                                   6 'XX XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX   XX XX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   7 'XX  XXX'
     O*********                                  22 'XX      XX'
     O*********                                  34 'XX      XX'
     O*********                                  41 'XX'
     O*********                                  52 'XX'
     O*********                                  67 'XX    XXXX'
     O*********                                  79 'XX      XX'
     O********* E            ROUTE          1
     O*********                                   8 'XX   XXX'
     O*********                                  21 'XXXXXXXX'
     O*********                                  33 'XXXXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX    XXXX'
     O*********                                  78 'XXXXXXXX'
     O********* E            ROUTE          3
     O*********                                   9 'XX    XXX'
     O*********                                  20 'XXXXXX'
     O*********                                  32 'XXXXXX'
     O*********                                  41 'XX'
     O*********                                  56 'XXXXXXXXXX'
     O*********                                  67 'XX     XXX'
     O*********                                  77 'XXXXXX'
     O********* E            ROUTE          1
     O*********                                  15 'ROUTE TO:'
     O*********      08      ORDIS1              60
     O*********         09   UPUSNM              67
     O********* E            ROUTE          1
     O*********      08      ORDIS2              60
     O********* E            ROUTE          1
     O*********      08      ORDIS3              60
     O********* E            ROUTE          2
     O*********      08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O********* E            ROUTE          2
     O*********                                  19 'PROGRAM NAME:'
     O*********              ORPNAM              30
     O*********                                  36 'FILE:'
     O*********              ORFIL#              38
     O*********                                  46 'COPIES:'
     O*********              ORCOPY        Z     50
     O*********                                  56 'HOLD:'
     O*********              ORHOLD              61
     O********* E            ROUTE          3
     O*********                                  19 'OUTPUT QUEUE:'
     O*********      08      OROUTQ              30
     O*********         09   QQQUE               30
     O*********                                  37 'FORMS:'
     O*********              ORFORM              48
     O*********                                  56 'SAVE:'
     O*********              ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE       3  2
     O                                           18 'Group:    '
     O                  78                       38 'All Groups'
     O                 N78   XXFND         Z     32
     O                 N78   XFNAME              92
     O          E            ROUTE          2
     O                                           19 'Date Range:'
     O                       XXFRDT        Y     37
     O                                           39 '-'
     O                       XXTODT        Y     48
     C*** BEGIN ADD ****** 04/06/04 ********************************************
     O          E            ROUTE          2
     O                                           19 'Check Type:'
     O                       QQCKTY              30
     C*** END ADD ******** 04/06/04 ********************************************
     O          E            ROUTE          2
     O                                           27 'Include Recoveries:'
     O                     79                    32 'Yes'
     O                    N79                    31 'No'
     O          E            ROUTE          2
     O                                           23 'Type of Output:'
     O               25                          41 'Printed Only'
     O               26                          38 'File Only'
     O               27                          41 'Printed/File'
      ***** BEG ADD ******* 02/16/00 **********************************
      *
     O          E    35      ROUTE       3  1
     O                                           23 '******* NOTE *******   '
      *
     O          E    35      ROUTE          1
     O                                           23 'LISTING MAY REFLECT USE'
     O                                         +000 'R AUTHORIZATION TO GROU'
     O                                         +000 'P RECORDS.'
      *
      ***** END ADD ******* 02/16/00 **********************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             118
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             129
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                              'GROUP:'
     O              N78      XXFND         Z   +  1
     O              N78      XFNAME            +  1
     O               78                          18 'ALL GROUPS'
     O          E            HEAD           2
     O                                           14 'SELECTION FROM'
     O                       XXFRDT        Y     24
     O                                           29 'TO'
     O                       XXTODT        Y     39
     O               79                          61 'INCLUDE RECOVERIES'
     O              N79                          61 'EXCLUDE RECOVERIES'
     O          E            HEADD          1
     O                                           55 'TRANSACTION'
     O                                           65 'RESERVE'
     O                                          198 'CHECK TYPE'
     O          E            HEADD          2
     O                                            8 'TRAN DT'
     O                                           18 'POST DT'
     O*** Begin Chg *** 11/23/20 **************************************
     O**********                                 27 'CHECK #'
     O                                           27 '  REF #'
     O*** End   Chg *** 11/23/20 **************************************
     O                                           41 'AMOUNT'
     O                                           54 'CODE TYPE'
     O                                           66 'CODE TYPE'
     O                                           80 'PAYABLE TO:'
     O                                          118 'FEIN / MOD'
     O*** BEGIN CHG *** 02/26/99 **************************************
     O**********                        123 'FND'
     O                                          123 'GRP'
     O*** END CHG ***** 02/26/99 **************************************
     O                                          128 'YR'
     O                                          137 'CASE#'
     O                                          148 'CLAIMANT'
     O                                          185 'ADJUSTER'
     O                                          197 'GRP/N-GRP'
     O          E            HEAD1          2
     O                                           24 'REPORT SUMMARY TOTALS '
     O                                           46 'BY GROUP/YEAR/PMT TYPE'
     O          E            HEADF          2
     O                                           23 '*** SUMMARY FOR GROUP'
     O                       SVFND         Z   +  1
     O                                         +  1 '***'
     O          E            HEAD2          2
     O                                           24 'REPORT SUMMARY TOTALS BY'
     O                                           45 'PMT TRANSACTION CODE'
     O          E            HEAD2          2
     O                                           19 'TRANSACTION CODE'
     O                                           61 'AMOUNT'
     O          E            HEAD3          2
     O                                           24 'REPORT GRAND TOTALS     '
     O          E            HEAD3          2
     O                                           24 '***** GRAND TOTALS *****'
     O                                           44 'COUNT'
     O                                           61 'AMOUNT'
     O          E            DETAIL      1
     O                       X2TRDT        Y      8
     O                       X2PODT        Y     18
     O                       PACHK#        Z     27
     O                       PAAMT         J     42
     O                       PATRCD              47
     O                       PATYPE              52
     O                       PARECD              60
     O                       PARETP              64
     O                       XXNAME             103
     O                       PAFEIN        Z    114
     O                       PAMOD         Z    118
     O                       PAFND         Z    123
     O                       PAFYR         Z    128
     O                       PACASE        Z    137
     O                       XXCNAM             170
     O                       XXADJ              187
     O                       PAPGTP             194
     O          E            TOTY1          0
     O                                           12 '*** YEAR'
     O                       SVFYR         Z     16
     O                                           20 '***'
     O          E            TOTY           1
     O                       X2TYPE              23
     O                       X2DESC              44
     O                       XXAMT3        JB    65
     O          E            TOTYT       1  2
     O                                           25 'YEAR'
     O                       SVFYR         Z     29
     O                                           36 'TOTAL:'
     O                       AMTY          JB    65
     O          E            TOTF           2
     O                                            8 'GROUP'
     O                       SVFND         Z     12
     O                                           19 'TOTAL:'
     O                       AMTF          JB    65
     O          E            TOTC           1
     O                       XXCOD2         B     7
     O                 N50   XXDESC         B    39
     O                       XXAMT2        JB    65
     O          E            TOT2        1  1
     O                                           26 'TOTAL TRANSACTION AMOUNT'
     O                                           27 ':'
     O                       AMCT          JB    65
     O          E            TOTG           1
     O                       TYP(X)               6
     O                  15   TABD                28
     O                       CNT(X)        ZB    44
     O                       AMTT(X)       JB    65
     O          E            TOT3        1  1
     O                                           14 'TOTAL:'
     O                       CNTG          ZB    44
     O                       AMTG          JB    65
     O**
     O          E            ERRMSG      3
     O                                           28 '*** NO RECORD MEETS'
     O                                           51 'THE SELECTED CRITERIA.'
     O                                           55 '***'
     O**
**  TABLE - PMTS TYPE DESCRIPTION
PCOMPUTER PAYMENTS
MMANUAL PAYMENTS
AADJUSTMENTS
VVOID CHECKS
SSTOP PAYMENTS
ZREVERSALS
RRECOVERIES
**  TABLE - PMTS TYPE SEQUENCE
P1
M2
A3
V4
S5
Z6
R7
