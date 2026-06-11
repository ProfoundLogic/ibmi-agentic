     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W0070R6 - LOG API
     H Bnddir('WTBNDDIR')
     H*
     H*****************************************************************
     H* PROGRAM DESCRIPTION:  THIS PROGRAM DETERMINES WHETHER AN EDI  *
     H*         LOG RECORD SHOULD BE WRITTEN AND IF SO, WHAT TYPE OF  *
     H*         TRANSACTION IT SHOULD BE.  THIS PROGRAM ALSO CREATES  *
     H*         AND COMPLETES THE EDI DIARIES AS NECESSARY.           *
     H*                                                               *
     H*   Called from:  W0070R5      QQACTF = blank                   *
     H*                 W8070R3      QQACTF = B      (daily batch)    *
     H*                 W7265R       QQACTF = C or D (claim transfer) *
     H*                 FIXWMLGP     QQACTF = E      (conversion pgm) *
     H*                 WTCLMDTL     QQACTF = T      (claim detail)   *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  CREATED: 06/18/97  for Version 3 BASE                        *
     H*                    C:58  PROJECT: WC468                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 07/24/23 C:53  SUPPORT 111651
     H*                Issue with Medical control flag.
     H*
     H* 07/14/21 C:079 Project: WC2816
     H*                Add logic for USL&H
     H*
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 07/02/20 C:180 Project: WC2738
     H*                Denied Claim now based on Date Last Worked.
     H*
     H* 03/14/19 C:212 Project: WC2658
     H*                Add processing related to addition of new eff.
     H*                dates for override
     H*
     H* 05/30/18 C:79  Project: WC2515
     H*                Add fields for release 3.1
     H*
     H* 02/20/18 C:53  SUPPORT 63514
     H*                Issue with FROI log records not being created.
     H*
     H* 05/26/16 C:310 SUPPORT 40363
     H*                Modified code to generate 148/04 record on
     H*                claim denial for TN state.
     H*
     H* 06/03/14 C:180 SUPPORT WC146 85708
     H*                Supress denials for NC
     H*
     H* 05/28/14 C:53  SUPPORT WC114 85682
     H*                Qualify new parm
     H*
     H* 03/07/14 C:180 SUPPORT WC045 84966
     H*                Do not allow denial transaction to be created
     H*                when claim is not denied and state is LA.
     H*
     H*                                                               *
     H* 01/31/14  C:180 SUPPORT WC146 83790                           *
     H*                 Create A49-04 instead of 148-04 when a        *
     H*                 JCN has been received.                        *
     H*
     H* 01/15/14  C:53 Project: 147WCJ
     H*                Add field for EDI start date override
     H*                                                               *
     H* 09/19/13  C:53 Project 114WJW
     H*                Reverse AC date to be Entered date
     H*
     H* 08/27/13 C:180 SUPPORT WC025 83104
     H*                Do not apply JCL# and Comp payments
     H*                restrictions to 148-02.
     H*
     H* 07/02/13  C:190 Project 147WBF(Additional changes)            *
     H*                 Add params on the call to W8070R7.            *
     H*
     H* 05/01/13 C:062 SUPPORT WC114 82228
     H*                Prevent rucursive call to W8070R4.
     H*
     H* 04/10/13 C:180 SUPPORT WC114 81418
     H*                CSI only - Allow creation of 148-00 when
     H*                ACL# exists.
     H*
     H* 10/10/12 C:180 SUPPORT WC136 80920                            *
     H*                Allow creation of 148-00 when EDI settings     *
     H*                ALL-CLAIMS = 'Y'.                              *
     H*                                                               *
     H* 05/24/12 C:180 SUPPORT WC025 79769                            *
     H*                Allow creation of 148-04 instead of A49-04     *
     H*                when no comp payments have been made.          *
     H*                This rule does not apply to GIC.               *
     H*                                                               *
     H* 04/05/12  C:255 Project WC2087                                *
     H*                 Add the criteria for writing a log record for *
     H*                 Medical Only Claims.                          *
     H*                                                               *
     H* 03/06/12 C:53  SUPPORT 79426 / 79448                          *
     H*                Qualify some changes for 77163 dealing with    *
     H*                SCL# on CLTID. This is causing issues.         *
     H*                This item was worked under support 79769 dated *
     H*                05/24/12.                                      *
     H*                                                               *
     H*                                                               *
     H* 01/18/12 C:53  Project WC2119                                 *
     H*                -Remove 90 day change functionality            *
     H*                -Correct function processing                   *
     H*                                                               *
     H* 11/17/11 C:180 SUPPORT 77163                                  *
     H*                Per GIC's request, add validation for claim    *
     H*                types.                                         *
     H*                                                               *
     H*                Modify change on 10/24/11 to allow             *
     H*                processing for FL denied claims.               *
     H*                                                               *
     H* 10/25/11 C:180 SUPPORT 78315                                  *
     H*                Check for EDI control status.                  *
     H*                                                               *
     H* 10/24/11 C:180 SUPPORT 78329                                  *
     H*                Move comparison of processing control date     *
     H*                against cmidt and cmacdt at top of pgm and     *
     H*                exit pgm if either cmidt or cmacdt (as         *
     H*                applicable) is prior to processing ctrl date.  *
     H*                                                               *
     H*                                                               *
     H* 10/20/11 C:180 SUPPORT 77163                                  *
     H*                Fix table search for valid MTCs.               *
     H*                                                               *
     H* 08/10/11 C:180 SUPPORT 77163                                  *
     H*                Re-arrange loading of Diary seq numbers.       *
     H*                                                               *
     H* 08/09/11 C:180 SUPPORT 77163                                  *
     H*                Do not create a 148-04 or A49-04 when these    *
     H*                these trans are not allowed.                   *
     H*                                                               *
     H*                Do not write diary records when an EDI trans   *
     H*                was not written.                               *
     H*                                                               *
     H*                                                               *
     H* 07/27/11 C:180 SUPPORT 77163                                  *
     H*                Per JB and client's request... override        *
     H*                EDI control settings for Indiana Medical Only  *
     H*                claims: If set to NOT send FROI, override to   *
     H*                send FROI when the More than 1 day of lost time*
     H*                flag (Designed to be applicabe to Indemnity    *
     H*                claims) is set to 'Y'.                         *
     H*                                                               *
     H* 07/20/11 C:180 SUPPORT 77163                                  *
     H*                Do create 148-04 if JCN exists regardless      *
     H*                of log history.                                *
     H*                                                               *
     H* 07/18/11 C:180 SUPPORT 77163 - Addt'l variation of prv issue  *
     H*                Do create 148-00 if JCN exists regardless      *
     H*                of log history.                                *
     H*                                                               *
     H*                                                               *
     H* 07/15/11 C:180 SUPPORT 77163 - Addt'l issue                   *
     H*                Do not create log rec when Include Med only flag
     H*                = 'N' and claim is medical only AS PER JUDY BOBO
     H*                                                               *
     H* 07/06/11 C:180 SUPPORT 77163                                  *
     H*                Fix dup trx.                                   *
     H*                                                               *
     H* 06/07/11 C:180 SUPPORT 74653                                  *
     H*                Initialize L@FR04 flag to allow creation       *
     H*                of A49-04 when applicable.                     *
     H*                                                               *
     H* 06/03/11 C:180 SUPPORT 74653                                  *
     H*                Do not create a 148-04 when an A49-04 has      *
     H*                been accepted or sent and claim has a JCL#     *
     H*                                                               *
     H* 05/17/11 C:180 SUPPORT 74653                                  *
     H*                Create A49-04 instead of 148-04 when JCL#      *
     H*                already exists.                                *
     H*                                                               *
     H* 05/09/11 C:180 SUPPORT 74653                                  *
     H*                Add new feature to not create 148-00 when      *
     H*                an JCN# has already been assigned and there's  *
     H*                no log history.                                *
     H*                                                               *
     H*                                                               *
     H*                Fix error: Pgm incorrectly adding dup 04 trans.*
     H*                                                               *
     H* 04/06/11 C:180 SUPPORT 75809 (ADDITIONAL ISSUE)               *
     H*                Populate the Darts remark date unconditionally *
     H*                with the system date as per specs of proj 139WL*
     H*                Per meeting with JB & MF, change applicable to *
     H*                all clients.                                   *
     H*                                                               *
     H* 04/05/11 C:062 SUPPORT SEQ76278                               *
     H*                CREATE HEIRARCHY ON CLAIM SELECTION            *
     H*                                                               *
     H* 03/30/11 C:180 SUPPORT 75809 (ADDITIONAL ISSUE)               *
     H*                Include MTC code in diary comment.             *
     H*                                                               *
     H* 03/18/11 C:180 SUPPORT 75809 (ADDITIONAL ISSUE)               *
     H*                Write DARTS remark when 148 00 EC is written.  *
     H*                                                               *
     H* 12/10/10 C:53  SUPPORT: 74653                                 *
     H*                CORRECT ISSUE W/148/00 ON DENIED W/A49 ISSUED  *
     H*                                                               *
     H* 03/24/10 C:232 PROJECT: 139WNN                                *
     H*                MODIFIED FOR EDI EXTRACT AND FTP AUTOMATION.   *
     H*                                                               *
     H* 02/01/10 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 10/09/09 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 08/20/09 C:180 SUPPORT 69335                                  *
     H*                Do not create dup A49-04                       *
     H*                                                               *
     H* 06/09/09 C:220 SUPPORT 67085                                  *
     H*                CHANGE AQ TO AU - REVERSE CHANGES UNDER EDI    *
     H*                RELEASE 3                                      *
     H*                                                               *
     H* 05/29/09 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 05/04/09 C:53  SUPPORT 68309                                  *
     H*                DO NOT DISPLAY SCREEN WHEN IN BATCH MODE.      *
     H*                                                               *
     H* 04/16/09 C:180 SUPPORT 68007 Create a 148/04 when no          *
     H*                prior MTC, claim denied and state of FL        *
     H*                                                               *
     H* 01/14/09 C:180 SUPPORT 66693                                  *
     H*                Do not create a 148/04 when accident date is   *
     H*                prior to the EDI START DATE in the EDI CTL file*
     H*                                                               *
     H* 12/24/08-B C:180  135WLG correction                           *
     H*                If the RAT record exists for the state,        *
     H*                go to EOJ.                                     *
     H*                                                               *
     H* 12/24/08 C:180 SUPPORT 66371                                  *
     H*                For IN denied claims: If no previous MTC       *
     H*                create a 148/04.                               *
     H*                                                               *
     H* 12/11/08 C:53  SUPPORT 66171                                  *
     H*                Compare processing control date against cmacdt *
     H*                instead of cmidt                               *
     H*                                                               *
     H* 12/12/08 C:180 SUPPORT 66002                                  *
     H*                Make changes applicable to state of FL too.    *
     H*                                                               *
     H* 12/05/08 C:180 SUPPORT 66002                                  *
     H*                Create A49 04 instead of 148 04 when 148 00    *
     H*                already exists and A49 04 does not previously  *
     H*                exist.                                         *
     H*                                                               *
     H* 12/05/08 C:180 SUPPORT 66110                                  *
     H*                Do not create a   148 04 when                  *
     H*                148 00 already exists for state of TN          *
     H*                                                               *
     H*                                                               *
     H* 10/07/08 C:189 Project: 139WJT                                *
     H*                Added 'Include Medical Only Claims' flag.      *
     H*                                                               *
     H* 09/25/08 C:53  Support: 65343                                 *
     H*                Correct issueing 148 log records when the claim*
     H*                has not passed control criteria                *
     H*                                                               *
     H* 09/24/08 C:180 SUPPORT ITEMS 65080, 63933                     *
     H* 07/09/08       Do not incorrectly write a '01' rec.           *
     H*                                                               *
     H* 07/02/08 C:183 Project: 135WLG                                *
     H*                If the RAT record exists for the state,        *
     H*                go to EOJ.                                     *
     H*                                                               *
     H* 05/26/08 C:191 Project: WC1584 PRISS# 37                      *
     H*                 EDI Release 3: Stop log record:controverted FL*
     H*                                claims                         *
     H*                                                               *
     H* 02/15/08 C:191 Project: WC1584 PRISS# 37 & 38                 *
     H*                 EDI Release 3: Stop 148/04 for controverted FL*
     H*                                claims                         *
     H*                                                               *
     H* 02/13/08 C:190 Project: WC1589                                *
     H*                Program is changed to only allow Log records   *
     H*                to be written if the MTC code is valid for     *
     H*                the reporting state.                           *
     H*                                                               *
     H* 11/20/07 C:183 Project: 139WID                                *
     H*                Added code to execute SRFROI subroutine if     *
     H*                CMLTFL = 'Y' and changed the logic in SRFROI   *
     H*                subroutine to include Record only claims.      *
     H*                                                               *
     H* 10/19/07 C:191 Project: WC1584                                *
     H*                 EDI Release 3                                 *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3 PROCESSING                       *
     H*                                                               *
     H* 05/16/06 C:15  Project: WC132/52787                           *
     H*                Corrected code, added on 2/17/05, to prevent   *
     H*                erroneous '04' transactions from being created.*
     H*                                                               *
     H* 03/15/05 C:15  Project: 114WJE                                *
     H*                Added code to check the EDI flag at the group  *
     H*                level to determine if EDI should be processed. *
     H*                                                               *
     H* 02/17/05 C:53  Support: WC082/45805                           *
     H*                If an "02" to change comes in and the claim    *
     H*                is denied issue an "04" instead.               *
     H*                                                               *
     H* 09/17/04 C:53  Support: WC121/43772                           *
     H*                Verify deny & controverted control info before *
     H*                writing the log rcd                            *
     H*                                                               *
     H* 03/30/03 C:15  Support: WC072/36593                           *
     H*                Added a 'Cancel' selection to the API prompt.  *
     H*                                                               *
     H* 09/04/02 C:15  Support: WC065/340XX                           *
     H*                Chgd logical used to retrieve claim transfer   *
     H*                information.                                   *
     H*                                                               *
     H* 04/20/01 C:15  Project: WC468                                 *
     H*                Changed pgm to prevent 'FS' and 'AU' type 148  *
     H*                transactions from being resent if they were    *
     H*                sent as the FROI.                              *
     H*                                                               *
     H* 01/04/01 C:15  Support: WC065/25696                           *
     H*                Added code to retrieve the claim record from   *
     H*                WACMP if the claim does not exist in WMCMP.    *
     H*                                                               *
     H* 11/30/00 C:15  Project: DR097                                 *
     H*                 Chg ALL PDS pgms                              *
     H*                 1. Replace write to SDCMP with API DA700R     *
     H*                                                               *
     H* 09/22/00 C:15  Project: WC468                                 *
     H*                Added code to load a unique seq# into LGDSQH.  *
     H*                                                               *
     H* 07/21/00 C:15 Support: WC065/23644                            *
     H*               Chgd pgm to process 'D' type activity so that a *
     H*               'cancel' transaction will be sent to the state  *
     H*               if the claim has been reported previously.      *
     H*                                                               *
     H* 07/20/00 C:15 Project: WC468                                  *
     H*               Load the system date into the log due date if   *
     H*               not creating the initial 148 log record.        *
     H*                                                               *
     H* 07/23/99 C:15 Support: WC025/18955                            *
     H*               Changed pgm to prevent old claims (prior to EDI *
     H*               install) from having a cancel transaction       *
     H*               created.                                        *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 04/28/99 C:15 Project: WC468                                  *
     H*              -Changed pgm to process claims with an initial   *
     H*               entry date prior to the EDI start date.  The    *
     H*               pgm will assume that the FROI has already been  *
     H*               sent.                                           *
     H*              -Changed pgm to use the MTC code 'AU' for        *
     H*               acquired claims.                                *
     H*                                                               *
     H* 02/26/99 C:15 Project: WC468                                  *
     H*               Added trigger logic to determine if controverted*
     H*               claims should be processed.                     *
     H*                                                               *
     H* 10/09/98 C:15 Project:                                        *
     H*               Changed trigger logic to create a log record    *
     H*               for med only claims if CMGT1 = 'Y'.             *
     H*                                                               *
     H* 04/13/98 C:15 Project: WC468                                  *
     H*               Changed trigger logic to use CMGT1 instead of   *
     H*               CMLOST when determining whether lost time clms  *
     H*               should be processed.                            *
     H*                                                               *
     H* 02/23/98 C:85 Project: WC577                                  *
     H*               CHANGED THE FIELD MOVED TO CMSMCD               *
     H*               FROM *BLANKS TO CTSMCD.                         *
     H*                                                               *
     H*  10/30/97 C:58 PROJECT: WC468                                 *
     H*               -Added CO and Fund to W0270R5 parameters.
     H*
     H*  01/06/97 C:58 PROJECT: 81WBP                                 *
     H*              o Changed basis of diary date back to date
     H*                claim received, from accident date.
     H*                                                               *
     H*  08/26/96 C:58 PROJECT: 81WBP                                 *
     H*              o REMOVED CMCVFL NOT IN WMCMP IN BASE.           *
     H*              o JURISDICTION STATE IS NO LONGER PULLED FROM    *
     H*                CLAIM/DARTS CONTROL FILE, IT WILL BE RETRIEVED *
     H*                FROM THE RATE SET CODE TABLE FILE              *
     H*              o USE DATA ELEMENT LOADER/EDITOR (W8070R6/7)     *
     H*                TO DETERMINE LOG RECORD STATUS.                *
     H*              o BASE DIARY DATE CALCULATIONS ON ACCIDENT DATE, *
     H*                RATHER THAN ON DATE CLAIM RECEIVED.            *
     H*              o SUBSTITUTE SYSTEM DATE IF CMACDT IS ZERO.      *
     H*                                                               *
     H*  08/28/96 C:15 PROJECT: 25WDE                                 *
     H*                CHANGED THE DIARY PROCESS TO PREVENT THE       *
     H*                DIARIES FROM BEING REOPENED.  SEE SUPPORT      *
     H*                SEQ# 4952.                                     *
     H*                                                               *
     H*  06/24/96 C:15 PROJECT: 25WDE                                 *
     H*                IF A LOG RECORD INDICATES THAT A TRANSACTION   *
     H*                HAS BEEN EXTRACTED OR SENT ('EX' OR 'SN'),     *
     H*                EXIT THE PROGRAM.                              *
     H*                                                               *
     H*  05/09/96 C:15 PROJECT: 25WDE                                 *
     H*                IF THE LOG RECORD IS A CANCEL TRANSACTION      *
     H*                (LGMTC = '01'), CHANGE TO A HOLD STATUS.       *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:
     H*
     H*      51  Claim selected for EDI FROI
     H*
     H*****************************************************************
     FW0070F6   CF   E             WORKSTN USROPN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWACML     IF   E           K DISK
     F                                     RENAME(PCM01:PACM01)
     FWMFNL     IF   E           K DISK
     FWMLGL     UF A E           K DISK
     FWMCDL     IF   E           K DISK
     FWMIXL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
     FWACTL1    IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     FSDCML     UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWTGIL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
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
     FWMLGP     IF   E             DISK
     F                                     RENAME(PLG01:PLG02)
     FWMLGL4    IF   E           K DISK
     F                                     RENAME(PLG01:PLG04)
     FWDF3L     IF   E           K DISK
     FWDN0L     UF A E           K DISK
     F*
     D*________________________________________________________________
     d/copy sPrMask
     D DN              S            150    DIM(999)
     D PRDS            S             40    DIM(1) CTDATA PERRCD(1)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  DSCO#                  1      3
     D  QXCO#                  1      3  0
     D                 DS
     D  QCCOM2                 1     80
     D  QCDRT3                 1     10
     D  QCUSER                12     61
     D*
     D*     Claim master reserve types, reserves 1 - 10
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D*
     D*     Claim master - amount of payments against reserves 1-10
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D*
     D*     Claim master - amount remaining in reserves 1-10
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     d ##pgm           s             10
     d ##err           s              1
     d W8070R4         s               n
     d indClmDtChk     s               n   inz(*off)
     d*** Beg Add *** 07/14/21 *****************************************
     d strUsLh         s                   Like(IXusLh)
     d isUsLh          s               n   Inz('0')
     d*** End Add *** 07/14/21 *****************************************
     I*________________________________________________________________
     IPCT03
     I              CTDESC                      C3DESC
     I*
     IPCM02
     I              CMKEY                       C2KEY
     C*________________________________________________________________
     C*          SMCOL - Company Master
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*          WMFNL - Fund Master
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C***        WMCML - Claim Master
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***        WMCDL - Claim Demographics File
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C***        WMALL1 - Additional Entity/Location File
     C*
     C     ALKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCTG1
     C*
     C***        WMEML - Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***        WDELL - Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C***        WACTL3 - Claim Transfer Transaction File
     C*
     C     CTKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***        WDACL - Auxillary Compensation Payee File
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C***        WMLGL - EDI Log File (Previous key - Transfers)
     C*
     C     LGPKEY        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X@XFYR
     C                   KFLD                    X@XCAS
     C*
     C***        WMCXL - Claims DARTS Setup Control File
     C*
     C     CXKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C***        WMIXL - EDI Claims Control Maintenance File
     C*
     C     IXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTSET
     C                   KFLD                    XXEDIF
     C*
     C*  KEY LIST FOR THE RATE SET CODE MASTER FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    X1TSET
     C                   KFLD                    X1MTC
     C     GIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID            4
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C     GIKYEX        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBEX            4
     C*  Group EDI info
     C*
     C     keyWDF3L      Klist
     C                   Kfld                    XXCO#
     C                   Kfld                    XXFND
     C                   Kfld                    XJRST2            2
      *
     C*
     C     *LIKE         DEFINE    LGDT          XXDATE
     C     *LIKE         DEFINE    QQFND         XCFND
     C     *LIKE         DEFINE    QQFYR         XCFYR
     C     *LIKE         DEFINE    QQCASE        XCCASE
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCTG1        XXCTG1
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    *IN60         Q@ERR
     C     *LIKE         DEFINE    *IN60         XXDEL
     C     *LIKE         DEFINE    *IN60         L@FROI
     C     *LIKE         DEFINE    *IN60         XXDNY
     C     *LIKE         DEFINE    *IN60         XXENVF
     C     *LIKE         DEFINE    *IN60         X@WCRF
     C     *LIKE         DEFINE    *IN60         XXSKIP
     C     *LIKE         DEFINE    CMCASE        X@XCAS
     C     *LIKE         DEFINE    CMFYR         X@XFYR
     C     *LIKE         DEFINE    LGMTC         XXMTC
     C     *LIKE         DEFINE    LGMTC         SVMTC
     C     *LIKE         DEFINE    LGDSQA        XXDSQA
     C     *LIKE         DEFINE    LGDSQB        XXDSQB
     C     *LIKE         DEFINE    LGDSQC        XXDSQC
     C     *LIKE         DEFINE    LGDSQD        XXDSQD
     C     *LIKE         DEFINE    LGDSQE        XXDSQE
     C     *LIKE         DEFINE    LGDSQF        XXDSQF
     C     *LIKE         DEFINE    LGDSQG        XXDSQG
     C     *LIKE         DEFINE    LGDSQH        XXDSQH
     C     *LIKE         DEFINE    LGDSQI        XXDSQI
     C     *LIKE         DEFINE    LGDSQJ        XXDSQJ
     C     *LIKE         DEFINE    LGDSQK        XXDSQK
     C     *LIKE         DEFINE    LGDSQL        XXDSQL
     C     *LIKE         DEFINE    LGDSQM        XXDSQM
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C     *LIKE         DEFINE    LGMTC         X1MTC
     C     *LIKE         DEFINE    LGTSET        X1TSET
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    C2KEY         XCKEY
     C     *LIKE         DEFINE    IXCSDT        SVCSDT
     C     *LIKE         DEFINE    IXMSDT        SVMSDT
     C     *LIKE         DEFINE    IXRSDT        SVRSDT
     C     *LIKE         DEFINE    IXEDIF        XXEDIF
     C                   MOVE      'N'           XXOCLM            1
     C     *LIKE         DEFINE    *IN60         L@FR04
     C*
     C     *LIKE         DEFINE    RSRTCD        XXRTCD
     C     *LIKE         DEFINE    LGDT          intDate
     C                   MOVE      *ZEROS        XI                5 0
     C                   Z-ADD     *ZEROS        XXMEDT           11 2
     C                   Z-ADD     *ZEROS        XXCOMT           11 2
     C                   Z-ADD     *ZEROS        XXMEDR           11 2
     C                   Z-ADD     *ZEROS        XXCOMR           11 2
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*________________________________________________________________
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQACTF            1
     C                   PARM                    QQMGID            7
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQEDAT            7
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      *BLANKS       Q@STS             2
     C     QQACTF        IFEQ      'B'
     C     QQACTF        OREQ      'E'
     C                   MOVE      *ON           XXENVF
     C                   ELSE
     C                   MOVE      *OFF          XXENVF
     C                   ENDIF
     C*
     C*   OPEN THE DISPLAY FILE IF NOT CALLED FROM THE BATCH PGM (W8070R3).
     C*
     C     XXENVF        IFEQ      *OFF
     C                   OPEN      W0070F6
     C                   ENDIF
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     c*
     c* Check to see if called from W8070R4.
     c*
     c                   call      'SRCHKPGM'                           99
     c                   parm      'W8070R4'     ##pgm            10
     c                   parm      *Blanks       ##err             1
     c*
     c                   eval      W8070R4=*Off
     c                   If        ##err=*on
     c                   eval      W8070R4=*On
     c                   EndIf
     C*** Format Year/Claim Number
     C*
     C                   MOVE      QQFYR         XXYR2             2 0
     C                   MOVE      QQCASE        SCYRCL
     C                   MOVEL     XXYR2         SCYRCL
     C*
     C     TOP           TAG
     C                   MOVE      *BLANKS       SCMSG
     C*
     C*** Retrieve Company Name
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*
     C*** Retrieve Fund Information
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      CMFND         SCFND
     C  N60              MOVEL     FNNAME        SCFNME
     C  N60              MOVEL     FNEDIF        XXEDIF
     C     FNEDI         CABEQ     'Y'           TERM
     C*
     C*** Retrieve Claim Master
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C   60CMKEY         CHAIN     PACM01                             60
      /Free
       scSS# = MaskData('SSN':'CMSS#':CMSS#);
      /End-Free
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C     CMLTFL        IFEQ      'I'
     C     CMLTFL        OREQ      'M'
     C     CMLTFL        OREQ      'R'
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF
     C*** Retrieve Claimant Demographics
     C*
     C                   MOVE      ','           COMMA             1
     C                   MOVE      *BLANKS       WRK              50
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ALL'*'       SCCLNM
     C                   ELSE
     C     CDLSN         CAT       COMMA:0       WRK
     C     WRK           CAT       CDFSN:1       WRK
     C     WRK           CAT       CDMID:1       WRK
     C                   MOVEL     WRK           SCCLNM
     C                   ENDIF
     C*
     C*
     C                   MOVEL     CMJRST        XXJRST
     C*
     C*  If no CMJRST, retrieve from employer rate set code.
     C*
     C     XXJRST        IFEQ      *BLANK
     C*
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  GET THE RATE SET CODE MASTER RECORD
     C*
     C                   MOVE      *BLANKS       XXRTCD
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXJRST
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*** Retrieve the EDI Master control record.
     C*
     C                   MOVE      *BLANKS       XXTSET
     C     IXKEY         CHAIN     PIX01                              60
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PIX01
     C                   ENDIF
     C   60              GOTO      TERM
     C                   MOVE      IXCSDT        SVCSDT
       //*** Beg Add *** 07/14/21 ******************************************************
       strUsLh = IXusLh;
       //*** End Add *** 07/14/21 ******************************************************
     C*
     C*   ONLY PROCESS CLAIM IF THE ACCID DATE IS NOT LESS THAN
     C*    EDI Control file - claim start date.
     C*
     C*** Retrieve the EDI RAT control record.
     C*
     C                   MOVE      'RAT'         XXTSET
     C     IXKEY         CHAIN     PIX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   CLEAR                   PIX01
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*** Retrieve the EDI 148 control record.
     C*
     C                   MOVE      '148'         XXTSET
     C     IXKEY         CHAIN     PIX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PIX01
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C/free
          svMSDT = IXmsdt;
          svRSDT = IXrsdt;
      /end-free
     C*
     C*** Determine if the original 148 transmission has been extracted.
     C*
     C                   MOVE      'N'           XXLOG             1
     C                   MOVE      *OFF          L@FROI
     C                   MOVE      *OFF          L@FR04
     C     CMKEY         CHAIN(N)  PLG01                              60
     C                   EXSR      SRPMT
      *
     C                   IF        XXCLT# <> '114'
     C                   SELECT
      *
     C     CMJRST        WHENEQ    'TX'
     C     CMACL#        IFGT      *BLANK
     C     XXCOMT        ANDGT     *ZERO
     C                   MOVE      *ON           L@FROI
     C                   ENDIF
      *
     C                   OTHER
     C     CMACL#        IFGT      *BLANK
     C                   MOVE      *ON           L@FROI
     C                   ENDIF
     C                   ENDSL
      *
     C                   ENDIF

     C     *IN60         DOWEQ     *OFF
     C*
     C     LGJST         IFEQ      XXJRST
     C                   MOVE      'Y'           XXLOG
     C*
     C     LGTSET        IFEQ      '148'
     C     LGSTS         IFNE      'EI'
     C     LGSTS         ANDNE     'EC'
     C     LGSTS         ANDNE     'TR'
     C                   MOVE      *ON           L@FROI
     C                   ENDIF
     C     LGMTC         IFEQ      '04'
     C     LGSTS         ANDNE     'TR'
     C                   MOVE      *ON           L@FR04
     C                   ENDIF
     C     LGSTS         IFEQ      'EX'
     C     LGSTS         OREQ      'SN'
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*
     C     LGTSET        IFEQ      'A49'
     C     LGMTC         IFEQ      '04'
     C     LGSTS         ANDNE     'TR'
     C                   MOVE      *ON           L@FR04
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMKEY         READE(N)  PLG01                                  60
     C                   ENDDO
     C*
     C*  If the claim entry date is less than the EDI start date, seton
     C*    L@FROI to indicate that the FROI has been sent.
     C*
      *
      /Free
        // Check for Group override date
       XJrSt2 = XXJrSt;
       Chain keyWDF3L pf301;
       If Not %Found(WDF3L);
         XJrSt2 = *blanks;
         Chain keyWDF3L pf301;
       EndIf;
       If %Found(WDF3L) and F3ovdt <> 0;
         SVCsDt = F3ovdt;
       EndIf;
        // Check for incoming override date
       If q1prms >= 8;
       If qqedat <> *blanks;
       intDate = %int(qqedat);
       EndIf;
       EndIf;
       If intDate <> 0;
         CMIDT = intDate;
       EndIf;
      /End-Free
      *

     C/free
         // initialize a common indicator to compare claim date with start date
         indClmDtChk = *off;

         Select;

          // check for medical date override
          when CMltfl = 'M' and svmsdt > *zeros;
             if CMidt < svmsdt;
                indClmDtChk = *on;
             endif;

          // check for record only date override
          when CMltfl = 'R' and svrsdt > *zeros;
             if CMidt < svrsdt;
                indClmDtChk = *on;
             endif;

          // check for other scenario
          other;
             if CMidt < svcsdt;
                indClmDtChk = *on;
             endif;

         EndSl;

       //*** Beg Add *** 07/14/21 ***************************************************************
       // Exclude USL&H claims
       If strUsLh = 'Y' and CMusLH = 'Y';
          isUsLh = *on;
       Endif;
       //*** End Add *** 07/14/21 ***************************************************************

      /end-free
       //*** Beg Chg *** 07/14/21 ***************************************************************
     C**********         If        indClmDtChk = *on
       If indClmDtchk = *on Or isUsLh = *on;
       //*** End Chg *** 07/14/21 ***************************************************************
     C                   MOVE      *ON           L@FROI
     C                   MOVE      'Y'           XXOCLM
      *
     C                   SELECT
     C     XXCLT#        WHENEQ    '139'
     C     XXJRST        IFEQ      'FL'
     C     CMACL#        ANDGT     *BLANKS
     C     CMDNY         ANDEQ     'Y'
     C*                          Continue processing
     C                   ENDIF
     C                   OTHER
     C                   GOTO      TERM
     C                   ENDSL
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      PRDS(1)       SCPRDS
     C                   MOVE      *BLANKS       SCCHG
     C                   MOVE      *BLANKS       SCFIX
     C                   MOVE      *BLANKS       SCHLD
     C*
     C*** Determine which screen is to be displayed.
     C*
     C                   MOVE      *BLANK        Q2RECF
     C     QQACTF        IFEQ      *BLANKS
     C     L@FROI        ANDEQ     *ON
     C     XXENVF        ANDEQ     *OFF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ENDIF
     C*
     C*          Display Flags/Message
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*   Process Command Keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*    F3=Exit
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   Command 12 = Cancel
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      'B'           QQRTCD
     C                   GOTO      TERM
     C                   ENDIF
     C*   COMMAND 10 = SEARCH
     C*   Maintenance Type Correction Code Search
     C*
     C     *INKJ         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C     HFLD          IFEQ      'SCMTCC'
     C*
     C                   MOVE      QQFYR         XXYR3             3 0
     C                   CALL      'W0875R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXYR3
     C                   PARM                    XXCASE
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT
      *
     C     ##RRN         CHAIN     PLG02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      LGMTC         SCMTCC
     C                   Z-ADD     LGMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMTC2
     C                   SETON                                        32
     C                   ENDIF
      *
      *
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*   Command 11 = EDI Log History Inquiry
     C*
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXCO#         QQCO#
     C     *INKK         IFEQ      *ON
     C                   CALL      'W0270R5'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCASE
     C                   PARM                    QQFYR
     C                   PARM                    QQFND
     C                   PARM                    QQCO#
     C                   PARM                    QQRTCD
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     Q2RECF        CABEQ     *BLANKS       UPDAT
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS SCREEN 1
     C*
     C     SCR01T        TAG
     C*
     C***   Change Flags
     C*
     C                   SELECT
     C*
     C     SCCHG         WHENEQ    'Y'
     C     SCFIX         ANDEQ     'Y'
     C     SCCHG         OREQ      'Y'
     C     SCHLD         ANDEQ     'Y'
     C     SCFIX         OREQ      'Y'
     C     SCHLD         ANDEQ     'Y'
     C     SCCHG         OREQ      'Y'
     C     SCCNCL        ANDEQ     'Y'
     C     SCFIX         OREQ      'Y'
     C     SCCNCL        ANDEQ     'Y'
     C     SCHLD         OREQ      'Y'
     C     SCCNCL        ANDEQ     'Y'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C*
     C     SCCHG         WHENNE    'Y'
     C     SCCHG         ANDNE     *BLANKS
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C*
     C*
     C     SCFIX         WHENNE    'Y'
     C     SCFIX         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C*
     C     SCCNCL        WHENNE    'Y'
     C     SCCNCL        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C*
     C     SCHLD         WHENNE    'Y'
     C     SCHLD         ANDNE     *BLANKS
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C*
     C     IXRLS         WHENEQ    '3'
     C***        Determine the Maint Type Code
     C*
     C                   MOVE      *BLANKS       XXMTC
     C                   SELECT
     C*
     C     SCFIX         WHENEQ    'Y'
     C                   MOVEL     'CO'          XXMTC
     C*
     C     SCCHG         WHENEQ    'Y'
     C     XXDNY         IFEQ      *ON
     C     L@FR04        ANDEQ     *OFF
     C                   MOVEL     '04'          XXMTC
     C                   ELSE
     C                   MOVEL     '02'          XXMTC
     C                   ENDIF
     C*
     C     XXDEL         WHENEQ    *ON
     C     SCCNCL        OREQ      'Y'
     C                   MOVEL     '01'          XXMTC
     C*
     C     XXDNY         WHENEQ    *ON
     C                   MOVEL     '04'          XXMTC
     C*
     C     CMACQR        WHENEQ    'Y'
     C                   MOVEL     'AU'          XXMTC
     C*
     C     XXMTC         WHENEQ    *BLANKS
     C                   MOVEL     '00'          XXMTC
     C*
     C                   ENDSL
     C*
     C     XXMTC         IFEQ      'CO'
     C*  For IXRLS = '3': If the Maintenance type code = "CO"
     C*    Edit for the existence of an A49 log record for the claim equal to
     C*    the Maintenance Type Correction Code entered on the screen and be in
     C*    a status of "TR" or "TE"
     C*
     C                   MOVE      SCMTCC        X1MTC
     C                   MOVE      'A49'         X1TSET
     C     LGKY          CHAIN(N)  PLG04                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     SCMTCC        IFEQ      LGMTC
     C     LGSTS         ANDEQ     'TR'
     C     SCMTCC        OREQ      LGMTC
     C     LGSTS         ANDEQ     'TE'
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     LGKY          READE(N)  PLG04                                  60
     C                   ENDDO
     C                   ENDIF
      *
     C                   ENDSL
     C*
     C     UPDAT         TAG
     C*
     C     XXENVF        IFEQ      *OFF
     C                   WRITE     SCRW1
     C                   ENDIF
     C                   MOVE      *OFF          Q@ERR
     C*
     C*  IF QQACTF IS NOT BLANK, PROCESSING A CLAIM TRANSFER OR CLAIM
     C*   DELETION.
     C*
     C     QQACTF        IFEQ      'C'
     C     QQACTF        OREQ      'D'
     C*
     C                   EXSR      SRTOD
     C     QQACTF        IFNE      'C'
     C                   EXSR      SAVSEQ
     C                   EXSR      SRLOG
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  IF QQACTF IS BLANK, PROCESSING NORMAL EDI LOG UPDATE.
     C*
     C                   EXSR      SRCM
     C*
     C*
     C***        Determine the Maint Type Code
     C*
     C                   MOVE      *BLANKS       XXMTC
     C                   SELECT
     C*
     C     SCFIX         WHENEQ    'Y'
     C                   MOVEL     'CO'          XXMTC
     C*
     C     SCCHG         WHENEQ    'Y'
     C     XXDNY         IFEQ      *ON
     C     L@FR04        ANDEQ     *OFF
     C                   MOVEL     '04'          XXMTC
     C                   ELSE
     C                   MOVEL     '02'          XXMTC
     C                   ENDIF
     C*
     C     XXDEL         WHENEQ    *ON
     C     SCCNCL        OREQ      'Y'
     C                   MOVEL     '01'          XXMTC
     C*
     C     XXDNY         WHENEQ    *ON
     C                   MOVEL     '04'          XXMTC
     C*
     C     CMACQR        WHENEQ    'Y'
     C                   MOVEL     'AU'          XXMTC
     C*
     C     XXMTC         WHENEQ    *BLANKS
     C     *IN51         ANDEQ     *ON
     C                   MOVEL     '00'          XXMTC
     C*
     C     L@FROI        IFEQ      *ON
     C                   MOVE      *OFF          *IN51
     C                   ENDIF
     C                   ENDSL
     C*
     C     *IN51         CABEQ     *OFF          TERM
     C*
     C                   EXSR      SRSTS
     C     Q@ERR         IFEQ      *OFF
     C     XXSKIP        ANDEQ     *OFF
     C                   EXSR      SAVSEQ
     C                   EXSR      SRLOG
     C                   MOVE      *BLANKS       QQRTCD
     C                   ELSE
     C                   MOVE      'E'           QQRTCD
     C                   END
     C*
     C                   ENDIF
      /Free
                         If xxMtc = '01' and (scCxRc <> *blanks or
                              scCrNa <> *blanks);
                            Exsr srCancel;
                         Endif;
      /End-Free
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C/EJECT
     C*  SRTOD - Process Claim Delete or Transfer.
     C*
     C     SRTOD         BEGSR
     C*
     C***        Initialize work fields
     C*
     C                   MOVE      *OFF          XXDEL
     C                   Z-ADD     *ZEROS        X@XCAS
     C                   Z-ADD     *ZEROS        X@XFYR
     C     CTKEY         CHAIN     PCT01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     CTCASE        XXCASE
     C                   Z-ADD     CTFYR         XXFYR
     C                   Z-ADD     CTXCAS        X@XCAS
     C                   Z-ADD     CTXFYR        X@XFYR
     C                   END
     C*
     C***  If this is a deletion, delete Log records.
     C*
     C     QQACTF        IFEQ      'D'
     C                   MOVE      *ON           XXDEL
     C                   MOVEL     '01'          XXMTC
     C                   MOVEL     'EC'          Q@STS
     C*
     C     X@XCAS        IFEQ      0
     C                   Z-ADD     XXFYR         X@XFYR
     C                   Z-ADD     XXCASE        X@XCAS
     C                   END
     C*
     C     LGPKEY        CHAIN     PLG01                              63
     C     *IN63         DOWEQ     '0'
     C*
     C     LGSTS         IFEQ      'EC'
     C     LGSTS         OREQ      'EI'
     C     LGSTS         OREQ      'EX'
     C                   DELETE    PLG01
     C                   END
     C*
     C     LGPKEY        READE     PLG01                                  63
     C                   ENDDO
     C*
     C                   END
     C*
     C***  If this is a transfer, change the log records.
     C*
     C     QQACTF        IFEQ      'C'
     C*
     C     LGPKEY        CHAIN     PLG01                              63
     C     *IN63         DOWEQ     *OFF
     C                   MOVE      XXFYR         LGFYR
     C                   MOVE      XXCASE        LGCASE
     C                   UPDATE    PLG01
     C     LGPKEY        READE     PLG01                                  63
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
     C*  SRCM - Process Claims Master
     C*
     C     SRCM          BEGSR
     C*
     C***        Initialize work fields
     C*
     C                   MOVE      *OFF          XXDEL
     C                   MOVE      *OFF          XXDNY
     C                   MOVEL     *BLANKS       X@XCAS
     C                   Z-ADD     *ZEROS        X@XFYR
     C*
     C*
     C*     Do EDI/FROI Trigger Processing if claim is activated
     C*
     C*
     C*   If it is a Record Only Claim and the 'Include Record only claim'
     C*   flag is 'Y' OR the flag is not a Record Only Claim - execute
     C*   SRFROI subroutine.
     C*
     C     CMLTFL        IFEQ      'R'
     C     IXRO          ANDEQ     'Y'
     C     CMLTFL        ORNE      'R'
     C                   EXSR      SRFROI
     C                   ENDIF
     C*
      /Free
       Select;
       When XXCLT# = '135';
          If *IN51 and GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw);
             XXDNY = *ON;
          Endif;
       Other;
          If (*IN51 and GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw)) or
             (*IN51 and CMCVFL = 'Y' and CMJRST <> 'LA');
             XXDNY = *ON;
          Endif;
       Endsl;
       If CMJRST = 'NC' and XXDNY = *ON;
          XXDNY = *OFF;
       Endif;
      /End-Free
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '135'
     C* Don't create cancel trans based on criteria below for clt 135
     C                   OTHER
     C     *IN51         IFEQ      *OFF
     C     XXOCLM        ANDEQ     'N'
     C     *IN51         OREQ      *OFF
     C     XXLOG         ANDEQ     'Y'
     C     XXOCLM        ANDEQ     'N'
     C                   MOVE      *ON           XXDEL
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDSR
      /Free
       //-------------------------------------------------------------
       // Subroutine srCancel - Write cancel reason code and narrative
       //-------------------------------------------------------------
       Begsr srCancel;
           Chain (LGco#:LGfnd:LGfYr:LGcase:LGmTc:LGtSet) pn001;

           If %Found(wdn0l);
              N0cxRc = scCxRc;
              N0jrc# = %EditC(LGco#:'X') + %EditC(LGfnd:'X') +
                       %EditC(LGfyr:'X') + %EditC(LGcase:'X');
              N0cRna = scCrNa;
              Update pn001;
           Else;
              N0co#  = LGco#;
              N0fnd  = LGfnd;
              N0fyr  = LGfyr;
              N0case = LGcase;
              N0mTc  = LGmTc;
              N0tSet = LGtSet;
              N0cxRc = scCxRc;
              N0jrc# = %EditC(LGco#:'X') + %EditC(LGfnd:'X') +
                       %EditC(LGfyr:'X') + %EditC(LGcase:'X');
              N0cRna = scCrNa;
              Write pn001;
           Endif;
       Endsr;
      /End-Free
     C*
     C*  SRFROI - Determine whether to do FROI EDI processing and
     C*             create EDI 148 log record; use EDI Claim Control
     C*             submission criteria.  If selected, set *IN51 ON.
     C*
     C     SRFROI        BEGSR
     C*
     C                   MOVE      *OFF          *IN51
     C                   MOVE      *OFF          *IN52
     C                   MOVE      'AL'          WHY               2
     C*
     C* Do not create log record for controverted FL claims
     C                   MOVE      *OFF          XXSKIP
     C     CMJRST        IFEQ      'FL'
     C     CMCVFL        ANDEQ     'Y'
     C                   MOVE      *ON           XXSKIP
     C                   GOTO      ENDFRO
     C                   ENDIF
     C*
     C     IXMO          IFEQ      'N'
     C     CMLTFL        ANDEQ     'M'
     C*** BEG ADD ****** 07/24/23 ************************************
     C     IXMO          OREQ      ' '
     C     CMLTFL        ANDEQ     'M'
     C*** END ADD ****** 07/24/23 ************************************
     C                   SELECT
     C     CMJRST        WHENEQ    'IN'
     C     CMGT1         IFEQ      'Y'
      *  Do not skip, State of IN exception for med only claims.
     C                   ELSE
     C                   MOVE      'MO'          WHY
     C                   MOVE      *ON           XXSKIP
     C                   GOTO      ENDFRO
     C                   ENDIF
     C*** BEG ADD ****** 10/10/12 ************************************
     C     IXEALL        WHENEQ    'Y'
      *  Do not skip, send all
      *
     C                   OTHER
     C                   MOVE      'MO'          WHY
     C                   MOVE      *ON           XXSKIP
     C                   GOTO      ENDFRO
     C                   ENDSL
     C                   ENDIF
        If  GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw);
     C     CMDNY         IFEQ      'Y'
     C* For FL denied claims: Check for A49/IP record. If exists create an
     C*                A49/04 record instead of 148/04
     C                   MOVE      'IP'          X1MTC
     C                   MOVE      'A49'         X1TSET
     C     LGKY          CHAIN(N)  PLG04                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '04'          X1MTC
     C                   MOVE      'A49'         X1TSET
     C     LGKY          CHAIN(N)  PLG04                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          *IN60
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '04'          XXMTC
     C                   MOVEL(P)  '  E2'        XXTBID
     C                   EXSR      SRMTC
     C     XXVMTC        IFEQ      'Y'
     c     W8070R4       ANDEQ     *OFF
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL     CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET            3
     C                   PARM      '04'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C                   PARM      'W0070R6'     QQCPGM           10
     C                   GOTO      ENDFRO
     C     LGKY          READE(N)  PLG04                                  60
     C                   ENDDO
     C                   ENDIF
     C                   ELSE
     C                   GOTO      ENDFRO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* For IN denied claims: If no previous MTC and claim is denied,   an
     C* create a 148/04.
     C*
     C     CMDNY         IFEQ      'Y'
     C* For FL denied claims: If no previous MTC and claim is denied,   an
     C* create a 148/04.
     C     CMACL#        IFEQ      *BLANKS
     C                   MOVE      '00'          X1MTC
     C                   MOVE      '148'         X1TSET
     C     CMKEY         CHAIN     PLG04                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '04'          XXMTC
     C                   MOVEL(P)  '  E1'        XXTBID
     C                   EXSR      SRMTC
     C     XXVMTC        IFEQ      'Y'
     c     W8070R4       ANDEQ     *OFF
     C                   MOVEL     CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      '148'         QQTSET            3
     C                   PARM      '04'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C                   PARM      'W0070R6'     QQCPGM           10
     C                   GOTO      ENDFRO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
        Endif;
      *
      * IF denied claims, create appropriate 04 trans if ACL# exists.
      *
     C                   SELECT
     C     CMJRST        WHENNE    'TX'
        If  GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw) and
            CMacl# > *blanks and L@FR04 = *Off;
     C                   MOVE      '04'          X1MTC
     C                   MOVE      'A49'         X1TSET
     C     LGKY          CHAIN(N)  PLG04                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '04'          XXMTC
     C                   MOVEL(P)  '  E2'        XXTBID
     C                   EXSR      SRMTC
     C     XXVMTC        IFEQ      'Y'
     c     W8070R4       ANDEQ     *OFF
     C                   MOVEL     CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET            3
     C                   PARM      '04'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C                   PARM      'W0070R6'     QQCPGM           10
     C                   GOTO      ENDFRO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     CMJRST        WHENEQ    'TX'
     C                   EXSR      SRPMT
      *
       If  GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw) or CMCVFL = 'Y';
     C     XXCOMT        IFGT      *ZERO
     C     CMACL#        ANDGT     *BLANKS
     C     L@FR04        ANDEQ     *OFF
     C                   MOVE      '04'          X1MTC
     C                   MOVE      'A49'         X1TSET
     C     LGKY          CHAIN(N)  PLG04                              60
     C     *IN60         IFEQ      *ON
     C     XXCOMT        IFGT      *ZERO
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '04'          XXMTC
     C                   MOVEL(P)  '  E2'        XXTBID
     C                   EXSR      SRMTC
     C     XXVMTC        IFEQ      'Y'
     c     W8070R4       ANDEQ     *OFF
     C                   MOVEL     CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET            3
     C                   PARM      '04'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C                   PARM      'W0070R6'     QQCPGM           10
     C                   GOTO      ENDFRO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSL
     C*     Submit FROI for all claims
     C     IXEALL        CABEQ     'Y'           ENDFRO                   51
     C*
     C*     Calculate compensation/medical paid amount if required
     C                   SELECT
     C     IXECMP        WHENEQ    'Y'
     C     IXEMED        OREQ      'Y'
     C                   EXSR      SRPMT
     C                   ENDSL
     C*
     C*   Test for claim conditions of each control flag that is 'Y'
     C                   SELECT
     C*
     C*     EDI FROI if claim is Record Only
     C*
     C     IXRO          WHENEQ    'Y'
     C     CMLTFL        ANDEQ     'R'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'RO'          WHY
     C*
     C*     EDI FROI if MEDICAL Only
      /Free
       When IXmo = 'Y' And  CMltfl = 'M'
        And CMgt2 = 'Y';
        *In51 = *On ;
        WHY = 'MO';

       When IXMO = 'Y' And  CMltfl = 'M'
        And CMgt2 = ' ' And IXmltd = 0;
        *In51 = *On ;
        WHY = 'MO';
      /End-Free
     C*
     C*     EDI FROI for lost time claims more than days specified
     C     IXELT         WHENEQ    'Y'
     C     CMGT1         ANDEQ     'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'LT'          WHY
      /Free
       When IXelt = 'Y' And  CMgt1 = ' '
        And IXeltd = 0;
        *In51 = *On ;
        WHY = 'LT';
      /End-Free
     C*
     C* For TN denied claims: Check for 148/00 record. If exists create an
     C*                A49/04 record instead of 148/04
      *
       If GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw);
      *
     C     CMJRST        IFEQ      'FL'
     C                   MOVE      '00'          X1MTC
     C                   MOVE      '148'         X1TSET
      *
     C     LGKY          CHAIN     PLG04                              52
      *
     C     *IN52         IFEQ      *OFF
     C                   MOVE      '04'          X1MTC
     C                   MOVE      'A49'         X1TSET
     C     LGKY          CHAIN(N)  PLG04                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '04'          XXMTC
     C                   MOVEL(P)  '  E2'        XXTBID
     C                   EXSR      SRMTC
     C     XXVMTC        IFEQ      'Y'
     c     W8070R4       ANDEQ     *OFF
     C                   MOVEL     CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET            3
     C                   PARM      '04'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C                   PARM      'W0070R6'     QQCPGM           10
     C                   GOTO      ENDFRO
     C                   ENDIF
     C                   ELSE
     C                   GOTO      ENDFRO
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDSL
     C*
     C     *IN52         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C*     EDI FROI for compensation payments over threshold,
     C*                  if compensation incurred is greater than zero
     C     IXECMP        WHENEQ    'Y'
     C     XXCOMR        ANDGE     0
     C     XXCOMT        ANDGE     IXECOA
     C                   MOVE      *ON           *IN51
     C                   MOVE      'CM'          WHY
     C*
     C*     EDI FROI for medical payments over threshold
     C*                  if medical incurred is greater than zero
     C     IXEMED        WHENEQ    'Y'
     C     XXMEDR        ANDGE     0
     C     XXMEDT        ANDGE     IXEMEA
     C                   MOVE      *ON           *IN51
     C                   MOVE      'ME'          WHY
     C*
     C*     EDI FROI for death claims
     C     IXEDTH        WHENEQ    'Y'
     C     CMDTH         ANDEQ     'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'DT'          WHY
     C*
     C*     EDI FROI for acquired claims
     C     IXEACQ        WHENEQ    'Y'
     C     CMACQR        ANDEQ     'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'AQ'          WHY
     C*
     C*     EDI FROI for occupational disease
     C     IXEOCD        WHENEQ    'Y'
     C     CMODIS        ANDEQ     'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'OD'          WHY
     C*
     C*     EDI FROI if salary continued
     C     IXESAL        WHENEQ    'Y'
     C     CMFSAL        ANDEQ     'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'SA'          WHY
     C*
     C*     EDI FROI if claim denied
        When GetDenyFlag(IXedny:CMdny:IXdnyDlw:CMdtlw);
          *in51 = *On;
          WHY = 'DY';
     C*
     C*     EDI FROI if claim denied
     C*
     C     IXECVT        WHENEQ    'Y'
     C     CMCVFL        ANDEQ     'Y'
     C                   MOVE      *ON           *IN51
     C                   MOVE      'DY'          WHY
     C*
     C*     EDI FROI if MEDICAL Only
     C     CMACQR        WHENNE    'Y'
     C     CMDTH         ANDNE     'Y'
     C     CMODIS        ANDNE     'Y'
     C     CMFSAL        ANDNE     'Y'
     C     CMDNY         ANDNE     'Y'
     C     CMCVFL        ANDNE     'Y'
     C     IXECMP        ANDNE     'Y'
     C     IXEMED        ANDNE     'Y'
     C                   MOVE      *ON           *IN51
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C                   ENDIF
     C*
     C     ENDFRO        ENDSR
     C*
     C*  SRSTS - CHECK ALL MANDATORY FIELDS TO DETERMINE THE STATUS
     C*          OF THE LOG RECORD.
     C     SRSTS         BEGSR
     C*
     C                   MOVEL     'EC'          Q@STS
     C*
     C*
     C*         Call the data element loader
     C*
     C                   MOVE      DSDATE        QQEXDT
     C                   MOVEL     '148'         QQTSET
     C                   MOVEL     QQCO#         DSCO#
     C                   Z-ADD     QXCO#         Q9CO#             3 0
     C*
     C                   CALL      'W8070R6'
     C                   PARM                    QQEXDT            8
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQTSET            3
     C                   PARM                    XXMTC
     C                   PARM                    XXJRST
     C                   PARM                    DN
     C*
     C     1             DO        100           P                 5 0
     C*
     C                   CLEAR                   QQERRS
     C                   CALL      'W8070R7'
     C                   PARM                    Q9CO#
     C                   PARM                    XXJRST
     C                   PARM                    P
     C                   PARM                    QQTSET
     C                   PARM                    XXMTC
     C                   PARM                    DN
     C                   PARM                    QQERRS           30
     c                   PARM                    QQFND
     c                   PARM                    QQFYR
     c                   PARM                    QQCASE
     C*
     C*
     C                   MOVEL     QQERRS        XXE001            3
     C     XXE001        IFEQ      '001'
     C                   MOVEL     'EI'          Q@STS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*
     C                   ENDSR
     C*  LODSEQ - LOAD DIARY SEQ NUMBERS
     C*
     C     LODSEQ        BEGSR
     C*
     C*
     C                   MOVE      XXDSQA        LGDSQA
     C                   MOVE      XXDSQB        LGDSQB
     C                   MOVE      XXDSQC        LGDSQC
     C                   MOVE      XXDSQD        LGDSQD
     C                   MOVE      XXDSQE        LGDSQE
     C                   MOVE      XXDSQF        LGDSQF
     C                   MOVE      XXDSQG        LGDSQG
     C                   MOVE      XXDSQI        LGDSQI
     C                   MOVE      XXDSQJ        LGDSQJ
     C                   MOVE      XXDSQK        LGDSQK
     C                   MOVE      XXDSQL        LGDSQL
     C                   MOVE      XXDSQM        LGDSQM
      *
     C                   ENDSR
     C/EJECT
     C*  SAVSEQ - Save Diary sequence numbers
     C*
     C     SAVSEQ        BEGSR
      *
      *
     C     CMKEY         CHAIN(N)  PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     LGTSET        IFEQ      '148'
     C                   EXSR      SRSAV
     C                   LEAVE
     C                   ENDIF
     C*
     C     CMKEY         READE(N)  PLG01                                  60
     C                   ENDDO
     C                   ENDSR
     C*
     C*  SRDDRY - ADD OR UPDATE DIARY RECORDS.
     C*
     C     SRDDRY        BEGSR
     C*
     C*
     C*  INITIALIZE THE DARTS KEY
     C*
     C                   MOVE      *BLANKS       XCKEY
     C                   MOVEL     XXCO#         XCKEY
     C                   MOVE      XXFND         XCFND
     C                   MOVE      XXFYR         XCFYR
     C                   MOVE      XXCASE        XCCASE
     C     XCKEY         CAT       XCFND:0       XCKEY
     C     XCKEY         CAT       XCFYR:0       XCKEY
     C     XCKEY         CAT       XCCASE:0      XCKEY
     C*
     C*   IF THE CLAIM FITS THE SUBMISSION CRITERIA AND THE FROI
     C*   HASN'T BEEN SENT, SEND THE APPROPRIATE DIARIES.
     C*
     C     XXDEL         IFNE      *ON
     C     L@FROI        ANDEQ     *OFF
     C                   MOVE      *OFF          X@WCRF
     C*
     C*  INITIALIZE THE DARTS FIELDS
     C*
     C                   MOVEL     XCKEY         C2KEY
     C                   MOVE      *BLANKS       CMCSID
     C                   TIME                    CMTIME
     C                   MOVE      *BLANKS       CMCOMM
     C                   Z-ADD     XXDATE        CMCDAT
     C                   MOVE      *BLANKS       CMCODE
     C                   MOVE      *BLANKS       CMDCID
     C                   MOVEL     'W0070R'      CMPNAM
     C                   Z-ADD     XXDATE        CMTRDT
     C                   TIME                    CMTRTM
     C                   MOVE      *ZEROS        CMSECL
     C                   MOVE      'N'           CMSBYL
     C                   MOVE      'A'           CMSTAT
     C                   MOVEL     Q1PGM         CMPGM
     C                   Z-ADD     1             CMTIME
     C                   Z-ADD     XXDATE        CMDT
     C                   MOVE      Q1USER        CMIUSR
     C                   MOVE      *BLANKS       CMFOLD
     C                   MOVE      *ZEROS        CMDCDT
     C                   MOVE      'Y'           CMDIFL
     C                   MOVE      *BLANKS       CMAPID
     C                   MOVE      *BLANKS       CMIDOC
     C                   MOVE      *BLANKS       CMIQUE
     C                   MOVE      *BLANKS       CMIMGF
     C                   MOVE      *BLANKS       CMIPTH
     C                   MOVE      *ZEROS        CMPIDX
     C                   MOVE      *BLANKS       CMQPTY
     C                   MOVE      *BLANKS       CMDCTP
     C                   MOVEL     Q1PGM         CMIPGM
     C                   MOVE      *BLANKS       CMFL01
     C                   MOVE      *BLANKS       CMFL02
     C                   MOVE      *BLANKS       CMFL03
     C                   MOVE      *BLANKS       CMFL04
     C                   MOVE      *BLANKS       CMFL05
     C                   Z-ADD     XXDATE        CMIDT
     C*
     C*  IF THE 148 DIARIES DON'T EXIST, 1) CREATE THE FROI DIARIES
     C*  IF THE MANDATORY ENTRIES ARE INCOMPLETE, AND 2) CREATE THE
     C*  90-DAY DIARIES.
     C*
     C     1             DO        7             XI
     C*
     C     XI            IFEQ      1
     C     XXDSQA        ANDEQ     0
     C     Q@STS         ANDEQ     'EI'
     C     XI            OREQ      2
     C     XXDSQB        ANDEQ     0
     C     Q@STS         ANDEQ     'EI'
     C     XI            OREQ      3
     C     XXDSQC        ANDEQ     0
     C     Q@STS         ANDEQ     'EI'
     C     XI            OREQ      4
     C     XXDSQD        ANDEQ     0
     C     XI            OREQ      5
     C     XXDSQE        ANDEQ     0
     C     XI            OREQ      6
     C     XXDSQF        ANDEQ     0
     C     XI            OREQ      7
     C     XXDSQL        ANDEQ     0
     C     Q@STS         ANDEQ     'EC'
     C*
     C*
     C     XI            IFEQ      1
     C                   Z-ADD     IXADRC        SRDAYS
     C                   MOVEL     IXADDR        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C     CMCLEX        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*
     C     XI            IFEQ      2
     C                   Z-ADD     IXSDIM        SRDAYS
     C                   MOVEL     IXSDDR        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C     CMSUPR        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*
     C     XI            IFEQ      3
     C                   Z-ADD     IXLRIM        SRDAYS
     C                   MOVEL     IXDDRC        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C     IXLRSC        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*
     C     XI            IFEQ      4
     C                   Z-ADD     IXCAIM        SRDAYS
     C                   MOVEL     IXCARC        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C     CMCLEX        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*
     C     XI            IFEQ      5
     C                   Z-ADD     IXCSIM        SRDAYS
     C                   MOVEL     IXCSRC        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C     CMSUPR        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*
     C     XI            IFEQ      6
     C                   Z-ADD     IXCLIM        SRDAYS
     C                   MOVEL     IXLDDC        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C     IXCLSC        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*
     C*
     C*
     C     XI            IFEQ      7
     C                   Z-ADD     0             SRDAYS
     C                   MOVEL     IXCECR        CMRMCD
     C                   MOVE      *BLANKS       CMUSER
     C                   MOVE      Q1USER        CMUSER
     C     CMCLEX        CHAIN     PUP01                              60
     C  N60              MOVEL     UPUSR         CMUSER
     C                   ENDIF
     C*     Select basis date for computing the remark date.
      *
     C                   Z-ADD     XXDATE        CMCDAT
      *
     C*
     C*
     C     CMRMCD        IFNE      *BLANKS
     C     CMUSER        ANDNE     *BLANKS
     C*
     C*
     C     CMRMCD        CHAIN     PCT03                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CTSMCD        CMSMCD
     C*
     C*
     C                   MOVEL     *BLANKS       CMCOMM
     C     XXMTC         CAT       '-':0         CMCOMM
     C                   CAT       C3DESC:0      CMCOMM
     C*
     C*
     C                   MOVEL     CTDEPG        CMDCID
     C     CMDCID        IFNE      *BLANKS
     C                   MOVEL     C2KEY         CMFOLD
     C                   MOVE      'Y'           CMFL01
     C                   ENDIF
     C     CTDPRG        IFGT      *ZEROS
     C                   MOVE      'Y'           CMFL02
     C                   ENDIF
     C                   MOVE      CTFL01        CMDIFL
     C                   MOVE      CTFL02        CMFL03
     C                   ENDIF
     C*
     C*  GET THE NEXT AVAILABLE SEQ# AND WRITE THE RECORD
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  XCKEY         Q#KEY
     C                   MOVEL(P)  CMCOMM        Q#COMM
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q#CDAT
     C*
     C                   MOVE(P)   CMUSER        Q#USER
     C                   MOVE(P)   CMDIFL        Q#DIFL
     C                   MOVE(P)   CMRMCD        Q#RMCD
     C                   MOVE(P)   CMDCID        Q#DCID
     C                   MOVE(P)   CMPNAM        Q#PNAM
     C                   MOVE(P)   'N'           Q#SBYL
     C                   MOVE(P)   Q1PGM         Q#PGM
     C                   MOVE      CMFOLD        Q#FOLD
     C                   MOVE      Q1PGM         Q#IPGM
     C                   MOVE      CMFL01        Q#FL01
     C                   MOVE      CMFL02        Q#FL02
     C*
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT            8
     C                   PARM                    Q#DCID           12
     C                   PARM                    Q#PNAM           10
     C                   PARM      *ZEROS        Q#SECL            3
     C                   PARM                    Q#USER           10
     C                   PARM                    Q#PGM            10
     C                   PARM                    Q#FOLD           80
     C                   PARM                    Q#DIFL            1
     C                   PARM                    Q#IPGM           10
     C                   PARM                    Q#FL01            1
     C                   PARM                    Q#FL02            1
     C                   PARM                    Q#RMCD            5
     C                   PARM      '00000'       Q#SEQ#            5
     C                   PARM                    Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      *BLANK        Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *ZEROS        Q#PIDX            5
     C                   PARM      *BLANKS       Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*
     C                   MOVE      Q#SEQ#        CMSEQ#
     C*
     C                   MOVE      *ON           X@WCRF
     C*
     C*  UPDATE THE APPROPRIATE LOG DIARY SEQ#
     C*
     C     XI            IFEQ      1
     C                   MOVE      CMSEQ#        XXDSQA
     C                   ENDIF
     C     XI            IFEQ      2
     C                   MOVE      CMSEQ#        XXDSQB
     C                   ENDIF
     C     XI            IFEQ      3
     C                   MOVE      CMSEQ#        XXDSQC
     C                   ENDIF
     C     XI            IFEQ      4
     C                   MOVE      CMSEQ#        XXDSQD
     C                   ENDIF
     C     XI            IFEQ      5
     C                   MOVE      CMSEQ#        XXDSQE
     C                   ENDIF
     C     XI            IFEQ      6
     C                   MOVE      CMSEQ#        XXDSQF
     C                   ENDIF
     C     XI            IFEQ      7
     C                   MOVE      CMSEQ#        XXDSQL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  IF THE 148 DIARIES EXIST AND THE FROI HASN'T BEEN SENT AND
     C*  THE MANDATORY ENTRIES ARE INCOMPLETE REOPEN THE DIARIES.
     C*
     C     XXDEL         IFNE      *ON
     C     L@FROI        ANDEQ     *OFF
     C     X@WCRF        ANDEQ     *OFF
     C*
     C     XCKEY         CHAIN     PCM02                              63
     C     *IN63         DOWEQ     *OFF
     C     CMSEQ#        IFNE      0
     C*
     C     CMSEQ#        IFEQ      XXDSQA
     C     Q@STS         ANDEQ     'EI'
     C     CMSEQ#        OREQ      XXDSQB
     C     Q@STS         ANDEQ     'EI'
     C     CMSEQ#        OREQ      XXDSQC
     C     Q@STS         ANDEQ     'EI'
     C     CMSEQ#        OREQ      XXDSQD
     C     CMSEQ#        OREQ      XXDSQE
     C     CMSEQ#        OREQ      XXDSQF
     C*
     C                   MOVE      *ZEROS        CMDCDT
     C                   UPDATE    PCM02
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C     XCKEY         READE     PCM02                                  63
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*  IF 148 DIARIES EXIST AND ALL ITEMS ARE COMPLETED, COMPLETE THE
     C*  FROI DIARIES.  IF THE PREVIOUS CONDITION IS TRUE AND THIS IS
     C*  THE 90-DAY SUBMISSION, COMPLETE THE 90-DAY DIARIES.  ALSO,
     C*  THESE DIARIES WILL BE COMPLETED IF THE CLAIM IS DELETED OR
     C*  THE CLAIM DOESN'T FIT THE EDI SUBMISSION CRITERIA.
     C*
     C     Q@STS         IFEQ      'EC'
     C     XXDEL         OREQ      *ON
     C*
     C     XCKEY         CHAIN     PCM02                              63
     C     *IN63         DOWEQ     *OFF
     C*
     C     CMSEQ#        IFEQ      XXDSQA
     C     CMSEQ#        OREQ      XXDSQB
     C     CMSEQ#        OREQ      XXDSQC
     C     CMSEQ#        OREQ      XXDSQL
     C                   Z-ADD     XXDATE        CMDCDT
     C                   ENDIF
     C*
     C     XXDEL         IFEQ      *ON
     C     CMSEQ#        IFEQ      XXDSQD
     C     CMSEQ#        OREQ      XXDSQE
     C     CMSEQ#        OREQ      XXDSQF
     C                   Z-ADD     XXDATE        CMDCDT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PCM02
     C*
     C     XCKEY         READE     PCM02                                  63
     C                   ENDDO
     C*
     C                   END
     C*
     C                   ENDSR
     C/EJECT
     C*  SRLOG - ADD OR UPDATE LOG RECORD.
     C*
     C     SRLOG         BEGSR
     C*
     C*
     C*
     C     CMKEY         CHAIN     PLG01                              63
     C     *IN63         DOWEQ     *OFF
     C*
     C     LGTSET        IFEQ      '148'
     C     LGJST         ANDEQ     XXJRST
     C                   MOVE      LGMTC         SVMTC
     C*
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CMKEY         READE     PLG01                                  63
     C                   ENDDO
     C*
     C*
     C*
     C                   Z-ADD     0             LGDUE
     C*
     C*     Select basis date to compute the submission due date.
     C                   SELECT
     C*
     C     XXMTC         WHENNE    '00'
     C     XXMTC         ANDNE     '04'
     C                   Z-ADD     XXDATE        SRDTI
     C*
     C     IXBASE        WHENEQ    'A'
     C     CMACDT        ANDGT     *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C*
     C     IXBASE        WHENEQ    'R'
     C     CMRCVD        ANDGT     *ZERO
     C                   Z-ADD     CMRCVD        SRDTI
     C*
     C     IXBASE        WHENEQ    'D'
     C     CMDBDT        ANDGT     *ZERO
     C                   Z-ADD     CMDBDT        SRDTI
     C*
     C                   OTHER
     C                   Z-ADD     XXDATE        SRDTI
     C                   ENDSL
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     IXSDUE        SRDAYS            5 0
     C                   MOVE      *BLANKS       SRERR             1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C*
     C     SRERR         IFNE      '1'
     C                   Z-ADD     SRDATE        SRDTI
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   ENDIF
     C     XXMTC         IFNE      '00'
     C     XXMTC         ANDNE     '04'
     C                   Z-ADD     DSDATE        SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C*
     C*
     C                   MOVE      Q@STS         LGSTS
     C     SCHLD         IFEQ      'Y'
     C                   MOVE      'H'           LGSTAT
     C                   ENDIF
     C*
     C** THIS SECTION MOVED TO LODSEQ SR
     C**
     C*
     C                   MOVE      SCMTCC        LGMTCC
     C                   Z-ADD     SCMTC2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGMTC2
     C*
     C     *IN63         IFEQ      *OFF
     C*
     C*   ONLY UPDATE THE MAINT TYPE CODE IF IS THE FIRST REPORT,
     C*   A FIX, A CHANGE, OR A CANCEL.
     C*
     C     XXMTC         IFEQ      '01'
     C     XXMTC         OREQ      'CO'
     C     XXMTC         OREQ      '02'
     C     SVMTC         ORNE      'CO'
     C     SVMTC         ANDNE     '02'
     C                   MOVE      XXMTC         LGMTC
     C                   Z-ADD     XXDATE        LGMTCD
     C                   ENDIF
     C*
     C*
     C*   DETERMINE IF THE LOG RECORD SHOULD BE DELETED.
     C*
     C     L@FROI        IFEQ      *OFF
     C     LGMTC         ANDEQ     '01'
     C     SVMTC         OREQ      '01'
     C     LGMTC         ANDEQ     '00'
     C     SVMTC         OREQ      '01'
     C     LGMTC         ANDEQ     '04'
     C     LGMTC         OREQ      '01'
     C     XXOCLM        ANDEQ     'Y'
     C     XXLOG         ANDEQ     'N'
     C     SVMTC         OREQ      '00'
     C     LGMTC         ANDEQ     '04'
     C     IXEALL        ANDEQ     'N'
     C     IXEDNY        ANDEQ     'N'
     C     IXECVT        ANDEQ     'N'
     C                   MOVEL     'I'           LGSTAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        LGUSER
     C                   Z-ADD     XXDATE        LGDT
     C                   TIME                    LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C                   MOVE      LGJST         XXJRST
     C                   MOVE      LGMTC         XXMTC
     C                   MOVEL(P)  '  E1'        XXTBID
     C                   EXSR      SRMTC
     C     LGSTAT        IFEQ      'I'
     C     XXVMTC        OREQ      'N'
     C                   DELETE    PLG01
     C                   ELSE
     C                   EXSR      SRDDRY
     C                   EXSR      LODSEQ
     C                   UPDATE    PLG01
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   EXSR      SRPMT
     C     L@FROI        IFEQ      *ON
     C     XXMTC         ANDEQ     '00'
     C     L@FROI        OREQ      *ON
     C     XXMTC         ANDEQ     '04'
     C     L@FR04        ANDEQ     *ON
     C     CMACL#        ORGT      *BLANK
     C     XXMTC         ANDEQ     '04'
     C     L@FR04        ANDEQ     *ON
     C     XXCOMT        ANDGT     *ZERO
     C     L@FROI        OREQ      *ON
     C     XXMTC         ANDEQ     '04'
     C     XXOCLM        ANDEQ     'Y'
     C     XXMTC         OREQ      '04'
     C     IXEALL        ANDEQ     'N'
     C     IXEDNY        ANDEQ     'N'
     C     IXECVT        ANDEQ     'N'
     C     L@FROI        OREQ      *ON
     C     XXMTC         ANDEQ     'FS'
     C     L@FROI        OREQ      *ON
     C     XXMTC         ANDEQ     'AU'
     C     L@FROI        OREQ      *OFF
     C     XXMTC         ANDEQ     '01'
     C     SVMTC         OREQ      '01'
     C     XXMTC         ANDEQ     '01'
     C     XXMTC         OREQ      '01'
     C     XXOCLM        ANDEQ     'Y'
     C     XXLOG         ANDEQ     'N'
     C     CMACL#        ORGT      *BLANK
     C     XXCOMT        ANDGT     *ZERO
     C     XXMTC         ANDNE     '02'
     C                   GOTO      SKIP
     C                   ENDIF
     C*
     C                   MOVEL(P)  '  E1'        XXTBID
     C                   EXSR      SRMTC
     C     XXVMTC        IFEQ      'Y'
     C                   EXSR      SRDDRY
     C                   EXSR      LODSEQ
     C                   ENDIF
      *
     C                   MOVE      XXCO#         LGCO#
     C                   MOVE      XXFND         LGFND
     C                   MOVE      XXFYR         LGFYR
     C                   MOVE      XXCASE        LGCASE
     C                   MOVE      XXJRST        LGJST
     C                   MOVEL     '148'         LGTSET
     C                   MOVE      XXMTC         LGMTC
     C                   MOVE      *ZEROS        LGEXDT
     C                   MOVE      *ZEROS        LGMTCD
     C                   MOVE      *ZEROS        LGSENT
     C                   MOVE      *ZEROS        LGARCV
     C                   MOVE      *ZEROS        LGAENT
     C                   MOVEL     IXFROI        LGHCPY
     C                   MOVE      *ZEROS        LGSCHP
     C                   MOVE      *ZEROS        LGPRSQ
     C                   MOVEL     Q1USER        LGIUSR
     C                   Z-ADD     XXDATE        LGIDT
     C                   TIME                    LGITM
     C                   MOVEL     Q1PGM         LGIPGM
     C                   MOVE      Q1USER        LGUSER
     C                   Z-ADD     XXDATE        LGDT
     C                   TIME                    LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C*  Call api to retrieve the next available log seq#.
     C*
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   MOVE      *ZEROS        ##SEQ#
     C                   CALL      'WA045R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##SEQ#            5
     C                   PARM                    ##RTCD            1
     C*
     C                   MOVE      ##SEQ#        LGDSQH
     C*
     C*
     C     XXVMTC        IFEQ      'Y'
     C                   WRITE     PLG01
      /Free
                         If xxMtc = '01' and (scCxRc <> *blanks or
                              scCrNa <> *blanks);
                            Exsr srCancel;
                         Endif;
      /End-Free
     C                   ENDIF
     C*
     C     SKIP          TAG
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*  SRSAV - SAVE PREVIOUS LOG RECORD VALUES.
     C*
     C     SRSAV         BEGSR
     C*
     C                   Z-ADD     LGDSQA        XXDSQA
     C                   Z-ADD     LGDSQB        XXDSQB
     C                   Z-ADD     LGDSQC        XXDSQC
     C                   Z-ADD     LGDSQD        XXDSQD
     C                   Z-ADD     LGDSQE        XXDSQE
     C                   Z-ADD     LGDSQF        XXDSQF
     C                   Z-ADD     LGDSQG        XXDSQG
     C                   Z-ADD     LGDSQI        XXDSQI
     C                   Z-ADD     LGDSQJ        XXDSQJ
     C                   Z-ADD     LGDSQK        XXDSQK
     C                   Z-ADD     LGDSQL        XXDSQL
     C                   Z-ADD     LGDSQM        XXDSQM
     C*
     C                   ENDSR
     C*  SRMTC - To check for a valid MTC for the reporting state.
     C*
     C     SRMTC         BEGSR
     C*
     C                   MOVE      'Y'           XXVMTC            1
      *
     C                   MOVEL(P)  '  E1'        XXTBEX
     C                   MOVEL     XXJRST        XXTBEX
     C     GIKYEX        CHAIN     PGI01                              64
     C     *IN64         IFEQ      *ON
     C                   MOVEL(P)  '  E2'        XXTBEX
     C                   MOVEL     XXJRST        XXTBEX
     C     GIKYEX        CHAIN     PGI01                              64
     C                   ENDIF
      *
     C     *IN64         IFEQ      *OFF
      *
     C*   - First, determine if a state table exists
     C                   MOVEL     XXJRST        XXTBID
     C     GIKY          CHAIN     PGI01                              90
     C*
     C*   - If not found, try generic NCCI table
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  XXMTC         XXKEY
     C     GTKY          CHAIN     PGT01                              90
     C*
     C*
     C                   ENDIF
     C     *IN90         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   MOVE      'N'           XXVMTC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*****************************************************************
     C*  SRPMT  -  Calculate claim payments
     C*****************************************************************
     C     SRPMT         BEGSR
     C                   Z-ADD     *ZEROS        XXMEDT           11 2
     C                   Z-ADD     *ZEROS        XXCOMT           11 2
     C                   Z-ADD     *ZEROS        XXMEDR           11 2
     C                   Z-ADD     *ZEROS        XXCOMR           11 2
     C*
     C                   DO        10            X                 3 0
     C     RT(X)         IFNE      *BLANK
     C                   MOVEL     RT(X)         XXKEY
     C     TBKEY         CHAIN     PRT01                              61
     C     *IN61         IFEQ      *OFF
     C*
     C     RTTYPE        IFEQ      'C'
     C                   ADD       AR(X)         XXCOMT
     C                   ADD       RR(X)         XXCOMR
     C                   ENDIF
     C     RTTYPE        IFEQ      'M'
     C                   ADD       AR(X)         XXMEDT
     C                   ADD       RR(X)         XXMEDR
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C*     Calculate total incurred
     C                   ADD       XXCOMT        XXCOMR
     C                   ADD       XXMEDT        XXMEDR
     C*
     C                   ENDDO
     C                   ENDSR
       DCL-PROC GetDenyFlag;
         DCL-PI *N IND;
            @IXedny char(1) Const;
            @cmdny char(1) Const;
            @IxdnyDlw char(1) Const;
            @cmdtlw packed(7:0) Const;
         END-PI;

         DCL-S isDeny ind ;

         isDeny = *Off;

         If @IXedny = 'Y' and @CMdny = 'Y';
            If @IXdnyDlw = 'Y';
               If @CMdtlw > *Zero;
                  isDeny = *On;
               Endif;
            Else;
                  isDeny = *On;
            Endif;
         Endif;

         Return isDeny;
       END-PROC GetDenyFlag;

**  PRDS - PRGRAM DESCRIPTION
     EDI 148 TRIGGER MAINTENANCE
