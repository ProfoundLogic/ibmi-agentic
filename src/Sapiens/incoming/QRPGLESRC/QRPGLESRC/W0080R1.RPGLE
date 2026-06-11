     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H/TITLE W0080R1 - A49 CORRECTION/CHANGE MAINTENANCE
     H*****************************************************************
     H*                                                               *
     H* CREATED BY:   C200                                            *
     H* PROJECT:      25WDE             08/27/96                      *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                                       *
     H* 05/09/25 C:053 Support 152/118567                                     *
     H*                Check for Primary Comp Rate record.                    *
     H*                                                                       *
     H* 10/10/24 C:053 Support 152/117512-118375                              *
     H*                Correct GWA reporting.                                 *
     H*                                                                       *
     H* 11/03/23 C:053 Support 114140                                         *
     H*                Correct fields                                         *
     H*
     H* 09/08/23 C:053 Support 114/113569                                     *
     H*                Correct Gross Weekly Amount reporting                  *
     H*                                                                       *
     H* 02/14/22 C:079  Project: WC2797                               *
     H*                 Add IAIABC 3.1.4 field(s)                     *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 06/03/20 C:180 Project: WC2711C                               *
     H*                Load Ben typ code based on file WMAWL3.        *
     H*                                                               *
     H* 10/12/18 C:079 Project: WC2515                                *
     H*                Fix User Parm Issue                            *
     H*                Add provison for release 3.1 (A)               *
     H*                                                               *
     H* 05/30/18 C:079 Project: WC2515                                *
     H*                Add EDI Release 3.1 changes for Narrative      *
     H*                                                               *
     H* 11/28/13 C:279 Support: WC025 84188                           *
     H*                Modified to capture the MTC information for    *
     H*                corrected filing.                              *
     H*                                                               *
     H* 09/11/13 C:190 Project: WC2227                                *
     H*                Add "MTC Date Override".                       *
     H*                                                               *
     H* 12/19/11 C:180 SUPPORT 78545
     H*                Populate GWEFD with DOI
     H*
     H* 02/22/11 C:180 SUPPORT 75918
     H*                When called from menu, return to SCR01 from
     H*                SCR03.
     H*
     H*                Do not exit if dates are invalid on SCR03
     H*
     H*                Do not clear invalid dates so user can see
     H*                the invalid date in error.
     H*
     H*                Do not validate line if all blanks.
     H*
     H*                Clear case, yr when called from menu
     H*                and re-displaying SCR01.
     H*
     H* 01/13/11 C:180 SUPPORT 74827
     H*                CLEAR SCR01
     H*
     H*
     H* 11/10/10 C:180 SUPPORT 74646
     H*                Fix number of weeks calc for Recoveries.
     H*
     H* 05/28/10 C:180 SUPPORT 70035
     H*                Fix non-consecutive period error caused by
     H*                the payment summary screen when checks
     H*                have been adjusted from TTD to TPD.
     H*
     H* 05/14/10 C:235 SUPPORT 72045
     H*                Change in support 66306 so that pgm does not
     H*                return to the menu unless F3 or F12 is pressed.
     H*                                                               *
     H* 05/10/10 C:247 SUPPORT 72364                                  *
     H*                EDI Screen - Option 18 is not working          *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 03/23/10 C:235 PROJECT 139WNF                                 *
     H*                EDI SCREEN DESIGN                              *
     H*                                                               *
     H* 03/22/10 C:180 SUPPORT 71744
     H*                DISPLAY FILE CHANGES ONLY - INCREASE SIZE
     H*                OF SCWKS TO 5,0
     H*
     H* 04/14/09 C:53  SUPPORT 67840
     H*                Allow update of GW eff dt if LT NW eff dt.
     H*
     H* 04/10/09 C:189 SUPPORT 67959
     H*                Transaction records should not be deleted
     H*
     H* 03/23/09 C:189 SUPPORT 67522-B
     H*                Add new flag for MTC link for ben seg
     H*                                                               *
     H*
     H* 03/14/09 C:53  SUPPORT 67522
     H*                Add new flag for MTC link for ben seg
     H*                                                               *
     H* 03/12/09 C:190 WC1584 PRISS#48
     H*                Process SCR03 if MTC code = 'IP' and           *
     H*                Payment summary flag = 'Y' instead of SCR02.   *
     H*                                                               *
     H* 02/11/09 C:53  SUPPORT 66077
     H*          Do not load info if PTD = ZERO
     H*
     H* 02/04/09 C:53  SUPPORT 66971
     H*                Change DSPF edit codes - Recompile only
     H*
     H* 12/18/08 C:53  SUPPORT 66306
     H*          Workflow changes
     H*
     H* 11/14/08 C:180 SUPPORT 66017
     H*          Remove F12 for client 065 - Send info message when
     H*          MTC code added.
     H*
     H* 09/26/08 C:180 SUPPORT  64234
     H*                For client 090 only -
     H*                Allow update of correction detail file WDAJP
     H*                when WMLGP log record processing is pending
     H*
     H* 06/24/08 C:180 SUPPORT  63750
     H*                Not Needed.
     H*                Changes covered under Project WC1584(10/19/07)
     H*
     H* 06/20/08 C:180 SUPPORT  63641
     H*                - Session or device error occurred. Logic error
     H*                  causing pgm to execute wrong section.
     H*
     H*                - Decimal data error.
     H*
     H* 02/13/08 C:190 PROJECT: WC1589                                *
     H*                Program is changed to only allow MTC codes     *
     H*                that are valid for the reporting state. Also,  *
     H*                the MTC search functionality will be changed   *
     H*                to search the state specific MTC table,if it   *
     H*                exists.                                        *
     H*                                                               *
     H* 02/05/08 C:53  Project :WC1584 PRISS 32                       *
     H*                RELAX ALL EDITS                                *
     H*                                                               *
     H* 10/19/07 C:191 Project :WC1584                                *
     H*                EDI Release 3.0 changes                        *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 05/14/07 C:191 Support: 58155                                 *
     H*                Add fund to entry params to fix an error with  *
     H*                A49 correction/change when claim fund is diff  *
     H*                from user params fund                          *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3                                  *
     H*                                                               *
     H* 03/15/05 C:15  Project: 114WJE                                *
     H*                Added code to check the EDI flag at the group  *
     H*                level to determine if EDI should be processed. *
     H*                                                               *
     H* 06/24/04 C:53 Support: WC071/42989                            *
     H*               Correct search on SCR03.                        *
     H*                                                               *
     H* 11/07/03 C:53 Project: WC1141                                 *
     H*               Add parms to process a specific case when       *
     H*               called from W0070R5                             *
     H*                                                               *
     H* 07/17/03 C:53 Support: WC065/38013                            *
     H*               Correct weeks/days calc.  When a neg recovery   *
     H*               is processed, the weeks/days should be added.   *
     H*                                                               *
     H* 03/30/03 C:15  Support: WC072/35391                           *
     H*               -Removed edit that prevents entry of subsequent *
     H*                A49's if an original A49 hasn't been accepted. *
     H*                This edit is handled in the extract process in *
     H*                edit # 63.                                     *
     H*               -Added code to clear the prompt fields after    *
     H*                processing the transactions.                   *
     H*                                                               *
     H* 01/08/02 C:15  Support: WC065/30451                           *
     H*                Added code to treat an 'AP' the same as 'IP'.  *
     H*                                                               *
     H* 09/26/00 C:15  Project: WC468                                 *
     H*                Chgd code to retrieve the correct Log record   *
     H*                when finding the most current record.          *
     H*                                                               *
     H* 09/22/00 C:15  Project: WC468                                 *
     H*                Added code to allow the entry of PTD/Reduced   *
     H*                Earnings/Recoveries information.               *
     H*                                                               *
     H* 05/12/00 C:15  Project: WC468                                 *
     H*                Changed pgm to prevent subfile error.          *
     H*                                                               *
     H* 02/08/00 C:15  Project: WC468                                 *
     H*                Y2K changes.                                   *
     H*                                                               *
     H* 11/29/99 C:118 PROJECT: WC577                                 *
     H*                V3 display file chgs.                          *
     H*                                                               *
     H* 04/28/99 C:15 Project: WC468                                  *
     H*               Changed pgm to process claims with an initial   *
     H*               entry date prior to the EDI start date.  The    *
     H*               pgm will assume that the 'IP' has already been  *
     H*               sent.                                           *
     H*                                                               *
     H*  10/15/98 C:15 Project: WC468                                 *
     H*                Reimplemented the usage of SCR02 and SCR03.    *
     H*                                                               *
     H*   9/24/98 C:15 Support: WC072/13989                           *
     H*                Fixed the F10=Search processing.               *
     H*                                                               *
     H*  10/31/97 C:58 PROJECT: WC468                                 *
     H*              o Changed additional address call to W0573R,     *
     H*                rather than W0572R2.                           *
     H*              o Changed DN array to 150.                       *
     H*                                                               *
     H*  10/30/97 C:58 PROJECT: WC468                                 *
     H*               -Added CO and Fund to W0270R5 parameters.       *
     H*                                                               *
     H*   6/12/97 C:15 PROJECT: WC468                                 *
     H*               -Chgd code to allow additional A49 MTC codes.   *
     H*               -Chgd code to prevent SCR02 and SCR03 from      *
     H*                being used.  If the user has to adjust the     *
     H*                pmt info, they must use the pmt adj process.   *
     H*                                                               *
     H*   1/21/97 C:58 PROJECT: 68AMN                                 *
     H*                Revise PLIST for Data element loader W8070R6   *
     H*                                                               *
     H*  12/02/96 C:58 PROJECT: 25WDE                                 *
     H*                CORRECTIONS TO LOGIC:                          *
     H*                 o CALL W0572R2 IF DATE OF DEATH NOT BLANK;    *
     H*                 o CHANGE SS# PARM TO ZONED (CHAR).            *
     H*                                                               *
     H*  11/05/96 C:58 PROJECT: 68AMN                                 *
     H*                COMMENTED IGNORE STATEMENT ON CERTAIN TABLE    *
     H*                FILE RECORD FORMATS USED ONLY FOR TASB.        *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     H*
     FW0080F1   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR03S:RRN2)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMCML1    IF   E           K DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWMLGL4    UF   E           K DISK
     F                                     INFDS(FIDS02)
     FWDPAL5    IF   E           K DISK
     FWDAJL     UF A E           K DISK
     FWTGTL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
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
     F                                     IGNORE(PPA01)
     FWMLGP     IF   E             DISK
     F                                     RENAME(PLG01:DBRCDF)
     FWMBNL     IF   E           K DISK
     f*** Beg Add *** 05/30/18 ***********************************
     fwdv0l     uf a e           k Disk
     f*** End Add *** 05/30/18 ***********************************
     f*** Beg Add *** 12/05/19 ***********************************
     FWMAWL3    IF   E           K DISK
     f*** End Add *** 12/05/19 ***********************************
     f*** Beg Add *** 05/09/25 ***********************************
     FWMAWL6    IF   E           K DISK
     F                                     RENAME(PAW01:PAW06)
     f*** End Add *** 05/09/25 ***********************************
     f*** Beg Add *** 02/14/22 ***********************************
     FWMIXL     IF   E           K DISK
     f*** End Add *** 02/14/22 ***********************************
     D*
     d*** Beg Add *** 09/11/13 ***********************************
     d/copy sPrDate
     d*** End Add *** 09/11/13 ***********************************
     D DN              S            150    DIM(999)
      *
     D PCD             S              3    DIM(10)
     D AMT             S             11  2 DIM(10)
     D SDT             S              7  0 DIM(10)
     D EDT             S              7  0 DIM(10)
     D WKS             S              5  0 DIM(10)
     D DYS             S              3  0 DIM(10)
     D GWA             S             11  2 DIM(10)
     D GDT             S              7  0 DIM(10)
     D NDT             S              7  0 DIM(10)
     D IDT             S              7  0 DIM(10)
     D WAM             S             11  2 DIM(10)
     d*** Beg Add *** 09/11/13 ************************************
     d isError         s               n   inz(*off)
     d qqOvrDate       s              7
     d numDate7        s              7  0
     d*** End Add *** 09/11/13 ************************************
     d*** Beg Add *** 11/28/13 ************************************
     d qqMtCc          s              2
     d qqMtC2          s              8
     d*** End Add *** 11/28/13 ************************************
     d*** Beg Add *** 02/14/22 ************************************
     d qqBcRc          s                   Like(LGbCrC)
     d svJob           s                   Like(Q1job)
     d svCusr          s                   Like(Q1cUsr)
     d svJobN          s                   Like(Q1JobN)
     d*** End Add *** 02/14/22 ************************************
      *** Begin Add *** 06/03/20 **********************************
     d kkEfDt          s                   Like(AWEFDT) inz
      *** End   Add *** 06/03/20 **********************************
      *** Beg Add *** 05/09/25 ***********************************
     d foundAW         s              1
      *** End Add *** 05/09/25 ***********************************
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D FIDS02          DS
     D  Q2RRN#               397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  AJCD01                 1      3
     D  AJCD02                 4      6
     D  AJCD03                 7      9
     D  AJCD04                10     12
     D  AJCD05                13     15
     D  AJCD06                16     18
     D  AJCD07                19     21
     D  AJCD08                22     24
     D  AJCD09                25     27
     D  AJCD10                28     30
     D  AJCD                   1     30
     D                                     DIM(10)
      *
     D                 DS
     D  AJPD01                 1      9  2
     D  AJPD02                10     18  2
     D  AJPD03                19     27  2
     D  AJPD04                28     36  2
     D  AJPD05                37     45  2
     D  AJPD06                46     54  2
     D  AJPD07                55     63  2
     D  AJPD08                64     72  2
     D  AJPD09                73     81  2
     D  AJPD10                82     90  2
     D  AJPD                   1     90  2
     D                                     DIM(10)
      *
     D                 DS
     D  AJWA01                 1      9  2
     D  AJWA02                10     18  2
     D  AJWA03                19     27  2
     D  AJWA04                28     36  2
     D  AJWA05                37     45  2
     D  AJWA06                46     54  2
     D  AJWA07                55     63  2
     D  AJWA08                64     72  2
     D  AJWA09                73     81  2
     D  AJWA10                82     90  2
     D  AJWA                   1     90  2
     D                                     DIM(10)
      *
     D                 DS
     D  AJST01               001    007  0
     D  AJST02               008    014  0
     D  AJST03               015    021  0
     D  AJST04               022    028  0
     D  AJST05               029    035  0
     D  AJST06               036    042  0
     D  AJST07               043    049  0
     D  AJST08               050    056  0
     D  AJST09               057    063  0
     D  AJST10               064    070  0
     D  AJST                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  AJEN01               001    007  0
     D  AJEN02               008    014  0
     D  AJEN03               015    021  0
     D  AJEN04               022    028  0
     D  AJEN05               029    035  0
     D  AJEN06               036    042  0
     D  AJEN07               043    049  0
     D  AJEN08               050    056  0
     D  AJEN09               057    063  0
     D  AJEN10               064    070  0
     D  AJEN                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  AJWK01                 1      5  0
     D  AJWK02                 6     10  0
     D  AJWK03                11     15  0
     D  AJWK04                16     20  0
     D  AJWK05                21     25  0
     D  AJWK06                26     30  0
     D  AJWK07                31     35  0
     D  AJWK08                36     40  0
     D  AJWK09                41     45  0
     D  AJWK10                46     50  0
     D  AJWK                   1     50  0
     D                                     DIM(10)
      *
     D                 DS
     D  AJDY01                 1      1  0
     D  AJDY02                 2      2  0
     D  AJDY03                 3      3  0
     D  AJDY04                 4      4  0
     D  AJDY05                 5      5  0
     D  AJDY06                 6      6  0
     D  AJDY07                 7      7  0
     D  AJDY08                 8      8  0
     D  AJDY09                 9      9  0
     D  AJDY10                10     10  0
     D  AJDY                   1     10  0
     D                                     DIM(10)
     D                 DS
     D  AJGA01                 1      9  2
     D  AJGA02                10     18  2
     D  AJGA03                19     27  2
     D  AJGA04                28     36  2
     D  AJGA05                37     45  2
     D  AJGA06                46     54  2
     D  AJGA07                55     63  2
     D  AJGA08                64     72  2
     D  AJGA09                73     81  2
     D  AJGA10                82     90  2
     D  AJGA                   1     90  2
     D                                     DIM(10)
      *
     D                 DS
     D  AJGE01               001    007  0
     D  AJGE02               008    014  0
     D  AJGE03               015    021  0
     D  AJGE04               022    028  0
     D  AJGE05               029    035  0
     D  AJGE06               036    042  0
     D  AJGE07               043    049  0
     D  AJGE08               050    056  0
     D  AJGE09               057    063  0
     D  AJGE10               064    070  0
     D  AJGE                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  AJWD01               001    007  0
     D  AJWD02               008    014  0
     D  AJWD03               015    021  0
     D  AJWD04               022    028  0
     D  AJWD05               029    035  0
     D  AJWD06               036    042  0
     D  AJWD07               043    049  0
     D  AJWD08               050    056  0
     D  AJWD09               057    063  0
     D  AJWD10               064    070  0
     D  AJWD                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  AJPI01               001    007  0
     D  AJPI02               008    014  0
     D  AJPI03               015    021  0
     D  AJPI04               022    028  0
     D  AJPI05               029    035  0
     D  AJPI06               036    042  0
     D  AJPI07               043    049  0
     D  AJPI08               050    056  0
     D  AJPI09               057    063  0
     D  AJPI10               064    070  0
     D  AJPI                   1    070  0
     D                                     DIM(10)
      *
     D                 DS
     D  AJLM01                 1      1
     D  AJLM02                 2      2
     D  AJLM03                 3      3
     D  AJLM04                 4      4
     D  AJLM05                 5      5
     D  AJLM06                 6      6
     D  AJLM07                 7      7
     D  AJLM08                 8      8
     D  AJLM09                 9      9
     D  AJLM10                10     10
     D  AJLM                   1     10
     D                                     DIM(10)
      *
     D                 DS
     D  XXSS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
      *
     D                 DS
     D XXPRDS          C                   CONST('A49 Correction/chan-
     D                                     ge')
     D                 DS
     D  BNACOD                 1     10
     D  XXACOD                 2      4


     C**************************************************************************
     C* Execute the Initializing Subroutine
     C*
     C                   EXSR      SRINIT
     C*
     C     STRT1         TAG
     C*
     C*** Retrieve the User Parameter File
     C*
     C     UPKY          CHAIN     PUP01                              60
     C  N60              MOVE      UPCO#         XXCO#
     C  N60              MOVE      UPFND         XXFND
     C*** Override the value of fund from User parms if passed in thru *entry
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@FND         XXFND
     C                   ENDIF
     C*** Override the value of fund from User parms if passed in thru *entry
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@RTCD        XXRTCD            1
     C                   ENDIF
     C*
     C*** Retrieve Company Record
     C*
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   MOVE      *BLANKS       COADR1
     C                   MOVE      *BLANKS       COADR2
     C                   MOVE      *BLANKS       COCTY
     C                   MOVE      *BLANKS       COST
     C                   MOVE      *ZEROS        COZIP
     C                   MOVE      *ZEROS        COFEIN
     C                   ENDIF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVEL(P)  XXPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        QQPRDS
      *
      *** Retrieve Fund Record
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ALL'*'       FNNAME
     C                   ENDIF
     C     FNEDI         CABEQ     'Y'           TERM
      *
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
      *** Display Formats
      *
     C     STRT          TAG
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXSR      SRLSC1
     C                   EXFMT     SCR01
      *
     C     Q2RECF        WHENEQ    'SCR02'
     C                   EXSR      SRLSC2
     C                   EXFMT     SCR02
      *
     C     Q2RECF        WHENEQ    'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   EXSR      SRLSC3
     C     SCRRN3        IFEQ      0
     C                   Z-ADD     1             SCRRN3
     C                   ENDIF
     C     HRRN          IFEQ      0
     C                   Z-ADD     1             HRRN
     C                   ENDIF
     C                   WRITE     SCR03
     C                   MOVE      *ON           *IN95
     C                   EXFMT     SCR03C
      *
     C                   OTHER
      *
     C                   GOTO      TERM
      *
     C                   ENDSL
      *
      *** Process Command Keys
      *
     C     *IN11         IFEQ      *ON
      *
     C                   SELECT
      *
      *** CMD 3 - End of Function
      *
     C     *INKC         WHENEQ    *ON
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
      *
      *** CMD 10 - Table File Inquiry
      *
     C     *INKJ         WHENEQ    *ON
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C*   SCR01
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** F10 - Claim # Search
     C*
     C     HFLD          IFEQ      'SCCASE'
     C     HFLD          OREQ      'SCYR'
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN1        @@RRN2            9 0
     C                   PARM      SCYR          @@FYR             3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ELSE
      *
     C     @@RRN         CHAIN     PCM02                              60
     C                   MOVE      CMFYR         SCYR
     C                   MOVE      CMCASE        SCCASE
     C                   MOVE      CMSS#         XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  XXSS1         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS2:0       SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS3:0       SCSS#
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   Maintenance Type Code search
     C*
     C     HFLD          IFEQ      'SCMTC'
     C*
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVE      *BLANKS       XXTBID
     C                   MOVEL     'SMTC'        XXTBID
     C*
     C     SCFND         IFNE      0
     C     SCCASE        ANDNE     0
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     CMJRST        CAT(P)    'E2':0        XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C   99              MOVEL     'SMTC'        XXTBID
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXTKEY
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     XXTKEY        CABEQ     *BLANKS       STRT                     30
     C     QQRTCD        CABEQ     'B'           STRT                     30
     C                   MOVEL     XXTKEY        SCMTC
     C     QQRTCD        CABEQ     '*'           STRT                     35
      *
     C                   ENDIF
     C*   Maintenance Type Correction Code Search
     C*
     C     HFLD          IFEQ      'SCMTCC'
     C*
     C                   CALL      'W0875R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           XXCALL            1
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    SCYR
     C                   PARM                    SCCASE
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT                     35
      *
     C                   SETON                                            31
     C     ##RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      LGMTC         SCMTCC
     C                   Z-ADD     LGMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCMTC2
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
      **  SCR02 - TABLE SEARCH
      *
     C     Q2RECF        IFEQ      'SCR02'
      *
     C     HFLD          IFEQ      'SCIPLC'
     C*
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVE      *BLANKS       XXTBID
     C                   MOVEL     'LRSN'        XXTBID
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXTKEY
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     XXTKEY        CABEQ     *BLANKS       STRT                     41
     C     QQRTCD        CABEQ     'B'           STRT                     41
     C                   MOVEL     XXTKEY        SCIPLC
     C     QQRTCD        CABEQ     '*'           STRT
      *
     C                   ENDIF
     C*
     C     HFLD          IFEQ      'SCPMTC'
     C*
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVE      *BLANKS       XXTBID
     C                   MOVEL     'PADJ'        XXTBID
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXTKEY
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     XXTKEY        CABEQ     *BLANKS       STRT                     41
     C     QQRTCD        CABEQ     'B'           STRT                     41
     C                   MOVEL     XXTKEY        SCPMTC
     C     QQRTCD        CABEQ     '*'           STRT
      *
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
      **  SCR03 - TABLE SEARCH
      *
     C     Q2RECF        IFEQ      'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C     HRRN          CHAIN     SCR03S                             62
     C     *IN62         IFEQ      *OFF
      *
      *
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVE      *BLANKS       XXTBID
     C                   MOVEL     'PADJ'        XXTBID
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXTKEY
     C                   PARM                    QQRTCD
      *
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C     QQRTCD        IFEQ      'B'
     C     XXTKEY        OREQ      *BLANKS
     C                   MOVE      *ON           *IN41
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     XXTKEY        SCPMTC
     C                   UPDATE    SCR03S
     C                   Z-ADD     HRRN          SCRRN3
     C                   MOVEL(P)  'SCR03C'      Q2RECF
     C                   ENDIF
      *
      *
     C                   ENDIF
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   GOTO      STRT
      *
      *** CMD 11 - EDI Log History
      *
     C     *INKK         WHENEQ    *ON
      *
     C                   CALL      'W0875R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    QQCALL
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    SCYR
     C                   PARM                    SCCASE
     C*
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT
     C                   GOTO      STRT
      *
     C     *IN16         WHENEQ    *ON
     C                   CALL      'W0070R1B'
     C                   PARM      *BLANKS       QQPRDS
     C                   PARM      ' '           QQCALL
     C                   PARM                    XXFND
     C                   PARM                    SCYR
     C                   PARM                    SCCASE
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQRTCD        CABEQ     'B'           STRT
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      'A'           QQRTCD
     C                   RETURN
     C                   ENDIF
      *** CMD 12 - Prior Screen
      *
     C     *INKL         WHENEQ    *ON
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
     C*
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
      *
     C     Q2RECF        WHENEQ    'SCR02'
     C                   MOVEL     'SCR01   '    Q2RECF
      *
     C     Q2RECF        WHENEQ    'SCR03C'
     C     Q2RECF        OREQ      'SCR03S'
     C                   MOVEL     'SCR01   '    Q2RECF
      *
     C                   ENDSL
     C                   GOTO      STRT
      *
      *** CMD 13 - Change User Parameters
      *
     C     *INKM         WHENEQ    *ON
     C                   CALL      'W0006R'
     C                   GOTO      STRT1
      *
     C                   OTHER
     C                   GOTO      STRT
     C                   ENDSL
      *
     C                   ENDIF
      *
      **************************************************************
      *
      *** Execute Appropriate Screen Routine
      *
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR03C'      SCR03T
     C     Q2RECF        CABEQ     'SCR03S'      SCR03T
     C*************************
     C*  Process Screen One
     C*************************
     C     SCR01T        TAG
     C                   MOVE      *OFF          *IN37
     C*
     C*  Case Master must exist for specified fund yr/case
     C*
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      *
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      *
      *   Maintenance Type Code must be valid
      *
     C                   MOVEL(P)  'SMTC'        XXTBID
     C     CMJRST        CAT(P)    'E2':0        XXTBID
     C     GIKY          CHAIN     PGI01                              99
     C   99              MOVEL     'SMTC'        XXTBID
     C                   MOVEL(P)  SCMTC         XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  Retrieve the EDI start date
     C*
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      *BLANKS       XXTSET
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C                   MOVE      IXCSDT        SVCSDT            7 0

     C*
     C*  If 'IP' is the MTC, retrieve the log record and
     C*  ensure that the original IP was a rejected status
     C*
     C     SCMTC         IFEQ      'IP'
     C     SCMTC         OREQ      'AP'
     C*
     C                   MOVE      SCMTC         XXMTC
     C                   MOVE      'A49'         XXTSET
     C     LGKY          CHAIN(N)  PLG01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     LGSTS         IFNE      'TR'
     C     LGSTS         ANDNE     'EI'
     C     LGSTS         ANDNE     'EC'
     C                   MOVE      *ON           *IN23
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     LGKY          READE(N)  PLG01                                  60
     C                   ENDDO
      *
     C                   ENDIF
     c*** Beg Add *** 09/11/13 ************************************
      /free
       If scMntOvrDt <> 0;
       // ** Date must be valid if entered.
          isError = *off;
          msOvrDate = ' ';
          erOvrDate = *Off;
          If dtCheck(scMntOvrDt)=0;
             iserror=*On;
             erOvrDate = *On;
             msOvrDate = 'Invalid Date';
          Endif;
       Endif;
      /end-free
     c*** End Add *** 09/11/13 ************************************
     C*  For IXRLS = '3': If the Maintenance type code = "CO"
     C*    Edit for the existence of an A49 log record for the claim equal to
     C*    the Maintenance Type Correction Code entered on the screen and be in
     C*    a status of "TR" or "TE"
     C*
     C*
     C     IXRLS         IFEQ      '3'
     C     SCMTC         ANDEQ     'CO'
     C*** Beg Add *** 10/12/18 *******************************************
     C     IXRLS         OREQ      'A'
     C     SCMTC         ANDEQ     'CO'
     C*** End Add *** 10/12/18 *******************************************
     C*
     C                   MOVE      SCMTCC        XXMTC
     C                   MOVE      'A49'         XXTSET
     C     LGKY          CHAIN(N)  PLG01                              60
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
     C     LGKY          READE(N)  PLG01                                  60
     C                   ENDDO
     C                   ENDIF
      *
      *
     C*
     C*  Validate the pmt summary screen flag
     C*
     C     SCFLAG        IFNE      'Y'
     C     SCFLAG        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  Validate the reduced earnings screen flag
     C*
     C     SCFLG2        IFNE      'Y'
     C     SCFLG2        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN33
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCFLG3        IFNE      'Y'
     C     SCFLG3        ANDNE     *BLANKS
     C                   MOVE      '1'           *IN36
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCMTC         IFEQ      'PY'
     C     SCFLG3        ANDNE     'Y'
     C                   MOVE      '1'           *IN39
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  At this point, all edits have been passed and the Generic
     C*    Log API will be called to create the log record.
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     c*** Beg Add *** 09/11/13 ***********************************
      /free
       numDate7  = dtCYMD(scMntOvrDt);
       qqOvrDate = %editc(numDate7:'X');
      /end-free
     c*** End Add *** 09/11/13 ***********************************
      ************** Beg Add ********* 11/28/13 ******************
      /Free
       qqMtCc = scMtCc;
       If scMtc2 > 0;
          qqMtC2 = %editC(scMtc2:'X');
       Endif;
      /End-free
       //*** Beg Add *** 02/14/22 *********************************************
          qqBcRc = scBcRc;
       //*** End Add *** 02/14/22 *********************************************
      ************** End Add ********* 11/28/13 ******************
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET
     C                   PARM      SCMTC         QQMTC
     C                   PARM      LGJST         QQJST
     C                   PARM                    QQRTCD
     c*** Beg Add *** 09/11/13 ***********************************
     c                   PARM                    QQCPGM           10
     c                   PARM                    QQOVRDATE
     c*** End Add *** 09/11/13 ***********************************
      ************** Beg Add ********* 11/28/13 ******************
     c                   Parm                    qqMtCc
     c                   Parm                    qqMtC2
      ************** End Add ********* 11/28/13 ******************
       //*** Beg Add *** 02/14/22 *********************************************
     c                   Parm                    qqBcRc
       //*** End Add *** 02/14/22 *********************************************
       //*** Beg Add *** 05/30/18 ********************************
      /Free
                         If scMtcNarr <> *blanks;
                            Callp ClmNarrative();
                         Endif;
      /End-Free
       //*** End Add *** 05/30/18 ********************************
     C*
     C     SCFLAG        IFNE      'Y'
     C     SCFLG2        ANDNE     'Y'
     C     SCFLG3        ANDNE     'Y'
     C     CLTID         IFEQ      '065'
     C     QQRTCD        IFEQ      *BLANKS
     C     SCFLAG        ANDEQ     *BLANKS
     C     SCFLG2        ANDEQ     *BLANKS
     C     SCFLG3        ANDEQ     *BLANKS
     C                   MOVE      *ON           *IN37
     C                   ELSE
     C                   MOVE      *OFF          *IN37
     C                   ENDIF
     C                   ENDIF
     C     QQRTCD        CABEQ     '2'           STRT                     29
     C     QQRTCD        CABEQ     '3'           STRT                     28
     C     QQRTCD        CABEQ     '4'           STRT                     42
     C                   ENDIF
     C*
     C*  Retrieve the current log record.
     C*
     C                   MOVE      'A49'         XXTSET
     C                   MOVE      SCMTC         XXMTC
     C     LGKY          CHAIN(N)  PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C                   LEAVE
     C                   ENDIF
     C*
     C     LGKY          READE(N)  PLG01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          X@SF01
     C                   MOVEL     'SCR01   '    Q2RECF
     C   15              MOVEL     *BLANKS       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*** SAVE THE RR# IN CASE THE ENTRY IS CANCELLED.
     C                   Z-ADD     Q2RRN#        SVRRN#
     C                   MOVE      LGMTC         SVMTC
     C*
     C*  If the summary flag = 'Y', goto SCR02 if MTC = 'IP' or SCR03
     C*  for all other MTC codes.
     C*
     C     SCFLAG        IFEQ      'Y'
      *
      ***        Retrieve data base elements
      *
     C                   CLEAR                   DN
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      'A49'         QQTSET
     C                   MOVEL     SCMTC         QQMTC
     C                   MOVEL     LGJST         QQJST
     C                   Z-ADD     LGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         QQEXDT
      *
     C                   CALL      'W8070R6'
     C                   PARM                    QQEXDT
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQTSET
     C                   PARM                    QQMTC
     C                   PARM                    QQJST
     C                   PARM                    DN
      *
     C     SCMTC         IFEQ      'IP'
     C     SCMTC         OREQ      'AP'
      *
      ***        Retrieve the Initial Payment data
      *
     C                   EXSR      SRRTIP
     C     SCFLAG        IFEQ      'Y'
     C     SCMTC         ANDEQ     'IP'
     C                   MOVEL     'SCR03C'      Q2RECF
     C                   MOVE      *OFF          X@SF03
     C                   ELSE
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVE      *OFF          X@SF02
     C                   ENDIF
      *
     C                   ELSE
      *
      ***        Retrieve the Payment/Adjustment data
      *
     C                   EXSR      SRRTPA
     C                   MOVEL     'SCR03C'      Q2RECF
     C                   MOVE      *OFF          X@SF03
      *
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*  If the reduced earnings screen was selected, call the
     C*    entry screen pgm.
     C*
     C     SCFLG2        IFEQ      'Y'
     C*
     C                   MOVE      LGCO#         ##CO#
     C                   MOVE      LGFND         ##FND
     C                   MOVE      LGFYR         ##FYR
     C                   MOVE      LGCASE        ##CASE
     C                   MOVE      LGDSQH        ##SEQ#
     C                   CALL      'W0072R'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##SEQ#            5
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*  If the link payment screen was selected, call the pgm.
     C*
     C     SCFLG3        IFEQ      'Y'
     C*
     C                   MOVE      LGCO#         ##CO#
     C                   MOVE      LGFND         ##FND
     C                   MOVE      LGFYR         ##FYR
     C                   MOVE      LGCASE        ##CASE
     C                   MOVE      LGDSQH        ##SEQ#
     C                   CALL      'W0074R'
     C                   PARM                    ##CALL            1
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##SEQ#            5
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      SCFLAG        XXFLAG            1
     C                   MOVE      SCMTC         S2MTC             2
     C                   CLEAR                   SCFLG3
     C                   CLEAR                   SCMTCC
      *
     C     SCFLAG        IFNE      'Y'
     C     XXCAS         IFEQ      *BLANKS
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF
     C                   ENDIF
     C*************************
     C*  Process Screen Two
     C*************************
     C     SCR02T        TAG
      *
      ***        Edit the input
      *
      ***        - Late Reason Code must be valid
      *
     C                   MOVE      *BLANKS       SCIPLD
     C     SCIPLC        IFNE      *BLANKS
     C                   CLEAR                   XXTKEY
     C                   MOVEL     'LRSN'        XXTBID
     C                   MOVEL     SCIPLC        XXTKEY
     C     XTKY          CHAIN     PGT01                              20
     C     *IN20         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCIPLD
     C                   ELSE
     C                   MOVE      *ALL'*'       SCIPLD
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      ***        - Pmt Code must be valid
      *
     C                   CLEAR                   XXTKEY
     C                   MOVEL     'PADJ'        XXTBID
     C                   MOVEL     SCPMTC        XXTKEY
     C     XTKY          CHAIN     PGT01                              21
     C     *IN21         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - Start Date must be valid
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN24
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - End Date must be valid
      *
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           *IN25
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - Start Date must not be greater than End Date
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT1            7 0
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT2            7 0
     C     XXDAT1        IFGT      XXDAT2
     C                   MOVE      *ON           *IN28
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - Number of Days must be valid
      *
     C     SCDYS         IFLT      *ZEROS
     C     SCDYS         ORGT      7
     C                   MOVE      *ON           *IN27
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        If the transaction is pending, update the log file
      ***        and the correction detail.  Otherwise, write a
      ***        correction detail and a log record.
      *
     C     LGDSQM        IFNE      0
     C                   MOVE      LGCO#         XXCO#
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C                   MOVE      LGDSQM        XXSEQ#
     C     AJKY          CHAIN     PAJ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SCIPLC        AJLTRN
     C                   MOVEL     SCPMTC        AJCD01
     C                   Z-ADD     SCTDAM        AJPD01
     C                   Z-ADD     SCWKAM        AJWA01
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJST01
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJEN01
     C                   Z-ADD     SCWKS         AJWK01
     C                   Z-ADD     SCDYS         AJDY01
     C                   MOVE      SCLNKM        AJLM01
     C                   Z-ADD     SCGWAM        AJGA01
     C                   Z-ADD     SCGWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJGE01
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJWD01
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJPI01
     C                   UPDATE    PAJ01
     C                   ENDIF
      *
     C                   ELSE
      *
      ***        Add AJP record and Log Record
      *
     C                   EXSR      SRAAJR
     C                   EXSR      SRALGR
     C     CLTID         IFEQ      '090'
     C                   EXSR      SRALGR
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   GOTO      NXTPGM
     C*************************
     C*  Process Screen Three
     C*************************
     C     SCR03T        TAG
      * LINK FLAG FOR CHECK ON AT LEAST 1 ENTRY
      *
     C                   MOVE      *OFF          XXLINK            1
      *
     C                   READC     SCR03S                                 62
     C     *IN62         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN21
     C                   MOVE      *OFF          *IN24
     C                   MOVE      *OFF          *IN25
     C                   MOVE      *OFF          *IN27
     C                   MOVE      *OFF          *IN28
     C                   MOVE      *OFF          *IN29
     C                   MOVE      *OFF          *IN30
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *OFF          *IN40
      *
      ***   PROCESS THE TRANSACTION IF AT LEAST ONE FIELD WAS ENTERED.
      *
     C     SCPMTC        IFGT      *BLANKS
     C     SCTDAM        ORGT      *ZEROS
     C     SCWKAM        ORGT      *ZEROS
     C     SCWKDT        ORGT      *ZEROS
     C     SCSTDT        ORGT      *ZEROS
     C     SCENDT        ORGT      *ZEROS
     C     SCWKS         ORGT      *ZEROS
     C     SCDYS         ORGT      *ZEROS
     C     SCGWAM        ORGT      *ZEROS
     C     SCGWDT        ORGT      *ZEROS
     C     SCISDT        ORGT      *ZEROS
     C     SCLNKM        ORGT      *ZEROS
      *
     C*          SCPMTC    IFNE *BLANKS
     C*          SCSTDT    ORNE *ZEROS
     C*          SCENDT    ORNE *ZEROS
     C*          SCWKS     ORNE *ZEROS
     C*          SCDYS     ORNE *ZEROS
      *
      ***        - Pmt Code must be valid
      *
     C                   CLEAR                   XXTKEY
     C                   MOVEL     'PADJ'        XXTBID
     C                   MOVEL     SCPMTC        XXTKEY
     C     XTKY          CHAIN     PGT01                              21
     C     *IN21         IFEQ      *ON
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      ***        - Weekly Eff Date must be valid
      *
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCWKDT
     C     SRERR         IFEQ      *ON
     C     SCWKDT        ANDNE     *ZEROS
     C     SRERR         OREQ      *ON
     C     SRDTI         ANDGT     *ZEROS
     C                   MOVE      SRDTI         SCWKDT
     C                   MOVE      *ON           *IN29
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - Start Date must be valid
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C     SRERR         IFEQ      *ON
     C     SCSTDT        ANDNE     *ZEROS
     C     SRERR         OREQ      *ON
     C     SRDTI         ANDGT     *ZEROS
     C                   MOVE      SRDTI         SCSTDT
     C                   MOVE      *ON           *IN24
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - End Date must be valid
      *
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C     SRERR         IFEQ      *ON
     C     SCENDT        ANDNE     *ZEROS
     C     SRERR         OREQ      *ON
     C     SRDTI         ANDGT     *ZEROS
     C                   MOVE      SRDTI         SCENDT
     C                   MOVE      *ON           *IN25
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      *
      ***        - Start Date must not be greater than End Date
      *
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT1
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXDAT2
     C     XXDAT1        IFNE      *ZEROS
     C     XXDAT2        ANDNE     *ZEROS
     C     XXDAT1        IFGT      XXDAT2
     C                   MOVE      *ON           *IN28
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      ***        - Number of Days must be valid
      *
     C     SCDYS         IFNE      *ZEROS
     C     SCDYS         IFLT      *ZEROS
     C     SCDYS         ORGT      7
     C                   MOVE      *ON           *IN27
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      ***        - Gross Weekly Eff Date Valid
      *
     C                   Z-ADD     SCGWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCGWDT
     C     SRERR         IFEQ      *ON
     C     SCGWDT        ANDNE     *ZEROS
     C     SRERR         OREQ      *ON
     C     SRDTI         ANDGT     *ZEROS
     C                   MOVE      SRDTI         SCGWDT
     C                   MOVE      *ON           *IN29
     C                   MOVE      *ON           *IN30
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      ***        - Payment Issue Date Valid
      *
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCISDT
     C     SRERR         IFEQ      *ON
     C     SCISDT        ANDNE     *ZEROS
     C     SRERR         OREQ      *ON
     C     SRDTI         ANDGT     *ZEROS
     C                   MOVE      SRDTI         SCISDT
     C                   MOVE      *ON           *IN30
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      ***        - MTC Link needed for specific MTC's
     C     SCMTC         IFEQ      'RB'
     C     SCMTC         OREQ      'S1'
     C     SCMTC         OREQ      'S2'
     C     SCMTC         OREQ      'S3'
     C     SCMTC         OREQ      'S4'
     C     SCMTC         OREQ      'S5'
     C     SCMTC         OREQ      'S6'
     C     SCMTC         OREQ      'S7'
     C     SCMTC         OREQ      'S8'
     C     SCMTC         OREQ      'S9'
     C     SCMTC         OREQ      'CB'
      * edit for valid value
     C     SCLNKM        IFNE      'X'
     C     SCLNKM        ANDNE     ' '
     C                   MOVE      *ON           *IN39
     C                   UPDATE    SCR03S
     C                   GOTO      STRT
     C                   ENDIF
      * edit for required entry
     C     SCLNKM        IFEQ      'X'
     C                   MOVE      *ON           XXLINK
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
     C                   UPDATE    SCR03S
     C*                    ENDIF
      *
     C                   READC     SCR03S                                 62
     C                   ENDDO
      *
      *
      ***        If the transaction is pending, update the log file
      ***        and the correction detail.  Otherwise, write a
      ***        correction detail and a log record.
      *
     C     LGDSQM        IFNE      0
     C                   MOVE      LGCO#         XXCO#
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C                   MOVE      LGDSQM        XXSEQ#
     C     AJKY          CHAIN     PAJ01                              60
     C     *IN60         IFEQ      *OFF
     C     1             DO        10            P
     C     P             CHAIN     SCR03S                             62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     SCPMTC        AJCD(P)
     C                   Z-ADD     SCTDAM        AJPD(P)
     C                   Z-ADD     SCWKAM        AJWA(P)
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJST(P)
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJEN(P)
     C                   Z-ADD     SCWKS         AJWK(P)
     C                   Z-ADD     SCDYS         AJDY(P)
     C                   MOVE      SCLNKM        AJLM(P)
     C                   Z-ADD     SCGWAM        AJGA(P)
     C                   Z-ADD     SCGWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJGE(P)
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJWD(P)
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJPI(P)
     C                   ENDIF
     C                   ENDDO
     C                   UPDATE    PAJ01
     C                   ENDIF
      *
     C                   ELSE
      *
      ***        Add AJP record and Log Record
      *
     C                   EXSR      SRAAJR
     C                   EXSR      SRALGR
     C     CLTID         IFEQ      '090'
     C                   EXSR      SRALGR
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     NXTPGM        TAG
     C     XXCAS         IFEQ      *BLANKS
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       SCMTC
     C                   MOVE      *BLANKS       SCFLAG
     C                   MOVE      *OFF          X@SF01
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
      *
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      *BLANKS       QQRTCD
      *
     C                   CALL      'W0070R7'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
      *
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
      *
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      *BLANKS       QQRTCD
      *
     C                   CALL      'W0070R9'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
      *
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
      *
      ***        If date of death exists, maintain addtnl address
     C*
     C     DN(57)        IFNE      *BLANKS
     C*
     C                   CALL      'W0573R'
     C                   PARM      ' '           XCALL             1
     C                   PARM      'Y'           XXPMTF            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    XXRRN             9 0
     C*
     C*
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
      *
     C                   ENDIF
      *
     C                   MOVE      *OFF          X@SF01
     C                   MOVEL     'SCR01   '    Q2RECF
     C                   GOTO      STRT
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      **************************************************************************
      *** ROUTINE - SRINIT
      *
     C     SRINIT        BEGSR
      *
      *** Define Parameter Lists
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQYR              3
     C                   PARM                    QQCAS             7
     C                   PARM                    @@FND             3
     C                   PARM                    @@RTCD            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMDp       PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYYp       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C     DTYYMDp       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST
     C                   PARM                    XXTSET
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C*                    PARM *BLANKS   IXTXID  1
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
      *
      *
      *** Define Key Lists
      *
      ***        SMCOL - Company Master
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      ***        WMFNL - Fund Master
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ***        WMUPL - User Parameter File
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      ***        WMCML - Claim Master File
      *
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      ***        WMLGL4 - EDI Claim Log File
      *
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
     C                   KFLD                    XXMTC
      *
     C     LGK2          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXTSET
      *
      ***        WDPAL5 - Payment Detail File
      *
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      ***        WDAJL - Pmt/Adj Correction/Change File
      *
     C     AJKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
      *
     C     AJK2          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
      ***        WTABL - System Table File
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTKEY
      *
      ***        WTGTL - Generic Table File
      *
     C     XTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C*
     C*  WMIXL - EDI control file
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTSET
     C*
     C     GIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C     BNKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
      *
      *** Define work fields
      *
     C     *LIKE         DEFINE    QQCALL        QSCALL
     C     *LIKE         DEFINE    QQPRDS        QSPRDS
     C     *LIKE         DEFINE    Q2RRN#        QQRRN#
     C     *LIKE         DEFINE    LGTSET        QQTSET
     C     *LIKE         DEFINE    LGMTC         QQMTC
     C     *LIKE         DEFINE    LGJST         QQJST
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    LGTSET        XXTSET
     C     *LIKE         DEFINE    LGMTC         XXMTC
     C     *LIKE         DEFINE    PATRDT        XXTRDT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C     *LIKE         DEFINE    Q1PGM         QQPGM
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    Q1JOBN        QQJOB#
     C     *LIKE         DEFINE    Q1USER        QQUSER
     C     *LIKE         DEFINE    Q1JOB         QQJOB
     C     *LIKE         DEFINE    *IN60         X@SF01
     C     *LIKE         DEFINE    *IN60         X@SF02
     C     *LIKE         DEFINE    *IN60         X@SF03
     C     *LIKE         DEFINE    CMJRST        XXJRST
     C     *LIKE         DEFINE    Q2RRN#        SVRRN#
     C     *LIKE         DEFINE    LGMTC         SVMTC
     C     *LIKE         DEFINE    BNAMT         XBNAMT
     C     *LIKE         DEFINE    BNENDT        XBNSDT
     C     *LIKE         DEFINE    BNAMT         XXCURD
     C     Q1PRMS        IFGT      3
     C                   MOVE      QQCAS         XXCAS             7
     C                   ELSE
     C                   MOVE      *BLANKS       XXCAS             7
     C                   ENDIF
     C                   MOVE      *BLANKS       XXWK4A            4
      *
     C                   MOVE      *BLANKS       QQCO#             3
     C                   MOVE      *BLANKS       QQFND             3
     C                   MOVE      *BLANKS       QQFYR             3
     C     Q1PRMS        IFLT      3
     C                   MOVE      *BLANKS       QQCASE            7
     C                   ENDIF
     C                   MOVE      *BLANKS       QQEXDT            8
     C                   MOVE      *BLANKS       QQRTCD            1
     C                   MOVE      *ZEROS        XXDATE            8 0
     C                   MOVE      *OFF          XXPF              1
     C                   MOVE      *OFF          X@SF01
     C                   MOVE      *OFF          X@SF02
     C                   MOVE      *OFF          X@SF03
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *ZEROS        RRN2              5 0
     C                   MOVE      *BLANKS       SCFLAG
     C                   Z-ADD     1             SCRRN3
     C                   Z-ADD     1             HRRN
      *
      ***        Determine if called from menu
      *
     C     QQFND         IFEQ      '000'
     C                   ENDIF
      *
      *** Retrieve the User Parameter File
      *
     C     UPKY          CHAIN     PUP01                              60
     C     XXCO#         IFEQ      0
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   ENDIF
     C*** Override the value of fund from User parms if passed in thru *entry
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@FND         XXFND
     C                   ENDIF
      *
     C*** Override the value of fund from User parms if passed in thru *entry
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@RTCD        XXRTCD
     C                   ENDIF
      *** Retrieve Company Record
      *
     C     COKY          CHAIN     PCO01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   MOVE      *BLANKS       COADR1
     C                   MOVE      *BLANKS       COADR2
     C                   MOVE      *BLANKS       COCTY
     C                   MOVE      *BLANKS       COST
     C                   MOVE      *ZEROS        COZIP
     C                   MOVE      *ZEROS        COFEIN
     C                   ENDIF
      *
      *** Retrieve Fund Record
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *ALL'*'       FNNAME
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C                   ENDSR
      **************************************************************************
      *** ROUTINE - SRRTIP
      *
     C     SRRTIP        BEGSR
      *
     C                   MOVE      *OFF          XXPF
      *
      *** If the log file is a pending transaction, attempt to retrieve
      *** the correction detail.
      *
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C                   MOVE      LGCO#         XXCO#
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C                   MOVE      LGDSQM        XXSEQ#
     C     AJKY          CHAIN(N)  PAJ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'A'           XXPF
     C                   MOVE      AJLTRN        SCIPLC
     C                   MOVE      AJCD01        SCPMTC
     C                   MOVE      AJPD01        SCTDAM
     C                   MOVE      AJWA01        SCWKAM
     C                   Z-ADD     AJST01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCSTDT
     C                   Z-ADD     AJEN01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCENDT
     C                   Z-ADD     AJWK01        SCWKS
     C                   Z-ADD     AJDY01        SCDYS
     C                   MOVE      AJLM01        SCLNKM
     C                   ENDIF
     C                   ENDIF
      *
     C     XXPF          IFEQ      *OFF
      *
      *** Start reading the payment detail
      *
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     PARETP        IFEQ      'C'
     C                   CLEAR                   XXTKEY
     C                   MOVEL     PABR2         XXTKEY
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *BLANKS       B2PYCD
     C                   ENDIF
     C     B2PYCD        IFNE      *BLANKS
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2PYCD        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     ' '
     C                   MOVE      'P'           XXPF
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   ENDSR
      **************************************************************************
      *** ROUTINE - SRRTPA
      *
     C     SRRTPA        BEGSR
      *
     C                   MOVE      *OFF          XXPF
     C                   CLEAR                   PCD
     C                   CLEAR                   AMT
     C                   CLEAR                   SDT
     C                   CLEAR                   EDT
     C                   CLEAR                   WKS
     C                   CLEAR                   DYS
     C                   CLEAR                   GWA
     C                   CLEAR                   WAM
     C                   CLEAR                   GDT
     C                   CLEAR                   NDT
     C                   CLEAR                   IDT
      *
      *** If the log file is a pending transaction, attempt to retrieve
      *** the correction detail.
      *
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C                   MOVE      LGCO#         XXCO#
     C                   MOVE      LGFND         XXFND
     C                   MOVE      LGFYR         XXFYR
     C                   MOVE      LGCASE        XXCASE
     C                   MOVE      LGDSQM        XXSEQ#
     C     AJKY          CHAIN(N)  PAJ01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'A'           XXPF
     C                   ENDIF
     C                   ENDIF
      *
     C     XXPF          IFEQ      *OFF
     C                   CLEAR                   PAJ01
      *
      *** Start reading the payment detail
      *
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   CLEAR                   XXTKEY
     C                   MOVEL     PABR2         XXTKEY
     C     TBKY          CHAIN     PB201                              61
      *
     C     *IN61         IFEQ      *ON
     C                   MOVE      *BLANKS       B2PYCD
     C                   ENDIF
      *
     C     B2PYCD        IFNE      *BLANKS
      *
     C                   MOVEL(P)  'PADJ'        XXTBID
     C                   MOVEL(P)  B2PYCD        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      '0'
     C     GTFG01        ANDEQ     ' '
      *
     C                   MOVE      'P'           XXPF
     C                   Z-ADD     1             P
     C     B2PYCD        LOOKUP    PCD(P)                                 61
      *
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     1             P
     C     *BLANKS       LOOKUP    PCD(P)                                 61
     C                   ENDIF
      *
     C     *IN61         IFEQ      *ON
     C                   SELECT
     C     PAADST        WHENEQ    'A'
     C                   MOVE      *OFF          I#SEL
      *
     C     PATYPE        WHENEQ    'P'
     C     PAADST        ANDEQ     'S'
     C     PAAMT         ANDGT     *ZERO
     C                   MOVE      *OFF          I#SEL
      *
     C     PATYPE        WHENEQ    'S'
     C     PAAMT         ANDGT     *ZERO
     C                   MOVE      *OFF          I#SEL
      *
     C     PATYPE        WHENEQ    'P'
     C     PAADST        ANDEQ     'V'
     C     PAAMT         ANDGT     *ZERO
     C                   MOVE      *OFF          I#SEL
      *
     C     PATYPE        WHENEQ    'V'
     C     PAAMT         ANDGT     *ZERO
     C                   MOVE      *OFF          I#SEL
     C                   OTHER
     C                   MOVE      *ON           I#SEL             1
     C                   ENDSL
      *
     C     I#SEL         IFEQ      *ON
     C*
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
     C                   ADD       PAWKS         WKS(P)
     C                   ADD       PADAYS        DYS(P)
     C     DYS(P)        IFGE      7
     C                   ADD       1             WKS(P)
     C                   SUB       7             DYS(P)
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     B2ERCV        XXERCV            1
     C     PATYPE        IFEQ      'R'
     C     XXERCV        ANDNE     '8'
      * Do nothing
     C                   ELSE
     C                   SUB       PAWKS         WKS(P)
     C                   SUB       PADAYS        DYS(P)
     C     DYS(P)        IFLT      *ZEROS
     C                   SUB       1             WKS(P)
     C                   ADD       7             DYS(P)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   MULT      -1            PAAMT
     C                   END
      *
     C                   MOVEL     B2PYCD        PCD(P)
     C                   ADD       PAAMT         AMT(P)
      *
     C     SDT(P)        IFEQ      *ZEROS
     C     PAFRDT        ORLT      SDT(P)
     C                   MOVE      PAFRDT        SDT(P)
     C                   MOVE      PAFRDT        NDT(P)
     C                   ENDIF
      *
     C     B2PYCD        IFEQ      '050'
     C     CMACDT        ANDGT     *ZEROS
     C     CMACDT        ANDLT     PATODT
     C                   MOVE      CMACDT        NDT(P)
     C                   ENDIF
     C     PATODT        IFGT      EDT(P)
     C                   MOVE      PATODT        EDT(P)
     C                   ENDIF
      *
     C*
     C     IXRLS         IFEQ      '3'
     C*** Beg Add *** 10/12/18 *******************************************
     C     IXRLS         OREQ      'A'
     C*** End Add *** 10/12/18 *******************************************
     C*
     C*
     C     PCD(P)        IFEQ      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XBNAMT
     C                   Z-ADD     NDT(P)        XBNSDT
     C*
     C     GDT(P)        IFEQ      *ZEROS
     C     NDT(P)        ORLT      GDT(P)
     C                   Z-ADD     NDT(P)        GDT(P)
     C                   ENDIF
     C*
     C     BNKY          CHAIN     PBN01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     BNSTAT        IFEQ      'A'
     C     PCD(P)        ANDEQ     XXACOD
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         XXCURD
     C* WITHIN RANGE
     C*
     C     BNSTDT        IFLE      XXCURD
     C     BNENDT        ANDGE     XXCURD
     C     BNSTDT        ORLE      XXCURD
     C     BNENDT        ANDEQ     *ZEROS
     C*
     C                   ADD       BNAMT         XBNAMT
     C*
     C     BNSTDT        IFGT      XBNSDT
     C                   MOVE      BNSTDT        XBNSDT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     BNKY          READE     PBN01                                  60
     C                   ENDDO
     C*
      *//*** Begin Change *** 06/03/20 ***********************************
     C***     CMCRT         IFNE      *ZEROS
     C***                   Z-ADD     CMCRT         GWA(P)                    GR
     C***                   Z-ADD     CMCRT         WAM(P)                    WE
     C***                   ENDIF
     C***
      /Free
     /*** Beg Add *** 05/09/25 ***********************************
     /* Check for Primpary Comp Rate record for the PV1
        foundAW = ' ';
        Chain (PAco#:PAfnd:PAfyr:PAcase) PAW06;
        If not %eof(WMAWL6) and AWbr2 = PAbr2;
           foundAW = 'Y';
        Endif;
        If foundAW <> 'Y';
     /*** End Add *** 05/09/25 ***********************************
        kkEfDt =   %dec(%char(%date():*cymd0):7:0);
        Setgt (PAco#:PAfnd:PAfyr:PAcase:PAbr2:kkEfDt) PAW01;
        Readpe (PAco#:PAfnd:PAfyr:PAcase:PAbr2) PAW01;
        If not %eof(WMAWL3);
     /*** Beg Add *** 05/09/25 ***********************************
           foundAW = 'Y';
        Endif;
        Endif;
        If foundAW = 'Y';
     /*** End Add *** 05/09/25 ***********************************
     /*** Beg Del *** 10/10/24 ********************************************
     /*** Beg Chg *** 09/08/23 ********************************************
     /**** GWA(P) = AWcrt; ***/
     /**** GWA(P) = AWaww; ***/
     /*** End Chg *** 09/08/23 ********************************************
     /*** End Del *** 10/10/24 ********************************************
     C*** Beg Add *** 10/10/24 ********************************************
           Select;
           When CLTID = '114';
              GWA(P) = AWaww;
           Other;
              GWA(P) = AWcrt;
           Endsl;
     C*** Eng Add *** 10/10/24 ********************************************
           WAM(P) = AWcrt;
        Else;
           GWA(P) = *Zeros;
           WAM(P) = *Zeros;
        Endif;
        //*** End Change *** 06/03/20 ***********************************
      /End-Free
     C     XBNAMT        IFNE      *ZEROS
     C*
     C     WAM(P)        SUB       XBNAMT        WAM(P)
     C*
     C     XBNSDT        IFNE      *ZEROS
     C                   MOVE      XBNSDT        NDT(P)
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
      *
     C                   ENDIF
      *
     C                   ENDIF
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
      *
      *
      * REMOVE ENTRIES WITH PTD = 0 AND ADJUST THE ARRAY BEFORE LOADING
      *
     C                   Z-ADD     0             B                 2 0
     C     1             DO        10            A                 2 0
      *
     C     AMT(A)        IFGT      0
     C                   MOVE      'Y'           XWRITE            1
      *
     C                   ELSE
     C                   MOVE      'N'           XWRITE
     C                   ENDIF
      *
     C     XWRITE        IFEQ      'Y'
     C                   ADD       1             B
     C                   MOVE      PCD(A)        PCD(B)
     C                   MOVE      AMT(A)        AMT(B)
     C                   MOVE      SDT(A)        SDT(B)
     C                   MOVE      EDT(A)        EDT(B)
     C                   MOVE      WKS(A)        WKS(B)
     C                   MOVE      DYS(A)        DYS(B)
     C                   MOVE      GWA(A)        GWA(B)
     C                   MOVE      WAM(A)        WAM(B)
     C                   MOVE      GDT(A)        GDT(B)
     C                   MOVE      NDT(A)        NDT(B)
     C                   MOVE      IDT(A)        IDT(B)
      *
     C     B             IFNE      A
     C                   RESET                   PCD(A)
     C                   RESET                   AMT(A)
     C                   RESET                   SDT(A)
     C                   RESET                   EDT(A)
     C                   RESET                   WKS(A)
     C                   RESET                   DYS(A)
     C                   RESET                   GWA(A)
     C                   RESET                   WAM(A)
     C                   RESET                   GDT(A)
     C                   RESET                   NDT(A)
     C                   RESET                   IDT(A)
     C                   ENDIF
      *
     C                   ELSE
     C                   RESET                   PCD(A)
     C                   RESET                   AMT(A)
     C                   RESET                   SDT(A)
     C                   RESET                   EDT(A)
     C                   RESET                   WKS(A)
     C                   RESET                   DYS(A)
     C                   RESET                   GWA(A)
     C                   RESET                   WAM(A)
     C                   RESET                   GDT(A)
     C                   RESET                   NDT(A)
     C                   RESET                   IDT(A)
     C                   ENDIF
     C                   ENDDO
      *
      ***        Load Values into 'AJ' arrays
      *
     C                   MOVE      PCD           AJCD
     C                   MOVE      AMT           AJPD
     C                   MOVE      SDT           AJST
     C                   MOVE      EDT           AJEN
     C                   MOVE      WKS           AJWK
     C                   MOVE      DYS           AJDY
     C                   MOVE      GWA           AJGA
     C                   MOVE      GDT           AJGE
     C                   MOVE      NDT           AJWD
     C                   MOVE      IDT           AJPI
     C                   MOVE      WAM           AJWA
      *
     C                   ENDIF
      *
     C                   ENDSR
      **************************************************************************
      *
      *** ROUTINE - SRLSC1
      *
     C     SRLSC1        BEGSR
      *
      *** Initialize Screen Fields
      *
     C                   TIME                    DSDTTM
      *
     C     X@SF01        IFEQ      *OFF
     C                   MOVE      *ON           X@SF01
      *
       //*** Beg Add *** 02/14/22 *********************************************
       // Save job information so we can restore the screen field values
       svJob  = Q1job;
       svCusr = Q1cUsr;
       svJobN = Q1jobN;
       //*** End Add *** 02/14/22 *********************************************
     C                   CLEAR                   SCR01
       //*** Beg Add *** 02/14/22 *********************************************
       // Restore Job Information so the screen fields keep the info
       Q1job  = svJob;
       Q1cUsr = svCusr;
       Q1jobN = svJobN;
       //*** End Add *** 02/14/22 *********************************************
      *
      *
      ** Retrieve client id
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '065'
     C                   MOVE      *OFF          *IN38
     C                   MOVE      *OFF          *IN45
     C                   ELSE
     C                   MOVE      *ON           *IN38
     C                   MOVE      *ON           *IN45
     C                   ENDIF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
     C     Q1PRMS        IFGT      2
     C                   MOVE      QQYR          SCYR
     C                   MOVE      QQCAS         SCCASE
     C                   MOVE      *ON           *IN15
     C                   ELSE
     C                   MOVE      *ZEROS        SCYR
     C                   MOVE      *ZEROS        SCCASE
     C                   ENDIF
     C                   MOVE      *BLANKS       SCMTC
      *
     C                   ELSE
     C     *INKJ         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCMTC
     C                   MOVE      *BLANKS       SCMTCC
     C                   MOVE      *ZEROS        SCMTC2
     C                   MOVE      *BLANKS       SCFLAG
     C                   MOVE      *BLANKS       SCFLG2
     C                   MOVE      *BLANKS       SCFLG3
     C*** Beg Add *** 09/11/13 ************************************
      /free
       scMntOvrDt = 0;
      /end-free
     C*** End Add *** 09/11/13 ************************************
     C                   ENDIF
     C     XXCAS         IFEQ      *BLANKS
     C     *INKJ         ANDEQ     *OFF
     C                   MOVE      *ZEROS        SCYR
     C                   MOVE      *ZEROS        SCCASE
     C                   ENDIF
     C                   ENDIF
       //*** Beg Add *** 02/14/22 *********************************************
       // If Claim is passed in then get the jurisdiction state and the EDI rele
       If Q1prms >= 5;
          xxFyr  = %Dec(qqYr:3:0);
       //*** Beg Chg *** 11/03/23 *********************************************
       //*** xxCase = %Dec(qqCas:5:0);
          xxCase = %Dec(qqCas:7:0);
       //*** End Chg *** 11/03/23 *********************************************
          Chain cmky pcm01;
          If %Found(WMCML1);
             Chain (xxCo#:CMjrSt) pix01;
             If %Found(WMIXL);
                If IXrls = 'A';
                   isRls31 = *on;
                Else;
                   isRls31 = *off;
                Endif;
             Endif;
          Endif;
       Endif;
       scCo# = xxCo#;
       //*** End Add *** 02/14/22 *********************************************
      *
     C                   ENDSR
      **************************************************************************
      *** ROUTINE - SRLSC2
      *
     C     SRLSC2        BEGSR
      *
      *** Initialize Screen Fields
      *
     C                   TIME                    DSDTTM
      *
     C     X@SF02        IFEQ      *OFF
     C                   MOVE      *ON           X@SF02
      *
     C                   CLEAR                   SCR02
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   MOVEL     LGFYR         SCYRCL
     C                   MOVE      LGCASE        SCYRCL
     C     DN(42)        IFNE      *BLANKS
     C                   MOVE      DN(42)        XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  XXSS1         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS2:0       SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS3:0       SCSS#
     C                   ENDIF
     C     DN(43)        IFNE      *BLANKS
     C                   MOVEL     DN(43)        SCCLNM
     C                   ENDIF
     C     DN(44)        IFNE      *BLANKS
     C                   CAT       ',':0         SCCLNM
     C                   CAT       DN(44):1      SCCLNM
     C                   ENDIF
     C     DN(45)        IFNE      *BLANKS
     C                   CAT       DN(45):1      SCCLNM
     C                   ENDIF
     C     DN(31)        IFNE      *BLANKS
     C                   MOVEL     DN(31)        XXDATE
     C                   MOVEL     XXDATE        XXWK4A
     C                   MOVE      XXWK4A        SCACDT
     C                   MOVE      XXDATE        XXWK4A
     C                   MOVEL     XXWK4A        SCACDT
     C                   ENDIF
      *
     C     XXPF          IFNE      *OFF
      *
     C     XXPF          IFEQ      'A'
     C                   MOVE      AJLTRN        SCIPLC
     C                   MOVE      AJCD01        SCPMTC
     C                   MOVE      AJPD01        SCTDAM
     C                   MOVE      AJWA01        SCWKAM
     C                   Z-ADD     AJST01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCSTDT
     C                   Z-ADD     AJEN01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCENDT
     C                   Z-ADD     AJWK01        SCWKS
     C                   Z-ADD     AJDY01        SCDYS
     C                   MOVE      AJLM01        SCLNKM
     C                   Z-ADD     AJGA01        SCGWAM
     C                   Z-ADD     AJGE01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCGWDT
     C                   Z-ADD     AJWD01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCWKDT
     C                   Z-ADD     AJPI01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCISDT
     C                   ENDIF
      *
     C     XXPF          IFEQ      'P'
     C                   MOVEL     DN(77)        SCIPLC
     C                   CLEAR                   XXTKEY
     C                   MOVEL     PABR2         XXTKEY
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *ON
     C                   MOVE      *BLANKS       B2PYCD
     C                   ENDIF
     C                   MOVEL     B2PYCD        SCPMTC
     C                   MOVE      PAAMT         SCTDAM
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCSTDT
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCENDT
     C                   Z-ADD     PAWKS         SCWKS
     C                   Z-ADD     PADAYS        SCDYS
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       SCIPLD
     C     SCIPLC        IFNE      *BLANKS
     C                   CLEAR                   XXTKEY
     C                   MOVEL     'LRSN'        XXTBID
     C                   MOVEL     SCIPLC        XXTKEY
     C     XTKY          CHAIN     PGT01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCIPLD
     C                   ELSE
     C                   MOVE      *ALL'*'       SCIPLD
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      **************************************************************************
      *** ROUTINE - SRLSC3
      *
     C     SRLSC3        BEGSR
      *
      *** Initialize Screen Fields
      *
     C                   TIME                    DSDTTM
      *
     C     X@SF03        IFEQ      *OFF
     C                   MOVE      *ON           X@SF03
      *
     C                   CLEAR                   SCR03C
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      *
     C                   MOVEL     LGFYR         SCYRCL
     C                   MOVE      LGCASE        SCYRCL
     C     DN(42)        IFNE      *BLANKS
     C                   MOVE      DN(42)        XXSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'XXSS#'       QQNAME           25
     C     XXSS#         PARM      XXSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVEL(P)  XXSS1         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS2:0       SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       XXSS3:0       SCSS#
     C                   ENDIF
     C     DN(43)        IFNE      *BLANKS
     C                   MOVEL     DN(43)        SCCLNM
     C                   ENDIF
     C     DN(44)        IFNE      *BLANKS
     C                   CAT       ',':0         SCCLNM
     C                   CAT       DN(44):1      SCCLNM
     C                   ENDIF
     C     DN(45)        IFNE      *BLANKS
     C                   CAT       DN(45):1      SCCLNM
     C                   ENDIF
     C     DN(31)        IFNE      *BLANKS
     C                   MOVE      DN(31)        XXDATE
     C                   MOVEL     XXDATE        XXWK4A
     C                   MOVE      XXWK4A        SCACDT
     C                   MOVE      XXDATE        XXWK4A
     C                   MOVEL     XXWK4A        SCACDT
     C                   ENDIF
      *
      *
      ***        Clear the subfile
      *
     C                   MOVE      *OFF          *IN01
     C                   MOVE      *ON           *IN04
     C                   WRITE     SCR03C
     C                   MOVE      *ON           *IN01
     C                   MOVE      *OFF          *IN04
      *
      ***        Write the subfile records, using any information
      ***        available in the 'AJ' arrays
      *
     C     1             DO        10            P
     C                   CLEAR                   SCR03S
      *
     C     AJCD(P)       IFNE      *BLANKS
     C                   MOVE      AJCD(P)       SCPMTC
     C                   MOVE      AJPD(P)       SCTDAM
     C                   MOVE      AJWA(P)       SCWKAM
     C                   Z-ADD     AJST(P)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCSTDT
     C                   Z-ADD     AJEN(P)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCENDT
     C                   MOVE      AJWK(P)       SCWKS
     C                   MOVE      AJDY(P)       SCDYS
     C                   MOVE      AJLM(P)       SCLNKM
     C                   MOVE      AJGA(P)       SCGWAM
     C                   Z-ADD     AJGE(P)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCGWDT
     C                   Z-ADD     AJWD(P)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCWKDT
     C                   Z-ADD     AJPI(P)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         SCISDT
     C                   ENDIF
      *
     C                   Z-ADD     P             RRN2
     C                   WRITE     SCR03S
      *
     C                   ENDDO
     C                   ENDIF
     C                   ENDSR
      **************************************************************************
      *** ROUTINE - SRAAJR
      *
     C     SRAAJR        BEGSR
      *
      *** Retrieve the highest sequence # associated with this claim
      *** and add 1.
      *
     C     AJK2          SETGT     PAJ01
     C     AJK2          READPE(N) PAJ01                                  60
     C     *IN60         IFEQ      *OFF
     C     AJSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PAJ01
     C                   MOVE      *ZEROS        AJGA
     C                   MOVE      *ZEROS        AJGE
     C                   MOVE      *ZEROS        AJWD
     C                   MOVE      *ZEROS        AJPI
     C                   MOVE      LGCO#         AJCO#
     C                   MOVE      LGFND         AJFND
     C                   MOVE      LGFYR         AJFYR
     C                   MOVE      LGCASE        AJCASE
     C                   Z-ADD     XXSEQ#        AJSEQ#
      *
     C     S2MTC         IFEQ      'IP'
     C     S2MTC         OREQ      'AP'
     C                   MOVEL     SCIPLC        AJLTRN
     C                   MOVEL     SCPMTC        AJCD01
     C                   Z-ADD     SCTDAM        AJPD01
     C                   Z-ADD     SCWKAM        AJWA01
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJST01
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJEN01
     C                   Z-ADD     SCWKS         AJWK01
     C                   Z-ADD     SCDYS         AJDY01
     C                   MOVE      SCLNKM        AJLM01
     C                   Z-ADD     SCGWAM        AJGA01
     C                   Z-ADD     SCGWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJGE01
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJWD01
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJPI01
      *
     C                   ELSE
      *
     C     1             DO        10            P
     C     P             CHAIN     SCR03S                             62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     SCPMTC        AJCD(P)
     C                   Z-ADD     SCTDAM        AJPD(P)
     C                   Z-ADD     SCWKAM        AJWA(P)
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJST(P)
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJEN(P)
     C                   Z-ADD     SCWKS         AJWK(P)
     C                   Z-ADD     SCDYS         AJDY(P)
     C                   MOVE      SCLNKM        AJLM(P)
     C                   Z-ADD     SCGWAM        AJGA(P)
     C                   Z-ADD     SCGWDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJGE(P)
     C                   Z-ADD     SCWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJWD(P)
     C                   Z-ADD     SCISDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJPI(P)
     C                   ENDIF
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   MOVEL     Q1USER        AJIUSR
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI = %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJIDT
     C                   TIME                    AJITM
     C                   MOVEL     Q1PGM         AJIPGM
     C                   MOVE      Q1USER        AJUSER
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI = %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         AJDT
     C                   TIME                    AJTIME
     C                   MOVEL     Q1PGM         AJPGM
     C                   WRITE     PAJ01
      *
     C                   ENDSR
     C*===================================================================
     C*   ROUTINE - SRALGR
     C*
     C     SRALGR        BEGSR
     C*
     C*
     C*  Retrieve the current log record.
     C*
     C                   MOVE      'A49'         XXTSET
     C                   MOVE      SVMTC         XXMTC
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     LGSTS         IFEQ      'EI'
     C     LGSTS         OREQ      'EC'
     C                   LEAVE
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     XXSEQ#        LGDSQM
     C                   MOVE      Q1USER        LGUSER
     C*PUIMOD
     C*                  Z-ADD     UDATE         SRDTI
     C                   EVAL      SRDTI = %Int(%Char(%Date():*MDY0))
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         LGDT
     C                   TIME                    LGTIME
     C                   MOVEL     Q1PGM         LGPGM
     C                   UPDATE    PLG01
     C                   ENDIF
     C*
     C                   ENDSR
      *----------------------------------------------------------------
      * INITIALIZATION
      *----------------------------------------------------------------
     C     *INZSR        BEGSR
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   ENDSR
       //*** Beg Add *** 05/30/18 ********************************
     P*=====================================================================
     P* ClmNarrative - Write to the Claim Narrative file
     P*  Parameters None
     P*=====================================================================
     PclmNarrative     B                   Export
     DclmNarrative     PI
     D strpos          s              3  0
     D n               s              2  0
     D xxLine50        s             50a
      /Free
        V0co# = %Dec(qqCo#:3:0);
        V0fnd = %Dec(qqFnd:3:0);
        V0fyr = %Dec(qqFyr:3:0);
        V0case = %Dec(qqCase:7:0);
        V0mtC  = scMtc;
        V0mtCd = numDate7;
        strPos = 1;
        //
        // Read through screen field in reverse order of 50A sections
        //
        For n = 1 to 10;
           xxLine50 = %SubSt(scMtcNarr:strPos:50);
           If xxLine50 <> *blanks;
              V0seq# = n;
              V0crNa = xxLine50;
              Write pv001;
              StrPos = strPos + 50;
              Clear xxLine50;
           Endif;
        Endfor;

      /End-Free
     PclmNarrative     E
       //*** End Add *** 05/30/18 ********************************
