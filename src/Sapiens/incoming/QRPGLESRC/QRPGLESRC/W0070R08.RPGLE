     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     h  bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0070R08 - CLAIMS, ACCIDENT DESCRIPTION
     H* ***************************************************************
     H*                                                               *
     H*  DESCRIPTION:  Processes Accident Information                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0070R - CLAIM MASTER SEARCH                               *
     H*                                                               *
     H*  CALLS TO:  FA003R - API to update MC claim master FMCMP      *
     H*             SA001R - API to determine if MC installed         *
     H*                                                               *
     H*  CREATED: 01/10/97  Version 3 BASE                            *
     H*                    C:58  PROJECT: 60ANC                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 06/27/12 C:53  Support: 79988                                 *
     H*                Correct Devereux fields to allow update/write  *
     H*                                                               *
     H* 04/05/12  C:255 Project WC2087                                *
     H*                 Added processing for field "If 'M' more than  *
     H*                 'X' days, where X is particular No. of days.  *
     H*                                                               *
     H* 08/10/11 C:62  Support WC102 77046
     H*                -Create buckets even if Record only.
     H*                -Ignore WMRHP if a closed record.
     H*
     H* 07/27/11  C:190 PROJECT 114WXT                                *
     H*                 Made "Loss recovery type" required on         *
     H*                 particular condition.                         *
     H*                                                               *
     H* 08/16/10 C:53  Project: 25WRK  PRISS ITEM
     H*                Condition Auto Claim letter to not produce for
     H*                CLT025
     H*
     H* 05/17/10 C:235 PROJRCT: 139WNF  Additional Changes            *
     H*                All New field and function keys will be        *
     H*                qualified on the WCMS EDI install flag='Y'     *
     H*                                                               *
     H*
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 03/23/10 C:235 PROJRCT: 139WNF                                *
     H*                EDI SCREEN DESIGN                              *
     H*
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*
     H* 02/01/10  C:247  Project: 25WQO                               *
     H*                  New Auto diary features                      *
     H*
     H* 12/01/09 C:235 SUPPORT 69373                                  *
     H*                When a claim is entered through WW CIC Internet*
     H*                claim,it automatically assigns an M for Medical*
     H*                only under LT Status,If user changes LT status *
     H*                to I at that time it will not generate auto    *
     H*                letter at Accident Description Screen.
     H*                This done to avoid Dublicate auto letter that  *
     H*                generates after the reserve entry screen.
     H*                                                               *
     H* 09/24/09  C:062  Project: 139WMO                              *
     H*                  Add EDI Claim Type Processing.               *
     H*                                                               *
     H* 07/21/09  C:062  Project: 114WTT                              *
     H*                  Add Devereax processing fields               *
     H*
     H* 12/03/08 C:53  SUPPORT: 66259
     H*                Correct edit for client 65 to be based on in31
     H*
     H* 11/06/08 C:180 PROJECT  135WLG
     H*                         Check for existence of RAT record
     H*                         to condition scr01 fields.
     H*
     H* 03/12/08 C:180 PRISS 8: WC1589
     H*                Client-flag changes for 65WGI
     H*
     H* 12/11/07 C:180 SUPPORT: 60969
     H*                When Claim Master record is in use, specify who
     H*                has a lock on the record.
     H*                Also, do not advise user to exit in the middle
     H*                of ADD mode since claim could be incorrectly
     H*                created without reserves.
     H*
     H* 08/20/07 C:193 PROJECT: 65WGI                                 *
     H*                EDI Rel 3.0 Impl/Setup/Training                *
     H*                                                               *
     H* 02/10/06 C:167 Project 114WKG                                 *
     H*                Date of Disability Edit.                       *
     H*                                                               *
     H* 11/21/06 C:15  Support: WC132/54623                           *
     H*                Chgd pgm to restrict access to state specific  *
     H*                system tables (WGTGP), if they exist.          *
     H*                                                               *
     H* 10/12/06 C:15  Support: WC139/55288                           *
     H*                Chgd condition that determines if the reserve  *
     H*                default is loaded.  Check for no incurred amt, *
     H*                and no reserve histhory records.               *
     H*                                                               *
     H* 09/25/06 C:58  P: 71WHI
     H*                Condition LTFL vs NCCI edits: all but CLT 071
     H*                                                               *
     H* 09/20/06 C:15  Support: WC135/54502                           *
     H*                Added code to clear the Reserve History record *
     H*                format before writing a new record.            *
     H*                                                               *
     H* 04/04/06 C:15  Support: WC045/50992                           *
     H*                Added code to load the reserve codes on closed *
     H*                claims if the LT flag is changed from R.  The  *
     H*                amounts will not be loaded on closed claims.   *
     H*
     H*  03/06/06 C:132 Support: WC071 51160
     H*                Change Code to load DSDTTM and use DSDATE
     H*                instead of *DATE anywhere in the program.
     H*                                                               *
     H* 02/23/05 C:58  P: WC1128   Auto-DARTS
     H*                Add call to WA700R if Lost Time flag changes.
     H*                                                               *
     H* 08/03/04 C:58  Support: WC128/43431                           *
     H*                If bucket not in RT LOKUP, add to next unused. *
     H*                                                               *
     H* 07/21/04 C:15  Support: WC090/43235                           *
     H*                The reserve buckets were not being defaulted   *
     H*                on lost time claims for PDSQuest clients.      *
     H*                                                               *
     H*  4/01/04 C:58  Project:  WC1140
     H*                Add default Expense reserve amount and code.
     H*                Rewrite default process and reserve bucket
     H*                preload logic.
     H*
     H* 12/05/03 C:15  Project:  WC1170                               *
     H*                Added code to default the reserve buckets to a *
     H*                specific order if PDSQuest is installed.       *
     H*                                                               *
     H* 08/04/03 C:53  Support: WC071/38381                           *
     H*                Change code to pickup the correct description  *
     H*                                                               *
     H* 04/22/03 C:53  Project: WC1121                                *
     H*                Add API for claim letter based on LTFL change  *
     H*                                                               *
     H* 01/17/03 C:15  Support: WC114/35559                           *
     H*                Added code to update the claim outstanding rsv *
     H*                total in WDELP when the default rsv is setup.  *
     H*                                                               *
     H* 09/11/02  C:142 PROJECT WC1077                                *
     H*                 ADDED RESERVE TYPE CODE         \             *
     H*                                                               *
     H* 09/05/00 C:15  Project: WC591                                 *
     H*                Added code to write a record to the Injury     *
     H*                Type History file if the NCCI injury code is   *
     H*                changed.                                       *
     H*                                                               *
     H*  08/25/00 C:062 PROJECT: MC232                                *
     H*                 ADD JURISDICTION STATE TO PARM LIST OF FA003R.*
     H*                                                               *
     H* 04/11/00 C:15  Support: WC088/22432                           *
     H*                Chgd code to only create med reserve default   *
     H*                if the claim is open.                          *
     H*                                                               *
     H* 04/03/00 C:118 PROJECT: WC854                                 *
     H*                Add parm to call of FA003R to pass WMCMP/      *
     H*                CMIMP.                                         *
     H*                                                               *
     H* 09/16/99 C:61  PROJECT WC577                                  *
     H*                Add employer cancel date to header info.       *
     H*                                                               *
     H* 09/02/99 C:112 Support: 19256 (MC048)                         *
     H*                Change QMSCL# from 10A to 25A                  *
     H*                                                               *
     H* 08/18/99 C:15  Support: WC048/19223                           *
     H*                Removed the condition which requires the claim *
     H*                to be in ADD mode when defaulting the medical  *
     H*                reserves.                                      *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H* 02/01/99 C:112 Support: WC060 (Seq#16324), WC096 (Seq#16061   *
     H*                Program now compares Lost Time Flag with the   *
     H*                NCCI code flag 1.  If the NCCI code flag is 'M'*
     H*                and Lost Time Flag is 'I' (indemnity) send an  *
     H*                error message to the screen.                   *
     H*                                                               *
     H* 11/16/98 C:15 Project: WC577                                  *
     H*               Changed the bar code process.  A bar code API   *
     H*               is now called to return certain parameters.     *
     H*                                                               *
     H* 08/11/98 C:62 Project: MC155                                  *
     H*               ADDED PARM FOR CMFEAD TO FA003R                 *
     H*                                                               *
     H* 08/10/98 C:61 Project: 48WLC                                  *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H* 08/06/98 C:87 PROJECT: WC591                                  *
     H*               Added Type of Coverage field and processing.    *
     H*                                                               *
     H* 08/04/98 C:15 Project: WC577                                  *
     H*               Fixed the claim record lockout routine.         *
     H*                                                               *
     H* 05/14/98 C:15 Seq#: WC060/11526                               *
     H*              -Fixed the NCCI code search so only one call is  *
     H*               made to the search pgm, using either the state  *
     H*               specific table or the default table.            *
     H*              -Reversed the change made on 5/7/98 to prevent   *
     H*               the users from entering the wrong NCCI code.    *
     H*                                                               *
     H* 05/07/98 C:33 SEQ#: WC095/11309                               *
     H*               CORRECT LOST TIME/NCCI CODE VALIDATION.         *
     H*                                                               *
     H* 04/28/98 C:33 SEQ#: WC078/11309                               *
     H*              -TURN OFF ERROR INDICATORS IN MVDBSC ALSO.       *
     H*              -VALIDATE THE LOST TIME FLAG TO THE CORRECT NCCI *
     H*               CODE LOST TIME VALUE.                           *
     H*                                                               *
     H* 04/13/98 C:15 Project: WC468                                  *
     H*              -Changed pgm to display "If 'I', more than X     *
     H*               days?" only if the EDI control file indicates   *
     H*               that this field is used as a trigger.           *
     H*              -Added call to the EDI Log API pgm.              *
     H*                                                               *
     H* 04/07/98 C:62 Project: MC155                                  *
     H*               Added additional fields to FA003R parm list.    *
     H*                                                               *
     H*  03/10/98  C:58  PROJECT: SUPPORT: WC087  sequence # 10555    *
     H*              Update entry date when claim is activated (lost  *
     H*              time, LTFL, changes from 'R' to 'M' or 'I').     *
     H*                                                               *
     H* 11/05/97 C:15 Project: WC729                                  *
     H*               Changed the format of date fields used in the   *
     H*               parm lists for pgms FA003R from MDCY to CYMD.   *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  07 - EDI INSTALLED                                           *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  20 - INVALID CURSOR POSITION FOR SEARCH                      *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  64 - CLAIM WAS CONVERTED FROM ANOTHER SYSTEM                 *
     H*  66 - COMMERCIAL APPLICATION INSTALLED                        *
     H*  67 - SECURITY LEVEL VALID                                    *
     H* N67 - SECURITY LEVEL INVALID                                  *
     H*  69 - ESCESS REINSURANCE                                      *
     H*  70 - BAR CODE INSTALLED                                      *
     H*****************************************************************
     H*                                                               *
     H* SCR01 edit indicators                                         *
     H*  90 - Error on field                                          *
     H*      21 - (WCR0501) Potential lost time cannot be blank       *
     H*      22 - (WCR0936) Lost time flag must be I,M, R             *
     H*      25 - (WCR2227) NCCI is medical, Lost time must           *
     H*      24 - (WCR2341) Lost time must be I                       *
     H*      25 - (WCR2342) No reserves can exist on R, record only   *
     H*                                                               *
     H*      26 - (WCR2228) NCCI not medical, Lost time cannot        *
     H*                                                               *
     H*      27 - (WCR0503) Potential lost time cannot be blank       *
     H*      28 - (WCR0501) Potential lost time flag must be Y, or N  *
     H*                                                               *
     H*      29 - (WCR0503) More than 1 day LT cannot be blank        *
     H*      30 - (WCR0501) More than 1 day LT flag must be Y, or N   *
     H*                                                               *
     H*      31 - (WCR0501) More than 1 day LT flag must be Y, or N   *
     H* ***************************************************************
      *
      *================================================================
      *          FILE DEFINITIONS
      *================================================================
      *
      *  Display File
     FW0070F08  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
      *________________________________________________________________
      *
      *  Claim Master - physical file
     FWMCMP     UF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
      *________________________________________________________________
      *
      *  Employer Master --Co/Fnd/Emp/Div                    -EMKY-
     FWMEML     IF   E           K DISK
      *________________________________________________________________
      *
      *  Employer Cross Reference
     FWZEXL     IF   E           K DISK
      *________________________________________________________________
      *
      *  Fund Master     --Co/Fnd                            -FNKY-
     FWMFNL     IF   E           K DISK
      *________________________________________________________________
      *
      *  Claim Demographics --SS#                            -CDKY-
     FWMCDL     IF   E           K DISK
      *________________________________________________________________
      *
      *  Bar Code
      ** ********BATRL   IF  E           K        DISK                    UC
      *________________________________________________________________
      *
      *  System Table Value File
     FWTGTL     IF   E           K DISK
      *________________________________________________________________
      *
      *  System Table ID File
     FWTGIL     IF   E           K DISK
      *________________________________________________________________
      *
      *  USER PARAMETER FILE
     FWMUPL     IF   E           K DISK
      *________________________________________________________________
      *
      *  Main Table Files
     FWTABL     IF   E           K DISK
      *________________________________________________________________
      *
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
      *           PNC01                             KIGNORE
      *         NA01   -- Nature Codes
      *
      *         PA01   -- Part Codes
      *
      *         CA01   -- Cause Codes
      *
     F                                     IGNORE(PRC01)
      *
      *         RT01   -- Reserve Type
      *
      *         PC01   -- Payment Code
      *
      *         B201   -- Payment Value 1
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
      *________________________________________________________________
      *
      *  System Control
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *________________________________________________________________
      *
      *  LOST TIME TRANSFER FILE
     FWMTHP     O    E             DISK
      *
      *  RESERVE HISTORY FILE
     FWMRHP     O    E             DISK
      *
      *  EDI CONTROL FILE
      *  CLAIM INJURY TYPE HISTORY FILE
     FWMIHP     O    E             DISK
     FWDELL     UF   E           K DISK
     FWMRHL     IF   E           K DISK
     F                                     RENAME(PRH01:PRH02)
     FWMCXL     IF   E           K DISK
      *
     FWMADL     IF   E           K DISK
      *================================================================
      *        TABLES AMD ARRAYS
      *================================================================
      *
      *  Reserves
     D TP              S              1    DIM(10)
      *
      *
      *  EMPLOYER'S EFFECTIVE DATES
      *
     D ARX             S              1    DIM(80)
     D USR             S              1    DIM(10)
      *
      *
      *
     D*****/COPY SCOPYMT01I
      *
     D DBFDS           DS
     D  FLSTCM           *STATUS
     D  DBRRN                397    400B 0
      *
     D                 DS                  INZ
     D  RT                     1     20
     D                                     DIM(10)                              Reserve types
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
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              Payments
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
     D                 DS                  INZ
     D  RR                     1     90  2
     D                                     DIM(10)                              Remaining res
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
     D*
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)                              RH rsv types
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)                              RH rsv amt
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
      *  DARTS
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
      *
      *   Parameters for MC Claim Master API        QMxxxx
      *
     D PARMCL          DS                  INZ
     D  QMRTCD                 1      1
     D  QMSS#                  2     10
     D  QMACDT                11     17
     D  FILL1                 18     18
     D  QMCO#                 19     21
     D  QMSUB#                22     26
     D  QMFND                 27     29
     D  QMFYR                 30     32
     D  QMCASE                33     39
     D  QMEMP#                40     48
     D  QMDIV                 49     53
     D  QMACTM                54     59
     D  QMDTRP                60     66
     D  FILL2                 67     67
     D  QMDTLW                68     74
     D  FILL3                 75     75
     D  QMACDS                76    135
     D  QMMCON               136    136
     D  QMENTD               137    143
     D  FILL4                144    144
     D  QMRCVD               145    151
     D  FILL5                152    152
     D  QMMMI                153    159
     D  FILL6                160    160
     D  QM1A1                161    161
     D  QM10A1               162    171
     D  QMLTFL               172    172
     D  QMSCL#               173    197
     D  QMMCTM               198    201
     D  QMSTWK               202    203
     D  QMDEDT               204    210
     D  FILL7                211    211
     D  QMDEEM               212    213
     D  QMCMNU               214    216
     D  QMCMSU               217    219
     D  QMCLAD               220    222
     D  QMRRN                223    231  0
     D  QMLSN                232    261
     D  QMFSN                262    276
     D  QMMID                277    277
     D  QMDTRE               278    284
     D  FILL8                282    282
     D  QMCLSU               285    287
     D  QM#DEP               288    290
      *
     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
      *
     D                 DS                  INZ
     D  ED                     1     35  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D                 DS                  INZ
     D  CD                     1     35  0
     D                                     DIM(5) ASCEND                        CANCEL  DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D OPTNM           C                   CONST('Claims Accident -
     D                                     Description')
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('CLAIM FILE STATUS IS-
     D                                      OUT, USER -')
     D XCBC02          C                   CONST('          CLAIM FILE-
     D                                      STATUS IS IN')
     D COMAPI          C                   CONST('WA001R')
     D XCOROP          C                   CONST('ORIGINAL RESERVES')
      *** Beg   Add *** 04/05/12 ***********************************************
     D IXmo            s              1
     D IXmltd          s              3  0
     D QQrtcd          s              1
     D QQcall          s              1
     D ##mode          s              1
     D #ADky           s             10
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     C/COPY SCOPYMT05C
      *** End   Add *** 04/05/12 ***********************************************
      *
      *===============================================================
      *       FILE KEY LISTS
      *===============================================================
      *
      *   KEY LIST FOR THE ENTITY/LOCATION FILE
      *
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    CMCTG1
      *
      *    Claim Demographics
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
      *    Claim/Darts Control File
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
      *
      *    Employer Master
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
      *
      *    Group Master
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXFND
      *
      *  SYSTEM TABLE ID FILE
      *
     C     GIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
      *
      *  SYSTEM TABLE VALUE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR THE SYSTEM CONTROL FILE
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *  BAR CODE TRANSACTION FILE
      **
      **         TRKY1     KLIST
      **                   KFLD           XXKY1  20
      **                   KFLD           XXDT6   60
      *
      *  USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Key list for the EDI Control file
      *
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
      *
      *  Employer Cross Reference
      *
     C     EXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C     ELKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
      *
      *  Reserve History File
      *
     C     RHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
     C     ADKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    #ADKY
      *
      *** Beg   Del *** 04/05/12 ***********************************************
     C****               MOVEL     'CLMNCCI '    #ADKY            10
      *** End   Del *** 04/05/12 ***********************************************
      *===============================================================
      *       ENTRY PARM LIST
      *===============================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CONT            1
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     ACDPL3        PLIST
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
     C                   PARM                    @@CONT
      *
     C     IXAPI         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    XXJRST            2
     C                   PARM                    XXTRID            3
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
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
      *** Beg   Add *** 04/05/12 ***********************************************
     C                   PARM      *Blanks       IXcrer            5
     C                   PARM      *Blanks       IXceir            5
     C                   PARM      *Blanks       IXcecr            5
     C                   PARM      *Blanks       IXctar            5
     C                   PARM      *Blanks       IXctrr            5
     C                   PARM      *Blanks       IXmo
     C                   PARM      *Zeros        IXmltd
      *** End   Add *** 04/05/12 ***********************************************
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMEMP#        SVEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDIV         SVDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    CMSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXDT
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMDT          SVDATE                         CURRENT DATE
     C     *LIKE         DEFINE    CMTIME        SVTIME                         CURRENT TIME
     C     *LIKE         DEFINE    CMLTFL        IXLTFL
     C     *LIKE         DEFINE    CMNCCI        SVNCCI
      *
      *     Screen name
     C     *LIKE         DEFINE    SCPRDS        QQPRDS
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    GTKEY         SCKEY
      *
      *     Generic Table
     C     *LIKE         DEFINE    GTKEY         XXKEY                          Table Key
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTFG01        XXNCMD
     C     *LIKE         DEFINE    *IN60         XXMANC
     C     *LIKE         DEFINE    CMLTFL        SVLTFL
      *
      *     Parameters
      *** Beg   Del *** 04/05/12 ***********************************************
     C*******            MOVE      *BLANK        QQRTCD            1
     C*******            MOVE      *BLANK        QQCALL            1
     C*******            MOVE      @@MODE        ##MODE            1
      *
     C******/COPY SCOPYMT01C
      *** End   Del *** 04/05/12 ***********************************************
      *
      *===============================================================
      * CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
      *===============================================================
      *
     C     CALLR         BEGSR
      *
     C     SCRN#         IFEQ      #OFSCR
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDEQ     'A'
     C                   MOVE      'A'           @@MODE
     C                   END
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   CALL      'W0070R09'    ACDPL3
     C                   END
      *
     C     *INKL         IFEQ      *ON
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     'C'           @@MODE
     C                   END
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C                   CAS                     PREOTH
     C                   ENDCS
      *
     C                   MOVE      *ON           *IN90
     C                   END
     C                   END
     C                   ENDSR                                                  CALLR
      *
      *===============================================================
      * DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
      *===============================================================
      *
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *===============================================================
      * DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
      *===============================================================
      *
     C     DSPLYR        BEGSR
     C     1             SUBST(P)  Q2RECF:5      SCRN#A            1
     C                   MOVE      SCRN#A        SCRN#
      *
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
      *
     C                   ENDSR                                                  DSPLYR
      *
      *===============================================================
      * ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
      *===============================================================
      *
     C     ERRCK1        BEGSR
     C                   MOVE      *OFF          *IN19                          POS CUSR
      *
     C                   DO
      *
     C                   MOVEA     '000000'      *IN(21)                        RESET *IN21-26
     C                   MOVEA     '00'          *IN(91)                           91 - 92
      *
      * ACCIDENT DESCRIPTION
      *     - Cannot be blank
     C     SCACDS        IFEQ      *BLANK
     C     CLTID         ANDEQ     '065'
     C                   MOVE      '1'           *IN91                          WCR0503
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *________________________________________________________________
      *  LOST TIME FLAG
      *     - May not be blank
     C     SCLTFL        IFEQ      *BLANK
     C                   MOVE      '1'           *IN21                          WCR0503
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *
      *     - Must be I, M, or R
     C     SCLTFL        IFNE      'I'
     C     SCLTFL        ANDNE     'M'
     C     SCLTFL        ANDNE     'R'
     C                   MOVE      '1'           *IN22                          WCR0936
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *
      *  IF THERE ARE COMP PAYMENTS, THE LOST TIME FLAG MUST BE 'I'.
      *
     C     SCLTFL        IFNE      'I'
     C     XXCMP         ANDEQ     'Y'
     C                   MOVE      '1'           *IN24                          WCR2341
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *
      *     - Must be I if there are comp payments or reserves
     C     SCLTFL        IFEQ      'R'
     C     XXTINC        ANDGT     *ZERO
     C                   MOVE      '1'           *IN25                          WCR2342
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *
      *     -
     C     CMJRST        IFEQ      'PA'
     C     IXLTFL        IFEQ      'M'
     C     IXLTFL        OREQ      'R'
     C     SCLTFL        IFEQ      'I'
     C                   CALL      'WA720R'
     C                   PARM      '2'           XXMGID            1
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM      *BLANK        ##KL              1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *________________________________________________________________
      *  POTENTIAL LOST TIME FLAG
      *     - Must be Y OR N
     C     SCPLFL        IFNE      'Y'
     C     SCPLFL        ANDNE     'N'
     C     SCPLFL        ANDNE     *BLANK
     C                   MOVE      '1'           *IN26
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *________________________________________________________________
      *  MORE THAN ONE DAY
      *     - Must be Y OR N
     C     SCGT1         IFNE      'Y'
     C     SCGT1         ANDNE     'N'
     C     SCGT1         ANDNE     *BLANK
     C                   MOVE      '1'           *IN72
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
     C     SCGT1         IFEQ      *BLANK
     C     CLTID         ANDEQ     '065'
     C     *IN31         ANDEQ     *OFF
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       If SCgt2 <> 'Y' And SCgt2 <> 'N' And
        SCgt2 <> ' ';
          *In77 = '1';
          *In90 = '1';
       EndIf;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C*________________________________________________________________
     C*  EDI Claim Type
     C*     - Must be in Table file
     C*
     C     SCETYP        IFNE      *BLANKS
     C     *IN16         ANDEQ     *ON
     C                   MOVEL(P)  'CLTY'        XXTBID
     C                   MOVEL(P)  SCETYP        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCETDS
     C                   ELSE
     C                   MOVEL     *BLANKS       SCETDS
     C                   SETON                                        8990
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *________________________________________________________________
      *  NATURE FLAG
      *     - Must be in Table file
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              90
     C     *IN90         IFEQ      '1'
     C     NASTAT        OREQ      'I'
     C                   MOVE      '1'           *IN74
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *________________________________________________________________
      *  PART CODE
      *     - Must be in Table file
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCPART        XXKEY
     C     TBKY          CHAIN     PPA01                              90
     C     *IN90         IFEQ      '1'
     C     PASTAT        OREQ      'I'
     C                   MOVE      '1'           *IN75
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *________________________________________________________________
      *  CAUSE CODE
      *     - Must be in Table file
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              90
     C     *IN90         IFEQ      '1'
     C     CASTAT        OREQ      'I'
     C                   MOVE      '1'           *IN76
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *________________________________________________________________
      *  NCCI FLAG
      *     - Must be in Table file
      *
      **                   MOVELSCNCCI    XXKEY     P
      **                   MOVELSVNCTB    XXTBID    P
      **         GTKY      CHAINPGT01                60
      **         *IN60     IFEQ *ON
      **         GTSTAT    ORNE 'A'
      **                   SETON                     9078
      **                   LEAVE
      **                   ENDIF
      *________________________________________________________________
      *  NCCI FLAG
     C     SCLTFL        IFEQ      'I'
     C     SCLTFL        OREQ      'M'
     C     SCNCCI        ORNE      *BLANK
      *     - Must be in Table file
     C                   MOVEL(P)  SCNCCI        XXKEY
     C                   MOVE(P)   'NC'          XXTBID
      *
      *     - First try state table
     C                   MOVEL     CMJRST        XXTBID
     C     GIKY          CHAIN     PGI01                              90
     C  N90GTKY          CHAIN     PGT01                              90
      *
      *     - If not found, try generic NCCI table
     C     *IN90         IFEQ      '1'
     C     CMJRST        ANDNE     *BLANK
     C                   MOVEL     '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C                   ENDIF
      *
     C     *IN90         IFEQ      '1'
     C     GTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN78
     C                   MOVE      '1'           *IN90
     C                   LEAVE
      *
     C                   ELSE
      *
     C                   MOVEL(P)  GTDESC        SCNCDS
     C                   MOVE      GTFG01        XXNCMD
      *
      *     - Must be consistent with NCCI flag
      *
     C*
     C*     Do not do these edits for client 071
     C     CLTID         IFNE      '071'
     C     SCLTFL        IFEQ      'M'
     C     XXNCMD        ANDNE     'M'
     C                   MOVE      '1'           *IN23                          WCR2340
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
     C     SCLTFL        IFEQ      'I'
     C     XXNCMD        ANDEQ     'M'
     C                   MOVE      '1'           *IN23                          WCR2340
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
     C*     End client 071 condition
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN47
     C     SCLTFL        IFEQ      'I'
     C     SCLTFL        OREQ      'M'
     C                   MOVE      *ON           *IN47
     C                   ENDIF
      *
     C                   ENDIF
      *________________________________________________________________
      *  LOSS COVERAGE CODE
      *     - Must be in Table file
     C                   MOVEL(P)  SCLSCV        XXKEY
     C                   MOVE(P)   'LC'          XXTBID
      *
      *     - First try state table
     C                   MOVEL     CMJRST        XXTBID
     C     GIKY          CHAIN     PGI01                              90
     C  N90GTKY          CHAIN     PGT01                              90
      *
      *     - If not found, try generic NCCI table
     C     *IN90         IFEQ      '1'
     C     CMJRST        ANDNE     *BLANK
     C                   MOVEL     '  LC'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C                   ENDIF
      *
     C     *IN90         IFEQ      '1'
     C     GTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN80
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      * _______________________________________________________________
      *  LOSS COVERAGE CODE
      *     - Must be in Table file
      *
      **                   MOVELSCLSCV    XXKEY     P
      **                   MOVELSVLCTB    XXTBID    P
      **         GTKY      CHAINPGT01                60
      **         *IN60     IFEQ *ON
      **         GTSTAT    ORNE 'A'
      **                   SETON                     9080
      **                   LEAVE
      **                   ENDIF
      *________________________________________________________________
      *  LOSS RECOVERY TYPE
      *
     C     CXLSRV        IFEQ      'Y'
     C     SCLTFL        ANDEQ     'I'
     C     SCLSRV        ANDEQ     ' '
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
     C     SCLSRV        IFNE      *BLANK
     C                   MOVEL(P)  'LSRV'        XXTBID
     C                   MOVEL(P)  SCLSRV        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9079
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *________________________________________________________________
      *  TYPE OF COVERAGE
      *
     C     SCCOVT        IFNE      *BLANK
      *
      *     - Must be in Table file
     C                   MOVEL(P)  SCCOVT        XXKEY
     C                   MOVE(P)   'CT'          XXTBID
      *
      *     - First try state table
     C                   MOVEL     CMJRST        XXTBID
     C     GIKY          CHAIN     PGI01                              90
     C  N90GTKY          CHAIN     PGT01                              90
      *
      *     - If not found, try generic NCCI table
     C     *IN90         IFEQ      '1'
     C     CMJRST        ANDNE     *BLANK
     C                   MOVEL     '  CT'        XXTBID
     C     GTKY          CHAIN     PGT01                              90
     C                   ENDIF
      *
     C     *IN90         IFEQ      '1'
     C     GTSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN81
     C                   MOVE      '1'           *IN90
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF                                                  SCCOVT<>*BLANK
      * _______________________________________________________________
      *  RESERVE TYPE CODE
      *
     C     SCRTYP        IFNE      *BLANK
     C                   MOVEL(P)  'RSVT'        XXTBID
     C                   MOVEL(P)  SCRTYP        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9068
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C**N99      SVDATE    IFNE CMDT
     C  N99
     CANN40SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS
      *
     C                   Z-ADD     1             SCRN#             1 0
     C     *IN90         IFEQ      *OFF
     C                   SETON                                        19
     C                   ENDIF
     C                   ENDSR                                                  ERRCK1
      *
      *===============================================================
      * ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
      *===============================================================
      *
     C     ERRCK2        BEGSR
      *
     C                   MOVE      *OFF          *IN19                          POS CUSR
     C*
     C* DEVEREUX CAUSE MUST BE VALID - REQUIRED
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  'ZCAS'        XXTBID
     C                   MOVEL(P)  SCDCAU        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCDCDS
     C                   ENDIF
     C                   ENDIF
     C*
     C* DEVEREUX RESTRAINT TYPE MUST BE VALID IF CAUSE = 01
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDCAU        IFEQ      '01'
     C                   MOVEL(P)  'ZRET'        XXTBID
     C                   MOVEL(P)  SCDRET        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7290
     C                   ELSE
     C                   MOVEL(P)  GTDESC        SCDRDS
     C                   ENDIF
     C                   ELSE
     C*
     C* IF CAUSE IS NOT EQUAL TO 01 THEN FIELD MUST BE BLANK
     C*
     C     SCDRET        IFNE      *BLANKS
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C**N99      SVDATE    IFNE CMDT
     C  N99
     CANN40SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     2             SCRN#
     C                   ENDSR                                                  ERRCK2
      *
      *===============================================================
      * ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
      *===============================================================
      *
     C     ERRCK3        BEGSR
      *
      *  If another user has updated this claim, send an error message.
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
      *
     C**N99      SVDATE    IFNE CMDT
     C  N99
     CANN40SVDATE        IFNE      CMDT
     C     SVTIME        ORNE      CMTIME
      *
     C                   CALL      'WA017R'
     C                   PARM                    CMUSER
     C                   PARM                    QQRTCD
      *
     C                   MOVE      *ON           *IN90
     C                   ENDIF
      *
      *  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
      *
     C                   Z-ADD     3             SCRN#
     C                   ENDSR                                                  ERRCK3
      *
      *** Beg   Add *** 04/05/12 ***********************************************
       // Error check subroutine 4
      /Free
       Begsr Errck4;
       Endsr;
      /End-Free
       // Error check subroutine 5
      /Free
       Begsr Errck5;
       Endsr;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
      *===============================================================
      * KGDO - SUBROUTINE FOR F7 = FINANCAIL SUMMMARY
      *===============================================================
      *
      *  F7=CLAIMANT FINANCIAL INFO WINDOW
      *
     C     KGDO          BEGSR
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM                    @@KC
     C     *INKL         PARM                    @@KL
     C                   PARM                    @@MODE
     C                   PARM                    @@RRN
      *
     C     *INKC         IFEQ      *ON
     C                   MOVE      *OFF          *IN90
     C                   MOVE      'D'           @@MODE
     C                   END
      *
     C                   ENDSR
      *
      *===============================================================
      * KJDO -  F10 - SEARCH PROCESSING
      *===============================================================
     C     KJDO          BEGSR
      *
     C                   MOVEA     '00'          *IN(19)
     C                   MOVEA     '000000'      *IN(41)                           41 - 46
     C                   MOVE      *BLANK        SCKEY
     C                   MOVEL     ##DATA        QQPRDS
     C                   MOVE      *BLANK        QQRTCD
     C                   MOVE      *BLANK        QQCALL
      *
      *     Perform search for field where cursor is now; field
      *     name is in screen's CSRLOC field: HFLD
      *
     C   16HFLD          CASEQ     'SCETYP'      KJETYP
     C     HFLD          CASEQ     'SCNATR'      KJNATR
     C     HFLD          CASEQ     'SCPART'      KJPART
     C     HFLD          CASEQ     'SCCAUS'      KJCAUS
     C     HFLD          CASEQ     'SCNCCI'      KJNCCI
     C     HFLD          CASEQ     'SCLSCV'      KJLSCV
     C     HFLD          CASEQ     'SCLSRV'      KJLSRV
     C     HFLD          CASEQ     'SCCOVT'      KJCOVT
     C     HFLD          CASEQ     'SCRTYP'      KJRTYP
     C     HFLD          CASEQ     'SCDCAU'      KJDCAU
     C     HFLD          CASEQ     'SCDRET'      KJDRET
     C                   CAS                     KJERR
     C                   ENDCS                                                  E Case
      *
     C                   ENDSR                                                  KJDO
      *
      *
      *---------------------------------------------------------------
      *       Individual Search Subroutines
      *---------------------------------------------------------------
      *
      *      Nature code
     C     KJNATR        BEGSR
     C                   CALL      'W2121R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    SCKEY
     C                   PARM      2             @@VIEW            1 0
      *
     C                   MOVE      *ON           *IN41                          POS CUSR
      *
     C     *INKL         IFNE      *ON
     C     SCKEY         ANDNE     *BLANKS
      *
     C                   MOVEL(P)  SCKEY         SCNATR
     C                   MOVEL(P)  SCKEY         XXKEY
      *
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        SCNADS
     C                   MOVE      *ON           *IN42                          POS CUSR
     C                   MOVE      *OFF          *IN41
     C                   ELSE
     C                   MOVE      *ON           *IN41                          POS CUSR
     C                   MOVE(P)   *BLANK        SCNADS
     C                   END
     C                   END
     C                   ENDSR                                                  KJNATR
      *
      *---------------------------------------------------------------
      *
      *      Body part
     C     KJPART        BEGSR
     C                   CALL      'W2126R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             @@VIEW            1 0
      *
     C                   MOVE      *ON           *IN42                          POS CUSR
      *
     C     *INKL         IFNE      *ON
     C     SCKEY         ANDNE     *BLANKS
      *
     C                   MOVEL     SCKEY         SCPART
     C                   MOVEL(P)  SCKEY         XXKEY
      *
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        SCPADS
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   MOVE      *OFF          *IN42                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN42                          POS CUSR
     C                   MOVE(P)   *BLANK        SCPADS
     C                   END
     C                   END
     C                   ENDSR                                                  KJPART
      *
      *---------------------------------------------------------------
      *
      *      Cause code
     C     KJCAUS        BEGSR
     C                   CALL      'W2131R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
     C                   PARM      2             @@VIEW            1 0
      *
     C                   MOVE      *ON           *IN43                          POS CUSR
      *
     C     *INKL         IFNE      *ON
     C     SCKEY         ANDNE     *BLANKS
      *
     C                   MOVEL     SCKEY         SCCAUS
     C                   MOVEL(P)  SCKEY         XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        SCCADS
     C                   MOVE      *ON           *IN44                          POS CUSR
     C                   MOVE      *OFF          *IN43
     C                   ELSE
     C                   MOVE      *ON           *IN43                          POS CUSR
     C                   MOVE(P)   *BLANK        SCCADS
     C                   END
     C                   END
     C                   ENDSR                                                  KJCAUS
      *
      *---------------------------------------------------------------
      *
      *      NCCI Code
     C     KJNCCI        BEGSR
     C                   MOVE(P)   'NC'          XXTBID
     C                   MOVEL     CMJRST        XXTBID
     C                   Z-ADD     2             Y                 1 0
     C     GIKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  NC'        XXTBID
     C                   Z-ADD     1             Y
     C                   END
      *
      *      Try first in state table
      *
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD
      *
      *
     C                   MOVE      *ON           *IN44                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     SCKEY         SCNCCI
     C                   MOVEL(P)  SCKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCNCDS
     C                   MOVE      GTFG01        XXNCMD
     C                   MOVE      *ON           *IN45                          POS CUSR
     C                   MOVE      *OFF          *IN44
     C                   ELSE
     C                   MOVE      *ON           *IN44                          POS CUSR
     C                   MOVE      *BLANK        SCNCDS
     C                   END
     C                   END
     C                   ENDSR                                                  KJNCCI
      *
      *---------------------------------------------------------------
      *
      *      Loss coverage code
     C     KJLSCV        BEGSR
     C                   MOVE(P)   'LC'          XXTBID
     C                   MOVEL     CMJRST        XXTBID
     C                   Z-ADD     2             Y
     C     GIKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  LC'        XXTBID
     C                   Z-ADD     1             Y
     C                   END
      *
      *      Try again on generic table if not found in state table.
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C*
      *
     C                   MOVE      *ON           *IN45                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     SCKEY         SCLSCV
     C                   MOVEL(P)  SCKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCLSCD
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   MOVE      *OFF          *IN45
     C                   ELSE
     C                   MOVE      *ON           *IN45                          POS CUSR
     C                   MOVE      *BLANK        SCLSCD
     C                   END
     C                   END
      *
     C                   ENDSR                                                  KJLSCV
      *
      *---------------------------------------------------------------
      *
      *      LOSS RECOVERY TYPE
     C     KJLSRV        BEGSR
     C                   MOVEL(P)  'LSRV'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN46                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCLSRV
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCLSRD
     C                   MOVE      *ON           *IN48                          POS CUSR
     C                   MOVE      *OFF          *IN46
     C                   ELSE
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   MOVE      *BLANK        SCLSRD
     C                   END
     C                   END
     C                   ENDSR                                                  KJLSRV
     C*---------------------------------------------------------------
     C*      LOSS RECOVERY TYPE
     C*
     C     KJETYP        BEGSR
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'CLTY'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C                   MOVE      *ON           *IN32                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCETYP
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCETDS
     C                   MOVE      *ON           *IN41                          POS CUSR
     C                   MOVE      *OFF          *IN32
     C                   ELSE
     C                   MOVE      *ON           *IN32                          POS CUSR
     C                   MOVE      *BLANK        SCETDS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  KJETYP
      *
      *---------------------------------------------------------------
      *
      *      RESERVE TYPE CODE
      *
     C     KJRTYP        BEGSR
     C                   MOVEL(P)  'RSVT'        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN50                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCRTYP
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRTYD
     C                   MOVE      *ON           *IN50                          POS CUSR
     C                   MOVE      *OFF          *IN46
     C                   ELSE
     C                   MOVE      *ON           *IN46                          POS CUSR
     C                   MOVE      *BLANK        SCRTYD
     C                   END
     C                   END
     C                   ENDSR                                                  KJRTYP
      *
     C*================================================================
     C* DEVEREUX CAUSE SEARCH
     C*================================================================
     C     KJDCAU        BEGSR
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'ZCAS'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCDCAU
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCDCDS
     C                   MOVE      *ON           *IN32                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN31                          POS CUSR
     C                   MOVE      *BLANK        SCDCDS
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR                                                  KJDCAU
     C*================================================================
     C* DEVEREUX RESTRAINT TYPE SEARCH
     C*================================================================
     C     KJDRET        BEGSR
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'ZRET'        XXTBID
     C                   PARM      *BLANKS       XXKEY
     C                   PARM      *BLANKS       QQRTCD
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL(P)  XXKEY         SCDRET
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCDRDS
     C                   MOVE      *ON           *IN33                          POS CUSR
     C                   ELSE
     C                   MOVE      *ON           *IN32                          POS CUSR
     C                   MOVE      *BLANK        SCDRDS
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR                                                  KJDCAU
      *---------------------------------------------------------------
      *      Type of coverage
      *---------------------------------------------------------------
     C     KJCOVT        BEGSR
     C                   MOVE(P)   'CT'          XXTBID
     C                   MOVEL     CMJRST        XXTBID
     C                   Z-ADD     2             Y
     C     GIKY          CHAIN     PGI01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  CT'        XXTBID
     C                   Z-ADD     1             Y
     C                   END
      *
      *      Try again on generic table if not found in state table.
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXTBID
     C                   PARM                    SCKEY
     C                   PARM                    QQRTCD            1
     C*
      *
     C                   MOVE      *ON           *IN48                          POS CUSR
     C     QQRTCD        IFEQ      '*'
     C                   MOVEL     SCKEY         SCCOVT
     C                   MOVEL(P)  SCKEY         XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCOVN
     C                   MOVE      *ON           *IN19                          POS CUSR
     C                   MOVE      *OFF          *IN48
     C                   ELSE
     C                   MOVE      *ON           *IN48                          POS CUSR
     C                   MOVE      *BLANK        SCCOVN
     C                   ENDIF                                                  *IN60=*OFF
     C                   END                                                    QQRTCD='*'
      *
     C                   ENDSR                                                  KJCOVT
      *
      *---------------------------------------------------------------
      *
      *      Error -- cursor nowhere
     C     KJERR         BEGSR
     C                   MOVE      '1'           *IN20
     C                   ENDSR                                                  KJERR
      *
      *===============================================================
      * KNDO - SUBROUTINE FOR F14 = DARTS
      *===============================================================
      *
     C     KNDO          BEGSR
      *
      *  ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
      *
      *  CALL DARTS ENTRY PROGRAM
      *
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0070R'      XXPGM
      *
     C                   ENDSR                                                  KNDO
      *
      *===============================================================
      * KQDO - SUBROUTINE FOR F16 = I/M/R HISTORY
      *===============================================================
      *
     C     KQDO          BEGSR
      *
      *  LOAD PARMS
      *
     C                   MOVE      CMFND         QQFND
     C                   MOVE      CMFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
      *
      *  CALL I/M/R HISTORY SEARCH
      *
     C                   CALL      'W4320R'                             99
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND             3 0
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQRTCD
      *
      * IF F3=EXIT IS HIT, RETURN TO CLAIM MENU.
      *
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   MOVE      *OFF          *IN90
     C                   MOVE      @@MODE        SVMODE            1
     C                   MOVE      'D'           @@MODE
     C     Q1PRMS        IFGE      5
     C     @@CONT        IFNE      'A'                                          CHG/DSP
     C                   MOVE      *BLANK        @@CONT
     C                   END
     C     @@CONT        IFEQ      'A'                                          ADD MODE
     C                   MOVE      *OFF          *INKC
     C                   MOVE      *ON           *IN90
     C                   MOVE      SVMODE        @@MODE
     C                   END
     C                   END                                                    Q1PRMS >= 5
     C     Q2RECF        IFEQ      'SCR01'
     C                   Z-ADD     1             #OFSCR
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   Z-ADD     2             #OFSCR
     C                   END
     C                   END
      *
     C                   ENDSR                                                  KQDO
      *
      *===============================================================
      * KWDO - SUBROUTINE FOR F22
      *===============================================================
      *
      *F22=WITNESS INFORMATION
     C     KWDO          BEGSR
      *
      *  LOAD PARMS
      *
     C                   MOVE      XXFND         #@FND
     C                   MOVE      XXFYR         ##FYR
     C                   MOVE      XXCASE        ##CASE
     C                   CALL      'W0070RA'                            99
     C                   PARM      *BLANKS       QQPRDS
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    #@FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      *BLANKS       ##RTCD            1
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *INKL
     C                   ENDIF
     C                   ENDSR
      *===============================================================
      * MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
      *===============================================================
      *
     C     MVDBSC        BEGSR
      *
     C                   MOVEA     '000000'      *IN(21)                        RESET *IN21-26
      *
     C     *IN40         IFEQ      *ON
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
      *
      *  Load Claim Master data to keys
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMEMP#        SVEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C                   Z-ADD     CMDIV         SVDIV
     C                   Z-ADD     CMFYR         XXFYR
     C                   Z-ADD     CMCASE        SVCASE
     C                   Z-ADD     CMCASE        XXCASE
     C                   Z-ADD     CMTIME        SVTIME
     C                   Z-ADD     CMDT          SVDATE
     C                   MOVEL     *BLANKS       SCETYP
     C                   MOVEL(P)  CMETYP        SCETYP
     C                   MOVEL(P)  'CLTY'        XXTBID
     C                   MOVEL(P)  CMETYP        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCETDS
     C                   ELSE
     C                   MOVEL     *BLANKS       SCETDS
     C                   ENDIF
      *
      *  GET EXCESS REINSURANCE INFO
      *
     C                   MOVE      *BLANK        SCMSG1
     C                   MOVE      *OFF          *IN69
     C     CMEXCS        IFNE      'Y'
     C     @@RRN         IFNE      *ZERO
     C                   CALL      'WA010R'
     C                   PARM                    @@RRN                          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C                   END
      *
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVEL(P)  XCEXRE        SCMSG1
     C                   MOVE      *ON           *IN69
     C                   END
      *
      * GET SYSTEM CONTROL FILE
      *
     C                   MOVEL(P)  'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C     *IN60         IFEQ      *OFF
      *
      * BAR CODE INSTALLED
      *
     C     SYBCIF        IFEQ      'Y'
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         Q#FND
     C                   MOVE      XXFYR         Q#FYR
     C                   MOVE      XXCASE        Q#CASE
     C                   CALL      'BA001R'
     C                   PARM                    QQCO#             3
     C                   PARM                    Q#FND             3
     C                   PARM                    Q#FYR             3
     C                   PARM                    Q#CASE            7
     C                   PARM                    QQBCFL            1
     C                   PARM                    QQTRTP            1
     C                   PARM                    QQTUSR           10
     C                   PARM                    QQRTCD            1
      *
     C                   MOVE      *OFF          *IN70
     C                   MOVE      *BLANK        SCMSG2
     C     QQBCFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN70
     C     QQTRTP        IFEQ      'I'
     C                   MOVEL(P)  XCBC02        SCMSG2
     C                   ELSE
     C                   MOVEL(P)  XCBC01        SCMSG2
     C                   CAT       QQTUSR:1      SCMSG2
     C                   END
     C                   END
     C                   END
     C*
     C     1             SUBST(P)  SYFL11:1      XXFL11            1
      *
     C                   END                                                    *IN60  SSY01
      *
      *  Get data and move to screen fields
      *
      *      Fund Master
      *
     C                   Z-ADD     CMFND         SCFND
     C                   MOVE      *BLANK        SCFNAM
      *
      *  GET FUND NAME & IF FUND IS EXPIRED, THEN DISPLAY IN INQUIRY MODE
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  FNNAME        SCFNAM
     C     FNEXDT        IFGT      *ZERO
     C     FNEXDT        IFLE      XXDT
     C                   MOVE      *ON           *IN09
     C                   MOVE      'D'           @@MODE
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
     C                   END
     C                   END
      *
     C                   MOVE      *OFF          *IN31
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      'RAT'         XXTRID
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      *ON           *IN31
     C                   MOVE      'N'           XXEDI
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       *In33 = *On;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C                   ELSE
      *  Get the EDI control record to determine if the following field
      *    should be displayed and how many days it should reference.
      *    "If 'I', more than X days? . ."
      *
     C                   MOVE      CMJRST        XXJRST
     C                   MOVE      '148'         XXTRID
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C  N60              MOVE      'Y'           XXEDI             1
     C     *IN60         IFEQ      *ON
     C                   MOVE      'N'           XXEDI
     C                   MOVE      *BLANK        IXELT
     C                   MOVE      *ZEROS        IXELTD
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       IXmltd = *zeros;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C                   END
      *
     C                   MOVE      IXELTD        SCELTD
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       SCmltd = IXmltd;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C     IXELT         IFEQ      *BLANKS
     C     IXELT         OREQ      'N'
     C                   MOVE      '1'           *IN31
     C                   END
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       If IXmo = ' ' Or Ixmo = 'N';
          *In33 = '1';
       EndIf;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
      *
     C                   ENDIF
      *
      *     Employer Master
     C     EMKY          CHAIN     PEM01                              60
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    XXEMP#
     C                   PARM                    XXDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
      *
     C     *IN60         IFEQ      *OFF
      *
      * Get employer cancel date
      *
     C                   Z-ADD     5             X                 3 0
      *
     C     ED(X)         DOWEQ     *ZERO
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ENDDO
      *
     C     CD(X)         IFNE      *ZERO
     C                   SETOFF                                       49
     C                   Z-ADD     CD(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         SCCXDT
      *
     C                   ELSE
      *
     C                   SETON                                        49
     C                   ENDIF                                                  CD,X
      *
     C                   ELSE
      *
     C                   SETON                                        49
      *
     C                   ENDIF                                                  *IN60
      *
      *
      *  CLAIMANT SS#
      *
     C     CMSS#         IFNE      *BLANK
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C     SCSS#         CAT       SS2:0         SCSS#
     C     SCSS#         CAT       '-':0         SCSS#
     C     SCSS#         CAT       SS3:0         SCSS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYP            25
     C                   PARM      'CMSS#'       QQNAME           25
     C     CMSS#         PARM      CMSS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
      *
      *     Claimant Name
     C                   MOVEL     CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C                   END
      *
     C                   END
      *
      *  Accident date
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        XXACDT
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
      *
      *  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
      *
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
      *________________________________________________________________
      *
      *  CHECK FOR COMP RESERVES OR PAYMENTS
      *
     C                   XFOOT     RR            XXRR             11 2          RESERVES
     C                   XFOOT     AR            XXAR             11 2          PAYMENTS
     C     XXAR          ADD       XXRR          XXTINC           11 2          TOTAL INCURRED
      *
      *  CHECK FOR COMP PAYMENTS OR RESERVES
      *
     C                   MOVE      'N'           XXCMP             1
     C                   Z-ADD     1             I                 3 0
     C     I             DOUGT     10
     C                   MOVEL(P)  RT(I)         XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C  N60              MOVEL     RTTYPE        TP(I)
     C   60              MOVE      *BLANK        TP(I)
      *
     C     TP(I)         IFEQ      'C'
     C     AR(I)         ANDGT     0
     C     TP(I)         OREQ      'C'
     C     RR(I)         ANDGT     0
     C                   MOVE      'Y'           XXCMP
     C                   END
      *
     C                   ADD       1             I
     C                   ENDDO
      *________________________________________________________________
      *  SCR01 Fields
      *
      *        Accident description
     C                   MOVE      *BLANK        SCACDS
     C                   MOVE      *BLANK        SCACD2
     C                   MOVEL(P)  CMACDS        SCACDS
     C                   MOVEL(P)  CMACD2        SCACD2
      *
      *        Lost time flags
     C                   MOVE      *BLANK        SCLTFL
     C                   MOVE      *BLANK        SCPLFL
     C                   MOVE      *BLANK        SCGT1
     C                   MOVEL(P)  CMLTFL        SCLTFL
     C                   MOVEL(P)  CMLTFL        IXLTFL
     C                   MOVEL(P)  CMPLFL        SCPLFL
     C                   MOVEL(P)  CMGT1         SCGT1
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       SCgt2 = CMgt2;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
     C                   MOVEL(P)  CMLTFL        SVLTFL
      *
      *
      * IF LOST TIME FLAG IS 'I' OR 'M', NCCI CODE IS REQUIRED.
      *
     C                   MOVE      *OFF          *IN47
     C     CMLTFL        IFEQ      'I'
     C     CMLTFL        OREQ      'M'
     C                   MOVE      *ON           *IN47                          DSPATR(HI)
     C                   END
      *
      *  Accident description codes
      *  NATURE
     C                   MOVE      *BLANK        SCNATR
     C                   MOVEL(P)  CMNATR        SCNATR
     C                   MOVE      *BLANK        SCNADS
     C                   MOVEL(P)  CMNATR        XXKEY
     C     TBKY          CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  NADESC        SCNADS
     C                   END
      *  BODY PART
     C                   MOVE      *BLANK        SCPART
     C                   MOVEL(P)  CMPART        SCPART
     C                   MOVE      *BLANK        SCPADS
     C                   MOVEL(P)  CMPART        XXKEY
     C     TBKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  PADESC        SCPADS
     C                   END
      *  CAUSE
     C                   MOVE      *BLANK        SCCAUS
     C                   MOVEL(P)  CMCAUS        SCCAUS
     C                   MOVE      *BLANK        SCCADS
     C                   MOVEL(P)  CMCAUS        XXKEY
     C     TBKY          CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CADESC        SCCADS
     C                   END
      *  NCCI
     C                   MOVE      *BLANK        SCNCCI
     C                   MOVEL     CMNCCI        SCNCCI
     C                   MOVEL(P)  CMNCCI        SVNCCI
     C                   MOVE      *BLANK        SCNCDS
     C                   MOVEL(P)  CMJRST        XXTBID
     C                   MOVE      'NC'          XXTBID
     C                   MOVEL(P)  CMNCCI        XXKEY
     C     GIKY          CHAIN     PGI01                              60
     C  N60GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  NC'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   END
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCNCDS
     C                   MOVE      GTFG01        XXNCMD
     C                   END
      *
      *  LOSS COVERAGE
     C                   MOVE      *BLANK        SCLSCV
     C                   MOVEL     CMLSCV        SCLSCV
     C                   MOVE      *BLANK        SCLSCD
     C                   MOVEL(P)  CMJRST        XXTBID
     C                   MOVE      'LC'          XXTBID
     C                   MOVEL(P)  CMLSCV        XXKEY
     C     GIKY          CHAIN     PGI01                              60
     C  N60GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  LC'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   END
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCLSCD
     C                   END
      *
      *  LOSS RECOVERY TYPE
     C                   MOVE(P)   *BLANK        SCLSRV
     C                   MOVEL(P)  CMLSRV        SCLSRV
     C                   MOVE      *BLANK        SCLSRD
     C                   MOVEL(P)  'LSRV'        XXTBID
     C                   MOVEL(P)  CMLSRV        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCLSRD
     C                   END
      *
     C                   MOVE      *ON           *IN19                          POS CUSR
      *
      *  RESERVE TYPE CODEE
     C                   MOVE(P)   *BLANK        SCRTYP
     C                   MOVEL(P)  CMRSVT        SCRTYP
     C                   MOVE      *BLANK        SCRTYD
     C                   MOVEL(P)  'RSVT'        XXTBID
     C                   MOVEL(P)  CMRSVT        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCRTYD
     C                   END
      *
     C                   MOVE      *ON           *IN19                          POS CUSR
      *
      *  TYPE OF COVERAGE
     C                   MOVE      *BLANK        SCCOVT
     C                   MOVEL     CMCOVT        SCCOVT
     C                   MOVE      *BLANK        SCCOVN
     C                   MOVEL(P)  CMJRST        XXTBID
     C                   MOVE      'CT'          XXTBID
     C                   MOVEL(P)  CMCOVT        XXKEY
     C     GIKY          CHAIN     PGI01                              60
     C  N60GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  '  CT'        XXTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCOVN
     C                   ENDIF
     C*
     C     *IN40         IFEQ      *ON
      *** BEG DEL *** 06/27/12 **********************************
     C***********          CLEARSCDCAU
     C***********          CLEARSCDRET
     C***********          CLEARSCDPGM
      *** END DEL *** 06/27/12 **********************************
      *** BEG ADD *** 06/27/12 **********************************
     C                   MOVEL(P)  'ZCAS'        XXTBID
     C                   MOVEL(P)  CMDCAU        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C                   MOVEL(P)  CMDCAU        SCDCAU
     C                   CLEAR                   SCDCDS
     C  N99              MOVEL(P)  GTDESC        SCDCDS
     C*
     C                   MOVEL(P)  'ZRET'        XXTBID
     C                   MOVEL(P)  CMDRET        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C                   MOVEL(P)  CMDRET        SCDRET
     C                   CLEAR                   SCDRDS
     C                   MOVEL(P)  GTDESC        SCDRDS
     C*
     C                   MOVEL(P)  CMDPGM        SCDPGM
     C*
      *** END ADD *** 06/27/12 **********************************
     C                   CLEAR                   SCDCDS
     C                   CLEAR                   SCDRDS
     C                   ELSE
     C                   MOVEL(P)  'ZCAS'        XXTBID
     C                   MOVEL(P)  CMDCAU        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   SCDCAU
     C                   CLEAR                   SCDCDS
     C                   ELSE
     C                   MOVEL(P)  CMDCAU        SCDCAU
     C                   MOVEL(P)  GTDESC        SCDCDS
     C                   ENDIF
     C*
     C                   MOVEL(P)  'ZRET'        XXTBID
     C                   MOVEL(P)  CMDRET        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   SCDRET
     C                   CLEAR                   SCDRDS
     C                   ELSE
     C                   MOVEL(P)  CMDRET        SCDRET
     C                   MOVEL(P)  GTDESC        SCDRDS
     C                   ENDIF
     C*
     C                   MOVEL(P)  CMDPGM        SCDPGM
     C*
     C                   ENDIF
     C*
      *
     C                   ENDSR                                                  MVDBSC
      *
      *===============================================================
      * MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
      *===============================================================
      *
     C     MVSCDB        BEGSR
     C                   MOVEL(P)  SCSCL#        CMACL#
      *________________________________________________________________
      *  SCR01 Fields
      *
      *        Accident description
     C                   MOVEL(P)  SCACDS        CMACDS
     C                   MOVEL(P)  SCACD2        CMACD2
      *
      *        Lost time flags
     C                   MOVEL(P)  SCLTFL        CMLTFL
     C                   MOVEL(P)  SCETYP        CMETYP
     C                   MOVEL(P)  SCPLFL        CMPLFL
     C                   MOVEL(P)  SCGT1         CMGT1
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       CMgt2 = SCgt2;
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
      *
      *        Accident description codes
     C                   MOVEL(P)  SCNATR        CMNATR
     C                   MOVEL(P)  SCPART        CMPART
     C                   MOVEL(P)  SCCAUS        CMCAUS
     C                   MOVEL(P)  SCNCCI        CMNCCI
      *
     C                   MOVEL(P)  SCLSCV        CMLSCV
     C                   MOVEL(P)  SCLSRV        CMLSRV
     C                   MOVEL(P)  SCCOVT        CMCOVT
     C                   MOVEL(P)  SCRTYP        CMRSVT
     C                   MOVEL(P)  SCDCAU        CMDCAU
     C                   MOVEL(P)  SCDRET        CMDRET
     C                   MOVEL(P)  SCDPGM        CMDPGM
      *
     C                   ENDSR                                                  MVSCDB
      *
      *===============================================================
      * OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
      *===============================================================
      *
     C     OTHERR        BEGSR
     C                   ENDSR                                                   OTHERR
      *
      *===============================================================
      * OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
      *===============================================================
      *
     C     OTHFKS        BEGSR
      *
     C                   MOVEA     '00'          *IN(19)
     C                   MOVEA     '000000'      *IN(21)                        RESET *IN21-26
      *
     C     *INKG         CASEQ     *ON           KGDO                     90
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C     *INKQ         CASEQ     *ON           KQDO                     90
     C     *INKW         CASEQ     *ON           KWDO                     90
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *===============================================================
      * PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
      *===============================================================
      *
     C     PGMEND        BEGSR
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           @@MODE
     C                   END
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
      *
      * *******            CLOSEBATRL
     C                   RETURN
     C                   ENDSR
      *
      *===============================================================
      * PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
      *===============================================================
      *
     C     PGMSTR        BEGSR
     C                   Z-ADD     1             #OFSCR            1 0          # OF SCREENS
      *** Beg   Add *** 04/05/12 ***********************************************
      /Free
       QQrtcd = ' '    ;
       QQcall = ' '    ;
       ##mode = @@mode;
       #ADKY = 'CLMNCCI';
      /End-Free
      *** End   Add *** 04/05/12 ***********************************************
      *
      *   Establish Mode: inquiry vs maintain
     C     @@MODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09
     C                   ENDIF
      *
      *   Establish Add mode vs Update
      *                                 (NOTE: master record WMCMP has
      *                                 in all cases been created, even
      *                                 if mode is A = add).
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN40
     C                   MOVE      'C'           @@MODE
     C                   MOVEL     MODEA         SCMODE
     C                   ELSE
     C                   MOVE      *OFF          *IN40
     C                   ENDIF
      *
     C     *IN09         IFEQ      *ON
     C                   MOVEL(P)  MODED         SCMODE
     C                   END
      *
     C                   MOVE      *ON           DSPTYP
     C*
     C*  ESTABLISH CLIENT NUMBER
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C* Check for client 139
     C*
     C                   SETOFF                                       16
     C     CLTID         IFEQ      '139'
     C                   SETON                                        16
     C                   ENDIF
      *
      * GET WORKERS COMP CONTROL FILE
      *
     C                   MOVEL(P)  'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
      *
      * COMMERCIAL APPLICATION INSTALLED
      *
     C                   MOVE      *OFF          *IN66
     C     WCFL07        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   END
      *
     C                   END
      *
      *     WCMC Installed determination
      *
     C                   CALL      'SA001R'
     C                   PARM                    QQWCIF            1
     C                   PARM                    QQFBIF            1
     C                   PARM                    QQFL01            1
      *
     C     QQFBIF        IFEQ      'Y'
     C     QQFL01        ANDEQ     'Y'
     C                   MOVE      *ON           XXMANC
     C                   ELSE
     C                   MOVE      *OFF          XXMANC
     C                   ENDIF
      *
      *  GET THE USER PARAMETER FILE
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
      *
      *  GET SECURITY LEVEL OF THE USER
      *
     C                   MOVE      *BLANK        XXAUTH            1
     C                   MOVE      *OFF          *IN67
     C     UPLVL         IFGE      WCSL03
     C                   MOVE      *ON           *IN67
     C                   MOVE      'Y'           XXAUTH                         AUTHORIZED
     C                   END
      *
     C                   END
      *
      *  GET CURRENT DATE, CONVERT TO 7,0  TO CHAIN TO FILES
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
      *
      *
      *
      *  Get Employer Cross Reference info for header
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EXKY          CHAIN     PEX01                              63
     C  N63              Z-ADD     EXOFND        SCOFND
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
     C*
     C* IF THIS CLAIM BELONGS TO A DEVEREUX GROUP THEN MAKE SCREEN 2
     C* AVAILABLE
     C*
     C                   Z-ADD     1             #OFSCR
     C                   Z-ADD     CMFND         XXFND
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
     C     CXDEVF        ANDEQ     'Y'
     C                   Z-ADD     2             #OFSCR
     C                   ENDIF
      *
      *
      *
      *
     C                   CALL      'S6000R'
     C                   PARM                    @@WCMC            1            WCMC INSTALLED
     C                   PARM                    @@CHK             1            CHECK REC
     C                   PARM                    @@BAR             1            INTRFACD W/WCMS
     C                   PARM                    @@DED             1
     C                   PARM                    @@ADMN            1
     C                   PARM                    @@CLM             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@MICR            1
     C     @@EDI         IFEQ      'Y'
     C                   MOVE      *ON           *IN35
     C                   ELSE
     C                   MOVE      *OFF          *IN35
     C                   ENDIF
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
      *===============================================================
      *
     C     PREOTH        BEGSR
     C                   ENDSR
      *
      *===============================================================
      * UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
      *===============================================================
      *
     C     UPDATR        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             9997
      *
B02  C     *IN97         IFEQ      *ON
B02  C     FLSTCM        ANDEQ     01218
     C                   EXSR      GETUSR
     C                   CALL      'WA017R'
     C                   PARM                    QQUSER           10
     C                   PARM                    QQRTCD
     C                   MOVE      *ON           *IN99                          SKIP TO BOTTOM
     C                   END
      *
B02  C     *IN99         IFEQ      *OFF
      *
      *
      *  MOVE FIELDS FOR TRANSFER LOST TIME - MED ONLY CHANGE HISTORY
      *
     C     CMLTFL        IFNE      SCLTFL
     C                   MOVE      CMLTFL        THFROM
     C                   MOVE      SCLTFL        THTO
      *
     C                   Z-ADD     XXCO#         THCO#
     C                   Z-ADD     XXFND         THFND
     C                   Z-ADD     XXFYR         THFYR
     C                   Z-ADD     XXCASE        THCASE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         THTRDT
     C                   MOVE      *BLANK        THSTAT
     C                   MOVEL     Q1USER        THUSER
     C                   MOVEL     Q1PGM         THPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          THDT
     C                   Z-ADD     DSTIME        THTIME
     C                   WRITE     PTH01                                        |
     C                   END
      *
     C*  CHAIN TO GET THE EXCLUDE MEDICAL FLAG.
     C     ELKY          CHAIN     PEL01                              60
     C*
     C*
     C*
     C*  If the incurred amount is 0, and no reserve history records
     C*  exist, continue with the reserve default process.
     C*
     C     RHKY          CHAIN     PRH02                              99
     C     *IN99         DOWEQ     *OFF
     C     RHIPGM        IFNE      'W8608R'
     C                   LEAVE
     C                   ENDIF
     C     RHKY          READE     PRH02                                  99
     C                   ENDDO
     C     *IN99         IFEQ      *ON
     C     XXTINC        ANDEQ     0
     C*
     C*     For PDSQuest customers , if no codes have been
     C*       entered yet, get from group record.
     C     XXFL11        IFEQ      'Y'
     C                   MOVE      FNRS01        CMRT01
     C                   MOVE      FNRS02        CMRT02
     C                   MOVE      FNRS03        CMRT03
     C                   MOVE      FNRS04        CMRT04
     C                   MOVE      FNRS05        CMRT05
     C                   MOVE      FNRS06        CMRT06
     C                   MOVE      FNRS07        CMRT07
     C                   MOVE      FNRS08        CMRT08
     C                   MOVE      FNRS09        CMRT09
     C                   MOVE      FNRS10        CMRT10
     C                   ENDIF                                                  e XXFL11=Y
     C*
     C     SCLTFL        IFEQ      'M'
     C     CMSTAT        ANDEQ     'O'
     C*
     C                   CLEAR                   PRH01
     C*
     C*    Move in the medical default, if there.
     C     FNDFRC        IFNE      *BLANK
     C     ELEXMF        ANDEQ     ' '
     C*
     C                   Z-ADD     1             I
     C     FNDFRC        LOOKUP    RT(I)                                  99
     C     *IN99         IFEQ      *OFF
     C     '  '          LOOKUP    RT(I)                                  99
     C                   ENDIF                                                  *IN99 OFF
     C                   MOVE      FNDFRC        RT(I)
     C                   MOVE      FNDFRA        RR(I)
     C                   MOVE      FNDFRC        RRT(I)
     C                   MOVE      FNDFRA        RSU(I)
     C                   ENDIF                                                  FRC <> ' '
     C*
     C*    Move in the expense default, if there.
     C     FNDFXC        IFNE      *BLANK
     C*
     C                   Z-ADD     2             I
     C     FNDFXC        LOOKUP    RT(I)                                  99
     C     *IN99         IFEQ      *OFF
     C     '  '          LOOKUP    RT(I)                                  99
     C                   ENDIF                                                  *IN99 OFF
     C                   MOVE      FNDFXC        RT(I)
     C                   MOVE      FNDFXA        RR(I)
     C                   MOVE      FNDFXC        RRT(I)
     C                   MOVE      FNDFXA        RSU(I)
     C                   ENDIF                                                  FXC <> ' '
     C*
     C*      Test for amount in reserve history array.
     C                   XFOOT     RSU           TOTSU            11 2
     C*
     C*      If default amount present, write reserve history.
     C     TOTSU         IFNE      *ZERO
     C                   MOVE      XXCO#         RHCO#
     C                   MOVE      XXFND         RHFND
     C                   Z-ADD     XXFYR         RHFYR
     C                   Z-ADD     XXCASE        RHCASE
     C                   Z-ADD     XXDT          RHEFDT
     C                   MOVEL     XCOROP        RHEXPL                         ORIGINAL OPEN
     C                   MOVE      *BLANK        RHSTAT
     C                   MOVE      *BLANK        RHCLST
     C                   MOVEL     Q1USER        RHUSER
     C                   MOVEL     Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RHTIME
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
      *
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
      *
     C                   WRITE     PRH01
     C*
     C*        Update detail outstanding total.
     C                   ADD       TOTSU         ELOUTS
     C  N60              UPDATE    PEL01
     C                   SETON                                            60
     C                   ENDIF                                                  e TOTSU <> 0
     C*
     C                   ENDIF                                                  end LTFL=M
     C                   ENDIF                                                  end RT = ' '
      *
     C  N60              UPDATE    PEL01
      *
     C**N60*********       UPDATPEL01
     C*
      *
      *
     C                   EXSR      MVSCDB
      *
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     XXDT          CMDT
     C                   MOVEL     DSTIME        CMTIME
     C     CMENTD        IFEQ      *ZERO
     C     CMLTFL        ANDNE     'R'
     C                   Z-ADD     CMDT          CMENTD
     C                   ENDIF                                                  e ENTD = 0  DATE
      *
      *    Call API to update Managed Care Claim Master - FMCMP
      *
      *
     C                   UPDATE    DBRCDF
      *
      *    Call API to update Managed Care Claim Master - FMCMP
      *
     C     XXMANC        IFEQ      *ON
     C                   CLEAR                   PARMCL
     C                   EXSR      MCAPIC
     C                   ENDIF                                                  MANC ON     DATE
      *
      *  If this pgm was called using option 8 and EDI is active, call
      *    the EDI Log API.
      *
     C     Q1PRMS        IFLT      5
     C     WCFL09        ANDEQ     'Y'
     C     XXEDI         ANDEQ     'Y'
     C     SCRN#         ANDEQ     #OFSCR
      *
     C                   MOVE      XXCO#         QACO#
     C                   MOVE      CMFND         QAFND
     C                   MOVE      CMFYR         QAFYR
     C                   MOVE      CMCASE        QACASE
     C                   CALL      'W0070R6'
     C                   PARM                    QACO#             3
     C                   PARM                    QAFND             3
     C                   PARM                    QAFYR             3
     C                   PARM                    QACASE            7
     C                   PARM      *BLANK        QQACTF            1
     C                   PARM      *BLANKS       QQMGID            7
     C                   PARM                    QQRTCD            1
      *
     C                   END
     C*
     C     SVLTFL        IFNE      CMLTFL
     C*
     C     CLTID         IFNE      '025'
     C*
     C                   SELECT
     C     SVLTFL        WHENEQ    'R'
     C     CMLTFL        ANDNE     'R'
     C                   CALL      'WA049R'
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RTCD            1
     C*
     C     SVLTFL        WHENEQ    'M'
     C     CMLTFL        ANDEQ     'I'
     C     ##MODE        ANDNE     'A'
     C                   CALL      'WA049R'
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RTCD            1
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*  If lost time flag changed, call Auto DARTS.
     C     *IN40         IFEQ      *OFF
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'CLMCHG02'    @@DKEY           10
     C                   PARM                    @@COMM          256
     C                   ENDIF
     C*
     C                   ENDIF
     C*  If the NCCI injury code was changed, write a history record.
     C*
     C     SVNCCI        IFNE      *BLANKS
     C     SVNCCI        ANDNE     SCNCCI
      *
     C                   Z-ADD     XXCO#         IHCO#
     C                   Z-ADD     XXFND         IHFND
     C                   Z-ADD     XXFYR         IHFYR
     C                   Z-ADD     XXCASE        IHCASE
     C                   Z-ADD     XXDT          IHTRDT
     C                   MOVE      SVNCCI        IHFROM
     C                   MOVE      SCNCCI        IHTO
     C                   MOVE      *BLANK        IHSTAT
     C                   MOVEL     Q1USER        IHUSER
     C                   MOVEL     Q1PGM         IHPGM
     C                   Z-ADD     XXDT          IHDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        IHTIME
     C                   WRITE     PIH01
      *
     C                   END
      *
     C     SCNCCI        IFNE      *BLANKS
     C     ADKY          SETLL     WMADL
     C     ADKY          READE     WMADL                                  95
     C     *IN95         IFEQ      *OFF
     C     SCNCCI        IFEQ      ADNCC1
     C     SCNCCI        OREQ      ADNCC2
     C     SCNCCI        OREQ      ADNCC3
      *
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'CLMNCCI '    @@DKEY           10
     C                   PARM                    @@COMM          256
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      * CALL NEXT PROGRAM IF F6, OPT 2, OR 5 WAS SELECTED FROM CLM MASTER
      *
     C     Q1PRMS        IFGE      5
     C     @@CONT        ANDNE     *BLANK
     C                   EXSR      CALLR
     C                   END
      *
E02  C                   ENDIF                                                  *IN99=*OFF
      *
     C                   MOVE      *ON           OUTFLG
      *
     C                   ENDSR                                                  UPDATR
      *
      *===============================================================
      * WRITER - SUBROUTINE FOR WRITING DATA BASE RECORDS
      *===============================================================
      *
     C     WRITER        BEGSR
     C                   ENDSR                                                  WRITER
      *===============================================================
      * MCAPIC - DATA INTERFACE: MC CLAIM MASTER - FMCMP
      *===============================================================
      *
     C     MCAPIC        BEGSR
      *
     C                   MOVE      CMACDT        QMACDT
      *
     C                   MOVE      CMEMP#        QMEMP#
     C                   MOVE      CMDIV         QMDIV
      *
     C                   MOVE      XXCO#         QMCO#
     C                   MOVE      *BLANKS       QMSUB#
     C                   MOVE      XXFND         QMFND
     C                   MOVE      XXFYR         QMFYR
     C                   MOVE      XXCASE        QMCASE
      *
     C                   MOVE      CMACTM        QMACTM                         Accidnt time
     C                   MOVE(P)   CMDTRP        QMDTRP                         Reported
     C                   MOVE(P)   CMDTLW        QMDTLW                         Last worked
     C                   MOVE(P)   CMENTD        QMENTD                         ENTERED
     C                   MOVE(P)   CMRCVD        QMRCVD                         Received
     C                   MOVE(P)   CMMMI         QMMMI                          Max med impr
     C                   MOVE(P)   CMDOD         QMDEDT                         Death date
     C     CMDTH         IFEQ      'Y'                                          Death flag
     C                   MOVEL     'DI'          QMDEEM
     C                   ELSE
     C                   MOVEL     *BLANKS       QMDEEM
     C                   ENDIF
      *
     C                   Z-ADD     DBRRN         QMRRN
     C                   MOVE(P)   CMDTRE        QMDTRE                         Rehabilitation
     C                   MOVE      CM#DEP        QM#DEP                         # dependents
     C                   MOVEL(P)  CMSHFT        QQWKPT                         Shift
      *
      *===============================================================
     C                   CALL      'FA003R'
      *
     C                   PARM      *BLANK        QMRTCD            1
     C                   PARM      CMSS#         QMSS#             9
     C                   PARM                    QMACDT            7
      *
     C                   PARM                    QMCO#             3
     C                   PARM      *BLANK        QMSUB#            5
     C                   PARM                    QMFND             3
     C                   PARM                    QMFYR             3
     C                   PARM                    QMCASE            7
      *
     C                   PARM                    QMEMP#            9
     C                   PARM                    QMDIV             5
     C                   PARM                    QMACTM            6
     C                   PARM                    QMDTRP            7
     C                   PARM                    QMDTLW            7
     C                   PARM      CMACDS        QMACDS           60            Acc description
     C                   PARM      CMMCON        QMMCON            1            Method contact
     C                   PARM                    QMENTD            7
     C                   PARM                    QMRCVD            7
     C                   PARM                    QMMMI             7
     C                   PARM      *BLANKS       QM1A1             1            Unavailable
     C                   PARM      *BLANKS       QM10A1           10            Unavailable
     C                   PARM      CMLTFL        QMLTFL            1            Lost time flag
     C                   PARM      CMACL#        QMSCL#           25            State claim numbr
     C                   PARM      *BLANKS       QMMCTM            4            Unavailable
     C                   PARM      *BLANKS       QMSTWK            2            Unavailable
     C                   PARM                    QMDEDT            7
     C                   PARM                    QMDEEM            2            Death claim flag
     C                   PARM      CMNRSE        QMCMNU            3            Nurse
     C                   PARM      *BLANKS       QMCMSU            3            Unavailable
     C                   PARM      CMCLEX        QMCLAD            3            Claim examiner
     C                   PARM                    QMRRN             9 0
     C                   PARM      CMLSN         QMLSN            30            Last name
     C                   PARM      CMFSN         QMFSN            15            First name
     C                   PARM      CMMID         QMMID             1            Middle initial
     C                   PARM                    QMDTRE            7
     C                   PARM      CMSUPR        QMCLSU            3            Supervisor
     C                   PARM                    QM#DEP            3
     C                   PARM      CMOCDS        QQOCDS           40            OCDS
     C                   PARM      CMHIDT        QQHIDT            7 0          HIDT
     C                   PARM      CMESTS        QQESTS            2            ESTS
     C                   PARM                    QQWKPT            2            SHFT
     C                   PARM      CMPREX        QQPREX            1            PREX
     C                   PARM      CMACD2        QQACD2           60            ACD2
     C                   PARM      CMCVFL        QQCVFL            1            CVFL
     C                   PARM      CMMCO         QQMCA             1            MCO
     C                   PARM      CMIPCT        QQMMIP            5 2          IPCT
     C                   PARM      CMODIS        QQODIS            1            ODIS
     C                   PARM      CMYRLX        QQYRLX            4 0          YRLX
     C                   PARM      CMDBDT        QQDBDT            7 0          DBDT
     C                   PARM      CMHRSD        QQHRDY            3 0          HRSD
     C                   PARM      CMDAYS        QQDYWK            1 0          DAYS
     C                   PARM      CMFEAD        QQCLFA            3            FEAD
     C                   PARM      CMDNY         QQDNY             1            DENIED
     C                   PARM      CMIMP         QQIMP             1            IMPAIRMENT
     C                   PARM      CMJRST        QQJRST            2            JUR STATE
      *
     C                   ENDSR
      *----------------------------------------------------------------
      * GETUSR - GET USER WHO HAS LOCK ON CLAIM MASTER
      *----------------------------------------------------------------
     C     GETUSR        BEGSR
     C     Q1RED         IFGT      *BLANK
     C                   MOVEA     Q1RED         ARX
     C                   Z-ADD     1             IX                3 0
      *
     C     ARX(IX)       DOUEQ     '/'
     C     IX            OREQ      80
     C                   ADD       1             IX
     C                   ENDDO
      *
     C     ARX(IX)       IFEQ      '/'
     C                   ADD       1             IX
     C                   MOVEA     ARX(IX)       USR
      *
     C                   Z-ADD     1             IX
     C     '/'           LOOKUP    USR(IX)                                61
      *
     C     *IN61         IFEQ      *ON
     C     IX            DO        10            IY                3 0
     C                   MOVE      *BLANKS       USR(IY)
     C                   ENDDO
     C                   ENDIF
      *
     C                   MOVEA     USR           QQUSER
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
