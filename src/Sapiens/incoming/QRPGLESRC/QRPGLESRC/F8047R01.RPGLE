     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*                                                               *
     H*****************************************************************
     H*  F8047R01 - Upload Bill Information                           *
     H*                                                               *
     H*  Called from: F8047R - Work with Upload Bills with Errors     *
     H*                                                               *
     H*  Calls to   : F8042R - Upload Upload Bills - Transfer Pgm 2   *
     H*   Or                                                          *
     H*  Calls to   : F8042RA- Upload Upload Bills - Transfer Pgm 2   *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/2004 by c:62 for Project 51MFN              *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*  06/18/18 C:079 Support: 146 71291                            *
     H*                 Add detail line reconsideration error msg.    *
     H*                                                               *
     H*  05/01/17 C:079 Project: MC325                                *
     H*                 Add logic for SA016R                          *
     H*                                                               *
     H*  06/23/16 C:309 Project : 45MKI                               *
     H*                 Changes for Reconsiderations                  *
     H*                 Added two ouput fields ScErmsbil1 & ScErmsbil2*
     H*                 ScErmsbil2 for future use                     *
     H*                                                               *
     H*  01/19/15 C:296 Project: 129MTU                               *
     H*                 Modify to skip the edit for inactive provider.*
     H*                                                               *
     H*  08/27/13 C:062 SUPPORT MC133 83257                           *
     H*                 Prevent overlapping messages.                 *
     H*                                                               *
     H*  04/16/12 C:280 SUPPORT 79605
     H*                 CORRECT ERROR ON CANCEL FROM TRANSFER
     H*                                                               *
     H*  03/22/12 C:180 SUPPORT 79569
     H*                 Prevent dec data error on prov mod.
     H*
     H*  12/14/10 C:251 PROJECT: 146MDQ                               *
     H*                 Check U1PROV for zeros and amounts for blanks *
     H*                                                               *
     H*  09/23/10 C:079 PROJECT: MC346                                *
     H*                 Use U1PROV for provider information           *
     H*                                                               *
     H*  08/23/10 C:079 SUPPORT: 73980                                *
     H*                 Use IN29 for Record only claims               *
     H*                                                               *
     H*  03/05/10 C:220 PROJECT: 136MEX                               *
     H*                 Med Bill Upload Use Prov FEIN/Mod             *
     H*                                                               *
     H*  11/02/09 C:232 PROJECT: 139WMR                               *
     H*                 OFAC Reporting-Add Pending Status Indicator.  *
     H*                                                               *
     H*  06/08/09 C:198 PROJECT 136MEI                                *
     H*                 CHANGED TO  HAVE THE ABILITY TO DISPLAY       *
     H*                 'RECORD ONLY' FILES.                          *
     H*                                                               *
     H*  08/04/08 C:47  PROJECT: 144MBK                               *
     H*                 Only check for duplicate request/payment
     H*                   if WCMS is installed.
     H*                                                               *
     H*  10/16/07 C:062 PROJECT: MC307  (COMPILE ONLY)                *
     H*                 COMPILE ONLY                                  *
     H*                                                               *
     H*  09/27/07 C:193 PROJECT: 139MIA (COMPILE ONLY)                *
     H*                 MCMC UPLOAD - ERROR LIST CHANGE               *
     H*                                                               *
     H*  06/30/06 C:62 PROJECT 132MFO                                 *
     H*                -ADD SERVICE DATE PROCESSING                   *
     H*                                                               *
     H*  10/25/05 C:163 Project: 136MBR                               *
     H*                 Managed Care/Prov Bill Interface Process      *
     H*                                                               *
     H*  08/15/05 C:62 PROJECT 126MCT                                 *
     H*                -ADD DUPLICATE INFO MESSAGE                    *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8047F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAU1P     UF A E             DISK
     F                                     RENAME(PU101:DBRCDF)
     FFMC1L     UF   E           K DISK
     FFMCMLB    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FFMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM03)
     FFMPDL     IF   E           K DISK
     F***** Beg Add ***** 01/19/15 ******************************
     FNMNWL     IF   E           K DISK
     F***** End Add ***** 01/19/15 ******************************
     f*** Beg Add *** 06/18/18 *************************************
     ffau2l1    if   e           k disk
     f*** End Add *** 06/18/18 *************************************

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************
     d*** Beg Add *** 05/01/17 ***********************************
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1
     d  @@Cchg         s              1
     d  @@Cifs         s              1
     d  @@Csts         s              1
     d  @@Cdny         s              1
     d  @@Ccvfl        s              1
     d  @@Cgrp         s              1
     d  @@Cemp         s              1
     d  @@Eifs         s              1
     d  @@Eagt         s              1
     d  @@Eemp         s              1
     d  @@Fifs         s              1
     d  @@Ftyp         s              1
     d  @@Udup         s              1
     d  @@Udrc         s              1
     d  @@Urel         s              1
     d  @@Upcl         s              1
     d  @@Updn         s              1
     d  @@Udpn         s              1
     d  @@ClDays       s              5  0
     d  @@IncZro       s              1
     d  @@Crcd         s              1
     d  @@CrDth        s              1
     d  @@RtCd         s              1
     d  isSettings     s              1
     d*** End Add *** 05/01/17 ***********************************
     d*** Beg Add *** 06/18/18 ***********************************
     d  xxRlin         s              1
     d*** End Add *** 06/18/18 ***********************************
      /COPY SCOPYMT01D
     D*
     D CLM             S              1    DIM(20)
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  VALUE                  1     11
     D  SIGN                   1      1
     D  AMOUNT                 2     11  2
     D                 DS
     D  CLAIM#                 1     20
     D  GROUP                  1      3
     D  YEAR                   4      6
     D  CASE                   7     13
     D**** BEGIN CHANGE *** 03/22/12 ****************************
     D******* BEG ADD  ****** 03/05/10 ************************************
     D**U1PROV      DS
     D**                                      1   90U1FN1
     D**                                     10  120U1MD1
     D******* END ADD  ****** 03/05/10 ************************************
     D U1PROV          DS
     D  U1FN1                  1      9  0
     D  U1MD1                 10     12
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Upload Bill -
     D                                     Transfer')
     D OPTNMT          C                   CONST('Upload Bill -
     D                                     Transfer')
     D OPTNMD          C                   CONST('Upload Bill -
     D                                     Deletion')
     D COMAPI          C                   CONST('FA007R')
     D*** Beg Add *** 12/14/10 ****************************************
     D XXZVAL          C                   CONST('0000000000000')

     I*** End Add *** 12/14/10 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C*** BEGIN ADD *** 08/15/05 **************************************
     C                   PARM                    @@RTCD            1
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C*
     C     CMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FYR
     C                   KFLD                    ##CASE
     C*
     C     CMKY3         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMSS#         ##SS#
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMSUB#        ##SUB#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##FYR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    PDFEIN        HDFEIN
     C     *LIKE         DEFINE    PDMOD         HDMOD
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK - Error checking for why this bill wasn't processed.
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
     C*
     C* LETS SEE IF THIS IS A RECONSIDERATION
     C*
     C                   EXSR      GETCLM
     C*
     C     U1PRFL        IFEQ      'E'
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C     CMKY3         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        COUNT             3 0
     C                   MOVEL     U1SS#         KKSS#
     C                   Z-ADD     U1DOI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         KKACDT
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             COUNT
     C     CMKY1         READE     PCM01                                  99
     C                   ENDDO
     C                   ELSE
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* SS#/ACDT doesn't exist.
     C*
     C     COUNT         WHENEQ    0
     C                   SETON                                        21
     C*
     C* Let's get the WCMS group number
     C*
     C     COUNT         WHENEQ    1
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMSUB#        SCSUB#
     C                   MOVE      CMFND         SCFND
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C* SS#/ACDT occurs more than once.
     C*
     C     COUNT         WHENGT    1
     C                   SETON                                        23
     C*
     C                   ENDSL
     C*
     C* FEIN/MOD does not exist.
     C*
     C                   MOVE      U1FEIN        KKFEIN
     C                   Z-ADD     *ZEROS        KKMOD
     C*** BEG ADD *** 03/05/10 ****************************************
     C*** BEG DEL *** 09/23/10 ****************************************
     C********** XXCLT#    IFEQ '136'
     C*** END DEL *** 09/23/10 ****************************************
     C     U1PROV        IFNE      *BLANKS
     C*** Beg Add *** 12/14/10 ****************************************
     C     U1PROV        ANDNE     XXZVAL
     C*** End Add *** 12/14/10 ****************************************
     C                   MOVE      U1FN1         KKFEIN
     C*** BEGIN CHANGE *** 03/22/12 ***********************************
     C***                  Z-ADDU1MD1     KKMOD
     C                   MOVE      U1MD1         KKMOD
     C*** END   CHANGE *** 03/22/12 ***********************************
     C                   ENDIF
     C*** BEG DEL *** 09/23/10 ****************************************
     C****************     ENDIF
     C*** END DEL *** 09/23/10 ****************************************
     C*** END ADD *** 03/05/10 ****************************************
     C*
     C     PDKY          CHAIN     PPD01                              99
     C****** Beg Chg ***** 01/19/15 *****************************
     C* Modify to skip the edit for inactive provider,
     C* if flag NMNWP/NWFL03 = Y.
     C******     *IN99         IFEQ      *ON
     C******     *IN99         OREQ      *OFF
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDEQ'I'
     C******     PDSTAT        ANDNE     'A'
     C******     PDSTAT        ANDNE     *BLANKS
     C*** END CHG *** 11/02/09 **************************************
     C******             SETON                                        22
     C******             MOVEL     'Y'           XXCHG
     C******             ENDIF
      /Free
       If *in99 = *on;                                           //Provider not found.
          *in22 = *on;
          xxChg = 'Y';
       Else;                                                     //Provider found.
       Chain (##CO#:U1NtWk) PNW01;
          If %found(NMNWL) and NWFL03 = 'Y';                     //Check if ina. prov. flg. is on.
             // Skip the check for inactive provider.
          Elseif PDstat <> 'A' and PDstat <> *blanks;             //Check if prov. is active.
             *in22 = *on;
             xxChg = 'Y';
          Endif;
       Endif;
      /End-Free
     C****** End Chg ***** 01/19/15 *****************************
     C*
     C                   ELSE
     C*
     C* WHAT KIND OF RECON ERROR
     C*
     C     U1RCNF        IFEQ      'Y'
     C****** Beg Add *** 06/23/16 **************************
      /Free
           if XXCLT#='045';
             ScErmsBil1 = 'EOB being reconsidered is :' + U1oTrid;
             erBill1= *ON;
           endif;
      /End-Free
     C****** End Add *** 06/23/16 **************************
     C                   SETON                                        24
     C                   ELSE
     C                   SETON                                        25
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY3         CHAIN     PCM02                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        COUNT             3 0
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     SCSS#         KKSS#
      /Free
       kkSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   Z-ADD     SCACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C     *IN99         DOWEQ     *OFF
     C                   ADD       1             COUNT
     C     CMKY1         READE     PCM01                                  99
     C                   ENDDO
     C                   ELSE
     C                   Z-ADD     1             COUNT
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* SS#/ACDT doesn't exist.
     C*
     C     COUNT         WHENEQ    0
     C                   SETON                                        7190
     C*
     C* SS#/ACDT occurs more than once.
     C*
     C     COUNT         WHENGT    1
     C                   SETON                                        75
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* FEIN/MOD does not exist.
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C****** Beg Chg ***** 01/19/15 *****************************
     C* Modify to skip the edit for inactive provider,
     C* if flag NMNWP/NWFL03 = Y.
     C******     *IN99         IFEQ      *ON
     C******     *IN99         OREQ      *OFF
     C*** BEG CHG *** 11/02/09 **************************************
     C****       PDSTAT    ANDEQ'I'
     C******     PDSTAT        ANDNE     'A'
     C******     PDSTAT        ANDNE     *BLANKS
     C*** BEG CHG *** 11/02/09 **************************************
     C******                   SETON                                        7290
     C******                   ENDIF
      /Free
       If *in99 = *on;                                           //Provider not found.
          *in72 = *on;
          *in90 = *on;
       Else;                                                     //Provider found.
       Chain (##CO#:U1NtWk) PNW01;
          If %found(NMNWL) and NWFL03 = 'Y';                     //Check if ina. prov. flg. is on.
             // Skip the check for inactive provider.
          Elseif PDstat <> 'A' and PDstat <> *blanks;             //Check if prov. is active.
             *in72 = *on;
             *in90 = *on;
          Endif;
       Endif;
      /End-Free
     C****** End Chg ***** 01/19/15 *****************************
     C                   ENDIF
     C*
     C* CHECK TO SEE IF MULTIPLE CASES
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN75         ANDEQ     *ON
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       SCSS# = UnMaskData('SSN':'SCSS#':SCSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   CALL      'F0267R'
     C     *INKC         PARM      *BLANKS       ##KC              1
     C     *INKL         PARM      *BLANKS       ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCSS#         ##SS#
     C                   PARM      SCACDT        ##ACDT
     C                   PARM      'Y'           ##SDOI            1            Y = subset on doi
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C     ##RRN         ANDNE     *ZEROS
     C     ##RRN         CHAIN     PCM03                              99
     C                   ELSE
     C                   SETON                                        90
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 08/15/05 **************************************
     C*
     C* CHECK TO SEE IF DUPLICATE
     C*
     C                   MOVE      U1DLRE        XXDERR
     C*
     C                   SETOFF                                       91
     C     *IN90         IFEQ      *OFF
     C     XXDERR        ANDEQ     *BLANKS
     C*
     C                   MOVE      U1PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        $#AMT
     C                   ELSE
     C                   Z-ADD     AMOUNT        $#AMT
     C                   ENDIF
     C*
     C     $#AMT         IFNE      *ZEROS
     C*
     C*
     C                   Z-ADD     U1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     U1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        $#FND
     C                   ELSE
     C                   Z-ADD     *ZEROS        $#FND
     C                   ENDIF
     C*
     C* Check for duplicate request/payment
     C*   if WCMS is installed.
     C*
     C*** BEG   ADD *** 08/04/08 **************************************
     C     ##WCIF        IFEQ      'Y'
     C*** END   ADD *** 08/04/08 **************************************
     C*
     C                   CALL      'WA052R'
     C                   PARM      CMCO#         $#CO#             3 0          Company #
     C                   PARM                    $#FND             3 0          Fund
     C                   PARM      CMFYR         $#FYR             3 0          Fund Year
     C                   PARM      CMCASE        $#CASE            7 0          Case
     C                   PARM      XXFRDT        $#FROM            7 0          From Date
     C                   PARM      XXTODT        $#TO              7 0          To Date
     C                   PARM                    $#AMT             9 2          Amount
     C                   PARM      'ME'          $#RECD            2            Reserve Code
     C                   PARM      PDFEIN        $#FEIN            9 0          FEIN #
     C                   PARM      'M'           $#RETP            1            Reserve Type
     C                   PARM      *BLANKS       $#RTCD            2
     C*
     C     $#RTCD        IFNE      *BLANKS
     C                   EXFMT     WDW01
     C*** BEG   CHG *** 04/16/12 **************************************
     C******INKL         IFEQ      *ON
 |   C     *INKL         IFEQ      *OFF
     C***********        SETON                                        91
     C************       SETOFF                                       KL
     C*** END   CHG *** 04/16/12 **************************************
     C*** Beg Add *** 05/01/17 *******************************************
     C     isSettings    ifne      '1'
     C*** End Add *** 05/01/17 *******************************************
     C     $#RTCD        IFEQ      'DR'
     C     $#RTCD        OREQ      'DP'
     C                   MOVE      'D'           XXDERR
     C                   MOVEL     'D'           @@RTCD
     C                   ELSE
     C                   MOVE      'P'           XXDERR
     C                   MOVEL     'P'           @@RTCD
     C                   ENDIF
     C*** Beg Add *** 05/01/17 *******************************************
     C                   endif
       //* Check for duplicates using the new settings file
       //* If @@Udup = blank then dont check for duplicates at all
      /free
           if $#rtCd <> ' ' and isSettings = '1';
           //* Complete duplicates
              if @@Udup = 'C' or @@Udup = 'A';
                 if $#rtCd = 'DR' or $#rtCd = 'DP';
                    xxDerr = 'D';
                    @@RtCd = 'D';
                 endif;
              endif;
           //* Partial duplicates
              if @@Udup = 'P';
                 if $#rtCd = 'PR' or $#rtCd = 'PP';
                    xxDerr = 'P';
                    @@RtCd = 'P';
                 endif;
              endif;
           endif;
      /end-free
       //*** End Add *** 05/01/17 **************************************
     C*** End Add *** 05/01/17 *******************************************
     C*** BEG   ADD *** 04/16/12 **************************************
     C                   ELSE
     C                   SETON                                        91
     C                   SETOFF                                       KL
     C                   MOVEL     'E'           @@RTCD
     C*** END   ADD *** 04/16/12 **************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG   ADD *** 08/04/08 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/04/08 **************************************
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*** BEGIN ADD *** 06/30/06 **************************************
     C     *IN90         IFEQ      *OFF
     C     *IN91         ANDEQ     *OFF
     C     XXCLT#        ANDEQ     '132'
     C*
     C* Check for line items less than the accident date
     C*
     C                   CALL      'F8042R1'
     C                   PARM      U1TRID        ##TRID           15
     C                   PARM      CMACDT        #@ACDT            7 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'F'
     C                   EXFMT     WDW02
     C     *INKL         IFEQ      *ON
     C                   SETON                                        91
     C                   SETOFF                                       KL
     C                   MOVE      'S'           XXDERR
     C                   MOVEL     'S'           @@RTCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 06/30/06 **************************************
     C*
     C                   ELSE
     C*
     C* DELETION REASON CAN'T BE BLANKS
     C*
     C     SCDLRE        IFEQ      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C*
     C* HAS THE PROVIDER CHANGED
     C*
     C     *IN90         IFEQ      *OFF
     C     XXCHG         ANDNE     'Y'
     C     SCFEIN        IFNE      HDFEIN
     C     SCMOD         ORNE      HDMOD
     C                   MOVEL     'Y'           XXCHG
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* GETCLM - GET THE CLAIM NUMBER
     C*================================================================
     C     GETCLM        BEGSR
     C*
     C                   MOVEL     *BLANKS       XXCLM#           20
     C                   Z-ADD     *ZEROS        XXONE             1 0
     C                   Z-ADD     *ZEROS        XXTWO             2 0
     C                   Z-ADD     *ZEROS        XXTHR             3 0
     C                   Z-ADD     *ZEROS        XXFOU             4 0
     C                   Z-ADD     *ZEROS        XXFIV             5 0
     C                   Z-ADD     *ZEROS        XXSIX             6 0
     C                   Z-ADD     *ZEROS        XXSEV             7 0
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   Z-ADD     *ZEROS        KKSUB#
     C                   Z-ADD     *ZEROS        KKFND
     C                   Z-ADD     *ZEROS        KKFYR
     C                   Z-ADD     *ZEROS        KKCASE
     C*
     C* LET'S EXTRACT THE GROUP
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     U1CLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     '-'           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     U1CLM#        XXONE
     C                   Z-ADD     XXONE         KKWCF#
     C     X             WHENEQ    3
     C                   MOVEL     U1CLM#        XXTWO
     C                   Z-ADD     XXTWO         KKWCF#
     C     X             WHENEQ    4
     C                   MOVEL     U1CLM#        XXTHR
     C                   Z-ADD     XXTHR         KKWCF#
     C                   ENDSL
     C*
     C                   ADD       1             X
     C                   MOVEA(P)  CLM(X)        XXCLM#
     C*
     C* LET'S GET THE YEAR
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     XXCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     '-'           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     XXCLM#        XXONE
     C                   Z-ADD     XXONE         SCFYR
     C     X             WHENEQ    3
     C                   MOVEL     XXCLM#        XXTWO
     C                   Z-ADD     XXTWO         SCFYR
     C     X             WHENEQ    4
     C                   MOVEL     XXCLM#        XXTHR
     C                   Z-ADD     XXTHR         SCFYR
     C                   ENDSL
     C                   ADD       1             X
     C                   MOVEA(P)  CLM(X)        XXCLM#
     C                   ENDIF
     C*
     C* LET'S GET THE YEAR
     C*
     C                   CLEAR                   CLM
     C                   MOVEA     XXCLM#        CLM
     C                   Z-ADD     1             X                 3 0
     C     ' '           LOOKUP    CLM(X)                                 99
     C     *IN99         IFEQ      *ON
     C                   SELECT
     C     X             WHENEQ    2
     C                   MOVEL     XXCLM#        XXONE
     C                   Z-ADD     XXONE         SCCASE
     C     X             WHENEQ    3
     C                   MOVEL     XXCLM#        XXTWO
     C                   Z-ADD     XXTWO         SCCASE
     C     X             WHENEQ    4
     C                   MOVEL     XXCLM#        XXTHR
     C                   Z-ADD     XXTHR         SCCASE
     C     X             WHENEQ    5
     C                   MOVEL     XXCLM#        XXFOU
     C                   Z-ADD     XXFOU         SCCASE
     C     X             WHENEQ    6
     C                   MOVEL     XXCLM#        XXFIV
     C                   Z-ADD     XXFIV         SCCASE
     C     X             WHENEQ    7
     C                   MOVEL     XXCLM#        XXSIX
     C                   Z-ADD     XXSIX         SCCASE
     C     X             WHENEQ    8
     C                   MOVEL     XXCLM#        XXSEV
     C                   Z-ADD     XXSEV         SCCASE
     C                   ENDSL
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     U1CLM#        CLAIM#
     C                   MOVE      GROUP         KKWCF#
     C                   MOVE      YEAR          SCFYR
     C                   MOVE      CASE          SCCASE
     C                   ENDIF
     C*
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      KKWCF#        #1WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1SUB#        SCSUB#
     C                   MOVE      #1FND         SCFND
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCSUB#'
     C     HFLD          OREQ      'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##SUB#
     C                   PARM      *ZEROS        ##FND
     C                   PARM      *ZEROS        ##FYR
     C                   PARM      *ZEROS        ##CASE
     C                   PARM      *BLANKS       ##SS#
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C                   SETON                                        3190
     C                   ELSE
     C     CMKY2         CHAIN     PCM02                              99
     C*
     C                   MOVEL(P)  CMLSN         ##LAST
     C                   MOVEL(P)  CMFSN         ##FRST
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   MOVE      '1'           ##TERM
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST           56
     C                   PARM                    ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  ##SS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'##SS#':##SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     ##ACDT        SCACDT
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C* PROVIDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   SETON                                        9032
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C*
     C                   CALL      'FA725R'
     C                   PARM      SCFEIN        #1FEIN            9 0
     C                   PARM      SCMOD         #1MOD             3 0
     C                   PARM      *BLANKS       #1NAME           40
     C                   PARM      *BLANKS       #1ERR             1
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SCPNAM
     C                   ENDIF
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  U1TRID        SCTRID
     C*
     C                   CALL      'SRNAME'
     C                   PARM      U1CLSN        ##LAST           56
     C                   PARM      U1CFSN        ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
      ***** Beg Add *** 06/09/20 *******************************************
     C***                MOVEL(P)  U1SS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'U1SS#':U1SS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C                   Z-ADD     U1DOI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      U1FEIN        SCFEIN
     C                   Z-ADD     *ZEROS        SCMOD
     C*
     C*** BEG ADD *** 03/05/10 ****************************************
     C*** BEG DEL *** 09/23/10 ****************************************
     C*********  XXCLT#    IFEQ '136'
     C*** END DEL *** 09/23/10 ****************************************
     C     U1PROV        IFNE      *BLANKS
     C*** Beg Add *** 12/14/10 ****************************************
     C     U1PROV        ANDNE     XXZVAL
     C*** End Add *** 12/14/10 ****************************************
     C                   MOVE      U1FN1         SCFEIN
     C*** BEGIN CHANGE *** 03/22/12 ***********************************
     C***                  Z-ADDU1MD1     SCMOD
     C                   MOVE      U1MD1         SCMOD
     C*** END   CHANGE *** 03/22/12 ***********************************
     C                   ENDIF
     C*** BEG DEL *** 09/23/10 ****************************************
     C*******************  ENDIF                            CLT136
     C*** END DEL *** 09/23/10 ****************************************
     C*** END ADD *** 03/05/10 ****************************************
     C*
     C*** BEG ADD *** 09/23/10 ****************************************
     C     U1PROV        IFEQ      *BLANKS
     C*** END ADD *** 09/23/10 ****************************************
     C*** Beg Add *** 12/14/10 ****************************************
     C     U1PROV        OREQ      XXZVAL
     C*** End Add *** 12/14/10 ****************************************
     C     U1PFSN        IFEQ      *BLANKS
     C                   MOVEL(P)  U1PNAM        SCPNAM
     C                   ELSE
     C     U1PFSN        CAT(P)    U1PNAM:1      SCPNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  U1ADR1        SCPAD1
     C                   MOVEL(P)  U1CTY         SCPCTY
     C                   MOVEL(P)  U1ST          SCPST
     C                   MOVE      U1ZIP         SCPZIP
     C*** BEG ADD *** 09/23/10 ****************************************
     C                   ENDIF
     C*
     C     U1PROV        IFNE      *BLANKS
     C*** Beg Add *** 12/14/10 ****************************************
     C     U1PROV        ANDNE     XXZVAL
     C*** End Add *** 12/14/10 ****************************************
     C                   MOVE      U1FN1         KKFEIN
     C*** BEGIN CHANGE *** 03/22/12 ***********************************
     C***                  Z-ADDU1MD1     KKMOD
     C                   MOVE      U1MD1         KKMOD
     C*** END   CHANGE *** 03/22/12 ***********************************
     C*
     C     PDKY          CHAIN     PPD01                              99
     C****** Beg Chg ***** 01/19/15 *****************************
     C* Modify to skip the edit for inactive provider,
     C* if flag NMNWP/NWFL03 = Y.
     C******     *IN99         IFEQ      *OFF
     C******     PDSTAT        ANDEQ     'A'
      /Free
       If *in99 = *off;
          Chain (##CO#:U1NtWk) PNW01;
             If (%found(NMNWL) and NWFL03 = 'Y') or PDstat = 'A';
      /End-Free
     C****** End Chg ***** 01/19/15 *****************************
     C*
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVEL(P)  PDNAME        SCPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      SCPNAM
     C                   ENDIF
     C*
     C                   MOVEL(P)  PDADR1        SCPAD1
     C                   MOVEL(P)  PDCTY         SCPCTY
     C                   MOVEL(P)  PDST          SCPST
     C                   MOVE      PDZIP         SCPZIP
     C*
     C****** Beg Chg ***** 01/19/15 *****************************
     C******             ENDIF
      /Free
             Endif;
       Endif;
      /End-Free
     C****** End Chg ***** 01/19/15 *****************************
     C*
     C                   ENDIF
     C*** END ADD *** 09/23/10 ****************************************
     C*
     C                   Z-ADD     SCFEIN        HDFEIN
     C                   Z-ADD     SCMOD         HDMOD
     C*
     C                   MOVEL     *BLANKS       SCDLRE
     C*
     C     *IN16         IFEQ      *OFF
     C                   EXSR      ERRCK
     C                   ENDIF
     C*
     C                   SETOFF                                       17
     C*** BEGIN ADD *** 08/15/05 **************************************
     C                   SETOFF                                       2627
     C                   MOVE      U1DLRE        XXDERR            1
     C*** Begin Add *** 08/27/13 **************************************
     C     XXDERR        IFNE      *BLANKS
     C     XXDERR        IFEQ      'D'
     C                   SETON                                        26
     C                   ELSE
     C                   SETON                                        27
     C                   ENDIF
     C                   SETON                                        17
     C                   ENDIF
     C*** End   Add *** 08/27/13 **************************************
     C*** END   ADD *** 08/15/05 **************************************
     C     U1PRFL        IFEQ      'R'
     C*** Begin Del *** 08/27/13 **************************************
     C*** BEGIN ADD *** 08/15/05 **************************************
     C*****XXDERR        ORNE      *BLANKS
     C*****XXDERR        IFEQ      'D'
     C**********         SETON                                        26
     C**********         ELSE
     C**********         SETON                                        27
     C**********         ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*** End   Del *** 08/27/13 **************************************
     C                   SETON                                        17
     C                   ENDIF
     C*** BEG ADD *** 06/08/09 ****************************************
     C     XXDERR        IFEQ      'R'
     C*** BEG CHG *** 08/23/10 ****************************************
     C*******************  SETON                     26
     C                   SETON                                        29
     C*** END CHG *** 08/23/10 ****************************************
     C                   ENDIF
     C*** END ADD *** 06/08/09 ****************************************
       //*** Beg Add *** 06/18/18 *************************************
       //
       // Check to see if recon error and detail lines not present or
       // missing recon line number
       //
      /Free
           if U1prFl = 'R' and U1rcnF = 'Y';
              Chain(n) U1trId pu201;
              if %Found(fau2l1);
                 xxRlin = 'N';
                 dow not %Eof(fau2l1);
                    If u2rLin > 0;
                       xxRlin = 'Y';
                    Endif;
                    Reade(n) U1trId pu201;
                 Enddo;
                 If xxRlin = 'N';
                    ScErmsBil2 ='Reconsideration error: Detail +
                       lines missing or recon line number(s) +
                       missing';
                    erBill2= *on;
                    *in17 = *off;
                 Endif;
               Else;
                  ScErmsBil2 ='Reconsideration error: Detail +
                       lines missing or recon line number(s) +
                       missing';
                  erBill2= *on;
                  *in17 = *off;
               Endif;
           endif;
      /End-Free
       //*** End Add *** 06/18/18 *************************************
     C*
     C                   MOVE      U1CHRG        VALUE
     C*
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        SCCHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        SCCHRG
     C                   ENDIF
     C*
     C                   MOVE      U1MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        SCMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        SCMRA
     C                   ENDIF
     C*
     C                   MOVE      U1NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        SCNDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        SCNDIS
     C                   ENDIF
     C*
     C                   MOVE      U1PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        SCPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        SCPAID
     C                   ENDIF
     C*
     C                   Z-ADD     U1COST        SCCOST
     C*
     C                   Z-ADD     U1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     U1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q1USER        U1DLUS
     C                   MOVEL(P)  Q1PGM         U1DLPG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        U1DLTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         U1DLDT
     C                   MOVEL     SRDTO         U1PRDT
     C*
     C*** BEGIN CHG *** 08/15/05 **************************************
     C**********           MOVELSCDLRE    U1DLRE    P
     C                   MOVEL     SCDLRE        U1DLRE
     C*** END   CHG *** 08/15/05 **************************************
     C                   MOVEL(P)  'D'           U1PRFL
     C*
     C                   Z-ADD     *ZEROS        U1CO#
     C                   Z-ADD     *ZEROS        U1SUB#
     C                   Z-ADD     *ZEROS        U1FND
     C                   Z-ADD     *ZEROS        U1FYR
     C                   Z-ADD     *ZEROS        U1CASE
     C                   Z-ADD     *ZEROS        U1EOB#
     C                   MOVEL     *BLANKS       U1PSS#
     C                   Z-ADD     *ZEROS        U1PFEI
     C                   Z-ADD     *ZEROS        U1MOD
     C                   Z-ADD     *ZEROS        U1ACDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     CMCO#         U1CO#
     C                   Z-ADD     CMSUB#        U1SUB#
     C                   Z-ADD     CMFND         U1FND
     C                   Z-ADD     CMFYR         U1FYR
     C                   Z-ADD     CMCASE        U1CASE
     C                   MOVEL     CMSS#         U1PSS#
     C                   Z-ADD     CMACDT        U1ACDT
     C                   Z-ADD     PDFEIN        U1PFEI
     C                   Z-ADD     PDMOD         U1MOD
     C*
     C*** BEGIN ADD *** 08/15/05 **************************************
     C     *IN91         IFEQ      *OFF
     C*** END   ADD *** 08/15/05 **************************************
     C     U1EOB#        IFEQ      *ZEROS
     C     C1KY          CHAIN     PC101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             C1EOB#
     C                   UPDATE    PC101
     C                   ENDIF
     C                   Z-ADD     C1EOB#        U1EOB#
     C                   ENDIF
     C*** BEGIN ADD *** 08/15/05 **************************************
     C                   ELSE
     C                   MOVE      XXDERR        U1DLRE
     C                   ENDIF
     C*** END   ADD *** 08/15/05 **************************************
     C*
     C                   Z-ADD     *ZEROS        U1RCN
     C                   MOVEL     *BLANKS       U1DLUS
     C                   MOVEL     *BLANKS       U1DLPG
     C                   Z-ADD     *ZEROS        U1DLTM
     C                   Z-ADD     *ZEROS        U1DLDT
     C*** BEGIN CHG *** 08/15/05 **************************************
     C**********           MOVEL*BLANKS   U1DLRE
     C                   MOVE      *BLANKS       XXDLRE           69
     C                   MOVEL     XXDLRE        U1DLRE
     C*** END   CHG *** 08/15/05 **************************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   SETOFF                                       3132
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       0916
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@CALL        COMP      'D'                                    16
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C                   MOVEL     'N'           XXCHG             1
     C*
     C     *IN16         IFEQ      *OFF
     C                   MOVEL(P)  OPTNMT        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNMD        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDX
     C*** BEGIN CHG *** 10/25/05 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*** END   CHG *** 10/25/05 **************************************
     C*** BEG   ADD *** 08/04/08 **************************************
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
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##WCIF        IFEQ      'Y'
     C                   ENDIF
     C*** END   ADD *** 08/04/08 **************************************
     C****** Beg Add *** 06/23/16 **************************
      /Free
         ScErmsBil1= *Blanks;
         ScErmsBil2= *Blanks;
         erBill1   = *Off;
         erBill2   = *Off;
      /End-Free
     C****** End Add *** 06/23/16 **************************
     C****** Beg Add *** 05/01/17 **************************
      /Free
            rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                         @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                         @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                         @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd);
                         if @@RtCd = '1';
                           isSettings = '1';
                         Else;
                           isSettings = '0';
                         Endif;
      /End-Free
     C****** End Add *** 05/01/17 **************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*
     C     *IN16         IFEQ      *OFF
     C*** BEGIN ADD *** 06/30/06 **************************************
     C     *IN91         ANDEQ     *OFF
     C*** END   ADD *** 06/30/06 **************************************
     C*** BEGIN CHG *** 10/25/05 **************************************
     C     XXCLT#        IFEQ      '136'
     C                   CALL      'F8042RA'
     C                   PARM      *BLANKS       ##NAME           10            Document ID
     C                   PARM      *ZEROS        ##RRN             9 0          FAU9P RRN
     C                   PARM      @@RRN         ##RRN2            9 0          FAU1P RRN
     C                   PARM      XXCHG         ##PRVC            1            PROV CHANGE
     C                   ELSE
     C                   CALL      'F8042R'
     C                   PARM      *BLANKS       ##NAME           10            Document ID
     C                   PARM      *ZEROS        ##RRN             9 0          FAU9P RRN
     C                   PARM      @@RRN         ##RRN2            9 0          FAU1P RRN
     C                   PARM      XXCHG         ##PRVC            1            PROV CHANGE
     C                   ENDIF
     C*** END   CHG *** 10/25/05 **************************************
     C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAU1P
     C*
     C                   ENDSR
