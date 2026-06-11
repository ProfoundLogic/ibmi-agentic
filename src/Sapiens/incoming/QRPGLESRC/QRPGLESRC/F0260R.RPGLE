     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  F0260R - Medical Case Information   C:62 03/25/98            *
     H*                                                               *
     H*  CALLED FROM: F0269R                                          *
     H*               F0268R                                          *
     H*               F0267R                                          *
     H*  CALLS TO   : FA701R                                          *
     H*               FA702R                                          *
     H*               FA703R                                          *
     H*               F0175R                                          *
     H*               F0275R                                          *
     H*               F0276R                                          *
     H*               SA001R                                          *
     H*               WA107R
     H*               W0070R5                                         *
     H*               W0270R5                                         *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                  *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     h*                                                               *
     h*  04/10/19 C:062 Project: 152WAB                               *
     h*                 Apply CSI mods for S1 Medical.                *
     h*                                                               *
     h*  05/09/16 C:300 Project: 114WXV                               *
     h*                 Add Cell Number to Claimant Demographics.     *
     H*                                                               *
     H*  02/11/16 C:307 SUPPORT:37159                                 *
     H*                 To subset non adjudication state list and     *
     H*                 not to show in provider bill and work with    *
     H*                 medical cases.                                *
     H*                                                               *
     H*  06/22/11 C:58 Project 114MYS                                 *
     H*                Relax some edits if Group is non-WCMS          *
     H*                                                               *
     H*  03/12/08 C:62 PROJECT 62635                                  *
     H*                UPDATE NURSE FIELD ALL THE TIME.               *
     H*                                                               *
     H*  09/18/07 C:180 SUPPORT:60098
     H*                 Restore version with 139MEL changes from
     H*                 V7R1PNSRC + Copy 139MHK (Add catastrophic
     H*                 flag and case mgt dates) from WW/C062 version.
     H*
     H*  08/20/07 C:53 SUPPORT: 59363                                 *
     H*                CHANGES FOR 114WAS ONLY FOR 114                *
     H*                                                               *
     H*  06/20/07 C:53 114WMF  BACK TO BASE-114WAS                    *
     H*  01/26/01 C:61 PROJECT 114WAS                                 *
     H*                Protect update of case nurse if WCMS/WCMC      *
     H*                linked.                                        *
     H*                                                               *
     H*  06/24/07 C:079 Project: 114WMF                              *
     H*                 MAKE SURE CALL TO F0275R HAS THE CORRECT MODE *
     H*                 AND CALL                                     *
     H*                                                               *
     H*  01/22/07 C:062 Project:                                     *
     H*                 GET PROGRAM ID FROM API.                     *
     H*                                                               *
     H*  12/12/06 C:062 Project: 144MAC                              *
     H*                 SHOW COMPSERVICES CLAIM NUMBER.              *
     H*                                                               *
     H*  11/30/06 C:181 Project: 139MFW                              *
     H*                 NURSE STAFF CODE - KEEP IN  SYNC WITH WCMS IF*
     H*                 CLIENT# IS 139 & GROUP IS LINKED TO WCMS     *
     H*                                                               *
     H*  10/06/06 C:181 Project: 114MAC                              *
     H*                 CHANGE OUT CALL TO F0275R WITH F0263R WHEN   *
     H*                 CLIENT# IS 114                               *
     H*                                                               *
     H*  07/28/06 C:62 Project: 139MEL                                *
     H*                Issue diary when referral changes.             *
     H*                                                               *
     H*  10/20/03 C:53 Project: MC213                                 *
     H*                Added country address info.                    *
     H*                                                               *
     H* 03/23/01 C:62  PROJECT MCMNT                                  *
     H*                ADD PARM TO CALL OF F0263R                     *
     H*                                                               *
     H*  05/24/02 C:62 Project: MCMNT     BASE ONLY                   *
     H*                CORRECT SEARCH OF SCCLAD AND SCCLFA FIELDS     *
     H*                                                               *
     H*  02/28/02 C:62 Project: MCMNT #209                            *
     H*                Compile only.                                  *
     H*                                                               *
     H*  01/05/01 C:62 Project: MCMNT     BASE ONLY                   *
     H*                CORRECTED PROCESSING OF SCR03 STAFF CODES WHEN *
     H*                WC UPDATE FLAG IS SET TO VARIOUS VALUES.       *
     H*                                                               *
     H*  08/08/00 C:62 Project: MC182                                 *
     H*                ADDED ADDITIONAL PARMS, SUB# AND FND TO OVER-  *
     H*                RIDE USER PARM VALUES.                         *
     H*                                                               *
     H*  07/11/00 C:15 Project: WC913                                 *
     H*                Use the System Table file to get the 'Method   *
     H*                of Contact' desc regardless of whether or not  *
     H*                the group is 'stand-alone'.                    *
     H*                                                               *
     H*  06/19/00 C:62 SUPPORT: MC065 19768                           *
     H*                SWAP OUT USAGE OF STABL TO STSTL3, SO THAT UC  *
     H*                BECOMES INVALID                                *
     H*
     H*  06/08/00 C:62  Support: GU099/23024
     H*                 REMOVED EDI FKEYS.
     H*
     H*  06/01/00 C:15  Support: GU099/23024
     H*                 Added code to load required parms before
     H*                 calling the EDI Log History search pgm (F19).
     H*
     H*  04/03/00 C:118 PROJECT: WC854
     H*                 CHANGE '% IMPAIRMENT' LINE TO 'IMPAIRMENT
     H*                 DETERMINED/%'.  ADD EDIT IF % IMPAIRMENT <>0
     H*                 THEN IMPAIRMENT DETERMINED MUST BE Y.
     H*
     H*  01/31/00 C:62  ?
     H*                 ADD FA704R TO UPDATE OTHER FILES WHEN DEMO-
     H*                 GRAPHIC INFORMATION IS CHANGED.
     H*
     H*  01/24/00 C:62  MC035 21529
     H*                 IN UPDATE MODE, WHEN THE RECORD IS TO BE
     H*                 UPDATED TO CHECK TO SEE IF THE CLAIM DEMO-
     H*                 GRAPHIC RECORD EXISTS AND IF NOT THEN WRITE ONE
     H*
     H*  12/13/99 C:62  MCMNT
     H*                 CHANGE SCSS# TO ALPHA FIELD.
     H*
     H*  10/26/99 C:62  MC212
     H*                 PAD THE EDIT OF ADJUDICATION STATE.
     H*
     H*  05/10/99 C:53  MC145
     H*                 QUALIFIED CALL TO WCMC PGMS FOR LINK W/NON PDS
     H*                 QLAIM SYSTEM
     H*
     H*  02/25/99 C:53  MC145
     H*                 CORRECTED LOST TIME TO GET DESC FROM TABLE FILE
     H*
     H*  11/09/98 C:31  Support: 14873
     H*                 County search issue
     H*                 1. Change county code from 3A to 4A
     H*
     H* 10/15/98 C:110 PROJECT: MC145                                 *
     H*                CHANGED SCREEN 3 TO INCLUDE USER PROFILE       *
     H*                AND ADDED HEADERS FOR STAFF CODE, USER         *
     H*                PROFILE AND USER NAME                          *
     H*                                                               *
     H* 10/07/98 C:94  PROJECT: 87MFX                                 *
     H*                CHANGED F12=PREVIOUS TO F12=CANCEL             *
     H*                                                               *
     H* 10/06/98 C:94  PROJECT: 87MFX                                 *
     H*                CHANGED @@WCUP TO LOOK FOR A 'Y' INSTEAD OF    *
     H*                BLANK                                          *
     H*                                                               *
     H* 09/29/98 C:53  PROJECT: MC098 # 14048                         *
     H*                CORRECT CASE NUMBER UPDATE TO IMMEDIATLY       *
     H*                AFTER CHAIN                                    *
     H*                                                               *
     H* 09/29/98 C:94  PROJECT: 87MFX                                 *
     H*                ADDED PARMS TO WA024R                          *
     H*                                                               *
     H* 09/22/98 C:53  PROJECT: MC101 # 13639                         *
     H*                ON ADD: DEFAULT CMPEND AND CMPREC TO "N"       *
     H*                                                               *
     H* 09/21/98 C:94  PROJECT: 87MFX                                 *
     H*                UPDATE WORKER'S COMP FIELDS                    *
     H*                                                               *
     H* 07/29/98 C:94  PROJECT: MC145                                 *
     H*                DEFAULT THE EMPLOYER FIELD FROM USER PARMS     *
     H*                ON AN ADD.                                     *
     H*                                                               *
     H* 06/22/98 C:94  PROJECT: MC145                                 *
     H*                DEFAULT TO SYSTEM DATE ON SCREEN 3 ON          *
     H*                STANDALONE                                     *
     H*                                                               *
     H* 06/16/98 C:62  PROJECT: MC145                                 *
     H*                1) FIX SEARCHES FOR ADD.                       *
     H*                2) STAFF CODE HEADERS.                         *
     H*                                                               *
     H* 06/12/98 C:94  PROJECT: MC145                                 *
     H*                1) DEFAULT TO USA AT THE ADD SCREEN.           *
     H*                2) ERRORCHK DATE AT ADD SCREEN.  IF GREATER    *
     H*                   THAN SYSTEM DATE, WILL GIVE ERROR.          *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   DISPLAY/CHANGE MODES    :  09/N09                           *
     H*   LINKED/STANDALONE       :  16/N16                           *
     H*   EDI INSTALLED/NOT INST. :  17/N17                           *
     H*   SCR02 FUNCTION LN1/LN2  :  18/N18                           *
     H*   SCR03 FUNCTION LN1/LN2  :  19/N19                           *
     H*   MEDICAL SERVICES YES/NO :  20/N20                           *
     H*   TML YES/NO              :  21/N21                           *
     H*   PLACE CURSOR INDICATORS :  31-39                            *
     H*   ADD/NOT ADD MODE        :  40/N40                           *
     H*   ERRCK1 ERROR INDICATORS :  71-82                            *
     H*   ERRCK2 ERROR INDICATORS :  72-79,81,85,87,88                *
     H*   ERRCK3 ERROR INDICATORS :  71-86                            *
     H*                                                               *
     H*****************************************************************
     H* SPECIAL NOTES:                                                *
     H*                                                               *
     H*   1) SCR02/SCR03 FUNCTION LINES ARE BUILT USING CRTFN2/CRTFN3.*
     H*      FUNCTIONS ARE DEFINED IN THE CONSTANT SECTION.           *
     H*                                                               *
     H*   2) PROGRAMS IN WORK FLOW THAT ARE CALLED ISOLATED FROM      *
     H*      WORK FLOW ARE SENT L (LOOK) AND M (MODIFY) IN THE        *
     H*      CALL/MODE PARAMETERS.  THIS IS DONE TO KEEP THE PROGRAM  *
     H*      FROM CALLING THE NEXT PROGRAM IN THE WORK SERIES.        *
     H*                                                               *
     H*   3) F0269C IS NO LONGER USED. F0269R CALLS F0260R WHICH      *
     H*      CALLS F0Z75R WHICH CALLS F0276R WHICH CALLS F0277R.      *
     H*                                                               *
     H*   4) SCRN# AND SCREEN ARE USED TO KEEP TRACK OF THE SCREEN    *
     H*      NUMBER OF THE SCREEN CURRENTLY DISPLAYED.  SCRN# IS      *
     H*      USED SO THAT THE PROGRAM KNOWS WHEN TO CALL THE NEXT     *
     H*      PROGRAM.  SCREEN IS USED TO ISOLATE THE OPERATION OF     *
     H*      SCR02'S AND SCR03'S F24 FUNCTION.                        *
     H*                                                               *
     H*   5) MVDBSC EXECUTES SUBROUTINES THAT HOLD VARIABLES OF THAT  *
     H*      SCREEN: MVDBS1, MVDBS2, MVDBS3. MVSCDB EXECUTES SUBROU-  *
     H*      TINES THAT HOLD VARIABLES OF THAT SCREEN: MVSCD1, MVSCD2 *
     H*      MVSCD3.                                                  *
     H*                                                               *
     H*****************************************************************
     FF0260F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
     FFMC1L     UF   E           K DISK
     FFMCDP     UF A E             DISK
     F                                     RENAME(PCD01:DBRCDS)
     F*** BEGIN ADD *** 01/31/00 **************************************
     F**********FMCDL   IF  E           K        DISK
     FFMCDL     UF   E           K DISK
     F*** END   ADD *** 01/31/00 **************************************
     F                                     INFDS(DBFDS2)
     FFMCXL     IF   E           K DISK
     FFMEMP     IF   E             DISK
     FFMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     FFMSFP     IF   E             DISK
     FFMSFL     IF   E           K DISK
     F                                     RENAME(PSF01:PSF02)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FFAMSL1    IF   E           K DISK
     FFTCCP     IF   E             DISK
     F                                     RENAME(PCC01:PCC02)
     FFTABL     IF   E           K DISK
     F                                     RENAME(PCC01:PCC03)
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PEB01)
     F                                     IGNORE(PPM01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PPS01)
     F                                     IGNORE(PTS01)
     FSTSTP     IF   E             DISK
     F*** BEGIN CHG *** 06/19/00 **************************************
     F**********STABL   IF  E           K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END   CHG *** 06/19/00 **************************************
     F                                     RENAME(PST01:PST02)
     FSTSTL     IF   E           K DISK
     F                                     RENAME(PST01:PST03)
     FSMUPP     IF   E             DISK
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FSMUPL2    UF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
     F*
      /COPY SCOPYMT01D
     D*
     D*** Beg Add *** 02/11/16 ****************************************
     D ##adstate       s              1
     D*** End Add *** 02/11/16 ****************************************
     D*****************************************************************
     D* User defined tables and arrays.
     D*****************************************************************
     D*
     D CS              S             30    DIM(3) CTDATA PERRCD(1)              Case status   s
     D FN1             S              1    DIM(78)                              Func L1 S2
     D FN2             S              1    DIM(78)                              Func L2 S2
     D FN3             S              1    DIM(78)                              Func L1 S3
     D FN4             S              1    DIM(78)                              Func L2 S3
     D CAS             S              1    DIM(19)                              Case number
     D*
      /If Not Defined (WEB)
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
      /Endif
     D*****************************************************************
     D* User defined data structures.
     D*****************************************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D                 DS
     D*** BEGIN CHG *** 11/09/98 **************************************
     D********                                1   5 XXCNTY
     D  XXCNTY                 1      6
     D  STATE                  1      2
     D********                                3   5 COUNTY
     D  COUNTY                 3      6
     D*** END CHG *** 11/09/98 ****************************************
     D*** BEGIN ADD *** 12/13/99 **************************************
     D                 DS
     D  SCSS#                  1     11
     D  XXSS1                  1      3
     D  XXHP1                  4      4
     D  XXSS2                  5      6
     D  XXHP2                  7      7
     D  XXSS3                  8     11
     D                 DS
     D  KKSS#                  1      9
     D  KKSS1                  1      3
     D  KKSS2                  4      5
     D  KKSS3                  6      9
     D*** END   ADD *** 12/13/99 **************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D*
     D OPTNM           C                   CONST('Medical Case Inform-
     D                                     ation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(3)
     D F3              C                   CONST('F3=Exit')
     D F5              C                   CONST('F5=Refresh')
     D F6              C                   CONST('F6=Med Svcs')
     D F10             C                   CONST('F10=Search')
     D F12             C                   CONST('F12=Cancel  ')
     D F14             C                   CONST('F14=DARTS')
     D F18             C                   CONST('F18=EDI')
     D F19             C                   CONST('F19=EDI Log Hist')
     D F24             C                   CONST('F24=More Keys')
     D MCON1           C                   CONST('Mail')
     D MCON2           C                   CONST('Telephone')
     D MCON3           C                   CONST('Fax')
     D MCON4           C                   CONST('in Person')

     I********      'Indemnity'           C         LTFL1  DEL 02/25/99
     I********      'Medical'             C         LTFL2  DEL 02/25/99
     I********      'Record only'         C         LTFL3  DEL 02/25/99

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     C*****************************************************************
     C* *LIKE Defined variables
     C*****************************************************************
     C********                                             DEL 02/25/99
     C*
     C     *LIKE         DEFINE    FNWCF#        KKWCF#
     C     *LIKE         DEFINE    CMSTAT        XXSTAT
     C     *LIKE         DEFINE    CMSTAT        XXBFST
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMFND         KKFNDW
     C*** BEGIN DEL *** 12/13/99 **************************************
     C********** *LIKE     DEFN CMSS#     KKSS#
     C*** END   DEL *** 12/13/99 **************************************
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMIFEI        KKFEIN
     C     *LIKE         DEFINE    CMACDT        HDACDT
     C     *LIKE         DEFINE    CMACDT        XXACDT
     C     *LIKE         DEFINE    CMAST         Q1ST
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    UPSTAF        KKSTAF
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    CMEMP#        ##EMP#
     C     *LIKE         DEFINE    CMDIV         ##DIV
     C*** BEGIN ADD *** 07/28/06 **************************************
     C     *LIKE         DEFINE    CMRNDT        XXRNDT
     C*** END   ADD *** 07/28/06 **************************************
     C**** 11/30/06 **** BEGIN ADD **********************
     C     *LIKE         DEFINE    CMCMNU        SVCMNU
     C**** 11/30/06 **** END   ADD **********************
     C*
     C*****************************************************************
     C* Parameter lists
     C*****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C* Search parm list.
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C* Search parm list 2.
     C*
     C     ACDPL2        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0          FMEMP
     C                   PARM      DBRRN3        ##RRN2            9 0          FMFNP
     C                   PARM      2             ##VIEW            1 0
     C*
     C* Company/Subscriber/Fund/Fyr parm list.
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C* County search parm list.
     C*
     C     CTYPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCST          ##ST              2
     C*
     C*  Parm list for FA701R.
     C*
     C     INSPL         PLIST
     C                   PARM      ' '           ##RTCD            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      ' '           ##OUTS            1
     C*
     C*  Medical parm list.
     C*
     C     MEDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C*  State code search parm list
     C*
     C     STCPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C* User parm list.
     C*
     C     USRPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      3             ##VIEW            1 0
     C*****************************************************************
     C* Key lists.
     C*****************************************************************
     C*
     C*** Key list for FMC1P.
     C*
     C     C1KY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*** Key list for claimant demographics file.
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C*** Key list for claim master file.
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     CMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C*
     C     CMKY2         KLIST
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C** Key list for WCMC control file.
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C** Key list for employer master file.
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*** Key list for FS base control file.
     C*
     C     FBKY          KLIST
     C                   KFLD                    XFKEY             2
     C*
     C*** Key list for fund master file.
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*** Key list for WCMC generic table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C*** Key list for WCMC claim medical services transaction file.
     C*
     C     MSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*** Key list for Subscriber Staff Code
     C*
     C     SFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKSTAF
     C*
     C*** Key list for subscriber master file.
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*** Key list for state file.
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*** Key list for facility (county) code file.
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*** Key list for facility (county) code file.
     C*
     C     TFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*** KEY LIST FOR SMUPL2 - USER PARM FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKSTAF
     C*
     C     UPKY1         KLIST
     C                   KFLD                    KKUSR
     C*
     C*****************************************************************
     C*
     C* IF CALLED SEPERATED FROM PROGRAM CHAIN.
     C*
     C                   MOVEL     *OFF          STOP              1
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'L'
     C                   MOVEL     'D'           @@MODE
     C                   MOVEL     *ON           STOP
     C*
     C     @@MODE        WHENEQ    'M'
     C                   MOVEL     'C'           @@MODE
     C                   MOVEL     *ON           STOP
     C*
     C                   ENDSL
     C*
     C* Update user parmeters with the case co#/sub#/fnd/fyr/emp#/div.
     C*
     C     @@MODE        IFNE      'A'
     C*
     C                   EXSR      UPDTUP
     C*
     C                   ENDIF
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     CMSTAT        XXSTAT
     C*
     C* CHANGE THE STATUS
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   MOVE      'R'           CMSTAT
     C                   EXSR      STSCHG
     C*
     C                   ELSE
     C*
     C* CHECK TO SEE IF THERE ARE ANY MEDICAL SERVICES,
     C* IF NOT CLOSE CASE OTHERWISE NOTIFY USER OF OPEN SERVICES.
     C*
     C     MSKY          CHAIN     PMS01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'C'           CMSTAT
     C                   EXSR      STSCHG
     C*
     C                   ELSE
     C*
     C* WINDOW THAT NOTIFIES THAT THERE ARE OPEN MEDICAL SERVICES.
     C*
     C                   EXFMT     WIN100
     C*
     C* IF ENTER WAS PRESSED CALL WORK WITH MEDICAL SERVICES.
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVEL     'M'           ##CALL
     C*
     C                   CALL      'F0276R'      MEDPL
     C*
     C* CHECK TO SEE IF MEDICAL SERVICES WERE CLOSED, IF NOT RETURN.
     C*
     C     MSKY          CHAIN     PMS01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVEL     'C'           CMSTAT
     C                   EXSR      STSCHG
     C*
     C                   ELSE
     C*
     C                   MOVEL     XXSTAT        CMSTAT
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     XXSTAT        CMSTAT
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  CMSTAT
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*
     C*================================================================
     C* CRTFN2 - SUBROUTINE FOR CREATING FUNCTION LINES FOR SCR02.
     C*================================================================
     C*
     C     CRTFN2        BEGSR
     C*
     C* *IN09=DISPLAY        *IN40=ADD         *IN16=LINKED WITH WCMS
     C* *IN17=EDI INSTALLED
     C*
     C                   SETOFF                                       18
     C                   MOVE      *BLANKS       FN1
     C                   MOVE      *BLANKS       FN2
     C*
     C                   MOVEA     F3            FN1(1)
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEA     F5            FN1(11)
     C                   MOVEA     F10           FN1(24)
     C                   MOVEA     F12           FN1(37)
     C                   MOVEA     F14           FN1(52)
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F18           FN1(64)
     C*
     C                   ENDIF                                                  *IN17
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ELSE                                                   *IN40
     C*
     C                   MOVEA     F10           FN1(11)
     C                   MOVEA     F12           FN1(24)
     C                   MOVEA     F14           FN1(39)
     C*
     C                   ENDIF                                                  *IN40
     C*
     C                   ELSE                                                   *IN09
     C*
     C                   MOVEA     F12           FN1(11)
     C                   MOVEA     F14           FN1(26)
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F18           FN1(38)
     C*
     C                   ENDIF                                                  *IN17
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ENDIF                                                  *IN09
     C*
     C                   MOVEA     FN1           SCFKL2
      /If Defined (WEB)
     C                   MOVEA     FN1           fl2(1)
     C                   MOVEA     FN2           fl2(2)
      /Free
       clear scFkL2;
      /End-free
      /Endif
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* CRTFN3 - SUBROUTINE FOR CREATING FUNCTION LINES FOR SCR03.
     C*================================================================
     C*
     C     CRTFN3        BEGSR
     C*
     C                   SETOFF                                       19
     C                   MOVE      *BLANKS       FN3
     C                   MOVE      *BLANKS       FN4
     C*
     C* *IN09=DISPLAY        *IN40=ADD         *IN16=LINKED WITH WCMS
     C* *IN17=EDI INSTALLED  *IN20=MED INSTALLED
     C*
     C                   MOVEA     F3            FN3(1)
     C*
     C     *IN09         IFEQ      *OFF
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEA     F5            FN3(11)
     C                   MOVEA     F10           FN3(24)
     C                   MOVEA     F12           FN3(37)
     C                   MOVEA     F14           FN3(52)
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F24           FN3(64)
     C                   MOVEA     F18           FN4(1)
     C                   MOVEA     F19           FN4(11)
     C                   MOVEA     F24           FN4(30)
     C*
     C                   ENDIF                                                   *IN17
     C*
     C                   ENDIF                                                   *IN16
     C*
     C                   ELSE                                                    *IN40
     C*
     C                   MOVEA     F10           FN3(11)
     C                   MOVEA     F12           FN3(24)
     C                   MOVEA     F14           FN3(37)
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F18           FN3(48)
     C                   MOVEA     F19           FN3(58)
     C*
     C                   ENDIF                                                  *IN17
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ENDIF                                                  *IN40
     C*
     C                   ELSE                                                   *IN09
     C*
     C                   MOVEA     F12           FN3(11)
     C                   MOVEA     F14           FN3(26)
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     *IN17         IFEQ      *ON
     C*
     C                   MOVEA     F18           FN3(38)
     C                   MOVEA     F19           FN3(48)
     C*
     C                   ENDIF                                                  *IN17
     C*
     C                   ENDIF                                                  *IN16
     C*
     C                   ENDIF                                                  *IN09
     C*
     C                   MOVEA     FN3           SCFKL3
      /If Defined (WEB)
     C                   MOVEA     FN3           fl3(1)
     C                   MOVEA     FN4           fl3(2)
      /Free
       clear scFkL3;
      /End-free
      /Endif
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DATCV1 - RUNS A DATCHK TO GO FROM 6 TO 8 FOR SCR01 DATES.
     C*================================================================
     C     DATCV1        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTOB
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DATCV2 - RUNS A DATCHK TO GO FROM 6 TO 8 FOR SCR02 DATES.
     C*================================================================
     C     DATCV2        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMMI
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DATCV3 - RUNS A DATCHK TO GO FROM 6 TO 8 FOR SCR03 DATES.
     C*================================================================
     C     DATCV3        BEGSR
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRP
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRCVD
     C*
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRE
     C*
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDBDT
     C*
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDEDT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C* CALL F0275R IN DISPLAY MODE IF CURRENT SCREEN IS 3 AND
     C* F0260R WASN'T CALLED ISOLATED FROM WORK FLOW.
     C*
     C     SCRN#         IFEQ      3
     C     STOP          ANDEQ     *OFF
     C     *IN20         ANDEQ     *ON
     C*
     C     *INKL         IFEQ      *OFF
     C*** BEGIN ADD *** 10/06/06 **************************************
     C*EXECUTE WHEN CLIENT IS #114 ONLY.
     C     XXCLT#        IFEQ      '114'
     C*
     C                   CALL      'F0263R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0          FMCMP
     C                   PARM      'Y'           ##CONT            1
     C*** BEGIN ADD *** 03/23/01 **************************************
     C                   PARM      '1'           ##CALL
     C*** END   ADD *** 03/23/01 **************************************
     C                   ELSE                                                   + XXCLT#=114
     C*** END   ADD *** 10/06/06 **************************************
     C                   CALL      'F0275R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '1'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP
     C                   PARM      DBRRN         ##RRN2            9 0          FMCMP
     C*
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   MOVEL     'D'           @@MODE
     C                   Z-ADD     3             SCRN#
     C                   EXSR      GETDTM
     C*
     C                   ENDIF
     C*
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
     C* SET OFF PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C* FIRST AND LAST NAMES CANNOT BE BLANK
     C*
     C  N16
     CANN90SCLSN         IFEQ      *BLANKS
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*
     C  N16
     CANN90SCFSN         IFEQ      *BLANKS
     C*
     C                   SETON                                        9072
     C*
     C                   ENDIF
     C*
     C* ADDRESS 1 CAN'T BE BLANK.
     C*
     C  N16
     CANN90SCADR1        IFEQ      *BLANKS
     C*
     C                   SETON                                        9073
     C*
     C                   ENDIF
     C*** BEG ADD *** 10/20/03 ****************************
     C* DEFAULT TO USA
     C*
     C     SCCTRY        IFEQ      *BLANKS
     C                   MOVE      'USA'         SCCTRY
     C*
     C                   ELSE
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  SCCTRY        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      '1'
     C                   SETON                                        9078
     C                   ENDIF
     C                   ENDIF
      *
      *  EITHER THE C,S,Z IS ENTERED OR THE OUT OF COUNTRY ADDRESS
      *
     C     SCCTY         IFEQ      *BLANKS
     C     SCST          ANDEQ     *BLANKS
     C     SCZIP1        ANDEQ     *ZERO
     C     SCCTRY        ANDEQ     'USA'
     C     SCADR3        ANDEQ     *BLANK
     C                   SETON                                        9029
     C                   ENDIF
      *
     C**         SCCTRY    IFNE 'USA'
     C**         SCADR3    ANDNE*BLANK
     C**                   SETON                     9029
     C**                   ENDIF
      *
      * BOTH THE C,S,Z AND THE OUT OF COUNTRY ADDRESS CANNOT BE ENTERED
      *
     C*          SCCTY     IFNE *BLANKS
     C*          SCST      ANDNE*BLANKS
     C*          SCZIP1    ANDNE*ZERO
     C*          SCCTRY    ANDNE'USA'
     C*          SCADR3    ANDNE*BLANK
     C*                    SETON                     9030
     C*                    ENDIF
      *
     C     SCCTRY        IFNE      'USA'
      *
     C     SCCTY         IFNE      *BLANKS
     C     SCST          ORNE      *BLANKS
     C     SCZIP1        ORNE      *ZERO
     C                   SETON                                        9029
     C                   ENDIF
      *
     C     SCADR3        IFEQ      *BLANK
     C                   SETON                                        9029
     C                   ENDIF
      *
     C                   ELSE
      *
     C     SCADR3        IFNE      *BLANKS
     C                   SETON                                        9029
     C                   ENDIF
      *
     C*** END ADD *** 10/20/03 *****************************
     C*
     C* CITY CAN'T BE BLANK.
     C*
     C  N16
     CANN90SCCTY         IFEQ      *BLANKS
     C*
     C                   SETON                                        9074
     C*
     C                   ENDIF
     C*
     C** VALIDATE ADDRESS STATE
     C*
     C                   MOVEL(P)  SCST          KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C  N16
     CANN90              SETON                                        9075
     C*
     C                   ENDIF
     C*
     C* ZIP1 CAN'T BE ZERO.
     C*
     C  N16
     CANN90SCZIP1        IFEQ      *ZEROS
     C*
     C                   SETON                                        9076
     C*
     C                   ENDIF
     C*
     C* VALIDATE COUNTY CODE, IF ENTERED
     C*
     C     SCCNTY        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCST          STATE
     C                   MOVEL(P)  SCCNTY        COUNTY
     C                   MOVEL(P)  XXCNTY        KKKEY
     C*
     C     TFKY          CHAIN     PCC03                              99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL     CCDESC        SCCNNM
     C*
     C                   ELSE
     C*
     C  N90              SETON                                        9077
     C                   MOVE      *BLANKS       SCCNNM
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG ADD *** 10/20/03 *****************************
     C                   ENDIF
     C*** END ADD *** 10/20/03 *****************************
     C*
     C*
     C*** BEG DEL *** 10/20/03 ****************************
     C**DEFAULT*TO USA
     C**********
     C********** SCCTRY    IFEQ *BLANKS
     C**********
     C**********           MOVE 'USA'     SCCTRY
     C**********
     C**********           ELSE
     C**********
     C**********           MOVEL'CTRY'    KKTBID    P
     C**********           MOVELSCCTRY    KKTKEY    P
     C**********
     C********** GTKY      CHAINPGT02                99
     C**********
     C********** *IN99     IFEQ '1'
     C**********
     C**********           SETON                     9078
     C**********
     C**********           ENDIF
     C**********
     C**********           ENDIF
     C*** BEG DEL *** 10/20/03 ****************************
     C*
     C* DATE OF BIRTH IF ENTERED MUST BE VALID.
     C*
     C     SCDTOB        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTOB
     C*** BEG ADD *** 6/12/98 ***************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTOB            7 0
     C*
     C     XXDTOB        IFGT      XXDATE
     C                   SETON                                        9083
     C                   ENDIF
     C*** END ADD *** 6/12/98 ****************************************
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        9079
     C*
     C                   ELSE
     C*
     C                   MOVE      SCDTOB        ##DTOB
     C*
     C                   CALL      'SRAGE'
     C                   PARM                    ##DTOB            8
     C                   PARM                    ##AGE             3
     C*
     C                   MOVE      ##AGE         SCAGE
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* THE GENDER MUST BE VALID
     C*
     C                   MOVEL     'GNDR'        KKTBID
     C                   MOVEL(P)  SCSEX         KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N16
     CANN90*IN99         IFEQ      '1'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        9080
     C                   MOVE      *BLANKS       SCGDSC
     C*
     C                   ELSE
     C*
     C                   MOVEL     GTDESC        SCGDSC
     C*
     C                   ENDIF
     C*
     C* MARITAL STATUS MUST BE VALID IF NOT BLANK.
     C*
     C  N16
     CANN90SCMSTS        IFNE      *BLANK
     C*
     C                   MOVEL(P)  'MSTS'        KKTBID
     C                   MOVEL(P)  SCMSTS        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   SETON                                        9081
     C                   MOVE      *BLANKS       SCMDSC
     C*
     C                   ELSE
     C*
     C                   MOVEL     GTDESC        SCMDSC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* EMPLOYER/DIVISION MUST BE VALID IF NOT ZERO.
     C*
     C  N16
     CANN90SCEMP#        IFNE      *ZEROS
     C     SCDIV         ORNE      *ZEROS
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM02                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   SETON                                        9082
     C                   MOVE      *BLANKS       SCENAM
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMNAME        SCENAM
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CREATE NAME HEADER FOR SCREEN TWO AND THREE
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCLSN         CAT(P)    ',':0         SCCLNM
     C                   CAT(P)    SCFSN:1       SCCLNM
     C*
     C     SCMID         IFNE      *BLANKS
     C*
     C                   CAT(P)    SCMID:1       SCCLNM
     C                   CAT(P)    '.':0         SCCLNM
     C*
     C                   ENDIF
     C*
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     *IN21         IFEQ      *OFF
     C*** END   ADD *** 12/12/06 **************************************
     C                   MOVEL     SCSCL#        SCSCL1
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   ENDIF
     C*
     C  N90              Z-ADD     2             SCREEN            1 0
     C  N90              SETOFF                                       19
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* SET OFF PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C* ADJUDICATION STATE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C*** BEGIN CHG *** 10/26/99 **************************************
     C**********           MOVELSCAST     KKKEY
     C                   MOVEL(P)  SCAST         KKKEY
     C*** END   CHG *** 10/26/99 **************************************
     C*
     C     STKY          CHAIN     PST03                              99
     C*
     C   99              SETON                                        72  90
     C*
     C                   ENDIF
     C*** Beg Add *** 02/11/16 ************************************************
     C     *IN99         IFEQ      *OFF
     C     ST1A2         ANDEQ     'Y'
     C                   SETON                                        72  90
     C                   Endif
     C*** End Add *** 02/11/16 ************************************************
     C*
     C* ACCIDENT DESCRIPTION MUST BE ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C     SCACDS        IFEQ      *BLANKS
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        73  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* MCA CLAIM MUST BE Y/N.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C     SCMCA         IFNE      'Y'
     C     SCMCA         ANDNE     'N'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        74  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* LOST TIME MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C                   MOVEL     'LTFL'        KKTBID
     C                   MOVEL     SCLTFL        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C*** Beg Chg *** 06/22/11 ****************************************
     C*
     C** 99 *****          SETON                     75  90
     C     *IN99         IFEQ      *ON
     C     FNNWCG        ANDNE     'Y'
     C                   SETON                                        75  90
     C                   ENDIF
     C*** End Chg *** 06/22/11 ****************************************
     C*
     C                   ENDIF
     C*
     C* CONTROVERT MUST BE Y/N.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C     SCCVFL        IFNE      'Y'
     C     SCCVFL        ANDNE     'N'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        76  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEG ADD *** 02/09/99 ***********************************
     C*
     C* DENIED CLAIM MUST BE Y/ .
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C     SCDNY         IFNE      'Y'
     C     SCDNY         ANDNE     ' '
     C                   SETON                                        81  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 02/09/99 ***********************************
     C*** BEGIN ADD *** 09/18/07 **************************************
     C*
     C* CATASTROPHIC CLAIM MUST BE Y/ .
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN25         ANDEQ     *ON
     C     SCCFLG        IFNE      'Y'
     C     SCCFLG        ANDNE     ' '
     C                   SETON                                        86  90
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C* METHOD OF CONTACT MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C                   MOVEL     'MCON'        KKTBID
     C                   MOVEL     SCMCON        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C*** Beg Chg *** 06/22/11 ****************************************
     C** 99 *********      SETON                     77  90
     C     *IN99         IFEQ      *ON
     C     FNNWCG        ANDNE     'Y'
     C                   SETON                                        77  90
     C                   ENDIF
     C*** End Chg *** 06/22/11 ****************************************
     C*
     C                   ENDIF
     C*
     C* PEND ENTIRE CASE MUST BE Y/N.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCPEND        IFNE      'Y'
     C     SCPEND        ANDNE     'N'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        78  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PRE-CERT MUST BE Y/N.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCPREC        IFNE      'Y'
     C     SCPREC        ANDNE     'N'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        79  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* FINAL MMI DATE MUST BE A VALID DATE IF ENTERED.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C     SCMMI         ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMMI
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        8090
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C*
     C* IF % IMPAIRMENT IS NOT ZER0, THEN IMPAIRMENT DETERMINED MUST
     C*    BE Y
     C*
     C  N90SCMMIP        IFNE      *ZEROS
     C     SCIMP         ANDNE     'Y'
     C                   SETON                                        87  90
     C                   ENDIF                                                  SCMMIP<>0
     C*                                                    SCIMP<>Y
     C* IMPAIRMENT DETERMINED MUST BE Y OR BLANK
     C*
     C  N90SCIMP         IFNE      'Y'
     C     SCIMP         ANDNE     *BLANK
     C                   SETON                                        88  90
     C                   ENDIF
     C*
     C***** END   ADD **** 04/03/00 ***********************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CMDT          IFGT      SAVEDT
     C     CMDT          OREQ      SAVEDT
     C     CMTIME        ANDGT     SAVETM
     C*
     C                   SETON                                        9085
     C*
     C                   ENDIF
     C*
     C  N90              Z-ADD     3             SCREEN
     C  N90              SETOFF                                       18
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* SET OFF PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*
     C* NURSE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
      *** BEG CHG *** 08/20/07 **********************************
      ***** BEG ADD ******* 01/26/01 **********************************
     C********** *IN16     OREQ *ON
      ***** END ADD ******* 01/26/01 **********************************
     C     *IN30         IFEQ      *ON
     C     *IN16         ANDEQ     *ON
     C                   ELSE
      *** END CHG *** 08/20/07 **********************************
     C*
     C                   MOVEL(P)  SCCMNU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7190
     C*
      *** BEG ADD *** 08/20/07 **********************************
     C                   ENDIF
      *** END ADD *** 08/20/07 **********************************
     C                   ENDIF
     C*
     C* STAFF 3  MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCSTF3        ANDNE     *BLANKS
     C*          *IN16     ANDEQ*OFF
     C*
     C                   MOVEL(P)  SCSTF3        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* SUPERVISOR MUST BE VALID IF NOT BLANK.
     C*
     C  N90SCCMSU        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCMSU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* CLAIM SUPERVISOR MUST BE VALID IF NOT BLANK.
     C*
     C*** BEGIN ADD *** 01/05/01 **************************************
     C     *IN22         IFEQ      *OFF
     C*** END   ADD *** 01/05/01 **************************************
     C  N90
     CANN16SCCLSU        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCLSU        KKSTAF
     C*
     C     SFKY          CHAIN     PSF02                              99
     C*
     C   99              SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* CLAIMS ADJUSTER MUST BE VALID IF NOT BLANK.
     C*
     C  N90
     CANN16SCCLAD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCLAD        KKSTAF
     C*
     C     SFKY          CHAIN     PSF02                              99
     C*
     C   99              SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* CLAIM FIELD ADJUSTER MUST BE VALID IF NOT BLANK.
     C*
     C  N90
     CANN16SCCLFA        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCLFA        KKSTAF
     C*
     C     SFKY          CHAIN     PSF02                              99
     C*
     C   99              SETON                                        7690
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   ELSE
     C*
     C  N90SCCLSU        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCLSU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* CLAIMS ADJUSTER MUST BE VALID IF NOT BLANK.
     C*
     C  N90SCCLAD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCLAD        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* CLAIM FIELD ADJUSTER MUST BE VALID IF NOT BLANK.
     C*
     C  N90SCCLFA        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCCLFA        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C   99              SETON                                        7690
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/05/01 **************************************
     C*
     C* DATE REPORTED TO EMPLOYER MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDTRP        ANDNE     *ZEROS
     C     *IN16         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRP
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        79  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DATE RECIEVED BY CARRIER MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRCVD        ANDNE     *ZEROS
     C     *IN16         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRCVD
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        78  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* REFERRAL DATE ENTERED MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRNDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        80  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DATE REHAB ASSIGNED MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDTRE        ANDNE     *ZEROS
     C     *IN16         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRE
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        81  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DATE OF DISABILITY MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDBDT        ANDNE     *ZEROS
     C     *IN16         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDBDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        82  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* DATE OF DEATH MUST BE VALID IF NOT BLANK.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCDEDT        ANDNE     *ZEROS
     C     *IN16         ANDEQ     *OFF
     C*
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDEDT
     C*
     C     SRERR         IFEQ      *ON
     C*
     C                   SETON                                        83  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/18/07 **************************************
     C     *IN25         IFEQ      *ON
     C*
     C* ASSIGN DATE MUST BE A VALID DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCMAD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCMAD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCMAD
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        7790
     C                   ENDIF
     C                   ENDIF
     C*
     C* ASSIGN DATE MUST BE >= TO THE ACCIDENT DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCMAD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCMAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCMAD            7 0
     C*
     C     XXCMAD        IFLT      CMACDT
     C                   SETON                                        8890
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C* PRE-EXIST DISABILITY MUST BE Y/N.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C     SCPREX        IFNE      'Y'
     C     SCPREX        ANDNE     'N'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        84  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 09/18/07 **************************************
     C     *IN25         IFEQ      *ON
     C*
     C* CLOSURE DATE MUST BE A VALID DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCMCD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCMCD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCMCD
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        8790
     C                   ENDIF
     C                   ENDIF
     C*
     C* CLOSURE DATE MUST BE >= TO THE ASSIGN DATE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCCMCD        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SCCMCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCMCD            7 0
     C*
     C     XXCMCD        IFLT      XXCMAD
     C                   SETON                                        8990
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C* OCCUPATIONAL DISEASE MUST BE Y/N.
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN16         ANDEQ     *OFF
     C*
     C     SCODIS        IFNE      'Y'
     C     SCODIS        ANDNE     'N'
     C*** Beg Add *** 06/22/11 ****************************************
     C     FNNWCG        ANDNE     'Y'
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   SETON                                        85  90
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90CMDT          IFGT      SAVEDT
     C     CMDT          OREQ      SAVEDT
     C     CMTIME        ANDGT     SAVETM
     C*
     C                   SETON                                        9086
     C*
     C                   ENDIF
     C*
     C* CLEAR STAFF DESCRIPTION IF IT WAS BLANKED OUT.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCSTF3        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCS3DS
     C                   ENDIF
     C*
     C     SCCMSU        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCSUDS
     C                   ENDIF
     C*
     C     SCCLSU        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCCSDS
     C                   ENDIF
     C*
     C     SCCLAD        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCCADS
     C                   ENDIF
     C*
     C     SCCLFA        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCFCDS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*** BEGIN DEL *** 06/16/98 **************************************
     C*
     C* SET OFF PLACE CURSOR INDICATORS
     C*
     C**********           MOVEA'00000'   *IN,31
     C**********           MOVEA'0000'    *IN,36
     C*
     C**********           SELEC
     C*
     C********** SCRN#     WHEQ 1
     C*** END   DEL *** 06/16/98 **************************************
     C*
     C                   SELECT
     C*
     C* SCREEN 1 - STATE SEARCH
     C*
     C     HFLD          WHENEQ    'SCST'
     C*
     C                   CALL      'SY601R'      STCPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   MOVEL     STKEY         SCST
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C* SCREEN 1 - COUNTY SEARCH
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C*
     C                   CALL      'F2360R'      CTYPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PCC02                              99
     C*
     C                   MOVEL(P)  CCKEY         XXCNTY
     C                   MOVEL(P)  COUNTY        SCCNTY
     C                   MOVEL     CCDESC        SCCNNM
     C                   SETON                                        34
     C*
     C                   ENDIF
     C*
     C* SCREEN 1 - COUNTRY SEARCH
     C*
     C     HFLD          WHENEQ    'SCCTRY'
     C*
     C                   MOVEL     'CTRY'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCCTRY
     C                   MOVEL     GTDESC        SCCTRD
     C                   SETON                                        35
     C*
     C                   ENDIF
     C*
     C* SCREEN 1 - GENDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCSEX'
     C*
     C                   MOVEL     'GNDR'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        36
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCSEX
     C                   MOVEL     GTDESC        SCGDSC
     C                   SETON                                        37
     C*
     C                   ENDIF
     C*
     C* SCREEN 1 - MARITAL STATUS SEARCH
     C*
     C     HFLD          WHENEQ    'SCMSTS'
     C*
     C                   MOVEL     'MSTS'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        37
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCMSTS
     C                   MOVEL     GTDESC        SCMDSC
     C                   SETON                                        38
     C*
     C                   ENDIF
     C*
     C* SCREEN 1 - EMPLOYER/DIVISION SEARCH.
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   CALL      'F0140R'      ACDPL2
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        39
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PEM01                              99
     C*
     C                   MOVEL     EMNAME        SCENAM
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 06/16/98 **************************************
     C**********           ENDSL
     C*
     C**********           EXSR DATCV1
     C*
     C********** SCRN#     WHEQ 2
     C*
     C**********           SELEC
     C*** END   DEL *** 06/16/98 **************************************
     C*
     C* SCREEN 2 - ADJUDICATION STATE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCAST'
     C*
     C*** Beg Chg *** 02/11/16 ********************************************
     C**********         CALL      'F0601R'      STCPL
     C                   CALL      'F0601R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      *ZEROS        ##RRN1            9 0
     C                   PARM      'A'           ##adstate         1
     C*** End Chg *** 02/11/16 ********************************************
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PST01                              99
     C*
     C                   MOVEL     STKEY         SCAST
     C                   MOVEL     STNAME        SCASNM
     C  N16              SETON                                        32
     C   16              SETON                                        37
     C*
     C                   ENDIF
     C*
     C* SCREEN 2 - LOST TIME STATUS SEARCH.
     C*
     C     HFLD          WHENEQ    'SCLTFL'
     C*
     C                   MOVEL     'LTFL'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTDESC        SCLDSC
     C                   MOVEL     GTKEY         SCLTFL
     C                   SETON                                        34
     C*
     C                   ENDIF
     C*
     C* SCREEN 2 - METHOD OF CONTACT SEARCH.
     C*
     C     HFLD          WHENEQ    'SCMCON'
     C*
     C                   MOVEL     'MCON'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        35
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTDESC        SCMCDS
     C                   MOVEL     GTKEY         SCMCON
     C                   SETON                                        36
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 06/16/98 **************************************
     C**********           ENDSL
     C*
     C**********           EXSR DATCV2
     C*
     C********** SCRN#     WHEQ 3
     C*
     C**********           SELEC
     C*** END   DEL *** 06/16/98 **************************************
     C*
     C* SCREEN 3 - CLAIM NURSE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCCMNU'
      *** BEG CHG *** 08/20/07 **********************************
      ***** BEG ADD ******* 01/26/01 **********************************
     C********** *IN16     ANDEQ*OFF
      ***** END ADD ******* 01/26/01 **********************************
     C     *IN30         IFEQ      *ON
     C     *IN16         ANDEQ     *ON
     C                   ELSE
      *** END CHG *** 08/20/07 **********************************
      *** END ADD *** 08/20/07 **********************************
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCCMNU
     C*** BEG ADD *** 10/15/98 ****************************************
     C                   MOVEL     UPUSR         SCNUSR
     C*** END ADD *** 10/15/98 ****************************************
     C                   MOVEL     UPUSNM        SCNUDS
     C  N16              SETON                                          32
     C   16              SETON                                          38
     C*
      *** BEG ADD *** 08/20/07 **********************************
     C                   ENDIF
      *** END ADD *** 08/20/07 **********************************
     C                   ENDIF
     C*
     C* SCREEN 3 - STAFF 3 SEARCH.
     C*
     C     HFLD          WHENEQ    'SCSTF3'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          32
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCSTF3
     C*** BEG ADD *** 10/15/98 ****************************************
     C                   MOVEL     UPUSR         SC3USR
     C*** END ADD *** 10/15/98 ****************************************
     C                   MOVEL     UPUSNM        SCS3DS
     C                   SETON                                          33
     C*
     C                   ENDIF
     C*
     C* SCREEN 3 - CLAIM SUPERVISOR SEARCH.
     C*
     C     HFLD          WHENEQ    'SCCMSU'
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          33
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCCMSU
     C*** BEG ADD *** 10/15/98 ****************************************
     C                   MOVEL     UPUSR         SCSUSR
     C*** END ADD *** 10/15/98 ****************************************
     C                   MOVEL     UPUSNM        SCSUDS
     C                   SETON                                          34
     C*
     C                   ENDIF
     C*
     C* SCREEN 3 - CLAIM NURSE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCCLSU'
     C*** BEGIN ADD *** 01/05/01 **************************************
     C     *IN16         IFEQ      *OFF
     C*
     C     *IN22         IFEQ      *OFF
     C*** END   ADD *** 01/05/01 **************************************
     C                   CALL      'F2420R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                          34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSF01                              99
     C*
     C                   MOVEL     SFSTAF        SCCLSU
     C*** BEG ADD *** 10/15/98 ****************************************
     C                   MOVEL     *BLANKS       SCCSSR
     C*** END ADD *** 10/15/98 ****************************************
     C                   MOVEL     SFNAME        SCCSDS
     C                   SETON                                          35
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   ELSE
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          34
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCCLSU
     C                   MOVEL     UPUSR         SCCSSR
     C                   MOVEL     UPUSNM        SCCSDS
     C                   SETON                                          35
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/05/01 **************************************
     C*
     C* SCREEN 3 - CLAIM NURSE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCCLAD'
     C*** BEGIN CHG *** 05/24/02 **************************************
     C*** BEGIN ADD *** 01/05/01 **************************************
     C********** *IN16     IFEQ *ON
     C*
     C********** *IN22     IFEQ *ON
     C*** END   ADD *** 01/05/01 **************************************
     C     *IN16         IFEQ      *OFF
     C     *IN22         IFEQ      *OFF
     C*** END   CHG *** 05/24/02 **************************************
     C*
     C                   CALL      'F2420R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                          35
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSF01                              99
     C*
     C                   MOVEL     SFSTAF        SCCLAD
     C*** BEG ADD *** 10/15/98 ****************************************
     C                   MOVEL     *BLANKS       SCCASR
     C*** END ADD *** 10/15/98 ****************************************
     C                   MOVEL     SFNAME        SCCADS
     C                   SETON                                          36
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   ELSE
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          35
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCCLAD
     C                   MOVEL     UPUSR         SCCASR
     C                   MOVEL     UPUSNM        SCCADS
     C                   SETON                                          36
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/05/01 **************************************
     C*
     C* SUPERVISOR 3 - CLAIM NURSE SEARCH.
     C*
     C     HFLD          WHENEQ    'SCCLFA'
     C*** BEGIN CHG *** 05/24/02 **************************************
     C*** BEGIN ADD *** 01/05/01 **************************************
     C********** *IN16     IFEQ *ON
     C*
     C********** *IN22     IFEQ *ON
     C*** END   ADD *** 01/05/01 **************************************
     C     *IN16         IFEQ      *OFF
     C     *IN22         IFEQ      *OFF
     C*** END   CHG *** 05/24/02 **************************************
     C*
     C                   CALL      'F2420R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                          36
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSF01                              99
     C*
     C                   MOVEL     SFSTAF        SCCLFA
     C*** BEG ADD *** 10/15/98 ****************************************
     C                   MOVEL     *BLANKS       SCCFSR
     C*** END ADD *** 10/15/98 ****************************************
     C                   MOVEL     SFNAME        SCFCDS
     C                   SETON                                          37
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   ELSE
     C*
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      3             ##VIEW            1 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   SETON                                          36
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PUP01                              99
     C*
     C                   MOVEL     UPSTAF        SCCLFA
     C                   MOVEL     UPUSR         SCCFSR
     C                   MOVEL     UPUSNM        SCFCDS
     C                   SETON                                          37
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD *** 01/05/01 **************************************
     C*
     C                   ENDSL
     C*
     C*** BEGIN ADD *** 06/16/98 **************************************
     C                   EXSR      DATCV1
     C                   EXSR      DATCV2
     C*** END   ADD *** 06/16/98 **************************************
     C                   EXSR      DATCV3
     C*
     C*** BEGIN DEL *** 06/16/98 **************************************
     C**********           ENDSL
     C*** END   DEL *** 06/16/98 **************************************
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO  - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL API TO ACCESS DARTS
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE                   90
     C*
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KSDO  - F18=EDI
     C*================================================================
     C     KSDO          BEGSR
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      FNWCF#        #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'W0070R5'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFNE      'A'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C*                    SETON                     90
     C*
     C                   ENDSR
     C*================================================================
     C* KTDO - F19=EDI log history.
     C*================================================================
     C     KTDO          BEGSR
     C*
     C                   MOVE      FNWCF#        #1FND
     C*** BEGIN ADD *** 06/01/00 **************************************
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*** END ADD ***** 06/01/00 **************************************
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           ##CALL
     C                   ELSE
     C                   MOVE      ' '           ##CALL
     C                   ENDIF
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     *IN99         IFEQ      '0'
     C*
     C                   CALL      'W0270R5'
     C                   PARM      OPTNM         ##PRDS
     C*** BEGIN CHG *** 06/01/00 **************************************
     C******************** PARM ' '       ##CALL
     C                   PARM                    ##CALL
     C*** END CHG ***** 06/01/00 **************************************
     C                   PARM                    #1CASE
     C                   PARM                    #1FYR
     C                   PARM                    #1FND
     C                   PARM                    #1CO#
     C                   PARM      ' '           ##RTCD
     C*
     C                   ENDIF                                                  *IN99 IFEQ
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KYDO - F24=MORE KEYS
     C*================================================================
     C     KYDO          BEGSR
     C*
     C     SCREEN        IFEQ      2
     C*
     C                   SETOFF                                       19
     C*
     C     FN2(1)        IFNE      *BLANKS
     C*
     C     *IN18         IFEQ      *OFF
     C*
     C                   MOVEA     FN2           SCFKL2
     C                   SETON                                        18
     C*
     C                   ELSE
     C*
     C                   MOVEA     FN1           SCFKL2
     C                   SETOFF                                       18
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       18
     C*
     C     FN4(1)        IFNE      *BLANKS
     C*
     C     *IN19         IFEQ      *OFF
     C*
     C                   MOVEA     FN4           SCFKL3
     C                   SETON                                        19
     C*
     C                   ELSE
     C*
     C                   MOVEA     FN3           SCFKL3
     C                   SETOFF                                       19
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
      /If Defined (WEB)
      /Free
       clear scFkL2;
       clear scFkL3;
      /End-free
      /Endif
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   EXSR      MVDBS1
     C                   EXSR      MVDBS2
     C                   EXSR      MVDBS3
     C  NKE              EXSR      SCRNUM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBS1 - Move data base fields to screen 1 fields
     C*================================================================
     C     MVDBS1        BEGSR
     C*
     C*** BEGIN CHG *** 01/31/00 **************************************
     C********** CDKY      CHAINPCD01                99
     C     CDKY          CHAIN(N)  PCD01                              99
     C*** END   CHG *** 01/31/00 **************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     CDLSN         SCLSN
     C                   MOVEL     CDFSN         SCFSN
     C                   MOVEL     CDMID         SCMID
     C*
     C* CREATE NAME HEADER FOR SCREEN TWO AND THREE
     C*
     C     SCLSN         CAT(P)    ',':0         SCCLNM
     C                   CAT(P)    SCFSN:1       SCCLNM
     C*
     C     SCMID         IFNE      *BLANKS
     C*
     C                   CAT(P)    SCMID:1       SCCLNM
     C                   CAT(P)    '.':0         SCCLNM
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVEL     CDST          SCST
     C                   MOVEL     CDZIP         SCZIP1
     C                   MOVE      CDZIP         SCZIP2
     C*
     C                   MOVEL     CDCTRY        SCCTRY
     C                   MOVEL(P)  CDCTRY        KKTKEY
     C                   MOVEL     'CTRY'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCCTRD
     C   99              MOVEL     *BLANKS       SCCTRD
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVEL(P)  CDADR3        SCADR3
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   MOVE      CDAREA        SCAREA
     C                   MOVEL     CDPHN#        SCPHN1
     C                   MOVE      CDPHN#        SCPHN2
     C*
     C                   MOVE      CDWARA        SCWARA
     C                   MOVEL     CDWPH#        SCWPH1
     C                   MOVE      CDWPH#        SCWPH2
     C*
     C*** BEG ADD *** 05/09/16 ****************************
     C                   MOVE      CDCARA        SCCARA
     C                   MOVEL     CDCPHN        SCCPN1
     C                   MOVE      CDCPHN        SCCPN2
     C*** END ADD *** 05/09/16 ****************************
     C*
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTOB
     C*
     C     SCDTOB        IFNE      *ZEROS
     C*
     C                   MOVE      SCDTOB        ##DTOB
     C*
     C                   CALL      'SRAGE'
     C                   PARM                    ##DTOB            8
     C                   PARM                    ##AGE             3
     C*
     C                   MOVE      ##AGE         SCAGE
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             SCAGE
     C*
     C                   ENDIF
     C*
     C                   MOVEL     CDSEX         SCSEX
     C                   MOVEL(P)  CDSEX         KKTKEY
     C                   MOVEL     'GNDR'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCGDSC
     C   99              MOVEL     *BLANKS       SCGDSC
     C*
     C                   MOVEL     CDMSTS        SCMSTS
     C                   MOVEL(P)  CDMSTS        KKTKEY
     C                   MOVEL     'MSTS'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCMDSC
     C   99              MOVEL     *BLANKS       SCMDSC
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCLSN
     C                   MOVE      *BLANKS       SCFSN
     C                   MOVE      *BLANKS       SCMID
     C*
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   Z-ADD     *ZEROS        SCZIP1
     C                   Z-ADD     *ZEROS        SCZIP2
     C*
     C*** BEG ADD *** 06/12/98 *************************************
     C                   MOVE      'USA'         SCCTRY
     C                   MOVEL     SCCTRY        KKTKEY
     C                   MOVEL     'CTRY'        KKTBID
     C     GTKY          CHAIN     PGT02                              99
     C   99              MOVE      *BLANKS       SCCTRD
     C  N99              MOVEL(P)  GTDESC        SCCTRD
     C*** END ADD *** 06/12/98 *************************************
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVE      *BLANK        SCADR3
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   Z-ADD     *ZEROS        SCAREA
     C                   Z-ADD     *ZEROS        SCPHN1
     C                   Z-ADD     *ZEROS        SCPHN2
     C*
     C*** BEG CHG *** 05/09/16 ****************************
     C*********          Z-ADD     *ZEROS        SCAREA
     C                   Z-ADD     *ZEROS        SCWARA
     C*** END CHG *** 05/09/16 ****************************
     C                   Z-ADD     *ZEROS        SCWPH1
     C                   Z-ADD     *ZEROS        SCWPH2
     C*
     C*** BEG ADD *** 05/09/16 ****************************
     C                   Z-ADD     *ZEROS        SCCARA
     C                   Z-ADD     *ZEROS        SCCPN1
     C                   Z-ADD     *ZEROS        SCCPN2
     C*** END ADD *** 05/09/16 ****************************
     C*
     C                   Z-ADD     *ZEROS        SCDTOB
     C*
     C                   Z-ADD     *ZEROS        SCAGE
     C*
     C                   MOVE      *BLANKS       SCSEX
     C                   MOVE      *BLANKS       SCGDSC
     C*
     C                   MOVE      *BLANKS       SCMSTS
     C                   MOVE      *BLANKS       SCMDSC
     C*
     C                   ENDIF
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CMCNTY        SCCNTY
     C                   MOVEL(P)  SCCNTY        COUNTY
     C                   MOVEL(P)  SCST          STATE
     C*
     C                   MOVEL     XXCNTY        KKKEY
     C*
     C     TFKY          CHAIN     PCC03                              99
     C*
     C  N99              MOVEL(P)  CCDESC        SCCNNM
     C   99              MOVEL     *BLANKS       SCCNNM
     C*
     C*
     C                   MOVEL     CMSCL#        SCSCL#
     C*** BEGIN ADD *** 12/12/06 **************************************
     C     *IN21         IFEQ      *OFF
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   MOVEL     SCSCL#        SCSCL1
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   ELSE
     C                   MOVEL(P)  CMSRC         SCSCL1
     C                   ENDIF
     C*** END   ADD *** 12/12/06 **************************************
     C*
     C                   MOVE      CMEMP#        SCEMP#
     C                   MOVE      CMDIV         SCDIV
     C                   MOVE      CMEMP#        KKEMP#
     C                   MOVE      CMDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM02                              99
     C*
     C  N99              MOVEL(P)  EMNAME        SCENAM
     C   99              MOVEL     *BLANKS       SCENAM
     C*
     C                   ELSE
     C*
     C*
     C                   MOVEL     *BLANKS       SCCNTY
     C                   MOVEL     *BLANKS       SCCNNM
     C                   MOVEL     *BLANKS       SCSCL#
     C*
     C*** BEG CHG *** 07/29/98 **************************************
     C*
     C                   MOVEL(P)  Q1USER        KKUSR
     C     UPKY1         CHAIN     PUP03                              99
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C*
     C                   MOVE      UPEMP#        KKEMP#
     C                   MOVE      UPDIV         KKDIV
     C     EMKY          CHAIN     PEM02                              99
     C*
     C                   MOVEL     EMNAME        SCENAM
     C*****                Z-ADD0         SCEMP#           DEL 07/29/98
     C*****                Z-ADD0         SCDIV            DEL 07/29/98
     C*****                MOVEL*BLANKS   SCENAM           DEL 07/29/98
     C*** END CHG *** 07/29/98 **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBS2 - Move data base fields to screen 3 fields
     C*================================================================
     C     MVDBS2        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   Z-ADD     2             SCREEN
     C                   SETOFF                                       19
     C*
     C                   ENDIF
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C                   MOVEL     CMSTAT        SCSTAT
     C*
     C                   SELECT
     C*
     C     CMSTAT        WHENEQ    'O'
     C                   MOVEL(P)  CS(1)         SCSDSC
     C*
     C     CMSTAT        WHENEQ    'R'
     C                   MOVEL(P)  CS(2)         SCSDSC
     C*
     C     CMSTAT        WHENEQ    'C'
     C                   MOVEL(P)  CS(3)         SCSDSC
     C*
     C                   ENDSL
     C*
     C                   MOVEL     CMAST         SCAST
     C                   MOVEL(P)  CMAST         KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C  N99              MOVEL(P)  STNAME        SCASNM
     C   99              MOVEL     *BLANKS       SCASNM
     C*
     C                   MOVE      CMACTM        SCACTM
     C                   MOVEL     CMACDS        SCACDS
     C                   MOVEL     CMACD2        SCACD2
     C                   MOVEL     CMMCA         SCMCA
     C*** BEGIN ADD *** 09/18/07 **************************************
     C                   MOVEL     CMCFLG        SCCFLG
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C                   MOVEL     CMLTFL        SCLTFL
     C*
     C********   *IN16     IFEQ *OFF                       DEL 02/25/99
     C*
     C                   MOVEL     'LTFL'        KKTBID
     C                   MOVEL(P)  CMLTFL        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCLDSC
     C   99              MOVEL     *BLANKS       SCLDSC
     C*** BEGIN DEL *** 03/02/01 **************************************
     C**********           MOVEL'TSIT'    KKTBID
     C*
     C*** BEG DEL *** 02/25/99 ***************************
     C**********           ELSE
     C**********
     C**********           SELEC
     C**********
     C********** SCLTFL    WHEQ 'I'
     C**********           MOVELLTFL1     SCLDSC    P
     C**********
     C********** SCLTFL    WHEQ 'M'
     C**********           MOVELLTFL2     SCLDSC    P
     C**********
     C********** SCLTFL    WHEQ 'R'
     C**********           MOVELLTFL2     SCLDSC    P
     C**********
     C**********           ENDSL
     C**********
     C**********           ENDIF
     C*** END DEL *** 02/25/99 ***************************
     C*
     C                   MOVEL     CMCVFL        SCCVFL
     C                   MOVEL     CMDNY         SCDNY                          ADD 02/09/99
     C                   MOVEL     CMCCL#        SCCCL#
     C                   MOVEL     CMMCON        SCMCON
     C*
     C*** BEGIN DLT *** 07/11/00 **************************************
     C********** *IN16     IFEQ *OFF
     C*** END DLT ***** 07/11/00 **************************************
     C*
     C                   MOVEL     'MCON'        KKTBID
     C                   MOVEL(P)  CMMCON        KKTKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCMCDS
     C   99              MOVEL     *BLANKS       SCMCDS
     C*
     C*** BEGIN DLT *** 07/11/00 **************************************
     C**********           ELSE
     C**********
     C**********           SELEC
     C**********
     C********** SCMCON    WHEQ 'M'
     C**********           MOVELMCON1     SCMCDS    P
     C**********
     C********** SCMCON    WHEQ 'T'
     C**********           MOVELMCON2     SCMCDS    P
     C**********
     C********** SCMCON    WHEQ 'F'
     C**********           MOVELMCON3     SCMCDS    P
     C**********
     C********** SCMCON    WHEQ 'P'
     C**********           MOVELMCON4     SCMCDS    P
     C**********
     C**********           ENDSL
     C**********
     C**********           ENDIF
     C*** END DLT ***** 07/11/00 **************************************
     C*
     C                   MOVEL     CMPEND        SCPEND
     C                   MOVEL     CMPREC        SCPREC
     C*
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI
     C*
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   MOVEL     CMIMP         SCIMP
     C***** END   ADD **** 04/03/00 ***********************************
     C                   MOVEL     CMMMIP        SCMMIP
     C*
     C                   ELSE
     C*
     C                   MOVEL     'O'           SCSTAT
     C                   MOVEL(P)  CS(1)         SCSDSC
     C*
     C                   MOVEL     *BLANKS       SCAST
     C                   MOVEL     *BLANKS       SCASNM
     C                   MOVE      *ZEROS        SCACTM
     C                   MOVEL     *BLANKS       SCACDS
     C                   MOVEL     *BLANKS       SCACD2
     C                   MOVEL     *BLANKS       SCMCA
     C*
     C                   MOVEL     *BLANKS       SCLTFL
     C                   MOVEL     *BLANKS       SCCVFL
     C                   MOVEL     *BLANKS       SCDNY                          ADD 02/09/99
     C                   MOVEL     *BLANKS       SCCCL#
     C                   MOVEL     *BLANKS       SCMCON
     C                   MOVEL     *BLANKS       SCMCDS
     C*** BEG DEL *** 09/22/98 *****************
     C*********            MOVEL*BLANKS   SCPEND
     C*********            MOVEL*BLANKS   SCPREC
     C*** BEG DEL *** 09/22/98 *****************
     C*** BEG ADD *** 09/22/98 *****************
     C                   MOVE      'N'           SCPEND
     C                   MOVE      'N'           SCPREC
     C*** END ADD *** 09/22/98 *****************
     C                   MOVE      *ZEROS        SCMMI
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   MOVEL     *BLANKS       SCIMP
     C***** END   ADD **** 04/03/00 ***********************************
     C                   MOVEL     *BLANKS       SCMMIP
     C                   MOVEL     *BLANKS       SCLDSC
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBS3 - Move data base fields to screen 2 fields
     C*================================================================
     C     MVDBS3        BEGSR
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C                   MOVEL     CXHDNU        SCHDNU
     C                   MOVEL     CXHDS3        SCHDS3
     C                   MOVEL     CXHDSU        SCHDSU
     C                   MOVEL     CXHDCS        SCHDCS
     C                   MOVEL     CXHDCA        SCHDCA
     C                   MOVEL     CXHDFA        SCHDFA
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     CMCMNU        SCCMNU
     C**** 11/30/06 **** BEGIN ADD **********************
     C                   MOVEL     CMCMNU        SVCMNU
     C**** 11/30/06 **** END   ADD **********************
     C                   MOVEL(P)  CMCMNU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCNUDS
     C   99              MOVEL     *BLANKS       SCNUDS
     C*
     C*** BEG ADD *** 10/15/98 ****************************************
     C  N99              MOVEL(P)  UPUSR         SCNUSR
     C   99              MOVEL     *BLANKS       SCNUSR
     C*** END ADD *** 10/15/98 ****************************************
     C*
     C                   MOVEL     CMSTF3        SCSTF3
     C                   MOVEL(P)  CMSTF3        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCS3DS
     C   99              MOVEL     *BLANKS       SCS3DS
     C*
     C*** BEG ADD *** 10/15/98 ****************************************
     C  N99              MOVEL(P)  UPUSR         SC3USR
     C   99              MOVEL     *BLANKS       SC3USR
     C*** END ADD *** 10/15/98 ****************************************
     C*
     C                   MOVEL     CMCMSU        SCCMSU
     C                   MOVEL(P)  CMCMSU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCSUDS
     C   99              MOVEL     *BLANKS       SCSUDS
     C*
     C*** BEG ADD *** 10/15/98 ****************************************
     C  N99              MOVEL(P)  UPUSR         SCSUSR
     C   99              MOVEL     *BLANKS       SCSUSR
     C*** END ADD *** 10/15/98 ****************************************
     C*
     C     *IN16         IFEQ      *ON
     C*** BEGIN ADD *** 01/05/01 **************************************
     C     *IN22         OREQ      *ON
     C*** END   ADD *** 01/05/01 **************************************
     C*
     C                   MOVEL     CMCLSU        SCCLSU
     C                   MOVEL(P)  CMCLSU        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCCSDS
     C   99              MOVEL     *BLANKS       SCCSDS
     C*
     C*** BEG ADD *** 10/15/98 ****************************************
     C  N99              MOVEL(P)  UPUSR         SCCSSR
     C   99              MOVEL     *BLANKS       SCCSSR
     C*** END ADD *** 10/15/98 ****************************************
     C*
     C                   MOVEL     CMCLAD        SCCLAD
     C                   MOVEL(P)  CMCLAD        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCCADS
     C   99              MOVEL     *BLANKS       SCCADS
     C*
     C*** BEG ADD *** 10/15/98 ****************************************
     C  N99              MOVEL(P)  UPUSR         SCCASR
     C   99              MOVEL     *BLANKS       SCCASR
     C*** END ADD *** 10/15/98 ****************************************
     C*
     C                   MOVEL     CMCLFA        SCCLFA
     C                   MOVEL(P)  CMCLFA        KKSTAF
     C*
     C     UPKY          CHAIN     PUP02                              99
     C*
     C  N99              MOVEL(P)  UPUSNM        SCFCDS
     C   99              MOVEL     *BLANKS       SCFCDS
     C*
     C*** BEG ADD *** 10/15/98 ****************************************
     C  N99              MOVEL(P)  UPUSR         SCCFSR
     C   99              MOVEL     *BLANKS       SCCFSR
     C*** END ADD *** 10/15/98 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     CMCLSU        SCCLSU
     C                   MOVEL(P)  CMCLSU        KKSTAF
     C*
     C     SFKY          CHAIN     PSF02                              99
     C*
     C  N99              MOVEL(P)  SFNAME        SCCSDS
     C   99              MOVEL     *BLANKS       SCCSDS
     C*
     C                   MOVEL     CMCLAD        SCCLAD
     C                   MOVEL(P)  CMCLAD        KKSTAF
     C*
     C     SFKY          CHAIN     PSF02                              99
     C*
     C  N99              MOVEL(P)  SFNAME        SCCADS
     C   99              MOVEL     *BLANKS       SCCADS
     C*
     C                   MOVEL     CMCLFA        SCCLFA
     C                   MOVEL(P)  CMCLFA        KKSTAF
     C*
     C     SFKY          CHAIN     PSF02                              99
     C*
     C  N99              MOVEL(P)  SFNAME        SCFCDS
     C   99              MOVEL     *BLANKS       SCFCDS
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRP
     C*
     C*** BEG ADD *** 06/22/98 ************************************
     C     *IN40         IFEQ      *ON
     C     *IN16         ANDEQ     *OFF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENTD
     C                   ELSE
     C*
     C                   Z-ADD     CMENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENTD
     C                   ENDIF
     C*** END ADD *** 06/22/98 ************************************
     C*
     C                   Z-ADD     CMRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C                   Z-ADD     CMDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRE
     C*
     C                   Z-ADD     CMDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDBDT
     C*
     C                   Z-ADD     CMDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDEDT
     C*** BEGIN ADD *** 09/18/07 **************************************
     C*
     C                   Z-ADD     CMCMAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCMAD
     C*
     C                   Z-ADD     CMCMCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCMCD
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C                   MOVE      CMPREX        SCPREX
     C*
     C                   MOVEL     CMODIS        SCODIS
     C*
     C                   MOVE      CMYRLX        SCYRLX
     C*
     C     *IN16         IFEQ      *ON
     C     ##WCIF        ANDEQ     'Y'
     C     ##NPWC        ANDNE     'Y'                                          ADD 06/17/99
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C                   MOVE      FNWCF#        KKFNDW
     C*
     C                   CALL      'WA107R'                             98      CHG 05/10/99
     C                   PARM      KKCO#         ##CO#                           I
     C                   PARM      KKFNDW        ##FND                           I
     C                   PARM      KKFYR         ##FYR                           I
     C                   PARM      KKCASE        ##CASE                          I
     C                   PARM      KKEMP#        ##EMP#                          I
     C                   PARM      KKDIV         ##DIV                           I
     C     SCNATR        PARM      *BLANKS       ##NA              2             O
     C     SCNDSC        PARM      *BLANKS       ##NADS           30             O
     C     SCPART        PARM      *BLANKS       ##PA              2             O
     C     SCPDSC        PARM      *BLANKS       ##PADS           30             O
     C     SCCAUS        PARM      *BLANKS       ##CA              2             O
     C     SCCDSC        PARM      *BLANKS       ##CADS           30             O
     C                   PARM      *BLANKS       ##RTCD            1             I/O
     C                   PARM      *BLANKS       ##CTG1           10             O
     C                   PARM      *BLANKS       ##CTGD           40             O
     C                   PARM      *ZEROS        ##DTRP            7             O
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 06/16/98 **************************************
     C*
     C* SET INDICATORS FOR DISPLAY OF '. . . :'
     C**********
     C********** SCHDNU    IFEQ *BLANKS
     C********** SCCMNU    ANDEQ*BLANKS
     C**********
     C**********           SETON                     41
     C**********
     C**********           ENDIF
     C**********
     C********** SCHDS3    IFEQ *BLANKS
     C********** SCSTF3    ANDEQ*BLANKS
     C**********
     C**********           SETON                     42
     C*
     C**********           ENDIF
     C**********
     C********** SCHDSU    IFEQ *BLANKS
     C********** SCCMSU    ANDEQ*BLANKS
     C**********
     C**********           SETON                     43
     C**********
     C**********           ENDIF
     C*
     C********** SCHDCS    IFEQ *BLANKS
     C********** SCCLSU    ANDEQ*BLANKS
     C**********
     C**********           SETON                     44
     C**********
     C**********           ENDIF
     C**********
     C********** SCHDCA    IFEQ *BLANKS
     C********** SCCLAD    ANDEQ*BLANKS
     C**********
     C**********           SETON                     45
     C**********
     C**********           ENDIF
     C**********
     C********** SCHDFA    IFEQ *BLANKS
     C********** SCCLFA    ANDEQ*BLANKS
     C**********
     C**********           SETON                     46
     C**********
     C**********           ENDIF
     C*** END   DEL *** 06/16/98 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCCMNU
     C**** 11/30/06 **** BEGIN ADD **********************
     C                   MOVEL     *BLANKS       SVCMNU
     C**** 11/30/06 **** END   ADD **********************
     C                   MOVEL     *BLANKS       SCSTF3
     C                   MOVEL     *BLANKS       SCCMSU
     C                   MOVEL     *BLANKS       SCCLSU
     C                   MOVEL     *BLANKS       SCCLAD
     C                   MOVEL     *BLANKS       SCCLFA
     C*
     C                   MOVE      *ZEROS        SCRCVD
     C                   MOVE      *ZEROS        SCDTRP
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENTD
     C*
     C                   MOVE      *ZEROS        SCRNDT
     C                   MOVE      *ZEROS        SCDTRE
     C                   MOVE      *ZEROS        SCDBDT
     C                   MOVE      *ZEROS        SCDEDT
     C                   MOVEL     *BLANKS       SCPREX
     C*
     C                   MOVEL     *BLANKS       SCODIS
     C*
     C                   MOVE      *ZEROS        SCYRLX
     C                   MOVEL     *BLANKS       SCNATR
     C                   MOVEL     *BLANKS       SCPART
     C                   MOVEL     *BLANKS       SCCAUS
     C                   MOVEL     *BLANKS       SCNUDS
     C                   MOVEL     *BLANKS       SCS3DS
     C                   MOVEL     *BLANKS       SCSUDS
     C                   MOVEL     *BLANKS       SCCSDS
     C                   MOVEL     *BLANKS       SCCADS
     C                   MOVEL     *BLANKS       SCFCDS
     C                   MOVEL     *BLANKS       SCNDSC
     C                   MOVEL     *BLANKS       SCPDSC
     C                   MOVEL     *BLANKS       SCCDSC
     C*
     C                   ENDIF
     C*
     C* SET INDICATORS FOR DISPLAY OF '. . . :'
     C*
     C     SCHDNU        IFEQ      *BLANKS
     C     SCCMNU        ANDEQ     *BLANKS
     C*
     C                   SETON                                        41
     C*
     C                   ENDIF
     C*
     C     SCHDS3        IFEQ      *BLANKS
     C     SCSTF3        ANDEQ     *BLANKS
     C*
     C                   SETON                                        42
     C*
     C                   ENDIF
     C*
     C     SCHDSU        IFEQ      *BLANKS
     C     SCCMSU        ANDEQ     *BLANKS
     C*
     C                   SETON                                        43
     C*
     C                   ENDIF
     C*
     C     SCHDCS        IFEQ      *BLANKS
     C     SCCLSU        ANDEQ     *BLANKS
     C*
     C                   SETON                                        44
     C*
     C                   ENDIF
     C*
     C     SCHDCA        IFEQ      *BLANKS
     C     SCCLAD        ANDEQ     *BLANKS
     C*
     C                   SETON                                        45
     C*
     C                   ENDIF
     C*
     C     SCHDFA        IFEQ      *BLANKS
     C     SCCLFA        ANDEQ     *BLANKS
     C*
     C                   SETON                                        46
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 06/16/98 **************************************
     C*
     C* SET INDICATORS FOR DISPLAY OF '. . . :'
     C*
     C     SCHDNU        IFEQ      *BLANKS
     C     SCCMNU        ANDEQ     *BLANKS
     C*
     C                   SETON                                        41
     C*
     C                   ENDIF
     C*
     C     SCHDS3        IFEQ      *BLANKS
     C     SCSTF3        ANDEQ     *BLANKS
     C*
     C                   SETON                                        42
     C*
     C                   ENDIF
     C*
     C     SCHDSU        IFEQ      *BLANKS
     C     SCCMSU        ANDEQ     *BLANKS
     C*
     C                   SETON                                        43
     C*
     C                   ENDIF
     C*
     C     SCHDCS        IFEQ      *BLANKS
     C     SCCLSU        ANDEQ     *BLANKS
     C*
     C                   SETON                                        44
     C*
     C                   ENDIF
     C*
     C     SCHDCA        IFEQ      *BLANKS
     C     SCCLAD        ANDEQ     *BLANKS
     C*
     C                   SETON                                        45
     C*
     C                   ENDIF
     C*
     C     SCHDFA        IFEQ      *BLANKS
     C     SCCLFA        ANDEQ     *BLANKS
     C*
     C                   SETON                                        46
     C*
     C                   ENDIF
     C*** END   ADD *** 06/16/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      KKCO#         CMCO#
     C                   MOVE      KKSUB#        CMSUB#
     C                   MOVE      KKFND         CMFND
     C                   MOVE      KKFYR         CMFYR
     C                   MOVE      KKCASE        CMCASE
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   EXSR      MVSCD1
     C                   EXSR      MVSCD2
     C                   EXSR      MVSCD3
     C*
     C                   ELSE
     C*
     C     SCRN#         CASEQ     1             MVSCD1
     C     SCRN#         CASEQ     2             MVSCD2
     C     SCRN#         CASEQ     3             MVSCD3
     C                   ENDCS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCD1 - Move data base fields to screen 1 fields
     C*================================================================
     C     MVSCD1        BEGSR
     C*
     C                   MOVEL     SCLSN         CDLSN
     C                   MOVEL     SCFSN         CDFSN
     C                   MOVEL     SCMID         CDMID
     C*
     C                   MOVEL     SCLSN         CMLSN
     C                   MOVEL(P)  SCFSN         CMFSN
     C                   MOVEL(P)  SCMID         CMMID
     C*
     C                   MOVEL(P)  SCADR1        CDADR1
     C                   MOVEL(P)  SCADR2        CDADR2
     C                   MOVEL(P)  SCCTY         CDCTY
     C                   MOVEL(P)  SCST          CDST
     C                   MOVEL     SCZIP1        CDZIP
     C                   MOVE      SCZIP2        CDZIP
     C*
     C                   MOVEL(P)  COUNTY        CMCNTY
     C                   MOVEL(P)  SCCTRY        CDCTRY
     C*** BEG ADD *** 10/20/03 ****************************
     C                   MOVEL(P)  SCADR3        CDADR3
     C*** END ADD *** 10/20/03 ****************************
     C*
     C                   MOVE      SCAREA        CDAREA
     C                   MOVEL     SCPHN1        CDPHN#
     C                   MOVE      SCPHN2        CDPHN#
     C*
     C                   MOVE      SCWARA        CDWARA
     C                   MOVEL     SCWPH1        CDWPH#
     C                   MOVE      SCWPH2        CDWPH#
     C*
     C*** BEG ADD *** 05/09/16 ****************************
     C                   MOVE      SCCARA        CDCARA
     C                   MOVEL     SCCPN1        CDCPHN
     C                   MOVE      SCCPN2        CDCPHN
     C*** END ADD *** 05/09/16 ****************************
     C*
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDTOB
     C*
     C                   MOVEL     SCSEX         CDSEX
     C                   MOVEL     SCMSTS        CDMSTS
     C                   MOVEL     KKSS#         CDSS#
     C                   MOVEL     KKSS#         CMSS#
     C                   MOVEL     KKACDT        CMACDT
     C                   MOVEL     SCSCL#        CMSCL#
     C*
     C                   MOVE      SCEMP#        CMEMP#
     C                   MOVE      SCDIV         CMDIV
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCD2 - Move screen 2 fields to database fields.
     C*================================================================
     C     MVSCD2        BEGSR
     C*
     C                   MOVEL     SCSTAT        CMSTAT
     C                   MOVEL     SCAST         CMAST
     C                   MOVE      SCACTM        CMACTM
     C                   MOVEL     SCACDS        CMACDS
     C                   MOVEL     SCACD2        CMACD2
     C                   MOVEL     SCMCA         CMMCA
     C*
     C                   MOVEL     SCLTFL        CMLTFL
     C                   MOVEL     SCCVFL        CMCVFL
     C                   MOVEL     SCDNY         CMDNY                          ADD 02/09/99
     C                   MOVEL     SCCCL#        CMCCL#
     C                   MOVEL     SCMCON        CMMCON
     C                   MOVEL     SCPEND        CMPEND
     C*** BEGIN ADD *** 09/18/07 **************************************
     C                   MOVEL     SCCFLG        CMCFLG
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C                   MOVEL     SCPREC        CMPREC
     C*
     C                   Z-ADD     SCMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMMMI
     C*
     C                   MOVEL     SCMMIP        CMMMIP
     C*
     C***** BEGIN ADD **** 04/03/00 ***********************************
     C                   MOVEL     SCIMP         CMIMP
     C***** END   ADD **** 04/03/00 ***********************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCD3 - Move screen 3 fields to database fields.
     C*================================================================
     C     MVSCD3        BEGSR
     C*
     C                   MOVEL     SCCMNU        CMCMNU
     C                   MOVEL     SCSTF3        CMSTF3
     C                   MOVEL     SCCMSU        CMCMSU
     C                   MOVEL     SCCLSU        CMCLSU
     C                   MOVEL     SCCLAD        CMCLAD
     C                   MOVEL     SCCLFA        CMCLFA
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMRCVD
     C*
     C                   MOVEL     SCDTRP        SCDTRP
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDTRP
     C*
     C                   Z-ADD     SCENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMENTD
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMRNDT
     C*
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDTRE
     C*
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDBDT
     C*
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDEDT
     C*** BEGIN ADD *** 09/18/07 **************************************
     C                   Z-ADD     SCCMAD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCMAD
     C*
     C                   Z-ADD     SCCMCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCMCD
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C                   MOVE      SCPREX        CMPREX
     C*
     C                   MOVEL     SCODIS        CMODIS
     C*
     C                   MOVE      SCYRLX        CMYRLX
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*** BEGIN ADD *** 06/16/98 **************************************
     C*
     C* SET OFF PLACE CURSOR INDICATORS
     C*
     C                   MOVEA     '00000'       *IN(31)
     C                   MOVEA     '0000'        *IN(36)
     C*** END   ADD *** 06/16/98 **************************************
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C     *INKS         CASEQ     *ON           KSDO
     C     *INKT         CASEQ     *ON           KTDO
     C     *INKY         CASEQ     *ON           KYDO
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
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C                   MOVEA     '000000'      *IN(41)
     C*
     C                   Z-ADD     *ZEROS        SCRN#
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C*** GET SYSTEM INFO
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
     C*** BEG ADD *** 06/17/99 **************************************
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*** END ADD *** 06/17/99 **************************************
     C*
     C*** BEGIN ADD *** 10/06/06 **************************************
     C*
     C*RETRIEVE CLIENT ID#
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C                   SETOFF                                       30
     C     XXCLT#        IFEQ      '114'
     C*** Begin Add *** 04/10/19 ****************************************
     C     XXCLT#        OREQ      '152'
     C*** End   Add *** 04/10/19 ****************************************
     C                   SETON                                        30
     C                   ENDIF                                                  XXCLT#=114
     C*** BEGIN ADD *** 12/12/06 **************************************
     C                   SETOFF                                       21
     C     XXCLT#        IFEQ      '144'
     C                   SETON                                        21
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 12/12/06 **************************************
     C*** BEGIN ADD *** 09/18/07 **************************************
     C                   SETOFF                                       25
     C     XXCLT#        IFEQ      '139'
     C                   SETON                                        25
     C                   ENDIF
     C*** END   ADD *** 09/18/07 **************************************
     C*
     C*
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C*
     C*** IS EDI INSTALLED ?
     C*
     C*** BEGIN DEL *** 06/08/00 **************************************
     C********** ##EDI     IFEQ 'Y'
     C**********           SETON                     17
     C**********           ELSE
     C**********           SETOF                     17
     C**********           ENDIF                                       '1'
     C*** END   DEL *** 06/08/00 **************************************
     C*
     C*** IS MED INSTALLED ?
     C*
     C     ##MED         IFEQ      'Y'
     C                   SETON                                        20
     C                   ELSE
     C                   SETOFF                                       20
     C                   ENDIF                                                              '1'
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      CMCO#         KKCO#
     C                   MOVE      CMSUB#        KKSUB#
     C                   MOVE      CMFND         KKFND
     C                   MOVE      CMFYR         KKFYR
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      CMSS#         KKSS#
     C                   MOVE      CMEMP#        KKEMP#
     C                   MOVE      CMDIV         KKDIV
     C                   MOVE      CMACDT        KKACDT
     C*
     C                   ELSE
     C*
     C                   MOVE      ##CO#         KKCO#
     C*** BEGIN ADD *** 08/08/00 **************************************
     C     Q1PRMS        IFGT      6
     C     @@SUB#        ANDNE     *ZEROS
     C*
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@FND         KKFND
     C*
     C                   ELSE
     C*** END   ADD *** 08/08/00 **************************************
     C                   MOVE      ##SUB#        KKSUB#
     C                   MOVE      ##FND         KKFND
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C                   Z-ADD     @@ACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C*
     C                   MOVEL     KKACDT        KKFYR
     C                   MOVE      @@SS#         KKSS#
     C*
     C* ASSIGN A CASE NUMBER.
     C*
     C     *IN40         IFEQ      *ON
     C*
     C********** C1KY      CHAINPC101               N99    DEL 09/29/98
     C*
     C*** BEG ADD *** 09/29/98 *****************************
      *
      * UPDATE LAST CASE NUMER IN FMC1P
      *
     C     C1KY          CHAIN     PC101                              99
     C                   ADD       1             C1LCS#
     C                   UPDATE    PC101
     C*
     C                   Z-ADD     C1LCS#        CMCASE
     C*** END ADD *** 09/29/98 *****************************
     C********** C1LCS#    ADD  1         CMCASE           DEL 09/30/98
     C                   MOVE      CMCASE        KKCASE
     C                   MOVE      CMCASE        #1CASE
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*
     C     ##OUTS        IFEQ      'N'
     C                   SETON                                        16
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C                   ENDIF
     C*** BEG ADD *** 09/21/98 **************************************
     C*
     C*** BEG CHG *** 10/06/98 ****************************************
     C                   MOVE      *BLANK        @@WCUP
     C**********           MOVE 'F'       @@WCUP
     C*** END CHG *** 10/06/98 ****************************************
     C*
     C     *IN16         IFEQ      *ON
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C                   CALL      'FA715R'
     C                   PARM                    @@RTCD            1
     C                   PARM      KKCO#         @@CO#             3 0
     C*** BEGIN CHG *** 08/08/00 **************************************
     C**********           PARM KKSUB#    @@SUB#  50
     C**********           PARM KKFND     @@FND   30
     C                   PARM      KKSUB#        ##SUB#            5 0
     C                   PARM      KKFND         ##FND             3 0
     C*** END   CHG *** 08/08/00 **************************************
     C                   PARM                    @@PVMC            1 0          Init Med
     C                   PARM                    @@PVPR            1 0          Init Prov
     C                   PARM                    @@WCUP            1            Allw Upd WCMS Clms
     C                   PARM                    @@CLDL            3 0          Clm Dlt Lvl
     C                   PARM                    @@CLDT            1            Clm Dlt Typ
     C                   PARM                    @@CLTL            3 0          Clm Xfer Lvl
     C                   PARM                    @@CLTT            1            Clm Xfer Typ
     C                   PARM                    @@SDCL            3 0          Settle Dt Lvl
     C                   PARM                    @@SDCT            1            Settle Dt Typ
     C                   PARM                    @@BSCL            3 0          BWC Sts Lvl
     C                   PARM                    @@BSCT            1            BWC Sts Typ
     C                   PARM                    @@DCDL            3 0          Del Diag Cd Lvl
     C                   PARM                    @@DCDT            1            Del Diag Cd Typ
     C                   PARM                    @@DCCL            3 0          Chg Diag St Lvl
     C                   PARM                    @@DCCT            1            Chg Diag St Typ
     C                   PARM                    @@C#CL            3 0          Clt Case Lvl
     C                   PARM                    @@C#CT            1            Clt Case Typ
     C                   PARM                    @@FIMR            5            RMCD: From IMR
     C                   PARM                    @@TIMR            5            RMCD: To IMR
     C                   PARM                    @@FRN             5            RMCD: From RN
     C                   PARM                    @@TRN             5            RMCD: To RN
     C*
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   SETOFF                                       22
     C*** END   ADD *** 01/05/01 **************************************
     C*** BEG CHG *** 10/06/98 ****************************************
     C     @@WCUP        IFEQ      'Y'
     C********** @@WCUP    IFEQ ' '
     C*** END CHG *** 10/06/98 ****************************************
     C                   SETOFF                                       16
     C*** BEGIN ADD *** 01/05/01 **************************************
     C                   SETON                                        22
     C*** END   ADD *** 01/05/01 **************************************
     C                   ENDIF                                                  @@WCUP = Y
     C                   ENDIF                                                  ##WCIF ##WCIN
     C                   ENDIF                                                  *IN16 = ON
     C*** END ADD *** 09/21/98 **************************************
     C*
     C* LOAD HEADER FIELDS.
     C*
     C                   MOVE(P)   KKSUB#        #2SUB#            5
     C                   MOVE(P)   KKFND         #2FND             3
     C                   MOVE(P)   KKFYR         #2FYR             3
     C                   MOVE(P)   KKCASE        #2CASE            7
     C*
     C*  REMOVE LEADING ZEROS FROM SUB#
     C*
     C     '0'           CHECK     #2SUB#        SP1               3 0          START POSITION
     C     5             SUB       SP1           LN1               3 0
     C                   ADD       1             LN1                            LENGTH
     C     SP1           IFGT      *ZERO
     C     LN1           SUBST(P)  #2SUB#:SP1    #2SUB#
     C                   END
     C*
     C     #2SUB#        CAT(P)    '-':0         SCCAS#
     C                   CAT       #2FND:0       SCCAS#
     C                   CAT       '-':0         SCCAS#
     C                   CAT       #2FYR:0       SCCAS#
     C                   CAT       '-':0         SCCAS#
     C*
     C*  REMOVE LEADING ZEROS FROM CASE
     C*
     C     '0'           CHECK     #2CASE        SP4               3 0          START POSITION
     C     7             SUB       SP4           LN4               3 0
     C                   ADD       1             LN4                            LENGTH
     C     SP4           IFGT      *ZERO
     C     LN4           SUBST(P)  #2CASE:SP4    #2CASE
     C                   END
     C*
     C                   CAT       #2CASE:0      SCCAS#
     C*
     C*** BEGIN CHG *** 12/13/99 **************************************
     C**********           MOVE KKSS#     SCSS#
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVE      KKSS1         XXSS1
     C***                MOVE      '-'           XXHP1
     C***                MOVE      KKSS2         XXSS2
     C***                MOVE      '-'           XXHP2
     C***                MOVE      KKSS3         XXSS3
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'KKSS#':KKSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*** END   CHG *** 12/13/99 **************************************
     C*
     C                   Z-ADD     KKACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOI
     C*
     C                   EXSR      CRTFN2
     C                   EXSR      CRTFN3
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*** Beg Add *** 06/22/11 ****************************************
     C                   SETON                                        62
     C     *IN99         IFEQ      *OFF
     C     FNNWCG        IFEQ      'Y'
     C                   SETOFF                                       62
     C                   ENDIF
     C                   ENDIF
     C*** End Add *** 06/22/11 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* SCRNUM - Tells program what screen is being processed.
     C*================================================================
     C     SCRNUM        BEGSR
     C*
     C*
     C                   SELECT
     C*
     C     SCRN#         WHENLT    3
     C*
     C     *INKL         IFEQ      *OFF
     C                   ADD       1             SCRN#             1 0
     C                   ELSE
     C                   SUB       1             SCRN#
     C                   ENDIF
     C*
     C     SCRN#         WHENEQ    3
     C     *INKL         ANDEQ     *ON
     C*
     C                   SUB       1             SCRN#
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* STSCHG - Status change.
     C*================================================================
     C     STSCHG        BEGSR
     C*
     C* CALL THE API TO WRITE CLAIM HISTORY RECORD.
     C*
     C                   CALL      'FA004R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      CMSTAT        ##OCST            1
     C                   PARM      @@RRN         ##RRN             9 0          RRN FMCMP
     C*
     C     ##RTCD        IFNE      'F'
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   MOVEL     DSTIME        CMTIME
     C                   UPDATE    DBRCDF
     C                   ELSE
     C*
     C                   MOVEL     XXSTAT        CMSTAT
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEGIN ADD *** 07/28/06 **************************************
     C*
     C* Issue diary if referral date entered is entered or changed
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C     SCCMNU        CHAIN     PUP02                              99
     C*
     C     XXRNDT        IFNE      CMRNDT
     C     CXRDRC        ANDNE     *BLANKS
     C     CXDDAY        ANDNE     *ZEROS
     C     *IN99         ANDEQ     *OFF
     C*
     C*  Call API to build DARTS key
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1CASE
     C*
     C                   CALL      'FA703R'
     C                   PARM      'K'           ##RTCD            1
     C                   PARM                    ##WCIN            1
     C                   PARM                    ##WCDK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANK        ##KEY            40
     C                   PARM      *BLANK        ##PNAM           10
     C*
     C*  Remove existing remark if it exists
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##CLDT
     C*
     C                   CALL      'DA702R'
     C                   PARM                    ##KEY
     C                   PARM      CXRDRC        ##RMCD            5
     C                   PARM                    ##CLDT            7 0
     C*
     C* Add new remark
     C*
     C     SCRNDT        IFNE      *ZEROS
     C                   CALL      'SRFUTR8'
     C                   PARM      SCRNDT        SRDATE            8 0
     C                   PARM      CXDDAY        SRFUTR            5 0
     C                   PARM      *BLANKS       SRRTCD            1
     C*
     C                   MOVEL(P)  'Referral'    ##COMM
     C*
     C                   MOVE      SRDATE        ##CDAT
     C*** BEGIN ADD *** 01/22/2007 ************************************
     C                   MOVEL(P)  'F0269C1'     ##DCID
     C     #1CO#         CAT(P)    #1SUB#:0      ##FOLD
     C                   CAT       #1FND:0       ##FOLD
     C                   CAT       #1FYR:0       ##FOLD
     C                   CAT       #1CASE:0      ##FOLD
     C*** END   ADD *** 01/22/2007 ************************************
     C*
     C                   CALL      'DA700R'
     C                   PARM                    ##KEY
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C*** BEGIN CHG *** 01/22/2007 ************************************
     C**********           PARM 'W0070C'  ##DCID 12
     C                   PARM                    ##DCID           12
     C*** END   CHG *** 01/22/2007 ************************************
     C                   PARM                    ##PNAM
     C                   PARM      '000'         ##SECL            3
     C                   PARM      UPUSR         ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C*** BEGIN CHG *** 01/22/2007 ************************************
     C**********           PARM ##KEY     ##FOLD 80
     C                   PARM                    ##FOLD           80
     C*** END   CHG *** 01/22/2007 ************************************
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      'Y'           ##FL01            1
     C                   PARM      'Y'           ##FL02            1
     C                   PARM      CXRDRC        ##RMCD            5
     C                   PARM      *BLANKS       ##SEQ#            5
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 07/28/06 **************************************
     C                   EXSR      MVSCDB
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVEL     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   UPDATE    DBRCDF
     C*
     C*** BEGIN ADD *** 01/24/00 **************************************
     C     CDKY          CHAIN(N)  PCD01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   EXSR      WRITED
     C*
     C                   ENDIF
     C*** END   ADD *** 01/24/00 **************************************
     C                   ENDIF
     C*
     C*** UPDATE THE CLAIMAINT DEMOGRAPHICS - WORK PHONE/MARITAL STATUS
     C*
     C     DBRRN2        CHAIN     DBRCDS                             99
     C*
     C     *IN99         IFEQ      '0'
     C*
     C                   EXSR      MVSCD1
     C*** BEGIN ADD *** 01/31/00 **************************************
     C                   CALL      'FA704R'
     C                   PARM      CDSS#         ##SS#             9
     C                   PARM      CDLSN         ##LSN            30
     C                   PARM      CDFSN         ##FSN            15
     C                   PARM      CDMID         ##MID             1
     C*** END   ADD *** 01/31/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        CDUSER
     C                   MOVEL     Q1PGM         CDPGM
     C                   MOVEL     DSTIME        CDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDT
     C*
     C                   UPDATE    DBRCDS
     C*
     C                   ENDIF
     C**** 11/30/06 **** BEGIN ADD **********************
     C                   EXSR      UPDNCD
     C**** 11/30/06 **** END   ADD **********************
     C*** BEG ADD *** 09/21/98 ***************************************
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1RCVD
     C*
     C                   Z-ADD     SCDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1DTRP
     C*
     C                   Z-ADD     SCDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1DTRE
     C*
     C                   Z-ADD     SCDBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1DBDT
     C*
     C                   Z-ADD     SCENTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1ENTD
     C*
     C                   Z-ADD     SCDEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @1DEDT
     C*
     C*** BEG CHG *** 10/06/98 ***************************************
     C     @@WCUP        IFEQ      'Y'
     C********** @@WCUP    IFEQ ' '
     C*** END CHG *** 10/06/98 ***************************************
     C                   CALL      'WA024R'
01   C                   PARM      *BLANK        @1RTCD            1            WCMS fields
02   C                   PARM      KKCO#         @1CO#             3 0          CO#
03   C                   PARM      KKSUB#        @1SUB#            5 0          -----------
04   C                   PARM      KKFND         @1FND             3 0          FND
05   C                   PARM      KKFYR         @1FYR             3 0          FYR
06   C                   PARM      KKCASE        @1CASE            7 0          CASE
07   C                   PARM      KKEMP#        @1EMP#            9 0          EMP#
08   C                   PARM      KKDIV         @1DIV             5 0          DIV
09   C                   PARM      KKACDT        @1ACDT            7 0          ACDT
10   C                   PARM      SCACDS        @1ACDS           60            ACDS
11   C                   PARM      SCACTM        @1ACTM            6 0          ACTM
12   C                   PARM      SCLTFL        @1LTFL            1            LTFL
13   C                   PARM      SCCLAD        @1CLAD            3            CLEX
14   C                   PARM      SCCLSU        @1CLSU            3            SUPR
15   C                   PARM      SCCLFA        @1CLFA            3            FEAD
16   C                   PARM      SCMCON        @1MCON            1            MCON
17   C                   PARM                    @1RCVD            7 0          RCVD
18   C                   PARM                    @1DTRP            7 0          DTRP
19   C                   PARM                    @1DTRE            7 0          DTRE
20   C                   PARM                    @1DBDT            7 0          DBDT
21   C                   PARM                    @1ENTD            7 0          ENTD
22   C                   PARM                    @1DEDT            7 0          DOD
23   C                   PARM      SCMCA         @1MCO             1            MCO
24   C                   PARM      SCPREX        @1PREX            1            PREX
25   C                   PARM      SCODIS        @1ODIS            1            ODIS
     C*** BEG ADD *** 09/29/98 ***************************************
     C*
26   C                   PARM                    @@CCL#           25            CMMC#
27   C                   PARM                    @@NATR            2            NATR
28   C                   PARM                    @@PART            2            PART
29   C                   PARM                    @@CAUS            2            CAUS
     C*
     C*** END ADD *** 09/29/98 ***************************************
     C*
     C                   Z-ADD     SCDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         @2DTOB
     C*
     C*** BEGIN CHG *** 12/13/99 **************************************
     C**********           MOVE SCSS#     @2SS#
     C                   MOVE      KKSS#         @2SS#
     C*** END   CHG *** 12/13/99 **************************************
     C*
     C                   CALL      'WA031R'
     C                   PARM      *BLANK        @2RTCD            1
     C                   PARM                    @2SS#             9
     C                   PARM      SCLSN         @2LSN            30
     C                   PARM      SCFSN         @2FSN            15
     C                   PARM      SCMID         @2MID             1
     C                   PARM      SCADR1        @2ADR1           40
     C                   PARM      SCADR2        @2ADR2           40
     C                   PARM      SCCTY         @2CTY            25
     C                   PARM      SCST          @2ST              2
     C                   PARM      SCZIP1        @2ZIP1            5 0
     C                   PARM      SCZIP2        @2ZIP2            4 0
     C                   PARM      SCAREA        @2AREA            3 0
     C                   PARM      SCPHN1        @2PHN1            3 0
     C                   PARM      SCPHN2        @2PHN2            4 0
     C                   PARM                    @2DTOB            7 0
     C                   PARM      SCSEX         @2SEX             1
     C                   PARM      SCMSTS        @2MSTS            1
     C                   PARM      SCWARA        @2WARA            3 0
     C                   PARM      SCWPH1        @2WPH1            3 0
     C                   PARM      SCWPH2        @2WPH2            4 0
     C*** BEG ADD *** 10/20/03 ****************************
21   C                   PARM      SCCTRY        @2CTRY            3            Country
22   C                   PARM      SCADR3        @2ADR3           40            Address
     C*** END ADD *** 10/20/03 ****************************
     C*** BEG ADD *** 05/09/16 ****************************
23   C                   PARM      SCCARA        @2CARA            3 0
24   C                   PARM      SCCPN1        @2CPN1            3 0
25   C                   PARM      SCCPN2        @2CPN2            4 0
     C*** END ADD *** 05/09/16 ****************************
     C*
     C                   ENDIF                                                  @@WCUP = 'Y'
     C*** END ADD *** 09/21/98 ***************************************
     C*
     C* CALL F0275R IN CHANGE MODE IF CURRENT SCREEN IS 3 AND
     C* F0260R WASN'T CALLED ISOLATED FROM WORK FLOW.
     C*
     C     SCRN#         IFEQ      3
     C     STOP          ANDEQ     *OFF
     C     *IN20         ANDEQ     *ON
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*** BEGIN ADD *** 10/06/06 **************************************
     C*EXECUTE WHEN CLIENT IS #114 ONLY.
     C     XXCLT#        IFEQ      '114'
     C*
     C                   CALL      'F0263R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C*** BEG CHG *** 06/24/07 ****************************************
     C******************** PARM 'D'       ##MODE  1
     C                   PARM      'C'           ##MODE            1
     C*** END CHG *** 06/24/07 ****************************************
     C                   PARM      DBRRN         ##RRN             9 0          FMCMP
     C                   PARM      'Y'           ##CONT            1
     C*** BEGIN ADD *** 03/23/01 **************************************
     C*** BEG CHG *** 06/24/07 ****************************************
     C******************** PARM '1'       ##CALL
     C                   PARM      ' '           ##CALL
     C*** END CHG *** 06/24/07 ****************************************
     C*** END   ADD *** 03/23/01 **************************************
     C                   ELSE                                                   + XXCLT#=114
     C*** END   ADD *** 10/06/06 **************************************
     C                   CALL      'F0275R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP
     C                   PARM      DBRRN         ##RRN2            9 0          FMCMP
     C*
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   Z-ADD     3             SCRN#
     C                   EXSR      GETDTM
     C                   ENDIF
     C*
     C                   ELSE
     C                   SUB       1             SCRN#
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C**** 11/30/06 **** BEGIN ADD **********************
     C*================================================================
     C* UPDNCD - Update WCMS with Nurse Staff Code Change
     C*================================================================
     C     UPDNCD        BEGSR
     C*
     C* Client Restrictions
     C*
     C     XXCLT#        IFEQ      '139'
     C*
     C* Only do the following if this is a linked group
     C*
     C     *IN16         IFEQ      *ON
     C*** BEGIN DEL *** 03/12/08 **************************************
     C********** SVCMNU    ANDNECMCMNU
     C*** END   DEL *** 03/12/08 **************************************
     C*
     C* Retrieve the claims rrn
     C*
     C                   CALL      'WA721R'
     C                   PARM      CMCO#         $$CO#             3 0
     C                   PARM      FNWCF#        $$FND             3 0
     C                   PARM      CMFYR         $$FYR             3 0
     C                   PARM      CMCASE        $$CASE            7 0
     C                   PARM      CMCMNU        $$NRSE            3
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C**** 11/30/06 **** END   ADD **********************
     C*================================================================
     C* UPDTUP - Update User parm file upon selection (2, 5).
     C*================================================================
     C     UPDTUP        BEGSR
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   MOVEL     Q1USER        KKUSR
     C*
     C     UPKY1         CHAIN     PUP03                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CMCO#         UPCO#
     C                   MOVE      CMSUB#        UPSUB#
     C                   MOVE      CMFND         UPFND
     C                   MOVE      CMFYR         UPFYR
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         UPPGM
     C                   MOVEL     DSTIME        UPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         UPDT
     C*
     C                   UPDATE    PUP03
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* WRITES CLAIM MASTER RECORD
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVEL     Q1USER        CMIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   MOVEL     SRDTO         CMIDT
     C*
     C                   MOVEL     DSTIME        CMTIME
     C                   MOVEL     DSTIME        CMITIM
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMCMP
     C*
     C                   Z-ADD     DBRRN         @@RRN
     C*
     C*** BEGIN ADD *** 01/31/00 **************************************
     C********** CDKY      CHAINPCD01               N99
     C     CDKY          CHAIN     PCD01                              99
     C*** END   ADD *** 01/31/00 **************************************
     C*
     C     *IN99         IFEQ      *ON
     C                   EXSR      WRITED
     C*** BEGIN ADD *** 01/31/00 **************************************
     C                   ELSE
     C                   EXSR      MVSCD1
     C                   CALL      'FA704R'
     C                   PARM      CDSS#         ##SS#
     C                   PARM      CDLSN         ##LSN
     C                   PARM      CDFSN         ##FSN
     C                   PARM      CDMID         ##MID
     C                   UPDATE    PCD01
     C*** END   ADD *** 01/31/00 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 02/05/98 **************************************
     C*
     C* WRITE FIRST CLAIM HISTORY RECORD FOR THIS CLAIM
     C* BY USING FA004R.
     C*
     C                   Z-ADD     DBRRN         @@RRN
     C*
     C                   CALL      'FA004R'
     C                   PARM      'R'           ##RTCD
     C                   PARM      'O'           ##OCST            1
     C                   PARM      @@RRN         ##RRN
     C*
     C                   MOVE      *ON           OUTFLG
     C*
     C*** BEG DEL *** 09/29/98 *****************************
     C*********
     C**UPDATE*LAST CASE NUMER IN FMC1P
     C*********
     C*********  C1KY      CHAINPC101                99
     C*********
     C*********            MOVE KKCASE    C1LCS#
     C*********
     C*********            MOVELQ1USER    C1USER
     C*********            MOVELQ1PGM     C1PGM
     C*********            TIME           DSDTTM
     C*********
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     C1DT
     C*********
     C*********            MOVELDSTIME    C1TIME
     C*********
     C*********            UPDATPC101
     C*** END DEL *** 09/29/98 *****************************
     C**** 11/30/06 **** BEGIN ADD **********************
     C                   EXSR      UPDNCD
     C**** 11/30/06 **** END   ADD **********************
     C*
     C     *IN20         IFEQ      *ON
     C*
     C*** BEGIN ADD *** 10/06/06 **************************************
     C*EXECUTE WHEN CLIENT IS #114
     C     XXCLT#        IFEQ      '114'
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C                   CALL      'F0263R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      'C'           ##CALL            1
     C                   PARM      DBRRN         ##RRN             9 0          FMCMP
     C                   PARM      'Y'           ##CONT            1
     C*** BEGIN ADD *** 03/23/01 **************************************
     C                   PARM      ' '           ##CALL
     C*** END   ADD *** 03/23/01 **************************************
     C                   ELSE                                                   + XXCLT#=114
     C*** END   ADD *** 10/06/06 **************************************
     C                   CALL      'F0275R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0          FADAP
     C                   PARM      DBRRN         ##RRN2            9 0          FMCMP
     C*
     C*** BEGIN ADD *** 03/23/01 **************************************
     C                   ENDIF                                                  XXCLT#=114
     C*** END   ADD *** 10/06/06 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SETON                                        90
     C                   SETOFF                                       KL
     C                   Z-ADD     3             SCRN#
     C*** BEGIN ADD *** 10/06/06 **************************************
     C                   SETOFF                                       40
     C*** END   ADD *** 10/06/06 **************************************
     C                   MOVEL     'C'           @@MODE
     C                   Z-ADD     DBRRN         @@RRN
     C                   MOVEL     *BLANKS       @@SS#
     C                   MOVE      *ZEROS        @@ACDT
     C                   EXSR      GETDTM
     C                   EXSR      REFRSH
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITED - Write data base record for Claim demographics
     C*================================================================
     C     WRITED        BEGSR
     C*
     C* WRITES CLAIM DEMOGRAPHIC RECORD
     C*
     C                   CLEAR                   DBRCDS
     C*
     C                   EXSR      MVSCD1
     C*
     C                   MOVEL     Q1USER        CDUSER
     C                   MOVEL     Q1PGM         CDPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CDDT
     C*
     C                   MOVEL     DSTIME        CDTIME
     C*
     C                   WRITE     DBRCDS
     C*
     C                   FEOD      FMCDP
     C*
     C                   ENDSR
** CS - Case Status
Open
Reopened
Closed
