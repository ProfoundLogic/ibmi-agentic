     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*/TITLE W0055R - WORK WITH CARRIER POLICY# SEARCH
     H*****************************************************************
     H*  DESCRIPTION:
     H*
     H*  CALLED FROM: EMPLOYER PROCESSING
     H*
     H*  CALLS TO: NONE
     H*
     H*  CREATED: 9/7/99  PROGRAMMER: C:118  PROJECT: WC933
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*   10/22/09 C:227 Support: 70332                               *
     H*                  Add functionality to restrict group as set in*
     H*                  user parameters                              *
     H*                                                               *
     H*   06/22/05 C:58  Support: WC1313
     H*                  Add Employer/DBA and Federal names to subset.
     H*
     H* 05/26/04 C:15  Support: WC128/42607                           *
     H*                Chgd code to retrieve the CO# from WMUPP,      *
     H*                instead of SMUPP.                              *
     H*                                                               *
     H* 12/03/02 C:136 SUPPORT WA099 #35035                           *
     H*                Expand V1CAP# by one digit to accomodate       *
     H*                RPCAP#. Adjusted I2DATA to fit in SCDATA       *
     H*                without chopping off the name.                 *
     H*                                                               *
     H* 05/29/02 C:61  SUPPORT 32290                                  *
     H*                Don't show Emp name if don't have Emp record.  *
     H*                                                               *
     H* 12/05/00 C:61  SUPPORT 25307                                  *
     H*                Correct position to.                           *
     H*
     H*  10/06/00  C:33  SEQ#: WA113/24596
     H*                  LOAD KEY VALUE BEFOR CHAINING TO WDELP
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*
     H*  11 - SUBSET
     H*  21 - VIEW
     H*  60 - GENERAL CHAIN
     H*  95 - SUBSET
     H*  96 - SUBSET
     H*
     H*****************************************************************
     H*  FILE USAGE:
     H*
     H*  WMRPP - CARRIER/STATE REPORTING MASTER FILE
     H*  WMRPL3 - CARRIER/STATE REPORTING LOGICAL FILE
     H*  WMEML - EMPLOYER MASTER LOGICAL FILE
     H*  SMUPL - SYSTEM USER PARAMETER FILE
     H*  WDELL - EMPLOYER DETAIL LOGICAL FILE
     H*
     H*****************************************************************
     FW0055F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMRPP     IF   E             DISK
     F                                     RENAME(PRP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMRPL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRP01:DBRCDF)
     F*
     F*  Employer master physical file
     FWMEML     IF   E           K DISK
     F*
     F*  User parameter file
     F*** Begin chg *** 05/26/04 **********************************
     F******SMUPL   IF  E           K        DISK
     FWMUPL     IF   E           K DISK
     F*** Eeg chg ***** 05/26/04 **********************************
     F*
     F*  Employer detail logical file
     FWDELL     IF   E           K DISK
     F*
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Carrier Policy Numbe-
     D                                     rs')
     D LO              C                   CONST('abcdefghijklmnopqr-
     D                                     stuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQR-
     D                                     STUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0055R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(15)
     D*
     D***** BEG ADD ******* 06/22/05 **********************************
     D*  Data structure for subset name scans.
     D                 DS                  INZ
     D  U1PNAM                 1    100
     D  SSDTA1                 1     40
     D  SSDTA2                41    100
     D*
     D***** END ADD ******* 06/22/05 **********************************
     D*
     D*  Data structure for screen display
     D*
     D                 DS
     D  V1DATA                 1     40
      *** BEG CHG **** 12/03/02 ************************************************
     D*************************************** 1  17 V1CAP#
     D  V1CAP#                 1     18
      *** END CHG **** 12/03/02 ************************************************
     D  V1NAME                20     40
     D*
     D                 DS
     D  I1DATA                 1     34
     D  I1FND                  3      5
     D  I1EMP#                 8     16
     D  I1DIV                 19     23
     D  I1FYR                 26     28
     D  I1CARR                32     34
     D*
     D                 DS
      *** BEG CHG **** 12/03/02 ************************************************
     D*************************************** 1  36 I2DATA
     D  I2DATA                 1     34
      *** END CHG **** 12/03/02 ************************************************
     D  I2EFDT                 6     16
      *** BEG CHG **** 12/03/02 ************************************************
     D************************************** 19  36 I2EXDT
     D  I2EXDT                19     34
      *** END CHG **** 12/03/02 ************************************************

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*** BEG ADD 10/22/09 ********************************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C*** BEG ADD 10/22/09 ********************************************
     C*
     C*  File for changing values for display
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*  KEY LIST FOR READ AND READP
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  KEY LIST FOR SET GREATER THAN AND LOWER LIMITS
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCAP#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCARR
     C*
     C*  KEY LIST FOR EMPLOYER MASTER
     C*
     C     EMKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  KEY LIST FROM EMPLOYER DETAIL
     C*
     C     ELKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*** BEG ADD *** 10/22/09 ****************************************
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** END ADD *** 10/22/09 ****************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPEMP#        KKEMP#
     C     *LIKE         DEFINE    RPDIV         KKDIV
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCARR        KKCARR
     C     *LIKE         DEFINE    RPCARR        TEMP
     C     *LIKE         DEFINE    EMNAME        SCNAME
     C     *LIKE         DEFINE    RPCAP#        KKCAP#
     C***** BEG DEL ******* 06/22/05 **********************************
     C     *LIKE         DEFINE    RPCAP#        U1PDSC
     C     *LIKE         DEFINE    EMNAME        U2PNAM
     C***** END DEL ******* 06/22/05 **********************************
     C     *LIKE         DEFINE    RPCAP#        U2PDSC
     C     *LIKE         DEFINE    UPCO#         SCCO#
     C*** BEG ADD *** 10/22/09 ****************************************
     C     *LIKE         DEFINE    RPFND         @@FND
     C*** END ADD *** 10/22/09 ****************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSCARR        IFNE      *BLANKS
     C     SSCAP#        ORNE      *BLANKS
     C***** BEG ADD ******* 06/22/05 **********************************
     C     SSNAME        ORNE      *BLANKS
     C     SSFDNM        ORNE      *BLANKS
     C***** END ADD ******* 06/22/05 **********************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSCARR<>BLKS
     C*                                                    SSCAP#<>BLKS
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*  Get company number from user parameters
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     UPCO#         KKCO#
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      *LOVAL        KKCAP#
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKCAP#
      ***** BEG ADD ******* 12/05/00 **********************************
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C                   MOVE      *LOVAL        KKFYR
     C                   MOVE      *LOVAL        KKCARR
      ***** END ADD ******* 12/05/00 **********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCCAP#        KKCAP#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   Z-ADD     SCFYR         KKFYR
     C                   MOVEL(P)  SCCARR        KKCARR
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*
     C                   MOVEL(P)  RPCAP#        V1CAP#
     C*
     C*  Format numeric fields for output
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  RPFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I1FND
     C*
     C                   MOVEL(P)  RPEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I1EMP#
     C*
     C                   MOVEL(P)  RPDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I1DIV
     C*
     C                   MOVEL(P)  RPFYR         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I1FYR
     C*
     C                   MOVEL(P)  RPCARR        I1CARR
     C*
     C*  Load key fields for employer master to retrieve employer name
     C*
     C                   Z-ADD     RPFND         KKFND
     C                   Z-ADD     RPEMP#        KKEMP#
     C                   Z-ADD     RPDIV         KKDIV
     C     EMKEY         CHAIN     PEM01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EMNAME        V1NAME
      ***** BEG ADD ******* 05/29/02 **********************************
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       V1NAME
      ***** END ADD ******* 05/29/02 **********************************
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  Retrieve eff. date and exp. date from employer detail and
     C*     format for display.
     C*
     C                   MOVEL(P)  'DATE6'       EDTMSK
     C***  BEGIN ADD  ***  10/06/00  *********************************
     C                   CLEAR                   I2DATA
     C                   Z-ADD     RPFYR         KKFYR
     C***  END   ADD  ***  10/06/00  *********************************
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZEFDT            6 0
     C*
     C                   MOVEL(P)  ZZEFDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I2EFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZEXDT            6 0
     C*
     C                   MOVEL(P)  ZZEXDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         I2EXDT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  Load fields based on whether 'change info' is selected.
     C*
     C                   SELECT
     C*
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  V1DATA        SCDATA
     C                   MOVE      I1DATA        SCDATA
     C*
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  V1DATA        SCDATA
     C                   MOVE      I2DATA        SCDATA
     C*
     C                   ENDSL
     C*
     C*  Move record values into hidden subfile screen fields
     C*
     C                   MOVEL(P)  RPCAP#        SCCAP#
     C                   Z-ADD     RPFND         SCFND
     C                   Z-ADD     RPEMP#        SCEMP#
     C                   Z-ADD     RPDIV         SCDIV
     C                   Z-ADD     RPFYR         SCFYR
     C                   MOVEL(P)  RPCARR        SCCARR
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEG ADD *** 10/22/09 ****************************************
     C     ##XFND        IFEQ      'Y'
     C     UPKY          CHAIN     PUP01                              99
     C                   Z-ADD     UPFND         @@FND
     C                   ENDIF
     C*** END ADD *** 10/22/09 ****************************************
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1A         SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1A         SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* STRING - Subroutine to search strings
     C*================================================================
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      25            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      25            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*================================================================
     C* STRIN2 - Subroutine to search names: 40 in 100 characters
     C*================================================================
     C     STRIN2        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PNAM
     C                   PARM      100           STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PNAM
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*** BEG ADD *** 10/22/09 ****************************************
     C*
     C*  Check if group restriction exists.
     C*
     C     ##XFND        IFEQ      'Y'
     C     @@FND         IFNE      *ZERO
     C     @@FND         ANDNE     RPFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                              RR
     C                   ENDIF                                                              RR
     C*** END ADD *** 10/22/09 ****************************************
     C*
     C*  Check if subsetted on carrier code
     C*
     C     SSCARR        IFNE      *BLANKS
     C     LO:UP         XLATE(P)  SSCARR        SSCARR
     C*
     C     SSCARR        IFNE      RPCARR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSCARR<>RPCARR
     C*
     C                   ENDIF                                                  SSCARR<>BLKS
     C*
     C*  Check carrier policy# against subset carrier policy#
     C*
     C     SSCAP#        IFNE      *BLANKS
     C***** BEG CHG ******* 06/22/05 **********************************
     C********   MEETSS    ANDEQ*ON
     C***** END CHG ******* 06/22/05 **********************************
     C*
     C     LO:UP         XLATE(P)  RPCAP#        U1PDSC
     C     LO:UP         XLATE(P)  SSCAP#        U2PDSC
     C*
     C                   EXSR      STRING
     C*
     C                   ENDIF                                                  SSCAP#<>BLK
     C*
     C***** BEG ADD ******* 06/22/05 **********************************
     C     MEETSS        IFEQ      *ON
     C     SSNAME        IFNE      *BLANKS
     C     SSFDNM        ORNE      *BLANKS
     C                   Z-ADD     RPFND         KKFND
     C                   Z-ADD     RPEMP#        KKEMP#
     C                   Z-ADD     RPDIV         KKDIV
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON                                                NO
     C                   MOVE      *OFF          MEETSS                            employer
     C*                                                    dont display
     C*
     C                   ELSE                                                   found employ
     C*                                                     scan name
     C     SSNAME        IFNE      *BLANKS
     C     LO:UP         XLATE(P)  SSNAME        U2PNAM
     C     LO:UP         XLATE(P)  EMNAME        SSDTA1
     C     LO:UP         XLATE(P)  EMNAM2        SSDTA2
     C                   EXSR      STRIN2
     C                   ENDIF                                                   e.NAME<>BLK
     C*
     C     MEETSS        IFEQ      *ON
     C     SSFDNM        IFNE      *BLANKS
     C     LO:UP         XLATE(P)  SSFDNM        U2PNAM
     C     LO:UP         XLATE(P)  EMFDNM        SSDTA1
     C     LO:UP         XLATE(P)  EMFDN2        SSDTA2
     C                   EXSR      STRIN2
     C                   ENDIF                                                   e.FDNM<>BLK
     C                   ENDIF                                                   e.MEETSS=ON
     C*
     C                   ENDIF                                                   e.60 = OFF
     C                   ENDIF                                                   e.NAME,FDNM
     C                   ENDIF                                                   e.MEETSS=ON
     C***** END ADD ******* 06/22/05 **********************************
     C*
     C                   ENDSR                                                  SUBSET
     C*================================================================
** Column headings
Carrier policy#    Employer name          Grp  Employer#    Div   Yr  Carrier
Carrier policy#    Employer name             Eff date     Exp date
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
209F9=Change info           YYY
212F12=Cancel               NNN
217F17=Subset               YYY
3
