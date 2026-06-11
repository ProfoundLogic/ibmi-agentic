     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0851R - WORK WITH QUOTE TRANSFERS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL QUOTE  *
     H*                EMPLOYERS TO BE TRANSFERED IN THE DAILY BATCH. *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*  CALLS TO:                                                    *
     H*    W0851R02 - QUOTE TRANSFER INFORMATION (OPT 2/4/5)          *
     H*                                                               *
     H*  CREATED: 10/28/97  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/07/04  C:132 PROJECT: WC1017  PRISS 27                    *
     H*                  FIX subset for New Business/Renewals         *
     H*                                                               *
     H*  10/10/03  C:33  PROJECT: WC1017                              *
     H*                  Add subset for New Business/Renewals         *
     H*                                                               *
     H*  03/20/98  C:33  PROJECT: WC577     WA091/10801               *
     H*                  FIX PAGING AND REPOSTION                     *
     H*                                                               *
     H*  10/28/97  C:33  PROJECT: WC577                               *
     H*                  V3 ENHANCEMENTS                              *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  25 - DO NOT RUN A REVISED BILL IF ACCOUNT IS AUDITED.        *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0851F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWAQOP     IF   E             DISK
     F                                     RENAME(PQO01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWAQOL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PQO01:DBRCDF)
     FWMEML     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Quote Transfers')
     D DELETD          C                   CONST('*Deleted')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0851R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*
     D*================================================================
     D*  DATA STRUCTURES
     D*================================================================
     D*
     D                 DS
     D  DATA                   1     74
     D  XXFND                  2      4
     D  XXEMP#                 7     15
     D  XXDIV                 18     22
     D  EMNAME                26     65
     D***  Begin Add  ***  10/10/03  **************************************
     D  SCRQFL                68     68

     I***  End   Add  ***  10/10/03  **************************************
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
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
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     EMKY          KLIST
     C                   KFLD                    QOCO#
     C                   KFLD                    QOFND
     C                   KFLD                    QOEMP#
     C                   KFLD                    QODIV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    QOFND         KKFND
     C     *LIKE         DEFINE    QOEMP#        KKEMP#
     C     *LIKE         DEFINE    QODIV         KKDIV
     C     *LIKE         DEFINE    QOFND         @@FND
     C     *LIKE         DEFINE    EMNAME        U1PDSC
     C     *LIKE         DEFINE    EMNAME        U2PDSC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSFND         IFNE      *ZEROS
     C     SSNAME        ORNE      *BLANKS
      ***** BEG ADD ***** 04/07/04 ****************************************
     C     SSNRB         ORNE      *BLANKS
      ***** END ADD ***** 04/07/04 ****************************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C***  Begin Add  ***  10/10/03  **************************************
     C  N90SSNRB         IFNE      ' '
     C     SSNRB         ANDNE     'N'
     C     SSNRB         ANDNE     'R'
     C                   SETON                                        71  90
     C                   ENDIF
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSFND
     C                   CLEAR                   SSNAME
     C***  Begin Add  ***  10/10/03  **************************************
     C                   CLEAR                   SSNRB
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKFND
     C                   CLEAR                   KKEMP#
     C                   CLEAR                   KKDIV
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C***  BEGIN CHG  ***  03/20/98  *********************************
     C**********           Z-ADDSCFND     KKFND
     C**********           Z-ADDSCEMP#    KKEMP#
     C**********           Z-ADDSCDIV     KKDIV
     C                   MOVE      SCPST1        KKFND
     C                   MOVE      SCPSTA        KKEMP#
     C                   MOVE      SCPSTB        KKDIV
     C***  END   CHG  ***  03/20/98  *********************************
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C***  BEGIN CHG  ***  03/20/98  *********************************
     C**********           MOVE SCPST1    KKFND
     C**********           MOVE SCPSTA    KKEMP#
     C**********           MOVE SCPSTB    KKDIV
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C***  END   CHG  ***  03/20/98  *********************************
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C***  BEGIN ADD  ***  03/20/98  *********************************
     C                   Z-ADD     QOFND         SCFND
     C                   Z-ADD     QOEMP#        SCEMP#
     C                   Z-ADD     QODIV         SCDIV
     C***  END   ADD  ***  03/20/98  *********************************
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  QOFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFND
     C*
     C                   MOVEL(P)  QOEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEMP#
     C*
     C                   MOVEL(P)  QODIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDIV
     C*
      ***** BEG DEL ***** 04/07/04 ****************************************
     C**********           CLEAREMNAME
     C***  Begin Add  ***  10/10/03  **************************************
     C**********           CLEAREMRQFL
     C***  End   Add  ***  10/10/03  **************************************
     C********** EMKY      CHAINPEM01                60
      ***** END DEL ***** 04/07/04 ****************************************
     C*
     C***  Begin Add  ***  10/10/03  **************************************
     C                   SELECT
     C     EMRQFL        WHENEQ    'Y'
     C                   MOVE      'R'           SCRQFL
     C     EMRQFL        WHENNE    'Y'
     C                   MOVE      'N'           SCRQFL
     C                   ENDSL
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   MOVEL(P)  DATA          SCDATA
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C                   CLEAR                   EMNAME
     C                   CLEAR                   EMRQFL
     C     EMKY          CHAIN     PEM01                              60
      ***** END ADD ***** 04/07/04 ****************************************
     C*
     C*  CHECK EMPLOYERS GROUP AGAINST SUBSET GROUP
     C*
B01  C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     EMFND
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  EMFND<>SSFND<>*ZEROS
     C*
     C*  CHECK EMPLOYERS DBA AGAINST SEARCH STRING
     C*
B01  C     SSNAME        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE(P)  EMNAME        U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSNAME<>*BLANKS
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*  Check for new business vs. renewal
     C*
     C     SSNRB         IFNE      ' '
     C*
     C                   SELECT
     C     SSNRB         WHENEQ    'R'
      ***** BEG CHG ***** 04/07/04 ****************************************
     C********   SCRQFL    ANDNE'R'
     C     EMRQFL        ANDNE     'Y'
      ***** END CHG ***** 04/07/04 ****************************************
      ***** BEG CHG ***** 04/07/04 ****************************************
      ***** END CHG ***** 04/07/04 ****************************************
     C                   MOVE      *OFF          MEETSS
     C     SSNRB         WHENEQ    'N'
      ***** BEG CHG ***** 04/07/04 ****************************************
     C*********  SCRQFL    ANDNE'N'
     C     EMRQFL        ANDEQ     'Y'
      ***** END CHG ***** 04/07/04 ****************************************
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
     C*
     C                   ENDIF
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt  Grp  Employer#    Div   Name                                     N/R
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
