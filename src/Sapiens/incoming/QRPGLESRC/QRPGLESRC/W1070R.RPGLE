     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1070R - WORK WITH CITY/DEMOGRAPHIC CROSS REFERENCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                STATE/CITY CROSS REFERENCES BY STATE/CITY OR   *
     H*                CITY/STATE.  EACH CROSS REFERENCE CAN BE       *
     H*                ACCESSED FOR UPDATE, DISPLAY OR SELECTION      *
     H*                DEPENDING UPON THE CALLING PROGRAM.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    MENU                                                       *
     H*    W0050R02 - EMPLOYER INFORMATION MAINTENANCE                *
     H*
     H*  CALLS TO:                                                    *
     H*    W1070R02 - CITY/DEMOGRAPHIC CROSS REFERENCE (OPT 2/5, F6)  *
     H*                INFORMATION.                                   *
     H*                                                               *
     H*  CREATED: 01/26/99  PROGRAMMER: C:113 PROJECT: 102AO          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/19/13  C:267  PROJECT: Redmine 1509                       *
     H*            Update sub routine KEYSET - key fields are reversed*
     H*                                                               *
     H*  12/11/13  C:267  PROJECT: Redmine 1509                       *
     H*            Update UI by changing the textbox property for     *
     H*            SCPST2 - text transform changed from UPPERCASE to  *
     H*            CAPITALIZATION.                                    *
     H*                                                               *
     H*  NOTE: PLEASE ADD COMMENTS TO TOP OF THE REVISIONS.           *
     H* ------------------------------------------------------------- *
     H*****************************************************************
     FW1070F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMCYP     IF   E             DISK
     F                                     RENAME(PCY01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMCYL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCY01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FWMCYL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCY01:DBRCDF2)
      /COPY SCOPYWW01D
      ***---------------------------------------------------------------------------------
      ***---------------------------------------------------------------------------------
     D*
     D HD              S             75    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D                 DS
     D  XXDATE                 1     10
     D  XXDAY                  1      2
     D  XXHYP1                 3      3
     D  XXMTH                  4      5
     D  XXHYP2                 6      6
     D  XXYEAR                 7     10
     D                 DS
     D  XXEFDT                 1      8  0
     D  X1DAY                  1      2  0
     D  X1MTH                  3      4  0
     D  X1YEAR                 5      8  0
     D*
     D                 DS
     D  VIEW1                  1     31
     D  V1ST                   2      3
     D  V1CTY                  6     31
     D*
     D                 DS
     D  VIEW2                  1     31
     D  V2CTY                  1     25
     D  V2ST                  28     29
     D*
     D                 DS
     D  INFO1                  1     36
     D  I1CNTY                 5      6
     D  I1RGN                 13     15
     D  I1CHAP                21     23
     D  I1LREP                31     33
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('City/Demographic Cr-
     D                                     oss Reference')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1070R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D DELETD          C                   CONST('*Deleted')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C                   PARM                    @@VIEW            1 0
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
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
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKST
     C                   KFLD                    KKCTY
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCTY
     C                   KFLD                    KKST
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CYST          KKST
     C     *LIKE         DEFINE    CYCTY         KKCTY
     C     *LIKE         DEFINE    CYCTY         U1PDSC
     C     *LIKE         DEFINE    CYCTY         U2PDSC
     C*
     C/COPY SCOPYWW01C  hmkealy
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSST          IFNE      *BLANKS
     C     SSCTY         ORNE      *BLANKS
     C     SSCNTY        ORNE      *BLANKS
     C     SSRGN         ORNE      *ZEROS
     C     SSCHAP        ORNE      *ZEROS
     C     SSLREP        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
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
     C*
     C*
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSST
     C                   CLEAR                   SSCTY
     C                   CLEAR                   SSCNTY
     C                   CLEAR                   SSRGN
     C                   CLEAR                   SSCHAP
     C                   CLEAR                   SSLREP
     C*
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVE      *BLANKS       KKST
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVE      *BLANKS       KKCTY
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL     SCPST1        KKST
     C                   MOVEL     SCPST2        KKCTY
     C*
     C     *IN22         WHENEQ    *ON
      ********** Begin Delete - 12/19/13  *****************************
     C****               MOVEL     SCPST3        KKCTY
     C****               MOVEL     SCPST4        KKST
      ********** End Delete  -  12/19/13  *****************************
      ********** Begin Add    - 12/19/13  *****************************
     C                   MOVEL     SCPST3        KKST
     C                   MOVEL     SCPST4        KKCTY
      ********** End Add     -  12/19/13  *****************************
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCST          KKST
     C*
     C                   MOVEL     SCCTY         KKCTY
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CYST          SCST
     C                   MOVEL     CYCTY         SCCTY
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVE      CYST          V1ST
     C                   MOVE      CYCTY         V1CTY
     C                   MOVE      CYCNTY        I1CNTY
     C                   MOVE      CYRGN         I1RGN
     C                   MOVE      CYCHAP        I1CHAP
     C                   MOVE      CYLREP        I1LREP
     C                   MOVEL     VIEW1         SCDATA
     C                   MOVE      INFO1         SCDATA
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVE      CYST          V2ST
     C                   MOVE      CYCTY         V2CTY
     C                   MOVE      CYCNTY        I1CNTY
     C                   MOVE      CYRGN         I1RGN
     C                   MOVE      CYCHAP        I1CHAP
     C                   MOVE      CYLREP        I1LREP
     C                   MOVEL     VIEW2         SCDATA
     C                   MOVE      INFO1         SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     11            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     Q1PRMS        IFGT      5
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>6
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*===============================================================
     C* STRING - Subroutine to search strings
     C*===============================================================
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
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSCTY         IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  CYCTY         U1PDSC
     C     LO:HI         XLATE(P)  SSCTY         U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                              NKS
     C*
     C     SSST          IFNE      *BLANKS
     C     SSST          ANDNE     CYST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCNTY        IFNE      *BLANKS
     C     SSCNTY        ANDNE     CYCNTY
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSRGN         IFNE      *ZEROS
     C     SSRGN         ANDNE     CYRGN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSCHAP        IFNE      *ZEROS
     C     SSCHAP        ANDNE     CYCHAP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSLREP        IFNE      *BLANKS
     C     SSLREP        ANDNE     CYLREP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
** Column headings
State City name                  County  Region  Chapter  Loss Rep
City name                 State  County  Region  Chapter  Loss Rep
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
