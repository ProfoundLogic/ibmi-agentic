     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  TITLE:  W0045R
     H*  WORK WITH PREMIUM FINANCE COMPANIES
     H*
     H*****************************************************************
     H*
     H*  DESCRIPTION:  THIS PROGRAM ALLOWS USER TO ADD, CHANGE OR
     H*                DISPLAY THE PREMIUM FINANCE COMPANIES ASSIGNED
     H*                TO A GROUP.  FROM EMPLOYER DETAIL A USER MAY
     H*                ASSIGN A COMPANY TO A POLICY.
     H*
     H*****************************************************************
     H*
     H*  CALLED FROM: WORK WITH GROUPS
     H*               WORK WITH EMPLOYER DETAIL
     H*
     H*  CALLS TO:  W0045R02
     H*
     H*****************************************************************
     H*
     H*  CREATED:  09/22/99  PROGRAMMER:  C:118  PROJECT:  WC939
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  07/11/00  C:33  PROJECT: WC939  WA095/23491
     H*                  CORRECT PAGING.
     H*****************************************************************
     H*
     H*  FILE USAGE:
     H*  WMPFP - PREMIUM FINANCE PHYSICAL FILE
     H*  WMPFL - PREMIUM FINANCE LOGICAL - PFCO#/PFFND/PFFCOD/PFEFDT
     H*  WMPFL1 - PREMIUM FINANCE LOGICAL - PFCO#/PFFND/PFNAME/PFEFDT
     H*  WMFNP - FUND MASTER PHYSICAL FILE
     H*
     H*****************************************************************
     H*
     H*  INDICATOR USAGE:
     H*  21 - CHANGE VIEW 1
     H*  22 - CHANGE VIEW 2
     H*  60 - GENERAL CHAIN
     H*  95 - READ
     H*  96 - SETGT AND SETLL
     H*
     H*****************************************************************
     F*
     FW0045F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMPFP     IF   E             DISK
     F                                     RENAME(PPF01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMPFL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPF01:DBRCDF)
     F*
     F*  Logical file definition (for displaying items, 2nd view)
     FWMPFL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPF01:DBRCDF2)
     F*
     F*  Fund master file
     FWMFNP     IF   E             DISK
     F*
      /COPY SCOPYWW01D
     D*
     D HDA             S             78    DIM(2) CTDATA PERRCD(1)              Column heading
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
     D OPTNM           C                   CONST('Premium Finance Comp-
     D                                     anies')
     D LO              C                   CONST('abcdefghijklmnopqr-
     D                                     stuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQR-
     D                                     STUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0045R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D***  BEGIN CHG  ***  07/11/00  *********************************
     D**********    12                    C         SF1PAG
     D SF1PAG          C                   CONST(10)
     D***  END   CHG  ***  07/11/00  *********************************
     D*
     D*  Data structure for screen display
     D*
     D                 DS
     D  V1DATA                 1     72
     D  V1FCOD                 1      5
     D  V1EFDT                 8     17
     D  V1EXDT                20     29
     D  V1NAME                32     56
     D  V1AREA                59     63
     D  V1PHN#                65     72
     D*
     D                 DS
     D  V2DATA                 1     72
     D  V2NAME                 1     25
     D  V2EFDT                28     37
     D  V2EXDT                40     49
     D  V2FCOD                52     56
     D  V2AREA                59     63
     D  V2PHN#                65     72

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          WMFNP
     C                   PARM                    @@RRN2            9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Date format parameter lists
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
     C*  Screen format parameter list
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     PFKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFCOD
     C                   KFLD                    KKEFDT
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKNAME
     C                   KFLD                    KKEFDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PFCO#         KKCO#
     C     *LIKE         DEFINE    PFFND         KKFND
     C     *LIKE         DEFINE    PFFCOD        KKFCOD
     C     *LIKE         DEFINE    PFEFDT        KKEFDT
     C     *LIKE         DEFINE    PFNAME        KKNAME
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C* Load company and fund
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNCO#         SCCO#
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNNM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      *LOVAL        KKFCOD
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      *LOVAL        KKFCOD
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKNAME
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        KKFCOD
     C*
     C                   MOVE      SCPS1A        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  SCPST2        KKNAME
     C*
     C                   MOVE      SCPS2A        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCFCOD        KKFCOD
     C                   MOVEL     SCEFDT        KKEFDT
     C                   MOVEL     SCNAME        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      PFFCOD        V1FCOD
     C                   MOVE      PFFCOD        V2FCOD
     C*
     C*  Format date for screen display
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVE      *BLANKS       XXNUM
     C*
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   Z-ADD     PFEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ZZEFDT            8 0
     C*
     C                   MOVEL(P)  ZZEFDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         V1EFDT
     C                   MOVEL(P)  XXNUM         V2EFDT
     C*
     C                   Z-ADD     PFEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ZZEXDT            8 0
     C*
     C                   MOVEL(P)  ZZEXDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         V1EXDT
     C                   MOVEL(P)  XXNUM         V2EXDT
     C*
     C                   MOVEL(P)  PFNAME        V1NAME
     C                   MOVEL(P)  PFNAME        V2NAME
     C*
     C*  Concatenate area code with ()
     C*
     C                   MOVEL(P)  PFAREA        XXARE1            3
     C                   MOVE      '('           TEMP1             1
     C                   MOVE      ')'           TEMP2             1
     C     TEMP1         CAT       XXARE1        TEMP3             4
     C     TEMP3         CAT       TEMP2         XXAREA            5
     C                   MOVEL(P)  XXAREA        V1AREA            5
     C                   MOVEL(P)  XXAREA        V2AREA            5
     C*
     C*  Concatenate phone number with -
     C*
     C                   MOVEL(P)  PFPHN#        XXPHN1            3
     C                   MOVE      PFPHN#        XXPHN2            4
     C                   MOVE      '-'           TEMP4             1
     C     XXPHN1        CAT       TEMP4         TEMP5             4
     C     TEMP5         CAT       XXPHN2        XXPHN#            8
     C                   MOVEL     XXPHN#        V1PHN#            8
     C                   MOVEL     XXPHN#        V2PHN#            8
     C                   MOVE      PFPHN#        V1PHN2            4
     C                   MOVE      PFPHN#        V2PHN2            4
     C*
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C                   MOVE      V1DATA        SCDATA
     C                   MOVE      HDA(1)        SCHDLA
     C*
     C     VIEW#         WHENEQ    2
     C                   MOVE      V2DATA        SCDATA
     C                   MOVE      HDA(2)        SCHDLA
     C*
     C                   ENDSL
     C*
     C*  Move record values into hidden subfile screen fields
     C*
     C                   Z-ADD     PFCO#         SCCO#
     C                   Z-ADD     PFFND         SCFND
     C                   MOVEL     PFFCOD        SCFCOD
     C                   Z-ADD     PFEFDT        SCEFDT
     C                   MOVEL     PFNAME        SCNAME
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C*  Save @@RRN coming from WW Groups
     C*
     C                   MOVE      @@RRN         ##RRN2
     C*
     C*  Save @@RRN coming from WW Employer Detail
     C*
     C                   MOVE      @@RRN         ##RRN
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21PFKY          READE     DBRCDF                                 95
     C   22PFKY          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21PFKY          READPE    DBRCDF                                 95
     C   22PFKY          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C*  Load RRN for return to WW Employer Detail
     C*
     C                   Z-ADD     SCDBRR        @@RRN2
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
     C*
** Column headings - first line
    Company  Effective  Expiration
                                 Effective  Expiration Company
** Column headings - second line
Opt  Code      Date        Date     Name                       Phone#
Opt  Name                          Date        Date      Code  Phone#
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
