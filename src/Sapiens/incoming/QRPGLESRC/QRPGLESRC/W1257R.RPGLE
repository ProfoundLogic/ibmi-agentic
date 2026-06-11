     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W1257R - Direct Debit Transmission History Search
     H*****************************************************************
     H* Description:  This program will display a list of direct      *
     H*   debit transmission history records.                         *
     H*                                                               *
     H* Called from:  W1255R                                          *
     H* Calls to:     none                                            *
     H*                                                               *
     H* Created by:  C:136    04/11/03                                *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 06/17/03 C:136 SUPPORT                                        *
     H*                PAGE VALUE INCORRECT.                          *
     H*                                                               *
     H*****************************************************************
     H* Indicators:                                                   *
     H*                                                               *
     H*****************************************************************
     FW1257F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMDHP     IF   E             DISK
     F                                     RENAME(PDH01:PFRCDF)
     F*
     FWMDHL3    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDH01:DBRCDF)
     F*
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(6) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(6) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D***PY SCOPYWW01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Direct Debit Transm-
     D                                     ission History')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
      *** BEG CHG ***** 06/17/03 ***********************************************
     D************  12                    C         SF1PAG
     D SF1PAG          C                   CONST(14)
      *** END CHG ***** 06/17/03 ***********************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
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
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKPODT
     C                   KFLD                    KKBAT#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DHFND         KKFND
     C     *LIKE         DEFINE    DHPODT        KKPODT
     C     *LIKE         DEFINE    DHBAT#        KKBAT#
     C*
     C/COPY SCOPYWW01C
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSFND         IFNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
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
     C                   Z-ADD     0             SSFND
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKFND
     C                   MOVE      *LOVAL        KKPODT
     C                   MOVE      *LOVAL        KKBAT#
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        KKFND
     C                   Z-ADD     SCPS1B        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                         MDY --> YMD
     C                   MOVEL     SRDTO         KKPODT
     C                   Z-ADD     SCPS1C        KKBAT#
     C*
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *** BEG CHG ***** 06/17/03 ***********************************************
     C                   MOVEL     SCFND         KKFND
     C                   Z-ADD     SCFND         KKFND
      *** END CHG ***** 06/17/03 ***********************************************
     C                   Z-ADD     SCPODT        KKPODT
     C                   Z-ADD     SCBAT#        KKBAT#
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   Z-ADD     DHFND         SCFND
     C                   Z-ADD     DHPODT        SCPODT
     C                   Z-ADD     DHPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         S1PODT
     C                   Z-ADD     DHBAT#        SCBAT#
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
     C                   SETON                                        LR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21##CO#         READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21##CO#         READPE    DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSFND         IFNE      0
     C     DHFND         ANDNE     SSFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSR
     C*================================================================
** Column headings - HD
Group   Posting date     Batch#
** Description--------------MISN
1011=Select                 NNN
203F3=Exit                  NNN
205F5=Refresh               YNN
212F12=Cancel               YNN
217F17=Subset               YNN
3
