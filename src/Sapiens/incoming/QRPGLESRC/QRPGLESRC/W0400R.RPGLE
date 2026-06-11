     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W0400R - WORK WITH LOSS CONTROL TRACKING
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM: Menu                                            *
     H*                                                               *
     H*  CALLS TO:    W0401R                                          *
     H*               SA006R                                          *
     H*               W0400C                                          *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/20/00 C:61 SUPPORT                                        *
     H*                Correct paging issue for entries with same     *
     H*                visit date.                                    *
     H*                                                               *
     H*  06/01/99 C:61 PROJECT 102BG                                  *
     H*              - Automatically default to subset on user's      *
     H*                staff code.                                    *
     H*              - Disallow change of staff code subset value     *
     H*                without security authorization.                *
     H*                                                               *
     H*  05/14/99 C:113 PROJECT: 102BF                                *
     H*                 Changed subset to use a date range.           *
     H*                                                               *
     H*  04/27/99  C:33  PROJECT: 102AE                               *
     H*                  LOAD VALUE TO SCDATA AFTER DELETION          *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  ERROR:                                                       *
     H*  80 - (WCR0500) INVALID DATE ENTERED                          *
     H*  81 - (WCR0505) RECORD NOT FOUND FOR THIS INFORMATION         *
     H*  82 - (WCR0505) RECORD NOT FOUND FOR THIS INFORMATION         *
     H*                                                               *
     H*****************************************************************
      *
      *
     FW0400F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMLCP     IF   E             DISK
     F                                     RENAME(PLC01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 05/14/99 **************************************
     F********** WMLCL   IF  E           K        DISK
     FWMLCL     IF   E           K DISK    USROPN
     F*** END   CHG *** 05/14/99 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PLC01:DBRCDF1)
     F*** BEGIN CHG *** 05/14/99 **************************************
     F********** WMLCL1  IF  E           K        DISK
     FWMLCL1    IF   E           K DISK    USROPN
     F*** END   CHG *** 05/14/99 **************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PLC01:DBRCDF2)
     F*** BEGIN CHG *** 05/14/99 **************************************
     F********** WMLCL2  IF  E           K        DISK
     FWMLCL2    IF   E           K DISK    USROPN
     F*** END   CHG *** 05/14/99 **************************************
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PLC01:DBRCDF3)
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F*  User Profile File
     FWMUPL1    IF   E           K DISK
      ***** BEG ADD ******* 06/01/99 **********************************
     FWMUPL     IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
      *
      *  Client control master
     FSXCPL     IF   E           K DISK
      ***** END ADD ******* 06/01/99 **********************************
      *
      *
      *
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
      *
      *
      *
     D*
     D***PY SCOPYWW01I
     D                 DS
     D  VIEW1                  1     74
     D  V1VSDT                 1     10
     D  V1STF                 13     15
     D  V1STFN                17     31
     D  V1EMP#                34     42
     D  V1DIV                 44     48
     D  V1EMPN                50     74
     D                 DS
     D  VIEW2                  1     74
     D  V2STF                  1      3
     D  V2STFN                 5     19
     D  V2VSDT                22     31
     D  V2EMP#                34     42
     D  V2DIV                 44     48
     D  V2EMPN                50     74
     D                 DS
     D  VIEW3                  1     74
     D  V3EMP#                 1      9
     D  V3DIV                 11     15
     D  V3EMPN                17     41
     D  V3VSDT                44     53
     D  V3STF                 56     58
     D  V3STFN                60     74
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D*** BEGIN ADD *** 05/14/99 **************************************
     D QRYPGM          C                   CONST('W0400C')
     D OVRDB1          C                   CONST('OVRDBF FILE(')
     D OVRDB2          C                   CONST(') SHARE(*YES)')
     D DELOV1          C                   CONST('DLTOVR FILE(')
     D DELOV2          C                   CONST(')')
     D*** END   ADD *** 05/14/99 **************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Loss Control Trackin-
     D                                     g')
     D***  BEGIN ADD  ***  04/27/99  *********************************
     D DLT             C                   CONST('* * * Deleted')
     D***  END   ADD  ***  04/27/99  *********************************
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0400R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(10)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
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
      ***** BEG ADD ******* 06/01/99 **********************************
     C                   PARM                    ##ULVL
     C                   PARM                    ##UTYP
      ***** END ADD ******* 06/01/99 **********************************
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **** BEGIN ADD *** 05/14/99 **************************************
     C     QRYPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##VFDT
     C                   PARM                    ##VTDT
     **** END   ADD *** 05/14/99 **************************************
      *
      *
      *
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    KKVSDT
     C                   KFLD                    KKSTF
      ***** BEG ADD ******* 06/20/00 **********************************
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
      ***** END ADD ******* 06/20/00 **********************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    KKSTF
     C                   KFLD                    KKVSDT
      ***** BEG ADD ******* 06/20/00 **********************************
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
      ***** END ADD ******* 06/20/00 **********************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKVSDT
     C                   KFLD                    KKSTF
      ***** BEG ADD ******* 06/20/00 **********************************
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
      ***** END ADD ******* 06/20/00 **********************************
     C*
     C*  SYSTEM USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    LCSTF
      ***** BEG ADD ******* 06/01/99 **********************************
      *
     C     UPKY1         KLIST
     C                   KFLD                    Q1USER
      *
      ***** END ADD ******* 06/01/99 **********************************
     C*
     C*   KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SSEMP#
     C                   KFLD                    SSDIV
      ***** BEG ADD ******* 06/01/99 **********************************
      *
      *  Client Control Master
      *
     C     XCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      *
      ***** END ADD ******* 06/01/99 **********************************
      *
      *
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LCSTF         KKSTF
     C     *LIKE         DEFINE    LCVSDT        KKVSDT
     C     *LIKE         DEFINE    LCVSDT        XSVSDT
     C     *LIKE         DEFINE    LCEMP#        KKEMP#
     C     *LIKE         DEFINE    LCDIV         KKDIV
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
      ***** BEG ADD ******* 06/20/00 **********************************
     C     *LIKE         DEFINE    LCDT          KKDT
     C     *LIKE         DEFINE    LCTIME        KKTIME
      ***** END ADD ******* 06/20/00 **********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     **** BEGIN ADD *** 05/14/99 **************************************
     C     *LIKE         DEFINE    LCVSDT        ##VFDT
     C     *LIKE         DEFINE    LCVSDT        ##VTDT
     C     *LIKE         DEFINE    LCVSDT        XSVFDT
     C     *LIKE         DEFINE    LCVSDT        XSVTDT
     C     *LIKE         DEFINE    SSVFDT        SAVFDT
     C     *LIKE         DEFINE    SSVTDT        SAVTDT
     **** END   ADD *** 05/14/99 **************************************
      ***** BEG ADD ******* 06/01/99 **********************************
     C     *LIKE         DEFINE    UPLVL         ##ULVL
     C     *LIKE         DEFINE    UPTYPE        ##UTYP
     C     *LIKE         DEFINE    UPLVL         ##FLVL
     C     *LIKE         DEFINE    UPTYPE        ##FTYP
      ***** END ADD ******* 06/01/99 **********************************
     C*
     C/COPY SCOPYWW01C
      *
      *
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTF         IFNE      *BLANKS
     C     SSEMP#        ORNE      *ZEROS
     C     SSDIV         ORNE      *ZEROS
     **** BEGIN CHG *** 05/14/99 **************************************
     C********** SSVSDT    ORNE *ZEROS
     C     SSVFDT        ORNE      *ZEROS
     C     SSVTDT        ORNE      *ZEROS
     **** BEGIN CHG *** 05/14/99 **************************************
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
     C*
     C*  EDIT VISIT DATE
     C*
     **** BEGIN DEL *** 05/14/99 **************************************
     C**********
     C********** SSVSDT    IFNE *ZEROS
     C**********
     C**********           Z-ADDSSVSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SSVSDT
     C**********
     C* N90 **** SRERR     IFEQ *ON
     C**********           SETON                     80  90
     C**********           ENDIF                           SRERR=*ON
     C**********
     C**********           Z-ADDSSVSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XSVSDT
     C**********
     C**********           ENDIF                           SSVSDT NE *ZERO
     **** END   DEL *** 05/14/99 **************************************
     **** BEGIN ADD *** 05/14/99 **************************************
     C     SSVFDT        IFNE      *ZEROS
     C     SSVTDT        ORNE      *ZEROS
     C*
     C*  The FROM date must be valid
     C*
     C                   Z-ADD     SSVFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSVFDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        80  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   Z-ADD     SSVFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##VFDT
     C*
     C*  The TO date must be valid
     C*
     C                   Z-ADD     SSVTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSVTDT
     C*
     C                   Z-ADD     SSVTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##VTDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        83  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  The dates must be in ascending order
     C*
     C  N90##VFDT        IFGT      ##VTDT
     C                   SETON                                        84  90
     C                   ENDIF
     C*
     C                   ENDIF
     **** END   ADD *** 05/14/99 **************************************
     C*
     C*  VALIDATE POLICY SERVICES STAFF CODE, IF ENTERED
     C*
     C  N90SSSTF         IFNE      *BLANKS
     C     SSSTF         CHAIN     PUP01                              60
     C   60              SETON                                        817090
     C                   ENDIF                                                  SCPSRV<>*BLANKS
     C*
     C*  VALIDATE EMPLOYER / DIVISION
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     EMKY2         CHAIN     PEM01                              60
     C   60              SETON                                        827090
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSSTF
      ***** BEG ADD ******* 06/01/99 **********************************
      *
      * Subset to user's staff code
      *
     C     UPKY1         CHAIN     PUP02                              60
     C                   MOVEL(P)  UPSTAF        SSSTF
      *
      ***** END ADD ******* 06/01/99 **********************************
     C                   CLEAR                   SSEMP#
     C                   CLEAR                   SSDIV
     **** BEGIN CHG *** 05/14/99 **************************************
     C**********           CLEARSSVSDT
     C                   CLEAR                   SSVFDT
     C                   CLEAR                   SSVTDT
     C                   CLEAR                   SAVFDT
     C                   CLEAR                   SAVTDT
     **** BEGIN CHG *** 05/14/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKVSDT
     C                   MOVE      *HIVAL        KKVSDT
     C                   CLEAR                   KKSTF
     C                   CLEAR                   KKEMP#
     C                   CLEAR                   KKDIV
     C*
     **** BEGIN ADD *** 05/14/99 **************************************
     C     SAVFDT        IFNE      SSVFDT
     C     SAVTDT        ORNE      SSVTDT
     C*
     C                   EXSR      OVRRID
     C*
     C                   ENDIF
     C*
     C*  Store the new subset values
     C*
     C                   Z-ADD     SSVFDT        SAVFDT
     C                   Z-ADD     SSVTDT        SAVTDT
     **** END   ADD *** 05/14/99 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      *HIVAL        KKVSDT
     C                   MOVE      *LOVAL        KKSTF
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
      ***** BEG ADD ******* 06/20/00 **********************************
     C                   Z-ADD     *LOVAL        KKDT
     C                   Z-ADD     *LOVAL        KKTIME
      ***** END ADD ******* 06/20/00 **********************************
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKVSDT
     C                   MOVE      SCPT1A        KKSTF
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVE      SCPT2A        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKVSDT
     C                   MOVE      SCPST2        KKSTF
     C*
     C     *IN23         WHENEQ    *ON
     C                   MOVE      SCPST3        KKEMP#
     C                   MOVE      SCPT3A        KKDIV
     C                   MOVE      SCPT3B        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKVSDT
     C                   MOVE      SCPT3C        KKSTF
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCVSDT        KKVSDT
     C                   MOVEL     SCSTF         KKSTF
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
      ***** BEG ADD ******* 06/20/00 **********************************
     C                   Z-ADD     SCDT          KKDT
     C                   Z-ADD     SCTIME        KKTIME
      ***** END ADD ******* 06/20/00 **********************************
     C*
     C                   ENDSR
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C                   ENDSR                                                  KNDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     LCVSDT        SCVSDT
     C                   MOVEL     LCSTF         SCSTF
     C                   Z-ADD     LCEMP#        SCEMP#
     C                   Z-ADD     LCDIV         SCDIV
      ***** BEG ADD ******* 06/20/00 **********************************
     C                   Z-ADD     LCDT          SCDT
     C                   Z-ADD     LCTIME        SCTIME
      ***** END ADD ******* 06/20/00 **********************************
     C*
     C*  Format date fields
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C*
     C                   Z-ADD     LCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C                   MOVEL     XXDATE        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDAT            10
     C*
     C                   MOVEL     XXDAT         V1VSDT
     C                   MOVEL     XXDAT         V2VSDT
     C                   MOVEL     XXDAT         V3VSDT
     C*
     C                   MOVEL     LCSTF         V1STF
     C                   MOVEL     LCSTF         V2STF
     C                   MOVEL     LCSTF         V3STF
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C                   MOVEL     UPUSNM        V1STFN
     C                   MOVEL     UPUSNM        V2STFN
     C                   MOVEL     UPUSNM        V3STFN
     C*
     C*  Edit stuff for output
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  LCEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1EMP#
     C                   MOVEL     XXNUM         V2EMP#
     C                   MOVEL     XXNUM         V3EMP#
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  LCDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         V1DIV
     C                   MOVEL     XXNUM         V2DIV
     C                   MOVEL     XXNUM         V3DIV
     C*
     C   21              MOVEL     V1EMP#        XXEMP#
     C   22              MOVEL     V2EMP#        XXEMP#
     C   23              MOVEL     V3EMP#        XXEMP#
     C*
     C   21              MOVEL     V1DIV         XXDIV
     C   22              MOVEL     V2DIV         XXDIV
     C   23              MOVEL     V3DIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        V1EMPN
     C                   MOVEL     EMNAME        V2EMPN
     C                   MOVEL     EMNAME        V3EMPN
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  VIEW1         SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  VIEW2         SCDATA
     C     *IN23         WHENEQ    *ON
     C                   MOVEL(P)  VIEW3         SCDATA
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - Process option 7 (Work with Loss Control Tasks)
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   CALL      'W0401R'
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL
     C                   PARM      SCDBRR        @@RRN             9 0
     C*
     C                   ENDSR                                                  OPT7
     C*================================================================
     C* OPT14 - Process option 14 (DARTS)
     C*================================================================
     C*
     C* OPTION 14=DARTS
     C*
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     ##CO#         QCCO#
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     SCEMP#        QCEMP#
     C                   Z-ADD     SCDIV         QCDIV
     C*
     C*  CALL COMMENT PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      @@PGM            10
     C*
     C                   ENDSR                                                  OPT14
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '14'          OPT14
     C                   ENDCS
     C*
     C                   ENDSR
     **** BEGIN ADD *** 05/14/99 **************************************
     C*================================================================
     C* OVRDBF - Subroutine to override the database files for query
     C*================================================================
     C     OVRDBF        BEGSR
     C*
     C     OVRDB1        CAT(P)    'WMLCL':0     XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMLCL1':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     OVRDB1        CAT(P)    'WMLCL2':0    XXOVRD           50
     C                   CAT       OVRDB2:0      XXOVRD
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXOVRD        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C                   ENDSR                                                  OVRDBF
     C*================================================================
     C* OVRRID - Subroutine to run the query CL
     C*================================================================
     C     OVRRID        BEGSR
     C*
     C                   CLOSE     WMLCL
     C                   CLOSE     WMLCL1
     C                   CLOSE     WMLCL2
     C*
     C* Delete the overrides
     C*
     C     DELOV1        CAT(P)    'WMLCL':0     XXDOVR           50
     C                   CAT       DELOV2:0      XXDOVR
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXDOVR        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     DELOV1        CAT(P)    'WMLCL1':0    XXDOVR           50
     C                   CAT       DELOV2:0      XXDOVR
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXDOVR        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C     DELOV1        CAT(P)    'WMLCL2':0    XXDOVR           50
     C                   CAT       DELOV2:0      XXDOVR
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      XXDOVR        CMDSTR           50
     C                   PARM      31            LENGTH           15 5
     C*
     C*  We must reclaim the resources to clear the last query run
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM      'RCLRSC'      XXSTR             6
     C                   PARM      6             LENGTH           15 5
     C*
     C     SSVFDT        IFNE      *ZERO
     C     SSVTDT        ORNE      *ZERO
     C*
     C*  Override the files
     C*
     C                   EXSR      OVRDBF
     C*
     C*  Call CL to create an open query file over these files.
     C*
     C                   CALL      QRYPGM        QRYPL                  99
     C*
     C                   ENDIF                                                  SSVFDT != 0
     C*
     C                   OPEN      WMLCL
     C                   OPEN      WMLCL1
     C                   OPEN      WMLCL2
     C*
     C                   ENDSR                                                  OVRRID
     **** END   ADD *** 05/14/99 **************************************
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     **** BEGIN ADD *** 05/14/99 **************************************
     C                   CLOSE     WMLCL
     C                   CLOSE     WMLCL1
     C                   CLOSE     WMLCL2
     **** END   ADD *** 05/14/99 **************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCFNNM
     **** BEGIN ADD *** 05/14/99 **************************************
     C*
     C*  Override the files
     C*
     C                   EXSR      OVRDBF
     C*
     C                   OPEN      WMLCL
     C                   OPEN      WMLCL1
     C                   OPEN      WMLCL2
     **** END   ADD *** 05/14/99 **************************************
      ***** BEG ADD ******* 06/01/99 **********************************
      *
      * Check user security level for subset staff code update
      * access.
      *
     C     XCKY          CHAIN     SCP01                              60
      *
     C  N60              CALL      'SA006R'
     C                   PARM                    ##ULVL                         USER LEVEL
     C                   PARM                    ##UTYP                         USER TYPE
     C                   PARM      XCSL01        ##FLVL                         FILE LEVEL
     C                   PARM      XCST01        ##FTYP                         FILE TYPE
     C                   PARM      *BLANK        ##RTCD            1            RETURN CODE
      *
     C     *IN60         IFEQ      *ON
     C     ##RTCD        OREQ      'F'
     C                   SETON                                        51
     C                   ENDIF
      *
      ***** END ADD ******* 06/01/99 **********************************
     C*
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY           READE     DBRCDF1                                95
     C   22KEY           READE     DBRCDF2                                95
     C   23KEY           READE     DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF1                                95
     C   22KEY           READPE    DBRCDF2                                95
     C   23KEY           READPE    DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF1                            96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF1                            96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C***  BEGIN ADD  ***  04/27/99  *********************************
     C                   MOVEL     DLT           SCDATA
     C***  END   ADD  ***  04/27/99  *********************************
     C*
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
     C*
     C                   SELECT
     C*
     C*  CHECK STAFF CODE VS SUBSET STAFF CODE
     C*
     C     SSSTF         WHENNE    *BLANKS
     C     LCSTF         ANDNE     SSSTF
     C                   MOVE      *OFF          MEETSS
     C*
     C*  CHECK EMP# VS SUBSET EMP#
     C*
     C     SSEMP#        WHENNE    *ZEROS
     C     LCEMP#        ANDNE     SSEMP#
     C                   MOVE      *OFF          MEETSS
     C*
     C*  CHECK DIV VS SUBSET DIV
     C*
     C     SSDIV         WHENNE    *ZEROS
     C     LCDIV         ANDNE     SSDIV
     C                   MOVE      *OFF          MEETSS
     C*
     C*  CHECK VISIT DATE VS SUBSET VISIT DATE
     C*
     **** BEGIN CHG *** 05/14/99 **************************************
     C********** SSVSDT    WHNE *ZEROS
     C********** LCVSDT    ANDNEXSVSDT
     C**********           MOVE *OFF      MEETSS
     C*
     C     SSVFDT        WHENNE    *ZEROS
     C     LCVSDT        ANDLT     ##VFDT
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSVTDT        WHENNE    *ZEROS
     C     LCVSDT        ANDGT     ##VTDT
     C                   MOVE      *OFF          MEETSS
     **** BEGIN CHG *** 05/14/99 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
** Column headings
Opt  Visit date      Staff member      Employer   Div Employer name
Opt      Staff member     Visit date   Employer   Div Employer name
Opt   Employer   Div Employer name              Visit date      Staff  member
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YYY
1044=Delete                 YYY
1055=Display                YYY
1077=Tasks                  YYN
11414=DARTS                 YYY
203F3=Exit                  YYY
205F5=Refresh               YYY
206F6=Add                   YYY
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
