     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: 168        PROJECT: 114WKX *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  12/13/05 C:168  NEW BACKUP TABLE FOR THE NATURE OF BUSINESS  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0031F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDPHP     IF   E             DISK
     F                                     RENAME(PPH01:PFRCDF)
     F*
     F*  Logical by PHFEIN/PHMOD/PHDT/PHTIME
     FWDPHL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPH01:DBRCDF1)
     F*
     F*  Logical by PHDT/PHTIME/PHFEIN/PHMOD
     FWDPHL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPH01:DBRCDF2)
     F*
     F*  Logical by PHUSER/PHDT/PHTIME/PHFEIN/PHMOD
     FWDPHL3    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PPH01:DBRCDF3)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
      *
     D                 DS
     D  FEINMD                 1     14
      *
     D OPTNM           C                   CONST('Payee Field History')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0031R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('WA003R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@KEY
     C                   PARM                    @@VIEW            1 0
     C                   PARM                    @@FEIN            9 0
     C                   PARM                    @@MOD             3 0
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
      *
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C                   KFLD                    KKDESC
      *
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKDESC
      *
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKUSER
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKDESC
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PHFEIN        KKFEIN
     C     *LIKE         DEFINE    PHMOD         KKMOD
     C     *LIKE         DEFINE    PHDT          KKDT
     C     *LIKE         DEFINE    PHTIME        KKTIME
     C     *LIKE         DEFINE    PHUSER        KKUSER
     C     *LIKE         DEFINE    PHDESC        @@KEY
     C     *LIKE         DEFINE    PHDESC        U1PDSC
     C     *LIKE         DEFINE    PHDESC        U2PDSC
     C     *LIKE         DEFINE    PHDESC        KKDESC
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM                    SRFFMT            8
     C                   PARM                    SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
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
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
      *
     C                   Z-ADD     *ZEROS        KKFEIN
     C                   Z-ADD     *ZEROS        KKMOD
     C                   MOVE      *HIVAL        KKDT
     C                   MOVE      *HIVAL        KKTIME
     C                   MOVEL(P)  *LOVAL        KKUSER
     C                   MOVEL(P)  *BLANKS       KKDESC
      *
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@FEIN        KKFEIN
     C                   ENDIF
     C     Q1PRMS        IFGE      7
     C                   MOVE      @@MOD         KKMOD
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKFEIN
     C   23              MOVEL(P)  SCPST3        KKUSER
      *
     C     *IN(22)       IFEQ      *ON
     C                   MOVE      SCPST2        @@DT              6 0
     C                   Z-ADD     @@DT          SRDTI
     C                   MOVEL     *BLANKS       SRDTO
     C                   MOVEL(P)  '*MDY'        SRFFMT
     C                   MOVEL(P)  '*CYMD'       SRTFMT
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         KKDT
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVE      SCFEIN        KKFEIN
     C                   MOVE      SCMOD         KKMOD
     C                   MOVE      SCDT          KKDT
     C                   MOVE      SCTIME        KKTIME
     C                   MOVEL     SCUSER        KKUSER
     C                   MOVEL     SCDESC        KKDESC
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C                   Z-ADD     PHFEIN        SCFEIN
     C                   Z-ADD     PHMOD         SCMOD
     C                   MOVE      PHDT          SCDT
     C                   MOVE      PHTIME        SCTIME
     C                   MOVEL(P)  PHUSER        SCUSER
     C                   MOVEL(P)  PHDESC        SCDESC
      *
     C                   MOVEL(P)  PHFVAL        SCFVAL
     C                   MOVEL(P)  PHTVAL        SCTVAL
      *
      * CONSTRUCT THE FEIN/MOD FIELD
     C                   MOVE      *BLANKS       FEINMD
     C                   MOVEL     PHFEIN        FEINMD
     C                   MOVE      PHMOD         FEINMD
      *
     C                   MOVEL     PHDESC        ##DESC           25
     C                   Z-ADD     PHDT          SRDTI
     C                   MOVEL(P)  '*CYMD'       SRFFMT
     C                   MOVEL(P)  *BLANKS       SRTFMT
     C                   EXSR      CVTDT
      *
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   CALL      CHGNUM
     C                   PARM                    XXNUM            80
     C                   PARM      'DATE'        EDTMSK           10
      *
     C                   MOVEL(P)  XXNUM         @@XNUM           10
      *
     C     *IN(21)       IFEQ      *ON
     C                   MOVEL(P)  FEINMD        SCDAT1
     C                   MOVE      @@XNUM        SCDAT1
     C                   MOVEL(P)  PHUSER        SCDAT2
     C                   MOVE      ##DESC        SCDAT2
     C                   ENDIF
     C     *IN(22)       IFEQ      *ON
     C                   MOVEL(P)  @@XNUM        SCDAT1
     C                   MOVE      FEINMD        SCDAT1
     C                   MOVEL(P)  PHUSER        SCDAT2
     C                   MOVE      ##DESC        SCDAT2
     C                   ENDIF
     C     *IN(23)       IFEQ      *ON
     C                   MOVEL(P)  PHUSER        SCDAT1
     C                   MOVE      @@XNUM        SCDAT1
     C                   MOVEL(P)  FEINMD        SCDAT2
     C                   MOVE      ##DESC        SCDAT2
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C*
     C*** BEGIN ADD *** 02/25/98 **************************************
     C                   MOVE      *ON           *IN16                          POS. TO SCPST2
     C*** END   ADD *** 02/25/98 **************************************
     C*
     C                   Z-ADD     10            SF1PAG            5 0
     C                   SETON                                        12
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     Q1PRMS        IFEQ      5
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>4
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF1                                95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
      *
     C     *IN(96)       IFEQ      *OFF
     C     *IN(95)       ANDEQ     *ON
     C   21              READPE    DBRCDF1                                95
     C   22              READPE    DBRCDF2                                95
     C   23              READPE    DBRCDF3                                95
     C                   ENDIF
      *
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF1                            96
     C   22KEY3          SETGT     DBRCDF2                            96
     C   23KEY4          SETGT     DBRCDF3                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF1                            96
     C   22KEY3          SETLL     DBRCDF2                            96
     C   23KEY4          SETLL     DBRCDF3                            96
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
     C*
     C                   MOVEL     SCFEIN        @@KEY
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
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
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  PHDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
      *
      * USER PASSED THE FEIN/MOD ONLY LOOK FOR THESE
     C     Q1PRMS        IFEQ      7
      *
     C     PHFEIN        IFNE      @@FEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     PHMOD         IFNE      @@MOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR
** HEADERS - HD
FEIN #     Mod    Date         User             Field Name
Date          FEIN #     Mod   User             Field Name
User              Date         FEIN #     Mod   Field Name
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NNY
215F15=FOLD/TRUNCATE        YYY
217F17=Subset               YYY
3
