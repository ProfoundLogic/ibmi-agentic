     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F6601R - WORK WITH NCCI DATA CALL SUBMISSIONS
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: MENU                                            *
     H*                                                               *
     H*  CALLS TO: F6602C  F6603C F6604C  F6601R01                    *
     H*            SA020R(CHGNUM) - FORMAT SUBFILE FIELDS             *
     H*                                                               *
     H*  CREATED: 04/19/16  PROGRAMMER: C:308 PROJECT: C114MWK        *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*****************************************************************
     H*
     FF6601F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMNCP     IF   E             DISK
     F                                     RENAME(PNC01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMNCL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PNC01:DBRCDF)
     F*  SECONDARY logical file definition (for displaying items)
     FFMNCL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PNC01:DBRCDFB)
     F*
     FFXWNL1    UF A E           K DISK    USROPN
     FFXWNL     IF   E           K DISK
     F                                     RENAME(PWN01:PWN02)
     FSTSTL3    IF   E           K DISK
      /COPY SCOPYWW01D
     D HDA             S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('NCCI Data Call Submi-
     D                                     ssions')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F6601R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('S0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D DELETD          C                   CONST('*Deleted')
     D*
     D X1FND           S              3  0
     D X4RPMY          S              6
     D*  Data structures for SCDATA
     D                 DS
     D  XXDATA                 1     78
     D  XXDAT                  2     18
     D  XXRPMY                26     32
     D  XXRPTL                41     41
     D  XXSTAT                60     60
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
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
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     stky          klist
     C                   kfld                    keyst            10
     C
     C     KEY1          KLIST
     C                   KFLD                    KKCO#             3 0
     C                   KFLD                    KKFND
     C                   KFLD                    KKSBDT
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSBDT
     C                   KFLD                    KKFND
     C*
     C*  NCCI Medical Extract data file
     C     NCKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  EXTRACTION WORK FILE
     C     WNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    X1FND
     C                   KFLD                    X2RPMY
     C*
     C     WNKY1         KLIST
     C                   KFLD                    KKCO#
     C*
     C     WNKY2         KLIST
     C                   KFLD                    WNCO#
     C                   KFLD                    XXUSER
     C                   KFLD                    XXTIME
     C                   KFLD                    WNFND
     C                   KFLD                    WNRPYMTHF
     C*
     C     WNKY3         KLIST
     C                   KFLD                    WNCO#
     C                   KFLD                    XXUSER
     C                   KFLD                    XXTIME
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    NCCO#         KKCO#
     C     *LIKE         DEFINE    NCFND         KKFND
     C     *LIKE         DEFINE    NCSBDT        KKSBDT
     C     *LIKE         DEFINE    WNUSER        XXUSER
     C     *LIKE         DEFINE    WNTIME        XXTIME
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Check if extraction is in process
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     NCEXTDTA      IFEQ      'Y'
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C                   Z-ADD     SCFND         KKFND
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
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      *LOVAL        KKFND
     C   21              MOVE      *HIVAL        KKSBDT
     C   22              MOVE      *LOVAL        KKSBDT
     C   21              MOVEL     HDA(1)        SCHDLA
     C   22              MOVEL     HDA(2)        SCHDLA
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVE      SCPST1        KKFND
     C   21              Z-ADD     SCPS1A        SRDTI
     C   21              CALL      'SRCVTMY'     DTCYMD
     C   21              MOVEL     SRDTO         KKSBDT
     C*
     C   22              MOVE      SCPST2        SRDTI
     C   22              CALL      'SRCVTMY'     DTCYMD
     C   22              MOVEL     SRDTO         KKSBDT
     C   22              Z-ADD     SCPS2A        KKFND
     C*
     C     KKSBDT        IFEQ      0
     C                   MOVE      *HIVAL        KKSBDT
     C                   ENDIF
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCFND         KKFND
     C                   MOVE      SCRPMY        KKSBDT
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C*  KGDO - Process F7; Send Replacement
     C*================================================================
     C     KGDO          BEGSR
     C*
     C* READ SUBMIT SELECTION FILE TO CHECK FOR ERRORS.
     C*
     C     WNKY1         CHAIN     PWN02                              60
     C*
     C* DO NOT PROCESS SUBMIT IF NO RECORDS HAVE BEEN SELECTED.
     C*
     C     *IN60         IFEQ      *ON
     C     *INKL         DOUEQ     *ON
     C                   EXFMT     WDWERR
     C                   ENDDO
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           RSTPGM
     C                   ELSE
     C                   Dow       *in90 = *Off
     C                   Move      *Blanks       SCST
     C                   EXFMT     WDW02
     C                   IF        SCST <> *Blanks
     C                   Movel     SCST          keyst
     C     stKy          chain     PST01                              88
     C  N88              move      *On           *in90
     C                   Else
     C                   move      *On           *in90
     C                   EndIf
     C                   Enddo
     C                   move      *off          *in90
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   MOVE      'R'           ##MODE
     C                   IF        SCST <> *Blanks
     C                   MOVE      SCST          STATENC           2
     C                   Else
     C                   MOVE      'N'           STATENC           2
     C                   EndIf
     C*
     C                   CALL      'F6603C'
     C                   PARM                    ##MODE
     C                   PARM                    STATENC
     C                   EndIf
     C*
     C* CLEAR WORKFILE OF RECORDS.
     C*
     C     WNKY1         CHAIN     PWN01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   DELETE    PWN01
     C*
     C     WNKY1         READE     PWN01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  KHDO - Process F8; Delete File
     C*================================================================
     C     KHDO          BEGSR
     C*
     C* READ SUBMIT SELECTION FILE TO CHECK FOR ERRORS.
     C*
     C     WNKY1         CHAIN     PWN02                              60
     C*
     C* DO NOT PROCESS SUBMIT IF NO RECORDS HAVE BEEN SELECTED.
     C*
     C     *IN60         IFEQ      *ON
     C     *INKL         DOUEQ     *ON
     C                   EXFMT     WDWERR
     C                   ENDDO
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           RSTPGM
     C                   ELSE
     C                   Dow       *in90 = *Off
     C                   Move      *Blanks       SCST
     C                   EXFMT     WDW02
     C                   IF        SCST <> *Blanks
     C                   Movel     SCST          keyst
     C     stKy          chain     PST01                              88
     C  N88              move      *On           *in90
     C                   Else
     C                   move      *On           *in90
     C                   EndIf
     C                   Enddo
     C                   move      *off          *in90
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   MOVE      'D'           ##MODE
     C                   IF        SCST <> *Blanks
     C                   MOVE      SCST          STATENC           2
     C                   Else
     C                   MOVE      'N'           STATENC           2
     C                   EndIf
     C*
     C                   CALL      'F6603C'
     C                   PARM                    ##MODE
     C                   PARM                    STATENC
     C                   EndIf
     C*
     C* CLEAR WORKFILE OF RECORDS.
     C*
     C     WNKY1         CHAIN     PWN01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   DELETE    PWN01
     C*
     C     WNKY1         READE     PWN01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C*  KBDO - Process F2; Original Submission
     C*================================================================
     C     KBDO          BEGSR
     C*
     C* READ SUBMIT SELECTION FILE TO CHECK FOR ERRORS.
     C*
     C     WNKY1         CHAIN     PWN01                              60
     C*
     C* DO NOT PROCESS SUBMIT IF NO RECORDS HAVE BEEN SELECTED.
     C*
     C     *IN60         IFEQ      *ON
     C     *INKL         DOUEQ     *ON
     C                   EXFMT     WDWERR
     C                   ENDDO
     C                   MOVE      *OFF          *INKL
     C                   MOVE      *ON           RSTPGM
     C                   ELSE
     C                   Dow       *in90 = *Off
     C                   Move      *Blanks       SCST
     C                   EXFMT     WDW02
     C                   IF        SCST <> *Blanks
     C                   Movel     SCST          keyst
     C     stKy          chain     PST01                              88
     C  N88              move      *On           *in90
     C                   Else
     C                   move      *On           *in90
     C                   EndIf
     C                   Enddo
     C                   move      *off          *in90
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C                   MOVE      'O'           ##MODE
     C                   IF        SCST <> *Blanks
     C                   MOVE      SCST          STATENC           2
     C                   Else
     C                   MOVE      'N'           STATENC           2
     C                   EndIf
     C*
     C                   CALL      'F6603C'
     C                   PARM                    ##MODE
     C                   PARM                    STATENC
     C                   EndIf
     C*
     C* CLEAR WORKFILE OF RECORDS.
     C*
     C     WNKY1         CHAIN     PWN01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   DELETE    PWN01
     C*
     C     WNKY1         READE     PWN01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      KEDO
     C*
     C                   ENDIF
     C                   ENDSR                                                  KBDO SUBR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      NCFND         SCFND
     C                   MOVE      NCSBDT        SCRPMY
     C                   MOVE      NCRPYMTHF     SCRPMY1
     C                   MOVE      NCRPYMTHT     SCRPMY2
     C                   MOVE      NCRPMTH       SCRPMY3
     C                   MOVE      NCRPLVL       SCRPMY4
     C*
     C*  Edit stuff for output
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVEL(P)  NCFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFND             3
     C*
     C                   MOVEL     'DATE'        EDTMSK
     C                   Z-ADD     NCSBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXSBDT           10
     C*
      /Free
         Evalr  X4RPMY = '      ' + %Char(NCRPMTH);
         XXRPMY = %Subst(X4RPMY:1:2) + '/' +  %Subst(X4RPMY:3:4);
      /End-Free
     C                   MOVEL     NCRPLVL       XXRPTL
     C*
     C                   MOVE      NCFND         X1FND             3 0
     C                   MOVE      NCRPYMTHF     X1RPMY            5 0
     C                   MOVE      *BLANK        XXSTAT
     C     WNKY          CHAIN(N)  PWN01                              60
     C  N60              MOVE      'Y'           XXSTAT
     C                   CLEAR                   XXDAT
     C   21              MOVEL     XXFND         XXDAT
     C   21              MOVE      XXSBDT        XXDAT
     C   22              MOVEL     XXSBDT        XXDAT
     C   22              MOVE      XXFND         XXDAT
     C*
     C                   MOVEL(P)  XXDATA        SCDATA
     C   21              MOVEL     HDA(1)        SCHDLA
     C   22              MOVEL     HDA(2)        SCHDLA
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* OPT25 - Process option 25 (submit)
     C*================================================================
     C     OPT25         BEGSR
     C*
     C                   MOVE      SCFND         X1FND
     C                   MOVE      SCRPMY        X1RPMY
     C                   Z-ADD     SCRPMY1       X2RPMY            5 0
     C                   Z-ADD     SCRPMY2       X3RPMY            5 0
     C*
     C     WNKY          CHAIN     PWN01                              60
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PWN01
     C                   ELSE
     C                   Z-ADD     KKCO#         WNCO#
     C                   Z-ADD     X1FND         WNFND
     C                   Z-ADD     SCRPMY1       WNRPYMTHF
     C                   Z-ADD     SCRPMY2       WNRPYMTHT
     C                   Z-ADD     SCRPMY3       NCRPMTH
     C                   MOVE      SCRPMY4       NCRPLVL
     C                   Z-ADD     SCRPMY        WNSBDT
     C                   MOVE      'P'           WNPRST
     C                   MOVE      'Y'           WNEXTDTA
     C                   MOVEL     Q1PGM         WNPGM
     C                   MOVEL     Q1USER        WNUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        WNTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WNDT
     C                   WRITE     PWN01
     C                   ENDIF                                                  *IN60
     C*
     C                   ENDSR                                                  OPT25
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKB         CASEQ     *ON           KBDO
     C     *INKG         CASEQ     *ON           KGDO
     C     *INKH         CASEQ     *ON           KHDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '25'          OPT25
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   CLOSE     FXWNL1
     C*
     C                   MOVE      'W'           QQFLAG
     C                   EXSR      GRPF
     C                   ENDSR                                                  PGMEND
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      *on           *in21
     C   21              MOVEL     HDA(1)        SCHDLA
     C                   MOVE      *off          *in21
     C                   MOVE      'W'           QQFLAG
     C                   EXSR      GRPF
     C*
     C                   OPEN      FXWNL1
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21NCKY          READE     DBRCDF                                 95
     C   22NCKY          READE     DBRCDFB                                95
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21NCKY          READPE    DBRCDF                                 95
     C   22NCKY          READPE    DBRCDFB                                95
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDFB                            96
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDFB                            96
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
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
     C*================================================================
      * CALL CL TO CREATE TEMPORARY VERSIONS OF FILES.
     C*================================================================
     C     GRPF          BEGSR
      *
     C                   CALL      'F6604C'
     C                   PARM                    QQFLAG            1
      *
     C                   ENDSR                                                  GRPF ENDSR
     C*================================================================
** Column headings 1st line
              Submitted     Valuation    Reporting            Submit
       Submitted            Valuation    Reporting            Submit
** Column headings 2nd line
Opt  Group      Date            Month     Levels
Opt      Date      Group        Month     Levels
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12525=Submit Data           YNN
202F2=Original Submission   YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F4=Extract data          YNN
207F7=Send Replacement      YNN
208F8=Delete File           YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               NNN
3
