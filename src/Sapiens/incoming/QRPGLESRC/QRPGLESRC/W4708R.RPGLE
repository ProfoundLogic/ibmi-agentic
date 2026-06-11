     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4708R - AUDIT STATUS DETAIL LISTING
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM: W4700R21                                        *
     H*                                                               *
     H*  CALLS TO:    NONE                                            *
     H*                                                               *
     H*  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  XX/XX/XX  X:XX  PROJECT: XXXXX                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*
     FW4708F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMAVP     IF   E             DISK
     F                                     RENAME(PAV01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMAVL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAV01:DBRCDF)
     F*
     F*  Logical for Audit Tracking Detail file
     FWMA2L     IF   E           K DISK
     F*
     F*  Employer master file
     FWMEML     IF   E           K DISK
     F*
     F*  Fund master file
     FWMFNL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D HDA             S             80    DIM(7) CTDATA PERRCD(1)              Column headings
     D HDB             S             80    DIM(7) CTDATA PERRCD(1)              Column headings
     D TTL             S             40    DIM(7) CTDATA PERRCD(1)              TITLE
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Audit Status Detail-
     D                                      Listing')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W4708R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(7)
     D SF1PAG          C                   CONST(13)
     D*
     D*  Data structures for view changes
     D                 DS
     D  XXOP1                  1     78
     D  O1EMP#                 1      9
     D  O1DIV                 11     15
     D  O1NAME                17     47
     D  O1ACO                 51     53
     D  O1RQST                56     63
     D  O1RCVD                66     73
     D*
     D                 DS
     D  XXOP2                  1     78
     D  O2EMP#                 1      9
     D  O2DIV                 11     15
     D  O2NAME                17     38
     D  O2ACO                 40     42
     D  O2RQST                44     51
     D  O2RCVD                53     60
     D  O2PRDT                62     69
     D  O2MAIL                71     78
     D*
     D                 DS
     D  XXOP3                  1     78
     D  O3EMP#                 1      9
     D  O3DIV                 11     15
     D  O3NAME                17     47
     D  O3ACO                 51     53
     D  O3RQST                57     64
     D*
     D                 DS
     D  XXOP4                  1     78
     D  O4EMP#                 1      9
     D  O4DIV                 11     15
     D  O4NAME                17     47
     D  O4ACO                 51     53
     D  O4RQST                56     63
     D  O4RCVD                66     73
     D*
     D                 DS
     D  XXOP5                  1     78
     D  O5EMP#                 1      9
     D  O5DIV                 11     15
     D  O5NAME                17     47
     D  O5ACO                 51     53
     D  O5RQST                56     63
     D  O5ATTM                66     72
     D*
     D                 DS
     D  XXOP6                  1     78
     D  O6EMP#                 1      9
     D  O6DIV                 11     15
     D  O6NAME                17     40
     D  O6ACO                 42     45
     D  O6RQST                48     55
     D  O6RCVD                58     65
     D  O6PRDT                68     75
     D*
     D                 DS
     D  XXOP7                  1     78
     D  O7EMP#                 1      9
     D  O7DIV                 11     15
     D  O7NAME                17     40
     D  O7ACO                 42     44
     D  O7RQST                46     53
     D  O7RCVD                55     62
     D  O7CNTD                64     71
     D  O7STAF                74     76
     D  O7RSGN                78     78

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@OPT             1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKOPT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKOPT
     C*
     C*  Employer master key
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Fund Master key list
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  Audit Status Detail Listing key list
     C     A2KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    AVCO#         KKCO#
     C     *LIKE         DEFINE    AVFND         KKFND
     C     *LIKE         DEFINE    AVOPT         KKOPT
     C     *LIKE         DEFINE    AVEMP#        KKEMP#
     C     *LIKE         DEFINE    AVDIV         KKDIV
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
     C*
     C                   MOVEL(P)  OPTNM         SCPRDS
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@OPT         KKOPT
     C                   MOVE      *LOVAL        KKEMP#
     C                   MOVE      *LOVAL        KKDIV
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCEMP#        KKEMP#
     C                   MOVE      SCDIV         KKDIV
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      AVEMP#        KKEMP#
     C                   MOVE      AVDIV         KKDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C*  Format 6 digit date fields
     C*
     C                   MOVEL     'DATE6'       EDTMSK
     C*
     C*                Audit request date
     C                   Z-ADD     AVRQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZRQST            6 0
     C*
     C                   MOVEL     ZZRQST        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRQST            8
     C*
     C*                Audit received date
     C                   Z-ADD     AVRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZRCVD            6 0
     C*
     C                   MOVEL     ZZRCVD        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRCVD            8
     C*
     C*                Audit print date
     C                   Z-ADD     AVPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZPRDT            6 0
     C*
     C                   MOVEL     ZZPRDT        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXPRDT            8
     C*
     C*                Audit mail date
     C                   Z-ADD     AVMAIL        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZMAIL            6 0
     C*
     C                   MOVEL     ZZMAIL        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXMAIL            8
     C*
     C*                Contestec date
     C                   Z-ADD     AVCNTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZCNTD            6 0
     C*
     C                   MOVEL     ZZCNTD        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCNTD            8
     C*
     C*  Move record values into hidden subfile screen fields
     C                   MOVE      AVEMP#        SCEMP#
     C                   MOVE      AVDIV         SCDIV
     C                   MOVE      AVOPT         SCOPT
     C*
     C*  Load data structures for screen output
     C                   MOVE      *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  AVEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         O1EMP#
     C*
     C                   MOVEL(P)  AVDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         O1DIV
     C*
     C     @@OPT         CASEQ     '1'           SEL1
     C     @@OPT         CASEQ     '2'           SEL2
     C     @@OPT         CASEQ     '3'           SEL3
     C     @@OPT         CASEQ     '4'           SEL4
     C     @@OPT         CASEQ     '5'           SEL5
     C     @@OPT         CASEQ     '6'           SEL6
     C     @@OPT         CASEQ     '7'           SEL7
     C                   ENDCS
     C*
     C                   ENDSR                                                  MVDBSC
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVE      @@FND         SCFND
     C                   MOVEL     FNNAME        SCFNNM
     C*
     C     A2KY          CHAIN     PA201                              60
     C*
     C                   Z-ADD     A2RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C                   Z-ADD     A2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   Z-ADD     A2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C*
     C*  Set title and column headings according to option
     C                   MOVE      @@OPT         Z                 1 0
     C                   MOVEL     HDA(Z)        SCHDLA
     C                   MOVEL     HDB(Z)        SCHDLB
     C                   MOVEL(P)  TTL(Z)        SCTITL
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY2          READPE    DBRCDF                                 95
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
     C* SEL1 - Process option 1; Audits received
     C*================================================================
     C     SEL1          BEGSR
     C*
     C*
     C                   MOVEL     EMNAME        O1NAME
     C                   MOVE      AVACO         O1ACO
     C                   MOVE      XXRQST        O1RQST
     C                   MOVE      XXRCVD        O1RCVD
     C*
     C                   MOVEL(P)  XXOP1         SCDATA
     C*
     C                   ENDSR                                                  SEL1
     C*================================================================
     C* SEL2 - Process option 2; Completed audits
     C*================================================================
     C     SEL2          BEGSR
     C*
     C                   MOVEL     EMNAME        O2NAME
     C                   MOVE      O1EMP#        O2EMP#
     C                   MOVE      O1DIV         O2DIV
     C                   MOVE      AVACO         O2ACO
     C                   MOVE      XXRQST        O2RQST
     C                   MOVE      XXRCVD        O2RCVD
     C                   MOVE      XXPRDT        O2PRDT
     C*
     C                   MOVEL(P)  XXOP2         SCDATA
     C*
     C                   ENDSR                                                  SEL2
     C*================================================================
     C* SEL3 - Process option 3; Audits outstanding
     C*================================================================
     C     SEL3          BEGSR
     C*
     C                   MOVEL     EMNAME        O3NAME
     C                   MOVE      O1EMP#        O3EMP#
     C                   MOVE      O1DIV         O3DIV
     C                   MOVE      AVACO         O3ACO
     C                   MOVE      XXRQST        O3RQST
     C*
     C                   MOVEL(P)  XXOP3         SCDATA
     C*
     C                   ENDSR                                                  SEL3
     C*================================================================
     C* SEL4 - Process option 4; Audits in house
     C*================================================================
     C     SEL4          BEGSR
     C*
     C                   MOVEL     EMNAME        O4NAME
     C                   MOVE      O1EMP#        O4EMP#
     C                   MOVE      O1DIV         O4DIV
     C                   MOVE      AVACO         O4ACO
     C                   MOVE      XXRQST        O4RQST
     C                   MOVE      XXRCVD        O4RCVD
     C*
     C                   MOVEL(P)  XXOP4         SCDATA
     C*
     C                   ENDSR                                                  SEL4
     C*================================================================
     C* SEL5 - Process option 5; Non-productive status report
     C*================================================================
     C     SEL5          BEGSR
     C*
     C                   MOVEL     EMNAME        O5NAME
     C                   MOVE      O1EMP#        O5EMP#
     C                   MOVE      O1DIV         O5DIV
     C                   MOVE      AVACO         O5ACO
     C                   MOVE      XXRQST        O5RQST
     C                   MOVE      AVATTM        O5ATTM
     C*
     C                   MOVEL(P)  XXOP5         SCDATA
     C*
     C                   ENDSR                                                  SEL5
     C*================================================================
     C* SEL6 - Process option 6; Re-audits
     C*================================================================
     C     SEL6          BEGSR
     C*
     C                   MOVEL     EMNAME        O6NAME
     C                   MOVE      O1EMP#        O6EMP#
     C                   MOVE      O1DIV         O6DIV
     C                   MOVE      AVACO         O6ACO
     C                   MOVE      XXRQST        O6RQST
     C                   MOVE      XXRCVD        O6RCVD
     C                   MOVE      XXPRDT        O6PRDT
     C*
     C                   MOVEL(P)  XXOP6         SCDATA
     C*
     C                   ENDSR                                                  SEL6
     C*================================================================
     C* SEL7 - Process option 7; Contested audits
     C*================================================================
     C     SEL7          BEGSR
     C*
     C                   MOVEL     EMNAME        O7NAME
     C                   MOVE      O1EMP#        O7EMP#
     C                   MOVE      O1DIV         O7DIV
     C                   MOVE      AVACO         O7ACO
     C                   MOVE      XXRQST        O7RQST
     C                   MOVE      XXRCVD        O7RCVD
     C                   MOVE      XXCNTD        O7CNTD
     C                   MOVE      AVSTAF        O7STAF
     C                   MOVE      AVRSGN        O7RSGN
     C*
     C                   MOVEL(P)  XXOP7         SCDATA
     C*
     C                   ENDSR                                                  SEL7
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
     C                   ENDSR
** Column headings
 Employer   Div Name                     Co.   Rqsted   Rcvd     Cntstd Staff sn
** Column heading A
                                                 Audit   Date      Date
                                      Audit  Date     Date    Audit     Audit
                                                 Audit   Date
                                                 Audit   Date      Date
                                                 Audit   Date    Num of
                                         Audit   Date      Date    Revised
                                       Audit   Date     Date     Date      Rea-
** Column heading B
 Employer   Div Name                              Co.   Rqsted     Rcvd
 Employer   Div Name                   Co.  Rqsted    Rcvd   Print dt  Mail dt
 Employer   Div Name                              Co.   Rqsted
 Employer   Div Name                              Co.   Rqsted     Rcvd
 Employer   Div Name                              Co.   Rqsted  Attempts
 Employer   Div Name                      Co.   Rqsted     Rcvd    Print dt
 Employer   Div Name                    Co.   Rqsted    Rcvd    Cntstd     sgnd
**   TTL
    Listing of all audits received
    Listing of all audits completed
   Listing of all audits outstanding
  Listing of all audits still inhouse
  Listing of all non-productive audits
       Listing of all re-audits
    Listing of all contested audits
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
