     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W2101R - BILLING FILE SEARCH
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  05/22/00 C:90  PROJECT: WC577                                *
     H*                 ADDED DELETE - OPT4.                          *
     H*                                                               *
     H*  01/31/00 C:118 PROJECT: WC965                                *
     H*                 RE-WRITE OF BILLING REQUEST SEARCH.           *
     H*                                                               *
     H*  02/23/98 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                ADDED TABLES FD & TD FOR POLICY EFF. DATES.    *
     H*                                                               *
     H*  04/06/94 C:24 PROJECT WC407                                  *
     H*                SHOW BILLING FLAGS ON THE SEARCH.              *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     F*****************************************************************
     FW2101F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWABLP     IF   E             DISK
     F                                     RENAME(PBL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWABLL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBL01:DBRCDF)
     F*
     F*  Billing request file (subset on business renewal flag)
     FWABLL9    IF   E           K DISK
     F                                     INFDS(DBFDB1)
     F                                     RENAME(PBL01:DBRCDF1)
     F*
     F*  Billing request file (subset on FYR and RFLG)
     FWABLL8    IF   E           K DISK
     F                                     INFDS(DBFDB2)
     F                                     RENAME(PBL01:DBRCDF2)
     F*
     F*  Employer detail file for policy years
     FWDELL1    IF   E           K DISK
     F*
     F*  Fund master file for fund name
     FWMFNP     IF   E             DISK
     F*
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
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
     D OPTNM           C                   CONST('Billing Request Sear-
     D                                     ch')
      **** BEGIN ADD **** 05/22/00 ************************************
     D DELETD          C                   CONST('*Deleted')
      **** END   ADD **** 05/22/00 ************************************
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
      **** BEGIN CHG **** 05/22/00 ************************************
     D************* 'W2104R02'            C         ACDPGM
     D ACDPGM          C                   CONST('W2101R02')
      **** END   CHG **** 05/22/00 ************************************
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
     D*
     D                 DS
     D  XXDATA                 1     70
     D  XXFYR                  2      4
     D  XXEMP#                 7     15
     D  XXDIV                 18     22
     D  XXFLG1                27     27
     D  XXFLG3                35     35
     D  XXFLG2                41     41
     D  XXRFLG                48     48
     D  XXFRDT                53     60
     D  XXTODT                63     70
     D*
     D DBFDB1          DS
     D  DBRRB1               397    400B 0
     D DBFDB2          DS
     D  DBRRB2               397    400B 0

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          FND->IN
     C                   PARM                    @@RRN2            9 0          SEL->OUT
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
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1B         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRFLG
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY1C         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKRFLG
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Subset on policy year
     C*
     C     BLKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C*
     C*  Subset on business renewal flag
     C*
     C     BLKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKRFLG
     C*
     C*  Subset on policy year and business renewal flag
     C*
     C     BLKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKRFLG
     C*
     C*  Employer detail key list for policy eff and exp dates
     C*
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C*  Fund master key list for fund name
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    BLFYR         KKFYR
     C     *LIKE         DEFINE    BLEMP#        KKEMP#
     C     *LIKE         DEFINE    BLDIV         KKDIV
     C     *LIKE         DEFINE    BLFLG1        KKFLG1
     C     *LIKE         DEFINE    BLFLG3        KKFLG3
     C     *LIKE         DEFINE    BLFLG2        KKFLG2
     C     *LIKE         DEFINE    BLRFLG        KKRFLG
     C     *LIKE         DEFINE    ELPFDT        KKFRDT
     C     *LIKE         DEFINE    ELPTDT        KKTODT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSFYR         IFNE      *ZEROS
     C     SSRFLG        ORNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN11
     C*
     C                   ENDIF                                                  SSFYR/RFLG
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
     C     SSRFLG        IFNE      *BLANKS
     C     SSRFLG        ANDNE     'R'
     C     SSRFLG        ANDNE     'N'
     C                   SETON                                        79  90
     C*
     C                   ENDIF                                                  SSRFLG<>BLKS
     C*
     C*  Move subset values to key fields
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C                   Z-ADD     SSFYR         KKFYR
     C*
     C     SSRFLG        WHENNE    *BLANKS
     C                   MOVE      SSRFLG        KKRFLG
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN90=OFF
     C*
     C                   ENDSR                                                  ERRCK2
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   Z-ADD     *ZEROS        SSFYR
     C                   MOVEL     *BLANKS       SSRFLG
     C                   ENDSR                                                  INZSST
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*  Load Company and Fund
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNCO#         SCCO#
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCFNNM
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDSR                                                  KEYINZ
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        KKFYR
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCCO#         KKCO#
     C                   MOVEL     SCFND         KKFND
     C                   MOVEL     SCFYR         KKFYR
     C                   MOVEL     SCEMP#        KKEMP#
     C                   MOVEL     SCDIV         KKDIV
     C                   MOVEL     SCFLG1        KKFLG1
     C                   MOVEL     SCFLG3        KKFLG3
     C                   MOVEL     SCFLG2        KKFLG2
     C                   MOVEL     SCRFLG        KKRFLG
     C                   MOVEL     SCFRDT        KKFRDT
     C                   MOVEL     SCTODT        KKTODT
     C*
     C                   ENDSR                                                  KEYSFL
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     *BLANKS       XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  BLFYR         XXNUM
     C                   MOVEL     BLFYR         KKFYR
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFYR
     C*
     C                   MOVEL(P)  BLEMP#        XXNUM
     C                   MOVEL     BLEMP#        KKEMP#
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXEMP#
     C*
     C                   MOVEL(P)  BLDIV         XXNUM
     C                   MOVEL     BLDIV         KKDIV
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDIV
     C*
     C                   MOVEL     BLFLG1        XXFLG1
     C                   MOVEL     BLFLG3        XXFLG3
     C                   MOVEL     BLFLG2        XXFLG2
     C                   MOVEL     BLRFLG        XXRFLG
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZFRDT            6 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ZZTODT            6 0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  Format 6 digit date for output
     C*
     C                   MOVEL(P)  ZZFRDT        XXNUM
     C                   MOVEL     'DATE6'       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXFRDT
     C*
     C                   MOVEL(P)  ZZTODT        XXNUM
     C                   MOVEL     'DATE6'       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXTODT
     C*
     C                   MOVE      XXDATA        SCDATA
     C*
     C*  Move record values into hidden subfile screen fields
     C*
     C                   Z-ADD     BLCO#         SCCO#
     C                   Z-ADD     BLFND         SCFND
     C                   Z-ADD     BLFYR         SCFYR
     C                   Z-ADD     BLEMP#        SCEMP#
     C                   Z-ADD     BLDIV         SCDIV
     C                   MOVE      BLFLG1        SCFLG1
     C                   MOVE      BLFLG3        SCFLG3
     C                   MOVE      BLFLG2        SCFLG2
     C                   MOVE      BLRFLG        SCRFLG
     C                   Z-ADD     ELPFDT        SCFRDT
     C                   Z-ADD     ELPTDT        SCTODT
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
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C   21BLKY          READE     DBRCDF                                 95
     C*
     C     SSRFLG        WHENNE    *BLANKS
     C   21BLKY1         READE     DBRCDF1                                95
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSRFLG        ANDNE     *BLANKS
     C   21BLKY2         READE     DBRCDF2                                95
     C*
     C                   OTHER
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  READ
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C   21BLKY          READPE    DBRCDF                                 95
     C*
     C     SSRFLG        WHENNE    *BLANKS
     C   21BLKY1         READPE    DBRCDF1                                95
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSRFLG        ANDNE     *BLANKS
     C     BLKY2         READPE    DBRCDF2                                95
     C*
     C                   OTHER
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  READP
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C   21KEY1A         SETGT     DBRCDF                             96
     C*
     C     SSRFLG        WHENNE    *BLANKS
     C   21KEY1B         SETGT     DBRCDF1                            96
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSRFLG        ANDNE     *BLANKS
     C   21KEY1C         SETGT     DBRCDF2                            96
     C*
     C                   OTHER
     C   21KEY1A         SETGT     DBRCDF                             96
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETGT
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C*
     C     SSFYR         WHENNE    *ZEROS
     C   21KEY1A         SETLL     DBRCDF                             96
     C*
     C     SSRFLG        WHENNE    *BLANKS
     C   21KEY1B         SETLL     DBRCDF1                            96
     C*
     C     SSFYR         WHENNE    *ZEROS
     C     SSRFLG        ANDNE     *BLANKS
     C     KEY1C         SETLL     DBRCDF2                            96
     C*
     C                   OTHER
     C   21KEY1A         SETLL     DBRCDF                             96
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETLL
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
      **** BEGIN ADD **** 05/22/00 ************************************
     C                   MOVEL(P)  DELETD        SCDATA
      **** END   ADD **** 05/22/00 ************************************
     C*
     C                   ENDSR                                                  SF1UPD
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN2
     C*
     C                   ENDSR                                                  SLCTRD
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C                   SELECT
     C     SSFYR         WHENNE    *ZEROS
     C     SSFYR         ANDNE     BLFYR
     C                   MOVE      *OFF          MEETSS
     C*
     C     SSRFLG        WHENNE    *BLANKS
     C     SSRFLG        ANDNE     BLRFLG
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  SUBSET
     C*================================================================
** Column headings
Opt  Year   Employer    Div  Billing  A/R  Audit  Renew  Eff Date  Exp Date
** Description--------------MISN
1011=Select                 YYY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
212F12=Cancel               YNY
217F17=Subset               YYY
3
