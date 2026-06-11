     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SR00301I - WORK WITH ALTERNATE EMPLOYERS
     H*****************************************************************
     H*  DESCRIPTION:  This program will display a list of all        *
     H*                alternate employers for a selected employer    *
     H*                policy year.  Each alternate employer can be   *
     H*                accessed for update, display or selection      *
     H*                depending on the calling program.              *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    SR00301M - ALTERNATE EMPLOYER INFORMATION (OPT 2/5)        *
     H*                                                               *
     H*  CREATED: 04/29/99  PROGRAMMER: C:113  PROJECT: CM043         *
     H*                                                               *
     H*****************************************************************
     H*  NOTE:  @@RRN is used for returning the RRN of WDAEP for      *
     H*         selection.                                            *
     H*         @@RRN2 will need to be passed in as the RRN of WDELP. *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 02/08/11  C:190 Seq#: 79215/79209                             *
     H*                 Corrected paging issue.                       *
     H*                                                               *
     H* ------------------------------------------------------------- *
     H*  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.           *
     H* ------------------------------------------------------------- *
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
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FSR00301F  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDAEP     IF   E             DISK
     F                                     RENAME(PAE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDAEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAE01:DBRCDF)
     F*
     F*  Primary logical file definition (for displaying items, view 2)
     FWDAEL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAE01:DBRCDF2)
     FWDELP     IF   E             DISK
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*================================================================
     D*  Array & Table definitions
     D*================================================================
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*================================================================
     D*  Data Structure and Constant definitions
     D*================================================================
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Alternate Employer S-
     D                                     chedule')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('SR00301M')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D AGTAPI          C                   CONST('WA002R')
     D CHGNUM          C                   CONST('WA003R')
     D DELETD          C                   CONST('*Deleted')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(8)
     D*
     D*  Data structure for view 1
     D*
     D                 DS
     D  X1DATA                 1     52
     D  X1SEQ#                 1      5
     D  X1NAM1                12     52
     D*
     D*  Data structure for view 2
     D*
     D                 DS
     D  X2DATA                 1     52
     D  X2NAM1                 1     40
     D  X2SEQ#                43     47
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C*  Entry parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@VIEW            1 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  Change Numeric to Alpha program parameter list
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Reposition key
     C*
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*** BEG ADD ***** 02/08/12 ********************************
     C     KEY2          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    K#SEQ#            5 0
      *
     C     KEY3          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    K#NAM1           40
     C                   KFLD                    K#SEQ#            5 0
     C*** END ADD ***** 02/08/12 ********************************
     C*
     C*  Employer Master file
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  Group Master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    AECO#         KKCO#
     C     *LIKE         DEFINE    AEFND         KKFND
     C     *LIKE         DEFINE    AEFYR         KKFYR
     C     *LIKE         DEFINE    AEEMP#        KKEMP#
     C     *LIKE         DEFINE    AEDIV         KKDIV
     C     *LIKE         DEFINE    AESEQ#        KKSEQ#
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    SCCXDT        SYSDT1
     C                   Z-ADD     0             I1                3 0
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
     C*** BEG ADD ***** 02/08/12 ********************************
     C                   Z-ADD     *ZEROS        K#SEQ#
     C                   MOVE      *BLANKS       K#NAM1
     C*** END ADD ***** 02/08/12 ********************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*** BEG ADD ***** 02/08/12 ********************************
     C                   Z-ADD     SCSEQ#        K#SEQ#            5 0
     C                   MOVEL     SCNAM1        K#NAM1
     C*** END ADD ***** 02/08/12 ********************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   CLEAR                   X1DATA
     C                   CLEAR                   X2DATA
     C*
     C                   Z-ADD     AESEQ#        SCSEQ#
     C                   MOVEL     AENAM1        SCNAM1
     C*
     C                   MOVEL(P)  AESEQ#        XXNUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
     C                   MOVEL     XXNUM         X1SEQ#
     C                   MOVEL     XXNUM         X2SEQ#
     C*
     C                   MOVEL     AENAM1        X1NAM1
     C                   MOVEL     AENAM1        X2NAM1
     C*
     C                   SELECT
     C*
     C     VIEW#         WHENEQ    1
     C*
     C                   MOVEL     X1DATA        SCDATA
     C*
     C     VIEW#         WHENEQ    2
     C*
     C                   MOVEL     X2DATA        SCDATA
     C*
     C                   ENDSL
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
     C*  Pass back 'F' if this program was cancelled or ended.
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   MOVE      'F'           @@CALL
     C                   ELSE
     C                   MOVE      ' '           @@CALL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C*
     C*  Get Employer Detail record
     C*
     C     @@RRN2        CHAIN     PEL01                              60
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                   ELFAF=A
     C*
     C*  Get Employer Master record
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*  Compress EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C*  Get most recent cancel date
     C*
     C                   MOVEL     XXEDN         SCEDN
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   MOVE      *ON           *IN81
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  Get Fund Master information
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C     FNEXDT        IFNE      *ZEROS
     C     SYSDT         ANDGE     FNEXDT
     C                   MOVE      *ON           *IN09
     C                   MOVEL     '1'           @@CALL
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C                   Z-ADD     SYSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SYSDT1
     C*
     C*  Get Agent/Producer information
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C*  Get User Parm information
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C*  Start in view that was specified in parms or default to 1
     C*
     C     Q1PRMS        IFGT      5
     C     @@VIEW        ANDLE     VIEWMX
     C     @@VIEW        ANDGT     *ZERO
     C                   Z-ADD     @@VIEW        VIEW#
     C                   ENDIF                                                  Q1PRMS>5
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*** BEG CHG ***** 02/08/12 ********************************
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C***21**    KEY1      SETGTDBRCDF               96
     C***22**    KEY1      SETGTDBRCDF2              96
     C*** END CHG ***** 02/08/12 ********************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*** BEG CHG ***** 02/08/12 ********************************
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C***21**    KEY1      SETLLDBRCDF               96
     C***22**    KEY1      SETLLDBRCDF2              96
     C*** END CHG ***** 02/08/12 ********************************
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL     DELETD        SCDATA
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
Opt  Alt Emp #  Name
Opt  Name                                      Alt Emp #
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
3
