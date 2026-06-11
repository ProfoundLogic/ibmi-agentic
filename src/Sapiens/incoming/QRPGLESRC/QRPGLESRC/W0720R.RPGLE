     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  03/16/20 C:300 PROJECT: WC2196                               *
      *                 Make it pass-through for Region Search screen *
      *                                                               *
      * 02/11/02 C:136 SUPPORT# 31225                                 *
      *                NO HEADERS ON 2ND VIEW                         *
      *                                                               *
      *                                                               *
      * 11/03/00  C:90  PROJECT: WC577                                *
      *                 ALLOW SUBSET ON CHAPTER DESCRIPTION.          *
      *                                                               *
      * 04/06/98  C:89  PROJECT: WC577                                *
      *                 REMOVED F13 FROM SCREEN.                      *
      *                                                               *
      *****************************************************************
     FW0720F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMCPP     IF   E             DISK
     F                                     RENAME(PCP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMCPL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCP01:DBRCDF)
     F**** BEGIN ADD **** 11/03/00 ************************************
     FWMCPL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCP01:DBRCDF2)
     F**** END   ADD **** 11/03/00 ************************************
     FWMFNP     IF   E             DISK
      /COPY SCOPYWW01D
     D*
     D**** BEGIN CHG **** 11/03/00 ************************************
     D******************  HD      1   1 74               Column headings
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D**** END   CHG **** 11/03/00 ************************************
      **** BEGIN CHG **** 04/06/98 ************************************
     D******************* OF      1  11  1   OFD    35   Options, Function
     D**** BEGIN CHG **** 11/03/00 ************************************
     D******************* OF      1  10  1   OFD    35   Options, Function
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D**** END   CHG **** 11/03/00 ************************************
      **** END   CHG **** 04/06/98 ************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Chapters')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0720R02')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D**** BEGIN CHG **** 11/03/00 ************************************
     D************* 1                     C         VIEWMX
     D VIEWMX          C                   CONST(2)

     I**** END   CHG **** 11/03/00 ************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@CHAP
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
     C                   KFLD                    SCFND
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    KKCHAP
     C**** BEGIN ADD **** 11/03/00 ************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    KKDESC
     C**** END   ADD **** 11/03/00 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CPCHAP        KKCHAP
     C     *LIKE         DEFINE    CPCHAP        @@CHAP
     C**** BEGIN ADD **** 11/03/00 ************************************
     C     *LIKE         DEFINE    CPDESC        KKDESC
     C**** END   ADD **** 11/03/00 ************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
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
     C*
     C     SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKCHAP
     C**** BEGIN ADD **** 11/03/00 ************************************
     C                   MOVE      *LOVAL        KKDESC
     C**** END   ADD **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C**** BEGIN CHG **** 11/03/00 ************************************
     C******************** MOVE SCPST1    KKCHAP
     C   21              MOVE      SCPST1        KKCHAP
     C   22              MOVEL     SCPST2        KKDESC
     C**** END   CHG **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCCHAP        KKCHAP
     C**** BEGIN ADD **** 11/03/00 ************************************
     C                   MOVEL     SCDESC        KKDESC
     C**** END   ADD **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     CPSTAT        SCSTAT
     C                   MOVE      CPCHAP        SCCHAP
     C                   MOVEL     CPDESC        SCDESC
     C*
     C                   MOVEL     CPCHAP        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXCHAP            3
     C*
     C**** BEGIN ADD **** 11/03/00 ************************************
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C**** END   ADD **** 11/03/00 ************************************
     C                   MOVEL(P)  XXCHAP        SCDATA
     C                   MOVE      CPDESC        SCDATA
     C**** BEGIN ADD **** 11/03/00 ************************************
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  CPDESC        SCDATA
     C                   MOVE      XXCHAP        SCDATA
     C                   ENDSL
     C**** END   ADD **** 11/03/00 ************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
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
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
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
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C* GET GROUP INFORMATION
     C*
     C     @@RRN2        CHAIN     PFN01                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
      **** begin add **** 03/16/20 ******************************************
      * Pass-through this program, when called for a Search screen
     C                   IF        @@CALL = '2'
     C                   MOVE      @@RRN2        @@RRNCH
     C                   CALL      'WTGRPPRI'
     C                   PARM                    @@RRNCH           9
     C                   PARM      'D'           @@MODE            1
     C                   PARM      'CHAPTER'     @@NEXT           10
     C                   PARM      *Blanks       @@TABIND          5
      *
     C                   MOVEL     @@TABIND      @@CHAP
     C                   RETURN
     C                   ENDIF
      **** end add **** 03/16/20 ********************************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C**** BEGIN ADD **** 11/03/00 ************************************
     C   22KEY1          READE     DBRCDF2                                95
     C**** END   ADD **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C**** BEGIN ADD **** 11/03/00 ************************************
     C   22KEY1          READPE    DBRCDF2                                95
     C**** END   ADD **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C**** BEGIN ADD **** 11/03/00 ************************************
     C   22KEY3          SETGT     DBRCDF2                            96
     C**** END   ADD **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C**** BEGIN ADD **** 11/03/00 ************************************
     C   22KEY3          SETLL     DBRCDF2                            96
     C**** END   ADD **** 11/03/00 ************************************
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C                   MOVEL     SCCHAP        @@CHAP
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CPSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Headers - HD
Chp  Description                     Sts
Description                     Chp  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
