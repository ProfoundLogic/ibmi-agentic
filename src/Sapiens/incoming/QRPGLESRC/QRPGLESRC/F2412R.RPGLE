     H Copyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H* DESCRIPTION: Update state flags. Access state utility programs*
     H*****************************************************************
     H* CALLED FROM: Command line STATES                              *
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     H*  10/26/23  C: 342  Project: FS668                             *
     H*                    State Rules maintenance C/S Standards      *
     H*                                                               *
     H*  12/09/09  C: 198  Project: TX064                             *
     H*                    Added option 12.                           *
     H*                                                               *
     H*  12/30/08  C: 220  SUPPORT: 65592                             *
     H*                    STATES CMD NEED PASSWORD PROTECTION        *
     H*                                                               *
     H*  01/16/02  C: 062  PROJECT: 114MAO                            *
     H*                    ADD OPTION 10 AND 11.                      *
     H*                                                               *
     H*  01/23/01  C: 117  PROJECT: FSMNT #151                        *
     H*                    HAD TO MAKE A CHANGE TO PARMS FOR CALL TO  *
     H*                    F2385R                                     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     FF2412F    CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTSTL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PST01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(17) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(17) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('States')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('SY601R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D*** BEG ADD *** 12/30/08 ****************************************
     D XXPWD           C                   CONST('12345PDS            ')

     C*** END ADD *** 12/30/08 ****************************************
     d*** Beg Add *** 10/26/23 ****************************************
      // calling parameter for MtStRules
     d @@rrn1          s              9
     d @@mode1         s              1
     d @@next1         s             10
     d @@tabind1       s              5
     d MtStRules       pr                  ExtPgm('MTSTRULES')
     d @@rrn1                         9
     d @@mode1                        1
     d @@next1                       10
     d @@tabind1                      5
     D*** End Add *** 10/26/23 ****************************************
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
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
     C*  State rules retrieve program parameter list
     C*
     C     RULEL         PLIST
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C*
     C*  State messages retrieve program parameter list
     C*
     C     MESGL         PLIST
     C                   PARM      ' '           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C*** BEG ADD *** 01/23/01 ****************************************
     C                   PARM                    ##TYPE            1
     C                   PARM      SCST          ##ST              2
     C                   PARM                    ##MSG#            7
     C*** END ADD *** 01/23/01 ****************************************
     C*
     C*  State adj programs retrieve program parameter list
     C*
     C     ADJPL         PLIST
     C                   PARM      ' '           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C*
     C*  State BEAST records retrieve program parameter list
     C*
     C     BEASL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C*** END ADD **** 12/30/08 **************************************
     C*
     C                   EXFMT     SCRPWD
     C     *INKC         CABEQ     *ON           EOJ
     C     SCPWD         IFNE      XXPWD
     C     EOJ           TAG
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C*** END ADD **** 12/30/08 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 01/16/02 **************************************
     C     SCSEL         IFEQ      '10'
     C     SCSEL         OREQ      '11'
     C     SCST          IFNE      'CC'
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 01/16/02 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFNE      '2'
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
     C                   MOVE      *LOVAL        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL(P)  SCPST1        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCKEY         KKKEY
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     STKEY         SCKEY
     C                   MOVEL     STKEY         SCST
     C*
     C                   MOVEL(P)  STKEY         SCDATA
     C                   MOVE      STNAME        SCDATA
     C*
     C                   MOVEL     STSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - SUBROUTINE FOR OPTION 6
     C*================================================================
     C*
     C* OPTION 6=WORK WITH STATE RULES
     C*
     C     OPT6          BEGSR
     C*
     C*** Beg Chg *** 10/26/23 ****************************************
     C*                  CALL      'F2400R'      RULEL                  99
              @@rrn1    = %char(scdbrr);
              @@mode1   = 'C';
              @@next1   = *blanks;
              @@TabInd1 = *blanks;
              MtStRules(@@rrn1:@@mode1:@@next1:@@TabInd1);
     C*** End Chg *** 10/26/23 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - SUBROUTINE FOR OPTION 7
     C*================================================================
     C*
     C* OPTION 7=WORK WITH STATE MESSAGES
     C*
     C     OPT7          BEGSR
     C*
     C                   CALL      'F2385R'      MESGL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*================================================================
     C*
     C* OPTION 8=WORK WITH STATE ADJ PROGRAMS
     C*
     C     OPT8          BEGSR
     C*
     C                   CALL      'F2401R'      ADJPL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT9 - SUBROUTINE FOR OPTION 9
     C*================================================================
     C*
     C* OPTION 9=WORK WITH BEAST RECORDS
     C*
     C     OPT9          BEGSR
     C*
     C                   CALL      'F2411R'      BEASL
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/16/02 **************************************
     C*================================================================
     C* OPT10 - COMPREHENSIVE AND COMPONENT CODES
     C*================================================================
     C     OPT10         BEGSR
     C*
     C                   CALL      'F2533R'                             99
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      *BLANKS       ##KC              1
     C     *INKL         PARM      *BLANKS       ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT11 - MUTUALLY EXCLUSIVE PROCEDURES
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   CALL      'F2534R'                             99
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      *BLANKS       ##KC              1
     C     *INKL         PARM      *BLANKS       ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C                   ENDSR
     C*** END   ADD *** 01/16/02 **************************************
     C*** BEG ADD *** 12/09/09 ****************************************
     C*================================================================
     C* OPT12 - FS rate table
     C*================================================================
     C     OPT12         BEGSR
     C*
     C                   CALL      'F2402R'                             99
     C                   PARM      *BLANKS       ##CALL            1
     C     *INKC         PARM      *BLANKS       ##KC              1
     C     *INKL         PARM      *BLANKS       ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR
     C*** END ADD *** 12/09/09 ****************************************
     C*** END   ADD *** 01/16/02 **************************************
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
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '6 '          OPT6
     C     SCSEL         CASEQ     '7 '          OPT7
     C     SCSEL         CASEQ     '8 '          OPT8
     C     SCSEL         CASEQ     '9 '          OPT9
     C*** BEGIN ADD *** 01/16/02 **************************************
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C*** BEG ADD *** 12/09/09 ****************************************
     C     SCSEL         CASEQ     '12'          OPT12
     C*** END ADD *** 12/09/09 ****************************************
     C*** END   ADD *** 01/16/02 **************************************
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
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
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     STSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
State    Name                                                          Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1066=State rules            YYY
1077=State messages         YYY
1088=State adj programs     YYY
1099=Beast records          YYY
11010=Comp codes            YYY
11111=Mutual/Excl codes     YYY
11212=FS rate table         YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
