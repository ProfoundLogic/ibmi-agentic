     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/24/13  C:283 Support: OP099 82928                         *
     H*                  Menu setup edit for '&' to ensure an '&' is  *
     H*                  not in Menu Name when new menu is being      *
     H*                  added.                                       *
     H*                                                               *
     H*  06/28/13  C:65  Support: Redmine #3232                       *
     H*                  Remove edit on last position of the CMD      *
     H*                  field as that value is no longer used.       *
     H*                                                               *
     H*  01/16/13  C:283 Support: WA102 81476                         *
     H*                  Reverting the Changes as the 'a' was         *
     H*                  intentional.                                 *
     H*                                                               *
     H*  01/16/13  C:283 Support: WA102 81476                         *
     H*                  Correctly populate the OPstat value as       *
     H*                  'A' when the Status is changed from Inactive *
     H*                  to Active.                                   *
     H*                                                               *
     H*  12/19/12  C:282 Project: SY266                               *
     H*                  Removed Command prompt text and checkbox     *
     H*                  field SCYNO from display file. Commented out *
     H*                  variable SCYNO.                              *
     H*                  Also removed the menu option Prompt Command. *
     H*                  from rich format display file.               *
     H*                                                               *
     H*  06/22/12  C:33  Project: SY266                               *
     H*                  Allow for lowercase entry of desription.     *
     H*                                                               *
     H*  05/14/12  C:33  Project: SY266                               *
     H*                  Allow for 3rd status for each Menu option    *
     H*                   - 'A' = Active in GS and Transform          *
     H*                   - 'a' = Active in GS only                   *
     H*                   - 'I' = Inactive                            *
     H*                                                               *
     H*  06/09/98  C:69  Support: PDS internal                        *
     H*                  Checked to see if a menu exists when doing   *
     H*                  an option 3=Copy                             *
     H*  05/28/98  C:69  Support: PDS internal                        *
     H*                  Changed program to allow a ? when keying a   *
     H*                  command. No validation will be done with ?   *
     H*  05/20/98  C:69  Support: PDS internal                        *
     H*                  Corrected program when checking to see if a  *
     H*                  menu already exists                          *
     H*****************************************************************
     FM0010F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FMMOPL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*  Physical file definition (for maintaining record)
     FMMOPP     UF A E             DISK
     F                                     RENAME(POP01:DBRCDF)
      /COPY SCOPYMT01D
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Menus/Menu options')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(2)
      *** begin add *** 07/24/13 *****************************************************
     D @@RST           S              2P 0
      *** end   add *** 07/24/13 *****************************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*
     C*  Command checking parameter list
     C*
     C     CMDPL         PLIST
     C                   PARM                    ##CMD           231
     C                   PARM      231           ##LEN            15 5
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     MOKEY         KLIST
     C                   KFLD                    KKMNAM
     C                   KFLD                    KKOPTN
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    OPMNAM        KKMNAM
     C     *LIKE         DEFINE    OPOPTN        KKOPTN
     C     *LIKE         DEFINE    OPMNAM        SVMNAM
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* APSRCH - Application flag search call
     C*================================================================
     C     APSRCH        BEGSR
     C                   CALL      'M0502R'
      *** begin chg *** 06/28/13 *****************************************************
     C*********          PARM      *BLANKS       QQPRDS
     C*********          PARM      *BLANKS       QQAPFL
     C                   PARM      *BLANKS       QQPRDS           40
     C                   PARM      *BLANKS       QQAPFL            1
      *** end chg ***** 06/28/13 *****************************************************
     C                   PARM                    QQRTCD            1
     C*
     C                   SELECT
     C     QQRTCD        WHENEQ    'A'
     C                   MOVE      *ON           *INKC
     C     QQRTCD        WHENEQ    'B'
     C     QQAPFL        OREQ      *BLANK
     C                   MOVE      *ON           *INKL
     C     QQRTCD        WHENEQ    '*'
     C                   MOVE      QQAPFL        SCEXCL
     C                   ENDSL
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
      /Free
       Select;
       When OPstat = 'A';
          OPstat = 'I';
       When OPstat = 'I' and OPmYN = 'Y';
       //*** Beg Chg ***** 01/16/13 *************************************** //
       //*************OPstat = 'a';
       //*************OPstat = 'A';
          OPstat = 'a';
       //*** End Chg ***** 01/16/13 *************************************** //
       Other;
          OPstat = 'A';
       Endsl;
      /End-Free
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C* If a menu is being deleted, then delete the menu options
     C* first.
     C*
     C     OPOPTN        IFEQ      *BLANKS
     C     OPMNAM        CHAIN     MMOPL                              99
B01  C     *IN99         DOWEQ     *OFF
     C     OPOPTN        IFNE      *BLANKS
     C     DBRRN         DELETE    DBRCDF                             98
     C                   ENDIF
     C     OPMNAM        READE     MMOPL                                  99
E01  C                   ENDDO
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C     SCMNAM        IFEQ      *BLANKS
     C                   SETON                                        7190
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOPTN        ANDNE     *BLANKS
     C                   MOVEL     SCMNAM        KKMNAM
     C                   MOVEL     *BLANKS       KKOPTN
     C     MOKEY         CHAIN     MMOPL                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCOPTN        ANDNE     HHOPTN
     C                   MOVEL     SCMNAM        KKMNAM
     C                   MOVEL     SCOPTN        KKOPTN
     C     MOKEY         CHAIN     MMOPL                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMNTT        ANDEQ     *BLANKS
     C                   SETON                                        7490
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMYN         ANDNE     'Y'
     C     SCMYN         ANDNE     'N'
     C                   SETON                                        7590
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCAPP         ANDNE     *BLANKS
     C     'M'           CAT(P)    SCAPP:0       ##OBJN
     C                   CAT       'XA':0        ##OBJN
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    ##OBJN           10
     C                   PARM      '*DTAARA'     ##OBJT            7
     C     *IN90         PARM                    ##ERR             1
     C     *IN90         IFEQ      *ON
     C                   SETON                                        83
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMYN         ANDEQ     'Y'
     C     SCMINS        ANDNE     *ZERO
     C                   SETON                                        7790
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMYN         ANDEQ     'N'
     C     SCMMSC        ANDNE     *ZERO
     C                   SETON                                        7690
     C                   ENDIF
     C*
      *** Beg Del ***** 12/19/12 *****************************************************
     C******IN90         IFEQ      *OFF
     C*****SCYNO         ANDNE     'Y'
     C*****SCYNO         ANDNE     'N'
     C*****              SETON                                        7890
     C*****              ENDIF
      *** End Del ***** 12/19/12 *****************************************************
     C*
      *** Beg Del ***** 12/19/12 *****************************************************
     C******IN90         IFEQ      *OFF
     C*****SCYNO         ANDEQ     'Y'
     C*****'?'           SCAN      SCEXCL        Q                 3 0
     C*****Q             IFEQ      *ZEROS
     C*****              MOVEL(P)  SCEXCL        ##CMD
     C*****              CALL      'QCMDCHK'     CMDPL                  90
     C******IN90         IFEQ      *ON
     C*****              SETON                                        7990
     C*****              ENDIF
     C*****              ENDIF
     C*****              ENDIF
      *** End Del ***** 12/19/12 *****************************************************
     C*
      *** begin del *** 06/28/13 *****************************************************
     C******IN90         IFEQ      *OFF
     C*****SCMYN         ANDEQ     'Y'
     C*****1             SUBST(P)  SCEXCL:230    QQAPFL
     C*****              CALL      'M0502R'
     C*****              PARM      *BLANKS       QQPRDS           40
     C*****              PARM                    QQAPFL            1
     C*****              PARM      'V'           QQRTCD            1
     C*
     C*****QQRTCD        IFEQ      'E'
     C*****              SETON                                        8090
     C*****              ENDIF
     C*****              ENDIF
      *** end del ***** 06/28/13 *****************************************************
      *** begin add *** 07/24/13 *****************************************************
     C                   IF        *IN90 = *OFF
     C                   EVAL      @@RST = *Zeros
     C                   EVAL      @@RST = %SCAN('&' : SCMNAM)
     C                   IF        @@RST > 0
     C                   EVAL      *IN90 = *ON
     C                   EVAL      *IN84 = *ON
     C                   ENDIF
      *
     C                   ENDIF
      *** end   add *** 07/24/13 *****************************************************
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     OPMNAM        HHMNAM
     C     CPYFLG        IFEQ      *OFF
     C                   MOVEL     OPMNAM        SCMNAM
     C                   MOVEL     OPOPTN        SCOPTN
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     *HIVAL        HHOPTN
     C                   ELSE
     C                   MOVEL     OPOPTN        HHOPTN
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *HIVAL        HHOPTN
     C     OPOPTN        IFEQ      *BLANKS
     C                   MOVEL     *BLANKS       SCMNAM
     C                   ELSE
     C                   MOVEL     *BLANKS       SCOPTN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     OPMYN         SCMYN
     C                   MOVEL     OPAPP         SCAPP
     C                   MOVEL     OPEXCL        SCEXCL
      *** Beg Del ***** 12/19/12 *****************************************************
     C*****              MOVEL     OPYNO         SCYNO
      *** End Del ***** 12/19/12 *****************************************************
     C                   MOVEL     OPMMSC        SCMMSC
     C                   MOVEL     OPMINS        SCMINS
     C                   MOVEL     OPTY01        SCTY01
     C                   MOVEL     OPTY02        SCTY02
     C                   MOVEL     OPTY03        SCTY03
     C                   MOVEL     OPTY04        SCTY04
     C                   MOVEL     OPTY05        SCTY05
     C                   MOVEL     OPTY06        SCTY06
     C                   MOVEL     OPMNTT        SCMNTT
     C                   MOVEL     OPTX01        SCTX01
     C                   MOVEL     OPTX02        SCTX02
     C                   MOVEL     OPTX03        SCTX03
     C                   MOVEL     OPTX04        SCTX04
     C                   MOVEL     OPTX05        SCTX05
     C                   MOVEL     OPTX06        SCTX06
     C                   MOVEL     OPTX07        SCTX07
     C                   MOVEL     OPTX08        SCTX08
     C                   MOVEL     OPTX09        SCTX09
     C                   MOVEL     OPTX10        SCTX10
     C                   MOVEL     OPTX11        SCTX11
     C                   MOVEL     OPTX12        SCTX12
     C                   MOVEL     OPSTAT        SCSTAT
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL     SCMNAM        OPMNAM
     C                   MOVEL     SCOPTN        OPOPTN
     C                   MOVEL     SCMYN         OPMYN
     C                   MOVEL     SCAPP         OPAPP
     C                   MOVEL     SCEXCL        OPEXCL
      *** Beg Del ***** 12/19/12 *****************************************************
     C*****              MOVEL     SCYNO         OPYNO
      *** End Del ***** 12/19/12 *****************************************************
     C                   MOVEL     SCMMSC        OPMMSC
     C                   MOVEL     SCMINS        OPMINS
     C                   MOVEL     SCTY01        OPTY01
     C                   MOVEL     SCTY02        OPTY02
     C                   MOVEL     SCTY03        OPTY03
     C                   MOVEL     SCTY04        OPTY04
     C                   MOVEL     SCTY05        OPTY05
     C                   MOVEL     SCTY06        OPTY06
     C                   MOVEL     SCMNTT        OPMNTT
     C                   MOVEL     SCTX01        OPTX01
     C                   MOVEL     SCTX02        OPTX02
     C                   MOVEL     SCTX03        OPTX03
     C                   MOVEL     SCTX04        OPTX04
     C                   MOVEL     SCTX05        OPTX05
     C                   MOVEL     SCTX06        OPTX06
     C                   MOVEL     SCTX07        OPTX07
     C                   MOVEL     SCTX08        OPTX08
     C                   MOVEL     SCTX09        OPTX09
     C                   MOVEL     SCTX10        OPTX10
     C                   MOVEL     SCTX11        OPTX11
     C                   MOVEL     SCTX12        OPTX12
     C                   MOVEL     SCSTAT        OPSTAT
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
      *** Beg Del ***** 12/19/12 *****************************************************
     C******INKD         IFEQ      *ON
     C*****SCYNO         IFEQ      'Y'
     C*****'?'           CAT       SCEXCL:0      ##CMD
     C*****              CALL      'QCMDCHK'     CMDPL                  99
B03  C******IN99         IFEQ      *OFF
     C*****              MOVEL     ##CMD         SCEXCL
E03  C*****              END
     C*****              SETON                                        90
     C*****              ELSE
     C*****              SETON                                        8190
     C*****              ENDIF
     C*****              ENDIF
      *** End Del ***** 12/19/12 *****************************************************
     C*
     C     *INKJ         IFEQ      *ON
     C                   SELECT
     C     HFLD          WHENEQ    'SCEXCL'
     C                   EXSR      APSRCH
     C                   OTHER
     C                   SETON                                        8290
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      /Free
       *inlr = *on;
      /End-Free
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C                   SETOFF                                       40
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'P'
     C                   SETON                                        40
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      UPDATR
     C                   RETURN
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* PUTPDT - Put the program, date, time and user into fields
     C*================================================================
     C     PUTPDT        BEGSR
     C                   MOVEL     Q1USER        OPUSER
     C                   MOVEL     Q1PGM         OPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OPDT
     C                   MOVEL     DSTIME        OPTIME
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C     @@MODE        IFNE      'S'
     C                   EXSR      MVSCDB
     C                   ELSE
     C                   EXSR      CHGSTS
     C                   ENDIF
     C*
     C                   EXSR      PUTPDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      'A'           OPSTAT
     C*
     C                   EXSR      PUTPDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      MMOPP
     C*
     C     @@MODE        IFEQ      'P'
     C     OPOPTN        ANDEQ     *BLANKS
     C                   MOVE      OPMNAM        SVMNAM
     C     HHMNAM        CHAIN     MMOPL                              99
     C     *IN99         DOWEQ     *OFF
     C     OPOPTN        IFNE      *BLANKS
     C                   MOVE      SVMNAM        OPMNAM
     C                   EXSR      PUTPDT
     C                   WRITE     DBRCDF
     C                   ENDIF
     C     HHMNAM        READE     MMOPL                                  99
     C                   ENDDO
     C                   FEOD      MMOPP
     C                   ENDIF
     C*
     C                   ENDSR
