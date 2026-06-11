     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('S2010R')
     H*====================================================================
     H/TITLE ICD9 codes display/update
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H*  09/08/14  C:280  Project: 147MDC
     H*                   Add new field DISPECIFIC
     H*
     H*  05/18/10  C:198  Project: WC1879
     H*                   Updated processing of new field DICMS
     H*
     H*  04/14/10  C:251  Support: 72361
     H*                   Removed Procedure UpdateMc from PGMEND and fixed
     H*                   in Subroutine UPDATR, WRITER
     H*
     H*  12/21/09  C:198  Support: 71090
     H*                   Changed CHGSTS to update status in FMDIP.
     H*
     H*  12/15/09  C:198  Support: 70979
     H*                   Fixed the logic to write to FMDIP.
     H*
     H*====================================================================
     Fs2010f02  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     F*
     Fsmdip     UF A E             DISK    RENAME(PDI01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     Fsmdil     IF   E           K DISK
     F*
     Fsidip     uf a e           k disk    rename(pdi01:pdi0p)
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     DUpdateMC         pr                  extPgm('FA210R')
     D rrn                            9  0 const
     D mode                           1    const
     D fromFile                       2    const
     D toFile                         2    const
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('ICD9 Codes')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D @@area          s              2
     D*
     D/copy sCopyMT42c
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   parm                    @@area
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
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
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C*
     C*===============================================================
     C* CALLR - SUBROUTINE FOR CALLING THE NEXT PROGRAM
     C*===============================================================
     C*
     C     CALLR         BEGSR
     C     @@MODE        IFNE      'S'
     C     @@MODE        andne     'I'
     C*
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C*
     C                   MOVE      DBRRN         ##RRN
     C                   MOVE      'C'           ##MODE
     C*
     C                   ELSE
     C*
     C                   MOVE      @@RRN         ##RRN
     C                   MOVE      'D'           ##MODE
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2010R99'    ACDPL                  99
     C  N99              EXSR      CALLRT
     C                   ENDIF
     C*
     C                   ENDSR                                                  CALLR
     C*================================================================
     C* CALLRT - RETURN FROM STATE SPECIFIC SCREEN PROGRAMS
     C*================================================================
     C     CALLRT        BEGSR
     C*
     C     @@MODE        CASEQ     'C'           PRECHG
     C     @@MODE        CASEQ     'D'           PREDSP
     C*                    CAS            PREOTH
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* Change insufficient status
     C*================================================================
     C     ChgInsSts     begsr
     C*
     C*** Beg Chg *** 05/18/10 **********************************
     C***  @@RRN         chain(n)  dbrcdf
     C     @@RRN         chain     dbrcdf
     C*** End Chg *** 05/18/10 **********************************
     C*
     C                   if        %found
     C
     C*** Beg Chg *** 05/18/10 **********************************
B01  C****               eval      DIINSSTS = 'A'
     C**** DIICD9        chain     pdi0p
     C*
     C****               select
B01  C****               when      DIINSSTS = 'A'
     C****               eval      DIINSSTS = 'I'
     C****               other
     C****               eval      DIINSSTS = 'A'
     C****               endsl
     C*
     C                   select
B01  C                   when      DICMS = 'I'
     C                   eval      DICMS = 'E'
     C                   other
     C                   eval      DICMS = 'I'
     C                   endsl
     C                   EXSR      UPDFLD
     C*** End Chg *** 05/18/10 **********************************
     C*
     C*** Beg Del *** 05/18/10 **********************************
     C***                select
     C***                when      %found
     C***                exsr      MaintFlds
     C***                update    pdi0p
     C***                other
     C***                exsr      InitMaintFlds
     C***                write     pdi0p
     C***                endsl
     C*** End Del *** 05/18/10 **********************************
     C*
E01  C                   endif
     C*
     C*** Beg Add *** 05/18/10 **********************************
     C                   callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*** End Add *** 05/18/10 **********************************
     C                   endsr
     C*================================================================
     C* CHGSTS - Change status of item
     C*================================================================
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C     DISTAT        IFEQ      'I'
     C                   MOVEL     'A'           DISTAT
     C                   ELSE
     C                   MOVEL     'I'           DISTAT
     C                   ENDIF
     C                   EXSR      UPDFLD
E01  C                   ENDIF
     C*** Beg Add *** 12/21/09 ****************************************
     C                   callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*** End Add *** 12/21/09 ****************************************
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C                   Z-ADD     1             SCRN#
     C*
     C     SCRN#         IFEQ      #OFSCR
     C                   EXSR      CALLR
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C     SCICD9        IFEQ      *BLANKS
     C                   SETON                                        8990
     C                   ENDIF
     C     SCLOST        IFEQ      'D'
     C                   MOVEL(P)  SCICD9        KKICD9
     C                   ELSE
     C                   MOVEL(P)  SCICD9        XXONE             1
     C     XXONE         IFEQ      *BLANKS
     C                   MOVEL(P)  SCICD9        KKICD9
     C                   ELSE
     C                   MOVEL(P)  SCICD9        XXFOUR            4
     C                   MOVE(P)   XXFOUR        KKICD9
     C                   ENDIF
     C                   ENDIF
     C*
     C* MUST BE A VALID LOST TYPE
     C*
     C     *IN40         IFEQ      *ON
     C*
     C     *IN90         IFEQ      *OFF
     C     SCLOST        ANDNE     'D'
     C     SCLOST        ANDNE     'O'
     C                   SETON                                        8690
     C                   ENDIF
     C*
     C* CHECK FOR DUPLICATES
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL(P)  SCLOST        KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        8590
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCDSC1        ANDEQ     *BLANKS
     C                   SETON                                        8890
     C                   ENDIF
     C     *IN90         IFEQ      *OFF
     C     SCDS1         ANDEQ     *BLANKS
     C                   SETON                                        8790
     C                   ENDIF
     C*
     C                   Z-ADD     1             SCRN#             1 0
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
     C*
     C*================================================================
     C* Update the initial maintenance fields
     C*================================================================
     C     InitMaintFlds begsr
     C*
     C                   exsr      MaintFlds
     C*
     C                   eval      DIIUSR = DIUSER
     C                   eval      DIIPGM = DIPGM
     C                   eval      DIIDT  = DIDT
     C                   eval      DIITIM = DITIME
     C*
     C                   endsr
     C*
     C*================================================================
     C* Update the maintenance fields
     C*================================================================
     C     MaintFlds     begsr
     C*
     C                   eval      DIUSER = q1User
     C                   eval      DIPGM  = q1Pgm
     C                   eval      DIDT   = DateTo7(SystemDate)
     C                   eval      DITIME = TimeTo6(SystemTime)
     C*
     C                   endsr
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     DIICD9        SCICD9
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     DILGRP        SCLGRP
     C                   MOVEL     DILOST        SCLOST
     C                   MOVEL     DISTAT        SCSTAT
     C*
     C*** Beg Add *** 09/08/14 **********************************
     C*
     C                   MOVE      DIspecific    SCspecific
     C*
     C*** End Add *** 09/08/14 **********************************
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCICD9
     C                   MOVEL     *BLANKS       SCDSC1
     C                   MOVEL     *BLANKS       SCDS1
     C                   MOVEL     *BLANKS       SCLGRP
     C                   MOVEL     *BLANKS       SCLOST
     C                   MOVEL     'A'           SCSTAT
     C*
     C*** Beg Add *** 09/08/14 **********************************
     C*
     C                   MOVE      *blanks       SCspecific
     C*
     C*** End Add *** 09/08/14 **********************************
     C*
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   MOVEL(P)  KKICD9        DIICD9
     C                   MOVEL     SCDSC1        DIDSC1
     C                   MOVEL     SCDS1         DIDS1
     C                   MOVEL     SCLGRP        DILGRP
     C                   MOVEL     SCLOST        DILOST
     C                   MOVEL     SCSTAT        DISTAT
     C*
     C*** Beg Add *** 09/08/14 **********************************
     C*
     C                   MOVE      SCspecific    DIspecific
     C*
     C*** End Add *** 09/08/14 **********************************
     C*
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
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** Beg Del *** 04/14/10 ****************************************
     C******** Ensure WCMC is kept in sync
     C********           callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*** End Del *** 04/14/10 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C     @@MODE        COMP      'C'                                    10
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   select
     C                   when      @@Mode = 'S'
     C                   exsr      ChgSts
     C                   return
     C                   when      @@Mode = 'I'
     C                   exsr      ChgInsSts
     C                   return
     C                   endsl
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   EXSR      UPDFLD
E01  C                   ENDIF
     C*** Beg Add *** 04/14/10 ****************************************
     C****** Ensure WCMC is kept in sync
     C                   callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*** End Add *** 04/14/10 ****************************************
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDFLD        BEGSR
     C                   exsr      MaintFlds
     C                   UPDATE    DBRCDF
     C                   EXSR      CALLR                                        DEL 01/07/99
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVE      'A'           DISTAT
     C                   exsr      InitMaintFlds
     C                   WRITE     DBRCDF
     C*** Beg Del *** 12/15/09 ****************************************
     C**********         feod      smdip
     C*** End Del *** 12/15/09 ****************************************
     C                   EXSR      CALLR                                        DEL 01/07/99
     C*** Beg Add *** 04/14/10 ****************************************
     C****** Ensure WCMC is kept in sync
     C                   callp(e)  UpdateMC(dbrrn:'U':'SY':'MC')
     C*** End Add *** 04/14/10 ****************************************
     C                   ENDSR
