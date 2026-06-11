     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE W0321R -                            - REINSURANCE SYSTEM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/16/13  C:279 Redmine: 2803                                 *
     H*                 Modified to display the records only for Mult *
     H*                 Clm# passed in entry params.                  *
     H*                                                               *
     H* 07/23/98  C:35  PROJECT: SUPPORT WA051 12788                  *
     H*                 PROGRAM WAS NOT DISPLAYING ANY DATA DUE TO    *
     H*                 INCORRECT SUBSET CODING.  ALSO, POSITIONING   *
     H*                 AND SCROLLING WAS NOT WORKING CORRECTLY.      *
     H*                 ALSO CHANGED ORDER OF FIELDS ON SUBSET SCREEN *
     H*                 TO MATCH SORT ORDER.                          *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
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
     FW0321F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWAMCP     IF   E             DISK
     F                                     RENAME(PMC01:PFRCDF)
      **** BEGIN DEL **** 07/23/98 *********************************************
     F**********
     F********** Primary logical file definition (for displaying items)
     F********** WAMCL2  IF  E           K        DISK
     F**********                                               KINFDS DBFDS
     F**********             PMC01                             KRENAMEDBRCDF
      **** END   DEL **** 07/23/98 *********************************************
     F*
     F* * BEGIN ADD *** 01/28/98 ***********************************************
     F*
     F*  Primary logical file definition (for displaying items)
     F*  CO,FND,FYR,CLM#,CASE
     F*
     FWAMCL1    IF   E           K DISK
      **** BEGIN CHG **** 07/23/98 *********************************************
     F*********** PMC01                             KRENAMEDBRCDF2
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMC01:DBRCDF)
      **** END   CHG **** 07/23/98 *********************************************
     F*
     F* * END   ADD *** 01/28/98 ***********************************************
     F*
     FWMFNL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(5) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(5) ALT(OF)
     D***** beg add ****** 05/16/13 ***************************
     D intClm#         S              7  0
     D***** end add ****** 05/16/13 ***************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Multiple Claims')
     D*
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('NONE')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)
     D VIEWMX          C                   CONST(1)
     D*
     D                 DS
     D  LN2                    1     79
     D  LN2CSH                 1      6
     D  LN2CS                  8     18
     D  LN2A                  21     24
     D  LN2BB                 28     38
     D  LN2B1                 28     30
     D  LN2B2                 31     31
     D  LN2B3                 32     33
     D  LN2B4                 34     34
     D  LN2B5                 35     38
     D  LN2C                  39     62
     D  LN2C2                 39     58
     D  LN2D                  63     66
     D  LN2E                  69     78

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @#CO#             3 0
     C                   PARM                    @#FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CLM#            7 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Claim headers retrieve program parameter list
     C*
     C     CLMAPI        PLIST
     C                   PARM                    @#CO#
     C                   PARM                    @#FND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASN
      **** BEGIN DEL **** 07/23/98 *********************************************
     C******************** PARM           SCCASE
      **** END   DEL **** 07/23/98 *********************************************
     C                   PARM                    ##LN1            79
     C                   PARM                    ##LN2            79
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     FNKY          KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
     C*
     C* * BEGIN DEL *** 01/28/98 ***********************************************
     C*
     C********** KEY1      KLIST
     C**********           KFLD           @#CO#
     C**********           KFLD           @#FND
     C**********           KFLD           KKCLM#
     C**********
     C********** KEY2      KLIST
     C**********           KFLD           @#CO#
     C**********           KFLD           @#FND
     C**********           KFLD           KKCLM#
     C**********           KFLD           KKFYR
     C**********           KFLD           KKCASE
     C*
     C* * END   DEL *** 01/28/98 ***********************************************
     C*
     C* * BEGIN ADD *** 01/28/98 ***********************************************
     C*
     C     KEY1          KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
     C*
     C     KEY2          KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
     C                   KFLD                    KKFYR
     C*
     C     KEY3          KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCLM#
      **** BEGIN ADD **** 07/23/98 *********************************************
     C                   KFLD                    KKCASE
      **** END   ADD **** 07/23/98 *********************************************
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C********** KEY4      KLIST
     C******************** KFLD           @#CO#
     C******************** KFLD           @#FND
     C******************** KFLD           KKCLM#
     C********************
     C********** KEY5      KLIST
     C******************** KFLD           @#CO#
     C******************** KFLD           @#FND
     C******************** KFLD           KKCLM#
     C******************** KFLD           KKFYR
     C********************
      **** END   DEL **** 07/23/98 *********************************************
     C* * END   ADD *** 01/28/98 ***********************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    MCFYR         KKFYR
     C     *LIKE         DEFINE    MCCLM#        KKCLM#
     C     *LIKE         DEFINE    MCCASE        KKCASE
     C     *LIKE         DEFINE    @@CALL        ##MODE
     C     *LIKE         DEFINE    @#FND         ##FND
     C     *LIKE         DEFINE    @#CO#         ##CO#
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
      **** BEGIN CHG **** 07/23/98 *********************************************
     C********** SSSTAT    IFNE *BLANKS
     C     SSFYR         IFNE      0
      **** END   CHG **** 07/23/98 *********************************************
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
      **** BEGIN DEL **** 07/23/98 *********************************************
     C********** SSSTAT    IFNE ' '
     C********** SSSTAT    ANDNE'A'
     C********** SSSTAT    ANDNE'I'
     C******************** SETON                     8990
     C******************** ENDIF
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C********** @@CALL    IFEQ '2'
     C**********
     C******************** MOVEL'A'       SSSTAT
     C********************
     C******************** ENDIF
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
     C                   Z-ADD     @@FYR         SSFYR
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
      **** BEGIN CHG **** 07/23/98 *********************************************
     C******************** MOVE @@CLM#    KKCLM#
     C                   Z-ADD     0             KKCLM#
      **** END   CHG **** 07/23/98 *********************************************
     C*
     C* * BEGIN CHG *** 01/28/98 ***********************************************
     C**********           MOVE *LOVAL    KKFYR
      **** BEGIN CHG **** 07/23/98 *********************************************
     C******************** MOVE @@FYR     KKFYR
     C                   Z-ADD     0             KKFYR
      **** END   CHG **** 07/23/98 *********************************************
     C* * END   CHG *** 01/28/98 ***********************************************
     C*
     C                   MOVE      *ZEROS        KKCASE
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVE      SCPST1        KKFYR
      **** BEGIN CHG **** 07/23/98 *********************************************
     C******************** MOVE SCPSTC    KKCASE
     C                   MOVE      *LOVAL        KKCASE
     C                   MOVE      SCPSTC        KKCLM#
      **** END   CHG **** 07/23/98 *********************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVE      SCFYR         KKFYR
      **** BEGIN CHG **** 07/23/98 *********************************************
     C******************** MOVE SCCASE    KKCASE
     C                   MOVE      SCCASN        KKCASE
     C                   MOVE      SCCLM#        KKCLM#
      **** END   ADD **** 07/23/98 *********************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C******************** MOVELMCSTAT    SCSTAT
      **** END   DEL **** 07/23/98 *********************************************
     C                   MOVE      MCFYR         SCFYR
     C                   MOVE      MCCLM#        SCCLM#
     C                   MOVE      MCCASE        SCCASN
      **** BEGIN DEL **** 07/23/98 *********************************************
     C******************** MOVE MCCASE    SCCASE
     C******************** MOVE MCFYR     SCDAT1    P
     C******************** MOVELMCCLM#    SCDAT1
      **** END   DEL **** 07/23/98 *********************************************
     C*
     C                   CALL      'WA006R'      CLMAPI
     C*
     C                   MOVEL     ##LN2         LN2
     C*
      **** BEGIN CHG **** 07/23/98 *********************************************
     C******************** MOVELSCCASN    SCDAT2    P
     C******************** MOVE LN2C2     SCDAT2
     C                   MOVEL     LN2C2         SCCNAM
      **** END   CHG **** 07/23/98 *********************************************
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
      **** BEGIN ADD **** 07/23/98 *********************************************
     C                   Z-ADD     @@FYR         KKFYR
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   MOVE      *ON           *IN20
     C                   MOVEL     HD(2)         SCHDL2
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C* GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
      /free
       //**** beg add ******** 05/16/13 ****************************************
       intClm# = @@Clm#;
       //**** end add ******** 05/16/13 ****************************************
      /end-free
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C* * BEGIN DEL *** 01/28/98 ***********************************************
     C***21***** KEY1      READEDBRCDF                   95
     C* * END   DEL *** 01/28/98 ***********************************************
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C* * BEGIN ADD *** 01/28/98 ***********************************************
     C**********
     C********** KKFYR     IFNE *ZEROS
     C**********
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY3      READEDBRCDF2                  95
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY2      READEDBRCDF2                  95
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ELSE                            KKFYR=0
     C********************
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY4      READEDBRCDF                   95
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY1      READEDBRCDF                   95
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ENDIF                           KKFYR<>0
     C********************
     C* * END   ADD *** 01/28/98 ***********************************************
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
     C     SSFYR         IFEQ      0
     C   21KEY1          READE     DBRCDF                                 95
     C                   ELSE
     C                   Z-ADD     SSFYR         KKFYR
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDIF                                                  SSFYR IFEQ 0
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C* * BEGIN DEL *** 01/28/98 ***********************************************
     C***21***** KEY1      REDPEDBRCDF                   95
     C* * END   DEL *** 01/28/98 ***********************************************
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C* * BEGIN ADD *** 01/28/98 ***********************************************
     C**********
     C********** KKFYR     IFNE *ZEROS
     C**********
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY3      REDPEDBRCDF2                  95
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY2      REDPEDBRCDF2                  95
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ELSE                            KKFYR=0
     C********************
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY4      REDPEDBRCDF                   95
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY1      REDPEDBRCDF                   95
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ENDIF                           KKFYR<>0
     C********************
     C* * END   ADD *** 01/28/98 ***********************************************
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
     C     SSFYR         IFEQ      0
     C   21KEY1          READPE    DBRCDF                                 95
     C                   ELSE
     C                   Z-ADD     SSFYR         KKFYR
     C   21KEY2          READPE    DBRCDF                                 95
     C                   ENDIF                                                  SSFYR IFEQ 0
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C* * BEGIN DEL *** 01/28/98 ***********************************************
     C***21***** KEY2      SETGTDBRCDF               96
     C* * END   DEL *** 01/28/98 ***********************************************
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C**********
     C********** KKFYR     IFNE *ZEROS
     C**********
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY3      SETGTDBRCDF2              96
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY2      SETGTDBRCDF2              96
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ELSE                            KKFYR=0
     C********************
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY4      SETGTDBRCDF               96
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY1      SETGTDBRCDF               96
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ENDIF                           KKFYR<>0
     C********************
     C* * END   ADD *** 01/28/98 ***********************************************
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
     C     SSFYR         IFNE      0
     C                   Z-ADD     SSFYR         KKFYR
     C                   ENDIF                                                  SSFYR IFNE 0
      *
     C   21KEY3          SETGT     DBRCDF
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C* * BEGIN DEL *** 01/28/98 ***********************************************
     C***21***** KEY2      SETLLDBRCDF               96
     C* * END   DEL *** 01/28/98 ***********************************************
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C**********
     C********** KKFYR     IFNE *ZEROS
     C**********
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY3      SETLLDBRCDF2              96
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY2      SETLLDBRCDF2              96
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ELSE                            KKFYR=0
     C********************
     C********** KKCLM#    IFNE *ZEROS
     C******** 21KEY4      SETLLDBRCDF               96
     C********
     C******************** ELSE                            KKCLM#=0
     C******** 21KEY1      SETLLDBRCDF               96
     C******************** ENDIF                           KKCLM#<>0
     C********************
     C******************** ENDIF                           KKFYR<>0
     C********************
     C* * END   ADD *** 01/28/98 ***********************************************
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
     C     SSFYR         IFNE      0
     C                   Z-ADD     SSFYR         KKFYR
     C                   ENDIF                                                  SSFYR IFNE 0
      *
     C   21KEY3          SETLL     DBRCDF
      **** END   ADD **** 07/23/98 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C* * BEGIN ADD *** 01/28/98 ***********************************************
     C                   MOVE      SCFYR         @@FYR
     C                   MOVE      SCCLM#        @@CLM#
     C* * END   ADD *** 01/28/98 ***********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
      **** BEGIN DEL **** 07/23/98 *********************************************
     C********** SSSTAT    IFNE *BLANKS
     C********** SSSTAT    ANDNEMCSTAT
     C******************** MOVE *OFF      MEETSS
     C******************** ENDIF
      **** END   DEL **** 07/23/98 *********************************************
      **** BEGIN ADD **** 07/23/98 *********************************************
     C     SSFYR         IFNE      0
     C     SSFYR         ANDNE     MCFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      **** END   ADD **** 07/23/98 *********************************************
      /free
       //**** beg add ******** 05/16/13 ****************************************
       If intClm# <> 0 and intClm# <> MCclm#;
          meetSs = *off;
       Endif;
       //**** end add ******** 05/16/13 ****************************************
      /end-free
     C*
     C                   ENDSR
** Headers - HD
            Multiple
Opt   Year   Claim#       Case#   Claimant name
** Description--------------MISN
1011=Select                 NNY
203F3=Exit                  YYN
212F12=Cancel               YYY
217F17=Subset               YYY
3
