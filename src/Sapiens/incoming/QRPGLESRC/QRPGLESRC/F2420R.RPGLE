     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F2420R - WORK WITH SUBSCRIBER STAFF CODES   C:62 12/29/97    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*  CALLS TO:                                                    *
     H*               F2420R01 - SUBSCRIBER STAFF CODE MAINTENANCE    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               * *
     H*  10/09/00 C:62 PROJECT:MC035 24260                            * *
     H*                IF SUBSET TO SUB#, CHANGE KEYS USED.           * *
     H*                                                               * *
     H*  09/11/00 C:62 PROJECT:MC205                                  * *
     H*                -ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.* *
     H*                -ADD F21=PRINT LIST                            * *
     H*                                                               *
     H*  04/26/00 C:062 PROJECT: MC098 22686                          *
     H*                 DEFAULT SUBSET TO USER PARM VALUE.            *
     H*                                                               *
     H*  12/02/99 C:117 PROJECT: MC035 20767                          *
     H*                 CHANGE F20=CHANGE STATUS TO 20=CHANGE STATUS  *
     H*                 CHANGE MADE IN ARRAY AT BOTTOM OF PGM         *
     H*                                                               *
     H*  11/23/99 C:62  PROJECT: MC035 20726                          *
     H*                 ADD SUBSET FOR SUBSCRIBER NUMBER.             *
     H*                                                               *
     H*  04/13/99 C:56  CHANGE  COMPANY API TO FA007R PER MIKE FLOWER *
     H*                 FA011R WAS A DUPLICATE OF FA007R              *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF2420F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMSFP     IF   E             DISK
     F                                     RENAME(PSF01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMSFL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSF01:DBRCDF)
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 11/23/99 **************************************
     FFMSMP     IF   E             DISK
     F                                     RENAME(PSM01:PSM02)
     F*** END   ADD *** 11/23/99 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Subscriber Staff-
     D                                      Codes')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2420R01')
     D*** BEG CHG *** 04/13/99 ****************************************
     D**********    'FA011R'              C         COMAPI
     D COMAPI          C                   CONST('FA007R')
     D*** END CHG *** 04/13/99 ****************************************
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D*** BEGIN ADD *** 09/11/00 **************************************
     D FLAG            C                   CONST(' 1       ')

     I*** END   ADD *** 09/11/00 **************************************

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
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN DEL *** 11/23/99 **************************************
     C**********           KFLD           ##SUB#
     C*** END   DEL *** 11/23/99 **************************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C*** BEGIN CHG *** 11/23/99 **************************************
     C**********           KFLD           ##SUB#
     C                   KFLD                    KKSUB#
     C*** END   CHG *** 11/23/99 **************************************
     C                   KFLD                    KKSTAF
     C*** BEGIN ADD *** 10/09/00 **************************************
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C                   KFLD                    KKSTAF
     C*** END   ADD *** 10/09/00 **************************************
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*
     C     SMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SSSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*** BEGIN ADD *** 11/23/99 **************************************
     C     *LIKE         DEFINE    SFSUB#        KKSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C     *LIKE         DEFINE    SFSTAF        KKSTAF
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
     C*** BEGIN ADD *** 11/23/99 **************************************
     C     SSSUB#        ORNE      *ZEROS
     C*** END   ADD *** 11/23/99 **************************************
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
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
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*
     C     *INKJ         IFEQ      *ON
     C     *IN90         ANDEQ     *OFF
     C*
     C                   EXSR      KJDO
     C*
     C                   ENDIF
     C*
     C* VALIDATE SUBSCRIBER
     C*
     C  N90SSSUB#        IFNE      *ZEROS
     C*
     C     SMKY2         CHAIN     PSM01                              99
     C*
     C   99              SETON                                        8890
     C  N99              MOVEL(P)  SMNAME        SSSBNM
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSSBNM
     C*
     C                   ENDIF
     C*** END   ADD *** 11/23/99 **************************************
     C*
     C  N90SSSTAT        IFNE      ' '
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
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C                   Z-ADD     ##SUB#        SSSUB#
     C*
     C*** END   ADD *** 04/26/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*** BEGIN ADD *** 11/23/99 **************************************
     C                   Z-ADD     *ZEROS        KKSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C                   MOVE      *LOVAL        KKSTAF
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*** BEGIN ADD *** 11/23/99 **************************************
     C   21              MOVE      SCPST1        KKSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C*** BEGIN CHG *** 11/23/99 **************************************
     C***21*****           MOVELSCPST1    KKSTAF    P
     C   21              MOVEL(P)  SCPSTS        KKSTAF
     C*** END   CHG *** 11/23/99 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C*** BEGIN ADD *** 11/23/99 **************************************
     C   21              Z-ADD     SCSUB#        KKSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C*** BEGIN CHG *** 11/23/99 **************************************
     C***21*****           MOVELSFSTAF    KKSTAF    P
     C   21              MOVEL(P)  SCSTAF        KKSTAF
     C*** BEGIN CHG *** 11/23/99 **************************************
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C     HFLD          IFEQ      'SSSUB#'
     C*
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      '1'           OUTFLG
     C*                    SETON                     90
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PSM02                              99
     C                   Z-ADD     SMSUB#        SSSUB#
     C                   MOVEL(P)  SMNAME        SSSBNM
     C                   SETON                                        9087
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 11/23/99 **************************************
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         ##CO
     C                   MOVEL(P)  FLAG          ##FLAG
     C*
     C                   CALL      'F7050C'
     C                   PARM                    ##CO              3
     C                   PARM                    ##FLAG            9
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/11/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*** BEGIN ADD *** 11/23/99 **************************************
     C                   Z-ADD     SFSUB#        SCSUB#
     C*** END   ADD *** 11/23/99 **************************************
     C                   MOVEL     SFSTAF        SCSTAF
     C*
     C   21              MOVEL(P)  SFSTAF        SCDATA
     C   21              MOVE      SFNAME        SCDATA
     C*
     C                   MOVEL     SFSTAT        SCSTAT
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
     C*** BEGIN ADD *** 09/11/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 09/11/00 **************************************
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
     C*** BEGIN DEL *** 11/23/99 **************************************
     C********** SMKY      CHAINPSM01                99
     C*
     C**********           MOVELSMNAME    ##DATA
     C*
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 40        ##LENG  30
     C*
     C**********           MOVEL##DATA    SCSBNM
     C*** END   DEL *** 11/23/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   SELECT
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY3          READE     DBRCDF                                 95
     C*
     C                   OTHER
     C*** END   ADD *** 10/09/00 **************************************
     C   21KEY1          READE     DBRCDF                                 95
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   ENDSL
     C*** END   ADD *** 10/09/00 **************************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   SELECT
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY3          READPE    DBRCDF                                 95
     C*
     C                   OTHER
     C*** END   ADD *** 10/09/00 **************************************
     C   21KEY1          READPE    DBRCDF                                 95
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   ENDSL
     C*** END   ADD *** 10/09/00 **************************************
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   SELECT
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY4          SETGT     DBRCDF                             96
     C*
     C                   OTHER
     C*** END   ADD *** 10/09/00 **************************************
     C   21KEY2          SETGT     DBRCDF                             96
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   ENDSL
     C*** END   ADD *** 10/09/00 **************************************
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   SELECT
     C*
     C     SSSUB#        WHENNE    *ZEROS
     C   21KEY4          SETLL     DBRCDF                             96
     C*
     C                   OTHER
     C*** END   ADD *** 10/09/00 **************************************
     C   21KEY2          SETLL     DBRCDF                             96
     C*** BEGIN ADD *** 10/09/00 **************************************
     C                   ENDSL
     C*** END   ADD *** 10/09/00 **************************************
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
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*** BEGIN ADD *** 11/23/99 **************************************
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SSSUB#        ANDNE     SFSUB#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 11/23/99 **************************************
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     SFSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Sub # Staff   Name                                                     Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
