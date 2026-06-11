     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE W2525R - Charge Master Service Code Table
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* Work with program for Charge Master Service Code Table        *
     H*                                                               *
     H* CALLED BY: F0601R                                             *
     H*                                                               *
     H* CALLS TO:  F2525R01, F7054C                                   *
     H*                                                               *
     H* CREATED: 06/28/2001                                           *
     H*                                                               *
     H* 06/28/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disabled option 5 and function 17 for GS
     H*                                                               *
     H* 11/18/08 C:198 PROJECT PA046                                  *
     H*                Added NPI# after the medicare# on the screen   *
     H*                SCR01.                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  71       DATE MUST BE VALID IN PRINT SCREEN                  *
     H*  72       DATE MUST BE VALID IN SUBSET SCREEN                 *
     H*  73       STATUS MUST BE 'A','I' OR ' ' IN SUBSET SCREEN      *
     H*                                                               *
     H*****************************************************************
     H*
     FF2525F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFACGPPA   IF   E             DISK
     F                                     RENAME(PCG01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFACGL4PA  IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCG01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFACGL5PA  IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PCG01:DBRCDF2)
     F*
     F*  Tertiary logical file definition (for displaying items)
     FFACGL1PA  IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PCG01:DBRCDF3)
     F*
     F*  SUBSET logical file definition (for displaying items)
     FFACGL7PA  IF   E           K DISK
     F                                     INFDS(DBFDSA)
     F                                     RENAME(PCG01:DBRCDFA)
     FSTABL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             79    DIM(3) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Charge Master Table')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2525R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(3)
     D SF1PAG          C                   CONST(10)
     C*
     C/COPY SCOPYWW01C

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@MED#           10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
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
     C*
     C     KEYA          KLIST
     C                   KFLD                    SSMED#
     C*
     C     V1KY          KLIST
     C                   KFLD                    KKSVCD
     C                   KFLD                    KKEFDT
     C*
     C     V2KY          KLIST
     C                   KFLD                    KKDESC
     C                   KFLD                    KKSVCD
     C                   KFLD                    KKMED#
     C                   KFLD                    KKEFDT
     C*
     C     V3KY          KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKSVCD
     C                   KFLD                    KKEFDT
     C*
     C     V1KYA         KLIST
     C                   KFLD                    SSMED#
     C                   KFLD                    KKSVCD
     C                   KFLD                    KKEFDT
     C*
     C     V2KYA         KLIST
     C                   KFLD                    SSMED#
     C                   KFLD                    KKDESC
     C                   KFLD                    KKSVCD
     C                   KFLD                    KKEFDT
     C*
     C     V3KYA         KLIST
     C                   KFLD                    SSMED#
     C                   KFLD                    KKSVCD
     C                   KFLD                    KKEFDT
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CGMED#        KKMED#
     C     *LIKE         DEFINE    CGDESC        KKDESC
     C     *LIKE         DEFINE    CGSVCD        KKSVCD
     C     *LIKE         DEFINE    CGEFDT        KKEFDT
     C     *LIKE         DEFINE    CGEFDT        S1EFDT
     C*
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C*
     C     SSMED#        WHENNE    *BLANKS
     C*
     C   21              Z-ADD     DBRRN3        SCDBRR
     C   22              Z-ADD     DBRRNA        SCDBRR
     C   23              Z-ADD     DBRRN3        SCDBRR
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*** END   ADD *** 11/02/98 **************************************
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSEFDT        ORNE      *ZEROS
     C     SSMED#        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   MOVE      'Y'           VLDOPT            1
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        72  90
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C*
     C                   ENDIF                                                  IF SRERR = 1
     C                   ENDIF                                                  IF 90 OFF
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        73  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCKP - Error checking for the print prompt screen
     C*================================================================
     C     ERRCKP        BEGSR
     C*
     C     *IN90         IFEQ      *OFF
     C     SPEFDT        ANDNE     *ZEROS
     C                   MOVEL     SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SPEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        71  90
     C                   ENDIF                                                  IF SRERR 1
     C                   ENDIF                                                  IF 90 OFF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVEL(P)  'A'           SSSTAT
     C                   ENDIF
     C*
     C     Q1PRMS        IFGT      5
     C     @@MED#        IFNE      *BLANKS
     C                   MOVEL(P)  @@MED#        SSMED#
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKMED#
     C                   MOVE      *LOVAL        KKDESC
     C                   MOVE      *LOVAL        KKSVCD
     C                   MOVE      *LOVAL        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL(P)  SCPST1        KKSVCD
     C   21              MOVEL(P)  *LOVAL        KKEFDT
     C*
     C   22              MOVEL(P)  SCPST2        KKDESC
     C   22              MOVEL(P)  *LOVAL        KKSVCD
     C   22              MOVEL(P)  *LOVAL        KKMED#
     C   22              MOVEL     *LOVAL        KKEFDT
     C*
     C   23              MOVEL(P)  SCPST3        KKMED#
     C   23              MOVEL(P)  SCPSTS        KKSVCD
     C   23              MOVEL     *LOVAL        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C   21              MOVEL(P)  SCSVCD        KKSVCD
     C   22              MOVEL(P)  SCDESC        KKDESC
     C   22              MOVEL(P)  SCSVCD        KKSVCD
     C   22              MOVEL(P)  SCMED#        KKMED#
     C   23              MOVEL(P)  SCMED#        KKMED#
     C   23              MOVEL(P)  SCSVCD        KKSVCD
     C                   Z-ADD     SCEFDT        KKEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   MOVEL     *BLANKS       SPMED#
     C                   Z-ADD     *ZEROS        SPEFDT
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   EXSR      ERRCKP
     C*
     C                   MOVE      SPEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         #1EFDT            7
     C*
     C     *IN90         IFEQ      *OFF
     C                   MOVEL     ##CO#         #1CO
     C                   CALL      'F7054C'
     C                   PARM                    #1CO              3
     C                   PARM      SPMED#        #1MED#           10
     C                   PARM                    #1EFDT
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C                   PARM      @@ST          ##ST
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
      *
      * DISPLAY STATE CODE AND STATE NAME IN HEADER
      *
     C                   MOVEL(P)  @@ST          KKST
     C                   MOVEL     @@ST          SCST
      *
     C     STKY          CHAIN     PST01                              99
      *
     C                   MOVEL(P)  STNAME        SCSTNM
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     VLDOPT        IFEQ      'N'
     C                   EXSR      ASSRRN
     C                   ENDIF                                                  VLDOPT<>Y
     C*
     C                   MOVEL(P)  CGSVCD        SCSVCD
     C                   MOVEL(P)  CGDESC        SCDESC
     C                   MOVEL(P)  CGMED#        SCMED#
     C*** BEG ADD *** 11/18/08 ************************************************
     C                   MOVEL(P)  CGNPI#        SCNPI#
     C*** END ADD *** 11/18/08 ************************************************
     C                   Z-ADD     CGEFDT        SCEFDT
     C*
     C                   Z-ADD     CGEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD1
     C*
     C                   MOVEL     CGSTAT        SCSTAT
     C*
     C                   MOVEL(P)  CGSVCD        XXSVCD           15
     C*** BEG CHG *** 11/18/08 ************************************************
     C***********          MOVELCGDESC    XXDESC 30 P
     C                   MOVEL(P)  CGDESC        XXDESC           17
     C*** END CHG *** 11/18/08 ************************************************
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C*
     C                   MOVEL(P)  XXSVCD        XXDATA           27
     C                   MOVE      CGMED#        XXDATA
     C                   MOVEL(P)  XXDATA        SCDATA
     C*** BEG ADD *** 11/18/08 *************************************************
     C                   MOVEL(P)  SCNPI#        XXDTA            30
     C                   MOVE      XXDESC        XXDTA
     C*** END ADD *** 11/18/08 *************************************************
     C*** BEG CHG *** 11/18/08 ************************************************
     C***********          MOVE XXDESC    SCDATA
     C                   MOVE      XXDTA         SCDATA
     C*** END CHG *** 11/18/08 ************************************************
     C*
     C     *IN22         WHENEQ    *ON
     C*
     C*** BEG CHG *** 11/18/08 ************************************************
     C*********            MOVELXXSVCD    XXDATA    P
     C*********            MOVE CGMED#    XXDATA
     C                   MOVEL(P)  XXSVCD        XXDAT1           41
     C                   MOVEL(P)  CGMED#        XXDTA1           25
     C*** END CHG *** 11/18/08 ************************************************
     C*** BEG ADD *** 11/18/08 *************************************************
     C                   MOVE      SCNPI#        XXDTA1
     C                   MOVE      XXDTA1        XXDAT1
     C*** END ADD *** 11/18/08 *************************************************
     C                   MOVEL(P)  XXDESC        SCDATA
     C*** BEG CHG *** 11/18/08 ************************************************
     C*********            MOVE XXDATA    SCDATA
     C                   MOVE      XXDAT1        SCDATA
     C*** END CHG *** 11/18/08 ************************************************
     C*
     C     *IN23         WHENEQ    *ON
     C*
     C*** BEG CHG *** 11/18/08 *************************************************
     C********             MOVELCGMED#    XXDATA    P
     C********             MOVE XXSVCD    XXDATA
     C********             MOVELXXDATA    SCDATA    P
     C                   MOVEL(P)  CGMED#        XXDAT2           40
     C                   MOVEL(P)  SCNPI#        XXDTA2           29
     C                   MOVE      XXSVCD        XXDTA2
     C                   MOVE      XXDTA2        XXDAT2
     C                   MOVEL(P)  XXDAT2        SCDATA
     C*** END CHG *** 11/18/08 ************************************************
     C                   MOVE      XXDESC        SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
     C*================================================================
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKV         CASEQ     *ON           KVDO
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
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*
     C*** Beg Add *** MC367 *** 11/19/12 *****************************
      /Free
      /If Not Defined(WEB)
       //Disable option 05
           XXOpfn = '1';
           XXMIS = 'NNN';
           XXNmbr = '05';
           Exsr UPDOFA;
       //Disable Function 17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/19/12 *****************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C                   MOVE      'N'           VLDOPT
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C                   SELECT
     C     SSMED#        WHENNE    *BLANKS
     C   21KEYA          READE     DBRCDF3                                95
     C   22KEYA          READE     DBRCDFA                                95
     C   23KEYA          READE     DBRCDF3                                95
     C                   OTHER
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C                   MOVE      'N'           VLDOPT
     C                   SELECT
     C     SSMED#        WHENNE    *BLANKS
     C   21KEYA          READPE    DBRCDF3                                95
     C   22KEYA          READPE    DBRCDFA                                95
     C   23KEYA          READPE    DBRCDF3                                95
     C                   OTHER
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C                   SELECT
     C     SSMED#        WHENNE    *BLANKS
     C   21V1KYA         SETGT     DBRCDF3                            96
     C   22V2KYA         SETGT     DBRCDFA                            96
     C   23V3KYA         SETGT     DBRCDF3                            96
     C                   OTHER
     C   21V1KY          SETGT     DBRCDF                             96
     C   22V2KY          SETGT     DBRCDF2                            96
     C   23V3KY          SETGT     DBRCDF3                            96
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C                   SELECT
     C     SSMED#        WHENNE    *BLANKS
     C   21V1KYA         SETLL     DBRCDF3                            96
     C   22V2KYA         SETGT     DBRCDFA                            96
     C   23V3KYA         SETGT     DBRCDF3                            96
     C                   OTHER
     C   21V1KY          SETLL     DBRCDF                             96
     C   22V2KY          SETGT     DBRCDF2                            96
     C   23V3KY          SETGT     DBRCDF3                            96
     C                   ENDSL
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
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSMED#        IFNE      *BLANKS
     C     SSMED#        ANDNE     CGMED#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     CGEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     CGSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt Service Code     Medicare#   NPI#         Description         Eff. Date Sts
Opt Description       Service Code    Medicare#    NPI#           Eff. Date Sts
Opt Medicare#  NPI#          Service Code     Description         Eff. Date Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
12020=Change status         YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YNY
217F17=Subset               YYY
221F21=Print list           YYN
3
