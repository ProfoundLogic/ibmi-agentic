     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2531R - FEIN/Med# Table
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H* Work With Program for FEIN/Med# Table                         *
     H*                                                               *
     H* CALLED BY: F0601R                                             *
     H* CALLS TO : F2531R01, F7072C                                   *
     H*                                                               *
     H*****************************************************************
     H* Created 08/16/2001 by C:62 for Project:114MCG                 *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/15/13 C:079 Project: SY266_CVT                            *
     H*                 Initialize RRN                                *
     H*                                                               *
     H*  11/19/12 C:287 Project: MC367                                *
     H*                 Disabled option 5 and function 17 for GS      *
     H*                                                               *
     H* 11/21/02 C:79 Project: NY006                                  *
     H*               Change heading for medicare# to be variable     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99       GENERAL READ/CHAIN                                  *
     H*  71       STATUS MUST BE 'A','I' OR ' ' IN SUBSET SCREEN      *
     H*                                                               *
     H*****************************************************************
     FF2531F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMFMP     IF   E             DISK
     F                                     RENAME(PFM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMFML1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PFM01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMFML2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PFM01:DBRCDF2)
     FSTABL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D LIN             S              1    DIM(53)                              Line item build
     D*** BEG CHG *** 11/21/02 ****************************************
     D******************* HD      1   2 78               Column headings
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
     D*** END CHG *** 11/21/02 ****************************************
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('FEIN/Med# Table')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2531R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@FEIN            9 0
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
      *
     C     V1KY          KLIST
     C                   KFLD                    KKNAME
     C                   KFLD                    KKMED#
     C                   KFLD                    KKFEIN
      *
     C     V2KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKNAME
     C                   KFLD                    KKMED#
      *
     C     V3KYA         KLIST
     C                   KFLD                    SSFEIN
      *
     C     V3KYB         KLIST
     C                   KFLD                    SSFEIN
     C                   KFLD                    KKNAME
     C                   KFLD                    KKMED#
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    FMMED#        KKMED#
     C     *LIKE         DEFINE    FMNAME        KKNAME
     C     *LIKE         DEFINE    FMFEIN        KKFEIN
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* ASSRRN - ASSIGN VALUE TO SCREEN RRN.
     C*================================================================
     C     ASSRRN        BEGSR
     C*
     C                   SELECT
     C     SSFEIN        WHENNE    *ZEROS
     C   21              Z-ADD     DBRRN2        SCDBRR
     C   22              Z-ADD     DBRRN2        SCDBRR
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSSTAT        ANDNE     *BLANKS
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
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        71  90
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C     Q1PRMS        IFGT      5
     C                   Z-ADD     @@FEIN        SSFEIN
     C                   ENDIF
     C*
     C*** BEG ADD *** 11/21/02 ****************************************
     C     VIEW#         IFEQ      1
     C     @@ST          ANDEQ     'NY'
     C*
     C                   MOVEL(P)  HD(3)         SCHDL1
     C*
     C                   ENDIF
     C*
     C     VIEW#         IFEQ      2
     C     @@ST          ANDEQ     'NY'
     C*
     C                   MOVEL(P)  HD(4)         SCHDL1
     C*
     C                   ENDIF
     C*** END ADD *** 11/21/02 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKMED#
     C                   MOVE      *LOVAL        KKNAME
     C                   MOVE      *LOVAL        KKFEIN
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C   21              MOVEL(P)  SCPST1        KKNAME
     C   21              MOVE      *LOVAL        KKMED#
     C   21              MOVE      *LOVAL        KKFEIN
     C   22              MOVE      SCPST2        KKFEIN
     C   22              MOVE      *LOVAL        KKNAME
     C   22              MOVE      *LOVAL        KKMED#
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCNAME        KKNAME
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   MOVEL(P)  SCMED#        KKMED#
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVEL     ##CO#         #1CO
     C*
     C                   CALL      'F7072C'
     C                   PARM                    #1CO              3
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C                   PARM      @@ST          ##ST
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL(P)  @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
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
     C                   MOVEL(P)  FMNAME        SCNAME
     C                   Z-ADD     FMFEIN        SCFEIN
     C                   MOVEL     FMMED#        SCMED#
     C                   Z-ADD     FMZIP         SCZIP
     C                   MOVEL(P)  FMSTAT        SCSTAT
     C                   MOVEL     FMNAME        XXNAME           30
     C                   MOVE      FMFEIN        XXFEIN            9
     C*
     C                   CLEAR                   LIN
     C*
     C     *IN21         IFEQ      *ON
     C                   MOVEA     XXNAME        LIN(1)
     C                   MOVEA     FMMED#        LIN(33)
     C                   MOVEA     XXFEIN        LIN(45)
     C                   ENDIF
     C*
     C     *IN22         IFEQ      *ON
     C                   MOVEA     XXFEIN        LIN(1)
     C                   MOVEA     XXNAME        LIN(12)
     C                   MOVEA     FMMED#        LIN(44)
     C                   ENDIF
     C*
     C                   MOVEA     LIN           SCDATA
     C*
     C*** BEG ADD *** 11/21/02 ****************************************
     C     VIEW#         IFEQ      1
     C     @@ST          ANDEQ     'NY'
     C*
     C                   MOVEL(P)  HD(3)         SCHDL1
     C*
     C                   ENDIF
     C*
     C     VIEW#         IFEQ      2
     C     @@ST          ANDEQ     'NY'
     C*
     C                   MOVEL(P)  HD(4)         SCHDL1
     C*
     C                   ENDIF
     C*** END ADD *** 11/21/02 ****************************************
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
     C*** Beg Add *** 01/15/13 *********************************************
     C                   EVAL      DBRRN=0
     C                   EVAL      DBRRN2=0
     C                   EVAL      DBRRN3=0
     C*** End Add *** 01/15/13 *********************************************
     C                   SELECT
     C     SSFEIN        WHENNE    *ZEROS
     C   21V3KYA         READE     DBRCDF2                                95
     C   22V3KYA         READE     DBRCDF2                                95
     C                   OTHER
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C                   MOVE      'N'           VLDOPT
     C                   SELECT
     C     SSFEIN        WHENNE    *ZEROS
     C   21V3KYA         READPE    DBRCDF2                                95
     C   22V3KYA         READPE    DBRCDF2                                95
     C                   OTHER
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C                   SELECT
     C     SSFEIN        WHENNE    *ZEROS
     C   21V3KYB         SETGT     DBRCDF2                            96
     C   22V3KYB         SETGT     DBRCDF2                            96
     C                   OTHER
     C   21V1KY          SETGT     DBRCDF                             96
     C   22V2KY          SETGT     DBRCDF2                            96
     C                   ENDSL
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C                   SELECT
     C     SSFEIN        WHENNE    *ZEROS
     C   21V3KYB         SETLL     DBRCDF2                            96
     C   22V3KYB         SETLL     DBRCDF2                            96
     C                   OTHER
     C   21V1KY          SETLL     DBRCDF                             96
     C   22V2KY          SETLL     DBRCDF2                            96
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
     C                   Z-ADD     SCDBRR        @@RRN
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     FMSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Name                            Medicare#   FEIN       Zip Code  Sts
Opt  FEIN       Name                            Medicare#   Zip Code  Sts
Opt  Name                            Opcert      FEIN       Zip Code  Sts
Opt  FEIN       Name                            OpCert#     Zip Code  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
