     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE E0020R - Work With Employers
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: Menu                                               *
     H*                                                               *
     H* CALLS TO:  E0020R02 - MAINT Employers                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *
     FE0020F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FEMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FEMEML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEM01:DBRCDF)
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Employers')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('E0020R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D DELETD          C                   CONST('*Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     COKY          KLIST
     C                   KFLD                    ##CO#
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
      *
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    EMNAME        KKNAME
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMMAD1        XXADR1
     C     *LIKE         DEFINE    EMNAME        U1PDSC
     C     *LIKE         DEFINE    EMNAME        U2PDSC
      *
     C/COPY SCOPYWW01C
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C     SSFND         IFNE      *ZEROS
     C     SSNAME        ORNE      *BLANKS
     C     SSMAD1        ORNE      *BLANKS
     C     SSEMP#        ORNE      *ZEROS
     C     SSSTAT        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF
     C                   ENDSR
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   Z-ADD     0             SSFND
     C                   Z-ADD     0             SSEMP#
     C                   ENDSR
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   Z-ADD     *ZERO         KKFND
     C                   Z-ADD     *ZERO         KKEMP#
      *
     C                   ENDSR
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
      * Prevent keyset values from interfering with subset values
      *
     C                   Z-ADD     *HIVAL        KKFND
     C                   Z-ADD     *HIVAL        KKEMP#
     C                   MOVE(P)   SCPST1        XXFND             3 0
     C     EMKY          SETGT     DBRCDF
     C                   READP     DBRCDF                                 60
     C     SSFND         IFNE      *ZEROS
     C     XXFND         ANDNE     SSFND
     C                   Z-ADD     SSFND         KKFND
     C                   ELSE
     C                   Z-ADD     XXFND         KKFND
     C                   ENDIF
      *
     C     KKFND         IFEQ      *ZEROS
     C                   READ      DBRCDF                                 60
     C                   Z-ADD     EMFND         KKFND
     C                   ENDIF
      *
     C     EMKY          SETGT     DBRCDF
     C                   READP     DBRCDF                                 60
      *
     C     SSEMP#        IFNE      *ZEROS
     C     SCPSTA        ANDNE     SSEMP#
     C                   Z-ADD     SSEMP#        KKEMP#
     C                   ELSE
     C                   Z-ADD     SCPSTA        KKEMP#
     C                   ENDIF
      *
     C     KKEMP#        IFGT      EMEMP#
     C                   Z-ADD     EMEMP#        KKEMP#
     C                   ENDIF
      *
     C                   ENDSR
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
      *
     C                   ENDSR
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   MOVEL     EMNAME        SCNAME
     C                   MOVEL     EMSTAT        SCSTAT
      *
     C                   ENDSL
      *
     C                   ENDSR
      *================================================================
      * OPT8 - SUBROUTINE FOR OPTION 08 WORK WITH BATCHES
      *================================================================
     C     OPT8          BEGSR
      *
     C                   CALL      'E0040R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR
      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20 CHANGE STATUS
      *================================================================
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '8'           OPT8
     C                   ENDCS
     C                   ENDSR
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
     C                   ENDSR
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21##CO#         READE     DBRCDF                                 95
     C                   ENDSR
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21##CO#         READPE    DBRCDF                                 95
     C                   ENDSR
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21EMKY          SETGT     DBRCDF                             96
     C                   ENDSR
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21EMKY          SETLL     DBRCDF                             96
     C                   ENDSR
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   MOVEL(P)  DELETD        SCNAME
     C                   ENDSR
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C     Q1PRMS        IFGE      1
     C                   MOVE      *INKC         @@KC
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      2
     C                   MOVE      *INKL         @@KL
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      4
     C                   MOVEL     SCFND         @@FND
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      5
     C                   MOVEL     SCEMP#        @@EMP#
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================
      * STRING - SUBROUTINE TO SEARCH STRINGS
      *===============================================================
      *
     C     STRING        BEGSR
      *
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
      *
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
      *
     C                   ENDSR                                                  STRING
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C     @@FND         ANDNE     *ZERO
     C     @@FND         ANDNE     EMFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  @@CALL
      *
     C     SSFND         IFNE      *ZERO
     C     SSFND         ANDNE     EMFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND
      *
      *  CHECK EMPLOYER # AGAINST SEARCH STRING
      *
     C     SSEMP#        IFNE      *ZEROS
     C     SSEMP#        ANDNE     EMEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSNAME<>*ZEROS
      *
      *  CHECK EMPLOYERS DBA AGAINST SEARCH STRING
      *
     C     SSNAME        IFNE      *BLANKS
      *
     C     LO:UP         XLATE(P)  EMNAME        U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
      *
     C                   EXSR      STRING
      *
     C                   ENDIF                                                  SSNAME<>*BLANKS
      *
      *  CHECK EMPLOYERS PHYSICAL STREET ADDR 1 AGAINST SEARCH STRING
      *
     C     SSMAD1        IFNE      *BLANKS
      *
     C     LO:UP         XLATE(P)  EMMAD1        U1PDSC
     C     LO:UP         XLATE(P)  SSMAD1        U2PDSC
      *
     C                   EXSR      STRING
      *
     C                   ENDIF                                                  SSPAD1<>*BLANKS
      *
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     EMSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTAT
      *
     C                   ENDSR
** Column headings
Opt  Grp  Employer#   Employer Name              Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1088=Batches                YYN
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
