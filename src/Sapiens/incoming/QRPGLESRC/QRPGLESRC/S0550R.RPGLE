     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  S0550R - WORK WITH GROUPS DEPARTMENT CODES   C:62 05/14/98   *
     H*                                                               *
     H*  CALLED FROM: MANY PLACES                                     *
     H*  CALLS TO:                                                    *
     H*               S0550R01                                        *
     H*               S0555R                                          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/30/98 C:62 PROJ:MC184                                     *
     H*                ADDED LETTER TYPE.                             *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FS0550F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMDEP     IF   E             DISK
     F                                     RENAME(PDE01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMDEL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PDE01:DBRCDF)
     F*** BEGIN ADD *** 11/30/98 **************************************
     FSTGTL     IF   E           K DISK
     FSTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END   ADD *** 11/30/98 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Department codes')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('S0550R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)

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
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*
     C*  WW Documents parm list.
     C*
     C     DOCPL         PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDPCD
     C*** BEGIN ADD *** 11/30/98 **************************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END   ADD *** 11/30/98 **************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DECO#         KKCO#
     C     *LIKE         DEFINE    DEDPCD        KKDPCD
     C*** BEGIN ADD *** 11/30/98 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END   ADD *** 11/30/98 **************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSDLTY        ORNE      *BLANKS
     C*
     C                   MOVE      *ON           *IN11
     C*
     C                   ENDIF
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
     C                   SETOFF                                       3132
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C*
     C                   SETON                                        8990
     C*
     C                   ENDIF
     C*
     C  N90              MOVEL(P)  *BLANKS       SSLDSC
     C  N90SSDLTY        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SSDLTY        KKKEY
     C                   MOVEL(P)  'DLTY'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SSLDSC
     C   99              SETON                                        8890
     C*
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
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL     *BLANKS       KKDPCD
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKDPCD
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCDPCD        KKDPCD
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** LETTER TYPE SEARCH.
     C*
     C     HFLD          IFEQ      'SSDLTY'
     C*
     C                   CALL      'S2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'DLTY'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        9031
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C                   MOVEL(P)  GTKEY         SSDLTY
     C                   MOVEL(P)  GTDESC        SSLDSC
     C                   SETON                                        9032
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  DEDPCD        SCDPCD
     C*
     C                   MOVEL(P)  DEDPCD        SCDATA
     C                   MOVE      DENAME        SCDATA
     C*** BEGIN ADD *** 11/30/98 **************************************
     C                   MOVEL(P)  DEDLTY        SCDLTY
     C                   MOVEL(P)  DEDLTY        KKKEY
     C                   MOVEL(P)  'DLTY'        KKTBID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCLDSC
     C   99              MOVEL(P)  *BLANKS       SCLDSC
     C*
     C*** END   ADD *** 11/30/98 **************************************
     C*
     C                   MOVEL     DESTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - DOCUMENTS
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   CALL      'S0555R'      DOCPL
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
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY2          SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY2          SETLL     DBRCDF                             96
     C*
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
     C     SSDLTY        IFNE      *BLANKS
     C     SSDLTY        ANDNE     DEDLTY
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     DESTAT
     C*
     C                   MOVE      *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Dept    Description                               Letter type       Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
1077=Document               YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               NNY
217F17=Subset               YYY
3
