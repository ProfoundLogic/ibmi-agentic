     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* Work with Home Health Procedures                              *
     H*                                                               *
     H*****************************************************************
     H* Created on 04/04/2008 by c:62 for Project TX040               *
     H*****************************************************************
     H* Revisions:                                                    *
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disabled option 5 and function 17 for GS
     H*                                                               *
     H*****************************************************************
     FF2546F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMHHP     IF   E             DISK
     F                                     RENAME(PHH01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMHHL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PHH01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             75    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(14) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(14) ALT(OF)
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
     D OPTNM           C                   CONST('Home Health -
     D                                     Procedures')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2546R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)

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
     C*  CONVERTS DATE FROM MDYY TO CYMD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKFAGE
     C                   KFLD                    KKTAGE
     C                   KFLD                    KKMOD1
     C                   KFLD                    KKMOD2
     C                   KFLD                    KKTOS
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    HHPRCD        KKPRCD
     C     *LIKE         DEFINE    HHEFDT        KKEFDT
     C     *LIKE         DEFINE    HHFAGE        KKFAGE
     C     *LIKE         DEFINE    HHTAGE        KKTAGE
     C     *LIKE         DEFINE    HHMOD1        KKMOD1
     C     *LIKE         DEFINE    HHMOD2        KKMOD2
     C     *LIKE         DEFINE    HHTOS         KKTOS
     C     *LIKE         DEFINE    HHDESC        U1PDSC
     C     *LIKE         DEFINE    HHDESC        U2PDSC
     C     *LIKE         DEFINE    HHEFDT        S1EFDT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSSTAT        IFNE      *BLANKS
B01  C     SSEFDT        ORNE      *ZEROS
B01  C     SSDESC        ORNE      *BLANKS
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
     C*
     C     *IN90         IFEQ      *OFF
     C     SSEFDT        ANDNE     *ZEROS
     C*
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        8890
     C                   ELSE
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SSSTAT        ANDNE     ' '
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
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKPRCD
     C                   Z-ADD     *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKFAGE
     C                   MOVE      *LOVAL        KKTAGE
     C                   MOVE      *LOVAL        KKMOD1
     C                   MOVE      *LOVAL        KKMOD2
     C                   MOVE      *LOVAL        KKTOS
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL     SCPST1        KKPRCD
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   Z-ADD     SCFAGE        KKFAGE
     C                   Z-ADD     SCTAGE        KKTAGE
     C                   MOVEL     SCMOD1        KKMOD1
     C                   MOVEL     SCMOD2        KKMOD2
     C                   MOVEL     SCTOS         KKTOS
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL     SCPRCD        KKPRCD
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   Z-ADD     SCFAGE        KKFAGE
     C                   Z-ADD     SCTAGE        KKTAGE
     C                   MOVEL     SCMOD1        KKMOD1
     C                   MOVEL     SCMOD2        KKMOD2
     C                   MOVEL     SCTOS         KKTOS
     C*
     C                   ENDSR
     C*================================================================
     C* KVDO - F21=Print list
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *OFF
     C                   MOVE      ##CO#         #1CO
     C                   CALL      'F7088C'
     C                   PARM                    #1CO              3
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     HHFAGE        SCFAGE
     C                   Z-ADD     HHTAGE        SCTAGE
     C                   MOVEL     HHMOD1        SCMOD1
     C                   MOVEL     HHMOD2        SCMOD2
     C                   MOVEL     HHTOS         SCTOS
     C*
     C                   MOVEL(P)  HHPRCD        SCPRCD
     C                   Z-ADD     HHEFDT        SCEFDT
     C                   Z-ADD     HHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFD2
     C*
     C                   MOVEL     HHDESC        XXDESC           40
     C*
     C   21              MOVEL(P)  HHPRCD        SCDATA
     C   21              MOVE      XXDESC        SCDATA
     C*
     C                   Z-ADD     HHALOW        SCALOW
     C*
     C                   MOVEL     HHSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
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
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
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
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      140           STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      140           PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  HHDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     HHEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     HHSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Proc   Description                                Eff. date   Allowance Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1033=Copy                   NNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
211F11=Change View          NNN
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
