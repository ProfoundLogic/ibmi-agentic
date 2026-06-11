     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  Created by C: 117  Project: 115AAC  03/30/01
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  05/17/07 C:191 Support: 58389
     H*                 - Fix Paging, add unique key fields to SMI1L
     H*
     H*  10/08/04 C:62 PROJECT IM010 44087
     H*                FIX PAGING
     H*
     H*****************************************************************
     FSCI101F   CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMI1P     IF   E             DISK
     F                                     RENAME(PI101:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMI1L     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PI101:DBRCDF)
     F*
     F*  Second logical file definition (for displaying VIEW 2)
     FSMI1L2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PI101:DBRCDF2)
     F*
     F*  Third logical file definition (for displaying VIEW 3)
     FSMI1L3    IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PI101:DBRCDF3)
     F*
     F*  Fourth logical file definition (for displaying VIEW 4)
     FSMI1L4    IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PI101:DBRCDF4)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
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
     D OPTNM           C                   CONST('Object Import Proces-
     D                                     s')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('SCI101R1')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('S0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(4)
     D SF1PAG          C                   CONST(12)
     D*
     D*  Data Structures for VIEWS and INFO
     D*
     D                 DS
     D  VIEW1                  1     56
     D  V1PTY                  2      3  0
     D  V1IDXT                 8      8
     D  V1IMGF                15     15
     D  V1RMCD                20     24
     D  V1DOCT                28     35
     D  V1KPGM                37     46
     D  V1STAT                56     56
     D*
     D                 DS
     D  VIEW2                  1     56
     D  V2IDXT                 3      3
     D  V2PTY                  8      9  0
     D  V2IMGF                15     15
     D  V2RMCD                20     24
     D  V2DOCT                28     35
     D  V2KPGM                37     46
     D  V2STAT                56     56
     D*
     D                 DS
     D  VIEW3                  1     56
     D  V3IMGF                 3      3
     D  V3PTY                  8      9  0
     D  V3IDXT                15     15
     D  V3RMCD                20     24
     D  V3DOCT                28     35
     D  V3KPGM                37     46
     D  V3STAT                56     56
     D*
     D                 DS
     D  VIEW4                  1     56
     D  V4KPGM                 1     10
     D  V4PTY                 18     19  0
     D  V4IDXT                24     24
     D  V4IMGF                31     31
     D  V4RMCD                36     40
     D  V4DOCT                44     51
     D  V4STAT                56     56

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@CALL            1
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
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C* VIEW 1
     C     KEY1          KLIST
     C                   KFLD                    KKPTY
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   KFLD                    KKIDXT
     C                   KFLD                    KKIMGF
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDOCT
     C                   KFLD                    KKKPGM
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C* VIEW 2
     C     KEY2          KLIST
     C                   KFLD                    KKIDXT
     C                   KFLD                    KKPTY
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   KFLD                    KKIMGF
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDOCT
     C                   KFLD                    KKKPGM
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C* VIEW 3
     C     KEY3          KLIST
     C                   KFLD                    KKIMGF
     C                   KFLD                    KKPTY
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   KFLD                    KKIDXT
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDOCT
     C                   KFLD                    KKKPGM
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C* VIEW 4
     C     KEY4          KLIST
     C                   KFLD                    KKKPGM
     C                   KFLD                    KKPTY
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   KFLD                    KKIDXT
     C                   KFLD                    KKIMGF
     C                   KFLD                    KKRMCD
     C                   KFLD                    KKDOCT
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    I1PTY         KKPTY
     C     *LIKE         DEFINE    I1IDXT        KKIDXT
     C     *LIKE         DEFINE    I1IMGF        KKIMGF
     C     *LIKE         DEFINE    I1KPGM        KKKPGM
     C*** BEGIN ADD *** 10/08/04 **************************************
     C     *LIKE         DEFINE    I1RMCD        KKRMCD
     C     *LIKE         DEFINE    I1DOCT        KKDOCT
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C     *LIKE         DEFINE    I1IDT         KKIDT
     C     *LIKE         DEFINE    I1ITIM        KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
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
     C     SSSTAT        IFNE      ' '
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
     C                   MOVE      *LOVAL        KKPTY
     C                   MOVE      *LOVAL        KKIDXT
     C                   MOVE      *LOVAL        KKIMGF
     C                   MOVE      *LOVAL        KKKPGM
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   MOVE      *LOVAL        KKRMCD
     C                   MOVE      *LOVAL        KKDOCT
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   MOVE      *LOVAL        KKIDT
     C                   MOVE      *LOVAL        KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKPTY
     C                   MOVEL     SCPST2        KKIDXT
     C                   MOVEL     SCPST3        KKIMGF
     C                   MOVEL     SCPST4        KKKPGM
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   MOVE      *LOVAL        KKRMCD
     C                   MOVE      *LOVAL        KKDOCT
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   MOVE      *LOVAL        KKIDT
     C                   MOVE      *LOVAL        KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCPTY         KKPTY
     C                   MOVEL     SCIDXT        KKIDXT
     C                   MOVEL     SCIMGF        KKIMGF
     C                   MOVEL     SCKPGM        KKKPGM
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   MOVEL     SCRMCD        KKRMCD
     C                   MOVEL     SCDOCT        KKDOCT
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 *************************************
     C                   Z-ADD     SCIDT         KKIDT
     C                   Z-ADD     SCITIM        KKITIM
     C*** END ADD ****** 05/17/07 *************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   VIEW1
     C*
     C*  Load data into hidden fields
     C*
     C                   Z-ADD     I1PTY         SCPTY
     C                   MOVEL     I1IDXT        SCIDXT
     C                   MOVEL     I1IMGF        SCIMGF
     C                   MOVEL     I1KPGM        SCKPGM
     C*** BEGIN ADD *** 10/08/04 **************************************
     C                   MOVEL     I1RMCD        SCRMCD
     C                   MOVEL     I1DOCT        SCDOCT
     C*** END   ADD *** 10/08/04 **************************************
     C*** BEG ADD ****** 05/17/07 **************************************
     C                   Z-ADD     I1IDT         SCIDT
     C                   Z-ADD     I1ITIM        SCITIM
     C*** END ADD ****** 05/17/07 **************************************
     C*
     C*  Load views
     C*
     C                   SELECT
     C*  VIEW 1
     C     *IN21         WHENEQ    '1'
     C                   Z-ADD     I1PTY         V1PTY
     C                   MOVEL     I1IDXT        V1IDXT
     C                   MOVEL     I1IMGF        V1IMGF
     C                   MOVEL     I1RMCD        V1RMCD
     C                   MOVEL     I1DOCT        V1DOCT
     C                   MOVEL     I1KPGM        V1KPGM
     C                   MOVEL     I1STAT        V1STAT
     C                   MOVEL     VIEW1         SCDATA
     C*
     C*  VIEW 2
     C     *IN22         WHENEQ    '1'
     C                   Z-ADD     I1PTY         V2PTY
     C                   MOVEL     I1IDXT        V2IDXT
     C                   MOVEL     I1IMGF        V2IMGF
     C                   MOVEL     I1RMCD        V2RMCD
     C                   MOVEL     I1DOCT        V2DOCT
     C                   MOVEL     I1KPGM        V2KPGM
     C                   MOVEL     I1STAT        V2STAT
     C                   MOVEL     VIEW2         SCDATA
     C*
     C*  VIEW 3
     C     *IN23         WHENEQ    '1'
     C                   Z-ADD     I1PTY         V3PTY
     C                   MOVEL     I1IDXT        V3IDXT
     C                   MOVEL     I1IMGF        V3IMGF
     C                   MOVEL     I1RMCD        V3RMCD
     C                   MOVEL     I1DOCT        V3DOCT
     C                   MOVEL     I1KPGM        V3KPGM
     C                   MOVEL     I1STAT        V3STAT
     C                   MOVEL     VIEW3         SCDATA
     C*
     C*  VIEW 4
     C     *IN24         WHENEQ    '1'
     C                   Z-ADD     I1PTY         V4PTY
     C                   MOVEL     I1IDXT        V4IDXT
     C                   MOVEL     I1IMGF        V4IMGF
     C                   MOVEL     I1RMCD        V4RMCD
     C                   MOVEL     I1DOCT        V4DOCT
     C                   MOVEL     I1KPGM        V4KPGM
     C                   MOVEL     I1STAT        V4STAT
     C                   MOVEL     VIEW4         SCDATA
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - Subroutine for Option 20
     C*================================================================
     C     OPT20         BEGSR
     C*
     C*Option 20=Change Status
     C*
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                    99
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
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
     C                   SETON                                        LR
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
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C   23              READ      DBRCDF3                                95
     C   24              READ      DBRCDF4                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C   23              READP     DBRCDF3                                95
     C   24              READP     DBRCDF4                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C   23KEY3          SETGT     DBRCDF3                            96
     C   24KEY4          SETGT     DBRCDF4                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C   23KEY3          SETLL     DBRCDF3                            96
     C   24KEY4          SETLL     DBRCDF4                            96
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     I1STAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Pty  Type   Type   Code    Type     Program         Status
Opt  Type  Pty   Type   Code    Type     Program         Status
Opt  Type  Pty   Type   Code    Type     Program         Status
Opt  Program         Pty  Type   Type   Code    Type     Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYN
212F12=Cancel               NNY
217F17=Subset               YYY
3
