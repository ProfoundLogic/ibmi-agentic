     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Work with FL Provider Lic#
     H*  C: 117   Project: FL031   09/20/00
     H*****************************************************************
     H*  REVISIONS:
     H*****************************************************************
     FF2395F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMPLPFL   IF   E             DISK
     F                                     RENAME(PPL01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFMPLLFL   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PPL01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFMPLLFL1  IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PPL01:DBRCDF2)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('FL Provider Lic#')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2395R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D*
     D* Data Structures for VIEWS and INFO
     D*
     D                 DS
     D  VIEW1                  1     48
     D  V1LIC#                 1      9
     D  V1LNM                 12     31
     D  V1FNM                 34     47
     D*
     D                 DS
     D  VIEW2                  1     48
     D  V2LNM                  1     20
     D  V2FNM                 23     36
     D  V2LIC#                39     47
     D*
     D                 DS
     D  INFO                   1     22
     D  XXCITY                 1     18
     D  XXST                  20     22

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@LIC#            9
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKLIC#
     C                   KFLD                    KKLNM
     C                   KFLD                    KKFNM
     C     KEY2          KLIST
     C                   KFLD                    KKLNM
     C                   KFLD                    KKFNM
     C                   KFLD                    KKLIC#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PLLIC#        KKLIC#
     C     *LIKE         DEFINE    PLLNM         KKLNM
     C     *LIKE         DEFINE    PLFNM         KKFNM
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKLIC#
     C                   MOVE      *LOVAL        KKLNM
     C                   MOVE      *LOVAL        KKFNM
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKLIC#
     C   22              MOVEL     SCPST2        KKLNM
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCLIC#        KKLIC#
     C                   MOVEL     SCLNM         KKLNM
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   VIEW1
     C                   CLEAR                   VIEW2
     C                   CLEAR                   INFO
     C*
     C* Load data in hidden fields
     C*
     C                   MOVEL     PLLIC#        SCLIC#
     C                   MOVEL     PLLNM         SCLNM
     C                   MOVEL     PLFNM         SCFNM
     C*
     C* Load INFO DS
     C*
     C                   MOVEL     PLCITY        XXCITY
     C                   MOVEL     PLST          XXST
     C                   MOVE      INFO          SCDATA
     C*
     C* Load Views
     C*
     C                   SELECT
     C* VIEW 1
     C     *IN21         WHENEQ    '1'
     C                   MOVEL     PLLIC#        V1LIC#
     C                   MOVEL     PLLNM         V1LNM
     C                   MOVEL     PLFNM         V1FNM
     C                   MOVEL     VIEW1         SCDATA
     C* VIEW 2
     C     *IN22         WHENEQ    '1'
     C                   MOVEL     PLLIC#        V2LIC#
     C                   MOVEL     PLLNM         V2LNM
     C                   MOVEL     PLFNM         V2FNM
     C                   MOVEL     VIEW2         SCDATA
     C*
     C                   ENDSL
     C*
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
     C   21              READ      DBRCDF                                 95
     C   22              READ      DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C   22              READP     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
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
     C                   MOVEL     SCLIC#        @@LIC#
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt  License #  Last Name             First Name     City             State
Opt  Last Name             First Name      License # City             State
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
211F11=Change view          YYY
212F12=Cancel               YYY
3
