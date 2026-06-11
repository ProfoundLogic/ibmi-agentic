     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H***************************************************************
     H*                                                             *
     H*  CREATED:  07/31/98  C:94  PROJECT:  MC179                  *
     H*                                                             *
     H*  WORK WITH WCMC COMPANY STATE INFORMATION                   *
     H*                                                             *
     H***************************************************************
     H*                                                             *
     H*  REVISIONS:                                                 *
     H*  02/25/14 C:079 Support: MC099 84930                        *
     H*                 Allow Exit                                  *
     H*                                                             *
     H***************************************************************
     H*
     FF0053F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FSMCOP     IF   E             DISK
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTSTL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PST01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D DATA            S             70    DIM(1)
     D DTA             S              1    DIM(70)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Company State I-
     D                                     nformation')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D COMAPI          C                   CONST('FA007R')
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
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     @@CALL        IFEQ      'A'
     C                   MOVE      'A'           XXMODE            1
     C                   MOVE      ' '           @@CALL
     C                   ENDIF
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    STNAME        SCSTNM
     C     *LIKE         DEFINE    SCSTNM        KKSTNM
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
     C*
     C* STATE  PROCESSING
     C*
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '5'
     C*
     C                   SELECT
     C*
     C     SCST          WHENEQ    'FL'
     C                   MOVEL(P)  'F0053RFL'    ACDPGM           40
     C*
     C     SCST          WHENEQ    'TX'
     C                   MOVEL     'F0053RTX'    ACDPGM
     C*
     C                   OTHER
     C                   SETON                                        9071
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  SCSEL = '2'
     C*
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
     C                   MOVE      *BLANKS       KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL(P)  SCPST1        KKKEY
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL(P)  SCST          KKKEY
     C                   MOVEL(P)  SCSTNM        KKSTNM
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVE      *BLANKS       SCST
     C                   CLEAR                   SCDATA
     C     @@RRN         CHAIN     PCO01                              99
     C                   MOVE      COCO#         SCCO#
     C                   MOVEL(P)  CONAME        SCNAME
     C                   MOVEL(P)  STKEY         SCST
     C                   MOVEL(P)  STNAME        SCSTNM
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCST          DTA(Y)
     C                   Z-ADD     9             Y
     C                   MOVEA     SCSTNM        DTA(Y)
     C                   MOVEA     DTA           SCDATA
     C*
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
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   ENDSR
** Column headings
Opt   State    Name
** Description--------------MISN
1011=Select                 NNN
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYY
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
