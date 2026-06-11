     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/16/01 C:117 PROJECT: FSMNT #145
     H*                 1. Made changes for SCR02 so that 4=Delete
     H*                    will show line item. (COMPILE ONLY)
     H*
     H*  05/25/00 C:62 PROJECT:MC205                                  *
     H*                ADD ENTRY PARMS FOR *INKL AND *INKC.           *
     H*                                                               *
     H*  05/08/00 C:79 PROJECT:FSMNT                                  *
     H*                1. Remove SCCPT4                               *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                -CHANGE OPTNM.                                 *
     H*                -ADD F12=CANCEL                                *
     H*                                                               *
     H*  03/08/1999  C: 117 PROJECT: FS136                            *
     H*                     CHANGED THE TITLE OF THE SCREEN; ALSO     *
     H*                     DELETED THE PLISTS THAT THE COPYBOOK      *
     H*                     HANDLES.                                  *
     H*****************************************************************
     H*
     FF2011F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTUMP     IF   E             DISK
     F                                     RENAME(PUM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTUML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PUM01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEG CHG *** 03/08/1999 **************************************
     D**************'UC MODIFIER LOGIC'   C         OPTNM
     D*** BEGIN CHG *** 05/05/2000 ************************************
     D**********    'T/P COMPONENT TABLE' C         OPTNM
     D OPTNM           C                   CONST('T/P Components')
     D*** END   CHG *** 05/05/2000 ************************************
     D*** END CHG *** 03/08/1999 **************************************
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2011R01')
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
     C*** BEGIN ADD *** 05/25/00 **************************************
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*** END   ADD *** 05/25/00 **************************************
     C*
     C*** BEG DEL *** 03/17/1999 **************************************
     C***********DATCHK    PLIST
     C***********          PARM           SRDTI   80
     C***********          PARM *BLANKS   SRFFMT  8
     C***********          PARM *BLANKS   SRTFMT  8
     C***********          PARM           SRDTO  10
     C***********          PARM           SRERR   1
     C*** END DEL *** 03/17/1999 **************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG DEL *** 03/17/1999 **************************************
     C***********DTMDYY    PLIST
     C***********          PARM           SRDTI
     C***********          PARM '*CYMD'   SRFFMT
     C***********          PARM *BLANKS   SRTFMT
     C***********          PARM           SRDTO
     C***********          PARM           SRERR
     C*** END DEL *** 03/17/1999 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    UMCO#             3 0
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
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    UMCO#         KKCO#
     C     *LIKE         DEFINE    UMCPT4        KKCPT4
     C     *LIKE         DEFINE    UMEFDT        KKEFDT
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
     C                   MOVE      *LOVAL        KKEFDT
     C                   MOVE      *LOVAL        KKCPT4
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        KKCPT4
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   Z-ADD     XXDATE        KKEFDT
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*** BEG CHG *** 05/08/00 ****************************************
     C******************** MOVELSCCPT4    KKCPT4
     C                   MOVEL     SCFRCP        KKCPT4
     C*** END CHG *** 05/08/00 ****************************************
     C                   Z-ADD     SCFDT         KKEFDT
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEG DEL *** 05/08/00 ****************************************
     C******************** MOVELUMCPT4    SCCPT4
     C*** END DEL *** 05/08/00 ****************************************
     C                   Z-ADD     UMEFDT        SCEFDT
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     UMEFDT        SCFDT
     C                   MOVEL     UMFRCP        SCFRCP
     C                   MOVEL     UMTOCP        SCTOCP
     C                   MOVE      UMOD26        SCMD26
     C                   MOVE      UMOD27        SCMD27
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
     C                   MOVE      '1'           *INLR
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
     C   21KEY2          READE     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY2          READPE    DBRCDF                                 95
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
From     To       Date       Component (26) Component (27)
** Description--------------MISN
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
3
