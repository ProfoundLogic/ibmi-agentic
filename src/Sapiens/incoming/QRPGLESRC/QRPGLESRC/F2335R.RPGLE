     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2141R - WORK WITH SPECIALTY CODE TABLE
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays the specialty code table file     **
     H**     records.                                                **
     H**        The user can select the following options:           **
     H**           02 - Change                                       **
     H**           05 - Display                                      **
     H**        The user can select the following functions:         **
     H**           F3 - Exit                                         **
     H**           F5 - Refresh                                      **
     H**           F6 - Add                                          **
     H**           F17 - Subset (Status code selection for display)  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU    - Parms are:                                    **
     H**               @@CALL    ' ' if Maintenance                  **
     H**                         '1' if Inquiry                      **
     H**                         '2' if Select                       **
     H**                                                             **
     H**     VARIOUS - Parms are:                                    **
     H**               @@CALL    See above                           **
     H**               QQKEY     Start search key (Input)            **
     H**                         Selected key (Output)               **
     H**               QQRTN     '*' selection  made (Output)        **
     H**                                                             **
     H**   CALLS TO:                                                 **
     H**     F2141R02 - Specialty Code Table Update/Display          **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   07/21/97      PROJECT: MC156                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  11/19/12 C:287 Project: MC367
     H*                 Disable function code F17 and option 5 for GS
     H*                                                                 *
     H*  09/11/00 C:62 PROJECT:MC205                                    *
     H*                ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.   *
     H*                                                                 *
     H*  05/25/00 C:62 PROJECT:MC205                                    *
     H*                ADD F21=PRINT LIST                               *
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      89      Error: Invalid status code                     **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTSPP  (PFRCDF) : Specialty Code Table File            **
     H**      FTABL  (      ) : Logical Over Table Files (CO#/KEY)   **
     H**             (DBRCDF) : Specialty Code Table File            **
     H*****************************************************************
     H*
     FF2335F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTSPP     IF   E             DISK
     F                                     RENAME(PSP01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSP01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFTSPL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PSP01:DBRCDF2)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D*----------------------------------------------------------------
     D*           The following table was changed from 8 to 9 entries
     D*
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*----------------------------------------------------------------
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Specialty Code Tabl-
     D                                     e')
     D SLMSGI          C                   CONST('SYS0001')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2335R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D*** BEGIN ADD *** 05/25/00 **************************************
     D FLAG            C                   CONST('    1          ')

     I*** END   ADD *** 05/25/00 **************************************
     I*----------------------------------------------------------------
     I*           Screen Data Area
     I*
     I*----------------------------------------------------------------

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C*----------------------------------------------------------------
     C*           Parms added to pass back data
     C*
     C                   PARM                    QQKEY            10
     C                   PARM                    QQRTN             1
     C*----------------------------------------------------------------
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
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDESC
     C*----------------------------------------------------------------
     C*          Superfluous KFLD statements were deleted
     C*
     C*----------------------------------------------------------------
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SPCO#         KKCO#
     C     *LIKE         DEFINE    SPKEY         KKKEY
     C     *LIKE         DEFINE    SPDESC        KKDESC
     C     *LIKE         DEFINE    SPDESC        U1PDSC
     C     *LIKE         DEFINE    SPDESC        U2PDSC
     C*----------------------------------------------------------------
     C*          Superfluous DEFN statements were deleted
     C*
     C*----------------------------------------------------------------
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*----------------------------------------------------------------
     C*          Check if subset requested: IN11 = yes
     C*
     C     SSDESC        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C                   SETON                                        11
     C                   ENDIF                                                  STAT <> blanks
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*          Edit subset response: Status must be 'A' or 'I'
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF                                                  STAT <> blank
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*----------------------------------------------------------------
     C*          Assume 'A' when subset is requested
     C*
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVE      'A'           SSSTAT
     C*
     C                   ENDIF
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*----------------------------------------------------------------
     C*          Set key for first key: Use parm if one was passed
     C*
     C                   MOVE      *LOVAL        KKKEY
     C                   MOVE      *LOVAL        KKDESC
     C     Q1PRMS        IFGT      1
     C                   MOVEL(P)  QQKEY         KKKEY
     C                   ENDIF                                                  PRMS > 1
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*----------------------------------------------------------------
     C*          Set position to key
     C*
     C   21              MOVEL(P)  SCPST1        KKKEY
     C   22              MOVEL(P)  SCPST2        KKDESC
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*----------------------------------------------------------------
     C*          Set paging key
     C*
     C                   MOVEL(P)  SCKEY         KKKEY
     C                   MOVEL(P)  SCDESC        KKDESC
     C*----------------------------------------------------------------
     C                   ENDSR
     C*** BEGIN ADD *** 05/25/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   MOVE      ##CO#         ##CO
     C                   MOVEL(P)  FLAG          ##FLAG
     C*
     C                   CALL      'F7010C'
     C                   PARM                    ##CO              3
     C                   PARM                    ##FLAG           15
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      *BLANKS       ##STNM           40
     C*** BEGIN ADD *** 09/11/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 09/11/00 **************************************
     C*
     C                   ENDSR
     C*** END   ADD *** 05/25/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*----------------------------------------------------------------
     C*          Move data base fields
     C*
     C                   MOVEL(P)  SPDESC        SCDESC
     C                   MOVEL(P)  SPKEY         SCKEY
     C                   MOVEL     SPKEY         XXKEY             3
     C   21              MOVEL(P)  XXKEY         SCDATA
     C   21              MOVE      SPDESC        SCDATA
     C   22              MOVEL(P)  SPDESC        SCDATA
     C   22              MOVE      XXKEY         SCDATA
     C                   MOVEL(P)  SPSTAT        SCSTAT
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*** BEGIN ADD *** 05/25/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 05/25/00 **************************************
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*----------------------------------------------------------------
     C*          Process option 20: Change status
     C*
     C     SCSEL         IFEQ      '20'
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF                                                  SEL = 20
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*          Initialize company number
     C*
     C                   Z-ADD     ##CO#         KKCO#
     C*----------------------------------------------------------------
     C*** Beg Add *** MC367 *** 11/19/12 ****************************
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
     C*----------------------------------------------------------------
     C*          Read next record
     C*
     C   21##CO#         READE     DBRCDF                                 95
     C   22##CO#         READE     DBRCDF2                                95
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*----------------------------------------------------------------
     C*          Read previous record
     C*
     C   21##CO#         READPE    DBRCDF                                 95
     C   22##CO#         READPE    DBRCDF2                                95
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*----------------------------------------------------------------
     C*          Set greater than for read
     C*
     C     KEY1          SETGT     DBRCDF                             96
     C     KEY2          SETGT     DBRCDF2                            96
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*----------------------------------------------------------------
     C*          Set less than for read
     C*
     C     KEY1          SETLL     DBRCDF                             96
     C     KEY2          SETLL     DBRCDF2                            96
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*         If parm pass back needed: return selected entry
     C*
     C     Q1PRMS        IFGT      1
     C                   MOVEL(P)  SCKEY         QQKEY
     C                   MOVEL     '*'           QQRTN
     C                   ENDIF                                                  PRMS > 1
     C*----------------------------------------------------------------
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
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
     C*----------------------------------------------------------------
     C*         If parm pass back needed: return selected entry
     C*
B01  C     SSDESC        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  SPDESC        U1PDSC
     C     LO:HI         XLATE(P)  SSDESC        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     SPSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  STAT <> blanks
     C*----------------------------------------------------------------
     C                   ENDSR
     C*----------------------------------------------------------------
     C* In the following list, 1044=Delete was changed from YNN to NNN
     C*                        12020=Change Status was added
     C*----------------------------------------------------------------
** Column headings
Opt  Code Description                                        Status
Opt  Description                     Code                    Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
