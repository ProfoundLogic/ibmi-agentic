     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2435R - WORK WITH BILL TYPE CODES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays the Bill Status Code table file   **
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
     H**     F2435R01 - Bill Type Code Information                   **
     H**                                                             **
     H**   PROGRAMMER: C:85                                          **
     H**   CREATED:   04/03/98      PROJECT: MC152                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H*  REVISIONS:
     H*
     H*  11/14/12 C:287 Project: MC367
     H*                Disabled Function code F17 for GS
     H*                                                                 *
     H*  09/11/00 C:62 PROJECT:MC205                                    *
     H*                -ADD LOGIC TO INCLUDE PROMPT WINDOW FOR REPORT.  *
     H*                -ADD F21=PRINT LIST                              *
     H*
     H*  05/05/00 C:62 PROJECT: FSMNT
     H*                ADD SUBSET AND REFRESH.
     H*
     H*  07/19/99 C:62 PROJECT: MC208
     H*                DISPLAY ONLY ACTIVE CODES IN SEARCH MODE.
     H*
     H*  06/24/99 C:62 PROJECT: MC202
     H*                REMOVE DARTS CODE AND NUMBER OF DAYS.
     H*
     H*  09/10/98 C:56 PROJECT: MC152
     H*                Change title to 'Bill type codes'
     H*
     H*  06/15/98 C:61 PROJECT MC152
     H*                Added Parm to restrict access to file based on
     H*                BSTYPE.
     H*
     H*  PLEASE ADD TO TOP
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      88      Error: Invalid Type                            **
     H**      89      Error: Invalid status code                     **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTBSP  (PFRCDF) : Bill Status Code Table File          **
     H**      FTBSL  (      ) : Logical Over Table Files (CO#/TYPE/  **
     H**                        CODE)                                **
     H**             (DBRCDF) : Bill Status Code Table File          **
     H**      FTBSL1 (      ) : Logical Over Table Files (CO#/DESC)  **
     H**             (DBRCDF2): Bill Status Code Table File          **
     H*****************************************************************
     H*
     FF2435F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFTBSL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PBS01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
     FFTBSL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PBS01:DBRCDF2)
     F*** BEGIN ADD *** 05/05/00 **************************************
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END   ADD *** 05/05/00 **************************************
     F*
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D*----------------------------------------------------------------
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
     D*
     D*** BEG CHG *** 09/10/98 ****************************************
     D**********    'Bill Status Codes'   C         OPTNM
     D OPTNM           C                   CONST('Bill Status Type Co-
     D                                     des')
     D*** END CHG *** 09/10/98 ****************************************
     D SLMSGI          C                   CONST('SYS0001')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2435R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
     D*** BEGIN ADD *** 09/11/00 **************************************
     D FLAG            C                   CONST('    1    ')

     I*** END   ADD *** 09/11/00 **************************************
     I*----------------------------------------------------------------
     I*           Screen Data Area
     I*
     I*----------------------------------------------------------------

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
      ***** BEG ADD ******* 06/15/98 **********************************
     C                   PARM                    @@TYPE            1
      ***** END ADD ******* 06/15/98 **********************************
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
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDESC
     C*** BEGIN ADD *** 05/05/00 **************************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKGKEY
     C*** END   ADD *** 05/05/00 **************************************
     C*
     C*----------------------------------------------------------------
     C*          Superfluous KFLD statements were deleted
     C*
     C*----------------------------------------------------------------
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    BSCO#         KKCO#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    BSDESC        KKDESC
     C*** BEGIN ADD *** 05/05/00 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
     C*** END   ADD *** 05/05/00 **************************************
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
     C*** BEGIN ADD *** 05/05/00 **************************************
     C     SSTYPE        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANKS
     C*
     C                   SETON                                        11
     C*
     C                   ENDIF
     C*** END   ADD *** 05/05/00 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*** BEGIN ADD *** 05/05/00 **************************************
     C*
     C                   SETOFF                                       3132
     C*
     C  N90
     CAN KJ              EXSR      KJDO
     C*
     C* TYPE MUST BE VALID
     C*
     C     *IN90         IFEQ      *OFF
     C     SSTYPE        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SSTYPE        KKGKEY
     C                   MOVEL(P)  'BLPR'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SSTYPD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* STATUS MUST BE 'A'/'I'/' '
     C*
     C     *IN90         IFEQ      *OFF
     C     SSSTAT        ANDNE     *BLANKS
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C*
     C                   SETON                                        7290
     C*
     C                   ENDIF
     C*
     C*** END   ADD *** 05/05/00 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCKC - Additional errors on subfile selection
     C*================================================================
     C     ERRCKC        BEGSR
     C*
     C* If a code is entered and a type is not entered then
     C* set on error indicators 88 & 90.
     C*
     C*
     C* NOTE: This code will not work without a change to SCOPYWW01C.
     C*
     C     SCTYPE        IFEQ      *BLANK
     C     SCCODE        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN88
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCTYPE=*BLANK
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*** BEGIN ADD *** 05/05/00 **************************************
     C     @@CALL        IFEQ      '2'
     C*
     C                   MOVEL     'A'           SSSTAT
     C*
     C                   ENDIF
     C*
     C     @@TYPE        IFNE      *BLANKS
     C*
     C                   MOVEL     @@TYPE        SSTYPE
     C*
     C                   ENDIF
     C*** END   ADD *** 05/05/00 **************************************
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*----------------------------------------------------------------
     C*          Set key for first key: Use parm if one was passed
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVE      *LOVAL        KKTYPE
     C                   MOVE      *LOVAL        KKCODE
     C                   MOVE      *LOVAL        KKDESC
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*----------------------------------------------------------------
     C*          Set position to key
     C*
     C   21              MOVEL(P)  SCPST1        KKTYPE
     C   21              MOVEL(P)  SCPSTC        KKCODE
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
     C   21              MOVEL(P)  SCTYPE        KKTYPE
     C   21              MOVEL(P)  SCCODE        KKCODE
     C   22              MOVEL(P)  SCDESC        KKDESC
     C*----------------------------------------------------------------
     C                   ENDSR
     C*** BEGIN ADD *** 05/05/00 **************************************
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SSTYPE'
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'BLPR'        ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SSTYPE
     C                   MOVEL(P)  GTDESC        SSTYPD
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END   ADD *** 05/05/00 **************************************
     C*** BEGIN ADD *** 09/11/00 **************************************
     C*================================================================
     C* KVDO - F21=PRINT LIST
     C*================================================================
     C     KVDO          BEGSR
     C*
     C                   EXFMT     WDW01
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C                   MOVE      ##CO#         ##CO
     C                   MOVEL(P)  FLAG          ##FLAG
     C*
     C                   CALL      'F7050C'
     C                   PARM                    ##CO              3
     C                   PARM                    ##FLAG            9
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      'QPRINT'      ##QUE            10
     C                   PARM      'D'           ##MODE            1
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 09/11/00 **************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*----------------------------------------------------------------
     C*          Move data base fields
     C*
     C                   MOVEL(P)  BSTYPE        SCTYPE
     C                   MOVEL(P)  BSCODE        SCCODE
     C                   MOVEL(P)  BSDESC        SCDESC
     C                   MOVEL(P)  BSLTR         SCLTR
     C*** BEGIN DEL *** 06/24/99 **************************************
     C**********           MOVELBSRMCD    SCRMCD    P
     C**********           MOVELBSDIDT    SCDIDT    P
     C*** END   DEL *** 06/24/99 **************************************
     C                   MOVEL(P)  BSSTAT        SCSTAT
     C*
     C                   MOVEL(P)  BSTYPE        XXDATA            6
     C                   MOVE      BSCODE        XXDATA
     C*
     C   21              MOVEL(P)  XXDATA        SCDATA
     C   21              MOVE      BSDESC        SCDATA
     C   22              MOVEL(P)  BSDESC        SCDATA
     C   22              MOVE      XXDATA        SCDATA
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*** BEGIN ADD *** 09/11/00 **************************************
     C     *INKV         CASEQ     *ON           KVDO
     C                   ENDCS
     C*** END   ADD *** 09/11/00 **************************************
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
     C*
     C*** Beg Add *** MC367 *** 11/14/12 ******************************
      /Free
      /If Not Defined(WEB)
       //Disable function code F17
           XXOpfn = '2';
           XXMIS = 'NNN';
           XXNmbr = '17';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/14/12 ******************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*----------------------------------------------------------------
     C*          Read next record
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*----------------------------------------------------------------
     C*          Read previous record
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*----------------------------------------------------------------
     C*          Set greater than for read
     C*
     C     KEY2          SETGT     DBRCDF                             96
     C     KEY3          SETGT     DBRCDF2                            96
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*----------------------------------------------------------------
     C*          Set less than for read
     C*
     C     KEY2          SETLL     DBRCDF                             96
     C     KEY3          SETLL     DBRCDF2                            96
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
     C                   Z-ADD     SCDBRR        @@RRN
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*** BEGIN DEL *** 05/05/00 **************************************
     C**********
      ***** BEG ADD ******* 06/15/98 **********************************
      *
      * If type entry parm then restrict choices to specified type
      *
     C********** @@TYPE    IFNE *BLANK
     C********** @@TYPE    ANDNEBSTYPE
     C**********           MOVE *OFF      MEETSS
     C**********           ENDIF
      *
      ***** END ADD ******* 06/15/98 **********************************
     C*** BEGIN ADD *** 07/19/99 **************************************
     C*
     C********** @@CALL    IFEQ '2'
     C********** BSSTAT    ANDEQ'I'
     C*
     C**********           MOVE *OFF      MEETSS
     C*
     C**********           ENDIF
     C*** END   ADD *** 07/19/99 **************************************
     C*** END   DEL *** 05/05/00 **************************************
     C*** BEGIN ADD *** 05/05/00 **************************************
     C     SSTYPE        IFNE      *BLANKS
     C     SSTYPE        ANDNE     BSTYPE
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     BSSTAT
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*** END   ADD *** 05/05/00 **************************************
     C                   ENDSR
     C*----------------------------------------------------------------
** Column headings
Opt Typ Code Description                               Ltr   Sts
Opt  Description                              Typ Code Ltr   Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               YNN
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
221F21=Print list           YYN
3
