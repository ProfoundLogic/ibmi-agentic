     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC300R - WORK WITH SUMMARY CODES
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Summary Code    **
     H**     file.                                                   **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   - 'Work with Summary Codes'                      **
     H**   CALLS TO:                                                 **
     H**     SC300F01 - Work With Summary Code Maintenance           **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   12/11/97      PROJECT: dr047                   **
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 02/24/04 C:62 PROJECT:PR029
     H*               ADD PARMS SO THAT THIS PROGRAM CAN BE USED
     H*               AS A SEARCH
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      01   Used on the PAGEDOWN keyword
     H**      02   Used on the PAGEUP keyword
     H**      03   Used to condition SFLCLR
     H**     N03   Used to condition SFLDSPCTL
     H**      04   Used to condition SFLDSP
     H**      05   Used to condition DSPATR(RI) on the sel field
     H**     N06   Used to condition DSPATR(PC) on the pos to field
     H**      07   Used to condition DSPATR(PC) on the sel field
     H**      08   Used to condition SFLNXTCHG
     H**      09   Reserved for maintenance/inquiry conditions
     H**     N10   Used to condition SFLEND (on message subfile)
     H**      11   Used to indicate the list is subsetted
     H**      12   Used to condition SFLDROP/SFLFOLD
     H**      13   Used to condition SFLCLR (on confirm delete)
     H**     N13   Used to condition SFLDSPCTL (on confirm delete)
     H**      14   Used to condition SFLDSP (on confirm delete)
     H**      15   Used to condition SFLEND (on confirm delete)
     H**      21   View 1 - Remark code sequence
     H**      22 - 29 are reserved for other views
     H**      54   SCM0631 Error - Invalid value (Summary code)
     H**      60   Used for chaining
     H**      75 - 88 are reserved for error screen error indicators
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     FSC300F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTSMP     IF   E             DISK
     F                                     RENAME(PSM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTSML     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSM01:DBRCDF)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Summary Codes')
     D SLMSGI          C                   CONST('SC49998')
     D SLMSGF          C                   CONST('SCMESG')
     D ACDPGM          C                   CONST('SC300R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(12)
     D*
     D* Program specific constants
     D*
     D DL              C                   CONST('* * * DELETED * * *')

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*** BEGIN ADD *** 02/24/04 **************************************
     C                   PARM                    @@SMCD            4
     C                   PARM                    @@DESC           40
     C*** END   ADD *** 02/24/04 **************************************
     C*                                                    1 = Inquiry
     C*                                                    2 = Search
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
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    XXSMCD
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    SMSMCD        XXSMCD
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSTS         IFNE      *BLANKS
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
     C* CHECK ACTIVE/INACTIVE
     C*
     C     SSSTS         IFNE      'A'
     C     SSSTS         ANDNE     'I'
     C     SSSTS         ANDNE     *BLANKS
     C                   SETON                                        9054
     C                   ENDIF
     C*
     C     ERRSTP        ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        XXSMCD
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   MOVEL     SCPST1        XXSMCD
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCSMCD        XXSMCD
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     SMSMCD        SCSMCD
     C                   MOVEL     SMDESC        SCDESC
     C                   MOVEL     SMSTAT        SCSTAT
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
     C     SCSEL         IFEQ      '20'
     C                   MOVEL     'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF
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
     C                   MOVEL     SCSMCD        XXSMCD
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
     C                   MOVEL     DL            SCDESC
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*** BEGIN ADD *** 02/24/04 **************************************
     C     Q1PRMS        IFGT      1
     C                   MOVEL(P)  SCSMCD        @@SMCD
     C                   MOVEL(P)  SCDESC        @@DESC
     C                   ENDIF
     C*** END   ADD *** 02/24/04 **************************************
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C     SSSTS         IFNE      *BLANK
     C     SSSTS         ANDNE     SMSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Opt  Code  Description                              Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYY
12020=Change Status         YNN
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   YNN
212F12=Cancel               NYY
217F17=Subset               YYY
3
