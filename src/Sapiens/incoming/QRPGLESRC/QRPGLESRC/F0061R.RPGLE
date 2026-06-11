     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F0061R - WORK WITH WCMC GROUP STATE INFO    C:62 12/22/97    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*               F0060R                                          *
     H*               F0060R01 - IF A/R AND NETWORK NOT INSTALLED     *
     H*               F0060R02 - IF NETWORK NOT INSTALLED             *
     H*               F0060R03                                        *
     H*  CALLS TO:                                                    *
     H*               F0120R01 - WORK FLOW                            *
     H*               F0061RFL                                        *
     H*               F0061RKY                                        *
     H*               F0061RTX                                        *
     H*               F0061RCA                                        *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/13/07 C:188 PROJECT: CA002                                *
     H*                 ADDED CALL TO CA PROGRAM                      *
     H*                                                               *
     H*  01/02/00 C:62 PROJECT: MCMNT  BASE ONLY                      *
     H*                REMOVE SCMODE                                  *
     H*                                                               *
     H*  11/28/00 C:62 PROJECT: KY012                                 *
     H*                ADD CALL TO KENTUCKY PROGRAM.                  *
     H*                                                               *
     H*  05/10/00 C:62 PROJECT: MCMNT                                 *
     H*                REMOVED F14=DARTS.                             *
     H*                                                               *
     H*  03/03/00 C:62 PROJECT: MCMNT                                 *
     H*                ENHANCED THE WORKFLOW LOGIC.                   *
     H*                                                               *
     H*  11/29/99 C:53 PROJECT: MCMNT                                 *
     H*                ALLOW CALL TO F0120R01 IN ADD MODE             *
     H*                                                               *
     H*  10/14/99 C:53 PROJECT: MCMNT                                 *
     H*                CORRECT MODE PASSED TO F0120R01 TO NOT FORCE   *
     H*                ADD MODE.
     H*                                                               *
     H*  04/02/99 C:62 PROJECT: MC095 17425                           *
     H*                IF OPTION '9' IS USED, MAKE SURE F0120R01 IS   *
     H*                NOT CALLED.                                    *
     H*                                                               *
     H*  12/01/98 C:94 PROJECT: MC145                                 *
     H*                ADDED OPTION 14=DARTS TO PROGRAM               *
     H*                                                               *
     H*  11/17/98 C:62 PROJECT:MC145                                  *
     H*                ADDED F0120R01 TO THE WORK FLOW.               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*  ERROR INDICATORS IN OPT6:  71 (NO ADDITIONAL INFORMATION     *
     H*                                 NEEDED FOR THIS STATE)        *
     H*                                                               *
     H*****************************************************************
     H*  CALLS USED:                                                  *
     H*                                                               *
     H*  ' ' = Maintenance - workflow only                            *
     H*  '1' = Inquiry - workflow only                                *
     H*  'A' = Maintenance - when called from F0060R from opt9        *
     H*  'D' = Inquiry - when called from F0060R from opt9            *
     H*                                                               *
     H*  XXMODE is used to hold the mode that the program was called  *
     H*  with so that this program will know whether this program was *
     H*  call as part of the workflow or as an option from F0060R.    *
     H*                                                               *
     H*  This program is called if EDI is installed.                  *
     H*                                                               *
     H*****************************************************************
     FF0061F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSTSTP     IF   E             DISK
     F                                     RENAME(PST01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSTSTL2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PST01:DBRCDF)
     FFMFNP     IF   E             DISK
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 11/17/98 **************************************
     FFMCXL     IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F*** END   ADD *** 11/17/98 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D*** BEG CHG *** 12/01/98 ***************************************
     D OF              S              1    DIM(5) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(5) ALT(OF)
     D**********          OF      1   4  1   OFD    35   Options, Function
     D*** END CHG *** 12/01/98 ***************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('WCMC Group State-
     D                                      Information')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('STDEFAULT')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D MODEA           C                   CONST('-- Add --')

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C     *INKC         PARM      *INKC         @@KC              1
     C     *INKL         PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          STATE
     C                   PARM                    @@RRN2            9 0          fund
     C*
     C*
     C*** BEGIN ADD *** 04/02/99 **************************************
     C                   MOVEL     ' '           XXMODE
     C*** END   ADD *** 04/02/99 **************************************
     C*
     C     @@CALL        IFEQ      'A'
     C                   MOVE      'A'           XXMODE            1
     C                   MOVE      ' '           @@CALL
     C*** BEGIN ADD *** 04/02/99 **************************************
     C                   ELSE
     C*
     C     @@CALL        IFEQ      'D'
     C                   MOVE      'D'           XXMODE
     C                   MOVE      '1'           @@CALL
     C                   ENDIF
     C*** END   ADD *** 04/02/99 **************************************
     C*
     C                   ENDIF
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN2        ##RRN2            9 0
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
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKKEY
     C*
     C*** BEGIN ADD *** 11/17/98 **************************************
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C                   KFLD                    FNFND
     C*** END   ADD *** 11/17/98 **************************************
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNSUB#
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
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
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKKEY
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
     C*
     C                   MOVEL(P)  SCKEY         KKKEY
     C*
     C                   ENDSR
     C*** BEG ADD *** 12/01/98 ***************************************
     C*================================================================
     C* F14 = DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C* CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
     C*
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C*
     C                   MOVE      ##CO#         #@CO#
     C                   MOVE      SCSUB#        #@SUB#
     C                   MOVE      SCFND         #@FND
     C*
     C                   CALL      'FA718R'
     C                   PARM      *BLANKS       #@RTCD            1
     C                   PARM      ##WCIN        #@WCIN            1
     C                   PARM      ##WCDK        #@WDCK            1
     C                   PARM                    #@CO#             3
     C                   PARM                    #@SUB#            5
     C                   PARM                    #@FND             3
     C                   PARM      *BLANKS       #@KEY            40
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*** END ADD *** 12/01/98 ***************************************
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     STKEY         SCKEY
     C                   MOVEL     STKEY         SCST
     C*
     C                   MOVEL(P)  STKEY         SCDATA
     C                   MOVE      STNAME        SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* OPT6 - SELECT FOR STATE INFORMATION
     C*================================================================
     C     OPT6          BEGSR
     C*
     C                   MOVE      @@RRN2        ##RRN2
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C     XXMODE        IFEQ      'A'
     C                   MOVE      'A'           ##MODE
     C*
     C                   ELSE
     C                   MOVE      'C'           ##MODE
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVE      'D'           ##MODE
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C     SCST          WHENEQ    'FL'
     C                   CALL      'F0061RFL'    ACDPL
     C                   MOVEL     *ON           OUTFLG
     C*
     C     SCST          WHENEQ    'TX'
     C                   CALL      'F0061RTX'    ACDPL
     C                   MOVEL     *ON           OUTFLG
     C*** BEGIN ADD *** 11/28/00 **************************************
     C*
     C     SCST          WHENEQ    'KY'
     C                   CALL      'F0061RKY'    ACDPL
     C                   MOVEL     *ON           OUTFLG
     C*** END   ADD *** 11/28/00 **************************************
     C*
     C*** BEGIN ADD *** 02/13/07 **************************************
     C*
     C     SCST          WHENEQ    'CA'
     C                   CALL      'F0061RCA'    ACDPL
     C                   MOVEL     *ON           OUTFLG
     C*
     C*** END   ADD *** 02/13/07 **************************************
     C*
     C                   OTHER
     C                   SETON                                        7190
     C                   ENDSL
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *ON           *INKL
     C*** END   ADD *** 03/03/00 **************************************
     C*
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   ENDIF
     C*
     C*** BEG ADD *** 02/06/01 ****************************************
     C     *INKC         IFEQ      *ON
     C                   SETOFF                                       90
     C                   ENDIF
     C*** END ADD *** 02/06/01 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*** BEG ADD *** 12/01/98 ***************************************
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*** END ADD *** 12/01/98 ***************************************
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '6'           OPT6
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*** BEGIN ADD *** 11/17/98 **************************************
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*** BEGIN ADD *** 03/03/00 **************************************
     C     XXMODE        IFNE      'A'
     C     XXMODE        ANDNE     'D'
     C*** END   ADD *** 03/03/00 **************************************
     C*** BEGIN ADD *** 04/02/99 **************************************
     C********** XXMODE    ANDEQ*BLANKS                    DEL 11/29/99
     C*** END   ADD *** 04/02/99 **************************************
     C*
     C*** BEG ADD *** 10/13/99 **************************************
     C     @@CALL        IFEQ      '1'
     C                   MOVEL     'D'           ##MODE
     C                   ELSE
     C*
     C                   MOVEL     'C'           ##MODE
     C                   ENDIF
     C*** END ADD *** 10/13/99 **************************************
     C*
     C     CXKY          CHAIN     PCX01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*********  @@CALL    IFEQ '1'                        DEL 10/14/99
     C*********            MOVEL'D'       ##MODE           DEL 10/14/99
     C*********            ELSE                            DEL 10/14/99
     C*********                                            DEL 10/14/99
     C*********            MOVEL'C'       ##MODE           DEL 10/14/99
     C*********            ENDIF                           DEL 10/14/99
     C*
     C                   Z-ADD     DBRRN2        ##RRN
     C                   ELSE
     C*
     C     @@CALL        IFNE      '1'                                          ADD 10/14/99
     C                   MOVEL     'A'           ##MODE
     C                   ENDIF                                                  ADD 10/14/99
     C*
     C                   Z-ADD     *ZEROS        ##RRN
     C                   ENDIF
     C*
     C                   CALL      'F0120R01'
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVEL     *ON           RSTPGM
     C                   MOVEL     *OFF          *INKL
     C*
     C                   ELSE
     C*
     C                   SETON                                        LR
     C*
     C                   ENDIF
     C*** BEGIN ADD *** 03/03/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/03/00 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 11/17/98 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     8             SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     @@RRN2        CHAIN(N)  FMFNP                              99
     C*
     C                   MOVE      FNFND         SCFND
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C                   MOVE      FNSUB#        SCSUB#
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C*** BEGIN DEL *** 01/02/01 **************************************
     C********** XXMODE    IFEQ 'A'
     C*
     C**********           MOVELMODEA     SCMODE
     C*
     C**********           ELSE
     C*
     C**********           MOVE *BLANKS   SCMODE
     C*
     C**********           ENDIF
     C*** END   DEL *** 01/02/01 **************************************
     C*
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
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
** Column headings
State    Name
** Description--------------MISN
1066=Select                 YYN
203F3=Exit                  YYN
212F12=Cancel               YYN
214F14=DARTS                NNN
3
