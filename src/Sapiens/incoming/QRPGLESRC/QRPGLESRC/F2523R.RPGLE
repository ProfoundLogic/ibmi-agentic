     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F2523R - Work With Skilled Nursing Facilities
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:  F2523R01 - MAINT - Skilled Nursing                 *
     H*                                                               *
     H* CREATED:   06/28/01 C:61  PROJECT 114MCG                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/18/08 C:198 Project: PA046                                 *
     H*                Added NPI# after Effective Date.               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09       Inquiry mode                                        *
     H*  72       Invalid Effective Date                              *
     H*  88       Invalid Effective Date                              *
     H*  89       Invalid Status                                      *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
      *
      *
     FF2523F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FFASNPPA   IF   E             DISK
     F                                     RENAME(PSN01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FFASNLPA   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PSN01:DBRCDF)
      *
      * State Code
     FSTABL     IF   E           K DISK
      *
      *
      *
      /COPY SCOPYWW01D
     D HB              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
     D                 DS
     D  VIEW1                  1     74
     D  V1MED#                 1     10
     D  V1EFDT                13     22
     D*** BEG ADD *** 11/18/08 ****************************************
     D  V1NPI#                26     37
     D*** END ADD *** 11/18/08 ****************************************
     D*** BEG CHG *** 11/18/08 ****************************************
     D*********                              30  40 V1INPT
     D*********                              45  50 V1OUPT
     D*********                              60  60 V1STAT
     D  V1INPT                40     50
     D  V1OUPT                55     60
     D  V1STAT                72     72
     D*** END CHG *** 11/18/08 ****************************************
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Skilled Nursing Fac-
     D                                     ilities')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2523R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@ST              2
     C                   PARM                    @@MED#           15            10A LEFT JUST
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  CONVERTS DATE FROM MDYY TO CYMD
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY           KLIST
     C                   KFLD                    KKMED#
     C                   KFLD                    KKEFDT
      *
     C     STKY          KLIST
     C                   KFLD                    KKST
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    SNMED#        KKMED#
     C     *LIKE         DEFINE    SNEFDT        KKEFDT
     C     *LIKE         DEFINE    SNEFDT        S1EFDT
     C     *LIKE         DEFINE    STKEY         KKST
      *
      *
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
      *
B01  C     SSEFDT        IFNE      *ZEROS
B01  C     SSSTAT        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
      *
     C                   ENDSR                                                  CHKSSM
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
      *
      *  Effective Date
      *
     C     SSEFDT        IFNE      *ZEROS
      *
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSEFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        8890
     C                   ELSE
      *
     C                   Z-ADD     SSEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S1EFDT
     C                   ENDIF                                                  SRERR
      *
     C                   ENDIF                                                  SSEFDT
      *
      * Status
      *
     C  N90SSSTAT        IFNE      ' '
     C     SSSTAT        ANDNE     'A'
     C     SSSTAT        ANDNE     'I'
     C                   SETON                                        8990
     C                   ENDIF                                                  SSSTAT
      *
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCKP - Error checking for the F21 prompt screen
      *================================================================
     C     ERRCKP        BEGSR
      *
     C     *IN90         IFEQ      *OFF
     C     SPEFDT        ANDNE     *ZEROS
      *
     C                   Z-ADD     SPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SPEFDT
      *
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   ENDIF                                                  SRERR='1'
      *
     C                   ENDIF                                                  SPEFDT NE 0
      *
     C                   ENDSR                                                  ERRCKP
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
      *
     C     @@CALL        IFEQ      '2'
     C                   MOVEL     'A'           SSSTAT
     C                   ENDIF
      *
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
     C                   MOVE      *LOVAL        KKMED#
     C                   MOVE      *LOVAL        KKEFDT
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C     *IN21         IFEQ      *ON
     C                   MOVEL(P)  SCPST1        KKMED#
      *
     C                   Z-ADD     SCPSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPSTD
      *
     C                   Z-ADD     SCPSTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C                   ENDIF                                                  *IN21
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C   21              MOVEL     SCMED#        KKMED#
     C   21              MOVE      SCEFDT        KKEFDT
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * KVDO - F21=PRINT LIST
      *================================================================
     C     KVDO          BEGSR
     C                   Z-ADD     *ZEROS        SPEFDT
      *
     C     *IN90         DOUEQ     *OFF
     C                   EXFMT     WDW01
      *
     C     *INKL         IFEQ      *OFF
     C                   EXSR      ERRCKP
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      ##CO#         #1CO#
     C                   MOVE      SPEFDT        #1EFDT
      *
     C                   CALL      'F7052C'
     C                   PARM                    #1CO#             3
     C                   PARM      SCST          #1ST              2
     C                   PARM                    #1EFDT            8
     C                   PARM      *BLANK        #1RTCD            1
     C                   PARM      'QPRINT'      #1QUE            10
     C                   PARM      'D'           #1MODE            1
     C                   ENDIF                                                  *IN90=*OFF
      *
     C                   ENDIF                                                  *INKL=*OFF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  KVDO
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL(P)  SNMED#        SCMED#
     C*** BEG ADD *** 11/18/08 *************************************************
     C                   MOVEL(P)  SNNPI#        SCNPI#
     C*** END ADD *** 11/18/08 *************************************************
     C                   Z-ADD     SNEFDT        SCEFDT
     C                   Z-ADD     SNINPT        SCINPT
     C                   Z-ADD     SNOUPT        SCOUPT
     C                   MOVEL(P)  SNSTAT        SCSTAT
      *
     C                   SELECT
      *
      * VIEW #1
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  HB(1)         SCHDLB
     C                   MOVEL(P)  SNMED#        V1MED#
      *
      *    Effective Date
      *
     C                   Z-ADD     SNEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##NUM
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM      'DATE'        ##MASK
      *
     C                   MOVEL(P)  ##NUM         V1EFDT
     C*** BEG ADD *** 11/18/08 *******************************************
      *    NPI#
      *
     C                   MOVEL(P)  SNNPI#        V1NPI#
     C*** END ADD *** 11/18/08 *******************************************
      *
      * Inpatient Allowance
      *
     C                   MOVEL(P)  SNINPT        ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    ##MASK
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      2             ##DEC             3 0
      *
     C                   MOVEL(P)  ##NUM         V1INPT
      *
      * Outpatient Allowance
      *
     C                   MOVEL(P)  SNOUPT        ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    ##MASK
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##NUM            80
     C                   PARM                    ##MASK           10
     C                   PARM      2             ##DEC             3 0
      *
     C                   MOVEL(P)  ##NUM         V1OUPT
      *
     C                   MOVE      SNSTAT        V1STAT
     C                   MOVEL(P)  VIEW1         SCDATA
      *
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OPT20 - SUBROUTINE FOR OPTION 20
      *================================================================
      *
      * OPTION 20=CHANGE STATUS
      *
     C     OPT20         BEGSR
     C                   MOVE      'S'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDSR                                                  OPT20
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKV         CASEQ     *ON           KVDO                           F21=Print
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
      *
     C     SCSEL         CASEQ     '20'          OPT20                          Change Status
     C                   ENDCS
      *
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@CALL        COMP      '1'                                    09    INQUIRY
      *
      * DISPLAY STATE CODE AND STATE NAME IN HEADER
      *
     C                   MOVEL(P)  @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C     STKY          CHAIN     PST01                              60
     C  N60              MOVEL(P)  STNAME        SCSTNM
     C   60              MOVEL(P)  *BLANKS       SCSTNM
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY           SETGT     DBRCDF                             96
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY           SETLL     DBRCDF                             96
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
      *
     C                   MOVEL(P)  SCMED#        @@MED#
      *
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
      *
      *  Effective date
      *
     C     SSEFDT        IFNE      *ZEROS
     C     S1EFDT        ANDNE     SNEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Status
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     SNSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
                                         Inpatient     Outpatient
**
Medicare #   Eff Date    NPI#            Allowance     Allowance    Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YYN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
217F17=Subset               YYY
221F21=Print list           YYN
3
