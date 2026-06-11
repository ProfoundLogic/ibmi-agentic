     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr***dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*  F0267R - WORK WITH SELECTED SS#    C:62 01/16/97             *
     H*                                                               *
     H*  CALLED FROM: F0268R                                          *
     H*  CALLS TO:                                                    *
     H*  VIA F0269C   F0270R                                          *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20 C:212 Project : SY420                               *
     H*                 Mask SSN                                      *
     H*                                                               *
     H*  08/11/00 C:62  Project: MC235                                *
     H*                 CHANGE LOGICAL WHERE ACDT IS DESCENDING.      *
     H*                                                               *
     H*  10/11/99 C:56  Project: MC082 #20035                         *
     H*                 Add parm to indicate wheter no to subset on   *
     H*                 DOI.                                          *
     H*                                                               *
     H*  01/26/98 C:62  PROJECT: MC145                                *
     H*                 1) SA001R ADDED TO DEAL WITH ACCESS FLAGS     *
     H*                 2) ZERO SUPPRESSION.
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FF0267F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** BEGIN CHG *** 08/11/00 **************************************
     F**********FMCMLB  IF  E           K        DISK
     FFMCMLS    IF   E           K DISK
     F*** END   CHG *** 08/11/00 **************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCM01:DBRCDF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(6) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(6) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D                 DS
     D  SCSS#                  1      9
     D  SCSS#1                 1      3
     D  SCSS#2                 4      5
     D  SCSS#3                 6      9
     D*** BEG ADD *** 10/11/99 ****************************************
     D CLROPT          C                   CONST('01                  -
     D                                            NNN')
     D SELOPT          C                   CONST('011=Select          -
     D                                            NNY')
     D*** END ADD *** 10/11/99 ****************************************
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Selected SS#')
     D SLMSGI          C                   CONST('MC49998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0260R')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@SS#             9
     C                   PARM                    @@ACDT            8 0
     C*** BEG ADD *** 10/11/99 ****************************************
     C                   PARM                    @@SDOI            1            Y = subset on doi
     C*** END ADD *** 10/11/99 ****************************************
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##SS#             9
     C                   PARM                    ##ACDT            8 0
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FND             3 0
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C     ACSSPL        PLIST
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
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
     C*
     C* DATE CONVERSION FROM *MDYY TO *CYMD.
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@SS#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@SS#
     C                   KFLD                    KKACDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C*** BEG ADD *** 10/11/99 ****************************************
     C     *LIKE         DEFINE    CMACDT        ZZACDT
     C*** END ADD *** 10/11/99 ****************************************
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
     C*
     C                   SELECT
     C*
     C*** BEG ADD *** 10/11/99 ****************************************
     C     SCSEL         WHENEQ    '1'
     C     @@SDOI        ANDNE     'Y'
     C*
     C                   SETON                                        9093
     C*** END ADD *** 10/11/99 ****************************************
     C*
     C     SCSEL         WHENEQ    '2'
     C     SCSEL         OREQ      '5'
     C*
     C                   MOVEL     *BLANKS       ##SS#
     C                   MOVE      *ZEROS        ##ACDT
     C*** BEGIN ADD *** 08/08/00 **************************************
     C                   Z-ADD     *ZEROS        ##SUB#
     C                   Z-ADD     *ZEROS        ##FND
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCKC - Additional errors on subfile selection
     C*================================================================
     C     ERRCKC        BEGSR
     C*
     C                   MOVE      *OFF          OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C*** BEGIN ADD *** 11/30/99 **************************************
     C*
     C* HAS NOTHING TO DO WITH SUBSET BUT NEEDS TO BE HERE BECAUSE
     C* OF COPYBOOK LAYOUT.
     C*
     C     @@SDOI        IFNE      'Y'
     C*
     C                   MOVEL     'A'           @@CALL
     C                   MOVE      *ZEROS        @@RRN
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDIF
     C*** END   ADD *** 11/30/99 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C*** BEGIN CHG *** 08/11/00 **************************************
     C**********           MOVE *LOVAL    KKACDT
     C                   MOVE      *HIVAL        KKACDT
     C*** END   CHG *** 08/11/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        XXACDT            8 0
     C*
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKACDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVE      SCDATE        KKACDT
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     @@SS#         ##SS#
     C                   MOVE      @@ACDT        ##ACDT
     C*** BEGIN ADD *** 08/08/00 **************************************
     C     Q1PRMS        IFGT      7
     C                   Z-ADD     @@SUB#        ##SUB#
     C                   Z-ADD     @@FND         ##FND
     C                   ENDIF
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVE      CMACDT        SCDATE
     C*
     C                   MOVE      CMSUB#        SCSUB#
     C                   MOVE      CMFND         SCFND
     C                   MOVE      CMFYR         SCYEAR
     C                   MOVE      CMCASE        SCCASE
     C                   MOVEL     CMACDS        SCACDS
     C                   MOVEL     CMSTAT        SCSTAT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C                   SETON                                        90
     C*
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
     C* API TO RETRIEVE ACCESS FLAGS
     C*
     C                   CALL      'SA001R'      ACSSPL
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     9             SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     @@SS#         SCSS#
      /Free
       scSS# = MaskData('SSN':'@@SS#':@@SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C     KEY1          CHAIN(N)  DBRCDF                             99
     C*
     C     CMLSN         CAT(P)    ',':0         XXNAME           31
     C     XXNAME        CAT(P)    CMFSN:1       SCCLNM
     C*
     C*** BEGIN DEL *** 11/30/99 **************************************
     C*** BEG ADD *** 10/11/99 ****************************************
     C********** @@SDOI    IFNE 'Y'
     C*** END ADD *** 10/11/99 ****************************************
     C**********           MOVEL'A'       @@CALL
     C**********           MOVE *ZEROS    @@RRN
     C**********           MOVE *ON       OUTFLG
     C*** BEG ADD *** 10/11/99 ****************************************
     C**********           ENDIF
     C*** END ADD *** 10/11/99 ****************************************
     C*** END   DEL *** 11/30/99 **************************************
     C*
     C                   Z-ADD     0             SCDBRR
     C                   MOVEL     @@SS#         ##SS#
     C                   MOVE      @@ACDT        ##ACDT
     C*** BEGIN ADD *** 08/08/00 **************************************
     C     Q1PRMS        IFGT      7
     C                   Z-ADD     @@SUB#        ##SUB#
     C                   Z-ADD     @@FND         ##FND
     C                   ENDIF
     C*** END   ADD *** 08/08/00 **************************************
     C*
     C*** BEG ADD *** 10/11/99 ****************************************
     C*
     C* IF @@SDOI = Y, WE WANT THE NORMAL SELECTION OPTIONS TO BE
     C* AVAILABLE, THUS MUST RESET CALL PARM TO 2
     C*
     C     @@SDOI        IFEQ      'Y'
     C                   MOVEL(P)  SELOPT        OFD(1)
     C                   ELSE
     C                   MOVEL(P)  CLROPT        OFD(1)
     C                   ENDIF                                                  @@SDOI
     C*** END ADD *** 10/11/99 ****************************************
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
     C     SF1UPD        BEGSR                                                             C
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C     @@SDOI        IFNE      'Y'
     C*
     C                   MOVEL     'C'           @@CALL
     C*
     C                   ENDIF
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C*** BEG ADD *** 10/11/99 ****************************************
     C                   Z-ADD     @@ACDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZACDT
     C     @@SDOI        IFEQ      'Y'
     C     @@ACDT        ANDNE     *ZEROS
     C     ZZACDT        ANDNE     CMACDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  @@SDOI ' Y
     C*** END ADD *** 10/11/99 ****************************************
     C*
     C                   ENDSR
** Column headings
 Injury     Sub# Grp  Yr    Case Accident description                 Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
206F6=Add                   YNN
212F12=Cancel               YYY
3
