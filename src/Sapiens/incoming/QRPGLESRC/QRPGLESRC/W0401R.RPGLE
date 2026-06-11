     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0401R - WORK WITH LOSS CONTROL TASKS
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 09/29/09 C:62  PROJECT OP135 67535                            *
     H*                Swap out client file with generic.             *
     H*                                                               *
     H* 07/22/99 C:61  PROJECT 102BH                                  *
     H*                Refresh after delete.                          *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*
     FW0401F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMVSP     IF   E             DISK
     F                                     RENAME(PVS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMVSL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PVS01:DBRCDF)
     F*
     F*  Loss Control Tracking File
     FWMLCP     IF   E             DISK
     F*
     F*  Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F*  User Profile File
     FWMUPL1    IF   E           K DISK
     F*
     F*** Begin Chg *** 09/29/09 **************************************
     F**********WTVLL   IF  E           K        DISK
     FWTGTL     IF   E           K DISK
     F*** End   Chg *** 09/29/09 **************************************
     F*
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(8) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(8) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCFND                  1      3  0
     D  QCEMP#                 4     12  0
     D  QCDIV                 13     17  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Loss Control Tasks')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0401R02')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
     D*
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*                                                                *********
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               CANCEL DATE
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
      *
     D                 DS
     D  VIEW                   1     40
     D  VWVSTP                 1      2
     D  VWVSDC                 4     33
     D  VWHRS                 36     40

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
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
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      LCFND         ##FND
     C                   PARM      LCEMP#        ##EMP#            9 0
     C                   PARM      LCDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C     SCAGTN        PARM                    ##ANAM           60
     C     SCPRD#        PARM                    ##PROD            5 0
     C     SCPRDN        PARM                    ##PNAM           60
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   PARM                    XXDEC             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKVSDT
     C                   KFLD                    KKSTF
     C                   KFLD                    KKVSTP
     C                   KFLD                    KKHRS
      ***** BEG ADD ******* 12/13/12 **********************************
     C                   KFLD                    KKDt
     C                   KFLD                    KKTIME
      ***** END ADD ******* 12/13/12 **********************************
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKVSDT
     C                   KFLD                    KKSTF
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    ##EMP#
     C                   KFLD                    ##DIV
     C*
     C     UPKY          KLIST
     C                   KFLD                    SCSTF
      *
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           QQID    4
     C**********           KFLD           QQKEY  10
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    QQID              4
     C                   KFLD                    QQKEY            10
     C*** End   Chg *** 09/29/09 **************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LCEMP#        KKEMP#
     C     *LIKE         DEFINE    LCDIV         KKDIV
     C     *LIKE         DEFINE    LCVSDT        KKVSDT
     C     *LIKE         DEFINE    LCSTF         KKSTF
     C     *LIKE         DEFINE    VSVSTP        KKVSTP
     C     *LIKE         DEFINE    VSHRS         KKHRS
     C     *LIKE         DEFINE    Q1PGM         XXPGM
      ***** BEG ADD ******* 12/13/12 **********************************
     C     *LIKE         DEFINE    VSDT          KKDT
     C     *LIKE         DEFINE    VSTIME        KKTIME
      ***** END ADD ******* 12/13/12 **********************************
     C*
     C                   Z-ADD     0             I1                3 0
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
      ***** BEG ADD ******* 07/22/99 **********************************
      *
      * Refresh WW screen after delete
      *
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
      *
      ***** END ADD ******* 07/22/99 **********************************
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
     C*
     C     @@RRN         IFNE      *ZERO
     C*
     C     @@RRN         CHAIN     PLC01                              60
     C*
     C                   MOVEL     LCEMP#        SCEMP#
     C                   MOVEL     LCDIV         SCDIV
     C                   MOVEL     LCEMP#        KKEMP#
     C                   MOVEL     LCDIV         KKDIV
     C*
     C                   Z-ADD     LCVSDT        KKVSDT
     C                   Z-ADD     LCVSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCVSDT
     C*
     C                   MOVEL     LCSTF         SCSTF
     C                   MOVEL     LCSTF         KKSTF
      ***** BEG ADD ******* 12/13/12 **********************************
     C                   CLEAR                   KKDT
     C                   CLEAR                   KKTIME
      ***** END ADD ******* 12/13/12 **********************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVE      *LOVAL        KKVSTP
      ***** BEG ADD ******* 12/13/12 **********************************
     C                   CLEAR                   KKDT
     C                   CLEAR                   KKTIME
      ***** END ADD ******* 12/13/12 **********************************
     C*
     C                   SELECT
     C*
     C     *IN21         WHENEQ    *ON
     C                   MOVE      SCPST1        KKVSTP
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
      ***** BEG ADD ******* 12/13/12 **********************************
     C                   Z-ADD     SCDT          KKDT
     C                   Z-ADD     SCTIME        KKTIME
     C                   MOVEL     SCVSTP        KKVSTP
     C                   Z-ADD     SCHRS         KKHRS
      ***** END ADD ******* 12/13/12 **********************************
     C                   ENDSR
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFND         QCFND
     C                   Z-ADD     SCEMP#        QCEMP#
     C                   Z-ADD     SCDIV         QCDIV
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0400R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     VSVSTP        VWVSTP
     C*
     C*  Edit hours for output
     C*
     C                   MOVE      *BLANKS       EDTMSK
     C                   MOVE      2             XXDEC
     C*
     C                   MOVEL(P)  VSHRS         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         VWHRS
     C*
     C                   MOVEL     'LTRK'        QQID
     C                   MOVEL     VSVSTP        QQKEY
     C*** Begin Chg *** 09/29/09 **************************************
     C********** VLKY      CHAINPVL01                60
     C**********           MOVELVLDESC    VWVSDC
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL     GTDESC        VWVSDC
     C*** End   Chg *** 09/29/09 **************************************
     C*
     C                   MOVEL     VIEW          SCDATA
      ***** BEG ADD ******* 12/13/12 **********************************
     C                   Z-ADD     VSDT          SCDT
     C                   Z-ADD     VSTIME        SCTIME
     C                   MOVEL     VSVSTP        SCVSTP
     C                   Z-ADD     VSHRS         SCHRS
      ***** END ADD ******* 12/13/12 **********************************
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
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
     C*
     C                   MOVE      ##FND         SCFND
     C                   MOVEL     ##FNAM        SCFNAM
      *
      *  GET AGENT/PRODUCER INFORMATION
      *
     C                   Z-ADD     *ZEROS        ##FYR
      *
     C                   CALL      AGTAPI        AGTPL
      *
      *  GET EMPLOYER NAME
      *
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL     EMNAME        SCEMPN
     C*
     C     UPKY          CHAIN     PUP01                              60
     C                   MOVEL     UPUSNM        SCSTFN
      *
      *  GET CANCEL DATE
      *
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
     C*
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
Opt     Type of Visit                   Hours
** Description--------------MISN
1022=Change                 YYY
1044=Delete                 YYY
1055=Display                YYY
203F3=Exit                  YYY
206F6=Add                   YYY
212F12=Cancel               YYY
214F14=DARTS                YYY
3
