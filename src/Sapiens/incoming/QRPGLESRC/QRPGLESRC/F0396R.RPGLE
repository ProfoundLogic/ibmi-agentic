     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0396R - WORK WITH HEADER AMOUNTS
     H*****************************************************************
     H*  Created: 07/13/98  C:62 Project: MC152                       *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  05/26/17  C:79  Support: 090/47213                           *
     H*                  Allow recon delete if check is voided        *
     H*                                                               *
     H*  12/09/15  C:79  Support: 146/38730                           *
     H*                  Allow recon delete if eombup=R or P          *
     H*                                                               *
     H*  10/29/07  C:47  PROJECT: MC309                               *
     H*                  ABILITY TO DELETE MOST RECENT RECONSIDERATION*
     H*                                                               *
     H*  12/01/00  C:79  PROJECT: FSMNT                               *
     H*                  IF EORCN=0 LOAD BLANKS INTO RECON REASON     *
     H*                                                               *
     H*  08/15/00  C:62  PROJECT: MCMNT                               *
     H*                  GET THE CORRECT RECON EXPLANATION.           *
     H*                                                               *
     H*  05/19/00  C:62  PROJECT: MC172                               *
     H*                  RECON IS NOW IMPORTANT SO ADD TO PARM LIST.  *
     H*                                                               *
     H*  02/21/00 C:62 PROJ: FSMNT                                    *
     H*                CONDITION OPT 17 WITH INSTALL OF CM.           *
     H*                                                               *
     H*  01/18/99 C:79 PROJ: 25MKO                                    *
     H*                1. Dont allow option 17 on vendor fee bills    *
     H*                                                               *
     H*  01/17/99 C:62 PROJ: MC152                                    *
     H*                ADDED OPT17=WW TIME/MATERIALS                  *
     H*                                                               *
     H*****************************************************************
     FF0396F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FFAEAL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEA01:DBRCDF)
     FFAEAL2    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEA01:DBRCD2)
     FFAEOP     IF   E             DISK
     FFMCXL     IF   E           K DISK
     F                                     RENAME(PCX01:PCX02)
     FFAESL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PES01:PES02)
     F*** BEGIN ADD *** 10/29/07 **************************************
     FFELGL4    IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** END   ADD *** 10/29/07 **************************************
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(16) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(16) ALT(OF)
     D*
     D DATA            S             70    DIM(2)
     D DTA1            S              1    DIM(70)
     D DTA2            S              1    DIM(70)
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Provider Bill Header-
     D                                      Amounts')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D***           'F0397R'              C         ACDPGM
     D ACDPGM          C                   CONST('F0396R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)
      *** BEG ADD *** 01/18/99 ****************************************
     D CLRFKY          C                   CONST('                    -
     D                                            NNN')
      *** END ADD *** 01/18/99 ****************************************
      *** BEG ADD *** 10/29/07 ****************************************
     D DELETD          C                   CONST('*Deleted')
      *** END ADD *** 10/29/07 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0          FOR FAEOP
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      WWRRN3        ##RRN             9 0
     C                   PARM      WWRRNA        ##RRNA            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* PARM LIST FOR DARTS API
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       #1RTCD            1
     C                   PARM      ##WCIN        #1WCIN            1
     C                   PARM      ##WCDK        #1WDCK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##KEY            40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKTRDT
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     ESKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C     *LIKE         DEFINE    EATRDT        KKTRDT
     C     *LIKE         DEFINE    #1WCIN        ##WCIN
     C     *LIKE         DEFINE    #1WDCK        ##WCDK
     C     *LIKE         DEFINE    EOEOB#        HLEOB#
     C     *LIKE         DEFINE    EARCN         SVRCN
     C*
     C/COPY SCOPYWW01C
     C*
      *** BEG ADD *** 10/29/07 ****************************************
     C*================================================================
     C* CHKSCR - Check security level of user
     C*================================================================
     C     CHKSCR        BEGSR
     C*
     C                   MOVE      'N'           XXAUTH            1
     C                   Z-ADD     *ZERO         UPLVL
     C                   MOVE      *BLANK        UPTYPE
     C*
     C* MAKE SURE THAT WE HAVE A VALID USER.
     C*
     C     Q1USER        CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C* GET RECON ALLOWED FLAG, SECURITY LEVEL & TYPE.
     C*
     C                   CALL      'FA715R'
P01  C                   PARM      *BLANK        @@RTCD
P02  C                   PARM      ##CO#         @@CO#
P03  C                   PARM      EASUB#        @@SUB#            5 0
P04  C                   PARM      EAFND         @@FND             3 0
P05  C                   PARM                    @@PVMC            1 0          Init Med
P06  C                   PARM                    @@PVPR            1 0          Init Prov
P07  C                   PARM                    @@WCUP            1            Allw Upd WCMS Clms
P08  C                   PARM                    @@CLDL            3 0          Clm Dlt Lvl
P09  C                   PARM                    @@CLDT            1            Clm Dlt Typ
P10  C                   PARM                    @@CLTL            3 0          Clm Xfer Lvl
P11  C                   PARM                    @@CLTT            1            Clm Xfer Typ
P12  C                   PARM                    @@SDCL            3 0          Settle Dt Lvl
P13  C                   PARM                    @@SDCT            1            Settle Dt Typ
P14  C                   PARM                    @@BSCL            3 0          BWC Sts Lvl
P15  C                   PARM                    @@BSCT            1            BWC Sts Typ
P16  C                   PARM                    @@DCDL            3 0          Del Diag Cd Lvl
P17  C                   PARM                    @@DCDT            1            Del Diag Cd Typ
P18  C                   PARM                    @@DCCL            3 0          Chg Diag St Lvl
P19  C                   PARM                    @@DCCT            1            Chg Diag St Typ
P20  C                   PARM                    @@C#CL            3 0          Clt Case Lvl
P21  C                   PARM                    @@C#CT            1            Clt Case Typ
P22  C                   PARM                    @@FIMR            5            RMCD: From IMR
P23  C                   PARM                    @@TIMR            5            RMCD: To IMR
P24  C                   PARM                    @@FRN             5            RMCD: From RN
P25  C                   PARM                    @@TRN             5            RMCD: To RN
P26  C                   PARM                    @@CS#L            3 0          St Clm#  Lvl
P27  C                   PARM                    @@CS#T            1            St Clm#  Typ
P28  C                   PARM                    @@DNSL            3 0          Ovr DNYD Lvl
P29  C                   PARM                    @@DNST            1            Ovr DNYD Typ
P30  C                   PARM                    @@NERC            5            RMCD: New Emplr
P31  C                   PARM                    @@MEUS           10            New Employer
P32  C                   PARM                    @@NWPA            5            RMCD: ADD PC
P33  C                   PARM                    @@CPAA            5            RMCD: 'A' OR 'N'
P34  C                   PARM                    @@CPAD            5            RMCD: 'D'
P35  C                   PARM                    @@PLCL            3 0          LIAB CHG LVL
P36  C                   PARM                    @@PLCT            1            LIAB CHG TYP
P37  C                   PARM                    @@CKDL            3 0          EOMB DEL W/CHK LVL
P38  C                   PARM                    @@CKDT            1            EOMB DEL W/CHK TYP
P39  C                   PARM                    @@RCAL            1            EOMB RECON ALLOWED
P40  C                   PARM                    @@RCSL            3 0          EOMB RECON SEC LEVEL
P41  C                   PARM                    @@RCST            1            EOMB RECON SEC TYPE
     C*
     C* COMPARE USER'S SECURITY LEVEL TO THE
     C*   RECON SECURITY LEVEL.
     C*
     C                   CALL      'SA006R'
     C                   PARM      UPLVL         @@ULVL            3 0          USER LEVEL
     C                   PARM      UPTYPE        @@UTYP            1            USER TYPE
     C                   PARM                    @@RCSL            3 0          EOMB RECON SEC LEVEL
     C                   PARM                    @@RCST            1            EOMB RECON SEC TYPE
     C                   PARM                    @@RTCD                         RETURN CODE
     C                   ENDIF                                                  *IN99 = OFF
     C*
     C* IF RECON IS ALLOWED AND
     C*   AND USER'S SECURITY IS MORE THAN RECON SECURITY LEVEL
     C*   THEN CHECK USER'S TYPE.
     C*
     C     @@RCAL        IFEQ      'Y'
     C     @@ULVL        ANDGE     @@RCSL
     C                   MOVE      'Y'           XXAUTH
     C                   ENDIF                                                  ULVL
     C*
     C     @@RTCD        IFEQ      'F'
     C                   MOVE      'N'           XXAUTH
     C                   ENDIF                                                  RTCD = 'F'
     C*
     C                   ENDSR
      *** END ADD *** 10/29/07 ****************************************
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
      *
      *** BEG ADD *** 10/29/07 ****************************************
      *
     C*   The delete option is only available to users with high enough
     C*   security.
     C*
     C*   Other conditions are:
     C*
     C*   It has to be the most current recon.
     C*
     C*   A zero recon can not be deleted from this screen.
     C*
     C*   The recon can not have a processed check.
     C*   (FAEAP:EAWCPC must be zero.
     C*   Also check WDPAP for EOB# (PAEOB#) and Recon# (PAEOBS).
     C*   If you find a record in this file, you have a processed check
     C*   and can not delete the recon.)
     C*
     C*   Bills that have been submitted for EDI should not be allowed.
     C*   (If FELGP:LGSTS = SN, TA, TE, TR, disallow delete.)
     C*
     C*   Bills that have been uploaded from an outside vendor should
     C*   not be allowed.  (FAEOP:EOMBUP must be blank.)
     C*
     C* ---------------------------------------------------------------
     C                   DO
     C* OPTION 4
     C     SCSEL         IFEQ      '4 '
     C*
     C*   The delete option is only available to users with high enough
     C*   security.
     C*
     C                   EXSR      CHKSCR
     C     XXAUTH        IFNE      'Y'
     C                   SETON                                        9081
     C                   LEAVE
     C                   END
     C*
     C*   It has to be the latest recon and can't be zero.
     C*
     C                   Z-ADD     *ZERO         SVRCN
     C     KEY           SETGT     DBRCDF
     C     KEY           READPE    DBRCDF                                 97
     C     *IN91         IFEQ      *OFF
     C                   Z-ADD     EARCN         SVRCN
     C                   END
     C     SVRCN         IFNE      SCRCN
     C                   SETON                                        9072
     C                   LEAVE
     C                   END
     C     SVRCN         IFEQ      *ZERO
     C                   SETON                                        9073
     C                   LEAVE
     C                   END
     C*
     C*** Beg Del *** 05/26/17 ****************************************
     C*** The recon can not have a processed check                     oided
     C****(FAEAP:EAWCPC must be zero.
     C****
     C**** EAWCPC        IFNE      *ZERO
     C***********        SETON                                        9074
     C***********        LEAVE
     C***********        END
     C*** End Del *** 05/26/17 ****************************************
     C*
     C*   Also check WDPAP for EOB# (PAEOB#) and Recon# (PAEOBS).
     C*   If you find a record in this file, you have a processed check
     C*   and can not delete the recon.)
     C*     First, make sure that WC is installed
     C*     and the group is linked.
     C*
     C     ##WCIF        IFEQ      'Y'
     C     ##WCIN        ANDEQ     'Y'
     C                   CALL      'WA039R'
     C                   PARM      *BLANK        @@RTCD            1
     C**                                           RTCD will be Y or N.
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C*** Beg Chg *** 05/26/17 ****************************************
     C**********         PARM                    @@FYR             3 0
     C**********         PARM                    @@CASE            7 0
     C**********         PARM                    @@EOB#            9 0
     C                   PARM      EOfyr         @@FYR             3 0
     C                   PARM      EOcase        @@CASE            7 0
     C                   PARM      EOeob#        @@EOB#            9 0
     C                   PARM      EOrcn         @@Rcn             3 0
     C*** End Chg *** 05/26/17 ****************************************
     C     @@RTCD        IFEQ      'Y'
     C                   SETON                                        9077
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*   Bills that have been submitted for EDI should not be allowed.
     C*   (If FELGP:LGSTS = SN, TA, TE, TR, disallow delete.)
     C*
     C     KEY1          CHAIN     PLG01                              98
     C     *IN98         IFEQ      *OFF
     C     LGSTS         IFEQ      'SN'
     C     LGSTS         OREQ      'TA'
     C     LGSTS         OREQ      'TE'
     C     LGSTS         OREQ      'TR'
     C                   SETON                                        9079
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*   Bills that have been uploaded from an outside vendor should
     C*   not be allowed.  (FAEOP:EOMBUP must be blank.)
     C*
     C     EOMBUP        IFNE      *BLANK
     C*** Beg Add *** 12/09/15 ********************************************
     C     EOMBUP        ANDNE     'P'
     C     EOMBUP        ANDNE     'R'
     C*** End Add *** 12/09/15 ********************************************
     C                   SETON                                        9080
     C                   LEAVE
     C                   END
     C*
     C                   ENDIF                                                  SCSEL = 4
      *** END ADD *** 10/29/07 ****************************************
      *** BEG ADD *** 01/18/99 ****************************************
      *
      * Do not allow Vendor Fee bills to process time and materials
     C     SCSEL         IFEQ      '17'
     C     EOBTYP        ANDEQ     'V'
     C*
     C                   SETON                                        7190
     C                   LEAVE
     C*
     C                   ENDIF
     C*
      *** END ADD *** 01/18/99 ****************************************
     C                   ENDDO
     C*
     C     ERRCKE        ENDSR
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
     C                   MOVE      *LOVAL        KKRCN
     C                   MOVE      *LOVAL        KKTRDT
     C                   MOVE      *BLANKS       SCHMRA
     C                   MOVE      *BLANKS       SCMRA
     C                   MOVE      *BLANKS       SCHOT1
     C                   MOVE      *BLANKS       SCOTHR
     C                   MOVE      *BLANKS       SCHOT2
     C                   MOVE      *BLANKS       SCOTH2
     C                   MOVE      *BLANKS       SCHNET
     C                   MOVE      *BLANKS       SCNDIS
     C                   MOVE      *BLANKS       SCREAS
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C* VIEW# 1 (Recon#)
     C*
     C   21              MOVEL(P)  SCPST1        KKRCN
     C*
     C* VIEW# 2 (Transaction date)
     C*
     C   22              MOVE      SCPST2        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   MOVEL     SCRCN         KKRCN
     C                   MOVEL     SCTRDT        KKTRDT
     C                   ENDSR
     C*================================================================
     C* KNDO - DARTS Subroutine
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   CALL      'FA703R'      DRTSPL
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - Work with Detail
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   MOVE      @@RRN         ##RRN
     C*
     C     @@CALL        IFEQ      '1'
     C                   MOVE      'D'           ##MODE
     C                   ENDIF                                                  @@CALL IFEQ ' '
     C*
     C                   CALL      'F0397R'
     C                   PARM                    ##MODE
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      Q1PGM         ##1PGM            6
     C                   ENDSR
     C*================================================================
     C* OPT14 - DARTS
     C*================================================================
     C     OPT14         BEGSR
     C*
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/17/99 **************************************
     C*================================================================
     C* OPT17 - Work with Time/Materials
     C*================================================================
     C     OPT17         BEGSR
     C*
     C*
     C     @@CALL        IFEQ      ' '
     C*
     C                   MOVEL     ' '           ##CALL            1
     C*
     C                   ELSE
     C*
     C                   MOVEL     '1'           ##CALL
     C*
     C                   ENDIF
     C*
     C                   CALL      'F3040R'
     C                   PARM                    ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN         ##RRN2
     C                   PARM      *ZEROS        ##RRN3            9 0
     C                   PARM      *BLANKS       ##CERT           15
     C                   PARM      *ZEROS        ##EXT             3 0
     C*** BEGIN ADD *** 05/19/00 **************************************
     C                   PARM      'Y'           ##EOB             1
     C                   PARM      SCRCN         ##RCN             3 0
     C*** END   ADD *** 05/19/00 **************************************
     C*
     C*
     C                   ENDSR
     C*** END   ADD *** 01/17/99 **************************************
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     DBRRN         WWRRNA            9 0
     C*
     C                   MOVE      EARCN         SCRCN
     C*
     C     SCRCN         IFEQ      *ZEROS
     C*
     C                   MOVEL(P)  '  0'         ALRCN             3
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SCRCN         QQNUM
     C*
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM            80
     C                   PARM      *BLANKS       QQMASK           10
     C*
     C                   MOVEL     QQNUM         ALRCN
     C*
     C                   ENDIF
     C*
     C                   MOVE(P)   EATRDT        SCTRDT
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ALTRDT           10
     C                   MOVEL(P)  ALTRDT        QQNUM            80
     C                   MOVEL(P)  'DATE'        QQMASK           10
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         ALTRDT
     C*
     C                   MOVEL(P)  EACHRG        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         ALCHRG           80
     C*
     C                   MOVEL(P)  EAPAID        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         ALPAID           80
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   MOVEL     *BLANKS       SCREAS
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C     ESKY          CHAIN     PES02                              99
     C*** BEGIN ADD *** 08/15/00 **************************************
     C     *IN99         DOWEQ     *OFF
     C*
     C     ESRCN         IFEQ      EARCN
     C     ESBLST        ANDEQ     'R'
     C*** END   ADD *** 08/15/00 **************************************
     C*** BEG ADD *** 12/01/00 ****************************************
     C     EARCN         ANDNE     *ZEROS
     C*** END ADD *** 12/01/00 ****************************************
     C                   MOVEL(P)  ESREAS        SCREAS
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   LEAVE
     C*
     C                   ENDIF
     C*
     C     ESKY          READE     PES02                                  99
     C*
     C                   ENDDO
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C     CXKY          CHAIN     PCX02                              99
     C                   MOVEL(P)  CXHMRA        SCHMRA
     C                   MOVEL(P)  CXHNET        SCHNET
     C                   MOVEL(P)  CXHOT1        SCHOT1
     C                   MOVEL(P)  CXHOT2        SCHOT2
     C*
     C                   MOVEL(P)  EAMRA         QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXMRA            16
     C                   MOVE(P)   XXMRA         SCMRA
     C*
     C                   MOVE      EAOTHR        SCOTHR
     C                   MOVEL(P)  EAOTHR        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXOTHR           16
     C                   MOVE(P)   XXOTHR        SCOTHR
     C*
     C                   MOVEL(P)  EAOTH2        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXOTH2           16
     C                   MOVE(P)   XXOTH2        SCOTH2
     C*
     C                   MOVEL(P)  EANDIS        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL(P)  QQNUM         XXNDIS           16
     C                   MOVE(P)   XXNDIS        SCNDIS
     C*
     C*
     C                   SELECT
     C*
     C     HD#           WHENEQ    1
     C*
     C                   CLEAR                   SCDATA
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     ALRCN         DTA1(Y)
     C                   ADD       6             Y
     C                   MOVEA     ALTRDT        DTA1(Y)
     C                   ADD       11            Y
     C                   MOVEA     ALCHRG        DTA1(Y)
     C                   ADD       15            Y
     C                   MOVEA     ALPAID        DTA1(Y)
     C                   ADD       16            Y
     C                   MOVEA     SCREAS        DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C*** LOAD VIEW# 2 FIELDS
     C*
     C     HD#           WHENEQ    2
     C                   CLEAR                   SCDATA
     C*
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     ALTRDT        DTA2(Y)
     C                   ADD       12            Y
     C                   MOVEA     ALRCN         DTA2(Y)
     C                   ADD       5             Y
     C                   MOVEA     ALCHRG        DTA2(Y)
     C                   ADD       15            Y
     C                   MOVEA     ALPAID        DTA2(Y)
     C                   ADD       16            Y
     C                   MOVEA     SCREAS        DTA2(Y)
     C                   MOVEA(P)  DTA2          SCDATA
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKN         CASEQ     *ON           KNDO                           F14=DARTS
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '14'          OPT14
     C*** BEGIN ADD *** 01/17/99 **************************************
     C     SCSEL         CASEQ     '17'          OPT17
     C*** END   ADD *** 01/17/99 **************************************
     C                   ENDCS
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
      *
     C*****                MOVELHD,2      SCHDL2    P
     C                   MOVE      *ON           *IN12
     C*** BEGIN ADD *** 02/21/00 **************************************
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN                         WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK                         WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C     ##MED         IFNE      'Y'
     C*
     C                   MOVEL(P)  CLRFKY        OFD(7)
     C*
     C                   ENDIF
     C*** END   ADD *** 02/21/00 **************************************
     C*
     C** Get standard screen headings
     C*
     C     @@RRN         CHAIN     PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C*                    CLEAR#1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN
     C                   PARM                    #1L1
     C**                   PARM           #1L2
     C                   MOVEL     #1L1          SCHD3
     C*                    MOVEL#1L2      SCHD4
     C*
     C                   MOVEL     EOEOB#        HLEOB#
     C                   MOVEL(P)  HLEOB#        QQNUM            80
     C                   MOVEL(P)  *BLANKS       QQMASK           10
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         SCEOB#
     C*
     C                   MOVEL     EONTWK        SCNTWK
     C*
     C                   Z-ADD     EOCHRG        SCTCHG
     C                   Z-ADD     EOPAID        SCTPAY
     C                   Z-ADD     EOMRA         SCTDIS                         TOTAL DISCOUNT
     C                   ADD       EONDIS        SCTDIS
     C                   ADD       EOOTHR        SCTDIS
     C                   ADD       EOOTH2        SCTDIS
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVEL     EOEOB#        KKEOB#
     C*
      *** BEG ADD *** 10/29/07 ****************************************
     C* LOAD RRN3 FOR CALL TO ACDPGM
     C*
     C     ESKY          CHAIN     PES02                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN3        WWRRN3            9 0
     C                   ENDIF
      *** END ADD *** 10/29/07 ****************************************
     C*
      *** BEG ADD *** 01/18/99 ****************************************
     C     EOBTYP        IFEQ      'V'
     C                   MOVEL     CLRFKY        OFD(7)
     C                   ENDIF                                                  EOBTYP=V
      *** END ADD *** 01/18/99 ****************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCD2                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCD2                             96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C*
     C     ##KL          IFEQ      *OFF
     C     ##KC          ANDEQ     *OFF
     C                   MOVEL(P)  DELETD        SCDATA
     C                   END
     C*
     C     ##KL          IFEQ      *ON
     C     ##KC          OREQ      *ON
     C     ##RRNA        CHAIN     PFRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVDBSC
     C                   Z-ADD     ##RRN         SCDBRR
     C                   ENDIF
     C                   ENDIF
     C*
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
Opt Rcn   Date              Charged           Paid  Reconsidered Reason
Opt Date        Rcn         Charged           Paid  Reconsidered Reason
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                NNY
1077=Detail Amounts         YYN
11414=DARTS                 NNN
11717=Time/Materials        YYN
203F3=Exit                  YYN
205F5=Refresh               NNY
206F6=Add                   NNN
211F11=Change View          YYY
212F12=Cancel               YYY
214F14=DARTS                YYN
215F15=Fold/Truncate        YYN
217F17=Subset               NNY
3
