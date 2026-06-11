     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0680R - AGENT COMMISSION FILE SEARCH
     H*****************************************************************
     H* (C) Copyright Progressive Data Solutions, Inc., 1999 - 2004
     H*****************************************************************
     H*
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/15/13 C:65  PROJECT: WC2116                                *
     H*                Manually set function keys for prompt.         *
     H*                                                               *
     H* 06/07/04 C:132 PROJECT: WC1165                                *
     H*              - Change subset screen to add addition fields.   *
     H*                                                               *
     H* 05/18/04 C:132 PROJECT WC1165                                 *
     H*              - Change prompt screen to remove Policy year.    *
     H*              - Add new fields to WMACL5 to display in Posting *
     H*                Date order for First View.                     *
     H*              - Add F11=Change View to display in Employer/Div *
     H*                then Posting Date order.                       *
     H*                                                               *
     H*                                                               *
     H* 06/16/03 C:61  PROJECT WC577                                  *
     H*                Add second info.  Display Check# instead of    *
     H*                Transaction Date.                              *
     H*                                                               *
     H* 05/13/02 C:61  SUPPORT WA099 31974                            *
     H*                Add Group Policy Year Search.                  *
     H*                                                               *
     H* 02/18/02 C:136 SUPPORT# WA114 31305                           *
     H*                EXPANDED DAMT TO ACCOMODATE NEGATIVE SIGN      *
     H*                                                               *
     H* 02/02/00  C:33  SEQ#: WA090/21563                             *
     H*                 SWITCHED DATE HEADINGS.                       *
     H*                                                               *
     H*  REWRITE: 02/10/99   C:33   PROJECT: WC577  SEQ#: WA100/16244 *
     H*****************************************************************
      *
     FW0680F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMACP     IF   E             DISK
     F                                     RENAME(PAC01:PFRCDF)
     F*
      *
      ***** BEG CHG ***** 05/18/04 ************************************
     F*  Primary logical file definition (for displaying items)
     F***********WMACL4  IF  E           K        DISK
     FWMACL5    IF   E           K DISK
      ***** END CHG ***** 05/18/04 ************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAC01:DBRCDF)
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     F*  Second View logical file definition (for displaying items)
     FWMACL8    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAC01:DBRCD2)
      ***** END ADD ***** 05/18/04 ************************************
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDS)
     FWDF1L     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAHP     IF   E             DISK
     F                                     RENAME(PAH01:PAH0P)
     FWMAAL1    IF   E           K DISK
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA0P)
     FWMEML     IF   E           K DISK
     F                                     INFDS(EMFDS)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM0P)
      /COPY SCOPYWW01D
     D*
      ***** BEG DEL ******* 06/16/03 **********************************
     D*********           HD      1   1 78               Column headings
     D*********           OF      1  10  1   OFD    35   Options, Function
      ***** END DEL ******* 06/16/03 **********************************
      ***** BEG ADD ******* 06/16/03 **********************************
     D*
      ***** BEG CHG ***** 05/18/04 ************************************
     D*************       HD      1   2 78               Column headings
     D HD              S             78    DIM(4) CTDATA PERRCD(1)              Column headings
     D**************      OF      1  11  1   OFD    35   Options, Function
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
      ***** END CHG ***** 05/18/04 ************************************
     D*
      ***** END ADD ******* 06/16/03 **********************************
     D*
     D***PY SCOPYWW01E
     D*
     D FNFDS           DS
     D  FNRRN                397    400B 0
     D EMFDS           DS
     D  EMRRN                397    400B 0
     D                 DS
     D  DATA                   1     78
      ***** BEG CHG ***** 05/18/04 ************************************
     D**********                              1   9 DEMP#
     D**********                             11  15 DDIV
     D**********                             17  21 DPROD
     D**********                             23  32 DPODT
     D  DPODT                  1     10
     D  DEMP#                 13     21
     D  DDIV                  24     28
     D  DFYR                  31     33
     D  DPROD                 36     40
     D**************                         34  43 DTRDT
     D**************                         45  46 DCOCD
     D**************                         48  48 DCOTP
     D  DCOCD                 43     44
     D  DCOTP                 47     47
      ***** END CHG ***** 05/18/04 ************************************
      *** BEG CHG **** 02/18/02 ************************************************
     D************************************** 50  67 DDESC
     D************************************** 69  78 DAMT
     D  DDESC                 50     66
     D  DAMT                  68     78
      *** END CHG **** 02/18/02 ************************************************
      ***** BEG ADD ******* 06/16/03 **********************************
      *
     D                 DS
     D  DATA2                  1     78
      ***** BEG CHG ***** 05/18/04 ************************************
     D**********                              1   9 D2EMP#
     D**********                             11  15 D2DIV
     D**********                             17  21 D2PROD
     D**********                             23  32 D2PODT
     D**********                             35  44 D2CHCK
     D**********                             45  46 D2COCD
     D**********                             48  48 D2COTP
     D**********                             50  66 D2DESC
     D  D2PODT                 1     10
     D  D2EMP#                13     21
     D  D2DIV                 24     28
     D  D2FYR                 31     33
     D  D2PROD                36     40
     D  D2TRDT                43     52
     D  D2CHCK                55     64
      ***** END CHG ***** 05/18/04 ************************************
     D  D2AMT                 68     78
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     D                 DS
     D  DATA23                 1     78
      *
     D  D3EMP#                 1      9
     D  D3DIV                 12     16
     D  D3PODT                19     29
     D  D3FYR                 31     33
     D  D3PROD                36     40
     D  D3COCD                43     44
     D  D3COTP                47     47
     D  D3DESC                50     66
     D  D3AMT                 68     78
      *
     D                 DS
     D  DATA24                 1     78
      *
     D  D4EMP#                 1      9
     D  D4DIV                 12     16
     D  D4PODT                19     29
     D  D4FYR                 31     33
     D  D4PROD                36     40
     D  D4TRDT                43     52
     D  D4CHCK                55     64
     D  D4AMT                 68     78
      ***** END ADD ***** 05/18/04 ************************************
      *
      ***** END ADD ******* 06/16/03 **********************************
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Commission Inquiry')
     D CRCONS          C                   CONST('Select Criteria for')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0680R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
      ***** BEG CHG ******* 06/16/03 **********************************
     D*********     1                     C         INFOMX
     D INFOMX          C                   CONST(2)
      ***** END CHG ******* 06/16/03 **********************************
      ***** BEG CHG ***** 05/18/04 ************************************
     D********      1                     C         VIEWMX
     D VIEWMX          C                   CONST(2)
      ***** END CHG ***** 05/18/04 ************************************
     D SF1PAG          C                   CONST(12)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@PRDS           40
     C                   PARM                    @#CALL            1
     C                   PARM                    @@FYR             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@AGT#            5
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@FND             3
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
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   PARM                    XXDEC             3 0
     C*
      ***** BEG ADD ***** 05/18/04 ************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C*
     C*  GROUP DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      ***** BEG DEL ***** 05/18/04 ************************************
     C***********          KFLD           SCFYR
      ***** END DEL ***** 05/18/04 ************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      ***** BEG DEL ***** 05/18/04 ************************************
     C*************        KFLD           SCFYR
      ***** END DEL ***** 05/18/04 ************************************
     C*
      ***** BEG DEL ***** 05/18/04 ************************************
     C*********  KEY       KLIST
     C*************        KFLD           ##CO#
     C*************        KFLD           ##FND
     C*************        KFLD           SCFYR
     C*************        KFLD           SCAGT#
     C*************        KFLD           KKEMP#
     C*************        KFLD           KKDIV
     C*************        KFLD           KKPROD
     C*************        KFLD           KKPODT
     C*************        KFLD           KKTIME
     C**********
     C********** KEY1      KLIST
     C**********           KFLD           ##CO#
     C**********           KFLD           ##FND
     C*********            KFLD           SCFYR
     C*********            KFLD           SCAGT#
     C*********
     C*********  KEY2      KLIST
     C*********            KFLD           ##CO#
     C*********            KFLD           ##FND
     C*********            KFLD           SCFYR
     C*********            KFLD           SCAGT#
     C*********            KFLD           SSEMP#
     C*********            KFLD           SSDIV
      ***** END DEL ***** 05/18/04 ************************************
     C*
      ***** BEG ADD ***** 05/18/04 ************************************
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C                   KFLD                    KKPODT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C                   KFLD                    KKPODT
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C                   KFLD                    KKPODT
     C*
     C     KY            KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKPODT
     C                   KFLD                    KKFYR
     C                   KFLD                    KKPROD
     C                   KFLD                    KKTIME
     C*
     C     KY1           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C*
     C     KY2           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     KY3           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    SCAGT#
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKPODT
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @#CALL        @@CALL
     C     *LIKE         DEFINE    @@FND         QQFND
     C     *LIKE         DEFINE    @@FYR         QQFYR
     C     *LIKE         DEFINE    @@AGT#        QQAGT#
     C     *LIKE         DEFINE    @@EMP#        QQEMP#
     C     *LIKE         DEFINE    @@DIV         QQDIV
     C     *LIKE         DEFINE    @@RTCD        QQRTCD
     C     *LIKE         DEFINE    ACFYR         KKFYR
     C     *LIKE         DEFINE    ACAGT#        KKAGT#
     C     *LIKE         DEFINE    ACEMP#        KKEMP#
     C     *LIKE         DEFINE    ACDIV         KKDIV
     C     *LIKE         DEFINE    ACPROD        KKPROD
     C     *LIKE         DEFINE    ACPODT        KKPODT
     C     *LIKE         DEFINE    ACTIME        KKTIME
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    *IN60         XXCRT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     SSDIV         ORNE      *ZEROS
      ***** BEG ADD ***** 05/18/04 ************************************
     C     SSPFDT        ORNE      *ZEROS
      ***** END ADD ***** 05/18/04 ************************************
      ***** BEG ADD ***** 06/07/04 ************************************
     C     SSTFDT        ORNE      *ZEROS
     C     SSPFYR        ORNE      *ZEROS
     C     SSPROD        ORNE      *ZEROS
      ***** END ADD ***** 06/07/04 ************************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSEMP#<>*ZEROS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT  - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*================================================================
     C* EMPAGT - Validate the employer/div/agt#
     C*================================================================
     C*
     C     EMPAGT        BEGSR
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C   60              SETON                                        72  90
     C*
     C*  VALIDATE THE AGENT NUMBER FOR EMP# AND DIV
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     AHKY          CHAIN     PAH01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     SCAGT#        ANDNE     AHAGT#
     C     AHKY          READE     PAH01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C   60              SETON                                        74  90
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDSR                                                  EMPAGT
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
      ***** BEG ADD ***** 06/07/04 ************************************
     C*
     C*   VALIDATE the Policy Year if Entered
     C     SSPFYR        IFGT      *ZEROS
     C     SSPFYR        ANDLT     80
     C     SSPFYR        ORGT      *ZEROS
     C     SSPFYR        ANDGT     140
     C                   SETON                                        30  90
     C                   ELSE
     C                   Z-ADD     SSPFYR        KKFYR
     C                   ENDIF
     C*
     C*   VALIDATE the Entered FROM Date - POSTING DATE RANGE
     C                   Z-ADD     *ZEROS        X7PFDT            7 0
     C                   Z-ADD     *ZEROS        X7PTDT            7 0
     C*
     C     SSPFDT        IFGT      *ZEROS
     C                   Z-ADD     SSPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7PFDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        31  90
     C                   ENDIF
     C                   ENDIF                                                  SSPFDT <> ZEROS
     C*
     C*   VALIDATE the Entered TO Date - POSTING DATE RANGE
     C     *IN90         IFEQ      *OFF
     C     SSPTDT        ANDGT     *ZEROS
     C                   Z-ADD     SSPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7PTDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        33  90
     C                   ENDIF
     C                   ENDIF                                                  SSPTDT <> ZEROS
     C*
     C*   VALIDATE the FROM Posting Date < = Posting TO Date
     C     *IN90         IFEQ      *OFF
     C     SSPFDT        ANDGT     *ZEROS
     C     X7PFDT        ANDGT     X7PTDT
     C                   SETON                                        32  90
     C                   ENDIF                                                  SSPTDT <> ZEROS
     C*
     C*   VALIDATE the Entered FROM Date - TRANSACTION DATE RANGE
     C                   Z-ADD     *ZEROS        X7TFDT            7 0
     C                   Z-ADD     *ZEROS        X7TTDT            7 0
     C*
     C     SSTFDT        IFGT      *ZEROS
     C                   Z-ADD     SSTFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TFDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        34  90
     C                   ENDIF
     C                   ENDIF                                                  SSPFDT <> ZEROS
     C*
     C*   VALIDATE the Entered TO Date - Transaction Date Range
     C     *IN90         IFEQ      *OFF
     C     SSTTDT        ANDGT     *ZEROS
     C                   Z-ADD     SSTTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TTDT
     C     SRERR         IFEQ      *ON
     C                   SETON                                        36  90
     C                   ENDIF
     C                   ENDIF                                                  SSPTDT <> ZEROS
     C*
     C*   VALIDATE the FROM Transaction Date < = Transaction TO Date
     C     *IN90         IFEQ      *OFF
     C     SSTFDT        ANDGT     *ZEROS
     C     X7TFDT        ANDGT     X7TTDT
     C                   SETON                                        35  90
     C                   ENDIF                                                  SSPTDT <> ZEROS
      *
      ***** END ADD ***** 06/07/04 ************************************
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* ERRCKC - Error checking for search criteria screen (SCRCRT)
     C*================================================================
     C*
     C     ERRCKC        BEGSR
     C*
     C*  VALIDATE THE YEAR
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C   60              SETON                                        71  90
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C*  VALIDATE THE AGENT# FOR THE GROUP & GET AGENT NAME
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C   60              SETON                                        74  90
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   CLEAR                   SCAGNM
     C     AGKY          CHAIN     PAG01                              60
     C                   MOVEL(P)  AGNAME        SCAGNM
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C*
     C*  If have starting Commission Date, validate it, then check for
     C*  Employer number, must have date if Employer number entered.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCSPOD        IFGT      *ZEROS
     C                   Z-ADD     SCSPOD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C     SRERR         IFNE      '0'
     C                   SETON                                        90  73
     C                   ENDIF                                                  SRERR <> *ZEROS
      *
     C                   ELSE
      *
      * If don't have Start Date, then employer number is not allowed
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C                   SETON                                        90  73
     C                   ENDIF
      *
     C                   ENDIF                                                  SCSPOD <> *ZERO
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C*  EMP# & DIV IF NOT EQUAL TO 0: VALIDATE EMP#, DIV, & AGT#
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C     SCEMP#        IFNE      *ZERO
     C     SCDIV         ORNE      *ZERO
     C*
     C*  VALIDATE THE EMPLOYER/DIV
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C*
     C                   EXSR      EMPAGT
     C*
     C                   ENDIF                                                  SCEMP#<>*ZERO
     C*
     C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ENDSR                                                  ERRCKC
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   MOVE      SCAGT#        KKAGT#
      ***** END ADD ***** 05/18/04 ************************************
     C                   MOVE      SCEMP#        SSEMP#
     C                   MOVE      SCDIV         SSDIV
      ***** BEG ADD ***** 06/07/04 ************************************
     C                   Z-ADD     *ZEROS        SSPFYR
     C                   Z-ADD     *ZEROS        SSPFDT
     C                   Z-ADD     *ZEROS        SSPTDT
     C                   Z-ADD     *ZEROS        SSTFDT
     C                   Z-ADD     *ZEROS        SSTTDT
     C                   Z-ADD     *ZEROS        SSPROD
      ***** END ADD ***** 06/07/04 ************************************
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKEMP#
     C                   CLEAR                   KKDIV
     C                   CLEAR                   KKPROD
     C                   CLEAR                   KKPODT
     C                   CLEAR                   KKTIME
     C                   Z-ADD     SSEMP#        KKEMP#
     C                   Z-ADD     SSDIV         KKDIV
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   Z-ADD     SSPFYR        KKFYR
     C*
     C                   Z-ADD     SSPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPODT
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   MOVE      SCAGT#        KKAGT#
     C*
     C     *IN21         IFEQ      *ON
      ***** END ADD ***** 05/18/04 ************************************
     C                   MOVE      SCPSTE        KKEMP#
     C                   MOVE      SCPSTF        KKDIV
     C                   CLEAR                   KKPROD
     C                   CLEAR                   KKPODT
     C                   CLEAR                   KKTIME
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   MOVE      SCPST2        XXPST2            8 0
     C     XXPST2        IFNE      *ZEROS
     C                   Z-ADD     XXPST2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPODT
     C                   ENDIF
     C                   ENDIF
      *
     C     *IN22         IFEQ      *ON
     C                   MOVE      SCPST1        KKEMP#
     C                   MOVE      SCPSTA        KKDIV
     C                   CLEAR                   KKPROD
     C                   CLEAR                   KKPODT
     C                   CLEAR                   KKTIME
     C                   MOVE      SCPSTB        XXPSTB            8 0
     C     XXPSTB        IFGT      *ZEROS
     C                   Z-ADD     XXPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPODT
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   MOVE      SCAGT#        KKAGT#
     C                   Z-ADD     SCFYR         KKFYR
      ***** END ADD ***** 05/18/04 ************************************
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   Z-ADD     SCPROD        KKPROD
     C                   Z-ADD     SCPODT        KKPODT
     C                   Z-ADD     SCTIME        KKTIME
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* KJDO   - Process F10=Search
     C*================================================================
     C*
     C     KJDO          BEGSR
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   SELECT
      ***** BEG DEL ***** 05/18/04 ************************************
      ***** BEG ADD ******* 05/13/02 **********************************
      ********
      ********  Group Policy Year Search
      ********
     C********   HFLD      WHEQ 'SCFYR'
     C********             CALL 'W0011R'
     C********             PARM *BLANK    ##KC    1
     C********             PARM *BLANK    ##KL    1
     C********             PARM '2'       ##CALL  1
     C********             PARM FNRRN     ##RRN   90
     C********             PARM *ZERO     ##YR    30
      ********
     C********   ##YR      IFEQ *ZERO
     C********             SETON                     75
     C********             ELSE
     C********             Z-ADD##YR      SCFYR
     C********             SETON                     86
     C********             ENDIF                           ##YR
      ********
      ***** END ADD ******* 05/13/02 **********************************
      ***** END DEL ***** 05/18/04 ************************************
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      FNRRN         ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C                   MOVE      *ON           *IN85
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C     SCEMP#        ANDNE     *ZEROS
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C                   CALL      'W0553R'                             99
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      EMRRN         ##RRN             9 0
     C                   PARM                    ##QTE             1
     C                   PARM      'E'           ##MODE            1
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C                   MOVE      *ON           *IN86
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     WMAHP                              60
     C*
      ***** BEG DEL ***** 05/18/04 ************************************
     C**********           MOVE AHFYR     SCFYR   3
      ***** BEG DEL ***** 05/18/04 ************************************
     C                   MOVE      AHAGT#        SCAGT#
     C                   MOVE      *ON           *IN85
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*_______________________________________________________________
     C*
     C     HFLD          WHENEQ    'SCAGT#'
     C     SCEMP#        ANDEQ     *ZEROS
     C*
     C                   CALL      'W0040R'                             99
     C                   PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      FNRRN         ##RRN             9 0
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZERO
     C*
     C                   MOVE      *ON           *IN86
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     WMAAP                              60
     C*
     C                   MOVE      AAAGT#        SCAGT#
     C                   MOVE      *ON           *IN85
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*
     C*================================================================
     C* KMDO2  - Process F13; user parameters (SCRCRT)
     C*================================================================
     C*
     C     KMDO2         BEGSR
     C*
     C                   CALL      USPAPI                               99
     C*
     C                   EXSR      GETCOF
     C*
     C                   ENDSR                                                  KMDO2
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   XXDEC
     C                   CLEAR                   EDTMSK
     C                   Z-ADD     ACEMP#        SCEMP#
     C                   Z-ADD     ACDIV         SCDIV
     C                   Z-ADD     ACPROD        SCPROD
     C                   Z-ADD     ACPODT        SCPODT
     C                   Z-ADD     ACTIME        SCTIME
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   Z-ADD     ACFYR         SCFYR
     C                   MOVEL(P)  ACFYR         DFYR
     C                   MOVEL(P)  ACFYR         D2FYR
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   MOVEL(P)  ACEMP#        XXNUM
     C                   CALL      CHGNUM        CHGPL                    99
     C                   MOVEL(P)  XXNUM         DEMP#
      ***** BEG ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  XXNUM         D2EMP#
      ***** END ADD ******* 06/16/03 **********************************
     C*
     C                   MOVEL(P)  ACDIV         XXNUM
     C                   CALL      CHGNUM        CHGPL                    99
     C                   MOVEL(P)  XXNUM         DDIV
      ***** BEG ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  XXNUM         D2DIV
      ***** END ADD ******* 06/16/03 **********************************
     C*
     C                   MOVEL(P)  ACPROD        XXNUM
     C                   CALL      CHGNUM        CHGPL                    99
     C                   MOVEL(P)  XXNUM         DPROD
      ***** BEG ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  XXNUM         D2PROD
      ***** END ADD ******* 06/16/03 **********************************
      *
      ***** BEG ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  ACCHCK        XXNUM
     C                   CALL      CHGNUM        CHGPL                    99
     C                   MOVEL(P)  XXNUM         D2CHCK
      ***** END ADD ******* 06/16/03 **********************************
     C*
     C                   Z-ADD     ACPODT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL                    99
     C                   MOVEL(P)  XXNUM         DPODT
      ***** BEG ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  XXNUM         D2PODT
      ***** END ADD ******* 06/16/03 **********************************
     C*
     C                   Z-ADD     ACTRDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL(P)  SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL                    99
      *
      ***** BEG CHG ***** 05/18/04 ************************************
     C****************     MOVELXXNUM     DTRDT     P
     C                   MOVEL(P)  XXNUM         D2TRDT
      ***** END CHG ***** 05/18/04 ************************************
     C*
     C                   Z-ADD     2             XXDEC
     C                   MOVEL(P)  ACAMT         XXNUM
     C                   MOVEL(P)  'L'           EDTMSK
     C                   CALL      CHGNUM        CHGPL                    99
     C                   MOVEL(P)  XXNUM         DAMT
      ***** BEG ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  XXNUM         D2AMT
      ***** END ADD ******* 06/16/03 **********************************
     C*
     C                   MOVEL(P)  ACCOCD        DCOCD
      ***** BEG DEL ***** 05/18/04 ************************************
      ***** BEG ADD ******* 06/16/03 **********************************
     C*********            MOVELACCOCD    D2COCD    P
     C*********            MOVELACCOTP    D2COTP    P
     C*********            MOVELACDESC    D2DESC    P
      ***** END ADD ******* 06/16/03 **********************************
      ***** END DEL ***** 05/18/04 ************************************
     C                   MOVEL(P)  ACCOTP        DCOTP
      *
     C                   MOVEL(P)  ACDESC        DDESC
      ***** BEG ADD ******* 06/16/03 **********************************
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
      ***** END ADD ******* 06/16/03 **********************************
     C                   MOVEL(P)  DATA          SCDATA
     C                   MOVEL     HD(1)         SCHDL1
      ***** BEG ADD ******* 06/16/03 **********************************
      *
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  DATA2         SCDATA
     C                   MOVEL     HD(2)         SCHDL1
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  DEMP#         D3EMP#
     C                   MOVEL(P)  DDIV          D3DIV
     C                   MOVEL(P)  DFYR          D3FYR
     C                   MOVEL(P)  DPODT         D3PODT
     C                   MOVEL(P)  DPROD         D3PROD
     C                   MOVEL(P)  DCOCD         D3COCD
     C                   MOVEL(P)  DCOTP         D3COTP
     C                   MOVEL(P)  DDESC         D3DESC
     C                   MOVEL(P)  DAMT          D3AMT
     C                   MOVEL     HD(3)         SCHDL1
     C                   MOVEL(P)  DATA23        SCDATA
     C     VIEW#         WHENEQ    2
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  D2EMP#        D4EMP#
     C                   MOVEL(P)  D2DIV         D4DIV
     C                   MOVEL(P)  D2PODT        D4PODT
     C                   MOVEL(P)  D2FYR         D4FYR
     C                   MOVEL(P)  D2PROD        D4PROD
     C                   MOVEL(P)  D2TRDT        D4TRDT
     C                   MOVEL(P)  D2CHCK        D4CHCK
     C                   MOVEL(P)  D2AMT         D4AMT
     C                   MOVEL     HD(4)         SCHDL1
     C                   MOVEL(P)  DATA24        SCDATA
      ***** END ADD ***** 05/18/04 ************************************
      *
     C                   ENDSL
      *
      ***** END ADD ******* 06/16/03 **********************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C     *INKL         IFEQ      *ON
     C     XXCRT         ANDEQ     *ON
     C     Q2RECF        ANDNE     'SCRCRT'
     C*
     C                   MOVE      *BLANKS       XXOPFN
     C                   MOVE      *BLANKS       XXNMBR
     C                   MOVE      *BLANKS       XXMIS
     C                   Z-ADD     1             INFO#
     C                   Z-ADD     1             VIEW#
     C                   Z-ADD     1000          @INC                           BASE INCREMENT
     C                   Z-ADD     @INC          @INCC                          CURR INCREMENT
     C                   MOVE      *OFF          AUTORF
     C                   MOVE      *OFF          SNGITM
     C                   MOVE      *OFF          SELMOD
     C                   MOVE      *OFF          *IN06
     C                   EXSR      GETCOF
     C                   EXSR      KEYINZ
     C                   EXSR      PGMSTR
     C                   EXSR      GETHD#
     C                   EXSR      BLDLNS
     C                   EXSR      GETPGT
     C                   EXSR      INZSST
     C                   EXSR      CLRGET
     C                   CALL      'SRRMVMSG'                           99
     C                   EXSR      SNDSSM
     C*
     C                   MOVE      *ON           RSTPGM
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C     Q1PRMS        IFGE      8
     C   KC              MOVE      'A'           @@RTCD
     C   KL              MOVE      'B'           @@RTCD
     C                   ENDIF                                                  Q1PRMS>=8
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *OFF          XXCRT
     C                   CLEAR                   @@CALL
     C                   CLEAR                   QQFYR
     C                   CLEAR                   QQEMP#
     C                   CLEAR                   QQDIV
     C                   CLEAR                   QQAGT#
     C                   CLEAR                   QQFND
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   CLEAR                   SCSPOD
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C     Q1PRMS        IFGE      2
     C                   MOVE      @#CALL        @@CALL
     C                   ENDIF                                                  Q1PRMS>=2
     C*
     C     Q1PRMS        IFGE      3
     C                   MOVE      @@FYR         QQFYR
     C                   ENDIF                                                  Q1PRMS>=3
     C*
     C     Q1PRMS        IFGE      4
     C                   MOVE      @@EMP#        QQEMP#
     C                   ENDIF                                                  Q1PRMS>=4
     C*
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@DIV         QQDIV
     C                   ENDIF                                                  Q1PRMS>=5
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@AGT#        QQAGT#
     C                   ENDIF                                                  Q1PRMS>=6
     C*
     C     Q1PRMS        IFGE      7
     C                   MOVE      @@RTCD        QQRTCD
     C                   ENDIF                                                  Q1PRMS>=7
     C*
     C     Q1PRMS        IFGE      8
     C                   MOVE      @@FND         QQFND
     C                   ENDIF                                                  Q1PRMS>=8
     C*
     C                   MOVE      *OFF          *IN45
     C*
     C     QQFND         IFNE      *BLANKS
     C*
     C                   CLEAR                   ##FNAM
     C                   MOVE      QQFND         ##FND
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C  N60              MOVEL(P)  FNNAME        ##FNAM
     C*
     C                   MOVE      *ON           *IN45
     C*
     C                   ENDIF                                                  QQFND<>*BLANKS
     C*
      ***** BEG DEL ***** 05/18/04 ************************************
     C*********            MOVE QQFYR     SCFYR
      ***** END DEL ***** 05/18/04 ************************************
     C                   MOVE      QQAGT#        SCAGT#
     C                   MOVE      QQEMP#        SCEMP#
     C                   MOVE      QQDIV         SCDIV
     C*
     C                   MOVE      QQEMP#        SSEMP#
     C                   MOVE      QQDIV         SSDIV
     C*
     C     SCFND         IFEQ      *ZERO
      ***** BEG DEL ***** 05/18/04 ************************************
     C********   SCFYR     OREQ *ZERO
      ***** END DEL ***** 05/18/04 ************************************
     C     SCAGT#        OREQ      *ZERO
     C*
     C     CRCONS        CAT(P)    OPTNM:1       ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C*
     C                   MOVEL     ##DATA        SCPRDC
     C*
     C                   MOVE      *ON           XXCRT
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   Z-ADD     ##FND         SCFND
     C                   MOVEL(P)  ##FNAM        SCNAME
     C*
     c*** begin add ***  01/15/13  *********************************************************
     C*
      /If Defined(WEB)
      /free
       clear fl;
       fl(1) = 'F3=Exit   F10=Search   F13=Chg user parms';
       AV_KEYS_DS.ARRAY = fl;
       AV_KEYS = AV_KEYS_DS.KEYS;
      /End-free
      /Endif
    C*
     c*** end add *****  01/15/13  *********************************************************
     C                   EXFMT     SCRCRT
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '00000'       *IN(86)
     C*
B03  C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKM         CASEQ     *ON           KMDO2                    90
     C                   CAS                     OTHFKS
     C                   ENDCS
     C*
B04  C     *IN90         IFEQ      *OFF
     C                   EXSR      ERRCKC
E04  C                   ENDIF                                                  *IN90=*OFF
     C*
B04  C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   Z-ADD     SCFYR         KKFYR
      ***** BEG CHG ***** 06/07/04 ************************************
     C********             CLEARSSPODT
     C                   CLEAR                   SSPFYR
     C                   CLEAR                   SSPROD
     C                   CLEAR                   SSPFDT
     C                   CLEAR                   SSPTDT
     C                   CLEAR                   SSTFDT
     C                   CLEAR                   SSTTDT
      ***** END CHG ***** 06/07/04 ************************************
      *
     C     SCSPOD        IFGT      *ZEROS
     C                   Z-ADD     SCSPOD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKPODT
     C                   ENDIF
      ***** END ADD ***** 05/18/04 ************************************
      *
E04  C                   ENDIF                                                  *IN90=*OFF
     C*
     C                   ELSE
     C*
     C                   EXSR      PGMEND
     C*
     C                   RETURN
     C*
E04  C                   ENDIF                                                  *INKC=*OFF
     C*
     C                   ENDDO                                                  *IN90=*OFF
     C*
E04  C                   ENDIF                                                  SCFND=*ZEROS
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
      ***** BEG DEL ***** 05/18/04 ************************************
     *********             SELEC
     C********   SSEMP#    WHNE *ZERO
     C***21***   KEY2      READEDBRCDF                   95
     C********             OTHER
     C***21***   KEY1      READEDBRCDF                   95
     C*******              ENDSL
      ***** END DEL ***** 05/18/04 ************************************
      *
      ***** BEG ADD ***** 06/07/04 ************************************
      ***** END ADD ***** 06/07/04 ************************************
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   SELECT
     C     *IN21         WHENEQ    *ON
      *
     C     KEY1          READE     DBRCDF                                 95
      *
     C     *IN22         WHENEQ    *ON
     C     SSEMP#        IFNE      *ZERO
     C     KY2           READE     DBRCD2                                 95
     C                   ELSE
     C     KY1           READE     DBRCD2                                 95
     C                   ENDIF
     C                   OTHER
     C                   ENDSL
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
      ***** BEG DEL ***** 05/18/04 ************************************
     C********             SELEC
     C********   SSEMP#    WHNE *ZERO
     C***21***   KEY2      REDPEDBRCDF                   95
     C********             OTHER
     C***21***   KEY1      REDPEDBRCDF                   95
     C********             ENDSL
      ***** END DEL ***** 05/18/04 ************************************
      *
      ***** BEG ADD ***** 05/18/04 ************************************
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     KEY1          READPE    DBRCDF                                 95
     C*
     C     *IN22         WHENEQ    *ON
     C     SSEMP#        IFNE      *ZERO
     C     KY2           READPE    DBRCD2                                 95
     C                   ELSE
     C     KY1           READPE    DBRCD2                                 95
     C                   ENDIF
     C                   ENDSL
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY           SETGT     DBRCDF                             96
      ***** BEG ADD ***** 05/18/04 ************************************
     C   22KY            SETGT     DBRCD2                             96
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY           SETLL     DBRCDF                             96
      ***** BEG ADD ***** 05/18/04 ************************************
     C   22KY            SETLL     DBRCD2                             96
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     SSEMP#        ANDNE     ACEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEMP#<>*ZEROS
     C*
     C     SSDIV         IFNE      *ZEROS
     C     SSDIV         ANDNE     ACDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSDIV<>*ZEROS
     C*
      ***** BEG ADD ***** 05/18/04 ************************************
     C*********  SSPODT    IFNE *ZEROS
     C*********            Z-ADDSSPODT    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     XXPODT  70
     C*********  XXPODT    IFNE ACPODT
     C*********            MOVE *OFF      MEETSS
     C*********            ENDIF                           SSDIV<>*ZEROS
     C*********            ENDIF                           SSDIV<>*ZEROS
      ***** END ADD ***** 05/18/04 ************************************
     C*
      ***** BEG ADD ***** 06/07/04 ************************************
     C*
     C     SSPFYR        IFNE      *ZEROS
     C     SSPFYR        ANDNE     ACFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSPFYR<>*ZEROS
     C*
     C     SSPFDT        IFNE      *ZEROS
     C     ACPODT        IFLT      X7PFDT
     C     ACPODT        ORGT      X7PTDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  X7PFDT<>*ZEROS
     C                   ENDIF                                                  SSPFDT<>*ZEROS
     C*
     C     SSTFDT        IFNE      *ZEROS
     C     ACTRDT        IFLT      X7TFDT
     C     ACTRDT        ORGT      X7TTDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  X7TFDT<>*ZEROS
     C                   ENDIF                                                  SSTFDT<>*ZEROS
     C*
     C     SSPROD        IFNE      *ZEROS
     C     SSPROD        ANDNE     ACPROD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSPROD<>*ZEROS
     C*
      ***** END ADD ***** 05/18/04 ************************************
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
 Post Date   Employer    Div   Yr   Prod   CD/T  Description          Amount
 Post Date   Employer    Div   Yr  Prod    Trans Dt   Check #         Amount
 Employer   Div    Post Date   Yr   Prod   CD/T  Description          Amount
 Employer   Div    Post Date   Yr  Prod    Trans Dt   Check #         Amount
** Description--------------MISN
1011=Select                 NNN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
209F9=Change Info           YYY
211F11=Chg View             YYY
212F12=Cancel               YYY
217F17=Subset               YYY
3
