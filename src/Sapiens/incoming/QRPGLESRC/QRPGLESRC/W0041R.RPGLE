     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W0041R - WORK WITH AGENT ASSIGNMENTS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL AGENTS *
     H*                ASSIGNED TO A GROUP OR ALL ASSIGNMENTS FOR A   *
     H*                PARTICUAL AGENT.  EACH ASSIGNMENT CAN BE       *
     H*                ACCESSED FOR UPDATE, DISPLAY OR SELECTION      *
     H*                DEPENDING UPON THE CALLING PROGRAM.            *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0040R   - WORK WITH AGENTS                                *
     H*  CALLS TO:                                                    *
     H*    W0041R02 - AGENT ASSIGNMENT INFORMATION (OPT 2/4/5/20)     *
     H*    W4150R   - QUOTE STATUS (OPT 9)                            *
     H*    SC010R   - DARTS (OPT 14)                                  *
     H*                                                               *
     H*  CREATED: 11/10/97  PROGRAMMER: C:33  PROJECT: WC683          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 11/27/18 C:33  Project: SY406
     H*                Add Option 75 = Template Associations
     H*                                                               *
     H* 12/26/14 C:283 Support WA099 87243                            *
     H*                1) Use the Default Group when restriction      *
     H*                    flag is set to 'Y' and nothing is in the   *
     H*                    restriction table.                         *
     H*                2) When only single entry is there in the      *
     H*                    Group restriction table, then use the      *
     H*                    proper groups to set the Group Restrcition.*
     H*                                                               *
     H* 09/19/13 C:283 Support WA147 83464                            *
     H*                Correct the Group Restrictions handling if     *
     H*                there are multiple groups restricted.          *
     H*                                                               *
     H* 04/25/13 C:283 Support WA114 82188                            *
     H*                Use the WPIBANK to validate for EFT account.   *
     H*                                                               *
     H* 06/29/09 C:191 WC1806                                         *
     H*                EFT - UPDATE CALL PARAMS TO W0028R            *
     H*                                                               *
     H* 06/20/08 C:189 SY156                                          *
     H*                Recompiled  Program for level check error on   *
     H*                W0041F.                                        *
     H*                                                               *
     H*                                                               *
     H* 04/01/08 C:189 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                (ADDITIONAL CHANGES FOR CONTROL FLAG WCFL17)   *
     H*                                                               *
     H* 03/19/08 C:189 WC997                                          *
     H*                IMPLEMENT EFT - AGENT COMMISSION PAYMENTS      *
     H*                (ADDITIONAL CHANGES FOR CONTROL FLAG WCFL38)   *
     H*                                                               *
     H*  12/07/00  C:33  PROJECT: WC997                               *
     H*                  ADD OPTION 70=EFT ACCOUNT INFO               *
     H*                                                               *
     H*  08/21/06 C:53  Support: 53318                                *
     H*                 Added check for additional restriction info   *
     H*                                                               *
     H* 07/31/02 C:61  SUPPORT WA109 33613                            *
     H*                Display "20=Change status" in option list.     *
     H*                                                               *
     H* 10/13/00 C:61  PROJECT WC914                                  *
     H*              - Enable 3=Copy.                                 *
     H*                                                               *
     H* 08/07/00 C:61  SUPPORT WA095 23837                            *
     H*              - default ##QTE to *BLANK if not passed.         *
     H*                                                               *
     H* 03/17/00 C:61  PROJECT WC976                                  *
     H*              - Add parm to indicate when called from quote    *
     H*                menu.                                          *
     H*              - Pass @@QTE to Work with Quote Status           *
     H*                (opt 9, W4150R).                               *
     H*                                                               *
     H*  02/04/98  C:33  PROJECT: WC683                               *
     H*                  ADD OPTION 14-DARTS                          *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
     FW0041F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMAAL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAA01:DBRCDF)
     FWMAAL1    IF   E           K DISK
     F                                     INFDS(DBFDSB)
     F                                     RENAME(PAA01:DBRCDFB)
     FSMCOL     IF   E           K DISK
     FWMAHL3    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
     FWMFNL     IF   E           K DISK
     FWMAGP     IF   E             DISK
     F                                     RENAME(PAG01:PAG02)
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
     F***  BEGIN ADD  ***  12/07/00  *********************************
     FWMBIL2    IF   E           K DISK
     F                                     RENAME(PBI01:PBI02)
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  12/07/00  *********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings (AGE
     D***  BEGIN CHG  ***  02/04/98  *********************************
     D**********          OF      1  12  1   OFD    35   Options, Function
      ***** BEG CHG ******* 10/13/00 **********************************
     D**********          OF      1  13  1   OFD    35   Options, Function
     D***  BEGIN CHG  ***  12/07/00  *********************************
     D**********          OF      1  14  1   OFD    35   Options, Function
     D***  Begin Chg  ***  11/27/18  ************************************
     D*OF*******       S              1    DIM(15) CTDATA PERRCD(1)             Options, Function
     D*OFD******       S             35    DIM(15) ALT(OF)
     D#ofOFD           C                   20
     D OF              S              1    DIM(#ofOFD) CTDATA PERRCD(1)         Options, Function
     D OFD             S             35    DIM(#ofOFD) ALT(OF)
     D***  End   Chg  ***  11/27/18  ************************************
     D***  END   CHG  ***  12/07/00  *********************************
      ***** END CHG ******* 10/13/00 **********************************
     D***  END   CHG  ***  02/04/98  *********************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D DBFDSB          DS
     D  DBRRNB               397    400B 0
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D                 DS
     D  VIEWAG                 1     70
     D  X1FND                  1      3
     D  X1NAME                 6     62
     D  X1AGT#                65     69
     D                 DS
     D  VIEWGP                 1     70
     D  X2AGT#                 1      5
     D  X2NAME                 8     54
     D  X2FEIN                57     65
     D  X2MOD                 67     69
     D***  BEGIN ADD  ***  02/04/98  *********************************
     D                 DS
     D  QCKEY                  1     40
     D  QCFEIN                 1      9  0
     D  QCMOD                 10     12  0
     D***  END   ADD  ***  02/04/98  *********************************
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agent Assignments')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W0041R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D COMAPI          C                   CONST('WA001R')
     D CHGNUM          C                   CONST('WA003R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
      ***** BEG CHG ******* 07/31/02 **********************************
     D*********     12                    C         SF1PAG
     D SF1PAG          C                   CONST(10)
      ***** END CHG ******* 07/31/02 **********************************
      *******Beg Add********04/25/13**********************************
     D/copy WPRBANK
     d @@co            s              3
     d @@fnd           s              3
     d @@appl          s              2
     d @@srdt          s              8
     d @@acct          s             20
     d @@hdcy          s              1
     d @@chkq          s             10
     d @@ftyp          s             10
     d @@rtcd          s              1
     d @@efdt          s              7
     d @@cpyq          s             10
     d @@cktp          s             10
     d @@eftc          s              1
     d @@coid          s             20

     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
      *
      *******End Add********04/25/13**********************************
      *******Beg Add********09/19/13**********************************
     d count           s              3P 0
     d fndary          s              3P 0 dim(999)
      *******End Add********09/19/13**********************************
      ***** Beg Add ***** 12/26/14 ************************************
     d savFnd          s              3P 0 inz(000)
      ***** End Add ***** 12/26/14 ************************************
     D***  Begin Add  ***  11/27/18  *************************************
      /copy WITMPLTD
      /copy sPrModule
     D***  End   Add  ***  11/27/18  *************************************

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@DSPL            1
      ***** BEG ADD ******* 03/17/00 **********************************
     C                   PARM                    @@QTE             1
      ***** END ADD ******* 03/17/00 **********************************
     C*
     C* NOTE: @@DSPL must be 'F' if @@RRN is related to the group file.
     C*       If @@DSPL is 'F', @@CALL should only be '2' for search.
     C*       @@DSPL does not need to be passed if @@RRN is related to
     C*       the agent master file.
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      XXRRNA        ##RRN2            9 0
      *
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C     EFTPL         PLIST
     C                   PARM      @@CALL        ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C*** BEGIN CHG ****** 06/29/09 ************************************
     C**************       PARM *ZEROS    ##RRN   90
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** END   CHG ****** 06/29/09 ************************************
     C                   PARM      *BLANKS       ##CLM            13
     C                   PARM      *BLANKS       #1FEIN           12
     C                   PARM      *ZEROS        ##SEQ#            5 0
     C                   PARM      'A'           ##TYPE            1
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCAGT#        ##AGT#            5 0
     C***  END   ADD  ***  12/07/00  *********************************
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C                   PARM                    ##OUTQ           10
     C                   PARM                    ##XCO#            1
     C                   PARM                    ##XFND            1
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     KEY1A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKFND
     C*
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     KEY2A         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKAGT#
     C*
     C     AHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCAGT#
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C     FNKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFND
     C*
     C     FNKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    ##FND
     C*
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C     BIKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXAPPL
     C                   KFLD                    SCFND
     C                   KFLD                    XXCKTP
     C***  END   ADD  ***  12/07/00  *********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C***  END   ADD  ***  02/04/98  *********************************
     C     *LIKE         DEFINE    AACO#         KKCO#
     C     *LIKE         DEFINE    AAFEIN        KKFEIN
     C     *LIKE         DEFINE    AAMOD         KKMOD
     C     *LIKE         DEFINE    AAFND         KKFND
     C     *LIKE         DEFINE    AAAGT#        KKAGT#
     C     *LIKE         DEFINE    AGNAME        U1PDSC
     C     *LIKE         DEFINE    AGNAME        U2PDSC
     C     *LIKE         DEFINE    SCRRNA        XXRRNA
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C     *LIKE         DEFINE    BIAPPL        XXAPPL
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BICKTP        XXCKTP
     C***  END   ADD  ***  12/07/00  *********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSNAME        IFNE      *BLANKS
     C     SSSTAT        ORNE      *BLANK
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C  N32SSFND         IFNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSFND<>*ZEROS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '20'
     C*
     C     SCSTAT        IFEQ      'A'
     C*
     C     AHKY          CHAIN     PAH01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     AHSTAT        IFEQ      'A'
     C                   SETON                                        71  90
     C                   LEAVE
     C                   ENDIF                                                  AHSTAT=A
     C*
     C     AHKY          READE     PAH01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  SCSTAT=A
     C*
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C     SCSEL         WHENEQ    '70'
     C*
     C***  Beg   Chg  ***  04/25/13  *********************************
     C***********        MOVEL(P)  'AG'          XXAPPL
     C***********        Z-ADD     SCFND         XXFND
     C***  BEGIN CHG  ***  04/23/01  *********************************
     C**********           MOVEL'E'       XXCKTP    P
     C**********         MOVEL(P)  'EFT'         XXCKTP
     C***  END   CHG  ***  04/23/01  *********************************
     C*
     C*********BIKY2         CHAIN     PBI02                              90
     C*
     C*********          MOVE      *IN90         *IN72
     C                   Move      ##co#         @@co
     C                   Move      scfnd         @@fnd
     C                   Move      'AG'          @@appl
     C                   Time                    dsdttm
     C                   Move      dsdate        @@srdt


      /Free
       @@acct = *Blanks;
       @@call = *Blanks;
       @@hdcy = *Blanks;
       @@chkq = *Blanks;
       @@ftyp = *Blanks;
       @@rtcd = *Blanks;
       @@efdt = *Zeros ;
       @@cpyq = *Zeros ;
       @@cktp = *Blanks;
       @@coid = *Blanks;
       @@eftc = 'E'    ;

       @@acct= GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                @@cktp: @@eftc: @@coid: @@rtcd);
      /End-Free

     c*
     c                   if        @@rtcd='1'
     c                   seton                                        72  90
     c                   endif                                                  not %found
     c*** end   chg ***  04/25/13  *****************************************
     C***  END   ADD  ***  12/07/00  *********************************
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSNAME
     C  N32              CLEAR                   SSFND
     C                   CLEAR                   SSSTAT
     C*
     C     @@CALL        IFEQ      '2'
     C                   MOVE      'A'           SSSTAT
     C                   ENDIF                                                  @@CALL=2
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKFND
     C                   CLEAR                   KKFEIN
     C                   CLEAR                   KKMOD
     C                   CLEAR                   KKAGT#
     C*
     C     *IN31         IFEQ      *ON
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNFND         KKFND
     C                   ELSE
     C                   Z-ADD     AGCO#         KKCO#
     C                   Z-ADD     AGFEIN        KKFEIN
     C                   Z-ADD     AGMOD         KKMOD
     C                   ENDIF                                                  *IN31=*ON
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        KKFND
     C                   MOVE      SCPST5        KKAGT#
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCAGT#        KKAGT#
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     AAFND         SCFND
     C                   Z-ADD     AAAGT#        SCAGT#
     C                   Z-ADD     AAFEIN        SCFEIN
     C                   Z-ADD     AAMOD         SCMOD
     C                   MOVEL     AASTAT        SCSTAT
     C*
     C                   CLEAR                   VIEWGP
     C                   CLEAR                   VIEWAG
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL     AAFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         X1FND
     C*
     C                   MOVEL     AAMOD         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         X2MOD
     C*
     C                   MOVEL     AAAGT#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         X1AGT#
     C                   MOVEL     XXNUM         X2AGT#
     C*
     C                   MOVE      AAFEIN        X2FEIN
     C*
     C     *IN31         IFEQ      *ON
     C     AGKY          CHAIN     PAG01                              60
     C  N60              MOVEL(P)  AGNAME        X2NAME
     C                   MOVEL(P)  VIEWGP        SCDATA
     C                   Z-ADD     DBRRNB        SCDBRR
     C                   Z-ADD     DBRRNA        SCRRNA
     C                   ELSE
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        X1NAME
     C                   MOVEL(P)  VIEWAG        SCDATA
     C                   Z-ADD     XXRRNA        SCRRNA
     C                   ENDIF                                                  *IN31=*ON
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OPT9 - Processing for OPT 9
     C*================================================================
     C*
     C* OPTION 9=QUOTE STATUS
     C*
     C     OPT9          BEGSR
     C*
     C     *IN09         IFEQ      *ON
     C     *IN30         OREQ      *ON
     C                   MOVE      '1'           ##CALL
     C                   ELSE
     C                   MOVE      ' '           ##CALL
     C                   ENDIF                                                  *IN09=*ON
     C*
     C                   CALL      'W4150R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
      ***** BEG DEL ******* 08/07/00 **********************************
      ***** BEG ADD ******* 03/17/00 **********************************
     C****************     PARM @@QTE     ##QTE   1
      ***** END ADD ******* 03/17/00 **********************************
      ***** END DEL ******* 08/07/00 **********************************
      ***** BEG ADD ******* 08/07/00 **********************************
     C                   PARM                    ##QTE             1
      ***** END ADD ******* 08/07/00 **********************************
     C*
     C                   ENDSR                                                  OPT9
     C*
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C*===============================================================
     C* OPT14 - SUBROUTINE FOR OPTION 14
     C*===============================================================
     C*
     C* OPTION 14=DARTS
     C*
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   CLEAR                   QCKEY
     C*
     C                   Z-ADD     SCFEIN        QCFEIN
     C                   Z-ADD     SCMOD         QCMOD
     C*
     C*  CALL COMMENT PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0040R'      XXPGM
     C*
     C                   ENDSR                                                  OPT14
     C***  END   ADD  ***  02/04/98  *********************************
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C*===============================================================
     C* OPT70 - SUBROUTINE FOR OPTION 70
     C*===============================================================
     C*
     C* OPTION 70=EFT ACCOUNT INFO
     C*
     C     OPT70         BEGSR
     C*
     C                   CALL      'W0028R'      EFTPL                  99
     C*
     C                   ENDSR                                                  OPT70
     C***  END   ADD  ***  12/07/00  *********************************
     c***  Begin Add  ***  11/27/18  *************************************
    C*================================================================
    C* Opt75 - Process other function keys
    C*================================================================
      /Free
       Begsr Opt75;
        Select;
        When @@call = ' ';
           ##Mode = 'C';
        Other;
           ##Mode = 'D';
        Endsl;
        Reset dsWITMPLTparms;
        dsWITMPLTparms.co# = kkCo#;
        dsWITMPLTparms.fnd = scFnd;
        dsWITMPLTparms.fein = scFein;
        dsWITMPLTparms.mod = scMod;
        TemplateAssociations(dsWITMPLTparms:'Agency/Group':##Mode);
       Endsr;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
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
     C*
     C     SCSEL         CASEQ     '9'           OPT9
     C***  BEGIN ADD  ***  02/04/98  *********************************
     C     SCSEL         CASEQ     '14'          OPT14
     C***  END   ADD  ***  02/04/98  *********************************
     C     SCSEL         CASEQ     '20'          OPT20
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C     SCSEL         CASEQ     '70'          OPT70
     C***  END   ADD  ***  12/07/00  *********************************
     c***  Begin Add  ***  11/27/18  *************************************
     c     scsel         caseq     '75'          opt75
     c***  End   Add  ***  11/27/18  *************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   MOVE      *BLANK        XXDSPL            1
     C*
     C     Q1PRMS        IFGE      5
     C                   MOVE      @@DSPL        XXDSPL
     C                   ENDIF                                                  Q1PRMS>=5
     C*
      ***** BEG ADD ******* 08/07/00 **********************************
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@QTE         ##QTE
     C                   ELSE
     C                   MOVE      *BLANKS       ##QTE
     C                   ENDIF                                                  Q1PRMS>=5
     C*
      ***** END ADD ******* 08/07/00 **********************************
     C*
     C     XXDSPL        IFEQ      'F'
     C                   MOVE      *ON           *IN31
     C     @@RRN         CHAIN     WMFNP                              60
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNFND         KKFND
     C                   Z-ADD     FNFND         SCHFND
     C                   MOVEL(P)  FNNAME        SCFNAM
     C                   MOVEL(P)  HD(2)         HD(1)
     C                   ELSE
     C                   MOVE      *OFF          *IN31
     C     @@RRN         CHAIN     WMAGP                              60
     C                   Z-ADD     AGCO#         KKCO#
     C                   Z-ADD     AGFEIN        KKFEIN
     C                   Z-ADD     AGMOD         KKMOD
     C                   Z-ADD     AGFEIN        SCHFIN
     C                   Z-ADD     AGMOD         SCHMOD
     C                   MOVEL(P)  AGNAME        SCANAM
     C                   Z-ADD     @@RRN         XXRRNA
     C                   ENDIF                                                  XXDSPL=F
     C*
     C                   CLEAR                   @@RRN
     C*
     C     KKCO#         CHAIN     PCO01                              60
     C*
B02  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCCONM
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *OFF          *IN32
     C*
B01  C     ##XFND        IFEQ      'Y'
     C*
      *** Beg Add ***** 12/26/14 ************************************
     c                   Eval      savFnd = ##FND
      *** End Add ***** 12/26/14 ************************************
      *** Beg Add *** 09/19/13 ****************************
     C                   Eval      count = 0
     C                   Eval      *IN33 = *Off
     C     *Start        Setll     WMFNP
     C                   Read      WMFNP
     C                   Dow       Not %Eof(WMFNP)
      *** End Add *** 09/19/13 ****************************
      *** BEG ADD *** 08/21/06 ****************************
      *
      * Check for existence of Fund/Emplr restriction records for this
      *   user.  SMAZP
      *
     C                   CALL      'SA008R'
     C                   PARM                    ##CO#
     C                   PARM      Q1USER        ##USR            10
     C                   PARM      'W'           ##APPL            1
      *** Beg Chg ***** 12/26/14 ************************************
     C************       PARM      *OFF          ##RTCD            1
     C                   PARM      *ON           ##RTCD            1
      *** End Chg ***** 12/26/14 ************************************
     C                   PARM      *BLANK        ##AUTH            1
      *** Beg Chg *** 09/19/13 ****************************
     C**************     PARM                    ##FND
     C                   PARM      FNfnd         ##FND
      *** End Chg *** 09/19/13 ****************************
      *
      *** Beg Del *** 09/19/13 ****************************
     C**************     ##RTCD        IFEQ      *ON
      *
     C************       SETOFF                                       32
     C************       ELSE
      *** END ADD *** 08/21/06 ****************************
     C************       Z-ADD     ##FND         SSFND
     C************       SETON                                        32
     C*
      *** BEG ADD *** 08/21/06 ****************************
     C************       ENDIF                                                  ##RTCD
      *** END ADD *** 08/21/06 ****************************
      *** End Del *** 09/19/13 ****************************
      *** Beg Add *** 09/19/13 ****************************
     C     ##RTCD        Ifeq      *Off
     C                   Eval      count = count + 1
     C                   Eval      fndary(count) = FNfnd
     C                   Endif
     C
     C                   Read      WMFNP
      *
     C                   Enddo
      *
      *** Beg Add ***** 12/26/14 ************************************
      /Free
       If count = 0;
          count = 1;
          fndary(count) = savFnd;
       Endif;
      /End-Free
      *** End Add ***** 12/26/14 ************************************
     C                   If        count = 1
      *** Beg Add ***** 12/26/14 ************************************
      /Free
       ##FND = fndary(count);
      /End-Free
      *** End Add ***** 12/26/14 ************************************
     C                   Z-ADD     ##FND         SSFND
     C                   Endif
      *** End Add *** 09/19/13 ****************************
     C*
E01  C                   ENDIF                                                  ##XFND=Y
     C*
     C***  BEGIN ADD  ***  12/07/00  *********************************
     C     'WC'          CHAIN     SWC01                              60
     C*
     C***  BEGIN CHG  ***  04/23/01  *********************************
     C*********  WCFL16    IFNE 'Y'
     C****  BEG   CHG  ***  04/01/08  *********************************
     C****  BEG   CHG  ***  03/19/08  *********************************
     C*********  WCFL17    IFNE 'Y'
     C*********  WCFL38    IFNE 'Y'
     C     WCFL17        IFNE      'Y'
     C***   END   CHG ***  03/19/08  *********************************
     C****  END   CHG  ***  04/01/08  *********************************
     C***  END   CHG  ***  04/23/01  *********************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '70'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C****  BEG  CHG *****  04/01/08  *********************************
     C***   BEG  CHG ****  03/19/08  *********************************
     C                   ENDIF                                                  WCFL17<>Y
     C**********           ENDIF                           WCFL38<>Y
     C***   END   CHG ***  03/19/08  *********************************
     C****  END   CHG  ***  04/01/08  *********************************
     C***  END   ADD  ***  12/07/00  *********************************
     c***  Begin Add  ***  11/27/18  *************************************
     c* Opt 75 is only allowed if module installed
      /Free
       If isModEnabled('Portal Module');
          xxOpFn = '1';
          xxNmbr = '75';
          xxMIS  = 'YYN';
          Exsr UpdOfA;
       Endif;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C   21
     CANN31KEY1          READE     DBRCDF                                 95
     C   21
     CAN 31KEY2          READE     DBRCDFB                                95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C   21
     CANN31KEY1          READPE    DBRCDF                                 95
     C   21
     CAN 31KEY2          READPE    DBRCDFB                                95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21
     CANN31KEY1A         SETGT     DBRCDF                             96
     C   21
     CAN 31KEY2A         SETGT     DBRCDFB                            96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21
     CANN31KEY1A         SETLL     DBRCDF                             96
     C   21
     CAN 31KEY2A         SETLL     DBRCDFB                            96
     C*
     C                   ENDSR                                                  SETLL
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
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C*
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      40            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      40            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C*  CHECK AGENT NAME AGAINST SEARCH STRING
     C*
      *** Beg Add *** 09/19/13 ****************************
     C                   If        ##xfnd = 'Y'
     C     AAfnd         LookUp    fndary                                 89
     C                   If        *IN89 = *Off
     C                   Eval      Meetss = *Off
     C                   Endif
     C                   Endif
      *** End Add *** 09/19/13 ****************************
B01  C     SSNAME        IFNE      *BLANKS
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C     LO:UP         XLATE(P)  AGNAME        U1PDSC
     C     LO:UP         XLATE(P)  SSNAME        U2PDSC
     C*
     C                   EXSR      STRING
     C*
E01  C                   ENDIF                                                  SSNAME<>*BLANKS
     C*
     C     SSFND         IFNE      *ZERO
     C     SSFND         ANDNE     AAFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND<>*ZERO
     C*
     C     SSSTAT        IFNE      *BLANK
     C     SSSTAT        ANDNE     AASTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTAT<>*BLANK
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt Grp  Name                                                       Agt #  Sts
Opt Agt #  Name                                              Fed Id # Mod  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 NNN
1055=Display                YYY
1099=Quote Status           YYY
11414=DARTS                 YYN
12020=Change Status         YNN
17070=EFT Account info      YYN
17575=Template Associations NNN                            11/27/18
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               YYY
3
